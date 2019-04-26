#' Get or set dimensions of an array
#'
#' `rray_subset()` extracts dimensions from an array _by index_. It powers `[`
#' for rray objects. Notably, it _never_ drops dimensions, and ignores
#' trailing commas.
#'
#' @param x An rray.
#'
#' @param ... A specification of indices to extract.
#' - Integer-ish indices extract specific elements of dimensions.
#' - Logical indices must be length 1, or the length of the dimension you are
#' subsetting over.
#' - Character indices are only allowed if `x` has names for the corresponding
#' dimension.
#' - `NULL` is treated as `0`.
#'
#' @param drop Ignored, but preserved for better error messages with code
#' that might have used arrays before.
#'
#' @param value The value to assign to the location specified by `...`. Before
#' assignment, `value` is cast to the type and dimension of `x[...]`.
#'
#' @details
#'
#' `rray_subset()` and its assignment variant can also be used with base R
#' matrices and arrays to get rray subsetting behavior with them.
#'
#' @section Differences from base R:
#'
#' - `rray_subset()` _never_ drops dimensions.
#'
#' - `rray_subset()` ignores trailing commas. This has the nice property of
#' making `x[1] == x[1,]`.
#'
#' - `rray_subset()<-` casts `value` to `x`, rather than
#' casting `x` to `value`.
#'
#' @examples
#' x <- rray(1:8, c(2, 2, 2))
#'
#' # `rray_subset()` powers `[` so these are identical
#' rray_subset(x, 1)
#' x[1]
#'
#' # Trailing dots are ignored, so these are identical
#' x[1]
#' x[1,]
#'
#' # Missing arguments are treated as selecting the
#' # entire dimension, consistent with base R.
#' # This selects all of the rows, and the first column.
#' x[,1]
#'
#' # Notice that you can't actually do the above with base
#' # R. It requires you to fully specify the dimensions of `x`.
#' # This would throw an error.
#' x_arr <- as_array(x)
#' \dontrun{
#' x_arr[,1]
#' }
#'
#' # To get the same behavior, you have to do:
#' x_arr[, 1, , drop = FALSE]
#'
#' # Note that you can use base R arrays with `rray_subset()`
#' rray_subset(x_arr, , 1)
#'
#' # You can assign to index locations with
#' # x[...] <- value
#' # This assigns 99 to the entire first row
#' x[1] <- 99
#' x
#'
#' # First row in the first
#' # element of the 3rd dimension
#' x[1, , 1] <- 100
#' x
#'
#' # Note that `value` is broadcast to the shape
#' # of `x[...]`. So this...
#' x[,1] <- matrix(5)
#'
#' # ...becomes the same as
#' x[,1] <- array(5, c(2, 1, 2))
#'
#' # You can also use `rray_subset<-()` directly to
#' # use these semantics with base R
#' rray_subset(x_arr, , 1) <- matrix(5)
#' x_arr
#'
#' @family rray subsetters
#' @export
rray_subset <- function(x, ...) {
  indexer <- rray_as_index2(x, ...)

  # TODO
  if (is_any_na_int(indexer)) {
    abort("`NA` indices are not yet supported.")
  }

  out <- rray__subset(x, indexer)

  new_dim_names <- subset_dim_names(dim_names(x), indexer)
  out <- set_full_dim_names(out, new_dim_names)

  vec_restore(out, x)
}

#' @rdname rray_subset
#' @export
`[.vctrs_rray` <- function(x, ..., drop = FALSE) {
  maybe_warn_drop(drop)
  rray_subset(x, ...)
}

# ------------------------------------------------------------------------------

# This returns a list of correct C indices with one of:
# - A missing value for an xt::all()
# - An integer vector of non-contiguous positions for xt::keep()
# - A list of length 2 representing (start, stop) positions for an xt::range()
rray_as_index2 <- function(x, ...) {
  indexer <- dots_list(..., .preserve_empty = TRUE, .ignore_empty = "trailing")
  dim <- rray_dim(x)
  dims <- rray_dims(x)
  indexer <- expand_pad(indexer, dims)
  requested_dims <- vec_size(indexer)
  proxy_names <- dim_names(x)

  if (requested_dims > dims) {
    glubort(
      "The dimensionality of `x` is {dims}. ",
      "Cannot subset into dimension {requested_dims}."
    )
  }

  for (i in seq_along(indexer)) {

    index <- indexer[[i]]

    # Missing indices are converted to `xt::all()`
    if (is_missing(index)) {
      next
    }

    # User supplied `slice_range()`
    if (is_slice_range(index)) {
      indexer[[i]] <- as_xt_range(index)
      next
    }

    proxy <- seq_len(dim[i])

    # Character indices need to match by name in `vec_as_index()`
    if (is.character(index)) {
      names(proxy) <- proxy_names[[i]]
    }

    index <- vctrs:::vec_as_index(index, proxy)

    # Convert contiguous increasing indices to range lists
    if (is_contiguous_increasing(index)) {
      indexer[[i]] <- list(start = index[1] - 1L, stop = index[length(index)])
      next
    }

    # Convert to C index
    indexer[[i]] <- index - 1L
  }

  # After the loop, append any missing indices to the back side
  # to fill out the dimensionality
  indexer <- append_missing(indexer, dims)

  indexer
}

append_missing <- function(indexer, dims) {

  requested_dims <- vec_size(indexer)

  if (requested_dims == dims) {
    return(indexer)
  }

  # n_dots < d, need to pad with missing args
  n_missing <- dims - vec_size(indexer)
  padding <- rep(list(missing_arg()), times = n_missing)
  indexer <- c(indexer, padding)

  indexer
}

as_xt_range <- function(x) {
  # xt::range(start, stop) is [start, stop)
  start <- field(x, "start") - 1L
  stop <- field(x, "stop") # - 1L + 1L
  list(start = start, stop = stop)
}

expand_pad <- function(indexer, dims) {

  has_pad <- map_lgl(indexer, is_pad)

  count_pad <- sum(has_pad)

  if (count_pad == 0L) {
    return(indexer)
  }

  if (count_pad > 1) {
    glubort("Only one `pad()` is allowed in a subset.")
  }

  pad_loc <- which(has_pad)

  # number of dims without pad
  requested_dims <- vec_size(indexer)
  requested_dims_no_pad <- requested_dims - 1L

  n_padding <- dims - requested_dims_no_pad

  # Dimensionality subsetting error will be caught after the padding
  # (this also ensures the error message has the right dimensionality)
  if (n_padding < 0L) {
    n_padding <- 0L
  }

  padding <- rep_len(list(missing_arg()), n_padding)

  n_before <- pad_loc - 1L
  if (n_before == 0L) {
    before <- list()
  }
  else {
    before <- indexer[seq_len(n_before)]
  }

  n_after <- requested_dims - pad_loc
  if (n_after == 0L) {
    after <- list()
  }
  else {
    after <- indexer[(pad_loc + 1):(pad_loc + n_after)]
  }

  indexer <- c(before, padding, after)

  indexer
}
