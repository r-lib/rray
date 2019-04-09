#' Get or set dimensions of an array
#'
#' `rray_subset()` powers `[` for rray objects. Notably, it _never_ drops
#' dimensions, and ignores trailing commas.
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
#' @export
rray_subset <- function(x, ...) {
  out <- vec_data(x)

  indexer <- rray_as_index(x, ...)

  out <- eval_bare(expr(out[!!!indexer]))

  vec_restore(out, x)
}

#' @rdname rray_subset
#' @export
`[.vctrs_rray` <- function(x, ..., drop = FALSE) {
  maybe_warn_drop(drop)
  rray_subset(x, ...)
}

#' @rdname rray_subset
#' @export
`rray_subset<-` <- function(x, ..., value) {
  rray_subset_assign_impl(x, ..., value = value)
}

#' @rdname rray_subset
#' @export
`[<-.vctrs_rray` <- function(x, ..., value) {
  rray_subset_assign_impl(x, ..., value = value)
}

rray_subset_assign_impl <- function(x, ..., value) {
  x_subset <- rray_subset(x, ...)
  value <- vec_cast(value, x_subset)
  value <- rray_broadcast(value, vec_dim(x_subset))

  indexer <- rray_as_index(x, ..., with_drop = FALSE)

  x_data <- vec_data(x)

  eval_bare(expr(x_data[!!!indexer] <- value))

  res <- vec_restore(x_data, x)

  res
}

# ------------------------------------------------------------------------------

#' Get or set a slice of an array
#'
#' `rray_slice()` is a shortcut wrapper around `rray_subset()` that is useful
#' for easily subsetting a single axis.
#'
#' @param x A vector, matrix, array or rray.
#'
#' @param i An integer vector. The slice of `axis` to subset.
#'
#' @param axis An integer. The axis to subset.
#'
#' @param value A value to be assigned to the slice of `x` subset by `i`
#' and `axis`. It will be cast to the type and dimension of the slice of `x`.
#'
#' @details
#'
#' `rray_slice()` can be used with base R objects as well as rrays.
#'
#' @examples
#' x <- rray(1:16, c(2, 2, 2, 2))
#'
#' # Selecting the first column
#' rray_slice(x, i = 1, axis = 2)
#'
#' # rray_slice() is particularly useful for
#' # subsetting higher dimensions because you don't
#' # have to worry about the commas
#' rray_slice(x, i = 2, axis = 4)
#'
#' # Compare the above with the equivalent
#' # using `[`
#' x[, , , 2]
#'
#' # The assignment variation can be useful
#' # for assigning to higher dimensional elements
#' rray_slice(x, 1, 3) <- matrix(c(99, 100), nrow = 1)
#'
#' @export
rray_slice <- function(x, i, axis) {
  i <- vec_cast(i, integer())
  axis <- vec_cast(axis, integer())
  validate_axis(axis, x)

  indexer <- front_pad(i, axis)

  rray_subset(x, !!!indexer)
}

#' @rdname rray_slice
#' @export
`rray_slice<-` <- function(x, i, axis, value) {
  rray_slice_assign_impl(x, i = i, axis = axis, value = value)
}

rray_slice_assign_impl <- function(x, i, axis, value) {
  validate_axis(axis, x)
  indexer <- front_pad(i, axis)
  rray_subset(x, !!!indexer) <- value
  x
}

front_pad <- function(i, axis) {
  padding <- rep(list(missing_arg()), times = axis - 1L)
  c(padding, list(i))
}

# ------------------------------------------------------------------------------

rray_yank <- function(x, i) {
  i <- maybe_missing(i, TRUE)

  out <- vec_data(x)

  indexer <- as_yank_indexer(i, x)

  out <- eval_bare(expr(out[!!!indexer]))

  vec_restore(out, x)
}

`rray_yank<-` <- function(x, i, value) {
  rray_yank_assign_impl(x, i = maybe_missing(i), value = value)
}

# Separate function for easier RStudio debugging
rray_yank_assign_impl <- function(x, i, value) {
  i <- maybe_missing(i, TRUE)

  x_yank <- rray_yank(x, i)
  value <- vec_cast(value, x_yank)
  value <- rray_broadcast(value, vec_dim(x_yank))

  out <- vec_data(x)

  indexer <- as_yank_indexer(i, x)

  eval_bare(expr(out[!!!indexer] <- value))

  vec_restore(out, x)
}

# ------------------------------------------------------------------------------

# can only be used like (with 2D for example)
# x[[1, 1]] not like x[[1]]
# (i.e. must fully qualify pluck indices)

rray_pluck <- function(x, ...) {
  rray_pluck_impl(x, ..., single = FALSE)
}

#' @export
`[[.vctrs_rray` <- function(x, ..., exact = TRUE) {
  maybe_warn_exact(exact)
  rray_pluck_impl(x, ..., single = TRUE)
}

rray_pluck_impl <- function(x, ..., single = FALSE) {
  out <- vec_data(x)

  indexer <- rray_as_index(x, ..., with_drop = FALSE)

  if (single) {
    indexer <- validate_pluck_indexer(indexer)
  }

  out <- eval_bare(expr(out[!!!indexer]))

  out <- as.vector(out)

  vec_restore(out, x)
}

`rray_pluck<-` <- function(x, ..., value) {
  rray_pluck_assign_impl(x, ..., single = FALSE, value = value)
}

#' @export
`[[<-.vctrs_rray` <- function(x, ..., value) {
  rray_pluck_assign_impl(x, ..., single = TRUE, value = value)
}

rray_pluck_assign_impl <- function(x, ..., single = FALSE, value) {
  x_pluck <- rray_pluck_impl(x, ..., single = single)
  value <- vec_cast(value, x_pluck)
  value <- rray_broadcast(value, vec_dim(x_pluck))

  out <- vec_data(x)

  indexer <- rray_as_index(x, ..., with_drop = FALSE)

  eval_bare(expr(out[!!!indexer] <- value))

  vec_restore(out, x)
}

# ------------------------------------------------------------------------------

rray_as_index <- function(x, ..., with_drop = TRUE) {
  dots <- dots_list(..., .preserve_empty = TRUE, .ignore_empty = "trailing")

  indexer <- as_indexer(dots, x)

  if (with_drop) {
    indexer <- c(indexer, drop = FALSE)
  }

  indexer
}

as_indexer <- function(dots, x) {
  proxies <- map(vec_dim(x), seq_len)
  proxy_names <- dim_names(x)
  dots <- pad_missing(dots, x)

  # Set names on the proxy if the indexer is by name
  for (i in seq_along(proxies)) {
    if (is.character(dots[[i]])) {
      names(proxies[[i]]) <- proxy_names[[i]]
    }
  }

  indexer <- map2(dots, proxies, vec_as_index_wrapper)

  indexer
}

pad_missing <- function(dots, x) {
  x_dims <- vec_dims(x)
  requested_dims <- length(dots)

  if (requested_dims > x_dims) {
    glubort(
      "The dimensionality of `x` is {x_dims}. ",
      "Cannot subset into dimension {requested_dims}."
    )
  }

  # n_dots < d, need to pad with missing args
  # (if n_dots == d this does nothing)
  n_missing <- x_dims - requested_dims
  padding <- rep(list(missing_arg()), times = n_missing)
  dots <- c(dots, padding)

  dots
}

vec_as_index_wrapper <- function(i, x) {
  if (is_missing(i)) {
    missing_arg()
  }
  else {
    vctrs:::vec_as_index(i, x)
  }
}

# ------------------------------------------------------------------------------

as_yank_indexer <- function(i, x) {

  if (is.logical(i)) {
    i <- as_yank_indexer_lgl(i, x)
  }
  else if (is.character(i)) {
    glubort("Cannot yank with a character `i`.")
  }
  else {
    i <- as_yank_indexer_default(i, x)
  }

  list(i)
}

as_yank_indexer_default <- function(i, x) {

  if (vec_dims(i) > 1L) {
    glubort("`i` can only have >1 dimensions if it is a logical.")
  }

  # Not looking at vctrs "size" here
  proxy <- seq_len(rray_elems(x))

  vctrs:::vec_as_index(i, proxy)
}

as_yank_indexer_lgl <- function(i, x) {

  ok <- vec_dims(i) == 1L || identical(vec_dim(i), vec_dim(x))
  if (!ok) {
    glubort("A logical `i` must be 1D or have dimensions identical to `x`.")
  }

  i <- as.vector(i)

  as_yank_indexer_default(i, x)
}

# ------------------------------------------------------------------------------

validate_pluck_indexer <- function(indexer) {

  missing_indexes <- map_lgl(indexer, is_missing)

  if (any(missing_indexes)) {
    missing_indexes <- glue::glue_collapse(which(missing_indexes), ", ")
    glubort(
      "Subscript(s) {missing_indexes} must not ",
      "be missing in a pluck."
    )
  }

  lengths <- map_int(indexer, length)
  is_one <- map_lgl(lengths, identical, 1L)

  # Allow for multiple bad subscripts
  if (any(!is_one)) {
    bad_subscript <- which(!is_one)
    bad_lengths <- lengths[!is_one]
    msg <- glue::glue(
      "Subscript {bad_subscript} must have size 1, not {bad_lengths}."
    )
    msg <- glue::glue_collapse(msg, sep ="\n")
    glubort(msg)
  }

  invisible(indexer)
}

# ------------------------------------------------------------------------------

maybe_warn_exact <- function(exact) {
  if (!exact) {
    warn_exact()
  }
}

warn_exact <- function() {
  rlang::warn("`exact` ignored.")
}

maybe_warn_drop <- function(drop) {
  if (drop) {
    warn_drop()
  }
  invisible(drop)
}

warn_drop <- function() {
  rlang::warn("`drop` ignored.")
}

# ------------------------------------------------------------------------------

#' @export
head.vctrs_rray <- function (x, n = 6L, ...) {

  n_size <- vec_size(n)
  if (vec_size(n) != 1L) {
    glubort("`n` must be size 1, not {n_size}.")
  }

  n <- vec_cast(n, integer())

  x_size <- vec_size(x)

  if (n < 0L) {
    n <- max(x_size + n, 0L)
  }
  else {
    n <- min(n, x_size)
  }

  x[seq_len(n),]
}

#' @export
tail.vctrs_rray <- function(x, n = 6L, ...) {

  n_size <- vec_size(n)
  if (vec_size(n) != 1L) {
    glubort("`n` must be size 1, not {n_size}.")
  }

  n <- vec_cast(n, integer())

  x_size <- vec_size(x)

  if (n < 0L) {
    n <- max(x_size + n, 0L)
  }
  else {
    n <- min(n, x_size)
  }

  x[seq.int(to = x_size, length.out = n),]
}
