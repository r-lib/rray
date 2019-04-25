rray_subset2 <- function(x, ...) {
  indexer <- rray_as_index2(x, ...)

  out <- rray__subset(x, indexer)
  vec_restore(out, x)
}

# ------------------------------------------------------------------------------

is_slice_range <- function(x) {
  inherits(x, "vctrs_slice_range")
}

as_xt_range <- function(x) {
  x <- unclass(x)
  # xt::range(start, stop) is [start, stop)
  start <- x$start - 1L
  stop <- x$stop # - 1L + 1L
  list(start = start, stop = stop)
}

slice_range <- function(start, stop) {
  start <- vec_cast(start, integer())
  stop <- vec_cast(stop, integer())

  if (start > stop) {
    abort("`start` must be less than or equal to `stop`.")
  }

  if (start < 1) {
    abort("`start` must be greater than or equal to 1.")
  }

  new_slice_range(start, stop)
}

new_slice_range <- function(start, stop) {

  if (!is.integer(start)) {
    abort("`start` must be a single integer.")
  }

  if (!length(start) == 1L) {
    abort("`start` must be a single integer.")
  }

  if (!is.integer(stop)) {
    abort("`stop` must be a single integer.")
  }

  if (!length(stop) == 1L) {
    abort("`stop` must be a single integer.")
  }

  new_rcrd(list(start = start, stop = stop), class = "vctrs_slice_range")
}

format.vctrs_slice_range <- function(x, ...) {
  format(vec_data(x))
}

# ------------------------------------------------------------------------------

# This returns a list of correct C indices with one of:
# - A missing value for an xt::all()
# - An integer vector of non-contiguous positions for xt::keep()
# - A list of length 2 representing (start, stop) positions for an xt::range()
rray_as_index2 <- function(x, ...) {
  indexer <- dots_list(..., .preserve_empty = TRUE, .ignore_empty = "trailing")

  dim <- rray_dim(x)
  proxy_names <- dim_names(x)

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
  indexer <- append_missing(indexer, x)

  indexer
}

append_missing <- function(indexer, x) {
  x_dims <- rray_dims(x)
  requested_dims <- length(indexer)

  if (requested_dims > x_dims) {
    glubort(
      "The dimensionality of `x` is {x_dims}. ",
      "Cannot subset into dimension {requested_dims}."
    )
  }

  if (requested_dims == x_dims) {
    return(indexer)
  }

  # n_dots < d, need to pad with missing args
  n_missing <- x_dims - requested_dims
  padding <- rep(list(missing_arg()), times = n_missing)
  indexer <- c(indexer, padding)

  indexer
}
