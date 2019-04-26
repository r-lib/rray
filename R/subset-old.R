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
  value <- rray_broadcast(value, rray_dim(x_subset))

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
#' @family rray subsetters
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

#' Get or set elements of an array by position
#'
#' `rray_yank()` is the counterpart to [rray_extract()]. It extracts elements
#' from an array _by position_. It _always_ drops dimensions
#' (unlike [rray_subset()]), and a 1D vector is always returned. It powers
#' the `[[` method for rrays.
#'
#' @param x A vector, matrix, array or rray.
#'
#' @param i One of the following:
#' - An integer vector specifying the positions of the elements to yank.
#' - A 1D logical vector of length 1, or `rray_elems(x)`.
#' - A logical with the same dimension as `x`.
#'
#' @param value A 1D value to be assigned to the location yanked by `i`. It will
#' be cast to the type and length of `x` after being yanked by `i`.
#'
#' @param ... Not used. An error is thrown if extra arguments are supplied here.
#'
#' @details
#'
#' Dimension names are _only_ kept in the special case of calling `rray_yank()`
#' on a 1D object. Otherwise, the method of keeping them is not well defined.
#'
#' `rray_yank()` works with base R objects.
#'
#' `rray_yank()` is meant as a replacement for the traditional behavior of
#' `x[i]` since `[` for rray objects is much stricter. Separating this special
#' behavior into a different function is less surprising.
#'
#' Additionally, base R has `x[[i]]` which restricts `i` to be length 1.
#' For rray objects, `[[` acts more like `x[i]`, always dropping to 1D, but
#' allowing for the selection of multiple positions.
#'
#' You _cannot_ do `x[[i, j, ...]]` with rrays. For that behavior,
#' see [rray_extract()].
#'
#' @examples
#' x <- rray(10:17, c(2, 2, 2))
#'
#' # Resulting dimension is always 1D
#' rray_yank(x, 1:3)
#'
#' # With logical
#' rray_yank(x, FALSE)
#' rray_yank(x, rep(c(TRUE, FALSE), times = rray_elems(x) / 2))
#'
#' # You can assign a 1D vector to these yanked selections
#' rray_yank(x, c(1, 3, 5)) <- 9
#'
#' # Logicals with the same dim as `x`
#' # can also be used as a yank indexer
#' lgl <- rray(c(TRUE, FALSE), c(2, 2, 2))
#' rray_yank(x, lgl)
#'
#' # And you can set elements in these locations
#' rray_yank(x, lgl) <- NA
#'
#' # `[[` for rray objects is powered by
#' # rray_yank().
#' # This can be very useful for
#' # performing assignment
#' # by position.
#' x[[c(1, 3)]] <- NA
#'
#' # Logical arrays with the same shape as `x`
#' # can be assigned to. This is a useful way
#' # to get rid of NA values.
#' idx <- array(is.na(as.vector(x)), c(2, 2, 2))
#'
#' x[[idx]] <- 0
#'
#' @family rray subsetters
#' @export
rray_yank <- function(x, i) {
  rray_yank_impl(x, maybe_missing(i))
}

#' @rdname rray_yank
#' @export
`[[.vctrs_rray` <- function(x, i, ...) {

  validate_empty_yank_dots(...)

  # TODO - is this inconsistent behavior ok?
  # `rray_yank()` maintains dim names if `x` is 1D.
  # `[[` should never keep them
  dim_names(x) <- NULL

  rray_yank_impl(x, i)
}

rray_yank_impl <- function(x, i) {
  i <- maybe_missing(i, TRUE)

  out <- vec_data(x)

  indexer <- as_yank_indexer(i, x)

  out <- eval_bare(expr(out[!!!indexer]))

  vec_restore(out, x)
}

#' @rdname rray_yank
#' @export
`rray_yank<-` <- function(x, i, value) {
  rray_yank_assign_impl(x, i = maybe_missing(i), value = value)
}

#' @rdname rray_yank
#' @export
`[[<-.vctrs_rray` <- function(x, i, ..., value) {
  validate_empty_yank_assign_dots(...)
  rray_yank_assign_impl(x, i = i, value = value)
}

rray_yank_assign_impl <- function(x, i, value) {
  i <- maybe_missing(i, TRUE)

  x_yank <- rray_yank_impl(x, i)
  value <- vec_cast(value, x_yank)
  value <- rray_broadcast(value, rray_dim(x_yank))

  out <- vec_data(x)

  indexer <- as_yank_indexer(i, x)

  eval_bare(expr(out[!!!indexer] <- value))

  vec_restore(out, x)
}

# ------------------------------------------------------------------------------

#' Get or set elements of an array by index
#'
#' `rray_extract()` is the counterpart to [rray_yank()]. It extracts elements
#' from an array _by index_. It _always_ drops dimensions
#' (unlike [rray_subset()]), and a 1D object is always returned.
#'
#' @inheritParams rray_subset
#'
#' @param value The value to assign to the location specified by `...`.
#' Before assignment, `value` is cast to the type and dimension of `x` after
#' extracting elements with `...`.
#'
#' @details
#'
#' Like `[[`, `rray_extract()` will _never_ keep dimension names.
#'
#' `rray_extract()` works with base R objects as well.
#'
#' `rray_extract()` is similar to the traditional behavior of
#' `x[[i, j, ...]]`, but allows each subscript to have length >1.
#'
#' @examples
#' x <- rray(1:16, c(2, 4, 2), dim_names = list(c("r1", "r2"), NULL, NULL))
#'
#' # Extract the first row and flatten it
#' rray_extract(x, 1)
#'
#' # Extract the first row and first two columns
#' rray_extract(x, 1, 1:2)
#'
#' # You can assign directly to these elements
#' rray_extract(x, 1, 1:2) <- NA
#' x
#'
#' @family rray subsetters
#' @export
rray_extract <- function(x, ...) {
  rray_extract_impl(x, ...)
}

rray_extract_impl <- function(x, ...) {
  out <- vec_data(x)

  indexer <- rray_as_index(x, ..., with_drop = FALSE)

  out <- eval_bare(expr(out[!!!indexer]))

  out <- as.vector(out)

  vec_restore(out, x)
}

#' @rdname rray_extract
#' @export
`rray_extract<-` <- function(x, ..., value) {
  rray_extract_assign_impl(x, ..., value = value)
}

rray_extract_assign_impl <- function(x, ..., value) {
  x_extract <- rray_extract_impl(x, ...)
  value <- vec_cast(value, x_extract)
  value <- rray_broadcast(value, rray_dim(x_extract))

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
  proxies <- map(rray_dim(x), seq_len)
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
  x_dims <- rray_dims(x)
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

  if (rray_dims(i) > 1L) {
    glubort("`i` can only have >1 dimensions if it is a logical.")
  }

  # Not looking at vctrs "size" here
  proxy <- seq_len(rray_elems(x))

  vctrs:::vec_as_index(i, proxy)
}

as_yank_indexer_lgl <- function(i, x) {

  ok <- rray_dims(i) == 1L || identical(rray_dim(i), rray_dim(x))
  if (!ok) {
    glubort("A logical `i` must be 1D or have dimensions identical to `x`.")
  }

  i <- as.vector(i)

  as_yank_indexer_default(i, x)
}

validate_empty_yank_dots <- function(...) {

  dots <- dots_list(..., .preserve_empty = TRUE, .ignore_empty = "none")

  if (length(dots) > 0L) {
    glubort(
      "`[[` selects elements by position. ",
      "Only `x[[i]]` is supported, but {length(dots) + 1} ",
      "indexers were supplied."
    )
  }

  invisible()
}

validate_empty_yank_assign_dots <- function(...) {

  dots <- dots_list(..., .preserve_empty = TRUE, .ignore_empty = "none")

  if (length(dots) > 0L) {
    glubort(
      "`[[<-` assigns elements by position. ",
      "Only `x[[i]] <- value` is supported, but {length(dots) + 1} ",
      "indexers were supplied."
    )
  }

  invisible()
}

# ------------------------------------------------------------------------------

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
