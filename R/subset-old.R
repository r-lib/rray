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

  vec_cast_container(out, x)
}

#' @rdname rray_extract
#' @export
`rray_extract<-` <- function(x, ..., value) {
  rray_extract_assign_impl(x, ..., value = value)
}

rray_extract_assign_impl <- function(x, ..., value) {
  vec_assert(value, arg = "value")

  x_extract <- rray_extract_impl(x, ...)
  value <- vec_cast(value, x_extract)
  value <- rray_broadcast(value, rray_dim(x_extract))

  out <- vec_data(x)

  indexer <- rray_as_index(x, ..., with_drop = FALSE)

  eval_bare(expr(out[!!!indexer] <- value))

  vec_cast_container(out, x)
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
  proxy_names <- rray_dim_names(x)
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
