# vctrs doesn't quite do what I want here.
# see vctrs issue 136
# Personally, I think it is appropriate to do elementwise conversion
# but keep the dimensionality. That preserves the invariants mentioned there

new_array <- function(.data, dim = NULL, dimnames = NULL) {

  dim <- dim %||% length(.data)
  dimnames <- dimnames %||% new_empty_dim_names(length(dim))

  array(.data, dim = dim, dimnames = dimnames)
}

new_matrix <- function(.data, dim = NULL, dimnames = NULL) {

  dim <- dim %||% length(.data)
  dimnames <- dimnames %||% new_empty_dim_names(length(dim))

  dim <- at_least_2D(dim, 1L)
  dimnames <- at_least_2D(dimnames, new_empty_dim_names(1))

  matrix(.data, nrow = dim[1], ncol = dim[2], dimnames = dimnames)
}

at_least_2D <- function(x, elem) {
  if (vec_size(x) == 1) {
    x <- c(x, elem)
  }
  x
}

#' @export
as.double.vctrs_rray <- function(x, ...) {
  new_array(
    .data = vec_cast(vec_data(x), double()),
    dim = vec_dim(x),
    dimnames = dim_names(x)
  )
}

#' @export
as.integer.vctrs_rray <- function(x, ...) {
  new_array(
    .data = vec_cast(vec_data(x), integer()),
    dim = vec_dim(x),
    dimnames = dim_names(x)
  )
}

#' @export
as.logical.vctrs_rray <- function(x, ...) {
  new_array(
    .data = vec_cast(vec_data(x), logical()),
    dim = vec_dim(x),
    dimnames = dim_names(x)
  )
}
