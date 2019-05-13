new_array <- function(.data, dim = NULL, dimnames = NULL) {

  dim <- dim %||% length(.data)
  dimnames <- dimnames %||% rray_empty_dim_names(length(dim))

  array(.data, dim = dim, dimnames = dimnames)
}

new_matrix <- function(.data, dim = NULL, dimnames = NULL) {

  dim <- dim %||% length(.data)
  dimnames <- dimnames %||% rray_empty_dim_names(length(dim))

  dim <- at_least_2D(dim, 1L)
  dimnames <- at_least_2D(dimnames, rray_empty_dim_names(1))

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
  as.vector(vec_cast(x, new_shape(double(), rray_shape(x))))
}

#' @export
as.integer.vctrs_rray <- function(x, ...) {
  as.vector(vec_cast(x, new_shape(integer(), rray_shape(x))))
}

#' @export
as.logical.vctrs_rray <- function(x, ...) {
  as.vector(vec_cast(x, new_shape(logical(), rray_shape(x))))
}
