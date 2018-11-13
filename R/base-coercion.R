# vctrs doesn't quite do what I want here.
# see vctrs issue 136
# Personally, I think it is appropriate to do elementwise conversion
# but keep the dimensionality. That preserves the invariants mentioned there

as_array_proxy <- function(x, proxy) {
  n <- vec_size(vec_data(x))
  proxy <- array(rep(proxy, times = n), dim = vec_dim(x), dimnames = dim_names(x))
  vec_cast(x, proxy)
}

as_matrix_proxy <- function(x, proxy) {
  n <- vec_size(vec_data(x))
  proxy <- matrix(rep(proxy, times = n), dim = vec_dim(x), dimnames = dim_names(x))
  vec_cast(x, proxy)
}

#' @export
as.double.vctrs_rray <- function(x, ...) {
  as_array_proxy(x, 1.0)
}

#' @export
as.integer.vctrs_rray <- function(x, ...) {
  as_array_proxy(x, 1L)
}

#' @export
as.logical.vctrs_rray <- function(x, ...) {
  as_array_proxy(x, TRUE)
}

#' @export
as.double.vctrs_mtrx <- function(x, ...) {
  as_matrix_proxy(x, 1.0)
}

#' @export
as.integer.vctrs_mtrx <- function(x, ...) {
  as_matrix_proxy(x, 1L)
}

#' @export
as.integer.vctrs_mtrx <- function(x, ...) {
  as_matrix_proxy(x, TRUE)
}
