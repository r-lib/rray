#' @export
format.vctrs_mtrx <- function(x, ...) {
  format(as_matrix(x))
}

#' @export
t.vctrs_mtrx <- function(x) {
  as_mtrx(t(as_matrix(x)))
}
