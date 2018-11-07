#' @export
format.vctrs_mtrx <- function(x, ...) {
  format(as_matrix(x))
}

#' @export
t.vctrs_mtrx <- function(x) {
  as_mtrx(t(as_matrix(x)))
}

#' @export
vec_restore.vctrs_mtrx <- function(x, to) {
  as_mtrx(x)
}

#' @export
vec_ptype_abbr.vctrs_mtrx <- function(x) {
  "mtrx"
}
