#' @export
format.vctrs_rray <- function(x, ...) {
  format(as_array(x))
}

#' @export
t.vctrs_rray <- function(x) {
  as_rray(t(as_array(x)))
}

#' @export
vec_restore.vctrs_rray <- function(x, to) {
  as_rray(x) # nothing specific to `to`, compute new dims/names
}

#' @export
`dim<-.vctrs_rray` <- function(x, value) {
  validate_reshape(vec_dim(x), value)
  attr(x, "dim") <- value
  x
}
