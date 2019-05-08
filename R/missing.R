#' @export
is.na.vctrs_rray <- function(x) {
  res <- is.na(vec_data(x))
  vec_restore(res, x)
}

# Currently, I am choosing to use the vctrs implementation of
# `is.na<-vctrs_vctr`. It assigns entire rows to `NA`, and
# only accepts logicals of either length 1, or as long
# as the number of rows in `x`.
# We test for this in `test-missing.R`

