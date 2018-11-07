# vctrs doesn't quite do what I want here.
# see vctrs issue 136
# Personally, I think it is appropriate to do elementwise conversion
# but keep the dimensionality. That preserves the invariants mentioned there

as.double.vctrs_rray <- function(x, ...) {
  x_dbl <- vec_cast(vec_data(x), double())
  array(x_dbl, dim = dim(x), dimnames = dim_names(x))
}

as.integer.vctrs_rray <- function(x, ...) {
  x_int <- vec_cast(vec_data(x), integer())
  array(x_int, dim = dim(x), dimnames = dim_names(x))
}
