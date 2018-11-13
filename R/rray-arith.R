# arithmetic ----------------------------------------------------------------------

rray_arith_base <- function(op, x, y) {

  # precompute dimensionality and extend existing dims
  # xtensor-r issue #57 until we have a fix
  dim <- rray_dim2(vec_dim(x), vec_dim(y))
  x <- rray_dims_match(x, dim)
  y <- rray_dims_match(y, dim)

  op_fn <- switch(
    op,
    "+" = rray_add_cpp,
    "-" = rray_subtract_cpp,
    "/" = rray_divide_cpp,
    "*" = rray_multiply_cpp
  )

  restore_type <- vec_type2(x, y)

  vec_restore(op_fn(x, y), restore_type)
}

#' @method vec_arith vctrs_rray
#' @export vec_arith.vctrs_rray
vec_arith.vctrs_rray <- function(op, x, y) {
  UseMethod("vec_arith.vctrs_rray", y)
}

#' @method vec_arith.vctrs_rray vctrs_rray
#' @export
vec_arith.vctrs_rray.vctrs_rray <- function(op, x, y) {
  rray_arith_base(op, x, y)
}

#' @method vec_arith.vctrs_rray MISSING
#' @export
vec_arith.vctrs_rray.MISSING <- function(op, x, y) {
  switch(op,
         "+" = vec_restore(+as_array(x), x),
         "-" = vec_restore(-as_array(x), x)
  )
}

# vctrs_rray <-> numeric / matrix / array

#' @method vec_arith.vctrs_rray numeric
#' @export
vec_arith.vctrs_rray.numeric <- vec_arith.vctrs_rray.vctrs_rray

#' @importFrom vctrs vec_arith.numeric

#' @method vec_arith.numeric vctrs_rray
#' @export
vec_arith.numeric.vctrs_rray <- vec_arith.vctrs_rray.vctrs_rray

