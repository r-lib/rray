# arithmetic ----------------------------------------------------------------------

mtrx_arith_base <- function(op, x, y) {

  op_fn <- switch(
    op,
    "+" = mtrx_add,
    "-" = mtrx_subtract,
    "/" = mtrx_divide,
    "*" = mtrx_multiply
  )

  # Even though mtrx is not a true matrix, just by setting the dim
  # attribute, xtensorrr recognizes it as one through R_DimSymbol
  as_mtrx(op_fn(x, y))
}

# need to remember how to export these correctly!!!

#' @method vec_arith vctrs_mtrx
#' @export vec_arith.vctrs_mtrx
vec_arith.vctrs_mtrx <- function(op, x, y) {
  UseMethod("vec_arith.vctrs_mtrx", y)
}

#' @method vec_arith.vctrs_mtrx vctrs_mtrx
#' @export
vec_arith.vctrs_mtrx.vctrs_mtrx <- function(op, x, y) {
  mtrx_arith_base(op, x, y)
}

#' @method vec_arith.vctrs_mtrx MISSING
#' @export
vec_arith.vctrs_mtrx.MISSING <- function(op, x, y) {
  switch(op,
         "+" = as_mtrx(+as_matrix(x)),
         "-" = as_mtrx(-as_matrix(x))
  )
}

# vctrs_mtrx <--> numeric

#' @method vec_arith.vctrs_mtrx numeric
#' @export
vec_arith.vctrs_mtrx.numeric <- vec_arith.vctrs_mtrx.vctrs_mtrx

#' @importFrom vctrs vec_arith.numeric

#' @method vec_arith.numeric vctrs_mtrx
#' @export
vec_arith.numeric.vctrs_mtrx <- vec_arith.vctrs_mtrx.vctrs_mtrx

