#' @export
#' @rdname vctrs-compat
#' @method vec_arith vctrs_rray
#' @export vec_arith.vctrs_rray
vec_arith.vctrs_rray <- function(op, x, y) {
  UseMethod("vec_arith.vctrs_rray", y)
}

#' @method vec_arith.vctrs_rray default
#' @export
vec_arith.vctrs_rray.default <- function(op, x, y) {
  stop_incompatible_op(op, x, y)
}

# ------------------------------------------------------------------------------
# vctrs_rray <-> vctrs_rray

#' @method vec_arith.vctrs_rray vctrs_rray
#' @export
vec_arith.vctrs_rray.vctrs_rray <- function(op, x, y) {
  f <- rray_arith_binary_op_switch(op)
  f(x, y)
}

# ------------------------------------------------------------------------------
# Unary arith operations

#' @method vec_arith.vctrs_rray MISSING
#' @export
vec_arith.vctrs_rray.MISSING <- function(op, x, y) {
  f <- rray_arith_unary_op_switch(op)
  f(x)
}

# ------------------------------------------------------------------------------
# vctrs_rray <-> numeric / matrix / array

#' @method vec_arith.vctrs_rray numeric
#' @export
vec_arith.vctrs_rray.numeric <- vec_arith.vctrs_rray.vctrs_rray

#' @method vec_arith.numeric vctrs_rray
#' @export
vec_arith.numeric.vctrs_rray <- vec_arith.vctrs_rray.vctrs_rray

# ------------------------------------------------------------------------------
# vctrs_rray <-> logical / matrix / array

#' @method vec_arith.vctrs_rray logical
#' @export
vec_arith.vctrs_rray.logical <- vec_arith.vctrs_rray.vctrs_rray

#' @method vec_arith.logical vctrs_rray
#' @export
vec_arith.logical.vctrs_rray <- vec_arith.vctrs_rray.vctrs_rray

# ------------------------------------------------------------------------------

rray_arith_unary_op_switch <- function(op, x) {
  switch(
    op,
    "+" = rray_identity,
    "-" = rray_opposite,
    "!" = rray_logical_not,
    glubort("Unary arithmetic operation not known: {op}.")
  )
}

rray_arith_binary_op_switch <- function(op, x, y) {
  switch(
    op,
    "+" = rray_add,
    "-" = rray_subtract,
    "*" = rray_multiply,
    "/" = rray_divide,
    "&" = rray_logical_and,
    "|" = rray_logical_or,
    "^" = rray_pow,
    "%%" = rray_modulus,
    "%/%" = rray_integer_division_vctrs_wrapper,
    glubort("Binary arithmetic operation not known: {op}.")
  )
}
