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

rray_arith_binary_op_switch <- function(op) {
  switch(
    op,
    "+" = rray_add,
    "-" = rray_subtract,
    "*" = rray_multiply,
    "/" = rray_divide,
    "&" = rray_logical_and,
    "|" = rray_logical_or,
    "^" = rray_pow,
    "%%" = rray_arith_binary_generator(`%%`),
    "%/%" = rray_arith_binary_generator(`%/%`),
    glubort("Binary arithmetic operation not known: {op}.")
  )
}

# ------------------------------------------------------------------------------

# Integer division and modulus could be improved with a custom C++ version,
# but it is low hanging fruit for now. There is not a complete xtensor
# function that handles all of the cases for this.

rray_arith_binary_generator <- function(f) {
  function(x, y) {
    dim <- rray_dim_common(x, y)
    x <- rray_broadcast(x, dim)
    y <- rray_broadcast(y, dim)
    res <- f(vec_data(x), vec_data(y))
    res <- rray_set_dim_names(res, rray_dim_names2(x, y))
    vec_cast_container(res, vec_ptype2(x, y))
  }
}
