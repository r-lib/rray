#' Arithmetic operations
#'
#' These functions provide the implementations for their underlying infix
#' operators (i.e. `rray_add()` powers `+`). All operators apply broadcasting
#' to their input.
#'
#' @details
#'
#' In case you want to apply arithmetic operations with broadcasting to
#' purely base R objects using infix operators, custom infix functions have
#' been exported, such as `%b+%`, which will perform addition with
#' broadcasting no matter what type the input is.
#'
#' @param x,y A pair of vectors.
#'
#' @examples
#' library(magrittr)
#'
#' x <- rray(1:8, c(2, 2, 2)) %>%
#'   set_row_names(c("r1", "r2")) %>%
#'   set_col_names(c("c1", "c2"))
#'
#' y <- matrix(1:2, nrow = 1)
#'
#' # All arithmetic functions are applied with broadcasting
#' rray_add(x, y)
#'
#' # And the power `+` when any underlying input
#' # is an rray
#' x + y
#'
#' # If you happen to only have base R matrices/arrays
#' # you can use `rray_add()` or `%b+%` to get the
#' # broadcasting behavior
#' rray_add(y, matrix(1:2))
#'
#' y %b+% matrix(1:2)
#'
#' @name rray_arith
NULL

# ------------------------------------------------------------------------------

#' @rdname rray_arith
#' @export
`%b+%` <- function(x, y) {
  rray_add(x, y)
}

#' @rdname rray_arith
#' @export
rray_add <- function(x, y) {
  rray_arith_binary_base(rray__add, x, y)
}

# ------------------------------------------------------------------------------

#' @rdname rray_arith
#' @export
`%b-%` <- function(x, y) {
  rray_subtract(x, y)
}

#' @rdname rray_arith
#' @export
rray_subtract <- function(x, y) {
  rray_arith_binary_base(rray__subtract, x, y)
}

# ------------------------------------------------------------------------------

#' @rdname rray_arith
#' @export
`%b*%` <- function(x, y) {
  rray_multiply(x, y)
}

#' @rdname rray_arith
#' @export
rray_multiply <- function(x, y) {
  rray_arith_binary_base(rray__multiply, x, y)
}

# ------------------------------------------------------------------------------

#' @rdname rray_arith
#' @export
`%b/%` <- function(x, y) {
  rray_divide(x, y)
}

#' @rdname rray_arith
#' @export
rray_divide <- function(x, y) {
  rray_arith_binary_base_typed(rray__divide, x, y, numeric())
}

# ------------------------------------------------------------------------------

#' @rdname rray_arith
#' @export
`%b^%` <- function(x, y) {
  rray_pow(x, y)
}

#' @rdname rray_arith
#' @export
rray_pow <- function(x, y) {
  rray_arith_binary_base_typed(rray__pow, x, y, numeric())
}

# ------------------------------------------------------------------------------

#' @rdname rray_arith
#' @export
rray_modulus <- function(x, y) {
  rray_arith_binary_base_typed(rray__modulus, x, y, integer())
}

# ------------------------------------------------------------------------------

#' @rdname rray_arith
#' @export
rray_identity <- function(x) {
  rray_arith_unary_base(rray__identity, x)
}

# ------------------------------------------------------------------------------

#' @rdname rray_arith
#' @export
rray_opposite <- function(x) {
  rray_arith_unary_base(rray__opposite, x)
}

# ------------------------------------------------------------------------------

rray_arith_unary_base <- function(f, x) {
  res <- f(x)
  res <- set_full_dim_names(res, dim_names(x))
  vec_restore(res, x)
}

rray_arith_binary_base <- function(f, x, y) {

  args <- rray_cast_inner_common(x, y)

  res <- f(args[[1]], args[[2]])

  res <- set_full_dim_names(res, rray_dim_names2(x, y))

  vec_restore(res, vec_type2(x, y))
}

rray_arith_binary_base_typed <- function(f, x, y, type) {

  args <- list(
    rray_cast_inner(x, type),
    rray_cast_inner(y, type)
  )

  res <- f(args[[1]], args[[2]])

  res <- set_full_dim_names(res, rray_dim_names2(x, y))

  vec_restore(res, vec_type2(x, y))
}
