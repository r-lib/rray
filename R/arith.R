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
#'   rray_set_row_names(c("r1", "r2")) %>%
#'   rray_set_col_names(c("c1", "c2"))
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
  out <- rray__add(x, y)
  container <- vec_type_container2(x, y)
  vec_cast_container(out, container)
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
  out <- rray__subtract(x, y)
  container <- vec_type_container2(x, y)
  vec_cast_container(out, container)
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
  out <- rray__multiply(x, y)
  container <- vec_type_container2(x, y)
  vec_cast_container(out, container)
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
  out <- rray__divide(x, y)
  container <- vec_type_container2(x, y)
  vec_cast_container(out, container)
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
  out <- rray__pow(x, y)
  container <- vec_type_container2(x, y)
  vec_cast_container(out, container)
}

# ------------------------------------------------------------------------------

# Integer division could be improved with a custom C++ version, but it is low
# hanging fruit for now. There is not a complete xtensor function that handles
# all of the cases for this.

rray_integer_division_vctrs_wrapper <- function(x, y) {
  dim <- rray_dim_common(x, y)
  x <- rray_broadcast(x, dim)
  y <- rray_broadcast(y, dim)
  res <- vec_data(x) %/% vec_data(y)
  res <- rray_set_dim_names(res, rray_dim_names2(x, y))
  vec_cast_container(res, vec_type2(x, y))
}

# ------------------------------------------------------------------------------

#' @rdname rray_arith
#' @export
rray_identity <- function(x) {
  out <- rray__identity(x)
  container <- vec_type_container(x)
  vec_cast_container(out, container)
}

# ------------------------------------------------------------------------------

#' @rdname rray_arith
#' @export
rray_opposite <- function(x) {
  out <- rray__opposite(x)
  container <- vec_type_container(x)
  vec_cast_container(out, container)
}
