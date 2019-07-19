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
#' @return
#'
#' The value of the arithmetic operation, with dimensions identical to the
#' common dimensions of the input.
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
  container <- vec_ptype_container2(x, y)
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
  container <- vec_ptype_container2(x, y)
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
  container <- vec_ptype_container2(x, y)
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
  container <- vec_ptype_container2(x, y)
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
  container <- vec_ptype_container2(x, y)
  vec_cast_container(out, container)
}

# ------------------------------------------------------------------------------

#' @rdname rray_arith
#' @export
rray_identity <- function(x) {
  out <- rray__identity(x)
  container <- vec_ptype_container(x)
  vec_cast_container(out, container)
}

# ------------------------------------------------------------------------------

#' @rdname rray_arith
#' @export
rray_opposite <- function(x) {
  out <- rray__opposite(x)
  container <- vec_ptype_container(x)
  vec_cast_container(out, container)
}
