#' Logical operators
#'
#' These operators perform logical operations on arrays, with broadcasting. The
#' underlying functions powering the logical operations are also exported
#' for use with base R objects.
#'
#' @param x,y Two vectors, matrices, arrays, or rrays.
#'
#' @param e1,e2 Generally, the same as `x` and `y`. See Details.
#'
#' @param ... A single rray. An error is currently thrown if more than one
#' input is passed here.
#'
#' @param na.rm Should `NA` values be removed? Currently only `FALSE` is
#' allowed.
#'
#' @details
#'
#' The operators themselves rely on R's dispatching rules to
#' dispatch to the correct rray logical operator. When comparing rrays with
#' base R matrices and arrays, this generally works fine. However, if you
#' compare classed objects like `factor("x") & rray(1)` then a fall through
#' error is thrown. There is nothing we can do about
#' this. See `?groupGeneric` for more information on this.
#'
#' @examples
#' x <- rray(TRUE, c(2, 2, 3))
#' y <- matrix(c(TRUE, FALSE))
#'
#' # `TRUE` wherever `y` is broadcasted to be `TRUE`
#' x & y
#'
#'
#' @name rray-logical
NULL

# ------------------------------------------------------------------------------

#' @rdname rray-logical
#' @export
`&.vctrs_rray` <- function(e1, e2) {
  rray_logical_and(e1, e2)
}

#' @rdname rray-logical
#' @export
rray_logical_and <- function(x, y) {
  cast_compare(rray__logical_and, x, y)
}

# ------------------------------------------------------------------------------

#' @rdname rray-logical
#' @export
`|.vctrs_rray` <- function(e1, e2) {
  rray_logical_or(e1, e2)
}

#' @rdname rray-logical
#' @export
rray_logical_or <- function(x, y) {
  cast_compare(rray__logical_or, x, y)
}

# ------------------------------------------------------------------------------

#' @rdname rray-logical
#' @export
`!.vctrs_rray` <- function(x) {
  rray_logical_not(x)
}

#' @rdname rray-logical
#' @export
rray_logical_not <- function(x) {
  cast_inner_restore(rray__logical_not, x, logical())
}

# ------------------------------------------------------------------------------

# `any()` should always be on a flattened version of the input to maintain
# backwards compat with base R. `rray_any()` should only handle 1 input
# but should be able to look along an axis.
# TODO - Think more about this.

#' @rdname rray-logical
#' @export
`any.vctrs_rray` <- function(..., na.rm = FALSE) {

  if (!identical(na.rm, FALSE)) {
    abort("`na.rm` currently must be `FALSE` in `any()` for rrays.")
  }

  x <- map(list2(...), as.vector)
  x <- vec_c(!!! x)

  vctrs::vec_math_base("any", x)
}

#' @rdname rray-logical
#' @export
rray_any <- function(x) {
  cast_inner_restore(rray__any, x, logical())
}

# ------------------------------------------------------------------------------

cast_inner_restore <- function(f, x, to) {
  res <- rray_cast_inner(x, to)
  res <- f(res)
  res <- set_full_dim_names(res, dim_names(x))
  vec_restore(res, x)
}
