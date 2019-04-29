#' Compare arrays
#'
#' These operators compare multiple arrays together, with broadcasting. The
#' underlying functions powering the comparison operators are also exported
#' for use with base R objects.
#'
#' @param x,y Two vectors, matrices, arrays, or rrays.
#'
#' @param e1,e2 Generally, the same as `x` and `y`. See Details.
#'
#'
#' @details
#'
#' The comparison operators themselves rely on R's dispatching rules to
#' dispatch to the correct rray comparison operator. When comparing rrays with
#' base R matrices and arrays, this generally works fine. However, if you
#' compare classed objects like `factor("x") > rray(1)` then a fall through
#' method is used and a warning is thrown. There is nothing we can do about
#' this. See `?groupGeneric` for more information on this.
#'
#' @examples
#'
#' x <- rray(1:12, c(2, 2, 3))
#'
#' x > matrix(1:2)
#'
#'
#' @name rray-compare
NULL

#' @rdname rray-compare
#' @export
`>.vctrs_rray` <- function(e1, e2) {
  rray_gt(e1, e2)
}

#' @rdname rray-compare
#' @export
rray_gt <- function(x, y) {
  cast_compare(rray__gt, x, y)
}

cast_compare <- function(f, x, y) {

  # Check for common type early
  to <- vec_type2(x, y)

  x <- rray_cast_inner(x, to)
  y <- rray_cast_inner(y, to)

  res <- f(x, y)

  res <- set_full_dim_names(res, rray_dim_names_common(x, y))

  vec_restore(res, to)
}
