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
#' x <- rray(1:12, c(2, 2, 3))
#' y <- matrix(1:2)
#'
#' # True except in first 2 positions
#' x > y
#'
#' # All true
#' x >= y
#'
#' # False everywhere
#' x < y
#'
#' # False except in the first 2 positions
#' x <= y
#'
#' @name rray-compare
NULL

# ------------------------------------------------------------------------------

#' @rdname rray-compare
#' @export
`>.vctrs_rray` <- function(e1, e2) {
  rray_greater(e1, e2)
}

#' @rdname rray-compare
#' @export
rray_greater <- function(x, y) {
  cast_compare(rray__greater, x, y)
}

# ------------------------------------------------------------------------------

#' @rdname rray-compare
#' @export
`>=.vctrs_rray` <- function(e1, e2) {
  rray_greater_equal(e1, e2)
}

#' @rdname rray-compare
#' @export
rray_greater_equal <- function(x, y) {
  cast_compare(rray__greater_equal, x, y)
}

# ------------------------------------------------------------------------------

#' @rdname rray-compare
#' @export
`<.vctrs_rray` <- function(e1, e2) {
  rray_lesser(e1, e2)
}

#' @rdname rray-compare
#' @export
rray_lesser <- function(x, y) {
  cast_compare(rray__lesser, x, y)
}

# ------------------------------------------------------------------------------

#' @rdname rray-compare
#' @export
`<=.vctrs_rray` <- function(e1, e2) {
  rray_lesser_equal(e1, e2)
}

#' @rdname rray-compare
#' @export
rray_lesser_equal <- function(x, y) {
  cast_compare(rray__lesser_equal, x, y)
}

# ------------------------------------------------------------------------------

# unlike xtensor, let `==` be elementwise equality like base R

#' @rdname rray-compare
#' @export
`==.vctrs_rray` <- function(e1, e2) {
  rray_equal(e1, e2)
}

#' @rdname rray-compare
#' @export
rray_equal <- function(x, y) {
  cast_compare(rray__equal, x, y)
}

# ------------------------------------------------------------------------------

#' @rdname rray-compare
#' @export
`!=.vctrs_rray` <- function(e1, e2) {
  rray_not_equal(e1, e2)
}

#' @rdname rray-compare
#' @export
rray_not_equal <- function(x, y) {
  cast_compare(rray__not_equal, x, y)
}

# ------------------------------------------------------------------------------

cast_compare <- function(f, x, y) {

  # `NULL` are treated like logical()
  if (is.null(x)) {
    x <- logical()
  }

  if (is.null(y)) {
    y <- logical()
  }

  to <- vec_type2(x, y)

  x_cast <- rray_cast_inner(x, to)
  y_cast <- rray_cast_inner(y, to)

  res <- f(x_cast, y_cast)

  res <- set_full_dim_names(res, rray_dim_names_common(x, y))

  vec_restore(res, to)
}
