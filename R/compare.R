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
#' @return
#'
#' The value of the comparison, with dimensions identical to the common
#' dimensions of the inputs.
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
  out <- rray__greater(x, y)
  container <- vec_ptype_container2(x, y)
  vec_cast_container(out, container)
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
  out <- rray__greater_equal(x, y)
  container <- vec_ptype_container2(x, y)
  vec_cast_container(out, container)
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
  out <- rray__lesser(x, y)
  container <- vec_ptype_container2(x, y)
  vec_cast_container(out, container)
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
  out <- rray__lesser_equal(x, y)
  container <- vec_ptype_container2(x, y)
  vec_cast_container(out, container)
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
  out <- rray__equal(x, y)
  container <- vec_ptype_container2(x, y)
  vec_cast_container(out, container)
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
  out <- rray__not_equal(x, y)
  container <- vec_ptype_container2(x, y)
  vec_cast_container(out, container)
}

# ------------------------------------------------------------------------------

#' Strictly compare arrays
#'
#' @description
#'
#' Unlike [rray_equal()] and [rray_not_equal()], these functions perform a
#' strict comparison of two arrays, and return a single logical value.
#' Specifically:
#'
#' - Broadcasting is _not_ performed here, as the shape is part of the comparison.
#'
#' - The underlying type of the values matter, and `1` is treated
#' as different from `1L`.
#'
#' - Otherwise, attributes are not compared, so dimension names are ignored.
#'
#' @param x,y Vectors, matrices, arrays, or rrays.
#'
#' @examples
#' # This is definitely true!
#' rray_all_equal(1, 1)
#'
#' # Different types!
#' rray_all_equal(1, 1L)
#'
#' # Different types!
#' rray_all_equal(rray(1), matrix(1))
#'
#' # Different shapes!
#' rray_all_equal(matrix(1), matrix(1, nrow = 2))
#'
#' # Are any values different?
#' rray_any_not_equal(c(1, 1), c(1, 2))
#'
#' # Is the shape different?
#' rray_any_not_equal(1, c(1, 2))
#'
#' # Dimension names don't matter
#' x <- matrix(1, dimnames = list("foo", "bar"))
#' rray_all_equal(x, matrix(1))
#'
#' @export
rray_all_equal <- function(x, y) {
  rray__all_equal(x, y)
}

#' @rdname rray_all_equal
#' @export
rray_any_not_equal <- function(x, y) {
  rray__any_not_equal(x, y)
}
