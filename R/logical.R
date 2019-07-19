#' Logical operators
#'
#' These functions perform logical operations on arrays, with broadcasting. They
#' power the logical operators of `&`, `|`, and `!` with rrays, but are
#' also exported for use with base R objects.
#'
#' @param x,y Vectors, matrices, arrays, or rrays.
#'
#' @param axes An integer vector specifying the axes to reduce over.
#' `1` reduces the number of rows to `1`, performing the reduction along the
#' way. `2` does the same, but with the columns, and so on for higher
#' dimensions. The default reduces along all axes.
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
#' The behavior of comparing an array with a length 0 dimension
#' with another array is slightly different than base R since
#' broadcasting behavior is well defined. Length 0 dimensions are not exceptions
#' to the normal broadcasting rules. Comparing dimensions of `0` and `1`, the
#' common dimension is `0` because `1` always becomes the other dimension in the
#' comparison. On the other hand, comparing dimensions `0` and `2` is an error
#' because neither are `1`, and they are not identical.
#'
#' @return
#'
#' The value of the logical comparison, with broadcasting.
#'
#' `rray_any()` and `rray_all()` return a logical object with the same shape
#' as `x` everywhere except along `axes`, which have been reduced to size 1.
#'
#' @examples
#' x <- rray(TRUE, c(2, 2, 3))
#' y <- matrix(c(TRUE, FALSE))
#'
#' # `TRUE` wherever `y` is broadcasted to be `TRUE`
#' x & y
#'
#' # ---------------------------------------------------------------------------
#' # Behavior with edge cases
#'
#' x <- rray(TRUE, c(1, 2))
#'
#' # The common dim is (0, 2)
#' logical() & x
#'
#' # You can have empty arrays with shape
#' # The common dim is (0, 2, 2)
#' y <- array(logical(), c(0, 1, 2))
#' x & y
#'
#' # You cannot broadcast dimensions
#' # of 2 and 0. Following standard
#' # broadcasting rules, they do not
#' # match and neither are 1, so an
#' # error should be thrown
#' try(x & array(logical(), c(1, 0)))
#'
#' @name rray-logical
NULL

# ------------------------------------------------------------------------------

#' @rdname rray-logical
#' @export
rray_logical_and <- function(x, y) {
  out <- rray__logical_and(x, y)
  container <- vec_ptype_container2(x, y)
  vec_cast_container(out, container)
}

# ------------------------------------------------------------------------------

#' @rdname rray-logical
#' @export
rray_logical_or <- function(x, y) {
  out <- rray__logical_or(x, y)
  container <- vec_ptype_container2(x, y)
  vec_cast_container(out, container)
}

# ------------------------------------------------------------------------------

#' @rdname rray-logical
#' @export
rray_logical_not <- function(x) {
  out <- rray__logical_not(x)
  container <- vec_ptype_container(x)
  vec_cast_container(out, container)
}

# ------------------------------------------------------------------------------

#' @rdname rray-logical
#' @export
rray_any <- function(x, axes = NULL) {

  # only integer axes
  axes <- vec_cast(axes, integer())
  validate_axes(axes, x)

  # perform the reduction
  res <- rray__any(x, as_cpp_idx(axes))

  vec_cast_container(res, x)
}

# ------------------------------------------------------------------------------

#' @rdname rray-logical
#' @export
rray_all <- function(x, axes = NULL) {

  # only integer axes
  axes <- vec_cast(axes, integer())
  validate_axes(axes, x)

  # perform the reduction
  res <- rray__all(x, as_cpp_idx(axes))

  vec_cast_container(res, x)
}

# ------------------------------------------------------------------------------

#' Conditional selection
#'
#' `rray_if_else()` is like `ifelse()`, but works with matrices and arrays,
#' and fully supports broadcasting between the three inputs. Before the
#' operation is applied, `condition` is cast to a logical, and `true` and
#' `false` are cast to their common type.
#'
#' @param condition A logical vector, matrix, array of rray.
#'
#' @param true A vector, matrix, array, or rray. This is the value in the result
#' when `condition` is `TRUE`.
#'
#' @param false A vector, matrix, array, or rray. This is the value in the
#' result when `condition` is `FALSE`.
#'
#' @details
#'
#' The dimension names of the output are taken as the common names of `true`
#' and `false`.
#'
#' @examples
#'
#' cond <- c(TRUE, FALSE)
#'
#' true <- array(
#'   1:2,
#'   dimnames = list(c("r1", "r2"))
#' )
#'
#' false <- rray(
#'   c(3, 4, 5, 6),
#'   dim = c(2, 2),
#'   dim_names = list(c("rr1", "rr2"), c("c1", "c2"))
#' )
#'
#' # - All inputs are broadcast to a common
#' #   shape of (2, 2).
#' # - The first row of the output comes from
#' #   `true`, the second row comes from `false`.
#' # - The names come from both `true` and `false`.
#' rray_if_else(cond, true, false)
#'
#' @export
rray_if_else <- function(condition, true, false) {
  out <- rray__if_else(condition, true, false)
  container <- vec_ptype_container2(true, false)
  vec_cast_container(out, container)
}
