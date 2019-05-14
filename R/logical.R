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
#' The behavior of comparing either an array with a length 0 dimension
#' or `NULL` with another array is slightly different than base R since
#' broadcasting behavior is well defined. Length 0 dimensions are not exceptions
#' to the normal broadcasting rules. Comparing dimensions of `0` and `1`, the
#' common dimension is `0` because `1` always becomes the other dimension in the
#' comparison. On the other hand, comparing dimensions `0` and `2` is an error
#' because neither are one, and they are not identical.
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
#' # NULL is treated as logical(0)
#' NULL & x
#'
#' rray_logical_and(NULL, NULL)
#'
#' # You cannot broadcast dimensions
#' # of 2 and 0. Following standard
#' # broadcasting rules, they do not
#' # match and neither are 1, so an
#' # error should be thrown
#' \dontrun{
#' x & array(logical(), c(1, 0))
#' }
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
  logical_cast_compare(rray__logical_and, x, y)
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
  logical_cast_compare(rray__logical_or, x, y)
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
  logical_cast_call(rray__logical_not, x)
}

# ------------------------------------------------------------------------------

#' @rdname rray-logical
#' @export
rray_any <- function(x, axes = NULL) {

  if (is.null(x)) {
    x <- logical()
  }

  # only integer axes
  axes <- vec_cast(axes, integer())
  validate_axes(axes, x)

  # only logicals allowed through
  x_cast <- rray_cast_inner(x, logical())

  # perform the reduction
  res <- rray__any(x_cast, as_cpp_idx(axes))

  new_dim_names <- rray_reshape_dim_names(rray_dim_names(x), rray_dim(res))
  res <- set_full_dim_names(res, new_dim_names)

  vec_restore(res, x)
}

# ------------------------------------------------------------------------------

#' @rdname rray-logical
#' @export
rray_all <- function(x, axes = NULL) {

  if (is.null(x)) {
    x <- logical()
  }

  # only integer axes
  axes <- vec_cast(axes, integer())
  validate_axes(axes, x)

  # only logicals allowed through
  x_cast <- rray_cast_inner(x, logical())

  # perform the reduction
  res <- rray__all(x_cast, as_cpp_idx(axes))

  new_dim_names <- rray_reshape_dim_names(rray_dim_names(x), rray_dim(res))
  res <- set_full_dim_names(res, new_dim_names)

  vec_restore(res, x)
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

  condition <- rray_cast_inner(condition, logical())

  to <- vec_type2(true, false, x_arg = "true", y_arg = "false")
  to_inner <- rray_type_inner(to)

  true_cast <- rray_cast_inner(true, to_inner)
  false_cast <- rray_cast_inner(false, to_inner)

  res <- rray__if_else(condition, true_cast, false_cast)

  # Common dim names of true and false
  new_dim_names <- rray_dim_names2(true, false)
  res <- set_full_dim_names(res, new_dim_names)

  vec_restore(res, to)
}

# ------------------------------------------------------------------------------

logical_cast_compare <- function(f, x, y) {

  # `NULL` are treated like logical()
  if (is.null(x)) {
    x <- logical()
  }

  if (is.null(y)) {
    y <- logical()
  }

  x_cast <- rray_cast_inner(x, logical())
  y_cast <- rray_cast_inner(y, logical())

  res <- f(x_cast, y_cast)

  res <- set_full_dim_names(res, rray_dim_names_common(x, y))

  vec_restore(res, vec_type2(x, y))
}

logical_cast_call <- function(f, x) {

  if (is.null(x)) {
    x <- logical()
  }

  res <- rray_cast_inner(x, logical())

  res <- f(res)

  res <- set_full_dim_names(res, rray_dim_names(x))

  vec_restore(res, x)
}
