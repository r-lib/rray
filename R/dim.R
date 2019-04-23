#' Find common dimensions
#'
#' @description
#'
#' * `rray_dims_common()` finds the common dimensionality
#' among a set of objects.
#' * `rray_dim_common()` finds the common size of each dimension
#' of a set of objects.
#'
#' @details
#'
#' `rray_dims_common()` essentially takes the maximum dimensionality of the
#' inputs. This is the "common" dimensionality. Below is an example of
#' comparing a 4x5 matrix and a 4x1x2 array to get the common dimensionality.
#'
#' ```
#' (4, 5,  ) <- dimensionality of 2, but a 1 is implicit to match dimensionality
#' (4, 1, 2) <- dimensionality of 3
#' ---------
#' 3         <- resulting dimensionality is 3
#' ```
#'
#' `rray_dim_common()` finds the common dimensionality,
#' makes any implicit dimensions explicit, then recycles the size of each
#' dimension to a common size. With the above example, with implicit `1`
#' is made to be explicit, and then the dimensions are compared:
#'
#' ```
#' (4, 5, 1) <- implicit 1 is made to be explicit, then recycled to 2
#' (4, 1, 2) <- the 1 in the second dimension here is recycled to 5
#' ---------
#' (4, 5, 2) <- resulting common dim
#' ```
#'
#' The resulting dimensions from `rray_dim_common()` are the dimensions that
#' are used in broadcasted arithmetic operations.
#'
#' @param ... Objects to find common dimension/dimensionality for.
#'
#' @examples
#'
#' x_1_by_4 <- rray(c(1, 2, 3, 4), c(1, 4))
#' x_5_by_1 <- rray(1:5, c(5, 1))
#'
#' # these are both 2D
#' rray_dims_common(x_1_by_4, x_5_by_1)
#'
#' # recycle rows: 1 VS 5 = 5
#' # recycle cols: 4 VS 1 = 4
#' rray_dim_common(x_1_by_4, x_5_by_1)
#'
#' x_5_by_1_by_3 <- rray(1, c(5, 1, 3))
#'
#' # recycle rows:    5 VS 1 = 5
#' # recycle cols:    4 VS 1 = 4
#' # recycle 3rd dim: 1 VS 3 = 3
#' # (here, 3rd dim of 1 for the matrix is implicit)
#' rray_dim_common(x_1_by_4, x_5_by_1_by_3)
#'
#' @name common-dim
NULL

rray_dims2 <- function(x_dims, y_dims) {
  max(x_dims, y_dims)
}

rray_dim2 <- function(x_dim, y_dim) {
  dims_matched <- dim2(x_dim, y_dim)
  map2_int(dims_matched$x, dims_matched$y, rray_size2)
}

#' @rdname common-dim
#' @export
rray_dims_common <- function(...) {
  args <- compact(list2(...))

  dims_lst <- map(args, vec_dims)
  reduce(dims_lst, rray_dims2)
}

#' @rdname common-dim
#' @export
rray_dim_common <- function(...) {
  args <- compact(list2(...))

  dim_lst <- map(args, vec_dim)
  reduce(dim_lst, rray_dim2)

}

#' @export
`dim<-.vctrs_rray` <- function(x, value) {
  rray_reshape(x, value)
}

# vctrs:::dim2
# x and y are dim values
dim2 <- function(x, y) {
  nx <- length(x)
  ny <- length(y)

  if (nx == ny) {
    list(x = x, y = y)
  } else if (nx < ny) {
    list(x = c(x, rep(1L, ny - nx)), y = y)
  } else {
    list(x = x, y = c(y, rep(1L, nx - ny)))
  }
}

rray_increase_dims <- function(dim, dims) {
  dim <- vec_cast(dim, integer())
  dims <- vec_cast(dims, integer())
  vec_assert(dims, size = 1L)
  rray__increase_dims(dim, dims)
}
