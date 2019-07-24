#' Find common dimensions
#'
#' @description
#'
#' * `rray_dim()` finds the dimension of a single object.
#'
#' * `rray_dim_common()` finds the common dimensions of a set of objects.
#'
#' @details
#'
#' `rray_dim_common()` first finds the common dimensionality,
#' makes any implicit dimensions explicit, then recycles the size of each
#' dimension to a common size.
#'
#' As an example, the common dimensions of `(4, 5)` and `(4, 1, 2)` are:
#'
#' ```
#' (4, 5, 1) <- implicit 1 is made to be explicit, then recycled to 2
#' (4, 1, 2) <- the 1 in the second dimension here is recycled to 5
#' ---------
#' (4, 5, 2) <- resulting common dim
#' ```
#'
#' The resulting dimensions from `rray_dim_common()` are the dimensions that
#' are used in broadcasted operations.
#'
#' @param x An object.
#'
#' @param ... Objects to find common dimensions for.
#'
#' @return
#'
#' An integer vector containing the common dimensions.
#'
#' @seealso [rray_dim_n()]
#'
#' @examples
#' x_1_by_4 <- rray(c(1, 2, 3, 4), c(1, 4))
#' x_5_by_1 <- rray(1:5, c(5, 1))
#'
#' rray_dim(x_1_by_4)
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
#' # The dimensions of NULL are 0
#' rray_dim(NULL)
#'
#' @export
rray_dim <- function(x) {
  rray__dim(x)
}

rray_dim2 <- function(x_dim, y_dim) {
  x_dim <- vec_cast(x_dim, integer())
  y_dim <- vec_cast(y_dim, integer())
  rray__dim2(x_dim, y_dim)
}

#' @rdname rray_dim
#' @export
rray_dim_common <- function(...) {
  args <- list2(...)

  dim_lst <- map(args, rray_dim)
  reduce(dim_lst, rray_dim2)
}

# ------------------------------------------------------------------------------

#' @export
`dim<-.vctrs_rray` <- function(x, value) {
  rray_reshape(x, value)
}

# ------------------------------------------------------------------------------

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

rray_increase_dims <- function(dim, dim_n) {
  dim <- vec_cast(dim, integer())
  dim_n <- vec_cast(dim_n, integer())
  vec_assert(dim_n, size = 1L)
  rray__increase_dims(dim, dim_n)
}
