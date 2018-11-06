#' Find common dimensions
#'
#' @description
#'
#' * `mtrx_dims2()` finds the common dimensionality among two objects.
#' * `mtrx_dim2()` finds the common size of each dimension of two objects.
#' * `mtrx_dims_common()` and `mtrx_dim_common()` are extensions that take
#' multiple inputs.
#'
#' @details
#'
#' `mtrx_dims2()` essentially takes the maximum dimensionality of the inputs. This
#' is the "common" dimensionality. This is different from recycling
#' rules. Where comparing dimensionalities of 2 and 3 would throw an error
#' with tidy recycling rules, the common dimensionality of these is really 3,
#' because the 2D object has an implicit 3rd dimension of size 1.
#'
#' `mtrx_dim2()` uses `mtrx_dims2()` to find the common dimensionality,
#' makes any implicit dimensions explicit, then recycles the size of each
#' dimension to a common size.
#'
#' @section Invariants:
#'
#' * `mtrx_dims2(mtrx_dims2(x, y), z) == mtrx_dims2(x, mtrx_dims2(y, z)`
#' * `mtrx_dim2(mtrx_dim2(x, y), z) == mtrx_dim2(x, mtrx_dim2(y, z)`
#'
#' @examples
#'
#' x_1_by_4 <- matrix(1, nrow = 1, ncol = 4)
#' x_5_by_1 <- matrix(1, nrow = 5, ncol = 1)
#'
#' # these are both 2D
#' mtrx_dims_common(x_1_by_4, x_5_by_1)
#'
#' # recycle rows: 1 and 4 = 4
#' # recycle cols: 5 and 1 = 5
#' mtrx_dim_common(x_1_by_4, x_5_by_1)
#'
#' x_5_by_1_by_3 <- array(1, c(5, 1, 3))
#'
#' # recycle rows:  1 and 4 = 4
#' # recycle cols:  5 and 1 = 5
#' # recycle depth: 1 and 3 = 3
#' # (here, depth of 1 for the matrix is implicit)
#' mtrx_dim_common(x_1_by_4, x_5_by_1_by_3)
#'
#'
#' @name matrix-dims
NULL

#' @rdname matrix-dims
#' @export
mtrx_dims2 <- function(x, y, .dims = NULL) {
  if (!is.null(.dims)) {
    return(.dims)
  }
  max(vec_dims(x), vec_dims(y))
}

#' @rdname matrix-dims
#' @export
mtrx_dim2 <- function(x, y, .dims = NULL) {
  dims <- mtrx_dims2(x, y, .dims)
  x_dim <- extend(vec_dim(x), dims)
  y_dim <- extend(vec_dim(y), dims)
  map2_int(x_dim, y_dim, mtrx_size2)
}

#' @rdname matrix-dims
#' @export
mtrx_dims_common <- function(..., .dims = NULL) {
  args <- compact(list2(...))
  reduce(args, mtrx_dims2, .dims = .dims)
}

#' @rdname matrix-dims
#' @export
mtrx_dim_common <- function(..., .dims = NULL) {
  args <- compact(list2(...))
  reduce(args, mtrx_dim2, .dims = .dims)
}

# ------------------------------------------------------------------------------

extend <- function(dim, dims) {
  from_dims <- length(dim)
  dim_extra <- rep(1L, times = dims - from_dims)
  c(dim, dim_extra)
}

# this is vctrs:::vec_size2
mtrx_size2 <- function (nx, ny) {
  if (nx == ny) {
    nx
  }
  else if (nx == 0L || ny == 0L) {
    0L
  }
  else if (nx == 1L) {
    ny
  }
  else if (ny == 1L) {
    nx
  }
  else {
    abort(paste0("Incompatible lengths: ", nx, ", ", ny))
  }
}
