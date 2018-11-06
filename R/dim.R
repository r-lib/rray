#' Find common dimensions
#'
#' @description
#'
#' * `rray_dims2()` finds the common dimensionality among two objects.
#' * `rray_dim2()` finds the common size of each dimension of two objects.
#' * `rray_dims_common()` and `rray_dim_common()` are extensions that take
#' multiple inputs.
#'
#' @details
#'
#' `rray_dims_common()` essentially takes the maximum dimensionality of the inputs. This
#' is the "common" dimensionality. This is different from recycling
#' rules. Where comparing dimensionalities of 2 and 3 would throw an error
#' with tidy recycling rules, the common dimensionality of these is really 3,
#' because the 2D object has an implicit 3rd dimension of size 1.
#'
#' `rray_dim_common()` finds the common dimensionality,
#' makes any implicit dimensions explicit, then recycles the size of each
#' dimension to a common size.
#'
#'
#' @examples
#'
#' x_1_by_4 <- matrix(1, nrow = 1, ncol = 4)
#' x_5_by_1 <- matrix(1, nrow = 5, ncol = 1)
#'
#' # these are both 2D
#' rray_dims_common(x_1_by_4, x_5_by_1)
#'
#' # recycle rows: 1 and 4 = 4
#' # recycle cols: 5 and 1 = 5
#' rray_dim_common(x_1_by_4, x_5_by_1)
#'
#' x_5_by_1_by_3 <- array(1, c(5, 1, 3))
#'
#' # recycle rows:  1 and 4 = 4
#' # recycle cols:  5 and 1 = 5
#' # recycle depth: 1 and 3 = 3
#' # (here, depth of 1 for the matrix is implicit)
#' rray_dim_common(x_1_by_4, x_5_by_1_by_3)
#'
#'
#' @name matrix-dims
NULL

rray_dims2 <- function(x_dim, y_dim, .dims = NULL) {
  if (!is.null(.dims)) {
    return(.dims)
  }
  max(vec_size(x_dim), vec_size(y_dim))
}

rray_dim2 <- function(x_dim, y_dim, .dims = NULL) {
  dims <- rray_dims2(x_dim, y_dim, .dims)
  x_dim <- extend(x_dim, dims)
  y_dim <- extend(y_dim, dims)
  map2_int(x_dim, y_dim, rray_size2)
}

#' @rdname matrix-dims
#' @export
rray_dims_common <- function(..., .dims = NULL) {
  args <- compact(list2(...))

  dim_lst <- map(args, vec_dim)
  reduce(dim_lst, rray_dims2, .dims = .dims)

}

#' @rdname matrix-dims
#' @export
rray_dim_common <- function(..., .dims = NULL) {
  args <- compact(list2(...))

  dim_lst <- map(args, vec_dim)
  reduce(dim_lst, rray_dim2, .dims = .dims)

}

# ------------------------------------------------------------------------------

# this is vctrs:::vec_size2
rray_size2 <- function (nx, ny) {
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
