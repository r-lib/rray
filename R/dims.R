#' Find common dimensionalities
#'
#' @description
#'
#' * `rray_dims()` finds the dimensionality of a single object.
#'
#' * `rray_dims_common()` finds the common dimensionality
#' among a set of objects.
#'
#' @details
#'
#' `rray_dims_common()` essentially takes the maximum dimensionality of the
#' inputs. This is the "common" dimensionality. Below is an example of
#' comparing a `(4, 5)` matrix and a `(4, 1, 2)` array to get the common
#' dimensionality.
#'
#' ```
#' (4, 5,  ) | 2   <- dimensionality of 2
#' (4, 1, 2) | 3   <- dimensionality of 3
#' -------------
#'           | 3   <- maximum of the two is 3
#' ```
#'
#' A subtle point is that the dimensions do not have to be broadcastable
#' to still find a common dimensionality. So a `(2, 1)` matrix and a `(3, 1)`
#' matrix would not be broadcastable, but have a common dimensionality of `2`.
#'
#' @param x An object.
#'
#' @param ... Objects to find common dimensionality for.
#'
#' @examples
#' x_1_by_4 <- rray(c(1, 2, 3, 4), c(1, 4))
#' x_5_by_1 <- rray(1:5, c(5, 1))
#'
#' rray_dims(x_1_by_4)
#'
#' # these are both 2D
#' rray_dims_common(x_1_by_4, x_5_by_1)
#'
#' # 1D and 2D have a common dimensionality of 2D
#' rray_dims_common(x_1_by_4, 1)
#'
#' # NULL has a dimensionality of 1
#' rray_dims(NULL)
#'
#' @export
rray_dims <- function(x) {
  rray__dims(x)
}

rray_dims2 <- function(x_dims, y_dims) {
  x_dims <- vec_cast(x_dims, integer())
  y_dims <- vec_cast(y_dims, integer())

  if (is.null(x_dims)) {
    abort("`x_dims` cannot be `NULL`.")
  }

  if (is.null(y_dims)) {
    abort("`y_dims` cannot be `NULL`.")
  }

  rray__dims2(x_dims, y_dims)
}

#' @rdname rray_dims
#' @export
rray_dims_common <- function(...) {
  args <- list2(...)

  if (length(args) == 0L) {
    return(NULL)
  }

  dims_lst <- map_int(args, rray_dims)
  reduce(dims_lst, rray_dims2)
}
