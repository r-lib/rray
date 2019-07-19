# %*% is an S4 generic - but dispatches internally.
# Because of this line:
# https://github.com/wch/r-source/blob/1c4cba97b88d233d8a1e41c6f7d1f5e8cfcab359/src/main/array.c#L1231-L1239
# it won't dispatch on an S3 even if we define setOldClass() for it
# unless the other input is S4. This is obviously undesirable.
# There isn't much we can do besides provide a `rray_dot()` function
# and encourage use of that instead

#' Matrix multiplication
#'
#' `rray_dot()` works exactly like the base R function, `%*%`, but preserves
#' the rray class where applicable. For the exact details of how 1D objects
#' are promoted to 2D objects, see [%*%].
#'
#' @details
#'
#' Due to some peculiarities with how `%*%` dispatches with S3 objects, calling
#' `%*%` directly with an rray will compute the matrix multiplication correctly,
#' but the class will be lost. `rray_dot()` ensures that the rray class is
#' maintained.
#'
#' @param x,y Arrays or rrays that are either 1D or 2D.
#'
#' @return
#'
#' The result of the matrix multiplication of `x` and `y`. See `%*%` for the
#' exact details. The common type of `x` and `y` will be preserved.
#'
#' @examples
#' rray_dot(1:5, 1:5)
#'
#' rray_dot(rray(1:5), 1:5)
#'
#' @export
rray_dot <- function(x, y) {

  x_dim_n <- rray_dim_n(x)
  if (x_dim_n > 2L) {
    glubort("`x` must have a dimensionality of 1 or 2, not {x_dim_n}.")
  }

  y_dim_n <- rray_dim_n(y)
  if (y_dim_n > 2L) {
    glubort("`y` must have a dimensionality of 1 or 2, not {y_dim_n}.")
  }

  out <- x %*% y

  container <- vec_ptype_container2(x, y)
  vec_cast_container(out, container)
}
