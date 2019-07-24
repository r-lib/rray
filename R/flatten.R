#' Flatten an array
#'
#' `rray_flatten()` squashes the dimensionality of `x` so that the result is a
#' 1D object.
#'
#' @details
#'
#' This function is similar to `as.vector()`, but keeps the class of the object
#' and can keep the dimension names if applicable.
#'
#' Dimension names are kept using the same rules that would be applied if you
#' would have called `rray_reshape(x, prod(rray_dim(x)))`. Essentially this
#' means that names in the first dimension are kept if `x` is either already
#' a 1D vector, or a higher dimensional object with 1's in all dimensions
#' except for the first one.
#'
#' @param x A vector, matrix, array or rray.
#'
#' @return
#'
#' A 1D object with the same container type as `x`.
#'
#' @examples
#' library(magrittr)
#'
#' x <- rray(1:10, c(5, 2))
#'
#' rray_flatten(x)
#'
#' # Dimension names are kept here
#' # (2) -> (2)
#' y <- rray(1:2) %>% rray_set_axis_names(1, letters[1:2])
#' rray_flatten(y)
#'
#' # And they are kept here
#' # (2, 1) -> (2)
#' y_one_col <- rray_reshape(y, c(2, 1))
#' rray_flatten(y_one_col)
#'
#' # But not here, since the size of the first dim changes
#' # (1, 2) -> (2)
#' y_one_row <- t(y_one_col)
#' rray_flatten(y_one_row)
#'
#' @export
rray_flatten <- function(x) {
  res <- rray__flatten(x)
  vec_cast_container(res, x)
}
