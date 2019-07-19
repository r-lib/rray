#' Squeeze an rray
#'
#' `rray_squeeze()` is conceptually similar to [base::drop()], but it allows
#' for the specification of specific dimensions to squeeze.
#'
#' @details
#'
#' The dimension name handling of `rray_squeeze()` is essentially identical to
#' `drop()`, but some explanation is always helpful:
#'
#' - Dimension names are removed from the axes that are squeezed. So squeezing
#' a `(2, 1, 2)` object results in a `(2, 2)` object using the dimension names
#' from the original first and third dimensions.
#'
#' - When all dimensions are squeezed, as in the case of `(1, 1, 1)`, then
#' the first dimension names that are found are the ones that are used in the
#' `(1)` result.
#'
#' @param x A vector, matrix, array or rray.
#'
#' @param axes An integer vector specifying the size 1 dimensions to drop. If
#' `NULL`, all size 1 dimensions are dropped.
#'
#' @return
#'
#' `x` with the `axes` dropped, if possible.
#'
#' @examples
#' # (10, 1) -> (10)
#' x <- rray(1:10, c(10, 1))
#' rray_squeeze(x)
#'
#' # Multiple squeezed dimensions
#' # (10, 1, 1) -> (10)
#' y <- rray_reshape(x, c(10, 1, 1))
#' rray_squeeze(y)
#'
#' # Use `axes` to specify dimensions to drop
#' # (10, 1, 1) -> drop 2 -> (10, 1)
#' rray_squeeze(y, axes = 2)
#'
#' # Dimension names are kept here
#' # (10, 1) -> (10)
#' x <- rray_set_row_names(x, letters[1:10])
#' rray_squeeze(x)
#'
#' # And they are kept here
#' # (1, 10) -> (10)
#' rray_squeeze(t(x))
#'
#' @export
rray_squeeze <- function(x, axes = NULL) {

  if (is.null(axes)) {
    dim <- rray_dim(x)
    axes <- which(dim == 1L)

    # No axes are length 1
    if (length(axes) == 0L) {
      return(x)
    }
  }

  axes <- vec_cast(axes, integer())
  validate_axes(axes, x)

  out <- rray__squeeze(x, as_cpp_idx(axes))

  vec_cast_container(out, x)
}
