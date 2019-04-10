#' Squeeze an rray
#'
#' `rray_squeeze()` is conceptually similar to [base::drop()], but it allows
#' for the specification of specific dimensions to squeeze.
#'
#' @details
#'
#' The dimension name handling of `rray_squeeze()` is slightly different from
#' what happens with `drop()`. When a `(1, 10)` matrix is squeezed to a 1-D
#' array with dimension `(10)`, if there were _column names_ on the matrix,
#' they do not appear in the resulting 1-D array. While this might seem
#' intuitive at first, it is not consistent with the treatment of 1-D arrays
#' as 1 column matrices. On the other hand, a `(10, 1)` matrix with row names
#' will keep it's dimension names when squeezed as this is consistent with the
#' above-mentioned treatment of 1-D arrays. [base::drop()] will attempt to keep
#' names in both situations.
#'
#' @param x An rray.
#'
#' @param axes An integer vector specifying the size 1 dimensions to drop. If
#' `NULL`, all size 1 dimensions are dropped.
#'
#' @examples
#'
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
#' x <- set_row_names(x, letters[1:10])
#' rray_squeeze(x)
#'
#' # But not here
#' rray_squeeze(t(x))
#'
#' @export
rray_squeeze <- function(x, axes = NULL) {

  if (is.null(axes)) {
    dim <- vec_dim(x)
    axes <- which(dim == 1L)
  }

  axes <- vec_cast(axes, integer())
  validate_axes(axes, x)

  res <- squeeze_impl(x, axes)

  new_dim_names <- restore_dim_names(dim_names(x), vec_dim(res))
  res <- set_full_dim_names(res, new_dim_names)

  vec_restore(res, x)
}

squeeze_impl <- function(x, axes) {
  rray_op_unary_one_cpp("squeeze", x, as_cpp_idx(axes))
}
