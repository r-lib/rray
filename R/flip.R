#' Flip an rray along a dimension
#'
#' `rray_flip()` reverses the elements of an rray along a single dimension.
#'
#' @details
#'
#' Dimension names are flipped as well.
#'
#' @param x An rray.
#' @param axis An integer of size `1` specifying the dimension to flip.
#'
#' @examples
#'
#' x <- rray(1:10, c(5, 2))
#' x <- set_row_names(x, letters[1:5])
#' x <- set_col_names(x, c("c1", "c2"))
#'
#' rray_flip(x, 1)
#'
#' rray_flip(x, 2)
#'
#' @export
rray_flip <- function(x, axis) {

  # only integer axes
  axis <- vec_cast(axis, integer())
  validate_axis(axis, x)

  res <- rray__flip(x, as_cpp_idx(axis))

  # Reverse dim names along the specified axis
  x_dim_names <- dim_names(x)
  x_dim_names <- rev_dim_names(x_dim_names, axis)
  res <- set_full_dim_names(res, x_dim_names)

  vec_restore(res, x)
}

rev_dim_names <- function(dim_names, axis) {

  # Avoid `NULL` assignment in `dim_names[[axis]]<-`
  if (is.null(dim_names[[axis]])) {
    return(dim_names)
  }

  dim_names[[axis]] <- rev(dim_names[[axis]])

  dim_names
}
