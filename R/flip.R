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

  if (rlang::is_missing(axis)) {
    abort("`axis` must be supplied.")
  }

  axis <- vec_cast(axis, integer())

  axis_size <- vec_size(axis)
  if (axis_size != 1) {
    glubort("`axis` must have length 1, not {axis_size}.")
  }

  dims <- vec_dims(x)

  if (axis > dims) {
    glubort("`axis` for this `x` can be at most {dims}, not {axis}.")
  }

  res <- rray_flip_impl(x, axis)
  res <- rray_restore(res, x)

  # Reverse dim names along the specified axis
  x_dim_names <- dim_names(x)

  x_dim_names <- rev_dim_names(x_dim_names, axis)

  res <- set_full_dim_names(res, x_dim_names)

  res
}

rray_flip_impl <- function(x, axis) {
  rray_flip_cpp(x, as_cpp_idx(axis))
}

rev_dim_names <- function(dim_names, axis) {

  # Avoid `NULL` assignment in `dim_names[[axis]]<-`
  if (is.null(dim_names[[axis]])) {
    return(dim_names)
  }

  dim_names[[axis]] <- rev(dim_names[[axis]])

  dim_names
}
