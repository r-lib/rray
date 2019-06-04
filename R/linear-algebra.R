#' Compute the determinant
#'
#' `rray_det()` computes the determinant of a matrix.
#'
#' @param x A square matrix to compute the determinant for.
#' Alternatively, a higher dimensional array which is treated as a stack
#' of matrices computed from the first two dimensions.
#'
#' @examples
#' x <- matrix(1:4, ncol = 2)
#'
#' # Dimensions are kept
#' rray_det(x)
#'
#' x_3d <- rray_broadcast(x, c(2, 2, 3))
#'
#' # 3D matrices are treated as stacks of 2D matrices
#' rray_det(x_3d)
#'
#' @export
rray_det <- function(x) {

  dims <- rray_dims(x)

  if (dims < 2) {
    glubort(
      "Cannot compute the determinant of a {rray_dims(x)}D object."
    )
  }

  if (dims == 2L) {
    x_split <- list(x)
  }
  else {
    # non matrix axes, split in (correct) reverse order
    axes <- rev(seq_len(dims)[-c(1L ,2L)])
    x_split <- rray_split(x, axes)
  }

  res <- map_dbl(x_split, rray_det_single)

  res <- keep_dims(res, x, c(1L, 2L))

  new_dim_names <- rray_reshape_dim_names(rray_dim_names(x), rray_dim(res))
  res <- rray_set_dim_names(res, new_dim_names)

  vec_cast_container(res, x)
}

rray_det_single <- function(x) {
  det(as_matrix(x))
}

# ------------------------------------------------------------------------------

#' @export
determinant.vctrs_rray <- function(x, logarithm = TRUE, ...) {
  determinant(as.matrix(x), logarithm = logarithm, ...)
}
