#' Reshape an rray
#'
#' Reshape to a new dimension
#'
#' @inheritParams rray_broadcast
#'
#' @export
rray_reshape <- function(x, dim) {

  dim <- vec_cast(dim, integer())

  res <- rray__reshape(x, dim)

  # Actually going down in dimensions here,
  # but restore_dim_names() can handle that
  new_dim_names <- restore_dim_names(dim_names(x), dim)
  res <- set_full_dim_names(res, new_dim_names)

  vec_restore(res, x)
}
