#' @export
rray_broadcast <- function(x, dim) {
  # precompute new dims
  x_dim <- vec_dim(x)
  dims <- rray_dims2(x_dim, dim)
  x_dim <- extend(x_dim, dims)
  dim(x) <- x_dim

  res <- rray_broadcast_cpp(x, dim)
  res
}
