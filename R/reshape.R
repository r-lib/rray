rray_reshape <- function(x, dim) {
  x_dim <- vec_dim(x)
  validate_reshape(x_dim, dim)
  res <- rray_reshape_cpp(x, dim)
  vec_restore(res, x)
}
