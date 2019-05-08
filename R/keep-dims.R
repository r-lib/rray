# Only used in `rray_det()` for now
keep_dims <- function(res, x, axis) {

  new_dim <- rray_dim(x)

  if (is.null(axis)) {
    new_dim[] <- 1L
  }
  else {
    new_dim[axis] <- 1L
  }

  res <- rray_reshape(res, new_dim)

  res
}
