# TODO currently, xtensor reduces the result correctly,
# but the resulting dimensions are reduced as well.
# I don't think it should do this, so here we reshape
# maybe they can include an option to allow this?
# at the very least, do this at the cpp level
# Hopefully this will get added soon so we can default
# keepdims = True:
# https://github.com/QuantStack/xtensor-r/issues/75
keep_dims <- function(res, x, axis) {

  new_dim <- vec_dim(x)

  if (is.null(axis)) {
    new_dim[] <- 1L
  }
  else {
    new_dim[axis] <- 1L
  }

  res <- rray_reshape(res, new_dim)

  res
}
