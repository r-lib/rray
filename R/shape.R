rray_shape <- function(x) {
  # removes vctrs is.object() restriction
  rray_dim(x)[-1]
}

rray_shape2 <- function(x, y) {
  shape <- dim2(rray_shape(x), rray_shape(y))
  map2_int(shape$x, shape$y, rray_size2)
}

