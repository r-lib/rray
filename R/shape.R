rray_shape <- function(x) {
  # removes vctrs is.object() restriction
  rray_dim(x)[-1]
}

rray_shape2 <- function(x, y) {
  rray_dim2(rray_shape(x), rray_shape(y))
}
