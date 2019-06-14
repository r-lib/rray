rray_shape <- function(x) {
  # removes vctrs is.object() restriction
  rray_dim(x)[-1]
}

rray_shape2 <- function(x, y) {
  shape <- dim2(rray_shape(x), rray_shape(y))
  map2_int(shape$x, shape$y, rray_size2)
}

# this is vctrs:::vec_size2
rray_size2 <- function (nx, ny) {
  if (nx == ny) {
    nx
  }
  else if (nx == 0L || ny == 0L) {
    0L
  }
  else if (nx == 1L) {
    ny
  }
  else if (ny == 1L) {
    nx
  }
  else {
    abort(paste0("Incompatible lengths: ", nx, ", ", ny))
  }
}
