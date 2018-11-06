compact <- function (x) {
  is_null <- map_lgl(x, is.null)
  x[!is_null]
}

glubort <- function(..., .sep = "", .envir = parent.frame()) {
  abort(glue::glue(..., .sep = .sep, .envir = .envir))
}

extend <- function(dim, dims) {
  from_dims <- length(dim)
  dim_extra <- rep(1L, times = dims - from_dims)
  c(dim, dim_extra)
}

# modify dim to pre-match broadcasting operation
extend2 <- function(x, y) {
  x_dim <- vec_dim(x)
  y_dim <- vec_dim(y)
  dims <- rray_dims2(x_dim, y_dim)
  dim(x) <- extend(x_dim, dims)
  dim(y) <- extend(y_dim, dims)
  list(x, y)
}
