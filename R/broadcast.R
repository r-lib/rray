#' @export
rray_broadcast <- function(x, dim) {
  UseMethod("rray_broadcast")
}

#' @export
rray_broadcast.default <- function(x, dim) {
  x <- rray_dims_match(x, dim)
  res <- rray_broadcast_cpp(x, dim)
  res
}

#' @export
rray_broadcast.vctrs_rray <- function(x, dim) {
  vec_restore(rray_broadcast.default(x, dim), x)
}

#' @export
rray_broadcast.vctrs_mtrx <- function(x, dim) {
  out <- rray_broadcast.default(x, dim)

  if (length(dim) > 2) {
    vec_restore(out, new_rray(dim = c(0L, dim[-1])))
  } else {
    vec_restore(out, x)
  }

}


# Match up the dims of x with the dims of y
# by adding 1s to the dim of x and assigning it to x
# this helper is good with broadcasting
rray_dims_match <- function(x, dim) {

  dim <- vec_cast(dim, integer())

  x_dim <- vec_dim(x)

  if (vec_size(x_dim) > vec_size(dim)) {
    abort("cannot decrease dimensions of `x`")
  }

  dim(x) <- vctrs:::dim2(x_dim, dim)$x
  x
}
