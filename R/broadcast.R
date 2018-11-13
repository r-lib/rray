#' @export
rray_broadcast <- function(x, dim) {
  UseMethod("rray_broadcast")
}

#' @export
rray_broadcast.default <- function(x, dim) {
  res <- rray_dims_match(x, dim)
  validate_recyclable(vec_dim(res), dim)
  res <- rray_broadcast_cpp(res, dim)
  dim_names(res) <- restore_dim_names(dim_names(x), dim)
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

  attr(x, "dim") <- dim2(x_dim, dim)$x
  x
}

# when broadcasting, cant go from [0,2] to [1,1]. Column dimension is
# downcasted
validate_recyclable <- function(from_dim, to_dim) {
  ok <- from_dim == to_dim | from_dim == 1 | to_dim == 0
  if (any(!ok)) {
    abort("Non-recyclable dimensions")
  }
}

restore_dim_names <- function(dim_names, to_dim) {

  restored_dim_names <- new_empty_dim_names(vec_size(to_dim))

  # cant use map2 bc to_dim_names could be
  # shorter than x_dim (i.e. we added a dimension)

  for(i in seq_along(dim_names)) {

    nms <- dim_names[[i]]
    single_dim <- to_dim[i]

    if (vec_size(nms) == single_dim || single_dim == 0L) {
      restored_dim_names[[i]] <- nms
    }

  }

  restored_dim_names

}
