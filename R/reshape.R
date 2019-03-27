#' Reshape an rray
#'
#' Reshape to a new dimension
#'
#' @inheritParams rray_broadcast
#'
#' @export
rray_reshape <- function(x, dim) {

  dim <- vec_cast(dim, integer())

  x_dim <- vec_dim(x)
  validate_reshape(x_dim, dim)

  res <- rray_reshape_impl(x, dim)

  # Actually going down in dimensions here,
  # but restore_dim_names() can handle that
  new_dim_names <- restore_dim_names(x, dim)
  res <- set_full_dim_names(res, new_dim_names)

  vec_restore(res, x)
}

rray_reshape_impl <- function(x, dim) {
  rray_op_unary_1_arg_cpp("reshape", x, dim)
}

validate_reshape <- function(from, to) {

  validate_dim(to)

  size_from <- prod(from)
  size_to   <- prod(to)

  if (size_from != size_to) {
    glubort(
      "The size you are reshaping from ({size_from}) ",
      "must be equal to the size you are reshaping to ({size_to})."
    )
  }
}

validate_dim <- function(dim) {
  if (!all(dim >= 0L)) {
    abort("`dim` must be a positive vector.")
  }
}
