#' Reshape an rray
#'
#' Reshape to a new dimension
#'
#' @inheritParams rray_broadcast
#'
#' @export
rray_reshape <- function(x, dim) {

  dim <- vec_cast(dim, integer())

  validate_reshape(x, dim)

  res <- rray_reshape_impl(x, dim)

  # Actually going down in dimensions here,
  # but restore_dim_names() can handle that
  new_dim_names <- restore_dim_names(dim_names(x), dim)
  res <- set_full_dim_names(res, new_dim_names)

  vec_restore(res, x)
}

rray_reshape_impl <- function(x, dim) {
  rray_op_unary_one_cpp("reshape", x, dim)
}

validate_reshape <- function(x, to) {

  if (is.null(x)) {
    return(invisible(x))
  }

  validate_dim(to)

  from <- vec_dim(x)

  size_from <- prod(from)
  size_to   <- prod(to)

  if (size_from != size_to) {
    glubort(
      "The size you are reshaping from ({size_from}) ",
      "must be equal to the size you are reshaping to ({size_to})."
    )
  }

  invisible(x)
}

validate_dim <- function(dim) {
  if (!all(dim >= 0L)) {
    abort("`dim` must be a positive vector.")
  }
}
