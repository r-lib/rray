#' Reshape an rray
#'
#' Reshape to a new dimension
#'
#' @inheritParams rray_broadcast
#'
#' @export
rray_reshape <- function(x, dim) {
  res <- reshape_impl(x, dim)

  # Actually going down in dimensions here,
  # but restore_dim_names() can handle that
  new_dim_names <- restore_dim_names(x, vec_dim(res))
  res <- set_full_dim_names(res, new_dim_names)

  vec_restore(res, x)
}

# Reshapes, but does not try and restore
# any class or dim names (no copies there)
reshape_impl <- function(x, dim) {
  x_dim <- vec_dim(x)
  validate_reshape(x_dim, dim)
  rray_reshape_cpp(x, dim)
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
  if (!is_integerish(dim)) {
    abort("`dim` must be an integer vector.")
  }

  if (!all(dim >= 0L)) {
    abort("`dim` must be a positive vector.")
  }
}
