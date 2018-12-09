#' Reducers
#'
#'
#'
#' @export
reducer_dbl <- function(.x, .f, axes = 1) {
  reducer_impl(.x, .f, axes = axes, type = "double")
}

#' @export
reducer_int <- function(.x, .f, axes = 1) {
  reducer_impl(.x, .f, axes = axes, type = "integer")
}

#' @export
reducer_lgl <- function(.x, .f, axes = 1) {
  reducer_impl(.x, .f, axes = axes, type = "logical")
}

reducer_impl <- function(.x, .f, axes, type) {

  .x <- as_rray(.x)
  .f <- rlang::as_function(.f)

  # only integer axes
  axes <- vec_cast(axes, integer())
  validate_axes(axes, vec_dims(.x))

  # perform the reduction
  res <- rray_custom_reducer_cpp(.x, .f, as_cpp_idx(axes), type)

  # restore the type, but not dim_names
  res <- vec_restore(res, .x)

  new_dim <- vec_dim(.x)
  new_dim[axes] <- 1L
  res <- rray_reshape(res, new_dim)

  new_dim_names <- restore_dim_names(.x, new_dim)
  res <- set_full_dim_names(res, new_dim_names)

  res
}
