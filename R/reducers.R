#' Calculate the sum along an axis
#'
#' @export
rray_sum <- function(x, axes = 1) {

  # currently, this only works with rrays
  # because we need the correct vec_restore to be called
  # so dim_names can be recomputed
  x <- as_rray(x)

  # only integer axes
  axes <- vec_cast(axes, integer())
  validate_axes(axes, vec_dims(x))

  # perform the reduction
  res <- rray_reducer_cpp("sum", x, as_cpp_idx(axes))

  # restore the type, but not dim_names
  res <- vec_restore(res, x)

  # TODO currently, xtensor reduces the result correctly,
  # but the resulting dimensions are reduced as well.
  # I don't think it should do this, so here we reshape
  # maybe they can include an option to allow this?
  # at the very least, do this at the cpp level
  new_dim <- vec_dim(x)
  new_dim[axes] <- 1L
  res <- rray_reshape(res, new_dim)

  # is this right?
  new_dim_names <- dim_names_extend(dim_names(x), vec_dims(x))
  res <- set_full_dim_names(res, new_dim_names)

  res
}

validate_axes <- function(axes, dims) {
  ok_vec <- axes <= dims
  ok_axes <- all(ok_vec)

  if (!ok_axes) {
    pos <- which(!ok_vec)
    pos <- glue::glue_collapse(pos, sep = ", ")
    glubort(
      "Invalid `axes`.
       The maximum value for `axes` is {dims}.
       The following `axes` positions are incorrect: {pos}."
    )
  }

  ok_vec <- axes >= 1L
  ok_axes <- all(ok_vec)

  if (!ok_axes) {
    pos <- which(!ok_vec)
    pos <- glue::glue_collapse(pos, sep = ", ")
    glubort(
      "Invalid `axes`.
       The minimum value for `axes` is 1.
       The following `axes` positions are incorrect: {pos}."
    )
  }

}
