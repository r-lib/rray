# ------------------------------------------------------------------------------
# Base reducer implementation

rray_reducer_base <- function(reducer, x, axes) {

  # currently, this only works with rrays
  # because we need the correct vec_restore to be called
  # so dim_names can be recomputed
  x <- as_rray(x)

  # only integer axes
  axes <- vec_cast(axes, integer())
  validate_axes(axes, vec_dims(x))

  # perform the reduction
  res <- rray_reducer_cpp(reducer, x, as_cpp_idx(axes))

  # restore the type, but not dim_names
  res <- vec_restore(res, x)

  # TODO currently, xtensor reduces the result correctly,
  # but the resulting dimensions are reduced as well.
  # I don't think it should do this, so here we reshape
  # maybe they can include an option to allow this?
  # at the very least, do this at the cpp level
  # Hopefully this will get added soon so we can default
  # keepdims = True:
  # https://github.com/QuantStack/xtensor-r/issues/75
  new_dim <- vec_dim(x)
  new_dim[axes] <- 1L
  res <- rray_reshape(res, new_dim)

  new_dim_names <- restore_dim_names(x, new_dim)
  res <- set_full_dim_names(res, new_dim_names)

  res

}

# ------------------------------------------------------------------------------
# Reducers

#' Calculate the sum along an axis
#'
#' `rray_sum()` computes the sum along a given axis or axes. The dimensionality
#' of `x` is retained in the result.
#'
#' Currently, objects are coerced to `rray`s before the reduction is applied.
#'
#' @param x A vector, matrix, or array to reduce.
#' @param axes An integer vector specifying the axes to reduce over. `1` reduces
#' the number of rows to 1, performing the reduction along the way. `2` does the
#' same, but with the columns, and so on for higher dimensions.
#'
#' @examples
#'
#' x <- rray(1:10, c(5, 2))
#'
#' # Reduce the number of rows to 1,
#' # summing along the way
#' rray_sum(x, 1)
#'
#' # Reduce the number of columns to 1,
#' # summing along the way
#' rray_sum(x, 2)
#'
#' # Column-wise proportions
#' x / rray_sum(x, 1)
#'
#' # Row-wise proportions
#' x / rray_sum(x, 2)
#'
#' # Reducing over multiple axes
#' # This reduces over the rows and columns
#' # of each mini-matrix in the 3rd dimension
#' y <- rray(1:24, c(2, 3, 4))
#' rray_sum(y, c(1, 2))
#'
#' @export
#' @family reducers
rray_sum <- function(x, axes = 1) {
  rray_reducer_base("sum", x, axes = axes)
}

# ------------------------------------------------------------------------------
# Helpers

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
