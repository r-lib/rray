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
#' same, but with the columns, and so on for higher dimensions. The default
#' reduces along all axes.
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
#' # Reduce along all axes, but keep dimensions
#' rray_sum(x)
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
rray_sum <- function(x, axes = NULL) {
  rray_reducer_base(rray__sum, x, axes)
}

#' Calculate the product along an axis
#'
#' `rray_prod()` computes the product along a given axis or axes. The
#' dimensionality of `x` is retained in the result.
#'
#' @inherit rray_sum details
#'
#' @inheritParams rray_sum
#'
#' @examples
#'
#' x <- rray(1:10, c(5, 2))
#'
#' rray_prod(x)
#'
#' rray_prod(x, 1)
#'
#' rray_prod(x, 2)
#'
#' @export
#' @family reducers
rray_prod <- function(x, axes = NULL) {
  rray_reducer_base(rray__prod, x, axes)
}

#' Calculate the mean along an axis
#'
#' `rray_mean()` computes the mean along a given axis or axes. The
#' dimensionality of `x` is retained in the result.
#'
#' @inherit rray_sum details
#'
#' @inheritParams rray_sum
#'
#' @examples
#'
#' x <- rray(1:10, c(5, 2))
#'
#' rray_mean(x)
#'
#' rray_mean(x, 1)
#'
#' rray_mean(x, 2)
#'
#' @export
#' @family reducers
rray_mean <- function(x, axes = NULL) {
  rray_reducer_base(rray__mean, x, axes)
}

#' Calculate the maximum along an axis
#'
#' `rray_max()` computes the maximum along a given axis or axes. The
#' dimensionality of `x` is retained in the result.
#'
#' @inherit rray_sum details
#'
#' @inheritParams rray_sum
#'
#' @examples
#'
#' x <- rray(1:10, c(5, 2))
#'
#' rray_max(x)
#'
#' rray_max(x, 1)
#'
#' rray_max(x, 2)
#'
#' @export
#' @family reducers
rray_max <- function(x, axes = NULL) {
  rray_reducer_base(rray__max, x, axes)
}

#' Calculate the minimum along an axis
#'
#' `rray_min()` computes the minimum along a given axis or axes. The
#' dimensionality of `x` is retained in the result.
#'
#' @inherit rray_sum details
#'
#' @inheritParams rray_sum
#'
#' @examples
#'
#' x <- rray(1:10, c(5, 2))
#'
#' rray_min(x)
#'
#' rray_min(x, 1)
#'
#' rray_min(x, 2)
#'
#' @export
#' @family reducers
rray_min <- function(x, axes = NULL) {
  rray_reducer_base(rray__min, x, axes)
}

# ------------------------------------------------------------------------------

rray_reducer_base <- function(f, x, axes) {
  axes <- vec_cast(axes, integer())
  validate_axes(axes, x)

  out <- f(x, as_cpp_idx(axes))

  vec_cast_container(out, x)
}

# ------------------------------------------------------------------------------

validate_axis <- function(axis, x, dims = NULL) {
  validate_axes(axis, x, n = 1L, nm = "axis", dims = dims)
}

# `dims` argument is used as an override in a few cases (rray_expand_dims())
validate_axes <- function(axes, x, n = NULL, nm = "axes", dims = NULL) {

  if (is.null(axes)) {
    return(invisible(NULL))
  }

  if (is.null(x)) {
    return(invisible(NULL))
  }

  if (is.null(dims)) {
    dims <- rray_dim_n(x)
  }

  if (is.null(n)) {
    n <- dims
  }

  ok_axes <- vec_size(axes) <= n

  if (!ok_axes) {
    glubort(
      "Invalid `{nm}`.
       The maximum size of `{nm}` is {n}.
       The provided size of `{nm}` is {vec_size(axes)}."
    )
  }

  ok_vec <- axes <= dims
  ok_axes <- all(ok_vec)

  if (!ok_axes) {
    pos <- which(!ok_vec)
    pos <- glue::glue_collapse(pos, sep = ", ")
    glubort(
      "Invalid `{nm}`.
       The maximum value for `{nm}` is {dims}.
       The following `{nm}` positions are incorrect: {pos}."
    )
  }

  ok_vec <- axes >= 1L
  ok_axes <- all(ok_vec)

  if (!ok_axes) {
    pos <- which(!ok_vec)
    pos <- glue::glue_collapse(pos, sep = ", ")
    glubort(
      "Invalid `{nm}`.
       The minimum value for `{nm}` is 1.
       The following `{nm}` positions are incorrect: {pos}."
    )
  }

  invisible(axes)
}
