#' Split an array along axes
#'
#' `rray_split()` splits `x` into equal pieces, splitting along the `axes`.
#'
#' @param x A vector, matrix, array, or rray.
#'
#' @param axes An integer vector. The axes to split on. The default splits
#' along all axes.
#'
#' @details
#'
#' `rray_split()` works by splitting along the `axes`. The result is a list
#' of sub arrays, where the `axes` of each sub array have been reduced to
#' length 1. This is consistent with how reducers like [rray_sum()] work. As
#' an example, splitting a `(2, 3, 5)` array along `axes = c(2, 3)` would
#' result in a list of 15 (from `3 * 5`) sub arrays, each with
#' shape `(2, 1, 1)`.
#'
#' @return
#'
#' A list of sub arrays of type `x`.
#'
#' @examples
#'
#' x <- matrix(1:8, ncol = 2)
#'
#' # Split the rows
#' # (4, 2) -> (1, 2)
#' rray_split(x, 1)
#'
#' # Split the columns
#' # (4, 2) -> (4, 1)
#' rray_split(x, 2)
#'
#' # Split along multiple dimensions
#' # (4, 2) -> (1, 1)
#' rray_split(x, c(1, 2))
#'
#' # The above split is the default behavior
#' rray_split(x)
#'
#' # You can technically split with a size 0 `axes`
#' # argument, which essentially requests no axes
#' # to be split and is the same as `list(x)`
#' rray_split(x, axes = integer(0))
#'
#' # ---------------------------------------------------------------------------
#' # 4 dimensional example
#'
#' x_4d <- rray(
#'   x = 1:16,
#'   dim = c(2, 2, 2, 2),
#'   dim_names = list(
#'     c("r1", "r2"),
#'     c("c1", "c2"),
#'     c("d1", "d2"),
#'     c("e1", "e2")
#'   )
#' )
#'
#' # Split along the 1st dimension (rows)
#' # (2, 2, 2, 2) -> (1, 2, 2, 2)
#' rray_split(x_4d, 1)
#'
#' # Split along columns
#' # (2, 2, 2, 2) -> (2, 1, 2, 2)
#' rray_split(x_4d, 2)
#'
#' # Probably the most useful thing you might do
#' # is use this to split the 4D array into a set
#' # of 4 2D matrices.
#' rray_split(x_4d, c(3, 4))
#'
#' @export
rray_split <- function(x, axes = NULL) {

  axes <- vec_cast(axes, integer())
  validate_axes(axes, x)

  if (is_null(axes)) {
    axes <- seq_len(rray_dims(x))
  }

  res <- rray__split(x, as_cpp_idx(axes))

  res <- map(res, vec_cast_container, to = x)

  res
}

split_dim_names <- function(dim_names, axes, n) {

  # Prep container with existing one
  # (some names might already be correct)
  new_dim_names <- rep_len(list(dim_names), prod(n))

  for (i in seq_along(axes)) {
    axis <- axes[i]
    n_i <- n[i]

    # Nothing to do if there aren't existing names
    axis_nms <- dim_names[[axis]]
    if (is.null(axis_nms)) {
      next
    }

    # Split the existing axis names into `n_i` pieces
    by <- length(axis_nms) / n_i
    split_axis_nms <- new_list(n_i)
    k <- 1L
    for (j in seq_len(n_i)) {
      split_axis_nms[[j]] <- axis_nms[k:(k + by - 1)]
      k <- k + by
    }

    # Assign the split names to their correct location
    # in the output
    nms_idx <- 1L
    for (j in seq_along(new_dim_names)) {
      new_dim_names[[j]][[axis]] <- split_axis_nms[[nms_idx]]

      if (nms_idx == n_i) {
        nms_idx <- 1L
      }
      else {
        nms_idx <- nms_idx + 1L
      }
    }

  }

  new_dim_names
}
