#' Split an array along an axis
#'
#' `rray_split()` splits `x` into `n` equal pieces, splitting along an `axis`.
#'
#' @param x A vector, matrix, array, or rray.
#'
#' @param axes An integer vector. The axes to split on.
#'
#' @param n An integer vector, or `NULL`. By default, this is computed as the
#' dimension size of each axis specified in `axes`. If not `NULL`, it should
#' be the same length as `axes`.
#'
#' @return
#'
#' A list of equal pieces of type `x`.
#'
#' @examples
#'
#' x <- matrix(1:8, ncol = 2)
#'
#' # Split the rows
#' rray_split(x, 1)
#'
#' # Split the columns
#' rray_split(x, 2)
#'
#' # Split along multiple dimensions
#' # To have them in the most interpretable
#' # order, split backwards along the dimensions
#' # (i.e. 2 then 1)
#' rray_split(x, c(2, 1))
#'
#' # Split along rows in chunks of 2
#' rray_split(x, 1, 2)
#'
#' # Split along columns and then
#' # rows in chunks of 2
#' rray_split(x, c(2, 1), c(2, 2))
#'
#'
#' # 4 dimensional example
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
#' # End up with 2 equal sized elements
#' rray_split(x_4d, 1)
#'
#' # Split along columns
#' rray_split(x_4d, 2)
#'
#' # Probably the most useful thing you might do
#' # is use this to split the 4D array into a set
#' # of 4 2D matrices. To have them in order
#' # split by the 4th dimension, then the 3rd.
#' rray_split(x_4d, c(4, 3))
#'
#' @export
rray_split <- function(x, axes, n = NULL) {

  axes <- vec_cast(axes, integer())
  validate_axes(axes, x)

  # Default to size of the axes
  if (is_null(n)) {
    n <- rray_dim(x)[axes]
  }

  n <- vec_cast(n, integer())

  validate_axis_n_size(axes, n)

  res <- rray_multi_split(x, axes, n)

  # All non-axes dim names are the same
  # axes dim names are split up over the axes
  new_dim_names_lst <- split_dim_names(dim_names(x), axes, n)

  # Use anonymous function to work around dispatch bug with internal generics
  res <- map2(res, new_dim_names_lst, function(x, nms) {set_full_dim_names(x, nms)})

  res <- map(res, vec_restore, to = x)

  # # TODO - cant use this because of an issue related to
  # # https://github.com/DavisVaughan/rray/issues/51
  # # since as_list_of() calls vec_cast()
  # vctrs::as_list_of(res)

  res
}

rray_split_impl <- function(x, n, axis) {
  rray_op_unary_two_cpp("split", x, n, as_cpp_idx(axis))
}

rray_multi_split <- function(x, axes, n) {
  idx <- seq_along(axes)
  x <- list(x)

  for(i in idx) {
    x <- map(x, rray_split_impl, n = n[i], axis = axes[i])
    x <- rlang::squash(x)
  }

  x
}

validate_axis_n_size <- function(axes, n) {

  axes_size <- vec_size(axes)
  n_size <- vec_size(n)

  if (axes_size != n_size) {
    glubort(
      "The size of `axes` ({axes_size}) must be the same as `n` ({n_size})."
    )
  }

  invisible()
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
