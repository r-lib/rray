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
    axes <- seq_len(rray_dim_n(x))
  }

  res <- rray__split(x, as_cpp_idx(axes))

  # # Avoid copy by not calling map()
  # # map() always copies, even if `vec_cast_container()`
  # # does no work
  for (i in seq_along(res)) {
    res[[i]] <- vec_cast_container(res[[i]], x)
  }

  res
}
