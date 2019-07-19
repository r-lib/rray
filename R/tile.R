#' Tile an array
#'
#' @param x A vector, matrix, array or rray.
#'
#' @param times An integer vector. The number of times to repeat the
#' array along an axis.
#'
#' @details
#'
#' `rray_tile()` should not be used as a replacement for `rray_broadcast()`,
#' as it is generally less efficient.
#'
#' @return
#'
#' `x` with dimensions repeated as described by `times`.
#'
#' @examples
#'
#' x <- matrix(1:5)
#'
#' # Repeat the rows twice
#' rray_tile(x, 2)
#'
#' # Repeat the rows twice and the columns three times
#' rray_tile(x, c(2, 3))
#'
#' # Tile into a third dimension
#' rray_tile(x, c(1, 2, 2))
#'
#' @export
rray_tile <- function(x, times) {

  dim_n <- rray_dim_n(x)
  size_times <- vec_size(times)

  if (dim_n < size_times) {
    new_dim <- rray_increase_dims(rray_dim(x), size_times)
    x <- rray_reshape(x, new_dim)
  }

  if (dim_n > size_times) {
    times <- rray_increase_dims(times, dim_n)
  }

  dim <- rray_dim(x)

  slicer <- map2(times, dim, get_tile_index)

  res <- eval_bare(expr(x[!!!slicer, drop = FALSE]))

  res
}

# - Generate the repeated index for each dim
# - If `times == 1` for that dim, return a missing arg to return
#   that entire dim
# - `single_dim == 0` is handled gracefully and `integer()`
#   is returned which is correct
get_tile_index <- function(single_time, single_dim) {
  if (single_time == 1L) {
    return(missing_arg())
  }

  rep(seq_len(single_dim), times = single_time)
}
