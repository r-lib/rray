#' Tile an array
#'
#' @param x A vector, matrix, array or rray.
#'
#' @param times An integer vector. The number of times to repeat the
#' array along an axis.
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

  times <- vec_cast(times, integer())

  for (i in seq_along(times)) {
    time <- times[i]
    x <- rep_len(list(x), time)
    x <- rray_bind(!!!x, axis = i)
  }

  x
}
