#' Broadcast to a new dimension
#'
#' `rray_broadcast()` will _broadcast_ the dimensions of the current
#' object to a new dimension.
#'
#' Broadcasting works by _recycling dimensions_ and _repeating values_ in
#' those dimensions to match the new dimension.
#'
#' Here's an example. Assume you have a 1x3 matrix that you want to broadcast
#' to a dimension of 2x3. Since the 1st dimensions are different, and one of them
#' is 1, the 1 row of the 1x3 matrix is repeated to become a 2x3 matrix. For
#' the second dimension, both are already 3 so nothing is done.
#'
#' ```
#' (1, 3)
#' (2, 3)
#' ------
#' (2, 3)
#' ```
#'
#' As an example that _doesn't_ broadcast, here is an attempt to make a
#' 2x1x4 matrix broadcast to a 2x3x5 matrix (In the R world, 2x3x4 means
#' a 2 row, 3 column, and 4 "deep" array). The first 2 dimensions are fine,
#' but for the third dimension, 4 and 5 are not "recyclable" and are therefore
#' incompatible.
#'
#' ```
#' (2, 1, 4)
#' (2, 3, 5)
#' ---------
#' (2, 3, X)
#' ```
#'
#' You can broadcast to higher dimensions too. If you go from a 5x2 to a
#' 5x2x3 array, then the 5x2 matrix implicitly gets a 1 appended as another
#' dimension (i.e. 5x2x1)
#'
#'
#' ```
#' (5, 2,  ) <- implicit 1 is recycled
#' (5, 2, 3)
#' ---------
#' (5, 2, 3)
#' ```
#'
#' Broadcasting is an important concept in rray, as it is the engine behind
#' the different structure for arithmetic operations.
#'
#' @param x The object to broadcast.
#' @param dim An integer vector. The dimension to broadcast to.
#'
#' @return
#'
#' `x` broadcast to the new dimensions.
#'
#' @examples
#'
#' # From 5x1 ...
#' x <- rray(1:5)
#'
#' # ...to 5x2
#' rray_broadcast(x, c(5, 2))
#'
#' # Internally, rray() uses broadcasting
#' # for convenience so you could have also
#' # done this with:
#' rray(1:5, dim = c(5, 2))
#'
#' # Moar dimensions
#' rray_broadcast(x, c(5, 2, 3))
#'
#' # You cannot broadcast down in dimensions
#' try(rray_broadcast(x, 5))
#'
#' @export
rray_broadcast <- function(x, dim) {
  dim <- vec_cast(dim, integer())

  res <- rray__broadcast(x, dim)

  vec_cast_container(res, x)
}
