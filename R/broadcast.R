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
#' 5x2x3 array, then the 5x2 matrix implicitely gets a 1 appended as another
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
#' # rray_broadcast(x, 5)
#' # > Error: Cannot decrease dimensions of `x`
#'
#' @export
rray_broadcast <- function(x, dim) {
  UseMethod("rray_broadcast")
}

# default restores names, but not type (really not
# class, as a new shape is created)

#' @export
rray_broadcast.default <- function(x, dim) {
  res <- broadcast(x, dim)
  dim_names(res) <- restore_dim_names(x, dim)
  res
}

#' @export
rray_broadcast.vctrs_rray <- function(x, dim) {
  vec_restore(rray_broadcast.default(x, dim), x)
}

#' @export
rray_broadcast.vctrs_mtrx <- function(x, dim) {
  dim <- vec_cast(dim, integer())
  out <- rray_broadcast.default(x, dim)

  if (length(dim) > 2) {
    vec_restore(out, new_rray(shape = dim[-1]))
  } else {
    vec_restore(out, x)
  }
}

# makes no attempt to recover dim names
# or type
broadcast <- function(x, dim) {
  res <- rray_dims_match(x, vec_size(dim))
  validate_recyclable(vec_dim(res), dim)
  res <- rray_broadcast_cpp(res, dim)
  res
}

# Match up the dims of x with the dims of y
# by adding 1s to the dim of x and assigning it to x
# this helper is good with broadcasting
rray_dims_match <- function(x, dims) {

  x_dim <- vec_dim(x)

  if (vec_size(x_dim) > dims) {
    abort("Cannot decrease dimensions of `x`")
  }

  dim <- dim_extend(x_dim, dims)
  x <- set_dim(x, dim)
  x
}

# when broadcasting, cant go from [0,2] to [1,1]. Column dimension is
# downcasted
validate_recyclable <- function(from_dim, to_dim) {
  ok <- from_dim == to_dim | from_dim == 1 | to_dim == 0
  if (any(!ok)) {
    abort("Non-recyclable dimensions")
  }
}

