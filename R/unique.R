#' Find and count unique values in an array
#'
#' @description
#'
#' * `rray_unique()`: the unique values. Equivalent to `unique(x, MARGIN)`.
#'
#' * `rray_unique_loc()`: the locations of the unique values.
#'
#' * `rray_unique_count()`: the number of unique values.
#'
#' @param x A vector, matrix, array, or rray.
#'
#' @param axis A single integer.
#'
#' @export
rray_unique <- function(x, axis = 1L) {

  axis <- vec_cast(axis, integer())
  validate_axis(axis, x)

  if (identical(axis, 1L)) {
    return(vec_unique(x))
  }

  # Make the axis of interest the rows
  x <- rray_rotate(x, from = 1L, to = axis)
  x <- vec_unique(x)

  # Rotate back to get the result
  x <- rray_rotate(x, from = axis, to = 1L)

  x
}

#' @rdname rray_unique
#' @export
rray_unique_loc <- function(x, axis = 1L) {

  axis <- vec_cast(axis, integer())
  validate_axis(axis, x)

  if (identical(axis, 1L)) {
    return(vec_unique_loc(x))
  }

  x <- rray_rotate(x, from = 1L, to = axis)

  vec_unique_loc(x)
}

#' @rdname rray_unique
#' @export
rray_unique_count <- function(x, axis = 1L) {

  axis <- vec_cast(axis, integer())
  validate_axis(axis, x)

  if (identical(axis, 1L)) {
    return(vec_unique_count(x))
  }

  x <- rray_rotate(x, from = 1L, to = axis)

  vec_unique_count(x)
}
