#' Find and count unique values in an array
#'
#' @description
#'
#' * `rray_unique()`: the unique values. Equivalent to [unique()] with the
#' `MARGIN` argument.
#'
#' * `rray_unique_loc()`: the locations of the unique values.
#'
#' * `rray_unique_count()`: the number of unique values.
#'
#' @param x A vector, matrix, array, or rray.
#'
#' @param axis A single integer.
#'
#' @details
#'
#' When duplicates are detected, the _first_ one is used in the result.
#'
#' @examples
#' x <- rray(c(1, 1, 2, 2), c(2, 2))
#' x <- set_row_names(x, c("r1", "r2"))
#' x <- set_col_names(x, c("c1", "c2"))
#'
#' # Unique rows. The first unique
#' # row is used
#' rray_unique(x, 1L)
#'
#' # Create a 3d version of x
#' # where the columns are not unique
#' y <- rray_expand_dims(x, 1)
#'
#' # All of the rows are unique...
#' rray_unique(y, 1L)
#'
#' # ...but the columns are not
#' rray_unique(y, 2L)
#'
#' # The 3rd dimension is unique
#' rray_unique(y, 3L)
#'
#' # rray_unique_loc() returns an
#' # integer vector you can use
#' # to subset out the unique values along
#' # the axis you are interested in
#' y[, rray_unique_loc(y, 2L)]
#'
#' # Only 1 unique column
#' rray_unique_count(y, 2L)
#'
#' # 2 unique 3D slices
#' rray_unique_count(y, 3L)
#'
#' @export
rray_unique <- function(x, axis = 1L) {

  axis <- vec_cast(axis, integer())
  validate_axis(axis, x)

  if (identical(axis, 1L)) {
    return(vec_unique(x))
  }

  # Make the axis of interest the rows
  x <- rray_rotate(x, from = axis, to = 1L)
  x <- vec_unique(x)

  # Rotate back to get the result
  x <- rray_rotate(x, from = 1L, to = axis)

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

  x <- rray_rotate(x, from = axis, to = 1L)

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

  x <- rray_rotate(x, from = axis, to = 1L)

  vec_unique_count(x)
}
