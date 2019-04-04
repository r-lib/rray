#' Find duplicated values in an array
#'
#' @description
#'
#' * `rray_duplicate_any()`: detects the presence of duplicated values,
#' similar to [anyDuplicated()] with the `MARGIN` argument.
#'
#' * `rray_duplicate_detect()`: returns a logical vector describing if each
#' element of the vector is duplicated elsewhere. It is similar to
#' [duplicated()] with the `MARGIN` argument, but it reports all duplicated
#' values, not just the second and subsequent repetitions.
#'
#' * `rray_duplicate_id()`: returns an integer vector giving the location of the
#' first occurence of the value.
#'
#' @param x A vector, matrix, array, or rray.
#'
#' @param axis A single integer. The axis to look for duplicate values over.
#'
#' @return
#'
#' * `rray_duplicate_any()`: a logical vector of length 1.
#'
#' * `rray_duplicate_detect()`: a logical vector the same length as `x`.
#'
#' * `rray_duplicate_id()`: an integer vector the same length as `x`.
#'
#' @seealso
#'
#' [rray_unique()] for functions that work with the dual of duplicated values:
#' unique values.
#'
#' [vctrs::vec_duplicate_any()] for functions that detect duplicates among
#' any type of vector object.
#'
#' @examples
#' x <- rray(c(1, 1, 2, 2), c(2, 2))
#' x <- set_row_names(x, c("r1", "r2"))
#' x <- set_col_names(x, c("c1", "c2"))
#'
#' # Are there duplicates along the rows?
#' rray_duplicate_any(x, 1L)
#'
#' # Are there duplicates along the columns?
#' rray_duplicate_any(x, 2L)
#'
#' # Create a 3d version of x
#' # where the columns are not unique
#' y <- rray_expand_dims(x, 1)
#'
#' # All of the rows are unique...
#' rray_duplicate_any(y, 1L)
#'
#' # ...but the columns are not
#' rray_duplicate_any(y, 2L)
#'
#' # rray_duplicate_detect() returns
#' # `TRUE` if a duplicate is detected
#' # anywhere in the array (both
#' # columns are registered as
#' # duplicates)
#' rray_duplicate_detect(y, 2)
#'
#' # duplicated() only returns `TRUE`
#' # for the second and all subsequent
#' # duplicates
#' duplicated(as_array(y), MARGIN = 2L)
#'
#' # Find the location of the
#' # first unique values
#' rray_duplicate_id(y, 2L)
#'
#' # Both of the 3rd dimension elements
#' # are unique
#' rray_duplicate_id(y, 3L)
#'
#' @name rray_duplicate
#' @export
rray_duplicate_any <- function(x, axis = 1L) {

  axis <- vec_cast(axis, integer())
  validate_axis(axis, x)

  if (identical(axis, 1L)) {
    return(vec_duplicate_any(x))
  }

  x <- rray_rotate(x, from = axis, to = 1L)

  vec_duplicate_any(x)
}

#' @rdname rray_duplicate
#' @export
rray_duplicate_detect <- function(x, axis = 1L) {

  axis <- vec_cast(axis, integer())
  validate_axis(axis, x)

  if (identical(axis, 1L)) {
    return(vec_duplicate_detect(x))
  }

  x <- rray_rotate(x, from = axis, to = 1L)

  vec_duplicate_detect(x)
}

#' @rdname rray_duplicate
#' @export
rray_duplicate_id <- function(x, axis = 1L) {

  axis <- vec_cast(axis, integer())
  validate_axis(axis, x)

  if (identical(axis, 1L)) {
    return(vec_duplicate_id(x))
  }

  x <- rray_rotate(x, from = axis, to = 1L)

  vec_duplicate_id(x)
}
