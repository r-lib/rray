#' Get or set a slice of an array
#'
#' `rray_slice()` is a shortcut wrapper around [rray_subset()] that is useful
#' for easily subsetting along a single axis.
#'
#' @param x A vector, matrix, array or rray.
#'
#' @param i Indices to extract along a single axis.
#' - Integer indices extract specific elements of the `axis` dimension.
#' - Logical indices must be length 1, or the length of the `axis` dimension.
#' - Character indices are only allowed if `x` has names for the `axis` dimension.
#' - `NULL` is treated as `0`.
#'
#' @param axis An integer. The axis to subset along.
#'
#' @param value A value to be assigned to the location at
#' `rray_slice(x, i, axis)`. It will be cast to the type and dimension
#' of the slice of `x`.
#'
#' @details
#'
#' `rray_slice()` does exactly the same thing as [rray_subset()], and is
#' mainly helpful for higher dimensional objects, when you need to subset
#' along, for example, only the 4th dimension.
#'
#' @return
#'
#' `x` with the `i` elements extracted from the `axis`.
#'
#' @examples
#' x <- rray(1:16, c(2, 2, 2, 2))
#'
#' # Selecting the first column
#' rray_slice(x, i = 1, axis = 2)
#'
#' # rray_slice() is particularly useful for
#' # subsetting higher dimensions because you don't
#' # have to worry about the commas
#' rray_slice(x, i = 2, axis = 4)
#'
#' # Compare the above with the equivalent using `[`
#' x[, , , 2]
#'
#' # `i` can be a character vector if `x` has names along `axis`
#' x <- rray_set_axis_names(x, axis = 4, c("foo", "bar"))
#' rray_slice(x, "bar", axis = 4)
#'
#' # The assignment variation can be useful
#' # for assigning to higher dimensional elements
#' rray_slice(x, 1, 3) <- matrix(c(99, 100), nrow = 1)
#'
#' @family rray subsetters
#' @export
rray_slice <- function(x, i, axis) {
  axis <- vec_cast(axis, integer())
  validate_axis(axis, x)

  indexer <- front_pad(i, axis)

  rray_subset(x, !!!indexer)
}

front_pad <- function(i, axis) {
  padding <- rep(list(missing_arg()), times = axis - 1L)
  c(padding, list(i))
}
