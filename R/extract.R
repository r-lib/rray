#' Get or set elements of an array by index
#'
#' `rray_extract()` is the counterpart to [rray_yank()]. It extracts elements
#' from an array _by index_. It _always_ drops dimensions
#' (unlike [rray_subset()]), and a 1D object is always returned.
#'
#' @inheritParams rray_subset
#'
#' @param value The value to assign to the location specified by `...`.
#' Before assignment, `value` is cast to the type and dimension of `x` after
#' extracting elements with `...`.
#'
#' @details
#'
#' Like `[[`, `rray_extract()` will _never_ keep dimension names.
#'
#' `rray_extract()` works with base R objects as well.
#'
#' `rray_extract()` is similar to the traditional behavior of
#' `x[[i, j, ...]]`, but allows each subscript to have length >1.
#'
#' @return
#'
#' A 1D vector of elements extracted from `x`.
#'
#' @examples
#' x <- rray(1:16, c(2, 4, 2), dim_names = list(c("r1", "r2"), NULL, NULL))
#'
#' # Extract the first row and flatten it
#' rray_extract(x, 1)
#'
#' # Extract the first row and first two columns
#' rray_extract(x, 1, 1:2)
#'
#' # You can assign directly to these elements
#' rray_extract(x, 1, 1:2) <- NA
#' x
#'
#' @family rray subsetters
#' @export
rray_extract <- function(x, ...) {
  rray_extract_impl(x, ...)
}

rray_extract_impl <- function(x, ...) {
  indexer <- rray_as_index(x, ...)

  # TODO
  if (is_any_na_int(indexer)) {
    abort("`NA` indices are not yet supported.")
  }

  rray__extract(x, indexer)
}
