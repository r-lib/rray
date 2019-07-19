#' Pad missing dimensions when subsetting
#'
#' `pad()` is used alongside the standard rray subsetting operator `[` (and
#' the underlying `rray_subset()` function) to easily subset into higher
#' dimensions without having to explicitly list the intermediate commas.
#'
#' @return
#'
#' An object that can be used to pad dimensions with when subsetting.
#'
#' @examples
#' x <- rray(1:4, c(1, 1, 2, 2))
#'
#' # pad() fills in the missing dimensions
#' # essentially it adds commas automatically
#'
#' # second element in the 4th dimension
#' x[pad(), 2]
#'
#' # vs using
#' x[,,,2]
#'
#' # second element in 3rd
#' # first element in 4th
#' x[pad(), 2, 1]
#'
#' # can fill in the missing gaps too
#' # this fills in the 2nd/3rd dimensions
#' x[1, pad(), 1]
#'
#' # if a pad() isn't needed
#' # because the dimensionality is already fully
#' # specified by the indices, its ignored
#' x_flat <- rray_reshape(x, 4)
#' x_flat[pad(), 1]
#' x_flat[1, pad()]
#'
#' # `pad()` can be used with base R
#' # objects as well through `rray_subset()`
#' x_arr <- as.array(x)
#' rray_subset(x_arr, pad(), 1)
#'
#' @export
pad <- function() {
  new_pad()
}

new_pad <- function() {
  new_vctr(1L, class = "vctrs_pad")
}

#' @export
format.vctrs_pad <- function(x, ...) {
  "<padding>"
}

is_pad <- function(x) {
  inherits(x, "vctrs_pad")
}
