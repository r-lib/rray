#' Compute the number of dimensions of an object
#'
#' `rray_dim_n()` computes the dimensionality (i.e. the number of dimensions).
#'
#' @param x An object.
#'
#' @details
#'
#' One point worth mentioning is that `rray_dim_n()` is very strict. It does
#' not simply call the generic function `dim()` and then check the length.
#' Rather, it explicitly pulls the attribute for the `"dim"`, and checks
#' the length of that. If an object does not have an attribute, then the
#' dimensionality is 1.
#'
#' This means that data frames have a dimensionality of 1, even though
#' `dim()` defines a method for data frames that would imply a dimensionality
#' of 2.
#'
#' @return
#'
#' An integer vector containing the number of dimensions of `x`.
#'
#' @examples
#' x_1_by_4 <- rray(c(1, 2, 3, 4), c(1, 4))
#'
#' rray_dim_n(x_1_by_4)
#'
#' # NULL has a dimensionality of 1
#' rray_dim_n(NULL)
#'
#' # The dimensionality of a data frame is 1
#' rray_dim_n(data.frame())
#'
#' @export
rray_dim_n <- function(x) {
  rray__dim_n(x)
}
