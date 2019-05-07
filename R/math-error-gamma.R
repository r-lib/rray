#' Error and gamma functions
#'
#' @description
#'
#' - `rray_erf()` - Error function
#'
#' - `rray_erfc()` - Complementary error function
#'
#' - `rray_gamma()` - Gamma function
#'
#' - `rray_lgamma()` - Natural log of the absolute value of the gamma function
#'
#' - `rray_digamma()` - First derivative of the logarithm of the gamma function
#'
#' - `rray_trigamma()` - Second derivative of the logarithm of the gamma
#' function
#'
#' @param x A vector, matrix, array or rray.
#'
#' @details
#'
#' For more in depth details describing the gamma functions, see `?gamma`.
#'
#' `rray_gamma(0)` returns `Inf`, while `gamma(0)` returns `NaN`. The rray
#' behavior matches the IEEE standard defined in the error handling section
#' of the [`tgamma()` C++ definition](https://en.cppreference.com/w/cpp/numeric/math/tgamma).
#'
#' @examples
#' x <- matrix(c(2, 4, 6))
#'
#' rray_erf(x)
#' rray_erfc(x)
#'
#' rray_gamma(x)
#' rray_lgamma(x)
#' rray_digamma(x)
#' rray_trigamma(x)
#'
#' @family error and gamma math functions
#' @export
rray_erf <- function(x) {
  rray_math_unary_base(rray__erf, x)
}

#' @rdname rray_erf
#' @export
rray_erfc <- function(x) {
  rray_math_unary_base(rray__erfc, x)
}

#' @rdname rray_erf
#' @export
rray_gamma <- function(x) {
  rray_math_unary_base(rray__gamma, x)
}

#' @rdname rray_erf
#' @export
rray_lgamma <- function(x) {
  rray_math_unary_base(rray__lgamma, x)
}

# ------------------------------------------------------------------------------

# Not implemented by xtensor, but these are generics in
# `?Math` that `vec_math()` uses

#' @rdname rray_erf
#' @export
rray_digamma <- function(x) {
  rray_math_unary_base_raw(digamma, x)
}

#' @rdname rray_erf
#' @export
rray_trigamma <- function(x) {
  rray_math_unary_base_raw(trigamma, x)
}
