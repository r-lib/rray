#' Logarithms and exponentials
#'
#' @description
#'
#' - `rray_exp()` - computes the natural exponential of `x`.
#'
#' - `rray_exp2()` - computes the base 2 exponential of `x`.
#'
#' - `rray_expm1()` - computes `exp(x) - 1`.
#'
#' - `rray_log()` - computes the natural logarithm of `x`.
#'
#' - `rray_log2()` - computes the base 2 logarithm of `x`.
#'
#' - `rray_log10()` - computes the base 10 logarithm of `x`.
#'
#' - `rray_log1p()` - computes `log(1 + x)`.
#'
#' @param x A vector, matrix, array or rray.
#'
#' @param base A single positive number. The base that logarithms are computed
#' with respect to. The default of `NULL` uses a natural log, i.e. `exp(1)`.
#'
#' @examples
#'
#' # Exponential
#' rray_exp(1L)
#' rray_exp(rray(1:5, c(5, 1)))
#'
#' # Identical to 2 ^ x
#' rray_exp2(5)
#'
#' # Log with different bases
#' rray_log(1) == rray_log(1, exp(1))
#' rray_log(100, 2)
#' rray_log(100, exp(1))
#'
#' # From `?log`, the following example shows how `rray_log1p()`
#' # and `rray_expm1()` can be more accurate than `rray_log(1 + x)` or
#' # `rray_exp(x) - 1` for very small `|x|`.
#' x <- 10 ^ -(1 + 2 * 1:9)
#' x_base <- cbind(x, log(1+x), log1p(x), exp(x)-1, expm1(x))
#' x_rray <- cbind(x, rray_log(1+x), rray_log1p(x), rray_exp(x)-1, rray_expm1(x))
#'
#' # No difference with base R!
#' x_base - x_rray
#'
#' @family exponential math functions
#' @export
rray_exp <- function(x) {
  vec_cast_container(rray__exp(x), x)
}

#' @rdname rray_exp
#' @export
rray_exp2 <- function(x) {
  vec_cast_container(rray__exp2(x), x)
}

#' @rdname rray_exp
#' @export
rray_expm1 <- function(x) {
  vec_cast_container(rray__expm1(x), x)
}

#' @rdname rray_exp
#' @export
rray_log <- function(x, base = NULL) {
  base <- validate_base(base)
  vec_cast_container(rray__log(x, base), x)
}

#' @rdname rray_exp
#' @export
rray_log2 <- function(x) {
  vec_cast_container(rray__log2(x), x)
}

#' @rdname rray_exp
#' @export
rray_log10 <- function(x) {
  vec_cast_container(rray__log10(x), x)
}

#' @rdname rray_exp
#' @export
rray_log1p <- function(x) {
  vec_cast_container(rray__log1p(x), x)
}

# ------------------------------------------------------------------------------

validate_base <- function(base) {
  if (is.null(base)) {
    return(base)
  }

  base <- vec_cast(base, double())

  n_base <- length(base)
  if (n_base != 1L) {
    glubort("`base` must have size 1, not {n_base}.")
  }

  if (base <= 0) {
    glubort("`base` must be a positive number, not {base}.")
  }

  base
}
