#' Create an array like `x`
#'
#' @description
#'
#' - `rray_full_like()` creates an array with the same type and size as `x`, but
#' filled with `value`.
#'
#' - `rray_ones_like()` is `rray_full_like()` with `value = 1`.
#'
#' - `rray_zeros_like()` is `rray_full_like()` with `value = 0`.
#'
#' @details
#'
#' No dimension names will be on the result.
#'
#' @param x A vector, matrix, array or rray.
#'
#' @param value A value coercible to the same _inner_ type as `x` that will be
#' used to fill the result (If `x` is an integer matrix, then `value` will be
#' coerced to an integer).
#'
#' @return
#'
#' An object with the same type as `x`, but filled with `value`.
#'
#' @examples
#'
#' x <- rray(1:10, c(5, 2))
#'
#' # Same shape and type as x, but filled with 1
#' rray_full_like(x, 1L)
#'
#' # `fill` is coerced to `x` if it can be
#' rray_full_like(x, FALSE)
#'
#' # `value = 1`
#' rray_ones_like(x)
#'
#' # When logicals are used, it is filled with TRUE
#' rray_ones_like(c(FALSE, TRUE))
#'
#' # `value = 0`
#' rray_zeros_like(x)
#'
#' @export
rray_full_like <- function(x, value) {

  # ensure value is 1D
  value <- as.vector(value)

  res <- rray__full_like(x, value)

  vec_cast_container(res, x)
}

#' @rdname rray_full_like
#' @export
rray_ones_like <- function(x) {
  rray_full_like(x, 1L)
}

#' @rdname rray_full_like
#' @export
rray_zeros_like <- function(x) {
  rray_full_like(x, 0L)
}
