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
#' @param value A value coercable to the same _inner_ type as `x` that will be
#' used to fill the result (If `x` is an integer matrix, then `value` will be
#' coerced to an integer).
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

  # not vec_size() (consider array(1, c(1, 1, 2)))
  if (length(value) != 1) {
    glubort("`value` must have length 1, not {length(value)}.")
  }

  # ensure fill is 1D and cast to inner type of `x`
  value <- value[[1]]
  value <- vec_cast_inner(value, x)

  res <- rray__full_like(x, value)

  # no dim names on the result

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
