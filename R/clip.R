#' Bound the values of an array
#'
#' `rray_clip()` sets _inclusive_ lower and upper bounds on the values of `x`.
#'
#' @param x A vector, matrix, array or rray.
#'
#' @param low A single value. The lower bound. `low` is cast to the
#' inner type of `x`.
#'
#' @param high A single value. The upper bound. `high` is cast to the
#' inner type of `x`.
#'
#' @return
#'
#' `x` bounded by `low` and `high`.
#'
#' @examples
#'
#' # bound `x` between 1 and 5
#' x <- matrix(1:10, ncol = 2)
#' rray_clip(x, 1, 5)
#'
#' @export
rray_clip <- function(x, low, high) {

  vec_assert(low, size = 1L, arg = "low")
  vec_assert(high, size = 1L, arg = "high")

  inner <- vec_ptype_inner(x)
  low <- vec_cast_inner(low, inner)
  high <- vec_cast_inner(high, inner)

  if (low > high) {
    glubort("`low` must be less than or equal to `high`.")
  }

  out <- rray__clip(x, low, high)

  vec_cast_container(out, x)
}
