#' Rotate an array
#'
#' `rray_rotate()` rotates an array along the plane defined by `c(from, to)`. It
#' can be thought of as sequentially rotating the array 90 degrees a set number
#' of `times`.
#'
#' @param x A matrix, array, or rray.
#'
#' @param from,to Single integer values. The direction of the rotation goes
#' from `from` and towards `to`. When using a 2D matrix, this can be thought
#' of as rotating _counter clockwise_ starting at `from` and going towards `to`.
#' To go _clockwise_, reverse `from` and `to`.
#'
#' @param times A single integer. The number of times to perform the rotation.
#' One of: `1`, `2`, `3`. Or, equivalently: `-3`, `-2`, `-1`.
#'
#' @details
#'
#' A rotation can be a hard thing to wrap your head around. I encourage
#' looking at the examples and starting with 2D to try and understand what
#' is happening before moving up to higher dimensions.
#'
#' Note that a rotation is _not_ the same thing as a transpose.
#'
#' Generally, you can predict the output of rotating using `from` and `to`
#' by switching the dimensions at the `from` and `to` axes position. This gives
#' you the shape of the output. So, a `(5, 2, 4)` array rotated using `from = 1`
#' and `to = 3` would have a resulting shape of `(4, 2, 5)`. Note that using
#' `from = 3` and `to = 1` would give the same shape. The "direction"
#' of how these are rotated is controlled by the ordering of `from` and `to`.
#'
#' @return
#'
#' `x` rotated along the axis described by `from` and `to`.
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' # 2D example
#'
#' x <- rray(1:6, c(3, 2))
#' x <- rray_set_row_names(x, c("r1", "r2", "r3"))
#' x <- rray_set_col_names(x, c("c1", "c2"))
#'
#' # "counter clockwise" rotation turning the
#' # rows into columns
#' rray_rotate(x)
#'
#' # "clockwise" by reversing the direction
#' rray_rotate(x, from = 2, to = 1)
#'
#' # Rotate twice (180 degrees)
#' # Direction doesn't matter here, the following
#' # give the same result
#' rray_rotate(x, times = 2)
#' rray_rotate(x, from = 2, to = 1, times = 2)
#'
#' # ---------------------------------------------------------------------------
#' # 3D example
#'
#' x_3d <- rray_expand(x, 3)
#'
#' # - Rotations on the (1, 3) axis plane
#' # - Dimensions go from (3, 2, 1) -> (1, 2, 3) in both cases
#' # - And the direction of how that happens is controlled
#' #   by `from` and `to`
#' rray_rotate(x_3d, from = 1, to = 3)
#'
#' rray_rotate(x_3d, from = 3, to = 1)
#'
#' @export
rray_rotate <- function(x, from = 1, to = 2, times = 1) {

  validate_at_least_two_dimensions(x)

  dim_n <- rray_dim_n(x)

  from <- vec_cast(from, integer())
  validate_axes(from, x, n = 1L, nm = "from")

  to <- vec_cast(to, integer())
  validate_axes(to, x, n = 1L, nm = "to")

  if (identical(from, to)) {
    glubort("`from` and `to` must be different values.")
  }

  times <- vec_cast(times, integer())
  validate_scalar(times, "times")
  times <- normalize_times(times)

  out <- rray__rotate(x, as_cpp_idx(from), as_cpp_idx(to), times)

  vec_cast_container(out, x)
}

validate_scalar <- function(x, nm) {
  if (vec_size(x) != 1L) {
    glubort("`{nm}` must have size 1, not {vec_size(x)}.")
  }

  invisible(x)
}

validate_at_least_two_dimensions <- function(x, arg = "x") {

  if (is.null(x)) {
    return(NULL)
  }

  dim_n <- rray_dim_n(x)

  if (dim_n < 2L) {
    glubort("`{arg}` must have at least 2 dimensions, not {dim_n}.")
  }

  invisible(x)
}

# times == -1 -> 3
# times == -2 -> 2
# times == -3 -> 1
# stolen from xt::rot90()
normalize_times <- function(times) {
  (4 + (times %% 4)) %% 4
}
