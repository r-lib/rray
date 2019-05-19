#' Absolute value
#'
#' Compute the absolute value.
#'
#' @param x A vector, matrix, array or rray.
#'
#' @details
#'
#' If `x` is logical, an integer is returned.
#'
#' @examples
#'
#' rray_abs(-1)
#'
#' x <- rray(1:5 * -1L, c(5, 1))
#'
#' rray_abs(x)
#'
#' rray_abs(TRUE)
#'
#' @family math functions
#' @export
rray_abs <- function(x) {
  rray_math_unary_base(rray__abs, x)
}

# ------------------------------------------------------------------------------

#' Sign function
#'
#' @description
#'
#' Compute the elementwise sign of an array. Depending on the input the
#' following is returned:
#'
#' - `1`: Positive input
#' - `0`: Zero input
#' - `-1`: Negative input
#' - `NA`: `NA` input
#'
#' @param x A vector, matrix, array or rray.
#'
#' @return
#'
#' An array the same shape as `x`, but with numeric values indicating the
#' sign of the values in `x`.
#'
#' @examples
#'
#' rray_sign(-5)
#'
#' x <- rray(c(5, NA, -5), c(3, 2))
#'
#' rray_sign(x)
#'
#' @family math functions
#' @export
rray_sign <- function(x) {
  rray_math_unary_base(rray__sign, x)
}

# ------------------------------------------------------------------------------

#' Remainders
#'
#' @description
#'
#' * `rray_fmod()` computes the element-wise remainder of the floating point
#' division of `x / y`.
#'
#' * `rray_remainder()` computes the IEEE element-wise remainder of the
#' floating point division of `x / y`.
#'
#' @param x,y A vector, matrix, array or rray.
#'
#' @details
#'
#' * `rray_fmod()` - The floating-point remainder of the division operation
#' `x / y` calculated by this function is exactly the value `x - n * y`, where
#' `n` is `x / y` with its fractional part truncated.
#'
#' * `rray_remainder()` - The IEEE floating-point remainder of the division
#' operation `x / y` calculated by this function is exactly the
#' value `x - n * y`, where the value `n` is the integral value nearest the
#' exact value `x / y`. When `|n - x / y| = 1/2`, the value `n` is chosen
#' to be even. In contrast to `rray_fmod()`, the returned value is not
#' guaranteed to have the same sign as `x`.
#'
#' @examples
#' rray_fmod(1, 2)
#' rray_remainder(1, 2)
#'
#' # Always same sign as `x`
#' # 2 / 3 = 0.667 -> n = 0
#' # (2 - 0 * 3) = 2
#' rray_fmod(2, 3)
#'
#' # No guarantee of same sign as `x`
#' # 2 / 3 = 0.667 -> n = 1
#' # (2 - 1 * 3) = -1
#' rray_remainder(2, 3)
#'
#' # 12 / 8 = 1.5 -> n = 2 or 1?
#' # This is the `|n - x / y| = 1/2` case
#' # Chosen to be n = 2, i.e. even, by convention.
#' # (12 - 2 * 8) = -4
#' rray_remainder(12, 8)
#'
#' # Broadcasts appropriately
#' x <- matrix(1:5)
#' rray_remainder(x, t(x))
#'
#' # Be aware of the limits of floating
#' # point arithmetic!
#' # 1 / .2 = 5 -> n = 5
#' # (1 - 5 * .2) = 0
#' # I get -5.551115e-17
#' rray_remainder(1, .2)
#'
#' # More serious limitations with rray_fmod()
#' # 1 / .1 = 10 -> n = 10
#' # (1 - 10 * .1) = 0
#' # I get 0.1
#' rray_fmod(1, .1)
#'
#' # ^ is due to the implementation of `std::fmod()`
#' # in C++. It is basically the following, which
#' # gives the incorrect 0.1:
#' x <- 1
#' y <- 0.1
#' x_abs <- abs(x)
#' y_abs <- abs(y)
#' # I get -5.551115e-17
#' result <- rray_remainder(x_abs, y_abs)
#' if (sign(result)) result <- result + y_abs
#' rray_abs(result) * sign(x)
#'
#' @name remainder
#' @family math functions
#' @export
rray_fmod <- function(x, y) {
  rray_math_binary_base(rray__fmod, x, y)
}

#' @rdname remainder
#' @export
rray_remainder <- function(x, y) {
  rray_math_binary_base(rray__remainder, x, y)
}

# ------------------------------------------------------------------------------

#' Maximum and minimum values
#'
#' @description
#'
#' `rray_maximum()` and `rray_minimum()` compute the elementwise max / min
#' between `x` and `y`.
#'
#' @param x,y A vector, matrix, array or rray.
#'
#' @examples
#' # Elementwise maximum
#' rray_maximum(c(1, 2, 3), c(3, 2, 1))
#'
#' # Elementwise minimum
#' rray_minimum(c(1, 2, 3), c(3, 2, 1))
#'
#' # With broadcasting
#' x <- matrix(1:3)
#' rray_maximum(x, t(x))
#'
#' @family math functions
#' @export
rray_maximum <- function(x, y) {
  rray_math_binary_base(rray__maximum, x, y)
}

#' @rdname rray_maximum
#' @export
rray_minimum <- function(x, y) {
  rray_math_binary_base(rray__minimum, x, y)
}

# ------------------------------------------------------------------------------

#' Fused multiply-add
#'
#' `rray_multiply_add()` computes `x * y + z`, with broadcasting.
#' It is more efficient than simply doing those operations in sequence.
#'
#' @param x,y,z A vector, matrix, array or rray.
#'
#' @examples
#' # 2 * 3 + 5
#' rray_multiply_add(2, 3, 5)
#'
#' # Using broadcasting
#' rray_multiply_add(matrix(1:5), matrix(1:2, nrow = 1L), 3L)
#'
#' # ^ Equivalent to:
#' x <- matrix(rep(1:5, 2), ncol = 2)
#' y <- matrix(rep(1:2, 5), byrow = TRUE, ncol = 2)
#' z <- matrix(3L, nrow = 5, ncol = 2)
#' x * y + z
#'
#' @family math functions
#' @export
rray_multiply_add <- function(x, y, z) {
  rray_math_trinary_base(rray__multiply_add, x, y, z)
}

# ------------------------------------------------------------------------------

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
#' @examples
#'
#' # bound `x` between 1 and 5
#' x <- matrix(1:10, ncol = 2)
#' rray_clip(x, 1, 5)
#'
#' @family math functions
#' @export
rray_clip <- function(x, low, high) {
  inner <- vec_type_inner(x)
  low <- vec_cast_inner(low, inner)
  high <- vec_cast_inner(high, inner)

  n_low <- vec_size(low)
  if (n_low != 1L) {
    glubort("`low` must have size 1, not {n_low}.")
  }

  n_high <- vec_size(high)
  if (n_high != 1L) {
    glubort("`high` must have size 1, not {n_high}.")
  }

  if (low > high) {
    glubort("`low` must be less than or equal to `high`.")
  }

  rray_math_unary_base(rray__clip, x, low = low, high = high)
}

# ------------------------------------------------------------------------------

rray_math_unary_base <- function(f, x, ...) {
  rray_arith_unary_base(f, x, ...)
}

rray_math_binary_base <- function(f, x, y) {
  rray_arith_binary_base(f, x, y)
}

rray_math_trinary_base <- function(f, x, y, z) {

  # done before the inner cast
  new_dim_names <- rray_dim_names_common(x, y, z)

  args <- vec_cast_inner_common(x, y, z)

  res <- f(args[[1]], args[[2]], args[[3]])

  res <- set_full_dim_names(res, new_dim_names)

  vec_cast_container(res, vec_type_container_common(x, y, z))
}
