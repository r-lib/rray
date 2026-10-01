#ifndef RRAY_ONE_MULTIPLY_H
#define RRAY_ONE_MULTIPLY_H

#include <limits.h>

#include "rlang.h"

#include "arithmetic.h"
#include "missing.h"
#include "utils.h"

// --------------------------------------------------------------------------
// Elementwise

// - `x = 2`, `y = 3`: returns `6`.
// - `x = 2`, `y = NA`: returns `NA`.
// - `x = NA`, `y = 0`: returns `NA`.
// - `x = 2`, `y = INT_MAX`: errors.
static inline int rray_multiply_int_one(
  int x,
  int y,
  struct r_lazy error_call
) {
  if (rray_int_is_missing(x) || rray_int_is_missing(y)) {
    return r_globals.na_int;
  }

  // Benchmarked and this is just as fast as R's `GOODIPROD()`
  const double out = (double) x * (double) y;

  if (out > INT_MAX || out < -INT_MAX) {
    stop_int_overflow(error_call);
  }

  return (int) out;
}

static inline double rray_multiply_dbl_one(double x, double y) {
  return x * y;
}

// Matching R with `_Complex` `*`, which "recovers" infinities
static inline r_complex rray_multiply_cpl_one(r_complex x, r_complex y) {
  return rray_c99_to_cpl(rray_cpl_to_c99(x) * rray_cpl_to_c99(y));
}

// --------------------------------------------------------------------------
// Reduce

// `out` is a double, `x` is a logical
// - `out = 2`, `x = 1`: returns `2`.
// - `out = 2`, `x = NA`: returns `NA`.
static inline double rray_prod_lgl_one(double out, int x) {
  if (rray_lgl_is_missing(x)) {
    return r_globals.na_dbl;
  }

  return out * x;
}

// `out` is a double, `x` is a logical
// - `out = 2`, `x = 1`: returns `2`.
// - `out = 2`, `x = NA`: returns `2`.
static inline double rray_prod_lgl_one_na_rm(double out, int x) {
  if (rray_lgl_is_missing(x)) {
    return out;
  }

  return out * x;
}

// `out` is a double, `x` is an integer
// - `out = 2`, `x = 3`: returns `6`.
// - `out = 2`, `x = NA`: returns `NA`.
static inline double rray_prod_int_one(double out, int x) {
  if (rray_int_is_missing(x)) {
    return r_globals.na_dbl;
  }

  return out * x;
}

// `out` is a double, `x` is an integer
// - `out = 2`, `x = 3`: returns `6`.
// - `out = 2`, `x = NA`: returns `2`.
static inline double rray_prod_int_one_na_rm(double out, int x) {
  if (rray_int_is_missing(x)) {
    return out;
  }

  return out * x;
}

// Purposefully choose to match `rray_multiply()` rather than `prod()`
// regarding `c(NA, NaN)` behavior, see `rray_sum_dbl_one()`.
static inline double rray_prod_dbl_one(double out, double x) {
  return rray_multiply_dbl_one(out, x);
}

// `out` is never missing
// - `out = 2`, `x = 3`: returns `6`.
// - `out = 2`, `x = NA`: returns `2`.
// - `out = 2`, `x = NaN`: returns `2`.
static inline double rray_prod_dbl_one_na_rm(double out, double x) {
  if (rray_dbl_is_missing(x)) {
    return out;
  }

  return out * x;
}

// Purposefully choose to match `rray_multiply()` rather than `prod()`, which
// doesn't recover infinities
static inline r_complex rray_prod_cpl_one(r_complex out, r_complex x) {
  return rray_multiply_cpl_one(out, x);
}

// The whole element is removed
// - `out = 1+1i`, `x = 2+0i`: returns `2+2i`.
// - `out = 1+1i`, `x = NA`: returns `1+1i`.
// - `out = 1+1i`, `x = complex(real = 2, imaginary = NaN)`: returns `1+1i`.
static inline r_complex rray_prod_cpl_one_na_rm(r_complex out, r_complex x) {
  if (rray_cpl_is_missing(x)) {
    return out;
  }

  return rray_prod_cpl_one(out, x);
}

#endif
