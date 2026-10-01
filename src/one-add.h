#ifndef RRAY_ONE_ADD_H
#define RRAY_ONE_ADD_H

#include <limits.h>

#include "rlang.h"

#include "arithmetic.h"
#include "missing.h"

// --------------------------------------------------------------------------
// Elementwise

// - `x = 1`, `y = 1`: returns `2`.
// - `x = 1`, `y = NA`: returns `NA`.
// - `x = NA`, `y = INT_MAX`: returns `NA`.
// - `x = 1`, `y = INT_MAX`: errors.
static inline int rray_add_int_one(int x, int y, struct r_lazy error_call) {
  if (rray_int_is_missing(x) || rray_int_is_missing(y)) {
    return r_globals.na_int;
  }

  if ((y > 0 && x > INT_MAX - y) || (y < 0 && x < -INT_MAX - y)) {
    stop_int_overflow(error_call);
  }

  return x + y;
}

static inline double rray_add_dbl_one(double x, double y) {
  return x + y;
}

static inline r_complex rray_add_cpl_one(r_complex x, r_complex y) {
  return (r_complex) {.r = x.r + y.r, .i = x.i + y.i};
}

// --------------------------------------------------------------------------
// Reduce

// `out` is an integer, `x` is a logical
// - `out = 1`, `x = 1`: returns `2`.
// - `out = 1`, `x = NA`: returns `NA`.
// - `out = NA`, `x = 1`: returns `NA`.
static inline int rray_sum_lgl_one(int out, int x) {
  if (rray_int_is_missing(out)) {
    return r_globals.na_int;
  }

  if (rray_lgl_is_missing(x)) {
    return r_globals.na_int;
  }

  // Since long vectors aren't supported in arrays,
  // we can't ever integer overflow in a logical array

  return out + x;
}

// `out` is an integer that is never `NA`, `x` is a logical
// - `out = 1`, `x = 1`: returns `2`.
// - `out = 1`, `x = NA`: returns `1`.
static inline int rray_sum_lgl_one_na_rm(int out, int x) {
  if (rray_lgl_is_missing(x)) {
    return out;
  }

  return out + x;
}

static inline int rray_sum_int_one(int out, int x, struct r_lazy error_call) {
  return rray_add_int_one(out, x, error_call);
}

// `out` is never `NA`
// - `out = 1`, `x = 1`: returns `2`.
// - `out = 1`, `x = NA`: returns `1`.
// - `out = 1`, `x = INT_MAX`: errors.
static inline int rray_sum_int_one_na_rm(
  int out,
  int x,
  struct r_lazy error_call
) {
  if (rray_int_is_missing(x)) {
    return out;
  }

  if ((x > 0 && out > INT_MAX - x) || (x < 0 && out < -INT_MAX - x)) {
    stop_int_overflow(error_call);
  }

  return out + x;
}

// Purposefully choose to match `rray_add()` rather than `sum()` regarding
// `c(NA, NaN)` behavior. Base R `sum()` forces `NA` if present, but `+`
// doesn't, so R is inconsistent. It's much faster to avoid checking for this,
// so we just say "it's implementation defined" for both add and sum in rray.
static inline double rray_sum_dbl_one(double out, double x) {
  return rray_add_dbl_one(out, x);
}

// `out` is never missing
// - `out = 1`, `x = 2`: returns `3`.
// - `out = 1`, `x = NA`: returns `1`.
// - `out = 1`, `x = NaN`: returns `1`.
static inline double rray_sum_dbl_one_na_rm(double out, double x) {
  if (rray_dbl_is_missing(x)) {
    return out;
  }

  return out + x;
}

static inline r_complex rray_sum_cpl_one(r_complex out, r_complex x) {
  return rray_add_cpl_one(out, x);
}

// Each part is removed on its own
// - `out = 1+1i`, `x = 2+2i`: returns `3+3i`.
// - `out = 1+1i`, `x = NA`: returns `1+1i`.
// - `out = 1+1i`, `x = complex(real = 2, imaginary = NaN)`: returns `3+1i`.
static inline r_complex rray_sum_cpl_one_na_rm(r_complex out, r_complex x) {
  return (r_complex) {
    .r = rray_sum_dbl_one_na_rm(out.r, x.r),
    .i = rray_sum_dbl_one_na_rm(out.i, x.i),
  };
}

#endif
