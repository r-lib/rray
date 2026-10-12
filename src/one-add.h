#ifndef RRAY_ONE_ADD_H
#define RRAY_ONE_ADD_H

#include <limits.h>
#include <stdint.h>

#include "rlang.h"

#include "cast.h"
#include "missing.h"
#include "utils.h"

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

  const int64_t out = (int64_t) x + (int64_t) y;

  if (out > INT_MAX || out < -INT_MAX) {
    stop_int_overflow(error_call);
  }

  return (int) out;
}

static inline int rray_add_lgl_one(int out, int x, struct r_lazy error_call) {
  return rray_add_int_one(out, rray_cast_lgl_to_int_one(x), error_call);
}

// For `c(NA, NaN)` behavior, match `+` and let the order be "implementation
// defined" which is very fast. We consistently do this for both `rray_add()`
// and `rray_sum()`, while R itself only does this for `+`. `sum()` prefers
// `NA` when both are present.
static inline double rray_add_dbl_one(double x, double y) {
  return x + y;
}

static inline r_complex rray_add_cpl_one(r_complex x, r_complex y) {
  return (r_complex) {.r = x.r + y.r, .i = x.i + y.i};
}

#endif
