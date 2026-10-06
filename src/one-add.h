#ifndef RRAY_ONE_ADD_H
#define RRAY_ONE_ADD_H

#include <limits.h>

#include "rlang.h"

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

  if ((y > 0 && x > INT_MAX - y) || (y < 0 && x < -INT_MAX - y)) {
    stop_int_overflow(error_call);
  }

  return x + y;
}

// Purposefully choose to match `rray_add()` rather than `sum()` regarding
// `c(NA, NaN)` behavior. Base R `sum()` forces `NA` if present, but `+`
// doesn't, so R is inconsistent. It's much faster to avoid checking for this,
// so we just say "it's implementation defined" for both add and sum in rray.
static inline double rray_add_dbl_one(double x, double y) {
  return x + y;
}

static inline r_complex rray_add_cpl_one(r_complex x, r_complex y) {
  return (r_complex) {.r = x.r + y.r, .i = x.i + y.i};
}

#endif
