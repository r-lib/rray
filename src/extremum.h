#ifndef RRAY_EXTREMUM_H
#define RRAY_EXTREMUM_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_pmax(
  r_obj* x,
  r_obj* y,
  bool na_rm,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

r_obj* rray_pmin(
  r_obj* x,
  r_obj* y,
  bool na_rm,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

// Each of the "one" functions below is carefully tuned to maximize
// the chance of the resulting loop being vectorized by the compiler.
// Anecdotally tested on an M2 Mac, where the assembly was analyzed.

// Bitwise `|` improves efficiency here
// - `x = 1`, `y = 1`: returns `1` (`out = 1`, `has_na = false`).
// - `x = 1`, `y = NA`: returns `NA` (`out = 1`, `has_na = true`).
// - `x = NA`, `y = 1`: returns `NA` (`out = 1`, `has_na = true`).
// - `x = NA`, `y = NA`: returns `NA` (`out = NA`, `has_na = true`).
static inline int rray_pmax_int_one(int x, int y) {
  const int out = x < y ? y : x;
  const int na = r_globals.na_int;
  const bool has_na = (x == na) | (y == na);
  return has_na ? na : out;
}

// Integer `NA` is `INT_MIN`
// - `x = 1`, `y = 1`: returns `1` (`out = 1`).
// - `x = 1`, `y = NA`: returns `1` (`out = 1`).
// - `x = NA`, `y = 1`: returns `1` (`out = 1`).
// - `x = NA`, `y = NA`: returns `NA` (`out = NA`).
static inline int rray_pmax_int_one_na_rm(int x, int y) {
  return x < y ? y : x;
}

// A C comparison involving a `NaN`/`NA_real_` is false, so `out` selects `x`.
// `ISNAN(y)` propagates missing `y` and also ensures the second missing value
// wins when both inputs are missing (matching R):
// - `x = 1`, `y = 1`: returns `1` (`out = 1`, `ISNAN(y) = false`).
// - `x = 1`, `y = NaN`: returns `NaN` (`out = 1`, `ISNAN(y) = true`).
// - `x = 1`, `y = NA_real_`: returns `NA_real_` (`out = 1`, `ISNAN(y) = true`).
// - `x = NaN`, `y = 1`: returns `NaN` (`out = NaN`, `ISNAN(y) = false`).
// - `x = NaN`, `y = NaN`: returns `y` (`out = x`, `ISNAN(y) = true`).
// - `x = NaN`, `y = NA_real_`: returns `NA_real_` (`out = NaN`, `ISNAN(y) =
// true`).
// - `x = NA_real_`, `y = 1`: returns `NA_real_` (`out = NA_real_`, `ISNAN(y) =
// false`).
// - `x = NA_real_`, `y = NaN`: returns `NaN` (`out = NA_real_`, `ISNAN(y) =
// true`).
// - `x = NA_real_`, `y = NA_real_`: returns `y` (`out = x`, `ISNAN(y) = true`).
static inline double rray_pmax_dbl_one(double x, double y) {
  const double out = x < y ? y : x;
  return ISNAN(y) ? y : out;
}

// A C comparison involving a `NaN`/`NA_real_` is false, so `out` selects `x`.
// `ISNAN(x)` replaces `out` with `y` when `x` is missing but `y` is not and
// when both inputs are missing (matching R):
// - `x = 1`, `y = 1`: returns `1` (`out = 1`, `ISNAN(x) = false`).
// - `x = 1`, `y = NaN`: returns `1` (`out = 1`, `ISNAN(x) = false`).
// - `x = 1`, `y = NA_real_`: returns `1` (`out = 1`, `ISNAN(x) = false`).
// - `x = NaN`, `y = 1`: returns `1` (`out = NaN`, `ISNAN(x) = true`).
// - `x = NaN`, `y = NaN`: returns `y` (`out = x`, `ISNAN(x) = true`).
// - `x = NaN`, `y = NA_real_`: returns `NA_real_` (`out = NaN`, `ISNAN(x) =
// true`).
// - `x = NA_real_`, `y = 1`: returns `1` (`out = NA_real_`, `ISNAN(x) = true`).
// - `x = NA_real_`, `y = NaN`: returns `NaN` (`out = NA_real_`, `ISNAN(x) =
// true`).
// - `x = NA_real_`, `y = NA_real_`: returns `y` (`out = x`, `ISNAN(x) = true`).
static inline double rray_pmax_dbl_one_na_rm(double x, double y) {
  const double out = x < y ? y : x;
  return ISNAN(x) ? y : out;
}

// Integer `NA` is `INT_MIN`
// - `x = 1`, `y = 1`: returns `1` (`out = 1`).
// - `x = 1`, `y = NA`: returns `NA` (`out = NA`).
// - `x = NA`, `y = 1`: returns `NA` (`out = NA`).
// - `x = NA`, `y = NA`: returns `NA` (`out = NA`).
static inline int rray_pmin_int_one(int x, int y) {
  return x > y ? y : x;
}

// A regular minimum selects `NA`, so each missing operand is replaced by the
// other operand:
// - `x = 1`, `y = 1`: returns `1` (`out = 1`, neither replacement applies).
// - `x = 1`, `y = NA`: returns `1` (`out = NA`, replace `y` with `x`).
// - `x = NA`, `y = 1`: returns `1` (`out = NA`, replace `x` with `y`).
// - `x = NA`, `y = NA`: returns `NA` (`out = NA`, both replacements are `NA`).
static inline int rray_pmin_int_one_na_rm(int x, int y) {
  const int na = r_globals.na_int;
  int out = x > y ? y : x;
  out = y == na ? x : out;
  out = x == na ? y : out;
  return out;
}

// Same as `rray_pmax_dbl_one()`
// - `x = 1`, `y = 1`: returns `1` (`out = 1`, `ISNAN(y) = false`).
// - `x = 1`, `y = NaN`: returns `NaN` (`out = 1`, `ISNAN(y) = true`).
// - `x = 1`, `y = NA_real_`: returns `NA_real_` (`out = 1`, `ISNAN(y) = true`).
// - `x = NaN`, `y = 1`: returns `NaN` (`out = NaN`, `ISNAN(y) = false`).
// - `x = NaN`, `y = NaN`: returns `y` (`out = x`, `ISNAN(y) = true`).
// - `x = NaN`, `y = NA_real_`: returns `NA_real_` (`out = NaN`, `ISNAN(y) =
// true`).
// - `x = NA_real_`, `y = 1`: returns `NA_real_` (`out = NA_real_`, `ISNAN(y) =
// false`).
// - `x = NA_real_`, `y = NaN`: returns `NaN` (`out = NA_real_`, `ISNAN(y) =
// true`).
// - `x = NA_real_`, `y = NA_real_`: returns `y` (`out = x`, `ISNAN(y) = true`).
static inline double rray_pmin_dbl_one(double x, double y) {
  const double out = x > y ? y : x;
  return ISNAN(y) ? y : out;
}

// Same as `rray_pmax_dbl_one_na_rm()`
// - `x = 1`, `y = 1`: returns `1` (`out = 1`, `ISNAN(x) = false`).
// - `x = 1`, `y = NaN`: returns `1` (`out = 1`, `ISNAN(x) = false`).
// - `x = 1`, `y = NA_real_`: returns `1` (`out = 1`, `ISNAN(x) = false`).
// - `x = NaN`, `y = 1`: returns `1` (`out = NaN`, `ISNAN(x) = true`).
// - `x = NaN`, `y = NaN`: returns `y` (`out = x`, `ISNAN(x) = true`).
// - `x = NaN`, `y = NA_real_`: returns `NA_real_` (`out = NaN`, `ISNAN(x) =
// true`).
// - `x = NA_real_`, `y = 1`: returns `1` (`out = NA_real_`, `ISNAN(x) = true`).
// - `x = NA_real_`, `y = NaN`: returns `NaN` (`out = NA_real_`, `ISNAN(x) =
// true`).
// - `x = NA_real_`, `y = NA_real_`: returns `y` (`out = x`, `ISNAN(x) = true`).
static inline double rray_pmin_dbl_one_na_rm(double x, double y) {
  const double out = x > y ? y : x;
  return ISNAN(x) ? y : out;
}

#endif
