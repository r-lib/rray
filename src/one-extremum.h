#ifndef RRAY_ONE_EXTREMUM_H
#define RRAY_ONE_EXTREMUM_H

#include "rlang.h"

#include "missing.h"
#include "utils.h"

// Each of the "one" functions below is carefully tuned to maximize
// the chance of the resulting loop being vectorized by the compiler.
// Anecdotally tested on an M2 Mac, where the assembly was analyzed.
// For doubles, the double switch is less likely to vectorize but seems
// to be the most stable when reducing across different combinations of
// axes.

// --------------------------------------------------------------------------
// Elementwise

// See `rray_pmax_int_one()`
static inline int rray_pmax_lgl_one(int x, int y) {
  const int out = x < y ? y : x;
  const bool has_na =
    bool_bitwise_or(rray_lgl_is_missing(x), rray_lgl_is_missing(y));
  return has_na ? r_globals.na_lgl : out;
}

// See `rray_pmax_int_one_na_rm()`
static inline int rray_pmax_lgl_one_na_rm(int x, int y) {
  return x < y ? y : x;
}

// Bitwise `|` improves efficiency here
// - `x = 1`, `y = 1`: returns `1` (`out = 1`, `has_na = false`).
// - `x = 1`, `y = NA`: returns `NA` (`out = 1`, `has_na = true`).
// - `x = NA`, `y = 1`: returns `NA` (`out = 1`, `has_na = true`).
// - `x = NA`, `y = NA`: returns `NA` (`out = NA`, `has_na = true`).
static inline int rray_pmax_int_one(int x, int y) {
  const int out = x < y ? y : x;
  const bool has_na =
    bool_bitwise_or(rray_int_is_missing(x), rray_int_is_missing(y));
  return has_na ? r_globals.na_int : out;
}

// Integer `NA` is `INT_MIN`
// - `x = 1`, `y = 1`: returns `1` (`out = 1`).
// - `x = 1`, `y = NA`: returns `1` (`out = 1`).
// - `x = NA`, `y = 1`: returns `1` (`out = 1`).
// - `x = NA`, `y = NA`: returns `NA` (`out = NA`).
static inline int rray_pmax_int_one_na_rm(int x, int y) {
  return x < y ? y : x;
}

static inline double rray_pmax_dbl_one(double x, double y) {
  switch (rray_dbl_classify(x)) {
  case RRAY_DBL_number: {
    switch (rray_dbl_classify(y)) {
    case RRAY_DBL_number:
      return x < y ? y : x;
    case RRAY_DBL_missing:
      return y;
    case RRAY_DBL_nan:
      return y;
    }
  }
  case RRAY_DBL_missing: {
    return x;
  }
  case RRAY_DBL_nan: {
    switch (rray_dbl_classify(y)) {
    case RRAY_DBL_number:
      return x;
    case RRAY_DBL_missing:
      return y;
    case RRAY_DBL_nan:
      return x;
    }
  }
  }
  r_stop_unreachable();
}

static inline double rray_pmax_dbl_one_na_rm(double x, double y) {
  switch (rray_dbl_classify(x)) {
  case RRAY_DBL_number: {
    switch (rray_dbl_classify(y)) {
    case RRAY_DBL_number:
      return x < y ? y : x;
    case RRAY_DBL_missing:
      return x;
    case RRAY_DBL_nan:
      return x;
    }
  }
  case RRAY_DBL_missing: {
    switch (rray_dbl_classify(y)) {
    case RRAY_DBL_number:
      return y;
    case RRAY_DBL_missing:
      return x;
    case RRAY_DBL_nan:
      return x;
    }
  }
  case RRAY_DBL_nan: {
    switch (rray_dbl_classify(y)) {
    case RRAY_DBL_number:
      return y;
    case RRAY_DBL_missing:
      return y;
    case RRAY_DBL_nan:
      return x;
    }
  }
  }
  r_stop_unreachable();
}

// See `rray_pmin_int_one()`
static inline int rray_pmin_lgl_one(int x, int y) {
  return x > y ? y : x;
}

// See `rray_pmin_int_one_na_rm()`
static inline int rray_pmin_lgl_one_na_rm(int x, int y) {
  int out = x > y ? y : x;
  out = rray_lgl_is_missing(y) ? x : out;
  out = rray_lgl_is_missing(x) ? y : out;
  return out;
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
  int out = x > y ? y : x;
  out = rray_int_is_missing(y) ? x : out;
  out = rray_int_is_missing(x) ? y : out;
  return out;
}

static inline double rray_pmin_dbl_one(double x, double y) {
  switch (rray_dbl_classify(x)) {
  case RRAY_DBL_number: {
    switch (rray_dbl_classify(y)) {
    case RRAY_DBL_number:
      return x > y ? y : x;
    case RRAY_DBL_missing:
      return y;
    case RRAY_DBL_nan:
      return y;
    }
  }
  case RRAY_DBL_missing: {
    return x;
  }
  case RRAY_DBL_nan: {
    switch (rray_dbl_classify(y)) {
    case RRAY_DBL_number:
      return x;
    case RRAY_DBL_missing:
      return y;
    case RRAY_DBL_nan:
      return x;
    }
  }
  }
  r_stop_unreachable();
}

static inline double rray_pmin_dbl_one_na_rm(double x, double y) {
  switch (rray_dbl_classify(x)) {
  case RRAY_DBL_number: {
    switch (rray_dbl_classify(y)) {
    case RRAY_DBL_number:
      return x > y ? y : x;
    case RRAY_DBL_missing:
      return x;
    case RRAY_DBL_nan:
      return x;
    }
  }
  case RRAY_DBL_missing: {
    switch (rray_dbl_classify(y)) {
    case RRAY_DBL_number:
      return y;
    case RRAY_DBL_missing:
      return x;
    case RRAY_DBL_nan:
      return x;
    }
  }
  case RRAY_DBL_nan: {
    switch (rray_dbl_classify(y)) {
    case RRAY_DBL_number:
      return y;
    case RRAY_DBL_missing:
      return y;
    case RRAY_DBL_nan:
      return x;
    }
  }
  }
  r_stop_unreachable();
}

// --------------------------------------------------------------------------
// Reduce

static inline int rray_max_lgl_one(int out, int x) {
  return rray_pmax_lgl_one(out, x);
}

static inline int rray_max_lgl_one_na_rm(int out, int x) {
  return rray_pmax_lgl_one_na_rm(out, x);
}

static inline int rray_max_int_one(int out, int x) {
  return rray_pmax_int_one(out, x);
}

static inline int rray_max_int_one_na_rm(int out, int x) {
  return rray_pmax_int_one_na_rm(out, x);
}

static inline double rray_max_dbl_one(double out, double x) {
  return rray_pmax_dbl_one(out, x);
}

// Comparisons with `NA` or `NaN` are false, and `out` is never missing
// - `out = 1`, `x = 2`: returns `2`.
// - `out = 2`, `x = 1`: returns `2`.
// - `out = 1`, `x = NA`: returns `1`.
// - `out = 1`, `x = NaN`: returns `1`.
static inline double rray_max_dbl_one_na_rm(double out, double x) {
  return out < x ? x : out;
}

static inline int rray_min_lgl_one(int out, int x) {
  return rray_pmin_lgl_one(out, x);
}

// Logical `NA` is `INT_MIN`, and `out` is never `NA`
// - `out = 1`, `x = 0`: returns `0` (`is_less = true`).
// - `out = 0`, `x = 1`: returns `0` (`is_less = false`).
// - `out = 1`, `x = NA`: returns `1` (`is_less = false`).
static inline int rray_min_lgl_one_na_rm(int out, int x) {
  const bool is_less = bool_bitwise_and(x < out, !rray_lgl_is_missing(x));
  return is_less ? x : out;
}

static inline int rray_min_int_one(int out, int x) {
  return rray_pmin_int_one(out, x);
}

// Integer `NA` is `INT_MIN`, and `out` is never `NA`
// - `out = 2`, `x = 1`: returns `1` (`is_less = true`).
// - `out = 1`, `x = 2`: returns `1` (`is_less = false`).
// - `out = 1`, `x = NA`: returns `1` (`is_less = false`).
static inline int rray_min_int_one_na_rm(int out, int x) {
  const bool is_less = bool_bitwise_and(x < out, !rray_int_is_missing(x));
  return is_less ? x : out;
}

static inline double rray_min_dbl_one(double out, double x) {
  return rray_pmin_dbl_one(out, x);
}

// Comparisons with `NA` or `NaN` are false, and `out` is never missing
// - `out = 2`, `x = 1`: returns `1`.
// - `out = 1`, `x = 2`: returns `1`.
// - `out = 1`, `x = NA`: returns `1`.
// - `out = 1`, `x = NaN`: returns `1`.
static inline double rray_min_dbl_one_na_rm(double out, double x) {
  return out > x ? x : out;
}

#endif
