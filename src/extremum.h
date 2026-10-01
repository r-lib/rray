#ifndef RRAY_EXTREMUM_H
#define RRAY_EXTREMUM_H

#include "rlang.h"

#include "arg.h"
#include "missing.h"

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
// For doubles, the double switch is less likely to vectorize but seems
// to be the most stable when reducing across different combinations of
// axes.

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

#endif
