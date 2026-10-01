#ifndef RRAY_ONE_LOGICAL_H
#define RRAY_ONE_LOGICAL_H

#include "rlang.h"

#include "missing.h"
#include "utils.h"

// --------------------------------------------------------------------------
// Elementwise

static inline int rray_and_lgl_one(int x, int y) {
  const bool any_false = !x || !y;
  const bool equal = x == y;
  return !any_false * (equal * x + !equal * r_globals.na_lgl);
}

static inline int rray_or_lgl_one(int x, int y) {
  const bool any_true = (x == 1) || (y == 1);
  const bool equal = x == y;
  return any_true + !any_true * (equal * x + !equal * r_globals.na_lgl);
}

static inline int rray_xor_lgl_one(int x, int y) {
  const bool missing =
    bool_bitwise_or(rray_lgl_is_missing(x), rray_lgl_is_missing(y));
  const int elt = x != y;
  return missing ? r_globals.na_lgl : elt;
}

// --------------------------------------------------------------------------
// Reduce

static inline int rray_all_lgl_one(int out, int x) {
  return rray_and_lgl_one(out, x);
}

// Logical `NA` is `INT_MIN`, and `out` is never `NA`
// - `out = 1`, `x = 1`: returns `1`.
// - `out = 1`, `x = 0`: returns `0`.
// - `out = 1`, `x = NA`: returns `1`.
// - `out = 0`, `x = 1`: returns `0`.
static inline int rray_all_lgl_one_na_rm(int out, int x) {
  return bool_bitwise_and(out, x != 0);
}

static inline int rray_any_lgl_one(int out, int x) {
  return rray_or_lgl_one(out, x);
}

// Logical `NA` is `INT_MIN`, and `out` is never `NA`
// - `out = 0`, `x = 0`: returns `0`.
// - `out = 0`, `x = 1`: returns `1`.
// - `out = 0`, `x = NA`: returns `0`.
// - `out = 1`, `x = 0`: returns `1`.
static inline int rray_any_lgl_one_na_rm(int out, int x) {
  return bool_bitwise_or(out, x == 1);
}

#endif
