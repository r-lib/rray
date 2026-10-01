#ifndef RRAY_ONE_LOGICAL_H
#define RRAY_ONE_LOGICAL_H

#include "rlang.h"

#include "missing.h"
#include "utils.h"

static inline int rray_and_lgl_one(int x, int y) {
  const bool any_false = !x || !y;
  const bool equal = x == y;
  return !any_false * (equal * x + !equal * r_globals.na_lgl);
}

static inline int rray_and_lgl_one_na_rm(int x, int y) {
  return x && y;
}

static inline int rray_or_lgl_one(int x, int y) {
  const bool any_true = (x == 1) || (y == 1);
  const bool equal = x == y;
  return any_true + !any_true * (equal * x + !equal * r_globals.na_lgl);
}

static inline int rray_or_lgl_one_na_rm(int x, int y) {
  return (x == 1) || (y == 1);
}

static inline int rray_xor_lgl_one(int x, int y) {
  const bool missing =
    bool_bitwise_or(rray_lgl_is_missing(x), rray_lgl_is_missing(y));
  const int elt = x != y;
  return missing ? r_globals.na_lgl : elt;
}

#endif
