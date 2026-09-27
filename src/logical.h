#ifndef RRAY_LOGICAL_H
#define RRAY_LOGICAL_H

#include "rlang.h"

#include "arg.h"
#include "missing.h"
#include "utils.h"

r_obj* rray_and(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

r_obj* rray_or(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

r_obj* rray_xor(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

void check_logical(r_obj* x, struct rray_arg* arg, struct r_lazy error_call);

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

#endif
