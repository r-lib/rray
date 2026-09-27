#ifndef RRAY_LOGICAL_H
#define RRAY_LOGICAL_H

#include "rlang.h"

#include "arg.h"
#include "missing.h"
#include "type.h"
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

static inline void check_logical(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  switch (rray_typeof(x)) {
  case RRAY_TYPE_logical:
    return;

  case RRAY_TYPE_integer:
  case RRAY_TYPE_double:
  case RRAY_TYPE_complex:
  case RRAY_TYPE_character:
  case RRAY_TYPE_raw:
  case RRAY_TYPE_list:
  case RRAY_TYPE_scalar:
    r_abort_lazy_call(
      error_call,
      "%s must be a logical array, not %s.",
      rray_arg_format_input(arg),
      r_obj_type_friendly(x)
    );
  }

  r_stop_unreachable();
}

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
