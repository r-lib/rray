#ifndef RRAY_EQUAL_H
#define RRAY_EQUAL_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_equal(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

r_obj* rray_not_equal(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

static inline bool rray_cpl_is_missing(r_complex x) {
  return ISNAN(x.r) || ISNAN(x.i);
}

#endif
