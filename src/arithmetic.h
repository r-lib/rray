#ifndef RRAY_ARITHMETIC_H
#define RRAY_ARITHMETIC_H

#include "rlang.h"

#include "arg.h"
#include "strided-iterator.h"

typedef r_obj* (*rray_binary_arithmetic_fn)(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);

typedef rray_binary_arithmetic_fn (*rray_binary_arithmetic_switch_fn)(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

r_obj* rray_binary_arithmetic(
  r_obj* x,
  r_obj* y,
  rray_binary_arithmetic_switch_fn fn_switch,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

typedef r_obj* (*rray_binary_arithmetic_run_fn)(
  r_obj* x,
  const int* v_x_dimensions,
  int x_dimensionality,
  r_obj* y,
  const int* v_y_dimensions,
  int y_dimensionality,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
);

typedef rray_binary_arithmetic_run_fn (*rray_binary_arithmetic_run_switch_fn)(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

r_obj* rray_binary_arithmetic_run(
  r_obj* x,
  r_obj* y,
  rray_binary_arithmetic_run_switch_fn fn_switch,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

r_no_return void stop_unsupported_arithmetic(
  const char* op,
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

r_no_return void stop_int_overflow(struct r_lazy error_call);

#endif
