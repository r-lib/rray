#ifndef RRAY_ARITHMETIC_H
#define RRAY_ARITHMETIC_H

#include "rlang.h"

#include "arg.h"
#include "iterator.h"

typedef r_obj* (*rray_binary_arithmetic_fn)(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
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

r_no_return void stop_int_overflow(struct r_lazy error_call);

#endif
