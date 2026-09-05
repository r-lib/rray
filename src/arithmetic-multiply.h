#ifndef RRAY_ARITHMETIC_MULTIPLY_H
#define RRAY_ARITHMETIC_MULTIPLY_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_multiply(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

#endif
