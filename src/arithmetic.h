#ifndef RRAY_ARITHMETIC_H
#define RRAY_ARITHMETIC_H

#include "rlang.h"

#include "arg.h"
#include "op.h"

r_obj* rray_arithmetic(
  enum rray_binary_op op,
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

#endif
