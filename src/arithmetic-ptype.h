#ifndef RRAY_ARITHMETIC_PTYPE_H
#define RRAY_ARITHMETIC_PTYPE_H

#include "rlang.h"

#include "arg.h"

enum rray_binary_arithmetic_op {
  RRAY_BINARY_ARITHMETIC_OP_add,
  RRAY_BINARY_ARITHMETIC_OP_subtract,
  RRAY_BINARY_ARITHMETIC_OP_multiply,
  RRAY_BINARY_ARITHMETIC_OP_divide,
  RRAY_BINARY_ARITHMETIC_OP_power,
  RRAY_BINARY_ARITHMETIC_OP_modulo,
  RRAY_BINARY_ARITHMETIC_OP_integer_divide
};

r_obj* rray_binary_arithmetic_ptype(
  enum rray_binary_arithmetic_op op,
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

#endif
