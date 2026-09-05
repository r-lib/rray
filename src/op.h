#ifndef RRAY_OP_H
#define RRAY_OP_H

enum rray_binary_op {
  RRAY_BINARY_OP_add,
  RRAY_BINARY_OP_subtract,
  RRAY_BINARY_OP_multiply,
  RRAY_BINARY_OP_divide,
  RRAY_BINARY_OP_power,
  RRAY_BINARY_OP_modulo,
  RRAY_BINARY_OP_integer_divide
};

const char* rray_binary_op_as_c_string(enum rray_binary_op op);

#endif
