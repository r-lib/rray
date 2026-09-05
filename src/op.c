#include "op.h"

#include "rlang.h"

const char* rray_binary_op_as_c_string(enum rray_binary_op op) {
  switch (op) {
  case RRAY_BINARY_OP_add:
    return "+";
  case RRAY_BINARY_OP_subtract:
    return "-";
  case RRAY_BINARY_OP_multiply:
    return "*";
  case RRAY_BINARY_OP_divide:
    return "/";
  case RRAY_BINARY_OP_power:
    return "^";
  case RRAY_BINARY_OP_modulo:
    return "%%";
  case RRAY_BINARY_OP_integer_divide:
    return "%/%";
  }

  r_stop_unreachable();
}
