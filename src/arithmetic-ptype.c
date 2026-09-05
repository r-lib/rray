#include "arithmetic-ptype.h"

#include "ptype.h"
#include "type.h"

#include "decl/arithmetic-ptype-decl.h"

r_obj* rray_binary_arithmetic_ptype(
  enum rray_binary_arithmetic_op op,
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  enum rray_side side;
  r_obj* ptype = rray_ptype2(x, y, &side, x_arg, y_arg, error_call);

  switch (rray_typeof(ptype)) {
  case RRAY_TYPE_logical:
  case RRAY_TYPE_integer:
    switch (op) {
    case RRAY_BINARY_ARITHMETIC_OP_add:
    case RRAY_BINARY_ARITHMETIC_OP_subtract:
    case RRAY_BINARY_ARITHMETIC_OP_multiply:
    case RRAY_BINARY_ARITHMETIC_OP_modulo:
    case RRAY_BINARY_ARITHMETIC_OP_integer_divide:
      return r_globals.empty_int;
    case RRAY_BINARY_ARITHMETIC_OP_divide:
    case RRAY_BINARY_ARITHMETIC_OP_power:
      return r_globals.empty_dbl;
    }
    r_stop_unreachable();

  case RRAY_TYPE_double:
    return r_globals.empty_dbl;

  case RRAY_TYPE_complex:
    switch (op) {
    case RRAY_BINARY_ARITHMETIC_OP_add:
    case RRAY_BINARY_ARITHMETIC_OP_subtract:
    case RRAY_BINARY_ARITHMETIC_OP_multiply:
    case RRAY_BINARY_ARITHMETIC_OP_divide:
    case RRAY_BINARY_ARITHMETIC_OP_power:
      return r_globals.empty_cpl;
    case RRAY_BINARY_ARITHMETIC_OP_modulo:
    case RRAY_BINARY_ARITHMETIC_OP_integer_divide:
      stop_unsupported_binary_arithmetic_op(op, x, y, x_arg, y_arg, error_call);
    }
    r_stop_unreachable();

  case RRAY_TYPE_character:
  case RRAY_TYPE_raw:
  case RRAY_TYPE_list:
  case RRAY_TYPE_scalar:
    stop_unsupported_binary_arithmetic_op(op, x, y, x_arg, y_arg, error_call);
  }

  r_stop_unreachable();
}

static r_no_return void stop_unsupported_binary_arithmetic_op(
  enum rray_binary_arithmetic_op op,
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  r_abort_lazy_call(
    error_call,
    "Can't apply `%s` to %s and %s.",
    rray_binary_arithmetic_op_as_c_string(op),
    rray_arg_type_format(x_arg, rray_typeof(x)),
    rray_arg_type_format(y_arg, rray_typeof(y))
  );
}

static const char* rray_binary_arithmetic_op_as_c_string(
  enum rray_binary_arithmetic_op op
) {
  switch (op) {
  case RRAY_BINARY_ARITHMETIC_OP_add:
    return "+";
  case RRAY_BINARY_ARITHMETIC_OP_subtract:
    return "-";
  case RRAY_BINARY_ARITHMETIC_OP_multiply:
    return "*";
  case RRAY_BINARY_ARITHMETIC_OP_divide:
    return "/";
  case RRAY_BINARY_ARITHMETIC_OP_power:
    return "^";
  case RRAY_BINARY_ARITHMETIC_OP_modulo:
    return "%%";
  case RRAY_BINARY_ARITHMETIC_OP_integer_divide:
    return "%/%";
  }

  r_stop_unreachable();
}
