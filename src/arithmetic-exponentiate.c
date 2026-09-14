#include "arithmetic-exponentiate.h"

#include <Rmath.h>

#include "arithmetic.h"
#include "binary.h"
#include "cast.h"
#include "type.h"
#include "typeof2.h"
#include "utils.h"

#include "decl/arithmetic-exponentiate-decl.h"

r_obj* ffi_rray_exponentiate(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_exponentiate(ffi_x, ffi_y, rray_args.x, rray_args.y, error_call);
}

r_obj* rray_exponentiate(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  return rray_binary_arithmetic(
    x,
    y,
    rray_exponentiate_switch,
    x_arg,
    y_arg,
    error_call
  );
}

static rray_binary_arithmetic_fn rray_exponentiate_switch(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  const enum rray_type x_type = rray_typeof(x);
  const enum rray_type y_type = rray_typeof(y);

  enum rray_side side;

  switch (rray_typeof2(x_type, y_type, &side)) {
  case RRAY_TYPE2_logical_logical:
    return rray_exponentiate_lgl_lgl;
  case RRAY_TYPE2_logical_integer:
    return (side == RRAY_SIDE_right) ? rray_exponentiate_lgl_int
                                     : rray_exponentiate_int_lgl;
  case RRAY_TYPE2_logical_double:
    return (side == RRAY_SIDE_right) ? rray_exponentiate_lgl_dbl
                                     : rray_exponentiate_dbl_lgl;
  case RRAY_TYPE2_integer_integer:
    return rray_exponentiate_int_int;
  case RRAY_TYPE2_integer_double:
    return (side == RRAY_SIDE_right) ? rray_exponentiate_int_dbl
                                     : rray_exponentiate_dbl_int;
  case RRAY_TYPE2_double_double:
    return rray_exponentiate_dbl_dbl;

  case RRAY_TYPE2_logical_complex:
  case RRAY_TYPE2_logical_character:
  case RRAY_TYPE2_logical_raw:
  case RRAY_TYPE2_logical_list:
  case RRAY_TYPE2_integer_complex:
  case RRAY_TYPE2_integer_character:
  case RRAY_TYPE2_integer_raw:
  case RRAY_TYPE2_integer_list:
  case RRAY_TYPE2_double_complex:
  case RRAY_TYPE2_double_character:
  case RRAY_TYPE2_double_raw:
  case RRAY_TYPE2_double_list:
  case RRAY_TYPE2_complex_complex:
  case RRAY_TYPE2_complex_character:
  case RRAY_TYPE2_complex_raw:
  case RRAY_TYPE2_complex_list:
  case RRAY_TYPE2_character_character:
  case RRAY_TYPE2_character_raw:
  case RRAY_TYPE2_character_list:
  case RRAY_TYPE2_raw_raw:
  case RRAY_TYPE2_raw_list:
  case RRAY_TYPE2_list_list:
    stop_unsupported_arithmetic("^", x, y, x_arg, y_arg, error_call);

  case RRAY_TYPE2_logical_scalar:
  case RRAY_TYPE2_integer_scalar:
  case RRAY_TYPE2_double_scalar:
  case RRAY_TYPE2_complex_scalar:
  case RRAY_TYPE2_character_scalar:
  case RRAY_TYPE2_raw_scalar:
  case RRAY_TYPE2_list_scalar:
  case RRAY_TYPE2_scalar_scalar:
    if (x_type == RRAY_TYPE_scalar) {
      stop_scalar_input(x, x_arg, error_call);
    } else {
      stop_scalar_input(y, y_arg, error_call);
    }
  }

  r_stop_unreachable();
}

static r_obj* rray_exponentiate_lgl_lgl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
) {
  RRAY_BINARY(
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_dbl_one,
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_exponentiate_dbl_one,
    RRAY_BINARY_ARGS(error_call)
  );
}

static r_obj* rray_exponentiate_lgl_int(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
) {
  RRAY_BINARY(
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_dbl_one,
    int,
    r_int_cbegin,
    rray_cast_int_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_exponentiate_dbl_one,
    RRAY_BINARY_ARGS(error_call)
  );
}

static r_obj* rray_exponentiate_int_lgl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
) {
  RRAY_BINARY(
    int,
    r_int_cbegin,
    rray_cast_int_to_dbl_one,
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_exponentiate_dbl_one,
    RRAY_BINARY_ARGS(error_call)
  );
}

static r_obj* rray_exponentiate_lgl_dbl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
) {
  RRAY_BINARY(
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_dbl_one,
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_exponentiate_dbl_one,
    RRAY_BINARY_ARGS(error_call)
  );
}

static r_obj* rray_exponentiate_dbl_lgl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
) {
  RRAY_BINARY(
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_exponentiate_dbl_one,
    RRAY_BINARY_ARGS(error_call)
  );
}

static r_obj* rray_exponentiate_int_int(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
) {
  RRAY_BINARY(
    int,
    r_int_cbegin,
    rray_cast_int_to_dbl_one,
    int,
    r_int_cbegin,
    rray_cast_int_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_exponentiate_dbl_one,
    RRAY_BINARY_ARGS(error_call)
  );
}

static r_obj* rray_exponentiate_int_dbl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
) {
  RRAY_BINARY(
    int,
    r_int_cbegin,
    rray_cast_int_to_dbl_one,
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_exponentiate_dbl_one,
    RRAY_BINARY_ARGS(error_call)
  );
}

static r_obj* rray_exponentiate_dbl_int(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
) {
  RRAY_BINARY(
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    int,
    r_int_cbegin,
    rray_cast_int_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_exponentiate_dbl_one,
    RRAY_BINARY_ARGS(error_call)
  );
}

static r_obj* rray_exponentiate_dbl_dbl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
) {
  RRAY_BINARY(
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_exponentiate_dbl_one,
    RRAY_BINARY_ARGS(error_call)
  );
}

static inline double rray_exponentiate_dbl_one(
  double x,
  double y,
  struct r_lazy error_call
) {
  return R_pow(x, y);
}
