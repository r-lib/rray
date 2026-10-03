#include "arithmetic-divide.h"

#include "arithmetic.h"
#include "binary.h"
#include "cast.h"
#include "type.h"
#include "typeof2.h"
#include "utils.h"

#include "decl/arithmetic-divide-decl.h"

r_obj* ffi_rray_divide(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_divide(ffi_x, ffi_y, rray_args.x, rray_args.y, error_call);
}

r_obj* rray_divide(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  return rray_binary_arithmetic(
    x,
    y,
    rray_divide_switch,
    x_arg,
    y_arg,
    error_call
  );
}

static rray_binary_arithmetic_fn rray_divide_switch(
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
    return rray_divide_lgl_lgl;
  case RRAY_TYPE2_logical_integer:
    return (side == RRAY_SIDE_right) ? rray_divide_lgl_int
                                     : rray_divide_int_lgl;
  case RRAY_TYPE2_logical_double:
    return (side == RRAY_SIDE_right) ? rray_divide_lgl_dbl
                                     : rray_divide_dbl_lgl;
  case RRAY_TYPE2_logical_complex:
    return (side == RRAY_SIDE_right) ? rray_divide_lgl_cpl
                                     : rray_divide_cpl_lgl;
  case RRAY_TYPE2_integer_integer:
    return rray_divide_int_int;
  case RRAY_TYPE2_integer_double:
    return (side == RRAY_SIDE_right) ? rray_divide_int_dbl
                                     : rray_divide_dbl_int;
  case RRAY_TYPE2_integer_complex:
    return (side == RRAY_SIDE_right) ? rray_divide_int_cpl
                                     : rray_divide_cpl_int;
  case RRAY_TYPE2_double_double:
    return rray_divide_dbl_dbl;
  case RRAY_TYPE2_double_complex:
    return (side == RRAY_SIDE_right) ? rray_divide_dbl_cpl
                                     : rray_divide_cpl_dbl;
  case RRAY_TYPE2_complex_complex:
    return rray_divide_cpl_cpl;

  case RRAY_TYPE2_logical_character:
  case RRAY_TYPE2_logical_raw:
  case RRAY_TYPE2_logical_list:
  case RRAY_TYPE2_integer_character:
  case RRAY_TYPE2_integer_raw:
  case RRAY_TYPE2_integer_list:
  case RRAY_TYPE2_double_character:
  case RRAY_TYPE2_double_raw:
  case RRAY_TYPE2_double_list:
  case RRAY_TYPE2_complex_character:
  case RRAY_TYPE2_complex_raw:
  case RRAY_TYPE2_complex_list:
  case RRAY_TYPE2_character_character:
  case RRAY_TYPE2_character_raw:
  case RRAY_TYPE2_character_list:
  case RRAY_TYPE2_raw_raw:
  case RRAY_TYPE2_raw_list:
  case RRAY_TYPE2_list_list:
    stop_unsupported_arithmetic("/", x, y, x_arg, y_arg, error_call);

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

static r_obj* rray_divide_lgl_lgl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
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
    rray_divide_dbl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_divide_lgl_int(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
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
    rray_divide_dbl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_divide_int_lgl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
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
    rray_divide_dbl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_divide_lgl_dbl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
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
    rray_divide_dbl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_divide_dbl_lgl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
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
    rray_divide_dbl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_divide_lgl_cpl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
) {
  RRAY_BINARY(
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_cpl_one,
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    rray_divide_cpl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_divide_cpl_lgl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
) {
  RRAY_BINARY(
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_cpl_one,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    rray_divide_cpl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_divide_int_int(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
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
    rray_divide_dbl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_divide_int_dbl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
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
    rray_divide_dbl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_divide_dbl_int(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
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
    rray_divide_dbl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_divide_int_cpl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
) {
  RRAY_BINARY(
    int,
    r_int_cbegin,
    rray_cast_int_to_cpl_one,
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    rray_divide_cpl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_divide_cpl_int(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
) {
  RRAY_BINARY(
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    int,
    r_int_cbegin,
    rray_cast_int_to_cpl_one,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    rray_divide_cpl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_divide_dbl_dbl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
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
    rray_divide_dbl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_divide_dbl_cpl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
) {
  RRAY_BINARY(
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_cpl_one,
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    rray_divide_cpl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_divide_cpl_dbl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
) {
  RRAY_BINARY(
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_cpl_one,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    rray_divide_cpl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_divide_cpl_cpl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
) {
  RRAY_BINARY(
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    rray_divide_cpl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static inline double rray_divide_dbl_one(double x, double y) {
  return x / y;
}

static inline r_complex rray_divide_cpl_one(r_complex x, r_complex y) {
  return rray_c99_to_cpl(rray_cpl_to_c99(x) / rray_cpl_to_c99(y));
}
