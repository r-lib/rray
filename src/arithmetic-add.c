#include "arithmetic-add.h"

#include "arithmetic.h"
#include "binary.h"
#include "cast.h"
#include "one-add.h"
#include "type.h"
#include "typeof2.h"
#include "utils.h"

#include "decl/arithmetic-add-decl.h"

r_obj* ffi_rray_add(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_add(ffi_x, ffi_y, rray_args.x, rray_args.y, error_call);
}

r_obj* rray_add(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  return rray_binary_arithmetic_run(
    x,
    y,
    rray_add_switch,
    x_arg,
    y_arg,
    error_call
  );
}

static rray_binary_arithmetic_run_fn rray_add_switch(
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
    return rray_add_lgl_lgl;
  case RRAY_TYPE2_logical_integer:
    return (side == RRAY_SIDE_right) ? rray_add_lgl_int : rray_add_int_lgl;
  case RRAY_TYPE2_logical_double:
    return (side == RRAY_SIDE_right) ? rray_add_lgl_dbl : rray_add_dbl_lgl;
  case RRAY_TYPE2_logical_complex:
    return (side == RRAY_SIDE_right) ? rray_add_lgl_cpl : rray_add_cpl_lgl;
  case RRAY_TYPE2_integer_integer:
    return rray_add_int_int;
  case RRAY_TYPE2_integer_double:
    return (side == RRAY_SIDE_right) ? rray_add_int_dbl : rray_add_dbl_int;
  case RRAY_TYPE2_integer_complex:
    return (side == RRAY_SIDE_right) ? rray_add_int_cpl : rray_add_cpl_int;
  case RRAY_TYPE2_double_double:
    return rray_add_dbl_dbl;
  case RRAY_TYPE2_double_complex:
    return (side == RRAY_SIDE_right) ? rray_add_dbl_cpl : rray_add_cpl_dbl;
  case RRAY_TYPE2_complex_complex:
    return rray_add_cpl_cpl;

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
    stop_unsupported_arithmetic("+", x, y, x_arg, y_arg, error_call);

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

static r_obj* rray_add_lgl_lgl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
) {
  RRAY_BINARY_RUN(
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_int_one,
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_int_one,
    R_TYPE_integer,
    int,
    r_int_begin,
    rray_add_int_one,
    RRAY_BINARY_ARGS(error_call)
  );
}

static r_obj* rray_add_lgl_int(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
) {
  RRAY_BINARY_RUN(
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_int_one,
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    R_TYPE_integer,
    int,
    r_int_begin,
    rray_add_int_one,
    RRAY_BINARY_ARGS(error_call)
  );
}

static r_obj* rray_add_int_lgl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
) {
  RRAY_BINARY_RUN(
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_int_one,
    R_TYPE_integer,
    int,
    r_int_begin,
    rray_add_int_one,
    RRAY_BINARY_ARGS(error_call)
  );
}

static r_obj* rray_add_lgl_dbl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
) {
  RRAY_BINARY_RUN(
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_dbl_one,
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_add_dbl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_add_dbl_lgl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
) {
  RRAY_BINARY_RUN(
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_add_dbl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_add_lgl_cpl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
) {
  RRAY_BINARY_RUN(
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_cpl_one,
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    rray_add_cpl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_add_cpl_lgl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
) {
  RRAY_BINARY_RUN(
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_cpl_one,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    rray_add_cpl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_add_int_int(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
) {
  RRAY_BINARY_RUN(
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    R_TYPE_integer,
    int,
    r_int_begin,
    rray_add_int_one,
    RRAY_BINARY_ARGS(error_call)
  );
}

static r_obj* rray_add_int_dbl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
) {
  RRAY_BINARY_RUN(
    int,
    r_int_cbegin,
    rray_cast_int_to_dbl_one,
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_add_dbl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_add_dbl_int(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
) {
  RRAY_BINARY_RUN(
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    int,
    r_int_cbegin,
    rray_cast_int_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_add_dbl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_add_int_cpl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
) {
  RRAY_BINARY_RUN(
    int,
    r_int_cbegin,
    rray_cast_int_to_cpl_one,
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    rray_add_cpl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_add_cpl_int(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
) {
  RRAY_BINARY_RUN(
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    int,
    r_int_cbegin,
    rray_cast_int_to_cpl_one,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    rray_add_cpl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_add_dbl_dbl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
) {
  RRAY_BINARY_RUN(
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_add_dbl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_add_dbl_cpl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
) {
  RRAY_BINARY_RUN(
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_cpl_one,
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    rray_add_cpl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_add_cpl_dbl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
) {
  RRAY_BINARY_RUN(
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_cpl_one,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    rray_add_cpl_one,
    RRAY_BINARY_NO_ARGS
  );
}

static r_obj* rray_add_cpl_cpl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
) {
  RRAY_BINARY_RUN(
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    rray_add_cpl_one,
    RRAY_BINARY_NO_ARGS
  );
}
