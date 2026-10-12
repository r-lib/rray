#include "cumulative-sum.h"

#include "cumulative.h"
#include "one-add.h"
#include "type.h"
#include "utils.h"

#include "decl/cumulative-sum-decl.h"

r_obj* ffi_rray_cumulative_sum(
  r_obj* ffi_x,
  r_obj* ffi_axis,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const int axis = arg_as_int(ffi_axis, rray_args.axis, error_call);
  return rray_cumulative_sum(ffi_x, axis, rray_args.x, error_call);
}

r_obj* rray_cumulative_sum(
  r_obj* x,
  int axis,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_cumulative(x, axis, rray_cumulative_sum_switch, arg, error_call);
}

static rray_cumulative_fn rray_cumulative_sum_switch(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  switch (rray_typeof(x)) {
  case RRAY_TYPE_logical:
    return rray_cumulative_sum_lgl;
  case RRAY_TYPE_integer:
    return rray_cumulative_sum_int;
  case RRAY_TYPE_double:
    return rray_cumulative_sum_dbl;
  case RRAY_TYPE_complex:
    return rray_cumulative_sum_cpl;

  case RRAY_TYPE_character:
  case RRAY_TYPE_raw:
  case RRAY_TYPE_list:
    stop_unsupported_cumulative("sum", x, arg, error_call);

  case RRAY_TYPE_scalar:
    stop_scalar_input(x, arg, error_call);
  }

  r_stop_unreachable();
}

static r_obj* rray_cumulative_sum_lgl(
  r_obj* x,
  r_ssize state_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_state_broadcast_strides,
  struct r_lazy error_call
) {
  RRAY_CUMULATIVE(
    int,
    r_lgl_cbegin,
    R_TYPE_integer,
    int,
    r_int_begin,
    0,
    rray_add_lgl_one,
    RRAY_CUMULATIVE_ARGS(error_call)
  );
}

static r_obj* rray_cumulative_sum_int(
  r_obj* x,
  r_ssize state_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_state_broadcast_strides,
  struct r_lazy error_call
) {
  RRAY_CUMULATIVE(
    int,
    r_int_cbegin,
    R_TYPE_integer,
    int,
    r_int_begin,
    0,
    rray_add_int_one,
    RRAY_CUMULATIVE_ARGS(error_call)
  );
}

static r_obj* rray_cumulative_sum_dbl(
  r_obj* x,
  r_ssize state_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_state_broadcast_strides,
  struct r_lazy error_call
) {
  RRAY_CUMULATIVE(
    double,
    r_dbl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    0.0,
    rray_add_dbl_one,
    RRAY_CUMULATIVE_NO_ARGS
  );
}

static r_obj* rray_cumulative_sum_cpl(
  r_obj* x,
  r_ssize state_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_state_broadcast_strides,
  struct r_lazy error_call
) {
  RRAY_CUMULATIVE(
    r_complex,
    r_cpl_cbegin,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    ((r_complex){.r = 0, .i = 0}),
    rray_add_cpl_one,
    RRAY_CUMULATIVE_NO_ARGS
  );
}
