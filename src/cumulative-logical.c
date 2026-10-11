#include "cumulative-logical.h"

#include "cumulative.h"
#include "logical.h"
#include "one-logical.h"
#include "utils.h"

#include "decl/cumulative-logical-decl.h"

r_obj* ffi_rray_cumulative_all(
  r_obj* ffi_x,
  r_obj* ffi_axis,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const int axis = arg_as_int(ffi_axis, rray_args.axis, error_call);
  return rray_cumulative_all(ffi_x, axis, rray_args.x, error_call);
}

r_obj* ffi_rray_cumulative_any(
  r_obj* ffi_x,
  r_obj* ffi_axis,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const int axis = arg_as_int(ffi_axis, rray_args.axis, error_call);
  return rray_cumulative_any(ffi_x, axis, rray_args.x, error_call);
}

r_obj* rray_cumulative_all(
  r_obj* x,
  int axis,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_cumulative(x, axis, rray_cumulative_all_switch, arg, error_call);
}

r_obj* rray_cumulative_any(
  r_obj* x,
  int axis,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_cumulative(x, axis, rray_cumulative_any_switch, arg, error_call);
}

static rray_cumulative_fn rray_cumulative_all_switch(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_logical(x, arg, error_call);
  return rray_cumulative_all_lgl;
}

static rray_cumulative_fn rray_cumulative_any_switch(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_logical(x, arg, error_call);
  return rray_cumulative_any_lgl;
}

static r_obj* rray_cumulative_all_lgl(
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
    R_TYPE_logical,
    int,
    r_lgl_begin,
    1,
    rray_all_lgl_one
  );
}

static r_obj* rray_cumulative_any_lgl(
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
    R_TYPE_logical,
    int,
    r_lgl_begin,
    0,
    rray_any_lgl_one
  );
}
