#include "reduce-logical.h"

#include "logical.h"
#include "one-logical.h"
#include "reduce.h"

#include "decl/reduce-logical-decl.h"

r_obj* ffi_rray_all(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const bool na_rm = r_arg_as_bool(ffi_na_rm, "na_rm");
  return rray_all(ffi_x, ffi_axes, na_rm, rray_args.x, error_call);
}

r_obj* ffi_rray_any(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const bool na_rm = r_arg_as_bool(ffi_na_rm, "na_rm");
  return rray_any(ffi_x, ffi_axes, na_rm, rray_args.x, error_call);
}

r_obj* rray_all(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_reduce(x, axes, na_rm, rray_all_switch, arg, error_call);
}

r_obj* rray_any(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_reduce(x, axes, na_rm, rray_any_switch, arg, error_call);
}

static rray_reduce_fn rray_all_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_logical(x, arg, error_call);
  return na_rm ? rray_all_lgl_na_rm : rray_all_lgl;
}

static rray_reduce_fn rray_any_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_logical(x, arg, error_call);
  return na_rm ? rray_any_lgl_na_rm : rray_any_lgl;
}

static r_obj* rray_all_lgl(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  RRAY_REDUCE(
    int,
    r_lgl_cbegin,
    R_TYPE_logical,
    int,
    r_lgl_begin,
    1,
    rray_all_lgl_one,
    RRAY_REDUCE_NO_ARGS
  );
}

static r_obj* rray_all_lgl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  RRAY_REDUCE(
    int,
    r_lgl_cbegin,
    R_TYPE_logical,
    int,
    r_lgl_begin,
    1,
    rray_all_lgl_one_na_rm,
    RRAY_REDUCE_NO_ARGS
  );
}

static r_obj* rray_any_lgl(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  RRAY_REDUCE(
    int,
    r_lgl_cbegin,
    R_TYPE_logical,
    int,
    r_lgl_begin,
    0,
    rray_any_lgl_one,
    RRAY_REDUCE_NO_ARGS
  );
}

static r_obj* rray_any_lgl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  RRAY_REDUCE(
    int,
    r_lgl_cbegin,
    R_TYPE_logical,
    int,
    r_lgl_begin,
    0,
    rray_any_lgl_one_na_rm,
    RRAY_REDUCE_NO_ARGS
  );
}
