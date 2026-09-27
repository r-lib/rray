#include "reduce-logical.h"

#include "logical.h"
#include "reduce.h"

#include "decl/reduce-logical-decl.h"

r_obj* ffi_rray_all_along(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const bool na_rm = r_arg_as_bool(ffi_na_rm, "na_rm");
  return rray_all_along(ffi_x, ffi_axes, na_rm, rray_args.x, error_call);
}

r_obj* ffi_rray_any_along(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const bool na_rm = r_arg_as_bool(ffi_na_rm, "na_rm");
  return rray_any_along(ffi_x, ffi_axes, na_rm, rray_args.x, error_call);
}

r_obj* rray_all_along(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_reduce(x, axes, na_rm, rray_all_along_switch, arg, error_call);
}

r_obj* rray_any_along(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_reduce(x, axes, na_rm, rray_any_along_switch, arg, error_call);
}

static rray_reduce_fn rray_all_along_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_logical(x, arg, error_call);
  return na_rm ? rray_all_along_lgl_na_rm : rray_all_along_lgl;
}

static rray_reduce_fn rray_any_along_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_logical(x, arg, error_call);
  return na_rm ? rray_any_along_lgl_na_rm : rray_any_along_lgl;
}

static r_obj* rray_all_along_lgl(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_REDUCE(
    int,
    r_lgl_cbegin,
    R_TYPE_logical,
    int,
    r_lgl_begin,
    1,
    rray_and_lgl_one
  );
}

static r_obj* rray_all_along_lgl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_REDUCE(
    int,
    r_lgl_cbegin,
    R_TYPE_logical,
    int,
    r_lgl_begin,
    1,
    rray_all_along_lgl_one_na_rm
  );
}

static r_obj* rray_any_along_lgl(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_REDUCE(
    int,
    r_lgl_cbegin,
    R_TYPE_logical,
    int,
    r_lgl_begin,
    0,
    rray_or_lgl_one
  );
}

static r_obj* rray_any_along_lgl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_REDUCE(
    int,
    r_lgl_cbegin,
    R_TYPE_logical,
    int,
    r_lgl_begin,
    0,
    rray_any_along_lgl_one_na_rm
  );
}

static inline int rray_all_along_lgl_one_na_rm(int out, int x) {
  return out && x;
}

static inline int rray_any_along_lgl_one_na_rm(int out, int x) {
  return out || (x == 1);
}
