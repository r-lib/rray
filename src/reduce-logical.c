#include "reduce-logical.h"

#include "reduce.h"
#include "type.h"
#include "utils.h"

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
  return rray_reduce_flat(
    x,
    axes,
    na_rm,
    rray_all_along_switch,
    arg,
    error_call
  );
}

r_obj* rray_any_along(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_reduce_flat(
    x,
    axes,
    na_rm,
    rray_any_along_switch,
    arg,
    error_call
  );
}

static rray_reduce_flat_fn rray_all_along_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  switch (rray_typeof(x)) {
  case RRAY_TYPE_logical:
    return na_rm ? rray_all_along_lgl_na_rm : rray_all_along_lgl;

  case RRAY_TYPE_integer:
  case RRAY_TYPE_double:
  case RRAY_TYPE_complex:
  case RRAY_TYPE_character:
  case RRAY_TYPE_raw:
  case RRAY_TYPE_list:
    stop_non_logical_reduce(x, arg, error_call);

  case RRAY_TYPE_scalar:
    stop_scalar_input(x, arg, error_call);
  }

  r_stop_unreachable();
}

static rray_reduce_flat_fn rray_any_along_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  switch (rray_typeof(x)) {
  case RRAY_TYPE_logical:
    return na_rm ? rray_any_along_lgl_na_rm : rray_any_along_lgl;

  case RRAY_TYPE_integer:
  case RRAY_TYPE_double:
  case RRAY_TYPE_complex:
  case RRAY_TYPE_character:
  case RRAY_TYPE_raw:
  case RRAY_TYPE_list:
    stop_non_logical_reduce(x, arg, error_call);

  case RRAY_TYPE_scalar:
    stop_scalar_input(x, arg, error_call);
  }

  r_stop_unreachable();
}

static r_obj* rray_all_along_lgl(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_REDUCE_FLAT(
    int,
    r_lgl_cbegin,
    R_TYPE_logical,
    int,
    r_lgl_begin,
    1,
    rray_all_along_lgl_one
  );
}

static r_obj* rray_all_along_lgl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_REDUCE_FLAT(
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
  RRAY_REDUCE_FLAT(
    int,
    r_lgl_cbegin,
    R_TYPE_logical,
    int,
    r_lgl_begin,
    0,
    rray_any_along_lgl_one
  );
}

static r_obj* rray_any_along_lgl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_REDUCE_FLAT(
    int,
    r_lgl_cbegin,
    R_TYPE_logical,
    int,
    r_lgl_begin,
    0,
    rray_any_along_lgl_one_na_rm
  );
}

static inline int rray_all_along_lgl_one(int out, int x) {
  const bool any_false = !out || !x;
  const bool equal = out == x;
  return !any_false * (equal * out + !equal * r_globals.na_lgl);
}

static inline int rray_all_along_lgl_one_na_rm(int out, int x) {
  return out && x;
}

static inline int rray_any_along_lgl_one(int out, int x) {
  const bool any_true = (out == 1) || (x == 1);
  const bool equal = out == x;
  return any_true + !any_true * (equal * out + !equal * r_globals.na_lgl);
}

static inline int rray_any_along_lgl_one_na_rm(int out, int x) {
  return out || (x == 1);
}

static r_no_return void stop_non_logical_reduce(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  r_abort_lazy_call(
    error_call,
    "%s must be a logical array, not %s.",
    rray_arg_format_input(arg),
    r_obj_type_friendly(x)
  );
}
