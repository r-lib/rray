#include "reduce-sum.h"

#include "one-add.h"
#include "reduce.h"
#include "type.h"
#include "utils.h"

#include "decl/reduce-sum-decl.h"

r_obj* ffi_rray_sum(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const bool na_rm = r_arg_as_bool(ffi_na_rm, "na_rm");
  return rray_sum(ffi_x, ffi_axes, na_rm, rray_args.x, error_call);
}

r_obj* rray_sum(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_reduce_run(x, axes, na_rm, rray_sum_switch, arg, error_call);
}

static rray_reduce_run_fn rray_sum_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  switch (rray_typeof(x)) {
  case RRAY_TYPE_logical:
    return na_rm ? rray_sum_lgl_na_rm : rray_sum_lgl;
  case RRAY_TYPE_integer:
    return na_rm ? rray_sum_int_na_rm : rray_sum_int;
  case RRAY_TYPE_double:
    return na_rm ? rray_sum_dbl_na_rm : rray_sum_dbl;
  case RRAY_TYPE_complex:
    return na_rm ? rray_sum_cpl_na_rm : rray_sum_cpl;

  case RRAY_TYPE_character:
  case RRAY_TYPE_raw:
  case RRAY_TYPE_list:
    stop_unsupported_reduce("sum", x, arg, error_call);

  case RRAY_TYPE_scalar:
    stop_scalar_input(x, arg, error_call);
  }

  r_stop_unreachable();
}

static r_obj* rray_sum_lgl(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides,
  struct r_lazy error_call
) {
  RRAY_REDUCE_RUN(
    int,
    r_lgl_cbegin,
    R_TYPE_integer,
    int,
    r_int_begin,
    0,
    rray_sum_lgl_one,
    RRAY_REDUCE_NO_ARGS
  );
}

static r_obj* rray_sum_lgl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides,
  struct r_lazy error_call
) {
  RRAY_REDUCE_RUN(
    int,
    r_lgl_cbegin,
    R_TYPE_integer,
    int,
    r_int_begin,
    0,
    rray_sum_lgl_one_na_rm,
    RRAY_REDUCE_NO_ARGS
  );
}

static r_obj* rray_sum_int(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides,
  struct r_lazy error_call
) {
  RRAY_REDUCE_RUN(
    int,
    r_int_cbegin,
    R_TYPE_integer,
    int,
    r_int_begin,
    0,
    rray_sum_int_one,
    RRAY_REDUCE_ARGS(error_call)
  );
}

static r_obj* rray_sum_int_na_rm(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides,
  struct r_lazy error_call
) {
  RRAY_REDUCE_RUN(
    int,
    r_int_cbegin,
    R_TYPE_integer,
    int,
    r_int_begin,
    0,
    rray_sum_int_one_na_rm,
    RRAY_REDUCE_ARGS(error_call)
  );
}

static r_obj* rray_sum_dbl(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides,
  struct r_lazy error_call
) {
  RRAY_REDUCE_RUN(
    double,
    r_dbl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    0.0,
    rray_sum_dbl_one,
    RRAY_REDUCE_NO_ARGS
  );
}

static r_obj* rray_sum_dbl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides,
  struct r_lazy error_call
) {
  RRAY_REDUCE_RUN(
    double,
    r_dbl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    0.0,
    rray_sum_dbl_one_na_rm,
    RRAY_REDUCE_NO_ARGS
  );
}

static r_obj* rray_sum_cpl(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides,
  struct r_lazy error_call
) {
  RRAY_REDUCE_RUN(
    r_complex,
    r_cpl_cbegin,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    ((r_complex){.r = 0, .i = 0}),
    rray_sum_cpl_one,
    RRAY_REDUCE_NO_ARGS
  );
}

static r_obj* rray_sum_cpl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides,
  struct r_lazy error_call
) {
  RRAY_REDUCE_RUN(
    r_complex,
    r_cpl_cbegin,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    ((r_complex){.r = 0, .i = 0}),
    rray_sum_cpl_one_na_rm,
    RRAY_REDUCE_NO_ARGS
  );
}
