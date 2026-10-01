#include "reduce-extremum.h"

#include <limits.h>
#include <math.h>

#include "one-extremum.h"
#include "reduce.h"
#include "type.h"
#include "utils.h"

#include "decl/reduce-extremum-decl.h"

r_obj* ffi_rray_max(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const bool na_rm = r_arg_as_bool(ffi_na_rm, "na_rm");
  return rray_max(ffi_x, ffi_axes, na_rm, rray_args.x, error_call);
}

r_obj* ffi_rray_min(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const bool na_rm = r_arg_as_bool(ffi_na_rm, "na_rm");
  return rray_min(ffi_x, ffi_axes, na_rm, rray_args.x, error_call);
}

r_obj* rray_max(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_reduce(x, axes, na_rm, rray_max_switch, arg, error_call);
}

r_obj* rray_min(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_reduce(x, axes, na_rm, rray_min_switch, arg, error_call);
}

static rray_reduce_fn rray_max_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  switch (rray_typeof(x)) {
  case RRAY_TYPE_logical:
    return na_rm ? rray_max_lgl_na_rm : rray_max_lgl;
  case RRAY_TYPE_integer:
    return na_rm ? rray_max_int_na_rm : rray_max_int;
  case RRAY_TYPE_double:
    return na_rm ? rray_max_dbl_na_rm : rray_max_dbl;

  case RRAY_TYPE_complex:
  case RRAY_TYPE_character:
  case RRAY_TYPE_raw:
  case RRAY_TYPE_list:
    stop_unsupported_reduce("maximum", x, arg, error_call);

  case RRAY_TYPE_scalar:
    stop_scalar_input(x, arg, error_call);
  }

  r_stop_unreachable();
}

static rray_reduce_fn rray_min_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  switch (rray_typeof(x)) {
  case RRAY_TYPE_logical:
    return na_rm ? rray_min_lgl_na_rm : rray_min_lgl;
  case RRAY_TYPE_integer:
    return na_rm ? rray_min_int_na_rm : rray_min_int;
  case RRAY_TYPE_double:
    return na_rm ? rray_min_dbl_na_rm : rray_min_dbl;

  case RRAY_TYPE_complex:
  case RRAY_TYPE_character:
  case RRAY_TYPE_raw:
  case RRAY_TYPE_list:
    stop_unsupported_reduce("minimum", x, arg, error_call);

  case RRAY_TYPE_scalar:
    stop_scalar_input(x, arg, error_call);
  }

  r_stop_unreachable();
}

static r_obj* rray_max_lgl(
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
    rray_pmax_int_one
  );
}

static r_obj* rray_max_lgl_na_rm(
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
    rray_pmax_int_one_na_rm
  );
}

static r_obj* rray_max_int(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_REDUCE(
    int,
    r_int_cbegin,
    R_TYPE_integer,
    int,
    r_int_begin,
    -INT_MAX,
    rray_pmax_int_one
  );
}

static r_obj* rray_max_int_na_rm(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_REDUCE(
    int,
    r_int_cbegin,
    R_TYPE_integer,
    int,
    r_int_begin,
    -INT_MAX,
    rray_pmax_int_one_na_rm
  );
}

static r_obj* rray_max_dbl(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_REDUCE(
    double,
    r_dbl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    -INFINITY,
    rray_pmax_dbl_one
  );
}

static r_obj* rray_max_dbl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_REDUCE(
    double,
    r_dbl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    -INFINITY,
    rray_pmax_dbl_one_na_rm
  );
}

static r_obj* rray_min_lgl(
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
    rray_pmin_int_one
  );
}

static r_obj* rray_min_lgl_na_rm(
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
    rray_pmin_int_one_na_rm
  );
}

static r_obj* rray_min_int(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_REDUCE(
    int,
    r_int_cbegin,
    R_TYPE_integer,
    int,
    r_int_begin,
    INT_MAX,
    rray_pmin_int_one
  );
}

static r_obj* rray_min_int_na_rm(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_REDUCE(
    int,
    r_int_cbegin,
    R_TYPE_integer,
    int,
    r_int_begin,
    INT_MAX,
    rray_pmin_int_one_na_rm
  );
}

static r_obj* rray_min_dbl(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_REDUCE(
    double,
    r_dbl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    INFINITY,
    rray_pmin_dbl_one
  );
}

static r_obj* rray_min_dbl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_REDUCE(
    double,
    r_dbl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    INFINITY,
    rray_pmin_dbl_one_na_rm
  );
}
