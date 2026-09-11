#include "reduce-sum.h"

#include <limits.h>

#include "reduce.h"
#include "type.h"
#include "utils.h"

#include "decl/reduce-sum-decl.h"

r_obj* ffi_rray_sum_along(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const bool na_rm = r_arg_as_bool(ffi_na_rm, "na_rm");
  return rray_sum_along(ffi_x, ffi_axes, na_rm, rray_args.x, error_call);
}

r_obj* rray_sum_along(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_reduce(x, axes, na_rm, rray_sum_along_switch, arg, error_call);
}

static rray_reduce_fn rray_sum_along_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  switch (rray_typeof(x)) {
  case RRAY_TYPE_logical:
    return na_rm ? rray_sum_along_lgl_na_rm : rray_sum_along_lgl;
  case RRAY_TYPE_integer:
    return na_rm ? rray_sum_along_int_na_rm : rray_sum_along_int;
  case RRAY_TYPE_double:
    return na_rm ? rray_sum_along_dbl_na_rm : rray_sum_along_dbl;
  case RRAY_TYPE_complex:
    return na_rm ? rray_sum_along_cpl_na_rm : rray_sum_along_cpl;

  case RRAY_TYPE_character:
  case RRAY_TYPE_raw:
  case RRAY_TYPE_list:
    stop_unsupported_reduce("sum", x, arg, error_call);

  case RRAY_TYPE_scalar:
    stop_scalar_input(x, arg, error_call);
  }

  r_stop_unreachable();
}

static r_obj* rray_sum_along_lgl(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
) {
  RRAY_REDUCE(
    int,
    r_lgl_cbegin,
    R_TYPE_integer,
    int,
    r_int_begin,
    0,
    rray_sum_along_lgl_one
  );
}

static r_obj* rray_sum_along_lgl_na_rm(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
) {
  RRAY_REDUCE(
    int,
    r_lgl_cbegin,
    R_TYPE_integer,
    int,
    r_int_begin,
    0,
    rray_sum_along_lgl_one_na_rm
  );
}

static r_obj* rray_sum_along_int(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
) {
  RRAY_REDUCE(
    int,
    r_int_cbegin,
    R_TYPE_integer,
    int,
    r_int_begin,
    0,
    rray_sum_along_int_one
  );
}

static r_obj* rray_sum_along_int_na_rm(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
) {
  RRAY_REDUCE(
    int,
    r_int_cbegin,
    R_TYPE_integer,
    int,
    r_int_begin,
    0,
    rray_sum_along_int_one_na_rm
  );
}

static r_obj* rray_sum_along_dbl(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
) {
  RRAY_REDUCE(
    double,
    r_dbl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    0.0,
    rray_sum_along_dbl_one
  );
}

static r_obj* rray_sum_along_dbl_na_rm(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
) {
  RRAY_REDUCE(
    double,
    r_dbl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    0.0,
    rray_sum_along_dbl_one_na_rm
  );
}

static r_obj* rray_sum_along_cpl(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
) {
  RRAY_REDUCE(
    r_complex,
    r_cpl_cbegin,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    ((r_complex){.r = 0, .i = 0}),
    rray_sum_along_cpl_one
  );
}

static r_obj* rray_sum_along_cpl_na_rm(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
) {
  RRAY_REDUCE(
    r_complex,
    r_cpl_cbegin,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    ((r_complex){.r = 0, .i = 0}),
    rray_sum_along_cpl_one_na_rm
  );
}

static inline int rray_sum_along_lgl_one(int out, int x) {
  if (out == r_globals.na_int) {
    return r_globals.na_int;
  }

  if (x == r_globals.na_lgl) {
    return r_globals.na_int;
  }

  // Since long vectors aren't supported in arrays,
  // we can't ever integer overflow in a logical array

  return out + x;
}

static inline int rray_sum_along_lgl_one_na_rm(int out, int x) {
  if (x == r_globals.na_lgl) {
    return out;
  }

  return out + x;
}

static inline int rray_sum_along_int_one(int out, int x) {
  if (out == r_globals.na_int) {
    return r_globals.na_int;
  }

  if (x == r_globals.na_int) {
    return r_globals.na_int;
  }

  check_sum_along_int_overflow(out, x);

  return out + x;
}

static inline int rray_sum_along_int_one_na_rm(int out, int x) {
  if (x == r_globals.na_int) {
    return out;
  }

  check_sum_along_int_overflow(out, x);

  return out + x;
}

static inline double rray_sum_along_dbl_one(double out, double x) {
  if (ISNAN(out) || ISNAN(x)) {
    if (R_IsNA(out) || R_IsNA(x)) {
      // `NA` wins over numbers and `NaN`
      return r_globals.na_dbl;
    } else {
      // `NaN` wins over numbers
      return R_NaN;
    }
  } else {
    return out + x;
  }
}

static inline double rray_sum_along_dbl_one_na_rm(double out, double x) {
  if (ISNAN(x)) {
    return out;
  }

  return out + x;
}

static inline r_complex rray_sum_along_cpl_one(r_complex out, r_complex x) {
  return (r_complex){
    .r = rray_sum_along_dbl_one(out.r, x.r),
    .i = rray_sum_along_dbl_one(out.i, x.i),
  };
}

static inline r_complex rray_sum_along_cpl_one_na_rm(
  r_complex out,
  r_complex x
) {
  return (r_complex){
    .r = rray_sum_along_dbl_one_na_rm(out.r, x.r),
    .i = rray_sum_along_dbl_one_na_rm(out.i, x.i),
  };
}

static inline void check_sum_along_int_overflow(int out, int x) {
  if ((x > 0 && out > INT_MAX - x) || (x < 0 && out < -INT_MAX - x)) {
    r_abort("Integer overflow.");
  }
}
