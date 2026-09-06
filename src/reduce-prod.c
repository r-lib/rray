#include "reduce-prod.h"

#include "reduce.h"
#include "type.h"
#include "utils.h"

#include "decl/reduce-prod-decl.h"

r_obj* ffi_rray_prod(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const bool na_rm = r_arg_as_bool(ffi_na_rm, "na_rm");
  return rray_prod(ffi_x, ffi_axes, na_rm, rray_args.x, error_call);
}

r_obj* rray_prod(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_reduce(x, axes, na_rm, rray_prod_switch, arg, error_call);
}

static rray_reduce_fn rray_prod_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  switch (rray_typeof(x)) {
  case RRAY_TYPE_logical:
    return na_rm ? rray_prod_lgl_na_rm : rray_prod_lgl;
  case RRAY_TYPE_integer:
    return na_rm ? rray_prod_int_na_rm : rray_prod_int;
  case RRAY_TYPE_double:
    return na_rm ? rray_prod_dbl_na_rm : rray_prod_dbl;
  case RRAY_TYPE_complex:
    return na_rm ? rray_prod_cpl_na_rm : rray_prod_cpl;

  case RRAY_TYPE_character:
  case RRAY_TYPE_raw:
  case RRAY_TYPE_list:
    stop_unsupported_reduce("product", x, arg, error_call);

  case RRAY_TYPE_scalar:
    stop_scalar_input(x, arg, error_call);
  }

  r_stop_unreachable();
}

static r_obj* rray_prod_lgl(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
) {
  RRAY_REDUCE(
    int,
    r_lgl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    1.0,
    rray_prod_lgl_one
  );
}

static r_obj* rray_prod_lgl_na_rm(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
) {
  RRAY_REDUCE(
    int,
    r_lgl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    1.0,
    rray_prod_lgl_one_na_rm
  );
}

static r_obj* rray_prod_int(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
) {
  RRAY_REDUCE(
    int,
    r_int_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    1.0,
    rray_prod_int_one
  );
}

static r_obj* rray_prod_int_na_rm(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
) {
  RRAY_REDUCE(
    int,
    r_int_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    1.0,
    rray_prod_int_one_na_rm
  );
}

static r_obj* rray_prod_dbl(
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
    1.0,
    rray_prod_dbl_one
  );
}

static r_obj* rray_prod_dbl_na_rm(
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
    1.0,
    rray_prod_dbl_one_na_rm
  );
}

static r_obj* rray_prod_cpl(
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
    ((r_complex){.r = 1, .i = 0}),
    rray_prod_cpl_one
  );
}

static r_obj* rray_prod_cpl_na_rm(
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
    ((r_complex){.r = 1, .i = 0}),
    rray_prod_cpl_one_na_rm
  );
}

static inline double rray_prod_lgl_one(double out, int x) {
  if (R_IsNA(out)) {
    return r_globals.na_dbl;
  }

  if (x == r_globals.na_lgl) {
    return r_globals.na_dbl;
  }

  return out * x;
}

static inline double rray_prod_lgl_one_na_rm(double out, int x) {
  if (x == r_globals.na_lgl) {
    return out;
  }

  return out * x;
}

static inline double rray_prod_int_one(double out, int x) {
  if (R_IsNA(out)) {
    return r_globals.na_dbl;
  }

  if (x == r_globals.na_int) {
    return r_globals.na_dbl;
  }

  return out * x;
}

static inline double rray_prod_int_one_na_rm(double out, int x) {
  if (x == r_globals.na_int) {
    return out;
  }

  return out * x;
}

static inline double rray_prod_dbl_one(double out, double x) {
  if (ISNAN(out) || ISNAN(x)) {
    if (R_IsNA(out) || R_IsNA(x)) {
      return r_globals.na_dbl;
    } else {
      return R_NaN;
    }
  } else {
    return out * x;
  }
}

static inline double rray_prod_dbl_one_na_rm(double out, double x) {
  if (ISNAN(x)) {
    return out;
  }

  return out * x;
}

static inline r_complex rray_prod_cpl_one(r_complex out, r_complex x) {
  return rray_c99_to_cpl(rray_cpl_to_c99(out) * rray_cpl_to_c99(x));
}

static inline r_complex rray_prod_cpl_one_na_rm(r_complex out, r_complex x) {
  if (ISNAN(x.r) || ISNAN(x.i)) {
    return out;
  }

  return rray_prod_cpl_one(out, x);
}
