#include "reduce-mean.h"

#include "reduce.h"
#include "type.h"
#include "utils.h"

#include "decl/reduce-mean-decl.h"

r_obj* ffi_rray_mean_along(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const bool na_rm = r_arg_as_bool(ffi_na_rm, "na_rm");
  return rray_mean_along(ffi_x, ffi_axes, na_rm, rray_args.x, error_call);
}

r_obj* rray_mean_along(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_reduce_grouped(
    x,
    axes,
    na_rm,
    rray_mean_along_switch,
    arg,
    error_call
  );
}

static rray_reduce_grouped_fn rray_mean_along_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  switch (rray_typeof(x)) {
  case RRAY_TYPE_logical:
    return na_rm ? rray_mean_along_lgl_na_rm : rray_mean_along_lgl;
  case RRAY_TYPE_integer:
    return na_rm ? rray_mean_along_int_na_rm : rray_mean_along_int;
  case RRAY_TYPE_double:
    return na_rm ? rray_mean_along_dbl_na_rm : rray_mean_along_dbl;

  case RRAY_TYPE_complex:
  case RRAY_TYPE_character:
  case RRAY_TYPE_raw:
  case RRAY_TYPE_list:
    stop_unsupported_reduce("mean", x, arg, error_call);

  case RRAY_TYPE_scalar:
    stop_scalar_input(x, arg, error_call);
  }

  r_stop_unreachable();
}

static r_obj* rray_mean_along_lgl(
  r_obj* x,
  const struct rray_strided_iterator_plan* outer_plan,
  const struct rray_strided_iterator_plan* inner_plan
) {
  RRAY_REDUCE_OUTER(
    int,
    r_lgl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_mean_along_lgl_one
  );
}

static r_obj* rray_mean_along_lgl_na_rm(
  r_obj* x,
  const struct rray_strided_iterator_plan* outer_plan,
  const struct rray_strided_iterator_plan* inner_plan
) {
  RRAY_REDUCE_OUTER(
    int,
    r_lgl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_mean_along_lgl_one_na_rm
  );
}

static r_obj* rray_mean_along_int(
  r_obj* x,
  const struct rray_strided_iterator_plan* outer_plan,
  const struct rray_strided_iterator_plan* inner_plan
) {
  RRAY_REDUCE_OUTER(
    int,
    r_int_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_mean_along_int_one
  );
}

static r_obj* rray_mean_along_int_na_rm(
  r_obj* x,
  const struct rray_strided_iterator_plan* outer_plan,
  const struct rray_strided_iterator_plan* inner_plan
) {
  RRAY_REDUCE_OUTER(
    int,
    r_int_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_mean_along_int_one_na_rm
  );
}

static r_obj* rray_mean_along_dbl(
  r_obj* x,
  const struct rray_strided_iterator_plan* outer_plan,
  const struct rray_strided_iterator_plan* inner_plan
) {
  RRAY_REDUCE_OUTER(
    double,
    r_dbl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_mean_along_dbl_one
  );
}

static r_obj* rray_mean_along_dbl_na_rm(
  r_obj* x,
  const struct rray_strided_iterator_plan* outer_plan,
  const struct rray_strided_iterator_plan* inner_plan
) {
  RRAY_REDUCE_OUTER(
    double,
    r_dbl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_mean_along_dbl_one_na_rm
  );
}

static inline double rray_mean_along_lgl_one(
  const int* v_x,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* inner_plan
) {
  const r_ssize n = rray_strided_iterator_plan_size(inner_plan);

  long double s = 0.0;

  RRAY_REDUCE_INNER(int, {
    if (x_elt == r_globals.na_lgl) {
      return r_globals.na_dbl;
    }
    s += x_elt;
  });

  return (double) (s / n);
}

static inline double rray_mean_along_lgl_one_na_rm(
  const int* v_x,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* inner_plan
) {
  r_ssize n = 0;
  long double s = 0.0;

  RRAY_REDUCE_INNER(int, {
    const bool ok = x_elt != r_globals.na_lgl;
    s += ok ? x_elt : 0;
    n += ok;
  });

  return (double) (s / n);
}

static inline double rray_mean_along_int_one(
  const int* v_x,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* inner_plan
) {
  const r_ssize n = rray_strided_iterator_plan_size(inner_plan);

  long double s = 0.0;

  RRAY_REDUCE_INNER(int, {
    if (x_elt == r_globals.na_int) {
      return r_globals.na_dbl;
    }
    s += x_elt;
  });

  return (double) (s / n);
}

static inline double rray_mean_along_int_one_na_rm(
  const int* v_x,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* inner_plan
) {
  r_ssize n = 0;
  long double s = 0.0;

  RRAY_REDUCE_INNER(int, {
    const bool ok = x_elt != r_globals.na_int;
    s += ok ? x_elt : 0;
    n += ok;
  });

  return (double) (s / n);
}

static inline double rray_mean_along_dbl_one(
  const double* v_x,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* inner_plan
) {
  const r_ssize n = rray_strided_iterator_plan_size(inner_plan);

  long double s = 0.0;

  RRAY_REDUCE_INNER(double, s += x_elt);

  if (ISNAN((double) s)) {
    RRAY_REDUCE_INNER(double, {
      if (R_IsNA(x_elt)) {
        return r_globals.na_dbl;
      }
    });

    return R_NaN;
  }

  if (R_FINITE((double) s)) {
    s /= n;

    if (R_FINITE((double) s)) {
      long double t = 0.0;
      RRAY_REDUCE_INNER(double, t += x_elt - s);
      s += t / n;
    }
  } else {
    s = 0.0;
    RRAY_REDUCE_INNER(double, s += x_elt / n);

    if (R_FINITE((double) s)) {
      long double t = 0.0;
      RRAY_REDUCE_INNER(double, t += (x_elt - s) / n);
      s += t;
    }
  }

  return (double) s;
}

static inline double rray_mean_along_dbl_one_na_rm(
  const double* v_x,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* inner_plan
) {
  r_ssize n = 0;
  long double s = 0.0;

  RRAY_REDUCE_INNER(double, {
    const bool ok = !ISNAN(x_elt);
    s += ok ? x_elt : 0;
    n += ok;
  });

  if (ISNAN((double) s)) {
    return R_NaN;
  }

  if (R_FINITE((double) s)) {
    s /= n;

    if (R_FINITE((double) s)) {
      long double t = 0.0;
      RRAY_REDUCE_INNER(double, t += ISNAN(x_elt) ? 0 : x_elt - s);
      s += t / n;
    }
  } else {
    s = 0.0;
    RRAY_REDUCE_INNER(double, s += ISNAN(x_elt) ? 0 : x_elt / n);

    if (R_FINITE((double) s)) {
      long double t = 0.0;
      RRAY_REDUCE_INNER(double, t += ISNAN(x_elt) ? 0 : (x_elt - s) / n);
      s += t;
    }
  }

  return (double) s;
}
