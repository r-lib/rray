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

#define RRAY_REDUCE_OUTER(                                                     \
  X_CTYPE,                                                                     \
  X_CONST_DEREF,                                                               \
  OUT_RTYPE,                                                                   \
  OUT_CTYPE,                                                                   \
  OUT_DEREF,                                                                   \
  ONE                                                                          \
)                                                                              \
  const r_ssize size = rray_strided_iterator_plan_size(outer_plan);            \
                                                                               \
  r_obj* out = KEEP(r_alloc_vector(OUT_RTYPE, out_size));                      \
  OUT_CTYPE* v_out = OUT_DEREF(out);                                           \
                                                                               \
  const X_CTYPE* v_x = X_CONST_DEREF(x);                                       \
                                                                               \
  r_ssize out_start = 0;                                                       \
  const r_ssize out_run_size =                                                 \
    rray_strided_iterator_plan_run_size(outer_plan);                           \
                                                                               \
  r_ssize x_start = 0;                                                         \
  const r_ssize x_run_stride =                                                 \
    rray_strided_iterator_plan_run_stride(outer_plan);                         \
                                                                               \
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];                                    \
  r_memset(v_point, 0, sizeof(r_ssize) * (size_t) outer_plan->dimensionality); \
                                                                               \
  while (out_start != size) {                                                  \
    const r_ssize out_run_end = out_start + out_run_size;                      \
    r_ssize x_base = x_start;                                                  \
                                                                               \
    for (r_ssize i = out_start; i < out_run_end; ++i) {                        \
      v_out[i] = ONE(v_x, x_base, inner_plan);                                 \
      x_base += x_run_stride;                                                  \
    }                                                                          \
                                                                               \
    out_start = out_run_end;                                                   \
    RRAY_STRIDED_ITERATOR_NEXT(x_start, v_point, outer_plan);                  \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

static r_obj* rray_mean_along_lgl(
  r_obj* x,
  r_ssize out_size,
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
  r_ssize out_size,
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
  r_ssize out_size,
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
  r_ssize out_size,
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
  r_ssize out_size,
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
  r_ssize out_size,
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

#define RRAY_REDUCE_INNER(X_CTYPE, ACCUMULATE)                                 \
  do {                                                                         \
    const r_ssize size = rray_strided_iterator_plan_size(plan);                \
    const r_ssize run_size = rray_strided_iterator_plan_run_size(plan);        \
    const r_ssize x_run_stride = rray_strided_iterator_plan_run_stride(plan);  \
                                                                               \
    r_ssize run_start = 0;                                                     \
    r_ssize x_start = x_base;                                                  \
                                                                               \
    r_ssize v_point[RRAY_MAX_DIMENSIONALITY];                                  \
    r_memset(v_point, 0, sizeof(r_ssize) * (size_t) plan->dimensionality);     \
                                                                               \
    while (run_start != size) {                                                \
      const r_ssize run_end = run_start + run_size;                            \
      r_ssize x_loc = x_start;                                                 \
                                                                               \
      for (r_ssize i = run_start; i < run_end; ++i) {                          \
        const X_CTYPE x_elt = v_x[x_loc];                                      \
        ACCUMULATE;                                                            \
        x_loc += x_run_stride;                                                 \
      }                                                                        \
                                                                               \
      run_start = run_end;                                                     \
      RRAY_STRIDED_ITERATOR_NEXT(x_start, v_point, plan);                      \
    }                                                                          \
  } while (0)

static inline double rray_mean_along_lgl_one(
  const int* v_x,
  r_ssize x_base,
  const struct rray_strided_iterator_plan* plan
) {
  const r_ssize n = rray_strided_iterator_plan_size(plan);

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
  r_ssize x_base,
  const struct rray_strided_iterator_plan* plan
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
  r_ssize x_base,
  const struct rray_strided_iterator_plan* plan
) {
  const r_ssize n = rray_strided_iterator_plan_size(plan);

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
  r_ssize x_base,
  const struct rray_strided_iterator_plan* plan
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
  r_ssize x_base,
  const struct rray_strided_iterator_plan* plan
) {
  const r_ssize n = rray_strided_iterator_plan_size(plan);

  long double s = 0.0;

  RRAY_REDUCE_INNER(double, s += x_elt);

  if (ISNAN((double) s)) {
    return rray_mean_along_dbl_missing(v_x, x_base, plan);
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
  r_ssize x_base,
  const struct rray_strided_iterator_plan* plan
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

static inline double rray_mean_along_dbl_missing(
  const double* v_x,
  r_ssize x_base,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_REDUCE_INNER(double, {
    if (R_IsNA(x_elt)) {
      return r_globals.na_dbl;
    }
  });

  return R_NaN;
}

#undef RRAY_REDUCE_OUTER
#undef RRAY_REDUCE_INNER
