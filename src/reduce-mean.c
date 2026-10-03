#include "reduce-mean.h"

#include "reduce.h"
#include "type.h"
#include "utils.h"

#include "decl/reduce-mean-decl.h"

r_obj* ffi_rray_mean(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const bool na_rm = r_arg_as_bool(ffi_na_rm, "na_rm");
  return rray_mean(ffi_x, ffi_axes, na_rm, rray_args.x, error_call);
}

r_obj* rray_mean(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_reduce_nested(x, axes, na_rm, rray_mean_switch, arg, error_call);
}

static rray_reduce_nested_fn rray_mean_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  switch (rray_typeof(x)) {
  case RRAY_TYPE_logical:
    return na_rm ? rray_mean_lgl_na_rm : rray_mean_lgl;
  case RRAY_TYPE_integer:
    return na_rm ? rray_mean_int_na_rm : rray_mean_int;
  case RRAY_TYPE_double:
    return na_rm ? rray_mean_dbl_na_rm : rray_mean_dbl;

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

static r_obj* rray_mean_lgl(
  r_obj* x,
  const int* v_outer_dimensions,
  int outer_dimensionality,
  const r_ssize* v_outer_strides,
  const int* v_inner_dimensions,
  int inner_dimensionality,
  const r_ssize* v_inner_strides,
  struct r_lazy error_call
) {
  RRAY_REDUCE_OUTER(
    int,
    r_lgl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_mean_lgl_one,
    RRAY_REDUCE_NO_ARGS
  );
}

static r_obj* rray_mean_lgl_na_rm(
  r_obj* x,
  const int* v_outer_dimensions,
  int outer_dimensionality,
  const r_ssize* v_outer_strides,
  const int* v_inner_dimensions,
  int inner_dimensionality,
  const r_ssize* v_inner_strides,
  struct r_lazy error_call
) {
  RRAY_REDUCE_OUTER(
    int,
    r_lgl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_mean_lgl_one_na_rm,
    RRAY_REDUCE_NO_ARGS
  );
}

static r_obj* rray_mean_int(
  r_obj* x,
  const int* v_outer_dimensions,
  int outer_dimensionality,
  const r_ssize* v_outer_strides,
  const int* v_inner_dimensions,
  int inner_dimensionality,
  const r_ssize* v_inner_strides,
  struct r_lazy error_call
) {
  RRAY_REDUCE_OUTER(
    int,
    r_int_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_mean_int_one,
    RRAY_REDUCE_NO_ARGS
  );
}

static r_obj* rray_mean_int_na_rm(
  r_obj* x,
  const int* v_outer_dimensions,
  int outer_dimensionality,
  const r_ssize* v_outer_strides,
  const int* v_inner_dimensions,
  int inner_dimensionality,
  const r_ssize* v_inner_strides,
  struct r_lazy error_call
) {
  RRAY_REDUCE_OUTER(
    int,
    r_int_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_mean_int_one_na_rm,
    RRAY_REDUCE_NO_ARGS
  );
}

static r_obj* rray_mean_dbl(
  r_obj* x,
  const int* v_outer_dimensions,
  int outer_dimensionality,
  const r_ssize* v_outer_strides,
  const int* v_inner_dimensions,
  int inner_dimensionality,
  const r_ssize* v_inner_strides,
  struct r_lazy error_call
) {
  RRAY_REDUCE_OUTER(
    double,
    r_dbl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_mean_dbl_one,
    RRAY_REDUCE_NO_ARGS
  );
}

static r_obj* rray_mean_dbl_na_rm(
  r_obj* x,
  const int* v_outer_dimensions,
  int outer_dimensionality,
  const r_ssize* v_outer_strides,
  const int* v_inner_dimensions,
  int inner_dimensionality,
  const r_ssize* v_inner_strides,
  struct r_lazy error_call
) {
  RRAY_REDUCE_OUTER(
    double,
    r_dbl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_mean_dbl_one_na_rm,
    RRAY_REDUCE_NO_ARGS
  );
}

static inline double rray_mean_lgl_one(
  const int* v_x,
  r_ssize x_start,
  struct rray_run_iterator* inner
) {
  // State
  long double sum = 0.0;

  RRAY_REDUCE_INNER(int, {
    if (x_elt == r_globals.na_lgl) {
      return r_globals.na_dbl;
    }
    sum += x_elt;
  });

  const r_ssize count = rray_run_iterator_size(inner);

  return (double) (sum / count);
}

static inline double rray_mean_lgl_one_na_rm(
  const int* v_x,
  r_ssize x_start,
  struct rray_run_iterator* inner
) {
  // State
  r_ssize count = 0;
  long double sum = 0.0;

  RRAY_REDUCE_INNER(int, {
    const bool ok = x_elt != r_globals.na_lgl;
    sum += ok ? x_elt : 0;
    count += ok;
  });

  return (double) (sum / count);
}

// Impossible to overflow to `NaN`
static inline double rray_mean_int_one(
  const int* v_x,
  r_ssize x_start,
  struct rray_run_iterator* inner
) {
  // State
  long double sum = 0.0;

  RRAY_REDUCE_INNER(int, {
    if (x_elt == r_globals.na_int) {
      return r_globals.na_dbl;
    }
    sum += x_elt;
  });

  const r_ssize count = rray_run_iterator_size(inner);

  return (double) (sum / count);
}

static inline double rray_mean_int_one_na_rm(
  const int* v_x,
  r_ssize x_start,
  struct rray_run_iterator* inner
) {
  // State
  r_ssize count = 0;
  long double sum = 0.0;

  RRAY_REDUCE_INNER(int, {
    const bool ok = x_elt != r_globals.na_int;
    sum += ok ? x_elt : 0;
    count += ok;
  });

  return (double) (sum / count);
}

static inline double rray_mean_dbl_one(
  const double* v_x,
  r_ssize x_start,
  struct rray_run_iterator* inner
) {
  // State
  long double sum = 0.0;

  // Naively sum up the elements
  RRAY_REDUCE_INNER(double, sum += x_elt);

  // If the sum is `NaN` or `NA`, we return a missing value. Existing `NA`
  // should win over `NaN` so the result is deterministic but that's
  // implementation defined, so if we see either we do another pass through the
  // data looking for any `NA`, otherwise we return `NaN`.
  if (ISNAN((double) sum)) {
    RRAY_REDUCE_INNER(double, {
      if (R_IsNA(x_elt)) {
        return r_globals.na_dbl;
      }
    });

    return R_NaN;
  }

  const r_ssize count = rray_run_iterator_size(inner);

  if (R_FINITE((double) sum)) {
    // Naive sum was finite! Compute the mean, and apply the correction.
    sum /= count;

    // State
    long double correction = 0.0;
    RRAY_REDUCE_INNER(double, correction += x_elt - sum);
    sum += correction / count;
  } else {
    // Naive sum overflowed to infinity. This doesn't necessarily mean that the
    // mean would also overflow though (since it's scaled by the count). So
    // compute a more expensive and lossy scaled sum to see if that overflows.
    // And if it doesn't, apply a correction there too.
    sum = 0.0;
    RRAY_REDUCE_INNER(double, sum += x_elt / count);

    if (R_FINITE((double) sum)) {
      // State
      long double correction = 0.0;
      RRAY_REDUCE_INNER(double, correction += (x_elt - sum) / count);
      sum += correction;
    }
  }

  return (double) sum;
}

static inline double rray_mean_dbl_one_na_rm(
  const double* v_x,
  r_ssize x_start,
  struct rray_run_iterator* inner
) {
  // State
  r_ssize count = 0;
  long double sum = 0.0;

  RRAY_REDUCE_INNER(double, {
    const bool ok = !ISNAN(x_elt);
    sum += ok ? x_elt : 0;
    count += ok;
  });

  // Handles `c(Inf, -Inf)`. `NaN` and `NA` have otherwise been filtered out.
  if (ISNAN((double) sum)) {
    return R_NaN;
  }

  if (R_FINITE((double) sum)) {
    sum /= count;

    // State
    long double correction = 0.0;
    RRAY_REDUCE_INNER(double, correction += ISNAN(x_elt) ? 0 : x_elt - sum);
    sum += correction / count;
  } else {
    sum = 0.0;
    RRAY_REDUCE_INNER(double, sum += ISNAN(x_elt) ? 0 : x_elt / count);

    if (R_FINITE((double) sum)) {
      // State
      long double correction = 0.0;
      RRAY_REDUCE_INNER(
        double,
        correction += ISNAN(x_elt) ? 0 : (x_elt - sum) / count
      );
      sum += correction;
    }
  }

  return (double) sum;
}
