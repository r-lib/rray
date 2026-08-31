#include "axes.h"
#include "decl/sum-template-decl.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "names.h"
#include "reduce.h"
#include "reduction-iterator.h"
#include "size.h"
#include "utils.h"

#if RRAY_TYPE == RRAY_TYPE_LOGICAL
#define RRAY_FN rray_sum_lgl
#define RRAY_FN_ONE rray_sum_lgl_one
#define RRAY_FN_NA_RM_ONE rray_sum_lgl_one_na_rm
#define RRAY_X_C_TYPE int
#define RRAY_X_CONST_DEREF r_lgl_cbegin
#define RRAY_OUT_C_TYPE int
#define RRAY_OUT_ALLOC r_alloc_integer
#define RRAY_OUT_DEREF r_int_begin

#elif RRAY_TYPE == RRAY_TYPE_INTEGER
#define RRAY_FN rray_sum_int
#define RRAY_FN_ONE rray_sum_int_one
#define RRAY_FN_NA_RM_ONE rray_sum_int_one_na_rm
#define RRAY_X_C_TYPE int
#define RRAY_X_CONST_DEREF r_int_cbegin
#define RRAY_OUT_C_TYPE int
#define RRAY_OUT_ALLOC r_alloc_integer
#define RRAY_OUT_DEREF r_int_begin

#elif RRAY_TYPE == RRAY_TYPE_DOUBLE
#define RRAY_FN rray_sum_dbl
#define RRAY_FN_ONE rray_sum_dbl_one
#define RRAY_FN_NA_RM_ONE rray_sum_dbl_one_na_rm
#define RRAY_X_C_TYPE double
#define RRAY_X_CONST_DEREF r_dbl_cbegin
#define RRAY_OUT_C_TYPE double
#define RRAY_OUT_ALLOC r_alloc_double
#define RRAY_OUT_DEREF r_dbl_begin

#elif RRAY_TYPE == RRAY_TYPE_COMPLEX
#define RRAY_FN rray_sum_cpl
#define RRAY_FN_ONE rray_sum_cpl_one
#define RRAY_FN_NA_RM_ONE rray_sum_cpl_one_na_rm
#define RRAY_X_C_TYPE r_complex
#define RRAY_X_CONST_DEREF r_cpl_cbegin
#define RRAY_OUT_C_TYPE r_complex
#define RRAY_OUT_ALLOC r_alloc_complex
#define RRAY_OUT_DEREF r_cpl_begin
#endif

static inline r_obj* RRAY_FN(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  struct r_lazy error_call
) {
  r_obj* x_dimensions = KEEP(rray_dimensions(x, error_call));
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);

  const int dimensionality = rray_dimensionality_from_dimensions(x_dimensions);

  axes = KEEP(arg_as_axes(axes, dimensionality, axes_chr, error_call));
  const int* v_axes = r_int_cbegin(axes);
  const r_ssize axes_size = r_length(axes);

  r_obj* out_dimensions = KEEP(
    rray_reduce_dimensions(v_x_dimensions, dimensionality, v_axes, axes_size)
  );
  const int* v_out_dimensions = r_int_cbegin(out_dimensions);

  const r_ssize x_size =
    rray_size_from_dimensions(v_x_dimensions, dimensionality);
  const r_ssize out_size =
    rray_size_from_dimensions(v_out_dimensions, dimensionality);

  r_obj* out = KEEP(RRAY_OUT_ALLOC(out_size));
  r_attrib_poke_dim(out, out_dimensions);

  RRAY_OUT_C_TYPE* v_out = RRAY_OUT_DEREF(out);
  memset(v_out, 0, sizeof(RRAY_OUT_C_TYPE) * out_size);

  struct rray_iterator it;
  rray_reduction_iterator_init(
    &it,
    v_x_dimensions,
    v_out_dimensions,
    dimensionality
  );

  RRAY_X_C_TYPE const* v_x = RRAY_X_CONST_DEREF(x);

  if (na_rm) {
    for (r_ssize i = 0; i < x_size; ++i) {
      const r_ssize loc = rray_iterator_location(&it);
      v_out[loc] = RRAY_FN_NA_RM_ONE(v_out[loc], v_x[i]);
      rray_iterator_next(&it);
    }
  } else {
    for (r_ssize i = 0; i < x_size; ++i) {
      const r_ssize loc = rray_iterator_location(&it);
      v_out[loc] = RRAY_FN_ONE(v_out[loc], v_x[i]);
      rray_iterator_next(&it);
    }
  }

  r_obj* x_names = rray_names(x, error_call);
  if (x_names != r_null) {
    KEEP(x_names);
    r_obj* const* v_x_names = r_list_cbegin(x_names);
    r_obj* out_names =
      rray_reduce_names(v_x_names, dimensionality, v_axes, axes_size);
    if (out_names != r_null) {
      r_attrib_poke_dim_names(out, out_names);
    }
    FREE(1);
  }

  FREE(4);
  return out;
}

#ifndef RRAY_ONCE
#define RRAY_ONCE

#define RRAY_INT_MAX INT_MAX
#define RRAY_INT_MIN -INT_MAX

static inline void check_sum_int_overflow(int out, int x) {
  if ((x > 0 && out > RRAY_INT_MAX - x) || (x < 0 && out < RRAY_INT_MIN - x)) {
    r_abort("Integer overflow in `rray_sum()`.");
  }
}

#undef RRAY_INT_MAX
#undef RRAY_INT_MIN

static inline int rray_sum_lgl_one(int out, int x) {
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

static inline int rray_sum_lgl_one_na_rm(int out, int x) {
  if (x == r_globals.na_lgl) {
    return out;
  }

  return out + x;
}

static inline int rray_sum_int_one(int out, int x) {
  if (out == r_globals.na_int) {
    return r_globals.na_int;
  }

  if (x == r_globals.na_int) {
    return r_globals.na_int;
  }

  check_sum_int_overflow(out, x);

  return out + x;
}

static inline int rray_sum_int_one_na_rm(int out, int x) {
  if (x == r_globals.na_int) {
    return out;
  }

  check_sum_int_overflow(out, x);

  return out + x;
}

static inline double rray_sum_dbl_one(double out, double x) {
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

static inline double rray_sum_dbl_one_na_rm(double out, double x) {
  if (ISNAN(x)) {
    return out;
  }

  return out + x;
}

static inline r_complex rray_sum_cpl_one(r_complex out, r_complex x) {
  return (r_complex) {
    .r = rray_sum_dbl_one(out.r, x.r),
    .i = rray_sum_dbl_one(out.i, x.i),
  };
}

static inline r_complex rray_sum_cpl_one_na_rm(r_complex out, r_complex x) {
  return (r_complex) {
    .r = rray_sum_dbl_one_na_rm(out.r, x.r),
    .i = rray_sum_dbl_one_na_rm(out.i, x.i),
  };
}

#endif // RRAY_ONCE

#undef RRAY_TYPE
#undef RRAY_FN
#undef RRAY_FN_ONE
#undef RRAY_FN_NA_RM_ONE
#undef RRAY_X_C_TYPE
#undef RRAY_X_CONST_DEREF
#undef RRAY_OUT_C_TYPE
#undef RRAY_OUT_ALLOC
#undef RRAY_OUT_DEREF
