#include "sum.h"

#include "axes.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "names.h"
#include "reduce.h"
#include "reduction-iterator.h"
#include "size.h"
#include "utils.h"

#include "decl/sum-decl.h"

r_obj* ffi_rray_sum(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const bool na_rm = r_arg_as_bool(ffi_na_rm, "na_rm");
  return rray_sum(ffi_x, ffi_axes, na_rm, error_call);
}

r_obj* rray_sum(r_obj* x, r_obj* axes, bool na_rm, struct r_lazy error_call) {
  check_unclassed(x, "x", error_call);
  x = KEEP(arg_as_array(x, "x", error_call));

  r_obj* x_dimensions = KEEP(rray_dimensions(x, "x", error_call));
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);

  const int dimensionality = rray_dimensionality_from_dimensions(x_dimensions);

  axes = KEEP(arg_as_axes(axes, dimensionality, axes_chr, error_call));
  const int* v_axes = r_int_cbegin(axes);
  const r_ssize axes_size = r_length(axes);

  r_obj* out_dimensions = KEEP(
    rray_reduce_dimensions(v_x_dimensions, dimensionality, v_axes, axes_size)
  );
  const int* v_out_dimensions = r_int_cbegin(out_dimensions);

  const r_ssize out_size =
    rray_size_from_dimensions(v_out_dimensions, dimensionality);

  struct rray_iterator it;
  rray_reduction_iterator_init(
    &it,
    v_x_dimensions,
    v_out_dimensions,
    dimensionality
  );

  r_obj* out;

  switch (r_typeof(x)) {
  case R_TYPE_logical:
    if (na_rm) {
      out = rray_sum_lgl_na_rm(x, out_size, &it);
    } else {
      out = rray_sum_lgl(x, out_size, &it);
    }
    break;
  case R_TYPE_integer:
    if (na_rm) {
      out = rray_sum_int_na_rm(x, out_size, &it);
    } else {
      out = rray_sum_int(x, out_size, &it);
    }
    break;
  case R_TYPE_double:
    if (na_rm) {
      out = rray_sum_dbl_na_rm(x, out_size, &it);
    } else {
      out = rray_sum_dbl(x, out_size, &it);
    }
    break;
  case R_TYPE_complex:
    if (na_rm) {
      out = rray_sum_cpl_na_rm(x, out_size, &it);
    } else {
      out = rray_sum_cpl(x, out_size, &it);
    }
    break;
  default:
    r_abort_lazy_call(
      error_call,
      "`x` must be a logical, integer, double, or complex array, not %s.",
      r_obj_type_friendly(x)
    );
  }

  KEEP(out);
  r_attrib_poke_dim(out, out_dimensions);

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

  FREE(5);
  return out;
}

#define RRAY_SUM(OUT_RTYPE, CTYPE, X_CONST_DEREF, OUT_DEREF, ONE)              \
  r_obj* out = KEEP(r_alloc_vector(OUT_RTYPE, out_size));                      \
  CTYPE* v_out = OUT_DEREF(out);                                               \
  r_memset(v_out, 0, sizeof(CTYPE) * out_size);                                \
                                                                               \
  const r_ssize x_size = r_length(x);                                          \
  const CTYPE* v_x = X_CONST_DEREF(x);                                         \
                                                                               \
  for (r_ssize i = 0; i < x_size; ++i) {                                       \
    const r_ssize loc = rray_iterator_location(it);                            \
    v_out[loc] = ONE(v_out[loc], v_x[i]);                                      \
    rray_iterator_next(it);                                                    \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

static r_obj* rray_sum_lgl(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
) {
  RRAY_SUM(R_TYPE_integer, int, r_lgl_cbegin, r_int_begin, rray_sum_lgl_one);
}

static r_obj* rray_sum_lgl_na_rm(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
) {
  RRAY_SUM(
    R_TYPE_integer,
    int,
    r_lgl_cbegin,
    r_int_begin,
    rray_sum_lgl_one_na_rm
  );
}

static r_obj* rray_sum_int(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
) {
  RRAY_SUM(R_TYPE_integer, int, r_int_cbegin, r_int_begin, rray_sum_int_one);
}

static r_obj* rray_sum_int_na_rm(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
) {
  RRAY_SUM(
    R_TYPE_integer,
    int,
    r_int_cbegin,
    r_int_begin,
    rray_sum_int_one_na_rm
  );
}

static r_obj* rray_sum_dbl(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
) {
  RRAY_SUM(R_TYPE_double, double, r_dbl_cbegin, r_dbl_begin, rray_sum_dbl_one);
}

static r_obj* rray_sum_dbl_na_rm(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
) {
  RRAY_SUM(
    R_TYPE_double,
    double,
    r_dbl_cbegin,
    r_dbl_begin,
    rray_sum_dbl_one_na_rm
  );
}

static r_obj* rray_sum_cpl(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
) {
  RRAY_SUM(
    R_TYPE_complex,
    r_complex,
    r_cpl_cbegin,
    r_cpl_begin,
    rray_sum_cpl_one
  );
}

static r_obj* rray_sum_cpl_na_rm(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
) {
  RRAY_SUM(
    R_TYPE_complex,
    r_complex,
    r_cpl_cbegin,
    r_cpl_begin,
    rray_sum_cpl_one_na_rm
  );
}

#undef RRAY_SUM

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
  return (r_complex){
    .r = rray_sum_dbl_one(out.r, x.r),
    .i = rray_sum_dbl_one(out.i, x.i),
  };
}

static inline r_complex rray_sum_cpl_one_na_rm(r_complex out, r_complex x) {
  return (r_complex){
    .r = rray_sum_dbl_one_na_rm(out.r, x.r),
    .i = rray_sum_dbl_one_na_rm(out.i, x.i),
  };
}

static inline void check_sum_int_overflow(int out, int x) {
  if ((x > 0 && out > INT_MAX - x) || (x < 0 && out < -INT_MAX - x)) {
    r_abort("Integer overflow.");
  }
}
