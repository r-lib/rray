#include "combine.h"

#include <limits.h>

#include "axes.h"
#include "broadcast-names.h"
#include "cast-common.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "ptype-common.h"
#include "strided-iterator.h"
#include "strides.h"
#include "utils.h"

#include "decl/combine-decl.h"

r_obj* ffi_rray_combine(r_obj* ffi_xs, r_obj* ffi_axis, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};

  const int axis = arg_as_int(ffi_axis, rray_args.dot_axis, error_call);
  return rray_combine(ffi_xs, axis, error_call);
}

r_obj* rray_combine(r_obj* xs, int axis, struct r_lazy error_call) {
  if (r_length(xs) == 0) {
    r_abort_lazy_call(error_call, "Must supply at least one array to `...`.");
  }

  r_obj* ptype =
    KEEP(rray_ptype_common(xs, r_null, NULL, rray_args.empty, error_call));

  xs = KEEP(rray_cast_common(xs, ptype, NULL, rray_args.empty, error_call));

  const r_ssize xs_size = r_length(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);

  int dimensionality = 1;

  for (r_ssize i = 0; i < xs_size; ++i) {
    const int x_dimensionality =
      rray_dimensionality(v_xs[i], rray_args.empty, error_call);
    if (x_dimensionality > dimensionality) {
      dimensionality = x_dimensionality;
    }
  }

  check_axis(axis, dimensionality, rray_args.dot_axis, error_call);

  r_obj* xs_names = KEEP(r_names(xs));

  r_ssize x_i = 0;
  struct rray_arg* x_arg = new_subscript_arg(NULL, xs_names, xs_size, &x_i);
  KEEP(x_arg->shelter);

  r_ssize out_i = 0;
  struct rray_arg* out_arg = new_subscript_arg(NULL, xs_names, xs_size, &out_i);
  KEEP(out_arg->shelter);

  r_obj* out_dimensions = KEEP(r_alloc_integer(dimensionality));
  int* v_out_dimensions = r_int_begin(out_dimensions);

  r_ssize v_out_args[RRAY_MAX_DIMENSIONALITY];

  for (int i = 0; i < dimensionality; ++i) {
    v_out_dimensions[i] = 1;
    v_out_args[i] = 0;
  }

  r_ssize axis_dimension = 0;

  for (; x_i < xs_size; ++x_i) {
    r_obj* x_dimensions = r_dim(v_xs[x_i]);
    const int* v_x_dimensions = r_int_cbegin(x_dimensions);
    const int x_dimensionality = (int) r_length(x_dimensions);

    for (int i = 0; i < dimensionality; ++i) {
      const int x_dimension = (i < x_dimensionality) ? v_x_dimensions[i] : 1;

      if (i == axis - 1) {
        if (axis_dimension > INT_MAX - x_dimension) {
          r_abort_lazy_call(
            error_call,
            "The combined dimension on `.axis` is too large."
          );
        }
        axis_dimension += x_dimension;
        continue;
      }

      const int out_dimension = v_out_dimensions[i];

      if (out_dimension == x_dimension || x_dimension == 1) {
        continue;
      }

      if (out_dimension == 1) {
        v_out_dimensions[i] = x_dimension;
        v_out_args[i] = x_i;
        continue;
      }

      out_i = v_out_args[i];
      r_abort_lazy_call(
        error_call,
        "Can't find common dimensions at axis %d. "
        "%s has dimension %d and %s has dimension %d.",
        i + 1,
        rray_arg_format(out_arg),
        out_dimension,
        rray_arg_format(x_arg),
        x_dimension
      );
    }
  }

  v_out_dimensions[axis - 1] = (int) axis_dimension;
  const r_ssize out_size =
    rray_combine_size(v_out_dimensions, dimensionality, error_call);

  r_obj* out = KEEP(r_alloc_vector(r_typeof(ptype), out_size));

  r_ssize v_out_strides[RRAY_MAX_DIMENSIONALITY];
  rray_fill_strides_from_dimensions(
    v_out_dimensions,
    dimensionality,
    v_out_strides
  );

  r_ssize axis_offset = 0;

  for (x_i = 0; x_i < xs_size; ++x_i) {
    r_obj* x = v_xs[x_i];
    r_obj* x_dimensions = r_dim(x);
    const int* v_x_dimensions = r_int_cbegin(x_dimensions);
    const int x_dimensionality = (int) r_length(x_dimensions);
    const int x_axis_dimension =
      (axis <= x_dimensionality) ? v_x_dimensions[axis - 1] : 1;

    int v_x_broadcast_dimensions[RRAY_MAX_DIMENSIONALITY];
    r_memcpy(
      v_x_broadcast_dimensions,
      v_out_dimensions,
      sizeof(int) * dimensionality
    );
    v_x_broadcast_dimensions[axis - 1] = x_axis_dimension;

    r_ssize v_x_strides[RRAY_MAX_DIMENSIONALITY];
    rray__fill_broadcast_strides(
      v_x_dimensions,
      x_dimensionality,
      dimensionality,
      v_x_strides
    );

    const struct rray_strided_iterator2_plan plan = rray_strided_iterator2_plan(
      v_x_broadcast_dimensions,
      dimensionality,
      v_out_strides,
      v_x_strides
    );

    const r_ssize out_start = axis_offset * v_out_strides[axis - 1];
    rray_combine_copy(x, out, out_start, &plan);
    axis_offset += x_axis_dimension;
  }

  r_obj* out_names = KEEP(rray_combine_names(xs, out_dimensions, axis));

  r_attrib_poke_dim(out, out_dimensions);
  if (out_names != r_null) {
    r_attrib_poke_dim_names(out, out_names);
  }

  FREE(8);
  return out;
}

static r_ssize rray_combine_size(
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
) {
  r_ssize out = 1;

  for (int i = 0; i < dimensionality; ++i) {
    const int dimension = v_dimensions[i];
    if (dimension != 0 && out > R_SSIZE_MAX / dimension) {
      r_abort_lazy_call(error_call, "The result is too large.");
    }
    out *= dimension;
  }

  return out;
}

static r_obj* rray_combine_names(r_obj* xs, r_obj* dimensions, int axis) {
  r_obj* out = rray_broadcast_names_common(xs, dimensions);
  r_keep_loc out_loc;
  KEEP_HERE(out, &out_loc);

  if (out != r_null) {
    r_list_poke(out, axis - 1, r_null);
  }

  r_obj* axis_names = KEEP(rray_combine_axis_names(xs, axis));

  if (axis_names != r_null) {
    if (out == r_null) {
      out = r_alloc_list(r_length(dimensions));
      KEEP_AT(out, out_loc);
    }
    r_list_poke(out, axis - 1, axis_names);
  }

  FREE(2);
  return out;
}

static r_obj* rray_combine_axis_names(r_obj* xs, int axis) {
  const r_ssize n = r_length(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);

  bool any_names = false;
  r_ssize size = 0;

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* x = v_xs[i];
    r_obj* dimensions = r_dim(x);
    const int dimensionality = (int) r_length(dimensions);
    const int dimension =
      (axis <= dimensionality) ? r_int_get(dimensions, axis - 1) : 1;
    size += dimension;

    r_obj* names = r_dim_names(x);
    if (names != r_null && axis <= dimensionality) {
      any_names = any_names || r_list_get(names, axis - 1) != r_null;
    }
  }

  if (!any_names) {
    return r_null;
  }

  r_obj* out = KEEP(r_alloc_character(size));
  r_ssize out_i = 0;

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* x = v_xs[i];
    r_obj* dimensions = r_dim(x);
    const int dimensionality = (int) r_length(dimensions);
    const int dimension =
      (axis <= dimensionality) ? r_int_get(dimensions, axis - 1) : 1;
    r_obj* names = r_dim_names(x);
    r_obj* axis_names = (names != r_null && axis <= dimensionality)
      ? r_list_get(names, axis - 1)
      : r_null;

    if (axis_names == r_null) {
      for (int j = 0; j < dimension; ++j) {
        r_chr_poke(out, out_i, r_strs.empty);
        ++out_i;
      }
    } else {
      r_obj* const* v_axis_names = r_chr_cbegin(axis_names);
      for (int j = 0; j < dimension; ++j) {
        r_chr_poke(out, out_i, v_axis_names[j]);
        ++out_i;
      }
    }
  }

  FREE(1);
  return out;
}

static void rray_combine_copy(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
) {
  switch (r_typeof(x)) {
  case R_TYPE_logical:
    rray_combine_lgl(x, out, out_start, plan);
    break;
  case R_TYPE_integer:
    rray_combine_int(x, out, out_start, plan);
    break;
  case R_TYPE_double:
    rray_combine_dbl(x, out, out_start, plan);
    break;
  case R_TYPE_complex:
    rray_combine_cpl(x, out, out_start, plan);
    break;
  case R_TYPE_raw:
    rray_combine_raw(x, out, out_start, plan);
    break;
  case R_TYPE_character:
    rray_combine_chr(x, out, out_start, plan);
    break;
  case R_TYPE_list:
    rray_combine_list(x, out, out_start, plan);
    break;
  default:
    r_stop_unreachable();
  }
}

#define RRAY_COMBINE_ATOMIC(CTYPE, CONST_DEREF, DEREF)                         \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
  CTYPE* v_out = DEREF(out);                                                   \
  const r_ssize size = rray_strided_iterator2_plan_size(plan);                 \
  const r_ssize run_size = rray_strided_iterator2_plan_run_size(plan);         \
  const r_ssize out_stride = rray_strided_iterator2_plan_run_stride1(plan);    \
  const r_ssize x_stride = rray_strided_iterator2_plan_run_stride2(plan);      \
  r_ssize run_start = 0;                                                       \
  r_ssize x_start = 0;                                                         \
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];                                    \
  r_memset(v_point, 0, sizeof(r_ssize) * (size_t) plan->dimensionality);       \
  while (run_start != size) {                                                  \
    const r_ssize run_end = run_start + run_size;                              \
    r_ssize out_loc = out_start;                                               \
    r_ssize x_loc = x_start;                                                   \
    for (r_ssize i = run_start; i < run_end; ++i) {                            \
      v_out[out_loc] = v_x[x_loc];                                             \
      out_loc += out_stride;                                                   \
      x_loc += x_stride;                                                       \
    }                                                                          \
    run_start = run_end;                                                       \
    RRAY_STRIDED_ITERATOR_NEXT2(out_start, x_start, v_point, plan);            \
  }

#define RRAY_COMBINE_BARRIER(CONST_DEREF, POKE)                                \
  r_obj* const* v_x = CONST_DEREF(x);                                          \
  const r_ssize size = rray_strided_iterator2_plan_size(plan);                 \
  const r_ssize run_size = rray_strided_iterator2_plan_run_size(plan);         \
  const r_ssize out_stride = rray_strided_iterator2_plan_run_stride1(plan);    \
  const r_ssize x_stride = rray_strided_iterator2_plan_run_stride2(plan);      \
  r_ssize run_start = 0;                                                       \
  r_ssize x_start = 0;                                                         \
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];                                    \
  r_memset(v_point, 0, sizeof(r_ssize) * (size_t) plan->dimensionality);       \
  while (run_start != size) {                                                  \
    const r_ssize run_end = run_start + run_size;                              \
    r_ssize out_loc = out_start;                                               \
    r_ssize x_loc = x_start;                                                   \
    for (r_ssize i = run_start; i < run_end; ++i) {                            \
      POKE(out, out_loc, v_x[x_loc]);                                          \
      out_loc += out_stride;                                                   \
      x_loc += x_stride;                                                       \
    }                                                                          \
    run_start = run_end;                                                       \
    RRAY_STRIDED_ITERATOR_NEXT2(out_start, x_start, v_point, plan);            \
  }

static void rray_combine_lgl(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
) {
  RRAY_COMBINE_ATOMIC(int, r_lgl_cbegin, r_lgl_begin);
}

static void rray_combine_int(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
) {
  RRAY_COMBINE_ATOMIC(int, r_int_cbegin, r_int_begin);
}

static void rray_combine_dbl(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
) {
  RRAY_COMBINE_ATOMIC(double, r_dbl_cbegin, r_dbl_begin);
}

static void rray_combine_cpl(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
) {
  RRAY_COMBINE_ATOMIC(r_complex, r_cpl_cbegin, r_cpl_begin);
}

static void rray_combine_raw(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
) {
  RRAY_COMBINE_ATOMIC(Rbyte, r_raw_cbegin, r_raw_begin);
}

static void rray_combine_chr(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
) {
  RRAY_COMBINE_BARRIER(r_chr_cbegin, r_chr_poke);
}

static void rray_combine_list(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
) {
  RRAY_COMBINE_BARRIER(r_list_cbegin, r_list_poke);
}

#undef RRAY_COMBINE_ATOMIC
#undef RRAY_COMBINE_BARRIER
