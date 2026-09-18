#include "split.h"

#include "axes.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "strided-iterator.h"
#include "strides.h"
#include "utils.h"

#include "decl/split-decl.h"

r_obj* ffi_rray_split(
  r_obj* ffi_x,
  r_obj* ffi_axis,
  r_obj* ffi_dimensions,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const int axis = arg_as_int(ffi_axis, rray_args.axis, error_call);
  return rray_split(ffi_x, axis, ffi_dimensions, rray_args.x, error_call);
}

r_obj* rray_split(
  r_obj* x,
  int axis,
  r_obj* dimensions,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));

  r_obj* x_dimensions = KEEP(rray_dimensions(x, arg, error_call));
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(x_dimensions);
  check_max_dimensionality(dimensionality);

  check_axis(axis, dimensionality, rray_args.axis, error_call);
  const int axis_dimension = v_x_dimensions[axis - 1];

  dimensions = KEEP(arg_as_non_negative_bare_integer(
    dimensions,
    rray_args.dimensions,
    error_call
  ));
  check_split_dimensions(
    dimensions,
    axis_dimension,
    rray_args.dimensions,
    error_call
  );
  const int* v_dimensions = r_int_cbegin(dimensions);

  const bool uniform = r_length(dimensions) == 1;

  const r_ssize out_size =
    uniform ? axis_dimension / v_dimensions[0] : r_length(dimensions);

  r_obj* out = KEEP(r_alloc_list(out_size));

  r_obj* x_names = r_dim_names(x);

  r_obj* axis_names =
    (x_names == r_null) ? r_null : r_list_get(x_names, axis - 1);

  r_ssize v_x_strides[RRAY_MAX_DIMENSIONALITY];
  rray_fill_strides_from_dimensions(
    v_x_dimensions,
    dimensionality,
    v_x_strides
  );
  const r_ssize axis_stride = v_x_strides[axis - 1];

  const enum r_type type = r_typeof(x);

  r_obj* out_elt_dimensions = r_null;
  r_keep_loc out_elt_dimensions_loc;
  KEEP_HERE(out_elt_dimensions, &out_elt_dimensions_loc);

  struct rray_strided_iterator_plan plan = {0};
  int plan_dimension = -1;
  r_ssize out_elt_size = 0;

  int axis_offset = 0;

  for (r_ssize i = 0; i < out_size; ++i) {
    const int dimension = uniform ? v_dimensions[0] : v_dimensions[i];

    if (dimension != plan_dimension) {
      out_elt_dimensions = r_alloc_integer(dimensionality);
      KEEP_AT(out_elt_dimensions, out_elt_dimensions_loc);
      int* v_out_elt_dimensions = r_int_begin(out_elt_dimensions);
      r_memcpy(
        v_out_elt_dimensions,
        v_x_dimensions,
        sizeof(int) * dimensionality
      );
      v_out_elt_dimensions[axis - 1] = dimension;

      plan = rray_strided_iterator_plan(
        v_out_elt_dimensions,
        dimensionality,
        v_x_strides
      );

      plan_dimension = dimension;
      out_elt_size = rray_strided_iterator_plan_size(&plan);
    }

    r_obj* out_elt = r_alloc_vector(type, out_elt_size);
    r_list_poke(out, i, out_elt);
    r_attrib_poke_dim(out_elt, out_elt_dimensions);

    rray_split_fill(x, out_elt, axis_offset * axis_stride, &plan);

    if (axis_names != r_null) {
      r_obj* out_elt_names = KEEP(rray_split_elt_names(
        x_names,
        dimensionality,
        axis,
        axis_names,
        axis_offset,
        dimension
      ));
      r_attrib_poke_dim_names(out_elt, out_elt_names);
      FREE(1);
    } else if (x_names != r_null) {
      r_attrib_poke_dim_names(out_elt, x_names);
    }

    axis_offset += dimension;
  }

  FREE(5);
  return out;
}

static void check_split_dimensions(
  r_obj* dimensions,
  int axis_dimension,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  const r_ssize size = r_length(dimensions);
  const int* v_dimensions = r_int_cbegin(dimensions);

  if (size == 1) {
    const int dimension = v_dimensions[0];

    if (dimension == 0) {
      r_abort_lazy_call(
        error_call,
        "A single %s value must be positive, not 0.",
        rray_arg_format(arg)
      );
    }

    if (axis_dimension % dimension != 0) {
      r_abort_lazy_call(
        error_call,
        "A single %s value of %d must evenly divide the `axis` dimension "
        "of %d.",
        rray_arg_format(arg),
        dimension,
        axis_dimension
      );
    }
  } else {
    r_ssize total_dimension = 0;

    for (r_ssize i = 0; i < size; ++i) {
      total_dimension += v_dimensions[i];
    }

    if (total_dimension != axis_dimension) {
      r_abort_lazy_call(
        error_call,
        "%s must sum to the `axis` dimension of %d, not %" R_PRI_SSIZE ".",
        rray_arg_format(arg),
        axis_dimension,
        total_dimension
      );
    }
  }
}

static r_obj* rray_split_elt_names(
  r_obj* x_names,
  int dimensionality,
  int axis,
  r_obj* axis_names,
  int axis_offset,
  int dimension
) {
  r_obj* out = KEEP(r_alloc_list(dimensionality));

  r_obj* const* v_x_names = r_list_cbegin(x_names);

  for (int i = 0; i < dimensionality; ++i) {
    r_list_poke(out, i, v_x_names[i]);
  }

  r_obj* out_axis_names = r_alloc_character(dimension);
  r_list_poke(out, axis - 1, out_axis_names);

  r_obj* const* v_axis_names = r_chr_cbegin(axis_names);

  for (int i = 0; i < dimension; ++i) {
    r_chr_poke(out_axis_names, i, v_axis_names[axis_offset + i]);
  }

  FREE(1);
  return out;
}

static void rray_split_fill(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* plan
) {
  switch (r_typeof(x)) {
  case R_TYPE_logical:
    rray_split_fill_lgl(x, out, x_start, plan);
    break;
  case R_TYPE_integer:
    rray_split_fill_int(x, out, x_start, plan);
    break;
  case R_TYPE_double:
    rray_split_fill_dbl(x, out, x_start, plan);
    break;
  case R_TYPE_complex:
    rray_split_fill_cpl(x, out, x_start, plan);
    break;
  case R_TYPE_raw:
    rray_split_fill_raw(x, out, x_start, plan);
    break;
  case R_TYPE_character:
    rray_split_fill_chr(x, out, x_start, plan);
    break;
  case R_TYPE_list:
    rray_split_fill_list(x, out, x_start, plan);
    break;
  default:
    r_stop_unreachable();
  }
}

#define RRAY_SPLIT_FILL_ATOMIC(CTYPE, CONST_DEREF, DEREF)                      \
  const r_ssize size = rray_strided_iterator_plan_size(plan);                  \
                                                                               \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
  CTYPE* v_out = DEREF(out);                                                   \
                                                                               \
  r_ssize run_start = 0;                                                       \
  const r_ssize run_size = rray_strided_iterator_plan_run_size(plan);          \
  const r_ssize x_run_stride = rray_strided_iterator_plan_run_stride(plan);    \
                                                                               \
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];                                    \
  rray_strided_iterator_plan_point_init(plan, v_point);                        \
                                                                               \
  while (run_start != size) {                                                  \
    const r_ssize run_end = run_start + run_size;                              \
                                                                               \
    r_ssize x_loc = x_start;                                                   \
                                                                               \
    for (r_ssize i = run_start; i < run_end; ++i) {                            \
      v_out[i] = v_x[x_loc];                                                   \
      x_loc += x_run_stride;                                                   \
    }                                                                          \
                                                                               \
    run_start = run_end;                                                       \
    RRAY_STRIDED_ITERATOR_NEXT(x_start, v_point, plan);                        \
  }

#define RRAY_SPLIT_FILL_BARRIER(CONST_DEREF, POKE)                             \
  const r_ssize size = rray_strided_iterator_plan_size(plan);                  \
                                                                               \
  r_obj* const* v_x = CONST_DEREF(x);                                          \
                                                                               \
  r_ssize run_start = 0;                                                       \
  const r_ssize run_size = rray_strided_iterator_plan_run_size(plan);          \
  const r_ssize x_run_stride = rray_strided_iterator_plan_run_stride(plan);    \
                                                                               \
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];                                    \
  rray_strided_iterator_plan_point_init(plan, v_point);                        \
                                                                               \
  while (run_start != size) {                                                  \
    const r_ssize run_end = run_start + run_size;                              \
    r_ssize x_loc = x_start;                                                   \
                                                                               \
    for (r_ssize i = run_start; i < run_end; ++i) {                            \
      POKE(out, i, v_x[x_loc]);                                                \
      x_loc += x_run_stride;                                                   \
    }                                                                          \
                                                                               \
    run_start = run_end;                                                       \
    RRAY_STRIDED_ITERATOR_NEXT(x_start, v_point, plan);                        \
  }

static void rray_split_fill_lgl(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_SPLIT_FILL_ATOMIC(int, r_lgl_cbegin, r_lgl_begin);
}

static void rray_split_fill_int(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_SPLIT_FILL_ATOMIC(int, r_int_cbegin, r_int_begin);
}

static void rray_split_fill_dbl(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_SPLIT_FILL_ATOMIC(double, r_dbl_cbegin, r_dbl_begin);
}

static void rray_split_fill_cpl(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_SPLIT_FILL_ATOMIC(r_complex, r_cpl_cbegin, r_cpl_begin);
}

static void rray_split_fill_raw(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_SPLIT_FILL_ATOMIC(Rbyte, r_raw_cbegin, r_raw_begin);
}

static void rray_split_fill_chr(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_SPLIT_FILL_BARRIER(r_chr_cbegin, r_chr_poke);
}

static void rray_split_fill_list(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* plan
) {
  RRAY_SPLIT_FILL_BARRIER(r_list_cbegin, r_list_poke);
}

#undef RRAY_SPLIT_FILL_ATOMIC
#undef RRAY_SPLIT_FILL_BARRIER
