#include "roll.h"

#include "axes.h"
#include "broadcast.h"
#include "cast.h"
#include "dimensionality.h"
#include "reduce-names.h"
#include "slice.h"
#include "strided-iterator.h"
#include "utils.h"

#include "decl/roll-decl.h"

r_obj* ffi_rray_roll(
  r_obj* ffi_x,
  r_obj* ffi_n,
  r_obj* ffi_axes,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_roll(ffi_x, ffi_n, ffi_axes, rray_args.x, error_call);
}

r_obj* rray_roll(
  r_obj* x,
  r_obj* n,
  r_obj* axes,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  check_unclassed(x, x_arg, error_call);
  x = KEEP(arg_as_array(x, x_arg, error_call));

  r_obj* x_dimensions = r_dim(x);
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(x_dimensions);
  check_dimensionality(dimensionality);

  axes = KEEP(arg_as_axes(axes, dimensionality, rray_args.axes, error_call));
  const int* v_axes = r_int_cbegin(axes);
  const r_ssize axes_size = r_length(axes);

  n = KEEP(arg_as_roll_n(n, axes_size, rray_args.n, error_call));
  const int* v_n = r_int_cbegin(n);
  const r_ssize n_size = r_length(n);

  r_obj* indices = KEEP(r_alloc_list(dimensionality));

  for (int i = 0; i < dimensionality; ++i) {
    r_list_poke(indices, i, r_true);
  }

  for (r_ssize i = 0; i < axes_size; ++i) {
    const int axis = v_axes[i];
    const int dimension = v_x_dimensions[axis - 1];
    const int n = rray_roll_normalize(v_n[n_size == 1 ? 0 : i], dimension);
    r_list_poke(indices, axis - 1, rray_roll_locations(dimension, n));
  }

  r_obj* out = rray_slice(x, indices, x_arg, rray_args.empty, error_call);

  FREE(4);
  return out;
}

static r_obj* arg_as_roll_n(
  r_obj* n,
  r_ssize axes_size,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  n = KEEP(arg_as_bare_integer(n, arg, error_call));
  check_roll_n_not_missing(n, arg, error_call);

  const r_ssize n_size = r_length(n);

  if (n_size != 1 && n_size != axes_size) {
    stop_roll_n_size(n_size, axes_size, arg, error_call);
  }

  FREE(1);
  return n;
}

static r_no_return void stop_roll_n_size(
  r_ssize n_size,
  r_ssize axes_size,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  if (axes_size == 1) {
    r_abort_lazy_call(
      error_call,
      "%s must be size 1, not size %" R_PRI_SSIZE ".",
      rray_arg_format(arg),
      n_size
    );
  } else {
    r_abort_lazy_call(
      error_call,
      "%s must be size 1 or size %" R_PRI_SSIZE " to match `axes`, "
      "not size %" R_PRI_SSIZE ".",
      rray_arg_format(arg),
      axes_size,
      n_size
    );
  }
}

static r_obj* rray_roll_locations(int dimension, int n) {
  r_obj* out = KEEP(r_alloc_integer(dimension));
  int* v_out = r_int_begin(out);

  for (int i = 0; i < dimension; ++i) {
    v_out[i] = (i >= n) ? i - n + 1 : i - n + dimension + 1;
  }

  FREE(1);
  return out;
}

r_obj* ffi_rray_roll_each(
  r_obj* ffi_x,
  r_obj* ffi_n,
  r_obj* ffi_axis,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const int axis = arg_as_int(ffi_axis, rray_args.axis, error_call);
  return rray_roll_each(ffi_x, ffi_n, axis, rray_args.x, error_call);
}

r_obj* rray_roll_each(
  r_obj* x,
  r_obj* n,
  int axis,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  check_unclassed(x, x_arg, error_call);
  x = KEEP(arg_as_array(x, x_arg, error_call));

  r_obj* x_dimensions = r_dim(x);
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(x_dimensions);
  check_dimensionality(dimensionality);

  check_axis(axis, dimensionality, rray_args.axis, error_call);
  const int axis_dimension = v_x_dimensions[axis - 1];

  n = KEEP(arg_as_roll_each_n(n, rray_args.n, error_call));

  r_obj* n_dimensions = r_dim(n);
  const int* v_n_dimensions = r_int_cbegin(n_dimensions);
  const int n_dimensionality =
    rray_dimensionality_from_dimensions(n_dimensions);

  int v_lane_dimensions[RRAY_MAX_DIMENSIONALITY];
  r_memcpy(v_lane_dimensions, v_x_dimensions, sizeof(int) * dimensionality);
  v_lane_dimensions[axis - 1] = 1;

  check_broadcastable(
    v_n_dimensions,
    n_dimensionality,
    v_lane_dimensions,
    dimensionality,
    rray_args.n,
    error_call
  );

  const r_ssize size = r_length(x);

  r_obj* out = KEEP(r_alloc_vector(r_typeof(x), size));
  r_attrib_poke_dim(out, x_dimensions);

  if (size != 0) {
    const struct rray_strided_iterator_plan plan = rray_broadcast_iterator_plan(
      v_n_dimensions,
      n_dimensionality,
      v_lane_dimensions,
      dimensionality
    );

    r_obj* normalized_n =
      KEEP(rray_roll_each_normalize(n, &plan, axis_dimension));
    const int* v_normalized_n = r_int_cbegin(normalized_n);

    r_ssize block_size = 1;
    for (int i = 0; i < axis - 1; ++i) {
      block_size *= v_x_dimensions[i];
    }

    r_ssize n_groups = 1;
    for (int i = axis; i < dimensionality; ++i) {
      n_groups *= v_x_dimensions[i];
    }

    rray_roll_each_fill(
      x,
      out,
      v_normalized_n,
      block_size,
      axis_dimension,
      n_groups
    );

    FREE(1);
  }

  r_obj* axes = KEEP(r_int(axis));
  r_obj* out_names = KEEP(rray_reduce_names(x, axes));

  if (out_names != r_null) {
    r_attrib_poke_dim_names(out, out_names);
  }

  FREE(5);
  return out;
}

static r_obj* arg_as_roll_each_n(
  r_obj* n,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  n = KEEP(rray_cast(n, r_globals.empty_int, arg, rray_args.empty, error_call));
  check_roll_n_not_missing(n, arg, error_call);
  FREE(1);
  return n;
}

static r_obj* rray_roll_each_normalize(
  r_obj* n,
  const struct rray_strided_iterator_plan* plan,
  int axis_dimension
) {
  const r_ssize size = rray_strided_iterator_plan_size(plan);

  r_obj* out = KEEP(r_alloc_integer(size));
  int* v_out = r_int_begin(out);

  const int* v_n = r_int_cbegin(n);

  r_ssize run_start = 0;
  const r_ssize run_size = rray_strided_iterator_plan_run_size(plan);

  r_ssize n_start = 0;
  const r_ssize n_run_stride = rray_strided_iterator_plan_run_stride(plan);

  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];
  rray_strided_iterator_plan_point_init(plan, v_point);

  while (run_start != size) {
    const r_ssize run_end = run_start + run_size;
    r_ssize n_loc = n_start;

    if (n_run_stride == 0) {
      const int n_elt = rray_roll_normalize(v_n[n_loc], axis_dimension);
      for (r_ssize i = run_start; i < run_end; ++i) {
        v_out[i] = n_elt;
      }
    } else {
      for (r_ssize i = run_start; i < run_end; ++i) {
        v_out[i] = rray_roll_normalize(v_n[n_loc], axis_dimension);
        n_loc += n_run_stride;
      }
    }

    run_start = run_end;
    RRAY_STRIDED_ITERATOR_NEXT(n_start, v_point, plan);
  }

  FREE(1);
  return out;
}

static void rray_roll_each_fill(
  r_obj* x,
  r_obj* out,
  const int* v_n,
  r_ssize block_size,
  int axis_dimension,
  r_ssize n_groups
) {
  switch (r_typeof(x)) {
  case R_TYPE_logical:
    rray_roll_each_fill_lgl(x, out, v_n, block_size, axis_dimension, n_groups);
    break;
  case R_TYPE_integer:
    rray_roll_each_fill_int(x, out, v_n, block_size, axis_dimension, n_groups);
    break;
  case R_TYPE_double:
    rray_roll_each_fill_dbl(x, out, v_n, block_size, axis_dimension, n_groups);
    break;
  case R_TYPE_complex:
    rray_roll_each_fill_cpl(x, out, v_n, block_size, axis_dimension, n_groups);
    break;
  case R_TYPE_raw:
    rray_roll_each_fill_raw(x, out, v_n, block_size, axis_dimension, n_groups);
    break;
  case R_TYPE_character:
    rray_roll_each_fill_chr(x, out, v_n, block_size, axis_dimension, n_groups);
    break;
  case R_TYPE_list:
    rray_roll_each_fill_list(x, out, v_n, block_size, axis_dimension, n_groups);
    break;
  default:
    r_stop_unreachable();
  }
}

#define RRAY_ROLL_EACH_FILL_ATOMIC(CTYPE, CONST_DEREF, DEREF)                  \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
  CTYPE* v_out = DEREF(out);                                                   \
                                                                               \
  const r_ssize group_size = block_size * axis_dimension;                      \
                                                                               \
  r_ssize out_i = 0;                                                           \
                                                                               \
  for (r_ssize group = 0; group < n_groups; ++group) {                         \
    const CTYPE* v_x_group = v_x + group * group_size;                         \
    const int* v_n_group = v_n + group * block_size;                           \
                                                                               \
    for (int j = 0; j < axis_dimension; ++j) {                                 \
      for (r_ssize i = 0; i < block_size; ++i) {                               \
        const int n = v_n_group[i];                                            \
        const int source = (j >= n) ? j - n : j - n + axis_dimension;          \
        v_out[out_i] = v_x_group[source * block_size + i];                     \
        ++out_i;                                                               \
      }                                                                        \
    }                                                                          \
  }

#define RRAY_ROLL_EACH_FILL_BARRIER(CONST_DEREF, POKE)                         \
  r_obj* const* v_x = CONST_DEREF(x);                                          \
                                                                               \
  const r_ssize group_size = block_size * axis_dimension;                      \
                                                                               \
  r_ssize out_i = 0;                                                           \
                                                                               \
  for (r_ssize group = 0; group < n_groups; ++group) {                         \
    r_obj* const* v_x_group = v_x + group * group_size;                        \
    const int* v_n_group = v_n + group * block_size;                           \
                                                                               \
    for (int j = 0; j < axis_dimension; ++j) {                                 \
      for (r_ssize i = 0; i < block_size; ++i) {                               \
        const int n = v_n_group[i];                                            \
        const int source = (j >= n) ? j - n : j - n + axis_dimension;          \
        POKE(out, out_i, v_x_group[source * block_size + i]);                  \
        ++out_i;                                                               \
      }                                                                        \
    }                                                                          \
  }

static void rray_roll_each_fill_lgl(
  r_obj* x,
  r_obj* out,
  const int* v_n,
  r_ssize block_size,
  int axis_dimension,
  r_ssize n_groups
) {
  RRAY_ROLL_EACH_FILL_ATOMIC(int, r_lgl_cbegin, r_lgl_begin);
}

static void rray_roll_each_fill_int(
  r_obj* x,
  r_obj* out,
  const int* v_n,
  r_ssize block_size,
  int axis_dimension,
  r_ssize n_groups
) {
  RRAY_ROLL_EACH_FILL_ATOMIC(int, r_int_cbegin, r_int_begin);
}

static void rray_roll_each_fill_dbl(
  r_obj* x,
  r_obj* out,
  const int* v_n,
  r_ssize block_size,
  int axis_dimension,
  r_ssize n_groups
) {
  RRAY_ROLL_EACH_FILL_ATOMIC(double, r_dbl_cbegin, r_dbl_begin);
}

static void rray_roll_each_fill_cpl(
  r_obj* x,
  r_obj* out,
  const int* v_n,
  r_ssize block_size,
  int axis_dimension,
  r_ssize n_groups
) {
  RRAY_ROLL_EACH_FILL_ATOMIC(r_complex, r_cpl_cbegin, r_cpl_begin);
}

static void rray_roll_each_fill_raw(
  r_obj* x,
  r_obj* out,
  const int* v_n,
  r_ssize block_size,
  int axis_dimension,
  r_ssize n_groups
) {
  RRAY_ROLL_EACH_FILL_ATOMIC(Rbyte, r_raw_cbegin, r_raw_begin);
}

static void rray_roll_each_fill_chr(
  r_obj* x,
  r_obj* out,
  const int* v_n,
  r_ssize block_size,
  int axis_dimension,
  r_ssize n_groups
) {
  RRAY_ROLL_EACH_FILL_BARRIER(r_chr_cbegin, r_chr_poke);
}

static void rray_roll_each_fill_list(
  r_obj* x,
  r_obj* out,
  const int* v_n,
  r_ssize block_size,
  int axis_dimension,
  r_ssize n_groups
) {
  RRAY_ROLL_EACH_FILL_BARRIER(r_list_cbegin, r_list_poke);
}

#undef RRAY_ROLL_EACH_FILL_ATOMIC
#undef RRAY_ROLL_EACH_FILL_BARRIER

static void check_roll_n_not_missing(
  r_obj* n,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  const r_ssize n_size = r_length(n);
  const int* v_n = r_int_cbegin(n);

  for (r_ssize i = 0; i < n_size; ++i) {
    if (v_n[i] == r_globals.na_int) {
      r_abort_lazy_call(
        error_call,
        "%s must not contain missing values.",
        rray_arg_format(arg)
      );
    }
  }
}

// Bound `n` between `[0, dimension]`
static inline int rray_roll_normalize(int n, int dimension) {
  if (dimension == 0) {
    return 0;
  }

  int out = n % dimension;

  if (out < 0) {
    out += dimension;
  }

  return out;
}
