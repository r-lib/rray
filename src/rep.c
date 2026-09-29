#include "rep.h"

#include <limits.h>

#include "axes.h"
#include "dimensionality.h"
#include "size.h"
#include "utils.h"

#include "decl/rep-decl.h"

r_obj* ffi_rray_rep(
  r_obj* ffi_x,
  r_obj* ffi_times,
  r_obj* ffi_axes,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_rep(ffi_x, ffi_times, ffi_axes, rray_args.x, error_call);
}

r_obj* rray_rep(
  r_obj* x,
  r_obj* times,
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

  times = KEEP(arg_as_rep_times(times, axes_size, rray_args.times, error_call));
  const int* v_times = r_int_cbegin(times);
  const r_ssize times_size = r_length(times);

  r_obj* out_dimensions = KEEP(r_alloc_integer(dimensionality));
  int* v_out_dimensions = r_int_begin(out_dimensions);
  r_memcpy(v_out_dimensions, v_x_dimensions, sizeof(int) * dimensionality);

  for (r_ssize i = 0; i < axes_size; ++i) {
    const int axis = v_axes[i];
    const int times = v_times[times_size == 1 ? 0 : i];
    v_out_dimensions[axis - 1] =
      rray_rep_dimension(v_x_dimensions[axis - 1], times, error_call);
  }

  const r_ssize out_size = rray_size_from_dimensions_checked(
    v_out_dimensions,
    dimensionality,
    error_call
  );

  r_obj* out = KEEP(r_alloc_vector(r_typeof(x), out_size));
  r_attrib_poke_dim(out, out_dimensions);

  const int axis = axes_size == 0 ? dimensionality : v_axes[0];
  const int axis_times = axes_size == 0 ? 1 : v_times[0];

  r_ssize block_size = 1;
  for (int i = 0; i < axis; ++i) {
    block_size *= v_x_dimensions[i];
  }

  rray_rep_fill(
    x,
    out,
    v_x_dimensions,
    v_out_dimensions,
    dimensionality,
    axis,
    block_size,
    axis_times
  );

  r_obj* out_names = KEEP(
    rray_rep_names(r_dim_names(x), v_axes, axes_size, v_times, times_size)
  );
  if (out_names != r_null) {
    r_attrib_poke_dim_names(out, out_names);
  }

  FREE(6);
  return out;
}

static r_obj* arg_as_rep_times(
  r_obj* times,
  r_ssize axes_size,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  times = KEEP(arg_as_non_negative_bare_integer(times, arg, error_call));

  const r_ssize times_size = r_length(times);

  if (times_size != 1 && times_size != axes_size) {
    stop_rep_times_size(times_size, axes_size, arg, error_call);
  }

  FREE(1);
  return times;
}

static r_no_return void stop_rep_times_size(
  r_ssize times_size,
  r_ssize axes_size,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  if (axes_size == 1) {
    r_abort_lazy_call(
      error_call,
      "%s must be size 1, not size %" R_PRI_SSIZE ".",
      rray_arg_format(arg),
      times_size
    );
  } else {
    r_abort_lazy_call(
      error_call,
      "%s must be size 1 or size %" R_PRI_SSIZE " to match `axes`, "
      "not size %" R_PRI_SSIZE ".",
      rray_arg_format(arg),
      axes_size,
      times_size
    );
  }
}

static int rray_rep_dimension(
  int dimension,
  int times,
  struct r_lazy error_call
) {
  if (times != 0 && dimension > INT_MAX / times) {
    stop_rep_dimension_too_large(error_call);
  }

  return dimension * times;
}

r_no_return void stop_rep_dimension_too_large(struct r_lazy error_call) {
  r_abort_lazy_call(
    error_call,
    "The dimension implied by `times` is too large for R."
  );
}

static void rray_rep_fill(
  r_obj* x,
  r_obj* out,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis,
  r_ssize block_size,
  int times
) {
  switch (r_typeof(x)) {
  case R_TYPE_logical:
    rray_rep_fill_lgl(
      x,
      out,
      v_x_dimensions,
      v_out_dimensions,
      dimensionality,
      axis,
      block_size,
      times
    );
    break;
  case R_TYPE_integer:
    rray_rep_fill_int(
      x,
      out,
      v_x_dimensions,
      v_out_dimensions,
      dimensionality,
      axis,
      block_size,
      times
    );
    break;
  case R_TYPE_double:
    rray_rep_fill_dbl(
      x,
      out,
      v_x_dimensions,
      v_out_dimensions,
      dimensionality,
      axis,
      block_size,
      times
    );
    break;
  case R_TYPE_complex:
    rray_rep_fill_cpl(
      x,
      out,
      v_x_dimensions,
      v_out_dimensions,
      dimensionality,
      axis,
      block_size,
      times
    );
    break;
  case R_TYPE_raw:
    rray_rep_fill_raw(
      x,
      out,
      v_x_dimensions,
      v_out_dimensions,
      dimensionality,
      axis,
      block_size,
      times
    );
    break;
  case R_TYPE_character:
    rray_rep_fill_chr(
      x,
      out,
      v_x_dimensions,
      v_out_dimensions,
      dimensionality,
      axis,
      block_size,
      times
    );
    break;
  case R_TYPE_list:
    rray_rep_fill_list(
      x,
      out,
      v_x_dimensions,
      v_out_dimensions,
      dimensionality,
      axis,
      block_size,
      times
    );
    break;
  default:
    r_stop_unreachable();
  }
}

#define RRAY_REP_FILL_LOOP(CTYPE, POKE)                                        \
  const r_ssize out_size = r_length(out);                                      \
                                                                               \
  int v_point[RRAY_MAX_DIMENSIONALITY] = {0};                                  \
                                                                               \
  r_ssize out_i = 0;                                                           \
                                                                               \
  while (out_i != out_size) {                                                  \
    const r_ssize block =                                                      \
      rray_rep_x_block(v_point, v_x_dimensions, dimensionality, axis);         \
    CTYPE const* v_x_block = v_x + block * block_size;                         \
                                                                               \
    for (int time = 0; time < times; ++time) {                                 \
      for (r_ssize i = 0; i < block_size; ++i) {                               \
        POKE(out, out_i, v_x_block[i]);                                        \
        ++out_i;                                                               \
      }                                                                        \
    }                                                                          \
                                                                               \
    for (int i = axis; i < dimensionality; ++i) {                              \
      ++v_point[i];                                                            \
      if (v_point[i] < v_out_dimensions[i]) {                                  \
        break;                                                                 \
      }                                                                        \
      v_point[i] = 0;                                                          \
    }                                                                          \
  }

#define RRAY_REP_FILL_ATOMIC_POKE(OUT, I, VALUE) v_out[I] = (VALUE)

#define RRAY_REP_FILL_ATOMIC(CTYPE, CONST_DEREF, DEREF)                        \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
  CTYPE* v_out = DEREF(out);                                                   \
                                                                               \
  RRAY_REP_FILL_LOOP(CTYPE, RRAY_REP_FILL_ATOMIC_POKE);

#define RRAY_REP_FILL_BARRIER(CONST_DEREF, POKE)                               \
  r_obj* const* v_x = CONST_DEREF(x);                                          \
                                                                               \
  RRAY_REP_FILL_LOOP(r_obj*, POKE);

static void rray_rep_fill_lgl(
  r_obj* x,
  r_obj* out,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis,
  r_ssize block_size,
  int times
) {
  RRAY_REP_FILL_ATOMIC(int, r_lgl_cbegin, r_lgl_begin);
}

static void rray_rep_fill_int(
  r_obj* x,
  r_obj* out,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis,
  r_ssize block_size,
  int times
) {
  RRAY_REP_FILL_ATOMIC(int, r_int_cbegin, r_int_begin);
}

static void rray_rep_fill_dbl(
  r_obj* x,
  r_obj* out,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis,
  r_ssize block_size,
  int times
) {
  RRAY_REP_FILL_ATOMIC(double, r_dbl_cbegin, r_dbl_begin);
}

static void rray_rep_fill_cpl(
  r_obj* x,
  r_obj* out,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis,
  r_ssize block_size,
  int times
) {
  RRAY_REP_FILL_ATOMIC(r_complex, r_cpl_cbegin, r_cpl_begin);
}

static void rray_rep_fill_raw(
  r_obj* x,
  r_obj* out,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis,
  r_ssize block_size,
  int times
) {
  RRAY_REP_FILL_ATOMIC(Rbyte, r_raw_cbegin, r_raw_begin);
}

static void rray_rep_fill_chr(
  r_obj* x,
  r_obj* out,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis,
  r_ssize block_size,
  int times
) {
  RRAY_REP_FILL_BARRIER(r_chr_cbegin, r_chr_poke);
}

static void rray_rep_fill_list(
  r_obj* x,
  r_obj* out,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis,
  r_ssize block_size,
  int times
) {
  RRAY_REP_FILL_BARRIER(r_list_cbegin, r_list_poke);
}

#undef RRAY_REP_FILL_LOOP
#undef RRAY_REP_FILL_ATOMIC_POKE
#undef RRAY_REP_FILL_ATOMIC
#undef RRAY_REP_FILL_BARRIER

static inline r_ssize rray_rep_x_block(
  const int* v_point,
  const int* v_x_dimensions,
  int dimensionality,
  int axis
) {
  r_ssize out = 0;

  for (int i = dimensionality - 1; i >= axis; --i) {
    const int x_dimension = v_x_dimensions[i];
    out = out * x_dimension + v_point[i] % x_dimension;
  }

  return out;
}

static r_obj* rray_rep_names(
  r_obj* names,
  const int* v_axes,
  r_ssize axes_size,
  const int* v_times,
  r_ssize times_size
) {
  if (names == r_null) {
    return r_null;
  }

  const r_ssize names_size = r_length(names);
  r_obj* const* v_names = r_list_cbegin(names);

  r_obj* out = KEEP(r_alloc_list(names_size));

  for (r_ssize i = 0; i < names_size; ++i) {
    r_list_poke(out, i, v_names[i]);
  }

  for (r_ssize i = 0; i < axes_size; ++i) {
    const int axis = v_axes[i];
    r_obj* axis_names = v_names[axis - 1];

    if (axis_names == r_null) {
      continue;
    }

    const int times = v_times[times_size == 1 ? 0 : i];
    r_list_poke(out, axis - 1, rray_rep_axis_names(axis_names, times));
  }

  FREE(1);
  return out;
}

static r_obj* rray_rep_axis_names(r_obj* axis_names, int times) {
  const r_ssize axis_dimension = r_length(axis_names);
  r_obj* const* v_axis_names = r_chr_cbegin(axis_names);

  r_obj* out = KEEP(r_alloc_character(axis_dimension * times));

  r_ssize out_i = 0;

  for (int time = 0; time < times; ++time) {
    for (r_ssize i = 0; i < axis_dimension; ++i) {
      r_chr_poke(out, out_i, v_axis_names[i]);
      ++out_i;
    }
  }

  FREE(1);
  return out;
}
