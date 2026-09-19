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
  r_obj* ffi_axis,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const int axis = arg_as_int(ffi_axis, rray_args.axis, error_call);
  return rray_rep(ffi_x, ffi_times, axis, rray_args.x, error_call);
}

r_obj* rray_rep(
  r_obj* x,
  r_obj* times,
  int axis,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_rep_impl(x, times, axis, false, arg, error_call);
}

r_obj* ffi_rray_rep_each(
  r_obj* ffi_x,
  r_obj* ffi_times,
  r_obj* ffi_axis,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const int axis = arg_as_int(ffi_axis, rray_args.axis, error_call);
  return rray_rep_each(ffi_x, ffi_times, axis, rray_args.x, error_call);
}

r_obj* rray_rep_each(
  r_obj* x,
  r_obj* times,
  int axis,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_rep_impl(x, times, axis, true, arg, error_call);
}

static r_obj* rray_rep_impl(
  r_obj* x,
  r_obj* times,
  int axis,
  bool each,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));

  r_obj* dimensions = r_dim(x);
  const int* v_dimensions = r_int_cbegin(dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(dimensions);
  check_dimensionality(dimensionality);

  check_axis(axis, dimensionality, rray_args.axis, error_call);
  const int dimension = v_dimensions[axis - 1];

  times = KEEP(
    arg_as_times(times, each ? dimension : 1, rray_args.times, error_call)
  );
  const int* v_times = r_int_cbegin(times);
  const r_ssize times_size = r_length(times);
  const r_ssize times_stride = (times_size == 1) ? 0 : 1;

  const int out_dimension =
    rray_rep_dimension(dimension, v_times, times_size, error_call);

  r_obj* out_dimensions = KEEP(r_alloc_integer(dimensionality));
  int* v_out_dimensions = r_int_begin(out_dimensions);
  r_memcpy(v_out_dimensions, v_dimensions, sizeof(int) * dimensionality);
  v_out_dimensions[axis - 1] = out_dimension;

  const r_ssize out_size = rray_size_from_dimensions_checked(
    v_out_dimensions,
    dimensionality,
    error_call
  );

  r_obj* out = KEEP(r_alloc_vector(r_typeof(x), out_size));
  r_attrib_poke_dim(out, out_dimensions);

  r_ssize inner = 1;
  for (int i = 0; i < axis - 1; ++i) {
    inner *= v_dimensions[i];
  }

  r_ssize outer = 1;
  for (int i = axis; i < dimensionality; ++i) {
    outer *= v_dimensions[i];
  }

  if (each) {
    rray_rep_fill(x, out, inner, dimension, outer, v_times, times_stride);
  } else {
    rray_rep_fill(x, out, inner * dimension, 1, outer, v_times, times_stride);
  }

  r_obj* out_names = KEEP(rray_rep_names(
    r_dim_names(x),
    axis,
    out_dimension,
    each,
    v_times,
    times_stride
  ));
  if (out_names != r_null) {
    r_attrib_poke_dim_names(out, out_names);
  }

  FREE(5);
  return out;
}

static r_obj* arg_as_times(
  r_obj* times,
  r_ssize size,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  times = KEEP(arg_as_non_negative_bare_integer(times, arg, error_call));

  const r_ssize times_size = r_length(times);

  if (times_size != 1 && times_size != size) {
    stop_times_size(times_size, size, arg, error_call);
  }

  FREE(1);
  return times;
}

static r_no_return void stop_times_size(
  r_ssize times_size,
  r_ssize size,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  if (size == 1) {
    r_abort_lazy_call(
      error_call,
      "%s must be size 1, not size %" R_PRI_SSIZE ".",
      rray_arg_format(arg),
      times_size
    );
  } else {
    r_abort_lazy_call(
      error_call,
      "%s must be size 1 or the `axis` dimension of %" R_PRI_SSIZE ", "
      "not size %" R_PRI_SSIZE ".",
      rray_arg_format(arg),
      size,
      times_size
    );
  }
}

static int rray_rep_dimension(
  int dimension,
  const int* v_times,
  r_ssize times_size,
  struct r_lazy error_call
) {
  if (times_size == 1) {
    // Single `times`. Multiply by `axis`'s `dimension` regardless of
    // rep / rep-each.
    const int times = v_times[0];

    if (times != 0 && dimension > INT_MAX / times) {
      stop_dimension_too_large(error_call);
    }

    return dimension * times;
  } else {
    // Vector `times`. Must be rep-each. Sum them to get the output dimension.
    int out = 0;

    for (int i = 0; i < dimension; ++i) {
      const int times = v_times[i];

      if (out > INT_MAX - times) {
        stop_dimension_too_large(error_call);
      }

      out += times;
    }

    return out;
  }
}

static r_no_return void stop_dimension_too_large(struct r_lazy error_call) {
  r_abort_lazy_call(
    error_call,
    "The dimension implied by `times` is too large for R."
  );
}

static void rray_rep_fill(
  r_obj* x,
  r_obj* out,
  r_ssize inner,
  r_ssize middle,
  r_ssize outer,
  const int* v_times,
  r_ssize times_stride
) {
  switch (r_typeof(x)) {
  case R_TYPE_logical:
    rray_rep_fill_lgl(x, out, inner, middle, outer, v_times, times_stride);
    break;
  case R_TYPE_integer:
    rray_rep_fill_int(x, out, inner, middle, outer, v_times, times_stride);
    break;
  case R_TYPE_double:
    rray_rep_fill_dbl(x, out, inner, middle, outer, v_times, times_stride);
    break;
  case R_TYPE_complex:
    rray_rep_fill_cpl(x, out, inner, middle, outer, v_times, times_stride);
    break;
  case R_TYPE_raw:
    rray_rep_fill_raw(x, out, inner, middle, outer, v_times, times_stride);
    break;
  case R_TYPE_character:
    rray_rep_fill_chr(x, out, inner, middle, outer, v_times, times_stride);
    break;
  case R_TYPE_list:
    rray_rep_fill_list(x, out, inner, middle, outer, v_times, times_stride);
    break;
  default:
    r_stop_unreachable();
  }
}

#define RRAY_REP_FILL_ATOMIC(CTYPE, CONST_DEREF, DEREF)                        \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
  CTYPE* v_out = DEREF(out);                                                   \
                                                                               \
  r_ssize out_i = 0;                                                           \
                                                                               \
  for (r_ssize o = 0; o < outer; ++o) {                                        \
    for (r_ssize m = 0; m < middle; ++m) {                                     \
      const CTYPE* v_block = v_x + (o * middle + m) * inner;                   \
      const int times = v_times[m * times_stride];                             \
                                                                               \
      for (int j = 0; j < times; ++j) {                                        \
        r_memcpy(v_out + out_i, v_block, sizeof(CTYPE) * (size_t) inner);      \
        out_i += inner;                                                        \
      }                                                                        \
    }                                                                          \
  }

#define RRAY_REP_FILL_BARRIER(CONST_DEREF, POKE)                               \
  r_obj* const* v_x = CONST_DEREF(x);                                          \
                                                                               \
  r_ssize out_i = 0;                                                           \
                                                                               \
  for (r_ssize o = 0; o < outer; ++o) {                                        \
    for (r_ssize m = 0; m < middle; ++m) {                                     \
      const r_ssize x_start = (o * middle + m) * inner;                        \
      const int times = v_times[m * times_stride];                             \
                                                                               \
      for (int j = 0; j < times; ++j) {                                        \
        for (r_ssize i = 0; i < inner; ++i) {                                  \
          POKE(out, out_i, v_x[x_start + i]);                                  \
          ++out_i;                                                             \
        }                                                                      \
      }                                                                        \
    }                                                                          \
  }

static void rray_rep_fill_lgl(
  r_obj* x,
  r_obj* out,
  r_ssize inner,
  r_ssize middle,
  r_ssize outer,
  const int* v_times,
  r_ssize times_stride
) {
  RRAY_REP_FILL_ATOMIC(int, r_lgl_cbegin, r_lgl_begin);
}

static void rray_rep_fill_int(
  r_obj* x,
  r_obj* out,
  r_ssize inner,
  r_ssize middle,
  r_ssize outer,
  const int* v_times,
  r_ssize times_stride
) {
  RRAY_REP_FILL_ATOMIC(int, r_int_cbegin, r_int_begin);
}

static void rray_rep_fill_dbl(
  r_obj* x,
  r_obj* out,
  r_ssize inner,
  r_ssize middle,
  r_ssize outer,
  const int* v_times,
  r_ssize times_stride
) {
  RRAY_REP_FILL_ATOMIC(double, r_dbl_cbegin, r_dbl_begin);
}

static void rray_rep_fill_cpl(
  r_obj* x,
  r_obj* out,
  r_ssize inner,
  r_ssize middle,
  r_ssize outer,
  const int* v_times,
  r_ssize times_stride
) {
  RRAY_REP_FILL_ATOMIC(r_complex, r_cpl_cbegin, r_cpl_begin);
}

static void rray_rep_fill_raw(
  r_obj* x,
  r_obj* out,
  r_ssize inner,
  r_ssize middle,
  r_ssize outer,
  const int* v_times,
  r_ssize times_stride
) {
  RRAY_REP_FILL_ATOMIC(Rbyte, r_raw_cbegin, r_raw_begin);
}

static void rray_rep_fill_chr(
  r_obj* x,
  r_obj* out,
  r_ssize inner,
  r_ssize middle,
  r_ssize outer,
  const int* v_times,
  r_ssize times_stride
) {
  RRAY_REP_FILL_BARRIER(r_chr_cbegin, r_chr_poke);
}

static void rray_rep_fill_list(
  r_obj* x,
  r_obj* out,
  r_ssize inner,
  r_ssize middle,
  r_ssize outer,
  const int* v_times,
  r_ssize times_stride
) {
  RRAY_REP_FILL_BARRIER(r_list_cbegin, r_list_poke);
}

#undef RRAY_REP_FILL_ATOMIC
#undef RRAY_REP_FILL_BARRIER

static r_obj* rray_rep_names(
  r_obj* names,
  int axis,
  r_ssize out_dimension,
  bool each,
  const int* v_times,
  r_ssize times_stride
) {
  if (names == r_null) {
    return r_null;
  }

  r_obj* axis_names = r_list_get(names, axis - 1);

  if (axis_names == r_null) {
    return names;
  }

  const r_ssize names_size = r_length(names);
  r_obj* const* v_names = r_list_cbegin(names);

  r_obj* out = KEEP(r_alloc_list(names_size));

  for (r_ssize i = 0; i < names_size; ++i) {
    r_list_poke(out, i, v_names[i]);
  }

  r_obj* out_axis_names = KEEP(
    rray_rep_axis_names(axis_names, out_dimension, each, v_times, times_stride)
  );
  r_list_poke(out, axis - 1, out_axis_names);

  FREE(2);
  return out;
}

static r_obj* rray_rep_axis_names(
  r_obj* axis_names,
  r_ssize out_dimension,
  bool each,
  const int* v_times,
  r_ssize times_stride
) {
  const r_ssize dimension = r_length(axis_names);

  r_obj* out = KEEP(r_alloc_character(out_dimension));
  r_obj* const* v_axis_names = r_chr_cbegin(axis_names);

  r_ssize out_i = 0;

  if (each) {
    for (r_ssize i = 0; i < dimension; ++i) {
      const int times = v_times[i * times_stride];

      for (int j = 0; j < times; ++j) {
        r_chr_poke(out, out_i, v_axis_names[i]);
        ++out_i;
      }
    }
  } else {
    const int times = v_times[0];

    for (int j = 0; j < times; ++j) {
      for (r_ssize i = 0; i < dimension; ++i) {
        r_chr_poke(out, out_i, v_axis_names[i]);
        ++out_i;
      }
    }
  }

  FREE(1);
  return out;
}
