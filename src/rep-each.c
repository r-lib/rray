#include "rep-each.h"

#include <limits.h>

#include "axes.h"
#include "dimensionality.h"
#include "rep.h"
#include "size.h"
#include "utils.h"

#include "decl/rep-each-decl.h"

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

  times = KEEP(
    arg_as_rep_each_times(times, axis_dimension, rray_args.times, error_call)
  );
  const int* v_times = r_int_cbegin(times);
  const r_ssize times_size = r_length(times);

  const int out_dimension =
    rray_rep_each_dimension(axis_dimension, v_times, times_size, error_call);

  r_obj* out_dimensions = KEEP(r_alloc_integer(dimensionality));
  int* v_out_dimensions = r_int_begin(out_dimensions);
  r_memcpy(v_out_dimensions, v_x_dimensions, sizeof(int) * dimensionality);
  v_out_dimensions[axis - 1] = out_dimension;

  const r_ssize out_size = rray_size_from_dimensions_checked(
    v_out_dimensions,
    dimensionality,
    error_call
  );

  r_obj* out = KEEP(r_alloc_vector(r_typeof(x), out_size));
  r_attrib_poke_dim(out, out_dimensions);

  r_ssize block_size = 1;
  for (int i = 0; i < axis - 1; ++i) {
    block_size *= v_x_dimensions[i];
  }

  rray_rep_each_fill(x, out, v_times, times_size, block_size, axis_dimension);

  r_obj* out_names = KEEP(rray_rep_each_names(
    r_dim_names(x),
    axis,
    out_dimension,
    v_times,
    times_size
  ));
  if (out_names != r_null) {
    r_attrib_poke_dim_names(out, out_names);
  }

  FREE(5);
  return out;
}

static r_obj* arg_as_rep_each_times(
  r_obj* times,
  int axis_dimension,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  times = KEEP(arg_as_non_negative_bare_integer(times, arg, error_call));

  const r_ssize times_size = r_length(times);

  if (times_size != 1 && times_size != axis_dimension) {
    stop_rep_each_times_size(times_size, axis_dimension, arg, error_call);
  }

  FREE(1);
  return times;
}

static r_no_return void stop_rep_each_times_size(
  r_ssize times_size,
  int axis_dimension,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  if (axis_dimension == 1) {
    r_abort_lazy_call(
      error_call,
      "%s must be size 1, not size %" R_PRI_SSIZE ".",
      rray_arg_format(arg),
      times_size
    );
  } else {
    r_abort_lazy_call(
      error_call,
      "%s must be size 1 or the `axis` dimension of %d, "
      "not size %" R_PRI_SSIZE ".",
      rray_arg_format(arg),
      axis_dimension,
      times_size
    );
  }
}

static int rray_rep_each_dimension(
  int axis_dimension,
  const int* v_times,
  r_ssize times_size,
  struct r_lazy error_call
) {
  int out = 0;

  for (int i = 0; i < axis_dimension; ++i) {
    const int times = v_times[times_size == 1 ? 0 : i];

    if (out > INT_MAX - times) {
      stop_rep_dimension_too_large(error_call);
    }

    out += times;
  }

  return out;
}

static void rray_rep_each_fill(
  r_obj* x,
  r_obj* out,
  const int* v_times,
  r_ssize times_size,
  r_ssize block_size,
  int axis_dimension
) {
  switch (r_typeof(x)) {
  case R_TYPE_logical:
    rray_rep_each_fill_lgl(
      x,
      out,
      v_times,
      times_size,
      block_size,
      axis_dimension
    );
    break;
  case R_TYPE_integer:
    rray_rep_each_fill_int(
      x,
      out,
      v_times,
      times_size,
      block_size,
      axis_dimension
    );
    break;
  case R_TYPE_double:
    rray_rep_each_fill_dbl(
      x,
      out,
      v_times,
      times_size,
      block_size,
      axis_dimension
    );
    break;
  case R_TYPE_complex:
    rray_rep_each_fill_cpl(
      x,
      out,
      v_times,
      times_size,
      block_size,
      axis_dimension
    );
    break;
  case R_TYPE_raw:
    rray_rep_each_fill_raw(
      x,
      out,
      v_times,
      times_size,
      block_size,
      axis_dimension
    );
    break;
  case R_TYPE_character:
    rray_rep_each_fill_chr(
      x,
      out,
      v_times,
      times_size,
      block_size,
      axis_dimension
    );
    break;
  case R_TYPE_list:
    rray_rep_each_fill_list(
      x,
      out,
      v_times,
      times_size,
      block_size,
      axis_dimension
    );
    break;
  default:
    r_stop_unreachable();
  }
}

#define RRAY_REP_EACH_FILL_LOOP(CTYPE, POKE)                                   \
  const r_ssize out_size = r_length(out);                                      \
                                                                               \
  CTYPE const* v_x_block = v_x;                                                \
                                                                               \
  r_ssize out_i = 0;                                                           \
                                                                               \
  while (out_i != out_size) {                                                  \
    for (int j = 0; j < axis_dimension; ++j) {                                 \
      const int times = v_times[times_size == 1 ? 0 : j];                      \
                                                                               \
      for (int time = 0; time < times; ++time) {                               \
        for (r_ssize i = 0; i < block_size; ++i) {                             \
          POKE(out, out_i, v_x_block[i]);                                      \
          ++out_i;                                                             \
        }                                                                      \
      }                                                                        \
                                                                               \
      v_x_block += block_size;                                                 \
    }                                                                          \
  }

#define RRAY_REP_EACH_FILL_ATOMIC_POKE(OUT, I, VALUE) v_out[I] = (VALUE)

#define RRAY_REP_EACH_FILL_ATOMIC(CTYPE, CONST_DEREF, DEREF)                   \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
  CTYPE* v_out = DEREF(out);                                                   \
                                                                               \
  RRAY_REP_EACH_FILL_LOOP(CTYPE, RRAY_REP_EACH_FILL_ATOMIC_POKE);

#define RRAY_REP_EACH_FILL_BARRIER(CONST_DEREF, POKE)                          \
  r_obj* const* v_x = CONST_DEREF(x);                                          \
                                                                               \
  RRAY_REP_EACH_FILL_LOOP(r_obj*, POKE);

static void rray_rep_each_fill_lgl(
  r_obj* x,
  r_obj* out,
  const int* v_times,
  r_ssize times_size,
  r_ssize block_size,
  int axis_dimension
) {
  RRAY_REP_EACH_FILL_ATOMIC(int, r_lgl_cbegin, r_lgl_begin);
}

static void rray_rep_each_fill_int(
  r_obj* x,
  r_obj* out,
  const int* v_times,
  r_ssize times_size,
  r_ssize block_size,
  int axis_dimension
) {
  RRAY_REP_EACH_FILL_ATOMIC(int, r_int_cbegin, r_int_begin);
}

static void rray_rep_each_fill_dbl(
  r_obj* x,
  r_obj* out,
  const int* v_times,
  r_ssize times_size,
  r_ssize block_size,
  int axis_dimension
) {
  RRAY_REP_EACH_FILL_ATOMIC(double, r_dbl_cbegin, r_dbl_begin);
}

static void rray_rep_each_fill_cpl(
  r_obj* x,
  r_obj* out,
  const int* v_times,
  r_ssize times_size,
  r_ssize block_size,
  int axis_dimension
) {
  RRAY_REP_EACH_FILL_ATOMIC(r_complex, r_cpl_cbegin, r_cpl_begin);
}

static void rray_rep_each_fill_raw(
  r_obj* x,
  r_obj* out,
  const int* v_times,
  r_ssize times_size,
  r_ssize block_size,
  int axis_dimension
) {
  RRAY_REP_EACH_FILL_ATOMIC(Rbyte, r_raw_cbegin, r_raw_begin);
}

static void rray_rep_each_fill_chr(
  r_obj* x,
  r_obj* out,
  const int* v_times,
  r_ssize times_size,
  r_ssize block_size,
  int axis_dimension
) {
  RRAY_REP_EACH_FILL_BARRIER(r_chr_cbegin, r_chr_poke);
}

static void rray_rep_each_fill_list(
  r_obj* x,
  r_obj* out,
  const int* v_times,
  r_ssize times_size,
  r_ssize block_size,
  int axis_dimension
) {
  RRAY_REP_EACH_FILL_BARRIER(r_list_cbegin, r_list_poke);
}

#undef RRAY_REP_EACH_FILL_LOOP
#undef RRAY_REP_EACH_FILL_ATOMIC_POKE
#undef RRAY_REP_EACH_FILL_ATOMIC
#undef RRAY_REP_EACH_FILL_BARRIER

static r_obj* rray_rep_each_names(
  r_obj* names,
  int axis,
  int out_dimension,
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

  r_obj* axis_names = v_names[axis - 1];

  if (axis_names != r_null) {
    r_list_poke(
      out,
      axis - 1,
      rray_rep_each_axis_names(axis_names, out_dimension, v_times, times_size)
    );
  }

  FREE(1);
  return out;
}

static r_obj* rray_rep_each_axis_names(
  r_obj* axis_names,
  int out_dimension,
  const int* v_times,
  r_ssize times_size
) {
  const r_ssize axis_dimension = r_length(axis_names);
  r_obj* const* v_axis_names = r_chr_cbegin(axis_names);

  r_obj* out = KEEP(r_alloc_character(out_dimension));

  r_ssize out_i = 0;

  for (r_ssize i = 0; i < axis_dimension; ++i) {
    const int times = v_times[times_size == 1 ? 0 : i];

    for (int time = 0; time < times; ++time) {
      r_chr_poke(out, out_i, v_axis_names[i]);
      ++out_i;
    }
  }

  FREE(1);
  return out;
}
