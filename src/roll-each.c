#include "roll-each.h"

#include "axes.h"
#include "broadcast.h"
#include "cast.h"
#include "dimensionality.h"
#include "reduce-names.h"
#include "roll.h"
#include "size.h"
#include "utils.h"

#include "decl/roll-each-decl.h"

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

  r_obj* lane_dimensions = KEEP(r_clone(x_dimensions));
  int* v_lane_dimensions = r_int_begin(lane_dimensions);
  v_lane_dimensions[axis - 1] = 1;

  check_broadcastable(
    v_n_dimensions,
    n_dimensionality,
    v_lane_dimensions,
    dimensionality,
    rray_args.n,
    error_call
  );

  const r_ssize size =
    rray_size_from_dimensions(v_x_dimensions, dimensionality);

  r_obj* out = KEEP(r_alloc_vector(r_typeof(x), size));
  r_attrib_poke_dim(out, x_dimensions);

  if (size != 0) {
    n = KEEP(rray_roll_each_normalize(n, axis_dimension));
    n = KEEP(rray_broadcast(n, lane_dimensions, rray_args.n, error_call));
    const int* v_n = r_int_cbegin(n);

    r_ssize block_size = 1;
    for (int i = 0; i < axis - 1; ++i) {
      block_size *= v_x_dimensions[i];
    }

    r_ssize n_groups = 1;
    for (int i = axis; i < dimensionality; ++i) {
      n_groups *= v_x_dimensions[i];
    }

    rray_roll_each_fill(x, out, v_n, block_size, axis_dimension, n_groups);

    FREE(2);
  }

  // Using `rray_reduce_names()` is an easy way to clear the `axis` names, which
  // likely no longer make sense
  r_obj* axes = KEEP(r_int(axis));
  r_obj* out_names = KEEP(rray_reduce_names(x, axes));

  if (out_names != r_null) {
    r_attrib_poke_dim_names(out, out_names);
  }

  FREE(6);
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

static r_obj* rray_roll_each_normalize(r_obj* n, int axis_dimension) {
  const r_ssize size = r_length(n);
  const int* v_n = r_int_cbegin(n);

  if (rray_roll_each_is_normalized(v_n, size, axis_dimension)) {
    return n;
  }

  r_obj* out = KEEP(r_alloc_integer(size));
  int* v_out = r_int_begin(out);

  for (r_ssize i = 0; i < size; ++i) {
    v_out[i] = rray_roll_normalize(v_n[i], axis_dimension);
  }

  r_attrib_poke_dim(out, r_dim(n));

  FREE(1);
  return out;
}

static bool rray_roll_each_is_normalized(
  const int* v_n,
  r_ssize size,
  int axis_dimension
) {
  for (r_ssize i = 0; i < size; ++i) {
    if (rray_roll_normalize(v_n[i], axis_dimension) != v_n[i]) {
      return false;
    }
  }

  return true;
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

#define RRAY_ROLL_EACH_FILL_LOOP(CTYPE, POKE)                                  \
  const r_ssize group_size = block_size * axis_dimension;                      \
                                                                               \
  r_ssize out_i = 0;                                                           \
                                                                               \
  for (r_ssize group = 0; group < n_groups; ++group) {                         \
    CTYPE const* v_x_group = v_x + group * group_size;                         \
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

#define RRAY_ROLL_EACH_FILL_ATOMIC_POKE(OUT, I, VALUE) v_out[I] = (VALUE)

#define RRAY_ROLL_EACH_FILL_ATOMIC(CTYPE, CONST_DEREF, DEREF)                  \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
  CTYPE* v_out = DEREF(out);                                                   \
                                                                               \
  RRAY_ROLL_EACH_FILL_LOOP(CTYPE, RRAY_ROLL_EACH_FILL_ATOMIC_POKE);

#define RRAY_ROLL_EACH_FILL_BARRIER(CONST_DEREF, POKE)                         \
  r_obj* const* v_x = CONST_DEREF(x);                                          \
                                                                               \
  RRAY_ROLL_EACH_FILL_LOOP(r_obj*, POKE);

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

#undef RRAY_ROLL_EACH_FILL_LOOP
#undef RRAY_ROLL_EACH_FILL_ATOMIC_POKE
#undef RRAY_ROLL_EACH_FILL_ATOMIC
#undef RRAY_ROLL_EACH_FILL_BARRIER
