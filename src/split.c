#include "split.h"

#include "axes.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "strided-iterator2.h"
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
  check_dimensionality(dimensionality);

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

  // i.e. each array is the same dimension and splits `axis` evenly
  const bool uniform = r_length(dimensions) == 1;

  const r_ssize out_size =
    uniform ? axis_dimension / v_dimensions[0] : r_length(dimensions);

  r_obj* out = KEEP(r_alloc_list(out_size));

  r_obj* x_names = r_dim_names(x);
  r_obj* const* v_x_names = (x_names == r_null) ? NULL : r_list_cbegin(x_names);

  r_obj* x_axis_names = (v_x_names == NULL) ? r_null : v_x_names[axis - 1];
  r_obj* const* v_x_axis_names =
    (x_axis_names == r_null) ? NULL : r_chr_cbegin(x_axis_names);

  r_ssize v_x_strides[RRAY_MAX_DIMENSIONALITY];
  rray_fill_strides_from_dimensions(
    v_x_dimensions,
    dimensionality,
    v_x_strides
  );
  const r_ssize x_axis_stride = v_x_strides[axis - 1];

  // Initialized on the first iteration. Changes any time the output dimension
  // along `axis` changes, but in the uniform case all arrays share the same
  // dimensions object!
  r_obj* out_elt_dimensions = r_null;
  r_keep_loc out_elt_dimensions_loc;
  KEEP_HERE(out_elt_dimensions, &out_elt_dimensions_loc);

  r_ssize group_end = 0;

  // The location along the axis. For example, with a 2x10 array split along
  // columns this runs from 0-9. The `x_axis_stride` maps it to `x_start`, a
  // flat location into `x` itself.
  int x_axis_start = 0;

  for (r_ssize i = 0; i < out_size; ++i) {
    const int dimension = uniform ? v_dimensions[0] : v_dimensions[i];

    if (i == group_end) {
      group_end = i + 1;

      while (group_end < out_size &&
             (uniform || v_dimensions[group_end] == dimension)) {
        ++group_end;
      }

      out_elt_dimensions = r_alloc_integer(dimensionality);
      KEEP_AT(out_elt_dimensions, out_elt_dimensions_loc);
      int* v_out_elt_dimensions = r_int_begin(out_elt_dimensions);
      r_memcpy(
        v_out_elt_dimensions,
        v_x_dimensions,
        sizeof(int) * dimensionality
      );
      v_out_elt_dimensions[axis - 1] = dimension;

      const r_ssize x_start = x_axis_start * x_axis_stride;
      const r_ssize x_step = dimension * x_axis_stride;

      rray_split_fill(
        x,
        out,
        i,
        group_end,
        x_start,
        x_step,
        out_elt_dimensions,
        dimensionality,
        v_x_strides
      );
    }

    r_obj* out_elt = r_list_get(out, i);

    if (v_x_axis_names != NULL) {
      r_obj* out_elt_names = KEEP(rray_split_elt_names(
        v_x_names,
        dimensionality,
        axis,
        v_x_axis_names,
        x_axis_start,
        dimension
      ));
      r_attrib_poke_dim_names(out_elt, out_elt_names);
      FREE(1);
    } else if (x_names != r_null) {
      r_attrib_poke_dim_names(out_elt, x_names);
    }

    x_axis_start += dimension;
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

static void rray_split_fill(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  r_ssize out_end,
  r_ssize x_start,
  r_ssize x_step,
  r_obj* out_elt_dimensions,
  int dimensionality,
  const r_ssize* v_x_strides
) {
  switch (r_typeof(x)) {
  case R_TYPE_logical:
    rray_split_fill_lgl(
      x,
      out,
      out_start,
      out_end,
      x_start,
      x_step,
      out_elt_dimensions,
      dimensionality,
      v_x_strides
    );
    break;
  case R_TYPE_integer:
    rray_split_fill_int(
      x,
      out,
      out_start,
      out_end,
      x_start,
      x_step,
      out_elt_dimensions,
      dimensionality,
      v_x_strides
    );
    break;
  case R_TYPE_double:
    rray_split_fill_dbl(
      x,
      out,
      out_start,
      out_end,
      x_start,
      x_step,
      out_elt_dimensions,
      dimensionality,
      v_x_strides
    );
    break;
  case R_TYPE_complex:
    rray_split_fill_cpl(
      x,
      out,
      out_start,
      out_end,
      x_start,
      x_step,
      out_elt_dimensions,
      dimensionality,
      v_x_strides
    );
    break;
  case R_TYPE_raw:
    rray_split_fill_raw(
      x,
      out,
      out_start,
      out_end,
      x_start,
      x_step,
      out_elt_dimensions,
      dimensionality,
      v_x_strides
    );
    break;
  case R_TYPE_character:
    rray_split_fill_chr(
      x,
      out,
      out_start,
      out_end,
      x_start,
      x_step,
      out_elt_dimensions,
      dimensionality,
      v_x_strides
    );
    break;
  case R_TYPE_list:
    rray_split_fill_list(
      x,
      out,
      out_start,
      out_end,
      x_start,
      x_step,
      out_elt_dimensions,
      dimensionality,
      v_x_strides
    );
    break;
  default:
    r_stop_unreachable();
  }
}

#define RRAY_SPLIT_FILL_LOOP(POKE)                                             \
  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {         \
    const r_ssize start = rray_run_iterator_start(&it);                        \
    const r_ssize end = rray_run_iterator_end(&it);                            \
                                                                               \
    r_ssize x_loc = x_start + rray_run_iterator_loc(&it, 0);                   \
    const r_ssize x_stride = rray_run_iterator_stride(&it, 0);                 \
                                                                               \
    for (r_ssize j = start; j < end; ++j) {                                    \
      POKE(out_elt, j, v_x[x_loc]);                                            \
      x_loc += x_stride;                                                       \
    }                                                                          \
  }

#define RRAY_SPLIT_FILL_ATOMIC_POKE(OUT, I, VALUE) v_out_elt[I] = (VALUE)

#define RRAY_SPLIT_FILL_ATOMIC(RTYPE, CTYPE, CONST_DEREF, DEREF)               \
  const int* v_out_elt_dimensions = r_int_cbegin(out_elt_dimensions);          \
                                                                               \
  struct rray_run_iterator it =                                                \
    rray_run_iterator1(v_out_elt_dimensions, dimensionality, v_x_strides);     \
                                                                               \
  const r_ssize out_elt_size = rray_run_iterator_size(&it);                    \
                                                                               \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
                                                                               \
  for (r_ssize i = out_start; i < out_end; ++i) {                              \
    r_obj* out_elt = r_alloc_vector(RTYPE, out_elt_size);                      \
    r_list_poke(out, i, out_elt);                                              \
    r_attrib_poke_dim(out_elt, out_elt_dimensions);                            \
    CTYPE* v_out_elt = DEREF(out_elt);                                         \
                                                                               \
    RRAY_SPLIT_FILL_LOOP(RRAY_SPLIT_FILL_ATOMIC_POKE);                         \
                                                                               \
    rray_run_iterator_reset(&it);                                              \
    x_start += x_step;                                                         \
  }

#define RRAY_SPLIT_FILL_BARRIER(RTYPE, CONST_DEREF, POKE)                      \
  const int* v_out_elt_dimensions = r_int_cbegin(out_elt_dimensions);          \
                                                                               \
  struct rray_run_iterator it =                                                \
    rray_run_iterator1(v_out_elt_dimensions, dimensionality, v_x_strides);     \
                                                                               \
  const r_ssize out_elt_size = rray_run_iterator_size(&it);                    \
                                                                               \
  r_obj* const* v_x = CONST_DEREF(x);                                          \
                                                                               \
  for (r_ssize i = out_start; i < out_end; ++i) {                              \
    r_obj* out_elt = r_alloc_vector(RTYPE, out_elt_size);                      \
    r_list_poke(out, i, out_elt);                                              \
    r_attrib_poke_dim(out_elt, out_elt_dimensions);                            \
                                                                               \
    RRAY_SPLIT_FILL_LOOP(POKE);                                                \
                                                                               \
    rray_run_iterator_reset(&it);                                              \
    x_start += x_step;                                                         \
  }

static void rray_split_fill_lgl(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  r_ssize out_end,
  r_ssize x_start,
  r_ssize x_step,
  r_obj* out_elt_dimensions,
  int dimensionality,
  const r_ssize* v_x_strides
) {
  RRAY_SPLIT_FILL_ATOMIC(R_TYPE_logical, int, r_lgl_cbegin, r_lgl_begin);
}

static void rray_split_fill_int(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  r_ssize out_end,
  r_ssize x_start,
  r_ssize x_step,
  r_obj* out_elt_dimensions,
  int dimensionality,
  const r_ssize* v_x_strides
) {
  RRAY_SPLIT_FILL_ATOMIC(R_TYPE_integer, int, r_int_cbegin, r_int_begin);
}

static void rray_split_fill_dbl(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  r_ssize out_end,
  r_ssize x_start,
  r_ssize x_step,
  r_obj* out_elt_dimensions,
  int dimensionality,
  const r_ssize* v_x_strides
) {
  RRAY_SPLIT_FILL_ATOMIC(R_TYPE_double, double, r_dbl_cbegin, r_dbl_begin);
}

static void rray_split_fill_cpl(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  r_ssize out_end,
  r_ssize x_start,
  r_ssize x_step,
  r_obj* out_elt_dimensions,
  int dimensionality,
  const r_ssize* v_x_strides
) {
  RRAY_SPLIT_FILL_ATOMIC(R_TYPE_complex, r_complex, r_cpl_cbegin, r_cpl_begin);
}

static void rray_split_fill_raw(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  r_ssize out_end,
  r_ssize x_start,
  r_ssize x_step,
  r_obj* out_elt_dimensions,
  int dimensionality,
  const r_ssize* v_x_strides
) {
  RRAY_SPLIT_FILL_ATOMIC(R_TYPE_raw, Rbyte, r_raw_cbegin, r_raw_begin);
}

static void rray_split_fill_chr(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  r_ssize out_end,
  r_ssize x_start,
  r_ssize x_step,
  r_obj* out_elt_dimensions,
  int dimensionality,
  const r_ssize* v_x_strides
) {
  RRAY_SPLIT_FILL_BARRIER(R_TYPE_character, r_chr_cbegin, r_chr_poke);
}

static void rray_split_fill_list(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  r_ssize out_end,
  r_ssize x_start,
  r_ssize x_step,
  r_obj* out_elt_dimensions,
  int dimensionality,
  const r_ssize* v_x_strides
) {
  RRAY_SPLIT_FILL_BARRIER(R_TYPE_list, r_list_cbegin, r_list_poke);
}

#undef RRAY_SPLIT_FILL_LOOP
#undef RRAY_SPLIT_FILL_ATOMIC_POKE
#undef RRAY_SPLIT_FILL_ATOMIC
#undef RRAY_SPLIT_FILL_BARRIER

static r_obj* rray_split_elt_names(
  r_obj* const* v_x_names,
  int dimensionality,
  int axis,
  r_obj* const* v_x_axis_names,
  int x_axis_start,
  int dimension
) {
  r_obj* out = KEEP(r_alloc_list(dimensionality));

  for (int i = 0; i < dimensionality; ++i) {
    r_list_poke(out, i, v_x_names[i]);
  }

  r_obj* out_axis_names = r_alloc_character(dimension);
  r_list_poke(out, axis - 1, out_axis_names);

  for (int i = 0; i < dimension; ++i) {
    r_chr_poke(out_axis_names, i, v_x_axis_names[x_axis_start + i]);
  }

  FREE(1);
  return out;
}
