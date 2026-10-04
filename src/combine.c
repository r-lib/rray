#include "combine.h"

#include <limits.h>

#include "axes.h"
#include "broadcast-names.h"
#include "cast-common.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "ptype-common.h"
#include "size.h"
#include "strided-iterator.h"
#include "strides.h"
#include "utils.h"

#include "decl/combine-decl.h"

r_obj* ffi_rray_combine(r_obj* ffi_xs, r_obj* ffi_axis, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const int axis = arg_as_int(ffi_axis, rray_args.dot_axis, error_call);
  return rray_combine(
    ffi_xs,
    axis,
    r_null,
    rray_args.empty,
    rray_args.empty,
    error_call
  );
}

r_obj* rray_combine(
  r_obj* xs,
  int axis,
  r_obj* ptype,
  struct rray_arg* arg,
  struct rray_arg* ptype_arg,
  struct r_lazy error_call
) {
  if (r_length(xs) == 0) {
    r_abort_lazy_call(error_call, "Must supply at least one array to `...`.");
  }

  ptype = KEEP(rray_ptype_common(xs, ptype, arg, ptype_arg, error_call));

  xs = KEEP(rray_cast_common(xs, ptype, arg, ptype_arg, error_call));

  const r_ssize xs_size = r_length(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);

  const int out_dimensionality = list_max_dimensionality(xs, arg, error_call);
  check_dimensionality(out_dimensionality);

  check_axis(axis, out_dimensionality, rray_args.dot_axis, error_call);

  r_obj* out_dimensions =
    KEEP(rray_dimensions_common_opts(xs, &axis, 1, arg, error_call));
  int* v_out_dimensions = r_int_begin(out_dimensions);

  int axis_dimension = 0;

  for (r_ssize i = 0; i < xs_size; ++i) {
    r_obj* x = v_xs[i];
    r_obj* x_dimensions = r_dim(x);
    const int x_dimensionality =
      rray_dimensionality_from_dimensions(x_dimensions);
    const int x_axis_dimension =
      (axis <= x_dimensionality) ? r_int_get(x_dimensions, axis - 1) : 1;

    if (axis_dimension > INT_MAX - x_axis_dimension) {
      r_abort_lazy_call(
        error_call,
        "The combined dimension on `.axis` is too large."
      );
    }

    axis_dimension += x_axis_dimension;
  }

  v_out_dimensions[axis - 1] = axis_dimension;

  const r_ssize out_size = rray_size_from_dimensions_checked(
    v_out_dimensions,
    out_dimensionality,
    error_call
  );

  r_obj* out = KEEP(r_alloc_vector(r_typeof(ptype), out_size));

  r_ssize v_out_strides[RRAY_MAX_DIMENSIONALITY];
  rray_fill_strides_from_dimensions(
    v_out_dimensions,
    out_dimensionality,
    v_out_strides
  );

  int v_x_broadcast_dimensions[RRAY_MAX_DIMENSIONALITY];
  r_memcpy(
    v_x_broadcast_dimensions,
    v_out_dimensions,
    sizeof(int) * out_dimensionality
  );

  r_ssize v_x_strides[RRAY_MAX_DIMENSIONALITY];

  r_ssize axis_offset = 0;

  for (r_ssize i = 0; i < xs_size; ++i) {
    r_obj* x = v_xs[i];
    r_obj* x_dimensions = r_dim(x);
    const int* v_x_dimensions = r_int_cbegin(x_dimensions);
    const int x_dimensionality =
      rray_dimensionality_from_dimensions(x_dimensions);
    const int x_axis_dimension =
      (axis <= x_dimensionality) ? v_x_dimensions[axis - 1] : 1;

    v_x_broadcast_dimensions[axis - 1] = x_axis_dimension;

    rray_fill_broadcast_strides_from_dimensions(
      v_x_dimensions,
      x_dimensionality,
      out_dimensionality,
      v_x_strides
    );

    const r_ssize out_start = axis_offset * v_out_strides[axis - 1];

    rray_combine_fill(
      x,
      out,
      out_start,
      v_x_broadcast_dimensions,
      out_dimensionality,
      v_out_strides,
      v_x_strides
    );

    axis_offset += x_axis_dimension;
  }

  r_attrib_poke_dim(out, out_dimensions);

  r_obj* out_names =
    KEEP(rray_combine_names(xs, out_dimensions, axis, axis_dimension));

  if (out_names != r_null) {
    r_attrib_poke_dim_names(out, out_names);
  }

  FREE(5);
  return out;
}

static r_obj* rray_combine_names(
  r_obj* xs,
  r_obj* dimensions,
  int axis,
  int axis_dimension
) {
  r_obj* out = rray_broadcast_names_common_opts(xs, dimensions, &axis, 1);
  r_keep_loc out_loc;
  KEEP_HERE(out, &out_loc);

  r_obj* axis_names = KEEP(rray_combine_axis_names(xs, axis, axis_dimension));

  if (axis_names != r_null) {
    if (out == r_null) {
      out = r_alloc_list(rray_dimensionality_from_dimensions(dimensions));
      KEEP_AT(out, out_loc);
    }
    r_list_poke(out, axis - 1, axis_names);
  }

  FREE(2);
  return out;
}

static r_obj* rray_combine_axis_names(r_obj* xs, int axis, int axis_dimension) {
  const r_ssize n = r_length(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);

  r_obj* out = r_null;
  r_keep_loc out_loc;
  KEEP_HERE(out, &out_loc);

  r_ssize out_i = 0;

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* x = v_xs[i];
    r_obj* dimensions = r_dim(x);
    const int dimensionality = rray_dimensionality_from_dimensions(dimensions);
    const int dimension =
      (axis <= dimensionality) ? r_int_get(dimensions, axis - 1) : 1;

    r_obj* names = r_dim_names(x);
    r_obj* axis_names = (names != r_null && axis <= dimensionality)
      ? r_list_get(names, axis - 1)
      : r_null;

    if (axis_names != r_null) {
      if (out == r_null) {
        out = r_alloc_character(axis_dimension);
        KEEP_AT(out, out_loc);
      }

      r_obj* const* v_axis_names = r_chr_cbegin(axis_names);

      for (int j = 0; j < dimension; ++j) {
        r_chr_poke(out, out_i + j, v_axis_names[j]);
      }
    }

    out_i += dimension;
  }

  FREE(1);
  return out;
}

static void rray_combine_fill(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const int* v_x_broadcast_dimensions,
  int out_dimensionality,
  const r_ssize* v_out_strides,
  const r_ssize* v_x_strides
) {
  switch (r_typeof(x)) {
  case R_TYPE_logical:
    rray_combine_fill_lgl(
      x,
      out,
      out_start,
      v_x_broadcast_dimensions,
      out_dimensionality,
      v_out_strides,
      v_x_strides
    );
    break;
  case R_TYPE_integer:
    rray_combine_fill_int(
      x,
      out,
      out_start,
      v_x_broadcast_dimensions,
      out_dimensionality,
      v_out_strides,
      v_x_strides
    );
    break;
  case R_TYPE_double:
    rray_combine_fill_dbl(
      x,
      out,
      out_start,
      v_x_broadcast_dimensions,
      out_dimensionality,
      v_out_strides,
      v_x_strides
    );
    break;
  case R_TYPE_complex:
    rray_combine_fill_cpl(
      x,
      out,
      out_start,
      v_x_broadcast_dimensions,
      out_dimensionality,
      v_out_strides,
      v_x_strides
    );
    break;
  case R_TYPE_raw:
    rray_combine_fill_raw(
      x,
      out,
      out_start,
      v_x_broadcast_dimensions,
      out_dimensionality,
      v_out_strides,
      v_x_strides
    );
    break;
  case R_TYPE_character:
    rray_combine_fill_chr(
      x,
      out,
      out_start,
      v_x_broadcast_dimensions,
      out_dimensionality,
      v_out_strides,
      v_x_strides
    );
    break;
  case R_TYPE_list:
    rray_combine_fill_list(
      x,
      out,
      out_start,
      v_x_broadcast_dimensions,
      out_dimensionality,
      v_out_strides,
      v_x_strides
    );
    break;
  default:
    r_stop_unreachable();
  }
}

#define RRAY_COMBINE_FILL_LOOP(CTYPE, POKE)                                    \
  for (; !rray_run_iterator_done(&it); rray_run_iterator_next2(&it)) {         \
    const r_ssize start = rray_run_iterator_start(&it);                        \
    const r_ssize end = rray_run_iterator_end(&it);                            \
                                                                               \
    r_ssize out_loc = out_start + rray_run_iterator_loc(&it, 0);               \
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);               \
                                                                               \
    r_ssize x_loc = rray_run_iterator_loc(&it, 1);                             \
    const r_ssize x_stride = rray_run_iterator_stride(&it, 1);                 \
                                                                               \
    if (x_stride == 0) {                                                       \
      CTYPE const x_elt = v_x[x_loc];                                          \
      for (r_ssize i = start; i < end; ++i) {                                  \
        POKE(out, out_loc, x_elt);                                             \
        out_loc += out_stride;                                                 \
      }                                                                        \
    } else {                                                                   \
      for (r_ssize i = start; i < end; ++i) {                                  \
        POKE(out, out_loc, v_x[x_loc]);                                        \
        out_loc += out_stride;                                                 \
        x_loc += x_stride;                                                     \
      }                                                                        \
    }                                                                          \
  }

#define RRAY_COMBINE_FILL_ATOMIC_POKE(OUT, LOC, VALUE) v_out[LOC] = (VALUE)

#define RRAY_COMBINE_FILL_ATOMIC(CTYPE, CONST_DEREF, DEREF)                    \
  struct rray_run_iterator it;                                                 \
  rray_run_iterator_init2(                                                     \
    &it,                                                                       \
    v_x_broadcast_dimensions,                                                  \
    out_dimensionality,                                                        \
    v_out_strides,                                                             \
    v_x_strides                                                                \
  );                                                                           \
                                                                               \
  CTYPE* v_out = DEREF(out);                                                   \
                                                                               \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
                                                                               \
  RRAY_COMBINE_FILL_LOOP(CTYPE, RRAY_COMBINE_FILL_ATOMIC_POKE)

#define RRAY_COMBINE_FILL_BARRIER(CONST_DEREF, POKE)                           \
  struct rray_run_iterator it;                                                 \
  rray_run_iterator_init2(                                                     \
    &it,                                                                       \
    v_x_broadcast_dimensions,                                                  \
    out_dimensionality,                                                        \
    v_out_strides,                                                             \
    v_x_strides                                                                \
  );                                                                           \
                                                                               \
  r_obj* const* v_x = CONST_DEREF(x);                                          \
                                                                               \
  RRAY_COMBINE_FILL_LOOP(r_obj*, POKE)

static void rray_combine_fill_lgl(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const int* v_x_broadcast_dimensions,
  int out_dimensionality,
  const r_ssize* v_out_strides,
  const r_ssize* v_x_strides
) {
  RRAY_COMBINE_FILL_ATOMIC(int, r_lgl_cbegin, r_lgl_begin);
}

static void rray_combine_fill_int(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const int* v_x_broadcast_dimensions,
  int out_dimensionality,
  const r_ssize* v_out_strides,
  const r_ssize* v_x_strides
) {
  RRAY_COMBINE_FILL_ATOMIC(int, r_int_cbegin, r_int_begin);
}

static void rray_combine_fill_dbl(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const int* v_x_broadcast_dimensions,
  int out_dimensionality,
  const r_ssize* v_out_strides,
  const r_ssize* v_x_strides
) {
  RRAY_COMBINE_FILL_ATOMIC(double, r_dbl_cbegin, r_dbl_begin);
}

static void rray_combine_fill_cpl(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const int* v_x_broadcast_dimensions,
  int out_dimensionality,
  const r_ssize* v_out_strides,
  const r_ssize* v_x_strides
) {
  RRAY_COMBINE_FILL_ATOMIC(r_complex, r_cpl_cbegin, r_cpl_begin);
}

static void rray_combine_fill_raw(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const int* v_x_broadcast_dimensions,
  int out_dimensionality,
  const r_ssize* v_out_strides,
  const r_ssize* v_x_strides
) {
  RRAY_COMBINE_FILL_ATOMIC(Rbyte, r_raw_cbegin, r_raw_begin);
}

static void rray_combine_fill_chr(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const int* v_x_broadcast_dimensions,
  int out_dimensionality,
  const r_ssize* v_out_strides,
  const r_ssize* v_x_strides
) {
  RRAY_COMBINE_FILL_BARRIER(r_chr_cbegin, r_chr_poke);
}

static void rray_combine_fill_list(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const int* v_x_broadcast_dimensions,
  int out_dimensionality,
  const r_ssize* v_out_strides,
  const r_ssize* v_x_strides
) {
  RRAY_COMBINE_FILL_BARRIER(r_list_cbegin, r_list_poke);
}

#undef RRAY_COMBINE_FILL_LOOP
#undef RRAY_COMBINE_FILL_ATOMIC_POKE
#undef RRAY_COMBINE_FILL_ATOMIC
#undef RRAY_COMBINE_FILL_BARRIER
