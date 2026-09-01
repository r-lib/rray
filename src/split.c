#include "split.h"

#include "axes.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "names.h"
#include "reduce.h"
#include "reduction-iterator.h"
#include "size.h"
#include "utils.h"

#include "decl/split-decl.h"

r_obj* ffi_rray_split(r_obj* ffi_x, r_obj* ffi_axes, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_split(ffi_x, ffi_axes, error_call);
}

r_obj* rray_split(r_obj* x, r_obj* axes, struct r_lazy error_call) {
  check_unclassed(x, "x", error_call);
  x = KEEP(arg_as_array(x, "x", error_call));

  r_obj* x_dimensions = KEEP(rray_dimensions(x, error_call));
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);

  const int dimensionality = rray_dimensionality_from_dimensions(x_dimensions);

  axes = KEEP(arg_as_axes(axes, dimensionality, axes_chr, error_call));
  const int* v_axes = r_int_cbegin(axes);
  const r_ssize axes_size = r_length(axes);

  // Splitting 4x3x2 on axis 3 gives 4x3x1 out dimensions
  r_obj* out_elt_dimensions = KEEP(
    rray_reduce_dimensions(v_x_dimensions, dimensionality, v_axes, axes_size)
  );
  const int* v_out_elt_dimensions = r_int_cbegin(out_elt_dimensions);

  // Splitting 4x3x2 on axis 3 gives 1x1x2 split dimensions
  r_obj* out_dimensions = KEEP(
    rray_split_dimensions(v_x_dimensions, dimensionality, v_axes, axes_size)
  );
  const int* v_out_dimensions = r_int_cbegin(out_dimensions);

  const r_ssize out_elt_size =
    rray_size_from_dimensions(v_out_elt_dimensions, dimensionality);
  const r_ssize out_size =
    rray_size_from_dimensions(v_out_dimensions, dimensionality);

  struct rray_iterator out_it;
  rray_reduction_iterator_init(
    &out_it,
    v_x_dimensions,
    v_out_dimensions,
    dimensionality
  );

  struct rray_iterator out_elt_it;
  rray_reduction_iterator_init(
    &out_elt_it,
    v_x_dimensions,
    v_out_elt_dimensions,
    dimensionality
  );

  r_obj* out;

  switch (r_typeof(x)) {
  case R_TYPE_logical:
    out = rray_split_lgl(
      x,
      out_elt_dimensions,
      out_size,
      out_elt_size,
      &out_it,
      &out_elt_it
    );
    break;
  case R_TYPE_integer:
    out = rray_split_int(
      x,
      out_elt_dimensions,
      out_size,
      out_elt_size,
      &out_it,
      &out_elt_it
    );
    break;
  case R_TYPE_double:
    out = rray_split_dbl(
      x,
      out_elt_dimensions,
      out_size,
      out_elt_size,
      &out_it,
      &out_elt_it
    );
    break;
  case R_TYPE_complex:
    out = rray_split_cpl(
      x,
      out_elt_dimensions,
      out_size,
      out_elt_size,
      &out_it,
      &out_elt_it
    );
    break;
  case R_TYPE_raw:
    out = rray_split_raw(
      x,
      out_elt_dimensions,
      out_size,
      out_elt_size,
      &out_it,
      &out_elt_it
    );
    break;
  case R_TYPE_character:
    out = rray_split_chr(
      x,
      out_elt_dimensions,
      out_size,
      out_elt_size,
      &out_it,
      &out_elt_it
    );
    break;
  case R_TYPE_list:
    out = rray_split_list(
      x,
      out_elt_dimensions,
      out_size,
      out_elt_size,
      &out_it,
      &out_elt_it
    );
    break;
  default:
    r_stop_unreachable();
  }

  KEEP(out);

  r_obj* x_names = rray_names(x, error_call);
  if (x_names != r_null) {
    KEEP(x_names);

    rray_split_names(
      out,
      r_list_cbegin(x_names),
      v_out_dimensions,
      dimensionality,
      out_size
    );

    FREE(1);
  }

  FREE(6);
  return out;
}

#define RRAY_SPLIT_ATOMIC(RTYPE, CTYPE, CONST_DEREF, DEREF)                    \
  r_obj* out = KEEP(r_alloc_list(out_size));                                   \
                                                                               \
  for (r_ssize i = 0; i < out_size; ++i) {                                     \
    r_obj* out_elt = r_alloc_vector(RTYPE, out_elt_size);                      \
    r_list_poke(out, i, out_elt);                                              \
    r_attrib_poke_dim(out_elt, out_elt_dimensions);                            \
  }                                                                            \
                                                                               \
  r_obj* shelter = KEEP(r_alloc_raw(out_size * sizeof(CTYPE*)));               \
  CTYPE** v_v_out = (CTYPE**) r_raw_begin(shelter);                            \
                                                                               \
  r_obj* const* v_out = r_list_cbegin(out);                                    \
                                                                               \
  for (r_ssize i = 0; i < out_size; ++i) {                                     \
    v_v_out[i] = DEREF(v_out[i]);                                              \
  }                                                                            \
                                                                               \
  const r_ssize x_size = r_length(x);                                          \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
                                                                               \
  for (r_ssize i = 0; i < x_size; ++i) {                                       \
    const r_ssize out_loc = rray_iterator_location(out_it);                    \
    const r_ssize out_elt_loc = rray_iterator_location(out_elt_it);            \
                                                                               \
    v_v_out[out_loc][out_elt_loc] = v_x[i];                                    \
                                                                               \
    rray_iterator_next(out_it);                                                \
    rray_iterator_next(out_elt_it);                                            \
  }                                                                            \
                                                                               \
  FREE(2);                                                                     \
  return out;

#define RRAY_SPLIT_BARRIER(RTYPE, CONST_DEREF, POKE)                           \
  r_obj* out = KEEP(r_alloc_list(out_size));                                   \
                                                                               \
  for (r_ssize i = 0; i < out_size; ++i) {                                     \
    r_obj* out_elt = r_alloc_vector(RTYPE, out_elt_size);                      \
    r_list_poke(out, i, out_elt);                                              \
    r_attrib_poke_dim(out_elt, out_elt_dimensions);                            \
  }                                                                            \
                                                                               \
  r_obj* const* v_out = r_list_cbegin(out);                                    \
                                                                               \
  const r_ssize x_size = r_length(x);                                          \
  r_obj* const* v_x = CONST_DEREF(x);                                          \
                                                                               \
  for (r_ssize i = 0; i < x_size; ++i) {                                       \
    const r_ssize out_loc = rray_iterator_location(out_it);                    \
    const r_ssize out_elt_loc = rray_iterator_location(out_elt_it);            \
                                                                               \
    POKE(v_out[out_loc], out_elt_loc, v_x[i]);                                 \
                                                                               \
    rray_iterator_next(out_it);                                                \
    rray_iterator_next(out_elt_it);                                            \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

static r_obj* rray_split_lgl(
  r_obj* x,
  r_obj* out_elt_dimensions,
  r_ssize out_size,
  r_ssize out_elt_size,
  struct rray_iterator* out_it,
  struct rray_iterator* out_elt_it
) {
  RRAY_SPLIT_ATOMIC(R_TYPE_logical, int, r_lgl_cbegin, r_lgl_begin);
}

static r_obj* rray_split_int(
  r_obj* x,
  r_obj* out_elt_dimensions,
  r_ssize out_size,
  r_ssize out_elt_size,
  struct rray_iterator* out_it,
  struct rray_iterator* out_elt_it
) {
  RRAY_SPLIT_ATOMIC(R_TYPE_integer, int, r_int_cbegin, r_int_begin);
}

static r_obj* rray_split_dbl(
  r_obj* x,
  r_obj* out_elt_dimensions,
  r_ssize out_size,
  r_ssize out_elt_size,
  struct rray_iterator* out_it,
  struct rray_iterator* out_elt_it
) {
  RRAY_SPLIT_ATOMIC(R_TYPE_double, double, r_dbl_cbegin, r_dbl_begin);
}

static r_obj* rray_split_cpl(
  r_obj* x,
  r_obj* out_elt_dimensions,
  r_ssize out_size,
  r_ssize out_elt_size,
  struct rray_iterator* out_it,
  struct rray_iterator* out_elt_it
) {
  RRAY_SPLIT_ATOMIC(R_TYPE_complex, r_complex, r_cpl_cbegin, r_cpl_begin);
}

static r_obj* rray_split_raw(
  r_obj* x,
  r_obj* out_elt_dimensions,
  r_ssize out_size,
  r_ssize out_elt_size,
  struct rray_iterator* out_it,
  struct rray_iterator* out_elt_it
) {
  RRAY_SPLIT_ATOMIC(R_TYPE_raw, Rbyte, r_raw_cbegin, r_raw_begin);
}

static r_obj* rray_split_chr(
  r_obj* x,
  r_obj* out_elt_dimensions,
  r_ssize out_size,
  r_ssize out_elt_size,
  struct rray_iterator* out_it,
  struct rray_iterator* out_elt_it
) {
  RRAY_SPLIT_BARRIER(R_TYPE_character, r_chr_cbegin, r_chr_poke);
}

static r_obj* rray_split_list(
  r_obj* x,
  r_obj* out_elt_dimensions,
  r_ssize out_size,
  r_ssize out_elt_size,
  struct rray_iterator* out_it,
  struct rray_iterator* out_elt_it
) {
  RRAY_SPLIT_BARRIER(R_TYPE_list, r_list_cbegin, r_list_poke);
}

#undef RRAY_SPLIT_ATOMIC
#undef RRAY_SPLIT_BARRIER

static r_obj* rray_split_dimensions(
  const int* v_dimensions,
  int dimensionality,
  const int* v_axes,
  r_ssize axes_size
) {
  r_obj* out = KEEP(r_alloc_integer(dimensionality));
  int* v_out = r_int_begin(out);

  for (int i = 0; i < dimensionality; ++i) {
    v_out[i] = 1;
  }

  for (r_ssize i = 0; i < axes_size; ++i) {
    v_out[v_axes[i] - 1] = v_dimensions[v_axes[i] - 1];
  }

  FREE(1);
  return out;
}

static void rray_split_names(
  r_obj* out,
  r_obj* const* v_x_names,
  const int* v_out_dimensions,
  int dimensionality,
  r_ssize out_size
) {
  r_obj* const* v_out = r_list_cbegin(out);

  struct rray_iterator it;
  rray_reduction_iterator_init(
    &it,
    v_out_dimensions,
    v_out_dimensions,
    dimensionality
  );

  for (r_ssize i = 0; i < out_size; ++i) {
    const r_ssize* v_point = rray_iterator_point(&it);

    r_obj* names = r_null;
    r_keep_loc names_loc;
    KEEP_HERE(names, &names_loc);

    for (int j = 0; j < dimensionality; ++j) {
      r_obj* x_axis_names = v_x_names[j];
      if (x_axis_names == r_null) {
        // `names` stays `r_null` when there were no names before
        continue;
      }

      if (names == r_null) {
        names = r_alloc_list(dimensionality);
        KEEP_AT(names, names_loc);
      }

      if (v_out_dimensions[j] == 1) {
        // Axis isn't split, its names carry over whole
        r_list_poke(names, j, x_axis_names);
      } else {
        r_obj* axis_names = r_alloc_character(1);
        r_list_poke(names, j, axis_names);
        r_chr_poke(axis_names, 0, r_chr_get(x_axis_names, v_point[j]));
      }
    }

    r_attrib_poke_dim_names(v_out[i], names);

    FREE(1);
    rray_iterator_next(&it);
  }
}
