#include "axes.h"
#include "decl/split-template-decl.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "names.h"
#include "reduce.h"
#include "reduction-iterator.h"
#include "size.h"
#include "utils.h"

#if RRAY_TYPE == RRAY_TYPE_LOGICAL
#define RRAY_FN rray_split_lgl
#define RRAY_R_TYPE R_TYPE_logical
#define RRAY_C_TYPE int
#define RRAY_CONST_DEREF r_lgl_cbegin
#define RRAY_DEREF r_lgl_begin
#define RRAY_OUT_ASSIGN(out_loc, elt_loc, val)                                 \
  v_v_out_elt[out_loc][elt_loc] = val

#elif RRAY_TYPE == RRAY_TYPE_INTEGER
#define RRAY_FN rray_split_int
#define RRAY_R_TYPE R_TYPE_integer
#define RRAY_C_TYPE int
#define RRAY_CONST_DEREF r_int_cbegin
#define RRAY_DEREF r_int_begin
#define RRAY_OUT_ASSIGN(out_loc, elt_loc, val)                                 \
  v_v_out_elt[out_loc][elt_loc] = val

#elif RRAY_TYPE == RRAY_TYPE_DOUBLE
#define RRAY_FN rray_split_dbl
#define RRAY_R_TYPE R_TYPE_double
#define RRAY_C_TYPE double
#define RRAY_CONST_DEREF r_dbl_cbegin
#define RRAY_DEREF r_dbl_begin
#define RRAY_OUT_ASSIGN(out_loc, elt_loc, val)                                 \
  v_v_out_elt[out_loc][elt_loc] = val

#elif RRAY_TYPE == RRAY_TYPE_COMPLEX
#define RRAY_FN rray_split_cpl
#define RRAY_R_TYPE R_TYPE_complex
#define RRAY_C_TYPE r_complex
#define RRAY_CONST_DEREF r_cpl_cbegin
#define RRAY_DEREF r_cpl_begin
#define RRAY_OUT_ASSIGN(out_loc, elt_loc, val)                                 \
  v_v_out_elt[out_loc][elt_loc] = val

#elif RRAY_TYPE == RRAY_TYPE_RAW
#define RRAY_FN rray_split_raw
#define RRAY_R_TYPE R_TYPE_raw
#define RRAY_C_TYPE Rbyte
#define RRAY_CONST_DEREF r_raw_cbegin
#define RRAY_DEREF r_raw_begin
#define RRAY_OUT_ASSIGN(out_loc, elt_loc, val)                                 \
  v_v_out_elt[out_loc][elt_loc] = val

#elif RRAY_TYPE == RRAY_TYPE_CHARACTER
#define RRAY_FN rray_split_chr
#define RRAY_R_TYPE R_TYPE_character
#define RRAY_C_TYPE r_obj*
#define RRAY_CONST_DEREF r_chr_cbegin
#define RRAY_OUT_ASSIGN(out_loc, elt_loc, val)                                 \
  r_chr_poke(v_out[out_loc], elt_loc, val)

#elif RRAY_TYPE == RRAY_TYPE_LIST
#define RRAY_FN rray_split_list
#define RRAY_R_TYPE R_TYPE_list
#define RRAY_C_TYPE r_obj*
#define RRAY_CONST_DEREF r_list_cbegin
#define RRAY_OUT_ASSIGN(out_loc, elt_loc, val)                                 \
  r_list_poke(v_out[out_loc], elt_loc, val)
#endif

static inline r_obj* RRAY_FN(r_obj* x, r_obj* axes, struct r_lazy error_call) {
  int n_kept = 0;

  r_obj* x_dimensions = KEEP_N(rray_dimensions(x, error_call), &n_kept);
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);

  const int dimensionality = rray_dimensionality_from_dimensions(x_dimensions);

  axes =
    KEEP_N(arg_as_axes(axes, dimensionality, axes_chr, error_call), &n_kept);
  const int* v_axes = r_int_cbegin(axes);
  const r_ssize axes_size = r_length(axes);

  // Splitting 4x3x2 on axis 3 gives 4x3x1 out dimensions
  r_obj* out_elt_dimensions = KEEP_N(
    rray_reduce_dimensions(v_x_dimensions, dimensionality, v_axes, axes_size),
    &n_kept
  );
  const int* v_out_elt_dimensions = r_int_cbegin(out_elt_dimensions);

  // Splitting 4x3x2 on axis 3 gives 1x1x2 split dimensions
  r_obj* out_dimensions = KEEP_N(
    rray_split_dimensions(v_x_dimensions, dimensionality, v_axes, axes_size),
    &n_kept
  );
  const int* v_out_dimensions = r_int_cbegin(out_dimensions);

  const r_ssize x_size =
    rray_size_from_dimensions(v_x_dimensions, dimensionality);
  const r_ssize out_elt_size =
    rray_size_from_dimensions(v_out_elt_dimensions, dimensionality);
  const r_ssize out_size =
    rray_size_from_dimensions(v_out_dimensions, dimensionality);

  r_obj* out = KEEP_N(r_alloc_list(out_size), &n_kept);
  r_obj* const* v_out = r_list_cbegin(out);

  for (r_ssize i = 0; i < out_size; ++i) {
    r_obj* out_elt = r_alloc_vector(RRAY_R_TYPE, out_elt_size);
    r_list_poke(out, i, out_elt);
    r_attrib_poke_dim(out_elt, out_elt_dimensions);
  }

  struct rray_iterator out_elt_it;
  rray_reduction_iterator_init(
    &out_elt_it,
    v_x_dimensions,
    v_out_elt_dimensions,
    dimensionality
  );

  struct rray_iterator out_it;
  rray_reduction_iterator_init(
    &out_it,
    v_x_dimensions,
    v_out_dimensions,
    dimensionality
  );

  RRAY_C_TYPE const* v_x = RRAY_CONST_DEREF(x);

#ifdef RRAY_DEREF
  r_obj* out_elt_pointers =
    KEEP_N(r_alloc_raw(out_size * sizeof(RRAY_C_TYPE*)), &n_kept);
  RRAY_C_TYPE** v_v_out_elt = (RRAY_C_TYPE**) r_raw_begin(out_elt_pointers);
  for (r_ssize j = 0; j < out_size; ++j) {
    v_v_out_elt[j] = RRAY_DEREF(v_out[j]);
  }
#endif

  for (r_ssize i = 0; i < x_size; ++i) {
    const r_ssize out_loc = rray_iterator_location(&out_it);
    const r_ssize out_elt_loc = rray_iterator_location(&out_elt_it);

    RRAY_OUT_ASSIGN(out_loc, out_elt_loc, v_x[i]);

    rray_iterator_next(&out_it);
    rray_iterator_next(&out_elt_it);
  }

  r_obj* x_names = rray_names(x, error_call);
  if (x_names != r_null) {
    KEEP_N(x_names, &n_kept);
    r_obj* const* v_x_names = r_list_cbegin(x_names);
    rray_split_names(
      out,
      v_x_names,
      v_out_dimensions,
      dimensionality,
      out_size
    );
  }

  FREE(n_kept);
  return out;
}

#ifndef RRAY_ONCE
#define RRAY_ONCE

r_obj* rray_split_dimensions(
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

void rray_split_names(
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

#endif // RRAY_ONCE

#undef RRAY_TYPE
#undef RRAY_FN
#undef RRAY_R_TYPE
#undef RRAY_C_TYPE
#undef RRAY_CONST_DEREF
#ifdef RRAY_DEREF
#undef RRAY_DEREF
#endif
#undef RRAY_OUT_ASSIGN
