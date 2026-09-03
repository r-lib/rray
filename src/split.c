#include "split.h"

#include "axes.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "iterator2.h"
#include "reduce.h"
#include "size.h"
#include "split-names.h"
#include "utils.h"

#include "decl/split-decl.h"

r_obj* ffi_rray_split(r_obj* ffi_x, r_obj* ffi_axes, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_split(ffi_x, ffi_axes, rray_args.x, error_call);
}

r_obj* rray_split(
  r_obj* x,
  r_obj* axes,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));

  r_obj* x_dimensions = KEEP(rray_dimensions(x, arg, error_call));
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);

  const int dimensionality = rray_dimensionality_from_dimensions(x_dimensions);

  axes = KEEP(arg_as_axes(axes, dimensionality, rray_args.axes, error_call));
  const int* v_axes = r_int_cbegin(axes);
  const r_ssize axes_size = r_length(axes);

  // Splitting 4x3x2 on axis 3 gives 4x3x1 out dimensions
  r_obj* out_elt_dimensions = KEEP(
    rray_reduce_dimensions(v_x_dimensions, dimensionality, v_axes, axes_size)
  );
  const int* v_out_elt_dimensions = r_int_cbegin(out_elt_dimensions);

  r_obj* axes_complement =
    KEEP(rray_axes_complement(v_axes, axes_size, dimensionality));
  const int* v_axes_complement = r_int_cbegin(axes_complement);
  const r_ssize axes_complement_size = r_length(axes_complement);

  // Splitting 4x3x2 on axis 3 gives 1x1x2 split dimensions
  r_obj* out_dimensions = KEEP(rray_reduce_dimensions(
    v_x_dimensions,
    dimensionality,
    v_axes_complement,
    axes_complement_size
  ));
  const int* v_out_dimensions = r_int_cbegin(out_dimensions);

  const r_ssize out_elt_size =
    rray_size_from_dimensions(v_out_elt_dimensions, dimensionality);
  const r_ssize out_size =
    rray_size_from_dimensions(v_out_dimensions, dimensionality);

  const enum r_type type = r_typeof(x);

  r_obj* out = KEEP(r_alloc_list(out_size));

  for (r_ssize i = 0; i < out_size; ++i) {
    r_obj* out_elt = r_alloc_vector(type, out_elt_size);
    r_list_poke(out, i, out_elt);
    r_attrib_poke_dim(out_elt, out_elt_dimensions);
  }

  struct rray_iterator2 it;
  rray_iterator2_init(
    &it,
    v_x_dimensions,
    dimensionality,
    v_out_dimensions,
    dimensionality,
    v_out_elt_dimensions,
    dimensionality
  );

  switch (type) {
  case R_TYPE_logical:
    rray_split_lgl(x, out, &it);
    break;
  case R_TYPE_integer:
    rray_split_int(x, out, &it);
    break;
  case R_TYPE_double:
    rray_split_dbl(x, out, &it);
    break;
  case R_TYPE_complex:
    rray_split_cpl(x, out, &it);
    break;
  case R_TYPE_raw:
    rray_split_raw(x, out, &it);
    break;
  case R_TYPE_character:
    rray_split_chr(x, out, &it);
    break;
  case R_TYPE_list:
    rray_split_list(x, out, &it);
    break;
  default:
    r_stop_unreachable();
  }

  r_obj* names = KEEP(rray_split_names(x, out_dimensions));

  if (names != r_null) {
    r_obj* const* v_names = r_list_cbegin(names);
    r_obj* const* v_out = r_list_cbegin(out);

    for (r_ssize i = 0; i < out_size; ++i) {
      r_attrib_poke_dim_names(v_out[i], v_names[i]);
    }
  }

  FREE(8);
  return out;
}

#define RRAY_SPLIT_ATOMIC(CTYPE, CONST_DEREF, DEREF)                           \
  const r_ssize x_size = r_length(x);                                          \
  const r_ssize out_size = r_length(out);                                      \
                                                                               \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
  r_obj* const* v_out = r_list_cbegin(out);                                    \
                                                                               \
  r_obj* shelter = KEEP(r_alloc_raw(out_size * sizeof(CTYPE*)));               \
  CTYPE** v_v_out = (CTYPE**) r_raw_begin(shelter);                            \
                                                                               \
  for (r_ssize i = 0; i < out_size; ++i) {                                     \
    v_v_out[i] = DEREF(v_out[i]);                                              \
  }                                                                            \
                                                                               \
  for (r_ssize i = 0; i < x_size; ++i) {                                       \
    const r_ssize out_loc = rray_iterator2_location1(it);                      \
    const r_ssize out_elt_loc = rray_iterator2_location2(it);                  \
                                                                               \
    v_v_out[out_loc][out_elt_loc] = v_x[i];                                    \
                                                                               \
    rray_iterator2_next(it);                                                   \
  }                                                                            \
                                                                               \
  FREE(1);

#define RRAY_SPLIT_BARRIER(CONST_DEREF, POKE)                                  \
  const r_ssize x_size = r_length(x);                                          \
                                                                               \
  r_obj* const* v_x = CONST_DEREF(x);                                          \
  r_obj* const* v_out = r_list_cbegin(out);                                    \
                                                                               \
  for (r_ssize i = 0; i < x_size; ++i) {                                       \
    const r_ssize out_loc = rray_iterator2_location1(it);                      \
    const r_ssize out_elt_loc = rray_iterator2_location2(it);                  \
                                                                               \
    POKE(v_out[out_loc], out_elt_loc, v_x[i]);                                 \
                                                                               \
    rray_iterator2_next(it);                                                   \
  }

static void rray_split_lgl(r_obj* x, r_obj* out, struct rray_iterator2* it) {
  RRAY_SPLIT_ATOMIC(int, r_lgl_cbegin, r_lgl_begin);
}

static void rray_split_int(r_obj* x, r_obj* out, struct rray_iterator2* it) {
  RRAY_SPLIT_ATOMIC(int, r_int_cbegin, r_int_begin);
}

static void rray_split_dbl(r_obj* x, r_obj* out, struct rray_iterator2* it) {
  RRAY_SPLIT_ATOMIC(double, r_dbl_cbegin, r_dbl_begin);
}

static void rray_split_cpl(r_obj* x, r_obj* out, struct rray_iterator2* it) {
  RRAY_SPLIT_ATOMIC(r_complex, r_cpl_cbegin, r_cpl_begin);
}

static void rray_split_raw(r_obj* x, r_obj* out, struct rray_iterator2* it) {
  RRAY_SPLIT_ATOMIC(Rbyte, r_raw_cbegin, r_raw_begin);
}

static void rray_split_chr(r_obj* x, r_obj* out, struct rray_iterator2* it) {
  RRAY_SPLIT_BARRIER(r_chr_cbegin, r_chr_poke);
}

static void rray_split_list(r_obj* x, r_obj* out, struct rray_iterator2* it) {
  RRAY_SPLIT_BARRIER(r_list_cbegin, r_list_poke);
}

#undef RRAY_SPLIT_ATOMIC
#undef RRAY_SPLIT_BARRIER
