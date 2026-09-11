#include "permute-axes.h"

#include "axes.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "iterator.h"
#include "size.h"
#include "utils.h"

#include "decl/permute-axes-decl.h"

r_obj* ffi_rray_permute_axes(r_obj* ffi_x, r_obj* ffi_axes, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_permute_axes(ffi_x, ffi_axes, rray_args.x, error_call);
}

r_obj* rray_permute_axes(
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
  check_max_dimensionality(dimensionality);

  axes =
    KEEP(arg_as_permutation(axes, dimensionality, rray_args.axes, error_call));
  const int* v_axes = r_int_cbegin(axes);

  r_obj* dimensions = KEEP(r_alloc_integer(dimensionality));
  int* v_dimensions = r_int_begin(dimensions);

  for (int i = 0; i < dimensionality; ++i) {
    v_dimensions[i] = v_x_dimensions[v_axes[i] - 1];
  }

  r_ssize v_strides[RRAY_MAX_DIMENSIONALITY];
  rray_permute_axes_strides(v_strides, v_x_dimensions, v_axes, dimensionality);

  const r_ssize size =
    rray_size_from_dimensions(v_x_dimensions, dimensionality);

  struct rray_iterator it;
  rray_iterator_init_strides(
    &it,
    size,
    v_dimensions,
    dimensionality,
    v_strides
  );

  r_obj* out;

  switch (r_typeof(x)) {
  case R_TYPE_logical:
    out = rray_permute_axes_lgl(x, size, &it);
    break;
  case R_TYPE_integer:
    out = rray_permute_axes_int(x, size, &it);
    break;
  case R_TYPE_double:
    out = rray_permute_axes_dbl(x, size, &it);
    break;
  case R_TYPE_complex:
    out = rray_permute_axes_cpl(x, size, &it);
    break;
  case R_TYPE_raw:
    out = rray_permute_axes_raw(x, size, &it);
    break;
  case R_TYPE_character:
    out = rray_permute_axes_chr(x, size, &it);
    break;
  case R_TYPE_list:
    out = rray_permute_axes_list(x, size, &it);
    break;
  default:
    r_stop_unreachable();
  }

  KEEP(out);
  r_attrib_poke_dim(out, dimensions);

  r_obj* names = KEEP(rray_permute_axes_names(x, v_axes, dimensionality));

  if (names != r_null) {
    r_attrib_poke_dim_names(out, names);
  }

  FREE(6);
  return out;
}

static void rray_permute_axes_strides(
  r_ssize* v_strides,
  const int* v_x_dimensions,
  const int* v_axes,
  int dimensionality
) {
  r_ssize v_x_strides[RRAY_MAX_DIMENSIONALITY];

  r_ssize stride = 1;

  for (int i = 0; i < dimensionality; ++i) {
    v_x_strides[i] = stride;
    stride *= v_x_dimensions[i];
  }

  for (int i = 0; i < dimensionality; ++i) {
    v_strides[i] = v_x_strides[v_axes[i] - 1];
  }
}

#define RRAY_PERMUTE_AXES_ATOMIC(RTYPE, CTYPE, CONST_DEREF, DEREF)             \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, size));                              \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
  CTYPE* v_out = DEREF(out);                                                   \
                                                                               \
  RRAY_ITERATOR_FOR_EACH(it, i, loc, { v_out[i] = v_x[loc]; });                \
                                                                               \
  FREE(1);                                                                     \
  return out;

#define RRAY_PERMUTE_AXES_BARRIER(RTYPE, CONST_DEREF, POKE)                    \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, size));                              \
  r_obj* const* v_x = CONST_DEREF(x);                                          \
                                                                               \
  RRAY_ITERATOR_FOR_EACH(it, i, loc, { POKE(out, i, v_x[loc]); });             \
                                                                               \
  FREE(1);                                                                     \
  return out;

static r_obj* rray_permute_axes_lgl(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
) {
  RRAY_PERMUTE_AXES_ATOMIC(R_TYPE_logical, int, r_lgl_cbegin, r_lgl_begin);
}

static r_obj* rray_permute_axes_int(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
) {
  RRAY_PERMUTE_AXES_ATOMIC(R_TYPE_integer, int, r_int_cbegin, r_int_begin);
}

static r_obj* rray_permute_axes_dbl(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
) {
  RRAY_PERMUTE_AXES_ATOMIC(R_TYPE_double, double, r_dbl_cbegin, r_dbl_begin);
}

static r_obj* rray_permute_axes_cpl(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
) {
  RRAY_PERMUTE_AXES_ATOMIC(
    R_TYPE_complex,
    r_complex,
    r_cpl_cbegin,
    r_cpl_begin
  );
}

static r_obj* rray_permute_axes_raw(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
) {
  RRAY_PERMUTE_AXES_ATOMIC(R_TYPE_raw, Rbyte, r_raw_cbegin, r_raw_begin);
}

static r_obj* rray_permute_axes_chr(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
) {
  RRAY_PERMUTE_AXES_BARRIER(R_TYPE_character, r_chr_cbegin, r_chr_poke);
}

static r_obj* rray_permute_axes_list(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
) {
  RRAY_PERMUTE_AXES_BARRIER(R_TYPE_list, r_list_cbegin, r_list_poke);
}

#undef RRAY_PERMUTE_AXES_ATOMIC
#undef RRAY_PERMUTE_AXES_BARRIER

static r_obj* rray_permute_axes_names(
  r_obj* x,
  const int* v_axes,
  int dimensionality
) {
  r_obj* x_names = r_dim_names(x);

  if (x_names == r_null) {
    return r_null;
  }

  r_obj* const* v_x_names = r_list_cbegin(x_names);

  r_obj* out = KEEP(r_alloc_list(dimensionality));

  for (int i = 0; i < dimensionality; ++i) {
    r_list_poke(out, i, v_x_names[v_axes[i] - 1]);
  }

  FREE(1);
  return out;
}
