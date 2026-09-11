#include "permute-axes.h"

#include "axes.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "gather.h"
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

  r_obj* out = KEEP(rray_gather(x, size, &it));
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
