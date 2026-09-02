#include "broadcast-names.h"

#include "dimensionality.h"

#include "decl/broadcast-names-decl.h"

r_obj* ffi_rray_broadcast_names(r_obj* ffi_x, r_obj* ffi_dimensions) {
  return rray_broadcast_names(ffi_x, ffi_dimensions);
}

r_obj* rray_broadcast_names(r_obj* x, r_obj* dimensions) {
  return rray_broadcast_names_fill(r_null, x, dimensions);
}

r_obj* ffi_rray_broadcast_names2(
  r_obj* ffi_x,
  r_obj* ffi_y,
  r_obj* ffi_dimensions
) {
  return rray_broadcast_names2(ffi_x, ffi_y, ffi_dimensions);
}

r_obj* rray_broadcast_names2(r_obj* x, r_obj* y, r_obj* dimensions) {
  r_obj* out = KEEP(rray_broadcast_names_fill(r_null, x, dimensions));
  out = rray_broadcast_names_fill(out, y, dimensions);
  FREE(1);
  return out;
}

r_obj* ffi_rray_broadcast_names_common(r_obj* ffi_xs, r_obj* ffi_dimensions) {
  return rray_broadcast_names_common(ffi_xs, ffi_dimensions);
}

r_obj* rray_broadcast_names_common(r_obj* xs, r_obj* dimensions) {
  const r_ssize n = r_length(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);

  r_obj* out = r_null;
  r_keep_loc out_loc;
  KEEP_HERE(out, &out_loc);

  for (r_ssize i = 0; i < n; ++i) {
    out = rray_broadcast_names_fill(out, v_xs[i], dimensions);
    KEEP_AT(out, out_loc);
  }

  FREE(1);
  return out;
}

// Broadcasting of array names
//
// Assumes the inputs are validated arrays and are broadcastable to the
// `dimensions`. The caller typically checks all of this already.
static r_obj* rray_broadcast_names_fill(
  r_obj* out,
  r_obj* x,
  r_obj* dimensions
) {
  r_obj* x_names = r_dim_names(x);

  if (x_names == r_null) {
    return out;
  }

  r_obj* const* v_x_names = r_list_cbegin(x_names);

  r_obj* x_dimensions = r_dim(x);
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);
  const int x_dimensionality =
    rray_dimensionality_from_dimensions(x_dimensions);

  const int* v_dimensions = r_int_cbegin(dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(dimensions);

  r_keep_loc out_loc;
  KEEP_HERE(out, &out_loc);

  for (int i = 0; i < x_dimensionality; ++i) {
    if (v_x_names[i] == r_null) {
      // No names to contribute on this axis
      continue;
    }
    if (v_x_dimensions[i] != v_dimensions[i]) {
      // No names to contribute when axis is broadcast
      continue;
    }
    if (out != r_null && r_list_get(out, i) != r_null) {
      // Names already exist on this axis
      continue;
    }
    if (out == r_null) {
      out = r_alloc_list(dimensionality);
      KEEP_AT(out, out_loc);
    }
    r_list_poke(out, i, v_x_names[i]);
  }

  FREE(1);
  return out;
}
