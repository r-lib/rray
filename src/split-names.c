#include "split-names.h"

#include "dimensionality.h"
#include "names.h"
#include "reduction-iterator.h"
#include "size.h"

#include "decl/split-names-decl.h"

r_obj* ffi_rray_split_names(r_obj* ffi_x, r_obj* ffi_dimensions) {
  return rray_split_names(ffi_x, ffi_dimensions);
}

// Splitting of array names
//
// Assumes the inputs are validated arrays and that `dimensions` are the split
// dimensions of `x`. The caller typically checks all of this already.
//
// Mimics `rray_broadcast_names()` ideas, but splitting returns one set of names
// per output element
r_obj* rray_split_names(r_obj* x, r_obj* dimensions) {
  r_obj* x_names = rray_names(x, rray_args.x, r_lazy_null);

  if (x_names == r_null) {
    return r_null;
  }

  KEEP(x_names);

  r_obj* const* v_x_names = r_list_cbegin(x_names);

  const int* v_dimensions = r_int_cbegin(dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(dimensions);

  const r_ssize size = rray_size_from_dimensions(v_dimensions, dimensionality);

  r_obj* out = KEEP(r_alloc_list(size));

  struct rray_iterator it;
  rray_reduction_iterator_init(&it, v_dimensions, v_dimensions, dimensionality);

  for (r_ssize i = 0; i < size; ++i) {
    r_list_poke(
      out,
      i,
      rray_split_names_one(
        v_x_names,
        v_dimensions,
        dimensionality,
        rray_iterator_point(&it)
      )
    );
    rray_iterator_next(&it);
  }

  FREE(2);
  return out;
}

static r_obj* rray_split_names_one(
  r_obj* const* v_x_names,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_point
) {
  r_obj* out = r_null;
  r_keep_loc out_loc;
  KEEP_HERE(out, &out_loc);

  for (int i = 0; i < dimensionality; ++i) {
    r_obj* x_axis_names = v_x_names[i];

    if (x_axis_names == r_null) {
      // No names to contribute on this axis
      continue;
    }

    if (out == r_null) {
      out = r_alloc_list(dimensionality);
      KEEP_AT(out, out_loc);
    }

    if (v_dimensions[i] == 1) {
      // Axis isn't split, its names carry over whole
      r_list_poke(out, i, x_axis_names);
    } else {
      r_obj* axis_names = r_alloc_character(1);
      r_list_poke(out, i, axis_names);
      r_chr_poke(axis_names, 0, r_chr_get(x_axis_names, v_point[i]));
    }
  }

  FREE(1);
  return out;
}
