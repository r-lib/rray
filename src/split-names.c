#include "split-names.h"

#include "axes.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "iterator.h"
#include "names.h"
#include "size.h"

#include "decl/split-names-decl.h"

r_obj* ffi_rray_split_names(r_obj* ffi_x, r_obj* ffi_axes) {
  return rray_split_names(ffi_x, ffi_axes);
}

// Splitting of array names
//
// Assumes the inputs are validated arrays and that `axes` are valid axes of
// `x`. The caller typically checks all of this already.
//
// Mimics `rray_broadcast_names()` ideas, but splitting returns one set of names
// per output element
r_obj* rray_split_names(r_obj* x, r_obj* axes) {
  r_obj* x_names = rray_names(x, rray_args.x, r_lazy_null);
  if (x_names == r_null) {
    return r_null;
  }
  KEEP(x_names);

  r_obj* const* v_x_names = r_list_cbegin(x_names);

  r_obj* x_dimensions = KEEP(rray_dimensions(x, rray_args.x, r_lazy_null));
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(x_dimensions);

  const int* v_axes = r_int_cbegin(axes);
  const r_ssize axes_size = r_length(axes);

  r_obj* axes_complement =
    KEEP(rray_axes_complement(v_axes, axes_size, dimensionality));
  const int* v_axes_complement = r_int_cbegin(axes_complement);
  const r_ssize axes_complement_size = r_length(axes_complement);

  r_obj* dimensions = KEEP(rray_set_axes_dimension(
    v_x_dimensions,
    dimensionality,
    v_axes_complement,
    axes_complement_size,
    1
  ));
  const int* v_dimensions = r_int_cbegin(dimensions);

  // Pre-split axis names into size 1 elements we can reuse across multiple
  // output arrays. Reusing these size 1 names saves quite a bit of time and
  // memory when splitting across multiple axes.
  r_obj* axes_names =
    rray_split_axes_names(v_x_names, v_dimensions, dimensionality);
  if (axes_names == r_null) {
    // Names existed, but all axis names were `NULL`
    FREE(4);
    return r_null;
  }
  KEEP(axes_names);

  r_obj* const* v_axes_names = r_list_cbegin(axes_names);

  // Direct pointers into the names lists for faster accesses
  r_obj* const** v_v_axis_names =
    (r_obj* const**) R_alloc(dimensionality, sizeof(r_obj* const*));

  for (int i = 0; i < dimensionality; ++i) {
    r_obj* axis_names = v_axes_names[i];
    v_v_axis_names[i] =
      (axis_names == r_null) ? NULL : r_list_cbegin(axis_names);
  }

  const r_ssize size = rray_size_from_dimensions(v_dimensions, dimensionality);

  r_obj* out = KEEP(r_alloc_list(size));

  struct rray_point_iterator it;
  rray_point_iterator_init(&it, size, v_dimensions, dimensionality);

  RRAY_POINT_ITERATOR_FOR_EACH(&it, i, v_point, {
    r_obj* names = r_alloc_list(dimensionality);
    r_list_poke(out, i, names);

    for (int j = 0; j < dimensionality; ++j) {
      r_obj* const* v_axis_names = v_v_axis_names[j];
      if (v_axis_names != NULL) {
        r_list_poke(names, j, v_axis_names[v_point[j]]);
      }
    }
  });

  FREE(6);
  return out;
}

// Splitting a 2x2x2 array on axes 1 and 3 gives split dimensions of 2x1x2
//
// x           array(1:8, c(2, 2, 2))
// v_x_names   list(c("a1", "a2"), c("b1", "b2"), c("c1", "c2"))
// out         list(list("a1", "a2"), list(c("b1", "b2")), list("c1", "c2"))
//
// Axes 1 and 3 are split so they get 1 slot per name, axis 2 isn't split so
// it gets 1 slot with all its names
//
// The split gives 4 arrays of 1x2x1 that take their names from `out`
//
//   [[1]] 1 3   list("a1", c("b1", "b2"), "c1")
//   [[2]] 2 4   list("a2", c("b1", "b2"), "c1")
//   [[3]] 5 7   list("a1", c("b1", "b2"), "c2")
//   [[4]] 6 8   list("a2", c("b1", "b2"), "c2")
//
// "a1" is the same vector in [[1]] and [[3]], "c1" in [[1]] and [[2]], so 4
// length 1 vectors cover all 8 slots
static r_obj* rray_split_axes_names(
  r_obj* const* v_x_names,
  const int* v_dimensions,
  int dimensionality
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

    const int dimension = v_dimensions[i];

    r_obj* axis_names = r_alloc_list(dimension);
    r_list_poke(out, i, axis_names);

    if (dimension == 1) {
      // Axis isn't split, its names carry over whole
      r_list_poke(axis_names, 0, x_axis_names);
      continue;
    }

    r_obj* const* v_x_axis_names = r_chr_cbegin(x_axis_names);

    for (int j = 0; j < dimension; ++j) {
      r_obj* names = r_alloc_character(1);
      r_list_poke(axis_names, j, names);
      r_chr_poke(names, 0, v_x_axis_names[j]);
    }
  }

  FREE(1);
  return out;
}
