#include "slice.h"

#include "dimensionality.h"
#include "size.h"
#include "slice-iterator.h"
#include "slice-subscript.h"
#include "strides.h"
#include "utils.h"

#include "decl/slice-decl.h"

r_obj* ffi_rray_slice(r_obj* ffi_x, r_obj* ffi_indices, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  check_slice_indices_unnamed(ffi_indices, error_call);
  return rray_slice(
    ffi_x,
    ffi_indices,
    rray_args.x,
    rray_args.empty,
    error_call
  );
}

r_obj* rray_slice(
  r_obj* x,
  r_obj* indices,
  struct rray_arg* x_arg,
  struct rray_arg* indices_arg,
  struct r_lazy error_call
) {
  int n_prot = 0;

  check_unclassed(x, x_arg, error_call);
  x = KEEP_N(arg_as_array(x, x_arg, error_call), &n_prot);

  r_obj* x_dimensions = r_dim(x);
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(x_dimensions);
  check_dimensionality(dimensionality);

  r_obj* x_names = r_dim_names(x);
  r_obj* const* v_x_names = x_names == r_null ? NULL : r_list_cbegin(x_names);

  r_ssize v_x_strides[RRAY_MAX_DIMENSIONALITY];
  rray_fill_strides_from_dimensions(
    v_x_dimensions,
    dimensionality,
    v_x_strides
  );

  check_slice_indices_size(indices, dimensionality, error_call);
  r_obj* const* v_indices = r_list_cbegin(indices);
  r_obj* indices_names = r_names(indices);

  r_obj* dimensions = KEEP_N(r_alloc_integer(dimensionality), &n_prot);
  int* v_dimensions = r_int_begin(dimensions);

  // `...` is capped by the dimensionality of `x`
  const int* v_v_locations[RRAY_MAX_DIMENSIONALITY];

  r_ssize i = 0;
  struct rray_arg* index_arg =
    new_subscript_arg(indices_arg, indices_names, dimensionality, &i);
  KEEP_N(index_arg->shelter, &n_prot);

  for (; i < dimensionality; ++i) {
    r_obj* index = v_indices[i];
    const int x_dimension = v_x_dimensions[i];
    r_obj* x_axis_names = v_x_names == NULL ? r_null : v_x_names[i];

    const struct rray_subscript subscript = rray_as_slice_subscript(
      index,
      x_dimension,
      x_axis_names,
      index_arg,
      error_call
    );
    KEEP_N(subscript.index, &n_prot);

    v_dimensions[i] = r_ssize_as_integer(subscript.size);

    if (r_is_true(index)) {
      // Optimization! No allocation for `TRUE`, which replaces a base R
      // missing arg.
      v_v_locations[i] = NULL;
    } else {
      r_obj* locations = KEEP_N(rray_slice_as_locations(subscript), &n_prot);
      v_v_locations[i] = r_int_cbegin(locations);
    }
  }

  const r_ssize size =
    rray_size_from_dimensions_checked(v_dimensions, dimensionality, error_call);

  const bool any_missing = rray_slice_locations_any_missing(
    v_v_locations,
    v_dimensions,
    dimensionality
  );

  r_obj* out;

  switch (r_typeof(x)) {
  case R_TYPE_logical:
    out = rray_slice_lgl(
      x,
      v_v_locations,
      v_x_strides,
      v_dimensions,
      dimensionality,
      size,
      any_missing
    );
    break;
  case R_TYPE_integer:
    out = rray_slice_int(
      x,
      v_v_locations,
      v_x_strides,
      v_dimensions,
      dimensionality,
      size,
      any_missing
    );
    break;
  case R_TYPE_double:
    out = rray_slice_dbl(
      x,
      v_v_locations,
      v_x_strides,
      v_dimensions,
      dimensionality,
      size,
      any_missing
    );
    break;
  case R_TYPE_complex:
    out = rray_slice_cpl(
      x,
      v_v_locations,
      v_x_strides,
      v_dimensions,
      dimensionality,
      size,
      any_missing
    );
    break;
  case R_TYPE_raw:
    out = rray_slice_raw(
      x,
      v_v_locations,
      v_x_strides,
      v_dimensions,
      dimensionality,
      size,
      any_missing
    );
    break;
  case R_TYPE_character:
    out = rray_slice_chr(
      x,
      v_v_locations,
      v_x_strides,
      v_dimensions,
      dimensionality,
      size,
      any_missing
    );
    break;
  case R_TYPE_list:
    out = rray_slice_list(
      x,
      v_v_locations,
      v_x_strides,
      v_dimensions,
      dimensionality,
      size,
      any_missing
    );
    break;
  default:
    r_stop_unreachable();
  }

  KEEP_N(out, &n_prot);
  r_attrib_poke_dim(out, dimensions);

  r_obj* names = KEEP_N(
    rray_slice_names(v_x_names, v_dimensions, dimensionality, v_v_locations),
    &n_prot
  );

  if (names != r_null) {
    r_attrib_poke_dim_names(out, names);
  }

  FREE(n_prot);
  return out;
}

static r_obj* rray_slice_names(
  r_obj* const* v_x_names,
  const int* v_dimensions,
  int dimensionality,
  const int* const* v_v_locations
) {
  if (v_x_names == NULL) {
    return r_null;
  }

  r_obj* out = KEEP(r_alloc_list(dimensionality));

  for (int axis = 0; axis < dimensionality; ++axis) {
    r_obj* x_axis_names = v_x_names[axis];

    if (x_axis_names == r_null) {
      continue;
    }

    r_obj* axis_names = rray_slice_axis_names(
      x_axis_names,
      v_v_locations[axis],
      v_dimensions[axis]
    );
    r_list_poke(out, axis, axis_names);
  }

  FREE(1);
  return out;
}

static r_obj* rray_slice_axis_names(
  r_obj* x_axis_names,
  const int* v_locations,
  int dimension
) {
  if (v_locations == NULL) {
    // `TRUE` selects all names
    return x_axis_names;
  }

  r_obj* const* v_x_axis_names = r_chr_cbegin(x_axis_names);

  r_obj* out = KEEP(r_alloc_character(dimension));

  for (int i = 0; i < dimension; ++i) {
    const int location = v_locations[i];
    r_chr_poke(
      out,
      i,
      location == r_globals.na_int ? r_strs.empty : v_x_axis_names[location - 1]
    );
  }

  FREE(1);
  return out;
}

// The literal `true` / `false` values for `ANY_MISSING` allow the compiler to
// remove those checks when none are missing
#define RRAY_SLICE_RUN(LOCATION, N, ANY_MISSING, POKE, MISSING)                \
  for (r_ssize i = 0; i < run_size; ++i) {                                     \
    const int location = (LOCATION);                                           \
    const bool missing =                                                       \
      ANY_MISSING && (start == -1 || location == r_globals.na_int);            \
                                                                               \
    POKE(                                                                      \
      out,                                                                     \
      run_start + i,                                                           \
      missing ? MISSING : v_x[start + ((r_ssize) location - 1)]                \
    );                                                                         \
  }

#define RRAY_SLICE_ATOMIC_POKE(OUT, I, VALUE) v_out[I] = (VALUE)

#define RRAY_SLICE_ATOMIC(RTYPE, CTYPE, CONST_DEREF, DEREF, MISSING)           \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, size));                              \
  CTYPE* v_out = DEREF(out);                                                   \
                                                                               \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
                                                                               \
  RRAY_SLICE_ITERATE(RRAY_SLICE_RUN, RRAY_SLICE_ATOMIC_POKE, MISSING);         \
                                                                               \
  FREE(1);                                                                     \
  return out;

#define RRAY_SLICE_BARRIER(RTYPE, CONST_DEREF, POKE, MISSING)                  \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, size));                              \
                                                                               \
  r_obj* const* v_x = CONST_DEREF(x);                                          \
                                                                               \
  RRAY_SLICE_ITERATE(RRAY_SLICE_RUN, POKE, MISSING);                           \
                                                                               \
  FREE(1);                                                                     \
  return out;

static r_obj* rray_slice_lgl(
  r_obj* x,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
) {
  RRAY_SLICE_ATOMIC(
    R_TYPE_logical,
    int,
    r_lgl_cbegin,
    r_lgl_begin,
    r_globals.na_lgl
  );
}

static r_obj* rray_slice_int(
  r_obj* x,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
) {
  RRAY_SLICE_ATOMIC(
    R_TYPE_integer,
    int,
    r_int_cbegin,
    r_int_begin,
    r_globals.na_int
  );
}

static r_obj* rray_slice_dbl(
  r_obj* x,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
) {
  RRAY_SLICE_ATOMIC(
    R_TYPE_double,
    double,
    r_dbl_cbegin,
    r_dbl_begin,
    r_globals.na_dbl
  );
}

static r_obj* rray_slice_cpl(
  r_obj* x,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
) {
  RRAY_SLICE_ATOMIC(
    R_TYPE_complex,
    r_complex,
    r_cpl_cbegin,
    r_cpl_begin,
    r_globals.na_cpl
  );
}

static r_obj* rray_slice_raw(
  r_obj* x,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
) {
  RRAY_SLICE_ATOMIC(R_TYPE_raw, Rbyte, r_raw_cbegin, r_raw_begin, 0);
}

static r_obj* rray_slice_chr(
  r_obj* x,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
) {
  RRAY_SLICE_BARRIER(
    R_TYPE_character,
    r_chr_cbegin,
    r_chr_poke,
    r_globals.na_str
  );
}

static r_obj* rray_slice_list(
  r_obj* x,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
) {
  RRAY_SLICE_BARRIER(R_TYPE_list, r_list_cbegin, r_list_poke, r_null);
}

#undef RRAY_SLICE_RUN
#undef RRAY_SLICE_ATOMIC_POKE
#undef RRAY_SLICE_ATOMIC
#undef RRAY_SLICE_BARRIER
