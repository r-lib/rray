#include "slice-assign.h"

#include "broadcast.h"
#include "cast.h"
#include "clone.h"
#include "dimensionality.h"
#include "size.h"
#include "slice-iterator.h"
#include "slice-subscript.h"
#include "strides.h"
#include "utils.h"

#include "decl/slice-assign-decl.h"

r_obj* ffi_rray_slice_assign(
  r_obj* ffi_x,
  r_obj* ffi_indices,
  r_obj* ffi_value,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  // Checked at the FFI boundary to disallow names from the R side, but on the C
  // side we allow them so that `rray_slice_assign_axis()` and friends throw
  // good error messages for `i`
  check_slice_indices_unnamed(ffi_indices, error_call);
  return rray_slice_assign(
    ffi_x,
    ffi_indices,
    ffi_value,
    rray_args.x,
    rray_args.empty,
    rray_args.value,
    error_call
  );
}

r_obj* rray_slice_assign(
  r_obj* x,
  r_obj* indices,
  r_obj* value,
  struct rray_arg* x_arg,
  struct rray_arg* indices_arg,
  struct rray_arg* value_arg,
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

  int v_dimensions[RRAY_MAX_DIMENSIONALITY];
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
      v_v_locations[i] = NULL;
    } else {
      r_obj* locations = KEEP_N(rray_slice_as_locations(subscript), &n_prot);
      v_v_locations[i] = r_int_cbegin(locations);
    }
  }

  const r_ssize size =
    rray_size_from_dimensions_checked(v_dimensions, dimensionality, error_call);

  value = KEEP_N(rray_cast(value, x, value_arg, x_arg, error_call), &n_prot);

  r_obj* value_dimensions = r_dim(value);
  const int* v_value_dimensions = r_int_cbegin(value_dimensions);
  const int value_dimensionality =
    rray_dimensionality_from_dimensions(value_dimensions);

  check_broadcastable(
    v_value_dimensions,
    value_dimensionality,
    v_dimensions,
    dimensionality,
    value_arg,
    error_call
  );

  r_ssize v_value_strides[RRAY_MAX_DIMENSIONALITY];
  rray_fill_broadcast_strides_from_dimensions(
    v_value_dimensions,
    value_dimensionality,
    dimensionality,
    v_value_strides
  );

  const bool any_missing = rray_slice_locations_any_missing(
    v_v_locations,
    v_dimensions,
    dimensionality
  );

  r_obj* out = KEEP_N(r_clone_data(x), &n_prot);
  r_attrib_poke_dim(out, x_dimensions);

  if (x_names != r_null) {
    r_attrib_poke_dim_names(out, x_names);
  }

  switch (r_typeof(out)) {
  case R_TYPE_logical:
    rray_slice_assign_lgl(
      out,
      value,
      v_v_locations,
      v_x_strides,
      v_value_strides,
      v_dimensions,
      dimensionality,
      size,
      any_missing
    );
    break;
  case R_TYPE_integer:
    rray_slice_assign_int(
      out,
      value,
      v_v_locations,
      v_x_strides,
      v_value_strides,
      v_dimensions,
      dimensionality,
      size,
      any_missing
    );
    break;
  case R_TYPE_double:
    rray_slice_assign_dbl(
      out,
      value,
      v_v_locations,
      v_x_strides,
      v_value_strides,
      v_dimensions,
      dimensionality,
      size,
      any_missing
    );
    break;
  case R_TYPE_complex:
    rray_slice_assign_cpl(
      out,
      value,
      v_v_locations,
      v_x_strides,
      v_value_strides,
      v_dimensions,
      dimensionality,
      size,
      any_missing
    );
    break;
  case R_TYPE_raw:
    rray_slice_assign_raw(
      out,
      value,
      v_v_locations,
      v_x_strides,
      v_value_strides,
      v_dimensions,
      dimensionality,
      size,
      any_missing
    );
    break;
  case R_TYPE_character:
    rray_slice_assign_chr(
      out,
      value,
      v_v_locations,
      v_x_strides,
      v_value_strides,
      v_dimensions,
      dimensionality,
      size,
      any_missing
    );
    break;
  case R_TYPE_list:
    rray_slice_assign_list(
      out,
      value,
      v_v_locations,
      v_x_strides,
      v_value_strides,
      v_dimensions,
      dimensionality,
      size,
      any_missing
    );
    break;
  default:
    r_stop_unreachable();
  }

  FREE(n_prot);
  return out;
}

static inline r_ssize rray_slice_assign_value_start(
  const r_ssize* v_value_strides,
  const int* v_point,
  int dimensionality
) {
  r_ssize out = 0;

  for (int axis = 1; axis < dimensionality; ++axis) {
    out += v_point[axis] * v_value_strides[axis];
  }

  return out;
}

#define RRAY_SLICE_ASSIGN_RUN(LOCATION, N, ANY_MISSING, POKE)                  \
  const r_ssize value_start =                                                  \
    rray_slice_assign_value_start(v_value_strides, v_point, N);                \
  const r_ssize value_run_stride = v_value_strides[0];                         \
                                                                               \
  for (r_ssize i = 0; i < run_size; ++i) {                                     \
    const int location = (LOCATION);                                           \
    const bool missing =                                                       \
      ANY_MISSING && (start == -1 || location == r_globals.na_int);            \
                                                                               \
    if (!missing) {                                                            \
      POKE(                                                                    \
        out,                                                                   \
        start + ((r_ssize) location - 1),                                      \
        v_value[value_start + i * value_run_stride]                            \
      );                                                                       \
    }                                                                          \
  }

#define RRAY_SLICE_ASSIGN_ATOMIC_POKE(OUT, I, VALUE) v_out[I] = (VALUE)

#define RRAY_SLICE_ASSIGN_ATOMIC(CTYPE, CONST_DEREF, DEREF)                    \
  CTYPE* v_out = DEREF(out);                                                   \
  const CTYPE* v_value = CONST_DEREF(value);                                   \
                                                                               \
  RRAY_SLICE_ITERATE(RRAY_SLICE_ASSIGN_RUN, RRAY_SLICE_ASSIGN_ATOMIC_POKE);

#define RRAY_SLICE_ASSIGN_BARRIER(CONST_DEREF, POKE)                           \
  r_obj* const* v_value = CONST_DEREF(value);                                  \
                                                                               \
  RRAY_SLICE_ITERATE(RRAY_SLICE_ASSIGN_RUN, POKE);

static void rray_slice_assign_lgl(
  r_obj* out,
  r_obj* value,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const r_ssize* v_value_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
) {
  RRAY_SLICE_ASSIGN_ATOMIC(int, r_lgl_cbegin, r_lgl_begin);
}

static void rray_slice_assign_int(
  r_obj* out,
  r_obj* value,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const r_ssize* v_value_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
) {
  RRAY_SLICE_ASSIGN_ATOMIC(int, r_int_cbegin, r_int_begin);
}

static void rray_slice_assign_dbl(
  r_obj* out,
  r_obj* value,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const r_ssize* v_value_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
) {
  RRAY_SLICE_ASSIGN_ATOMIC(double, r_dbl_cbegin, r_dbl_begin);
}

static void rray_slice_assign_cpl(
  r_obj* out,
  r_obj* value,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const r_ssize* v_value_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
) {
  RRAY_SLICE_ASSIGN_ATOMIC(r_complex, r_cpl_cbegin, r_cpl_begin);
}

static void rray_slice_assign_raw(
  r_obj* out,
  r_obj* value,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const r_ssize* v_value_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
) {
  RRAY_SLICE_ASSIGN_ATOMIC(Rbyte, r_raw_cbegin, r_raw_begin);
}

static void rray_slice_assign_chr(
  r_obj* out,
  r_obj* value,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const r_ssize* v_value_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
) {
  RRAY_SLICE_ASSIGN_BARRIER(r_chr_cbegin, r_chr_poke);
}

static void rray_slice_assign_list(
  r_obj* out,
  r_obj* value,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const r_ssize* v_value_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
) {
  RRAY_SLICE_ASSIGN_BARRIER(r_list_cbegin, r_list_poke);
}

#undef RRAY_SLICE_ASSIGN_RUN
#undef RRAY_SLICE_ASSIGN_ATOMIC_POKE
#undef RRAY_SLICE_ASSIGN_ATOMIC
#undef RRAY_SLICE_ASSIGN_BARRIER
