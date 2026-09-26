#include "slice.h"

#include "dimensionality.h"
#include "missing.h"
#include "size.h"
#include "slice-subscript.h"
#include "strides.h"
#include "utils.h"

#include "decl/slice-decl.h"

r_obj* ffi_rray_slice(r_obj* ffi_x, r_obj* ffi_indices, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
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

  r_obj* const* v_indices = r_list_cbegin(indices);
  const r_ssize indices_size = r_length(indices);

  if (indices_size != dimensionality) {
    r_abort_lazy_call(
      error_call,
      "Must supply exactly %d subscript%s to `...`, not %" R_PRI_SSIZE ".",
      dimensionality,
      dimensionality == 1 ? "" : "s",
      indices_size
    );
  }

  if (r_names(indices) != r_null) {
    r_abort_lazy_call(error_call, "All elements of `...` must be unnamed.");
  }

  r_obj* dimensions = KEEP_N(r_alloc_integer(dimensionality), &n_prot);
  int* v_dimensions = r_int_begin(dimensions);

  const int* v_v_locations[RRAY_MAX_DIMENSIONALITY];

  r_ssize i = 0;
  struct rray_arg* index_arg =
    new_subscript_arg(indices_arg, r_null, dimensionality, &i);
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

static r_obj* rray_slice_as_locations(struct rray_subscript subscript) {
  switch (subscript.kind) {
  case RRAY_SUBSCRIPT_KIND_locations_int:
    return subscript.index;
  case RRAY_SUBSCRIPT_KIND_locations_dbl: {
    const double* v_index = r_dbl_cbegin(subscript.index);

    r_obj* out = KEEP(r_alloc_integer(subscript.size));
    int* v_out = r_int_begin(out);

    for (r_ssize i = 0; i < subscript.size; ++i) {
      const double location = v_index[i];
      v_out[i] =
        rray_dbl_is_missing(location) ? r_globals.na_int : (int) location;
    }

    FREE(1);
    return out;
  }
  case RRAY_SUBSCRIPT_KIND_mask: {
    const int* v_index = r_lgl_cbegin(subscript.index);

    r_obj* out = KEEP(r_alloc_integer(subscript.size));
    int* v_out = r_int_begin(out);

    if (r_length(subscript.index) == 1) {
      const int elt = v_index[0];

      if (elt == 1) {
        r_stop_internal("A scalar `TRUE` should have been handled already.");
      } else if (elt == 0) {
        // Nothing to do
      } else if (elt == r_globals.na_lgl) {
        for (r_ssize i = 0; i < subscript.size; ++i) {
          v_out[i] = r_globals.na_int;
        }
      } else {
        r_stop_unreachable();
      }
    } else {
      r_ssize i = 0;
      r_ssize location = 0;

      while (i < subscript.size) {
        const int elt = v_index[location];
        v_out[i] =
          elt == r_globals.na_lgl ? r_globals.na_int : (int) location + 1;
        i += elt != 0;
        ++location;
      }
    }

    FREE(1);
    return out;
  }
  case RRAY_SUBSCRIPT_KIND_points_int:
  case RRAY_SUBSCRIPT_KIND_points_dbl:
    r_stop_unreachable();
  }

  r_stop_unreachable();
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

static bool rray_slice_locations_any_missing(
  const int* const* v_v_locations,
  const int* v_dimensions,
  int dimensionality
) {
  for (int axis = 0; axis < dimensionality; ++axis) {
    const int* v_locations = v_v_locations[axis];
    if (v_locations == NULL) {
      continue;
    }

    const int dimension = v_dimensions[axis];

    for (int i = 0; i < dimension; ++i) {
      if (v_locations[i] == r_globals.na_int) {
        return true;
      }
    }
  }

  return false;
}

static inline r_ssize rray_slice_start(
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const int* v_point,
  int dimensionality,
  r_ssize size,
  bool any_missing
) {
  if (size == 0) {
    return 0;
  }

  r_ssize out = 0;

  for (int axis = 1; axis < dimensionality; ++axis) {
    const int* v_locations = v_v_locations[axis];
    const int location =
      v_locations == NULL ? v_point[axis] + 1 : v_locations[v_point[axis]];
    if (any_missing && location == r_globals.na_int) {
      return -1;
    }
    out += ((r_ssize) location - 1) * v_x_strides[axis];
  }

  return out;
}

static inline r_ssize rray_slice_offset(
  const int* v_locations,
  r_ssize stride,
  int point
) {
  return v_locations == NULL ? (r_ssize) point * stride
                             : ((r_ssize) v_locations[point] - 1) * stride;
}

#define RRAY_SLICE_NEXT(START, V_POINT)                                        \
  for (int axis = 1; axis < dimensionality; ++axis) {                          \
    const int* v_locations = v_v_locations[axis];                              \
    const r_ssize stride = v_x_strides[axis];                                  \
    START -= rray_slice_offset(v_locations, stride, V_POINT[axis]);            \
    ++V_POINT[axis];                                                           \
    if (V_POINT[axis] < v_dimensions[axis]) {                                  \
      START += rray_slice_offset(v_locations, stride, V_POINT[axis]);          \
      break;                                                                   \
    }                                                                          \
    V_POINT[axis] = 0;                                                         \
    START += rray_slice_offset(v_locations, stride, 0);                        \
  }

#define RRAY_SLICE_NEXT_POINT(V_POINT)                                         \
  for (int axis = 1; axis < dimensionality; ++axis) {                          \
    ++V_POINT[axis];                                                           \
    if (V_POINT[axis] < v_dimensions[axis]) {                                  \
      break;                                                                   \
    }                                                                          \
    V_POINT[axis] = 0;                                                         \
  }

#define RRAY_SLICE_LOOP(POKE, LOCATION)                                        \
  r_ssize start = rray_slice_start(                                            \
    v_v_locations,                                                             \
    v_x_strides,                                                               \
    v_point,                                                                   \
    dimensionality,                                                            \
    size,                                                                      \
    false                                                                      \
  );                                                                           \
  while (run_start != size) {                                                  \
    for (r_ssize i = 0; i < run_size; ++i) {                                   \
      const int location = (LOCATION);                                         \
      POKE(out, run_start + i, v_x[start + ((r_ssize) location - 1)]);         \
    }                                                                          \
                                                                               \
    run_start += run_size;                                                     \
    RRAY_SLICE_NEXT(start, v_point);                                           \
  }

#define RRAY_SLICE_LOOP_MISSING(POKE, MISSING, LOCATION)                       \
  while (run_start != size) {                                                  \
    const r_ssize start = rray_slice_start(                                    \
      v_v_locations,                                                           \
      v_x_strides,                                                             \
      v_point,                                                                 \
      dimensionality,                                                          \
      size,                                                                    \
      true                                                                     \
    );                                                                         \
    for (r_ssize i = 0; i < run_size; ++i) {                                   \
      const int location = (LOCATION);                                         \
      POKE(                                                                    \
        out,                                                                   \
        run_start + i,                                                         \
        start < 0 || location == r_globals.na_int                              \
          ? MISSING                                                            \
          : v_x[start + ((r_ssize) location - 1)]                              \
      );                                                                       \
    }                                                                          \
                                                                               \
    run_start += run_size;                                                     \
    RRAY_SLICE_NEXT_POINT(v_point);                                            \
  }

#define RRAY_SLICE_ITERATE(POKE, MISSING)                                      \
  const int* v_run_locations = v_v_locations[0];                               \
  const r_ssize run_size = v_dimensions[0];                                    \
  r_ssize run_start = 0;                                                       \
                                                                               \
  int v_point[RRAY_MAX_DIMENSIONALITY] = {0};                                  \
                                                                               \
  if (v_run_locations == NULL) {                                               \
    if (any_missing) {                                                         \
      RRAY_SLICE_LOOP_MISSING(POKE, MISSING, i + 1);                           \
    } else {                                                                   \
      RRAY_SLICE_LOOP(POKE, i + 1);                                            \
    }                                                                          \
  } else {                                                                     \
    if (any_missing) {                                                         \
      RRAY_SLICE_LOOP_MISSING(POKE, MISSING, v_run_locations[i]);              \
    } else {                                                                   \
      RRAY_SLICE_LOOP(POKE, v_run_locations[i]);                               \
    }                                                                          \
  }

#define RRAY_SLICE_ATOMIC_POKE(OUT, I, VALUE) v_out[I] = (VALUE)

#define RRAY_SLICE_ATOMIC(RTYPE, CTYPE, CONST_DEREF, DEREF, MISSING)           \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, size));                              \
  CTYPE* v_out = DEREF(out);                                                   \
                                                                               \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
                                                                               \
  RRAY_SLICE_ITERATE(RRAY_SLICE_ATOMIC_POKE, MISSING);                         \
                                                                               \
  FREE(1);                                                                     \
  return out;

#define RRAY_SLICE_BARRIER(RTYPE, CONST_DEREF, POKE, MISSING)                  \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, size));                              \
                                                                               \
  r_obj* const* v_x = CONST_DEREF(x);                                          \
                                                                               \
  RRAY_SLICE_ITERATE(POKE, MISSING);                                           \
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

#undef RRAY_SLICE_NEXT
#undef RRAY_SLICE_NEXT_POINT
#undef RRAY_SLICE_LOOP
#undef RRAY_SLICE_LOOP_MISSING
#undef RRAY_SLICE_ITERATE
#undef RRAY_SLICE_ATOMIC_POKE
#undef RRAY_SLICE_ATOMIC
#undef RRAY_SLICE_BARRIER
