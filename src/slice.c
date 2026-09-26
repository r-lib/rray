#include "slice.h"

#include "dimensionality.h"
#include "missing.h"
#include "size.h"
#include "slice-subscript.h"
#include "strides.h"
#include "utils.h"

struct rray_slice_axis {
  const int* v_locations;
  r_ssize stride;
  bool identity;
};

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

  struct rray_subscript v_subscripts[RRAY_MAX_DIMENSIONALITY];

  r_obj* dimensions = KEEP_N(r_alloc_integer(dimensionality), &n_prot);
  int* v_dimensions = r_int_begin(dimensions);

  r_ssize axis = 0;
  struct rray_arg* index_arg =
    new_subscript_arg(indices_arg, r_null, dimensionality, &axis);
  KEEP_N(index_arg->shelter, &n_prot);

  for (; axis < dimensionality; ++axis) {
    r_obj* x_axis_names = v_x_names == NULL ? r_null : v_x_names[axis];

    const struct rray_subscript subscript = rray_as_slice_subscript(
      v_indices[axis],
      v_x_dimensions[axis],
      x_axis_names,
      index_arg,
      error_call
    );
    KEEP_N(subscript.index, &n_prot);

    v_subscripts[axis] = subscript;
    v_dimensions[axis] = r_ssize_as_integer(subscript.size);
  }

  const r_ssize size =
    rray_size_from_dimensions_checked(v_dimensions, dimensionality, error_call);

  r_ssize v_x_strides[RRAY_MAX_DIMENSIONALITY];
  rray_fill_strides_from_dimensions(
    v_x_dimensions,
    dimensionality,
    v_x_strides
  );

  r_ssize locations_size = 0;
  struct rray_slice_axis v_axes[RRAY_MAX_DIMENSIONALITY];

  for (int i = 0; i < dimensionality; ++i) {
    const bool identity = r_is_true(v_indices[i]);
    v_axes[i].identity = identity;
    v_axes[i].stride = v_x_strides[i];
    if (
      !identity && v_subscripts[i].kind != RRAY_SUBSCRIPT_KIND_locations_int
    ) {
      locations_size += v_dimensions[i];
    }
  }

  int* v_locations = NULL;
  if (locations_size != 0) {
    r_obj* locations = KEEP_N(r_alloc_integer(locations_size), &n_prot);
    v_locations = r_int_begin(locations);
  }

  for (int i = 0; i < dimensionality; ++i) {
    if (v_axes[i].identity) {
      v_axes[i].v_locations = NULL;
    } else if (v_subscripts[i].kind == RRAY_SUBSCRIPT_KIND_locations_int) {
      v_axes[i].v_locations = r_int_cbegin(v_subscripts[i].index);
    } else {
      v_axes[i].v_locations = v_locations;
      rray_slice_fill_locations(v_subscripts[i], v_locations);
      if (v_dimensions[i] != 0) {
        v_locations += v_dimensions[i];
      }
    }
  }

  r_obj* names = KEEP_N(
    rray_slice_names(v_x_names, v_dimensions, dimensionality, v_axes),
    &n_prot
  );

  const bool any_missing =
    rray_slice_locations_any_missing(v_axes, v_dimensions, dimensionality);

  r_obj* out;

  switch (r_typeof(x)) {
  case R_TYPE_logical:
    out = rray_slice_lgl(
      x,
      v_axes,
      v_dimensions,
      dimensionality,
      size,
      any_missing
    );
    break;
  case R_TYPE_integer:
    out = rray_slice_int(
      x,
      v_axes,
      v_dimensions,
      dimensionality,
      size,
      any_missing
    );
    break;
  case R_TYPE_double:
    out = rray_slice_dbl(
      x,
      v_axes,
      v_dimensions,
      dimensionality,
      size,
      any_missing
    );
    break;
  case R_TYPE_complex:
    out = rray_slice_cpl(
      x,
      v_axes,
      v_dimensions,
      dimensionality,
      size,
      any_missing
    );
    break;
  case R_TYPE_raw:
    out = rray_slice_raw(
      x,
      v_axes,
      v_dimensions,
      dimensionality,
      size,
      any_missing
    );
    break;
  case R_TYPE_character:
    out = rray_slice_chr(
      x,
      v_axes,
      v_dimensions,
      dimensionality,
      size,
      any_missing
    );
    break;
  case R_TYPE_list:
    out = rray_slice_list(
      x,
      v_axes,
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

  if (names != r_null) {
    r_attrib_poke_dim_names(out, names);
  }

  FREE(n_prot);
  return out;
}

static void rray_slice_fill_locations(
  struct rray_subscript subscript,
  int* v_locations
) {
  switch (subscript.kind) {
  case RRAY_SUBSCRIPT_KIND_locations_dbl: {
    const double* v_index = r_dbl_cbegin(subscript.index);

    for (r_ssize i = 0; i < subscript.size; ++i) {
      const double location = v_index[i];
      v_locations[i] =
        rray_dbl_is_missing(location) ? r_globals.na_int : (int) location;
    }

    break;
  }
  case RRAY_SUBSCRIPT_KIND_mask: {
    const int* v_index = r_lgl_cbegin(subscript.index);
    const r_ssize index_step = r_length(subscript.index) == 1 ? 0 : 1;

    r_ssize i = 0;

    for (r_ssize location = 0; i < subscript.size; ++location) {
      const int elt = v_index[location * index_step];

      if (elt == 0) {
        continue;
      }

      v_locations[i] =
        elt == r_globals.na_lgl ? r_globals.na_int : (int) location + 1;
      ++i;
    }

    break;
  }
  case RRAY_SUBSCRIPT_KIND_locations_int:
  case RRAY_SUBSCRIPT_KIND_points_int:
  case RRAY_SUBSCRIPT_KIND_points_dbl:
    r_stop_unreachable();
  }
}

static r_obj* rray_slice_names(
  r_obj* const* v_x_names,
  const int* v_dimensions,
  int dimensionality,
  const struct rray_slice_axis* v_axes
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
      v_axes[axis].identity,
      v_axes[axis].v_locations,
      v_dimensions[axis]
    );
    r_list_poke(out, axis, axis_names);
  }

  FREE(1);
  return out;
}

static r_obj* rray_slice_axis_names(
  r_obj* x_axis_names,
  bool identity,
  const int* v_locations,
  int dimension
) {
  if (identity) {
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
  const struct rray_slice_axis* v_axes,
  const int* v_dimensions,
  int dimensionality
) {
  for (int axis = 0; axis < dimensionality; ++axis) {
    if (v_axes[axis].identity) {
      continue;
    }

    const int* v_locations = v_axes[axis].v_locations;
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
  const struct rray_slice_axis* v_axes,
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
    const struct rray_slice_axis* p_axis = &v_axes[axis];
    const int location =
      p_axis->identity ? v_point[axis] + 1 : p_axis->v_locations[v_point[axis]];
    if (any_missing && location == r_globals.na_int) {
      return -1;
    }
    out += ((r_ssize) location - 1) * p_axis->stride;
  }

  return out;
}

static inline r_ssize rray_slice_offset(
  const struct rray_slice_axis* v_axes,
  int axis,
  int point
) {
  const struct rray_slice_axis* p_axis = &v_axes[axis];
  return p_axis->identity
    ? (r_ssize) point * p_axis->stride
    : ((r_ssize) p_axis->v_locations[point] - 1) * p_axis->stride;
}

#define RRAY_SLICE_NEXT(START, V_POINT)                                        \
  for (int axis = 1; axis < dimensionality; ++axis) {                          \
    START -= rray_slice_offset(v_axes, axis, V_POINT[axis]);                   \
    ++V_POINT[axis];                                                           \
    if (V_POINT[axis] < v_dimensions[axis]) {                                  \
      START += rray_slice_offset(v_axes, axis, V_POINT[axis]);                 \
      break;                                                                   \
    }                                                                          \
    V_POINT[axis] = 0;                                                         \
    START += rray_slice_offset(v_axes, axis, 0);                               \
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
  r_ssize start =                                                              \
    rray_slice_start(v_axes, v_point, dimensionality, size, false);            \
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
    const r_ssize start =                                                      \
      rray_slice_start(v_axes, v_point, dimensionality, size, true);           \
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
  const int* v_run_locations = v_axes[0].v_locations;                          \
  const r_ssize run_size = v_dimensions[0];                                    \
  r_ssize run_start = 0;                                                       \
                                                                               \
  int v_point[RRAY_MAX_DIMENSIONALITY] = {0};                                  \
                                                                               \
  if (v_axes[0].identity) {                                                    \
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
  const struct rray_slice_axis* v_axes,
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
  const struct rray_slice_axis* v_axes,
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
  const struct rray_slice_axis* v_axes,
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
  const struct rray_slice_axis* v_axes,
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
  const struct rray_slice_axis* v_axes,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
) {
  RRAY_SLICE_ATOMIC(R_TYPE_raw, Rbyte, r_raw_cbegin, r_raw_begin, 0);
}

static r_obj* rray_slice_chr(
  r_obj* x,
  const struct rray_slice_axis* v_axes,
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
  const struct rray_slice_axis* v_axes,
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
