#include "extract.h"

#include <math.h>

#include "dimensionality.h"
#include "extract-subscript.h"
#include "strides.h"
#include "utils.h"

#include "decl/extract-decl.h"

r_obj* ffi_rray_extract(r_obj* ffi_x, r_obj* ffi_i, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_extract(ffi_x, ffi_i, rray_args.x, rray_args.i, error_call);
}

r_obj* rray_extract(
  r_obj* x,
  r_obj* i,
  struct rray_arg* x_arg,
  struct rray_arg* i_arg,
  struct r_lazy error_call
) {
  check_unclassed(x, x_arg, error_call);
  x = KEEP(arg_as_array(x, x_arg, error_call));

  r_obj* dimensions = r_dim(x);
  const int* v_dimensions = r_int_cbegin(dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(dimensions);
  check_dimensionality(dimensionality);

  const struct rray_extract_subscript subscript = rray_as_extract_subscript(
    i,
    v_dimensions,
    dimensionality,
    i_arg,
    error_call
  );
  KEEP(subscript.index);

  r_obj* out;

  switch (r_typeof(x)) {
  case R_TYPE_logical:
    out = rray_extract_lgl(x, subscript, v_dimensions, dimensionality);
    break;
  case R_TYPE_integer:
    out = rray_extract_int(x, subscript, v_dimensions, dimensionality);
    break;
  case R_TYPE_double:
    out = rray_extract_dbl(x, subscript, v_dimensions, dimensionality);
    break;
  case R_TYPE_complex:
    out = rray_extract_cpl(x, subscript, v_dimensions, dimensionality);
    break;
  case R_TYPE_raw:
    out = rray_extract_raw(x, subscript, v_dimensions, dimensionality);
    break;
  case R_TYPE_character:
    out = rray_extract_chr(x, subscript, v_dimensions, dimensionality);
    break;
  case R_TYPE_list:
    out = rray_extract_list(x, subscript, v_dimensions, dimensionality);
    break;
  default:
    r_stop_unreachable();
  }

  KEEP(out);
  r_attrib_poke_dim(out, r_int(r_ssize_as_integer(subscript.size)));

  FREE(3);
  return out;
}

static inline r_ssize rray_location_offset_int(int location) {
  return location == r_globals.na_int ? -1 : (r_ssize) location - 1;
}

static inline r_ssize rray_location_offset_dbl(double location) {
  return isnan(location) ? -1 : (r_ssize) location - 1;
}

static inline r_ssize rray_point_offset_int(
  const int* v_point,
  r_ssize size,
  const r_ssize* v_strides,
  int dimensionality
) {
  r_ssize out = 0;

  for (int axis = 0; axis < dimensionality; ++axis) {
    const int coordinate = v_point[axis * size];

    if (coordinate == r_globals.na_int) {
      return -1;
    }

    out += (r_ssize) (coordinate - 1) * v_strides[axis];
  }

  return out;
}

static inline r_ssize rray_point_offset_dbl(
  const double* v_point,
  r_ssize size,
  const r_ssize* v_strides,
  int dimensionality
) {
  r_ssize out = 0;

  for (int axis = 0; axis < dimensionality; ++axis) {
    const double coordinate = v_point[axis * size];

    if (isnan(coordinate)) {
      return -1;
    }

    out += ((r_ssize) coordinate - 1) * v_strides[axis];
  }

  return out;
}

#define RRAY_EXTRACT_LOCATIONS_LOOP(                                           \
  POKE,                                                                        \
  MISSING,                                                                     \
  INDEX_CTYPE,                                                                 \
  INDEX_CONST_DEREF,                                                           \
  OFFSET                                                                       \
)                                                                              \
  do {                                                                         \
    const INDEX_CTYPE* v_index = INDEX_CONST_DEREF(subscript.index);           \
                                                                               \
    for (r_ssize i = 0; i < subscript.size; ++i) {                             \
      const r_ssize offset = OFFSET(v_index[i]);                               \
      POKE(out, i, offset == -1 ? MISSING : v_x[offset]);                      \
    }                                                                          \
  } while (0)

#define RRAY_EXTRACT_ATOMIC_MASK_LOOP(POKE, MISSING)                           \
  do {                                                                         \
    const int* v_index = r_lgl_cbegin(subscript.index);                        \
    const r_ssize index_step = r_length(subscript.index) == 1 ? 0 : 1;         \
                                                                               \
    r_ssize i = 0;                                                             \
                                                                               \
    for (r_ssize offset = 0; i < subscript.size; ++offset) {                   \
      const int elt = v_index[offset * index_step];                            \
      POKE(out, i, elt == r_globals.na_lgl ? MISSING : v_x[offset]);           \
      i += elt != 0;                                                           \
    }                                                                          \
  } while (0)

#define RRAY_EXTRACT_BARRIER_MASK_LOOP(POKE, MISSING)                          \
  do {                                                                         \
    const int* v_index = r_lgl_cbegin(subscript.index);                        \
    const r_ssize index_step = r_length(subscript.index) == 1 ? 0 : 1;         \
    const r_ssize x_size = r_length(x);                                        \
                                                                               \
    r_ssize i = 0;                                                             \
                                                                               \
    for (r_ssize offset = 0; offset < x_size; ++offset) {                      \
      const int elt = v_index[offset * index_step];                            \
                                                                               \
      if (elt == 0) {                                                          \
        continue;                                                              \
      }                                                                        \
                                                                               \
      POKE(out, i, elt == r_globals.na_lgl ? MISSING : v_x[offset]);           \
      ++i;                                                                     \
    }                                                                          \
  } while (0)

#define RRAY_EXTRACT_POINTS_LOOP(                                              \
  POKE,                                                                        \
  MISSING,                                                                     \
  INDEX_CTYPE,                                                                 \
  INDEX_CONST_DEREF,                                                           \
  OFFSET                                                                       \
)                                                                              \
  do {                                                                         \
    const INDEX_CTYPE* v_index = INDEX_CONST_DEREF(subscript.index);           \
                                                                               \
    r_ssize v_strides[RRAY_MAX_DIMENSIONALITY];                                \
    rray_fill_strides_from_dimensions(                                         \
      v_dimensions,                                                            \
      dimensionality,                                                          \
      v_strides                                                                \
    );                                                                         \
                                                                               \
    for (r_ssize i = 0; i < subscript.size; ++i) {                             \
      const r_ssize offset =                                                   \
        OFFSET(v_index + i, subscript.size, v_strides, dimensionality);        \
      POKE(out, i, offset == -1 ? MISSING : v_x[offset]);                      \
    }                                                                          \
  } while (0)

#define RRAY_EXTRACT_ITERATE(POKE, MISSING, MASK_LOOP)                         \
  switch (subscript.kind) {                                                    \
  case RRAY_EXTRACT_SUBSCRIPT_KIND_locations_int:                              \
    RRAY_EXTRACT_LOCATIONS_LOOP(                                               \
      POKE,                                                                    \
      MISSING,                                                                 \
      int,                                                                     \
      r_int_cbegin,                                                            \
      rray_location_offset_int                                                 \
    );                                                                         \
    break;                                                                     \
  case RRAY_EXTRACT_SUBSCRIPT_KIND_locations_dbl:                              \
    RRAY_EXTRACT_LOCATIONS_LOOP(                                               \
      POKE,                                                                    \
      MISSING,                                                                 \
      double,                                                                  \
      r_dbl_cbegin,                                                            \
      rray_location_offset_dbl                                                 \
    );                                                                         \
    break;                                                                     \
  case RRAY_EXTRACT_SUBSCRIPT_KIND_mask:                                       \
    MASK_LOOP(POKE, MISSING);                                                  \
    break;                                                                     \
  case RRAY_EXTRACT_SUBSCRIPT_KIND_points_int:                                 \
    RRAY_EXTRACT_POINTS_LOOP(                                                  \
      POKE,                                                                    \
      MISSING,                                                                 \
      int,                                                                     \
      r_int_cbegin,                                                            \
      rray_point_offset_int                                                    \
    );                                                                         \
    break;                                                                     \
  case RRAY_EXTRACT_SUBSCRIPT_KIND_points_dbl:                                 \
    RRAY_EXTRACT_POINTS_LOOP(                                                  \
      POKE,                                                                    \
      MISSING,                                                                 \
      double,                                                                  \
      r_dbl_cbegin,                                                            \
      rray_point_offset_dbl                                                    \
    );                                                                         \
    break;                                                                     \
  }

#define RRAY_EXTRACT_ATOMIC_POKE(OUT, I, VALUE) v_out[I] = (VALUE)

#define RRAY_EXTRACT_ATOMIC(RTYPE, CTYPE, CONST_DEREF, DEREF, MISSING)         \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, subscript.size));                    \
  CTYPE* v_out = DEREF(out);                                                   \
                                                                               \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
                                                                               \
  RRAY_EXTRACT_ITERATE(                                                        \
    RRAY_EXTRACT_ATOMIC_POKE,                                                  \
    MISSING,                                                                   \
    RRAY_EXTRACT_ATOMIC_MASK_LOOP                                              \
  );                                                                           \
                                                                               \
  FREE(1);                                                                     \
  return out;

#define RRAY_EXTRACT_BARRIER(RTYPE, CONST_DEREF, POKE, MISSING)                \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, subscript.size));                    \
                                                                               \
  r_obj* const* v_x = CONST_DEREF(x);                                          \
                                                                               \
  RRAY_EXTRACT_ITERATE(POKE, MISSING, RRAY_EXTRACT_BARRIER_MASK_LOOP);         \
                                                                               \
  FREE(1);                                                                     \
  return out;

static r_obj* rray_extract_lgl(
  r_obj* x,
  struct rray_extract_subscript subscript,
  const int* v_dimensions,
  int dimensionality
) {
  RRAY_EXTRACT_ATOMIC(
    R_TYPE_logical,
    int,
    r_lgl_cbegin,
    r_lgl_begin,
    r_globals.na_lgl
  );
}

static r_obj* rray_extract_int(
  r_obj* x,
  struct rray_extract_subscript subscript,
  const int* v_dimensions,
  int dimensionality
) {
  RRAY_EXTRACT_ATOMIC(
    R_TYPE_integer,
    int,
    r_int_cbegin,
    r_int_begin,
    r_globals.na_int
  );
}

static r_obj* rray_extract_dbl(
  r_obj* x,
  struct rray_extract_subscript subscript,
  const int* v_dimensions,
  int dimensionality
) {
  RRAY_EXTRACT_ATOMIC(
    R_TYPE_double,
    double,
    r_dbl_cbegin,
    r_dbl_begin,
    r_globals.na_dbl
  );
}

static r_obj* rray_extract_cpl(
  r_obj* x,
  struct rray_extract_subscript subscript,
  const int* v_dimensions,
  int dimensionality
) {
  RRAY_EXTRACT_ATOMIC(
    R_TYPE_complex,
    r_complex,
    r_cpl_cbegin,
    r_cpl_begin,
    r_globals.na_cpl
  );
}

static r_obj* rray_extract_raw(
  r_obj* x,
  struct rray_extract_subscript subscript,
  const int* v_dimensions,
  int dimensionality
) {
  RRAY_EXTRACT_ATOMIC(R_TYPE_raw, Rbyte, r_raw_cbegin, r_raw_begin, 0);
}

static r_obj* rray_extract_chr(
  r_obj* x,
  struct rray_extract_subscript subscript,
  const int* v_dimensions,
  int dimensionality
) {
  RRAY_EXTRACT_BARRIER(
    R_TYPE_character,
    r_chr_cbegin,
    r_chr_poke,
    r_globals.na_str
  );
}

static r_obj* rray_extract_list(
  r_obj* x,
  struct rray_extract_subscript subscript,
  const int* v_dimensions,
  int dimensionality
) {
  RRAY_EXTRACT_BARRIER(R_TYPE_list, r_list_cbegin, r_list_poke, r_null);
}

#undef RRAY_EXTRACT_LOCATIONS_LOOP
#undef RRAY_EXTRACT_ATOMIC_MASK_LOOP
#undef RRAY_EXTRACT_BARRIER_MASK_LOOP
#undef RRAY_EXTRACT_POINTS_LOOP
#undef RRAY_EXTRACT_ITERATE
#undef RRAY_EXTRACT_ATOMIC_POKE
#undef RRAY_EXTRACT_ATOMIC
#undef RRAY_EXTRACT_BARRIER
