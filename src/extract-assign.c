#include "extract-assign.h"

#include "broadcast.h"
#include "cast.h"
#include "clone.h"
#include "dimensionality.h"
#include "extract-subscript.h"
#include "strides.h"
#include "utils.h"

#include "decl/extract-assign-decl.h"

r_obj* ffi_rray_extract_assign(
  r_obj* ffi_x,
  r_obj* ffi_i,
  r_obj* ffi_value,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_extract_assign(
    ffi_x,
    ffi_i,
    ffi_value,
    rray_args.x,
    rray_args.i,
    rray_args.value,
    error_call
  );
}

r_obj* rray_extract_assign(
  r_obj* x,
  r_obj* i,
  r_obj* value,
  struct rray_arg* x_arg,
  struct rray_arg* i_arg,
  struct rray_arg* value_arg,
  struct r_lazy error_call
) {
  check_unclassed(x, x_arg, error_call);
  x = KEEP(arg_as_array(x, x_arg, error_call));

  r_obj* dimensions = r_dim(x);
  const int* v_dimensions = r_int_cbegin(dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(dimensions);
  check_dimensionality(dimensionality);

  const struct rray_subscript subscript = rray_as_extract_subscript(
    i,
    v_dimensions,
    dimensionality,
    i_arg,
    error_call
  );
  KEEP(subscript.index);

  value = KEEP(rray_cast(value, x, value_arg, x_arg, error_call));

  r_obj* value_dimensions = r_dim(value);
  const int* v_value_dimensions = r_int_cbegin(value_dimensions);
  const int value_dimensionality =
    rray_dimensionality_from_dimensions(value_dimensions);

  const int size = r_ssize_as_integer(subscript.size);

  check_broadcastable(
    v_value_dimensions,
    value_dimensionality,
    &size,
    1,
    value_arg,
    error_call
  );

  r_obj* out = KEEP(r_clone_data(x));
  r_attrib_poke_dim(out, dimensions);

  r_obj* names = r_dim_names(x);

  if (names != r_null) {
    r_attrib_poke_dim_names(out, names);
  }

  switch (r_typeof(out)) {
  case R_TYPE_logical:
    rray_extract_assign_lgl(out, subscript, value);
    break;
  case R_TYPE_integer:
    rray_extract_assign_int(out, subscript, value);
    break;
  case R_TYPE_double:
    rray_extract_assign_dbl(out, subscript, value);
    break;
  case R_TYPE_complex:
    rray_extract_assign_cpl(out, subscript, value);
    break;
  case R_TYPE_raw:
    rray_extract_assign_raw(out, subscript, value);
    break;
  case R_TYPE_character:
    rray_extract_assign_chr(out, subscript, value);
    break;
  case R_TYPE_list:
    rray_extract_assign_list(out, subscript, value);
    break;
  default:
    r_stop_unreachable();
  }

  FREE(4);
  return out;
}

#define RRAY_EXTRACT_ASSIGN_LOCATIONS_LOOP(                                    \
  POKE,                                                                        \
  INDEX_CTYPE,                                                                 \
  INDEX_CONST_DEREF,                                                           \
  IS_MISSING                                                                   \
)                                                                              \
  do {                                                                         \
    const INDEX_CTYPE* v_index = INDEX_CONST_DEREF(subscript.index);           \
    const r_ssize value_step = r_length(value) == 1 ? 0 : 1;                   \
                                                                               \
    for (r_ssize i = 0; i < subscript.size; ++i) {                             \
      const INDEX_CTYPE location = v_index[i];                                 \
                                                                               \
      if (IS_MISSING(location)) {                                              \
        continue;                                                              \
      }                                                                        \
                                                                               \
      POKE(out, (r_ssize) location - 1, v_value[i * value_step]);              \
    }                                                                          \
  } while (0)

#define RRAY_EXTRACT_ASSIGN_MASK_LOOP(POKE)                                    \
  do {                                                                         \
    const int* v_index = r_lgl_cbegin(subscript.index);                        \
    const r_ssize index_step = r_length(subscript.index) == 1 ? 0 : 1;         \
    const r_ssize value_step = r_length(value) == 1 ? 0 : 1;                   \
                                                                               \
    r_ssize i = 0;                                                             \
    r_ssize location = 0;                                                      \
                                                                               \
    while (i < subscript.size) {                                               \
      const int elt = v_index[location * index_step];                          \
                                                                               \
      if (elt != 0) {                                                          \
        if (elt != r_globals.na_lgl) {                                         \
          POKE(out, location, v_value[i * value_step]);                        \
        }                                                                      \
        ++i;                                                                   \
      }                                                                        \
      ++location;                                                              \
    }                                                                          \
  } while (0)

#define RRAY_EXTRACT_ASSIGN_POINTS_LOOP(                                       \
  POKE,                                                                        \
  INDEX_CTYPE,                                                                 \
  INDEX_CONST_DEREF,                                                           \
  POINT_TO_LOCATION                                                            \
)                                                                              \
  do {                                                                         \
    const INDEX_CTYPE* v_index = INDEX_CONST_DEREF(subscript.index);           \
    const r_ssize value_step = r_length(value) == 1 ? 0 : 1;                   \
                                                                               \
    r_obj* dimensions = r_dim(out);                                            \
    const int* v_dimensions = r_int_cbegin(dimensions);                        \
    const int dimensionality =                                                 \
      rray_dimensionality_from_dimensions(dimensions);                         \
                                                                               \
    r_ssize v_strides[RRAY_MAX_DIMENSIONALITY];                                \
    rray_fill_strides_from_dimensions(                                         \
      v_dimensions,                                                            \
      dimensionality,                                                          \
      v_strides                                                                \
    );                                                                         \
                                                                               \
    for (r_ssize i = 0; i < subscript.size; ++i) {                             \
      const r_ssize location = POINT_TO_LOCATION(                              \
        v_index,                                                               \
        i,                                                                     \
        subscript.size,                                                        \
        v_strides,                                                             \
        dimensionality                                                         \
      );                                                                       \
      if (location != -1) {                                                    \
        POKE(out, location, v_value[i * value_step]);                          \
      }                                                                        \
    }                                                                          \
  } while (0)

#define RRAY_EXTRACT_ASSIGN_ITERATE(POKE)                                      \
  switch (subscript.kind) {                                                    \
  case RRAY_SUBSCRIPT_KIND_locations_int:                                      \
    RRAY_EXTRACT_ASSIGN_LOCATIONS_LOOP(                                        \
      POKE,                                                                    \
      int,                                                                     \
      r_int_cbegin,                                                            \
      rray_location_is_missing_int                                             \
    );                                                                         \
    break;                                                                     \
  case RRAY_SUBSCRIPT_KIND_locations_dbl:                                      \
    RRAY_EXTRACT_ASSIGN_LOCATIONS_LOOP(                                        \
      POKE,                                                                    \
      double,                                                                  \
      r_dbl_cbegin,                                                            \
      rray_location_is_missing_dbl                                             \
    );                                                                         \
    break;                                                                     \
  case RRAY_SUBSCRIPT_KIND_mask:                                               \
    RRAY_EXTRACT_ASSIGN_MASK_LOOP(POKE);                                       \
    break;                                                                     \
  case RRAY_SUBSCRIPT_KIND_points_int:                                         \
    RRAY_EXTRACT_ASSIGN_POINTS_LOOP(                                           \
      POKE,                                                                    \
      int,                                                                     \
      r_int_cbegin,                                                            \
      rray_point_to_location_int                                               \
    );                                                                         \
    break;                                                                     \
  case RRAY_SUBSCRIPT_KIND_points_dbl:                                         \
    RRAY_EXTRACT_ASSIGN_POINTS_LOOP(                                           \
      POKE,                                                                    \
      double,                                                                  \
      r_dbl_cbegin,                                                            \
      rray_point_to_location_dbl                                               \
    );                                                                         \
    break;                                                                     \
  }

#define RRAY_EXTRACT_ASSIGN_ATOMIC_POKE(OUT, LOCATION, VALUE)                  \
  v_out[LOCATION] = (VALUE)

#define RRAY_EXTRACT_ASSIGN_ATOMIC(CTYPE, CONST_DEREF, DEREF)                  \
  const CTYPE* v_value = CONST_DEREF(value);                                   \
  CTYPE* v_out = DEREF(out);                                                   \
                                                                               \
  RRAY_EXTRACT_ASSIGN_ITERATE(RRAY_EXTRACT_ASSIGN_ATOMIC_POKE);

#define RRAY_EXTRACT_ASSIGN_BARRIER(CONST_DEREF, POKE)                         \
  r_obj* const* v_value = CONST_DEREF(value);                                  \
                                                                               \
  RRAY_EXTRACT_ASSIGN_ITERATE(POKE);

static void rray_extract_assign_lgl(
  r_obj* out,
  struct rray_subscript subscript,
  r_obj* value
) {
  RRAY_EXTRACT_ASSIGN_ATOMIC(int, r_lgl_cbegin, r_lgl_begin);
}

static void rray_extract_assign_int(
  r_obj* out,
  struct rray_subscript subscript,
  r_obj* value
) {
  RRAY_EXTRACT_ASSIGN_ATOMIC(int, r_int_cbegin, r_int_begin);
}

static void rray_extract_assign_dbl(
  r_obj* out,
  struct rray_subscript subscript,
  r_obj* value
) {
  RRAY_EXTRACT_ASSIGN_ATOMIC(double, r_dbl_cbegin, r_dbl_begin);
}

static void rray_extract_assign_cpl(
  r_obj* out,
  struct rray_subscript subscript,
  r_obj* value
) {
  RRAY_EXTRACT_ASSIGN_ATOMIC(r_complex, r_cpl_cbegin, r_cpl_begin);
}

static void rray_extract_assign_raw(
  r_obj* out,
  struct rray_subscript subscript,
  r_obj* value
) {
  RRAY_EXTRACT_ASSIGN_ATOMIC(Rbyte, r_raw_cbegin, r_raw_begin);
}

static void rray_extract_assign_chr(
  r_obj* out,
  struct rray_subscript subscript,
  r_obj* value
) {
  RRAY_EXTRACT_ASSIGN_BARRIER(r_chr_cbegin, r_chr_poke);
}

static void rray_extract_assign_list(
  r_obj* out,
  struct rray_subscript subscript,
  r_obj* value
) {
  RRAY_EXTRACT_ASSIGN_BARRIER(r_list_cbegin, r_list_poke);
}

#undef RRAY_EXTRACT_ASSIGN_LOCATIONS_LOOP
#undef RRAY_EXTRACT_ASSIGN_MASK_LOOP
#undef RRAY_EXTRACT_ASSIGN_POINTS_LOOP
#undef RRAY_EXTRACT_ASSIGN_ITERATE
#undef RRAY_EXTRACT_ASSIGN_ATOMIC_POKE
#undef RRAY_EXTRACT_ASSIGN_ATOMIC
#undef RRAY_EXTRACT_ASSIGN_BARRIER
