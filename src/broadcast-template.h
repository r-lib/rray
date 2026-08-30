#include "broadcast-iterator.h"
#include "broadcast.h"
#include "decl/broadcast-template-decl.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "names.h"
#include "size.h"
#include "types.h"

#if RRAY_TYPE == RRAY_TYPE_LOGICAL
#define RRAY_FN rray_broadcast_lgl
#define RRAY_R_TYPE R_TYPE_logical
#define RRAY_X_CONST_DEREF const int* v_x = r_lgl_cbegin(x);
#define RRAY_OUT_DEREF int* v_out = r_lgl_begin(out);
#define RRAY_ASSIGN(i, loc) v_out[i] = v_x[loc]
#endif

#if RRAY_TYPE == RRAY_TYPE_INTEGER
#define RRAY_FN rray_broadcast_int
#define RRAY_R_TYPE R_TYPE_integer
#define RRAY_X_CONST_DEREF const int* v_x = r_int_cbegin(x);
#define RRAY_OUT_DEREF int* v_out = r_int_begin(out);
#define RRAY_ASSIGN(i, loc) v_out[i] = v_x[loc]
#endif

#if RRAY_TYPE == RRAY_TYPE_DOUBLE
#define RRAY_FN rray_broadcast_dbl
#define RRAY_R_TYPE R_TYPE_double
#define RRAY_X_CONST_DEREF const double* v_x = r_dbl_cbegin(x);
#define RRAY_OUT_DEREF double* v_out = r_dbl_begin(out);
#define RRAY_ASSIGN(i, loc) v_out[i] = v_x[loc]
#endif

#if RRAY_TYPE == RRAY_TYPE_COMPLEX
#define RRAY_FN rray_broadcast_cpl
#define RRAY_R_TYPE R_TYPE_complex
#define RRAY_X_CONST_DEREF const r_complex* v_x = r_cpl_cbegin(x);
#define RRAY_OUT_DEREF r_complex* v_out = r_cpl_begin(out);
#define RRAY_ASSIGN(i, loc) v_out[i] = v_x[loc]
#endif

#if RRAY_TYPE == RRAY_TYPE_RAW
#define RRAY_FN rray_broadcast_raw
#define RRAY_R_TYPE R_TYPE_raw
#define RRAY_X_CONST_DEREF const Rbyte* v_x = r_raw_cbegin(x);
#define RRAY_OUT_DEREF Rbyte* v_out = r_raw_begin(out);
#define RRAY_ASSIGN(i, loc) v_out[i] = v_x[loc]
#endif

#if RRAY_TYPE == RRAY_TYPE_CHARACTER
#define RRAY_FN rray_broadcast_chr
#define RRAY_R_TYPE R_TYPE_character
#define RRAY_X_CONST_DEREF r_obj* const* v_x = r_chr_cbegin(x);
#define RRAY_OUT_DEREF
#define RRAY_ASSIGN(i, loc) r_chr_poke(out, i, v_x[loc])
#endif

#if RRAY_TYPE == RRAY_TYPE_LIST
#define RRAY_FN rray_broadcast_list
#define RRAY_R_TYPE R_TYPE_list
#define RRAY_X_CONST_DEREF r_obj* const* v_x = r_list_cbegin(x);
#define RRAY_OUT_DEREF
#define RRAY_ASSIGN(i, loc) r_list_poke(out, i, v_x[loc])
#endif

static inline r_obj* RRAY_FN(
  r_obj* x,
  r_obj* dimensions,
  struct r_lazy error_call
) {
  dimensions = KEEP(arg_as_dimensions(dimensions, "dimensions", error_call));

  r_obj* x_dimensions = KEEP(rray_dimensions(x, error_call));

  const int* v_x_dimensions = r_int_cbegin(x_dimensions);
  const int* v_dimensions = r_int_cbegin(dimensions);

  const r_ssize x_dimensionality =
    rray_dimensionality_from_dimensions(x_dimensions);
  const r_ssize dimensionality =
    rray_dimensionality_from_dimensions(dimensions);

  if (
    rray_dimensions_are_equal(
      v_x_dimensions,
      x_dimensionality,
      v_dimensions,
      dimensionality
    )
  ) {
    FREE(2);
    return x;
  }

  check_broadcastable(
    v_x_dimensions,
    x_dimensionality,
    v_dimensions,
    dimensionality,
    error_call
  );

  const r_ssize size = rray_size_from_dimensions(v_dimensions, dimensionality);

  r_obj* out = KEEP(r_alloc_vector(RRAY_R_TYPE, size));
  r_attrib_poke_dim(out, dimensions);

  struct rray_iterator it;
  rray_broadcast_iterator_init(
    &it,
    v_x_dimensions,
    x_dimensionality,
    v_dimensions,
    dimensionality
  );

  RRAY_X_CONST_DEREF
  RRAY_OUT_DEREF

  for (r_ssize i = 0; i < size; ++i) {
    RRAY_ASSIGN(i, rray_iterator_location(&it));
    rray_iterator_next(&it);
  }

  r_obj* x_names = rray_names(x, error_call);
  if (x_names != r_null) {
    KEEP(x_names);
    r_obj* const* v_x_names = r_list_cbegin(x_names);
    r_obj* out_names = rray_broadcast_names(
      v_x_names,
      v_x_dimensions,
      x_dimensionality,
      v_dimensions,
      dimensionality
    );
    if (out_names != r_null) {
      r_attrib_poke_dim_names(out, out_names);
    }
    FREE(1);
  }

  FREE(3);
  return out;
}

#ifndef RRAY_ONCE
#define RRAY_ONCE

r_obj* rray_broadcast_names(
  r_obj* const* v_names,
  const int* v_dimensions,
  r_ssize dimensionality,
  const int* v_out_dimensions,
  r_ssize out_dimensionality
) {
  r_ssize i = 0;

  for (; i < dimensionality; ++i) {
    if (v_names[i] == r_null) {
      // `out` stays `r_null` when there were no names before
      continue;
    }
    if (v_dimensions[i] != v_out_dimensions[i]) {
      // `out` is "cleared" to `r_null` when dimension changes
      continue;
    }
    break;
  }

  if (i == dimensionality) {
    // Return `r_null` if:
    // - All names were `r_null` to begin with
    // - Broadcasting resulted in all `r_null` names
    return r_null;
  }

  // Pick up where we left off, actually assigning this time
  r_obj* out = KEEP(r_alloc_list(out_dimensionality));

  for (; i < dimensionality; ++i) {
    if (v_names[i] == r_null) {
      // `out` stays `r_null` when there were no names before
      continue;
    }
    if (v_dimensions[i] != v_out_dimensions[i]) {
      // `out` is "cleared" to `r_null` when dimension changes
      continue;
    }
    r_list_poke(out, i, v_names[i]);
  }

  FREE(1);
  return out;
}

#endif // RRAY_ONCE

#undef RRAY_TYPE
#undef RRAY_FN
#undef RRAY_R_TYPE
#undef RRAY_X_CONST_DEREF
#undef RRAY_OUT_DEREF
#undef RRAY_ASSIGN
