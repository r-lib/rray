#include "broadcast-iterator.h"
#include "broadcast.h"
#include "capacity.h"
#include "dimension-sizes.h"
#include "dimensionality.h"
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
  r_obj* dimension_sizes,
  struct r_lazy error_call
) {
  check_dimension_sizes(dimension_sizes, error_call);

  r_obj* x_dimension_sizes = KEEP(rray_dimension_sizes(x, error_call));

  const int* v_x_dimension_sizes = r_int_cbegin(x_dimension_sizes);
  const int* v_dimension_sizes = r_int_cbegin(dimension_sizes);

  const r_ssize x_dimensionality =
    rray_dimensionality_from_dimension_sizes(x_dimension_sizes);
  const r_ssize dimensionality =
    rray_dimensionality_from_dimension_sizes(dimension_sizes);

  check_broadcastable(
    v_x_dimension_sizes,
    x_dimensionality,
    v_dimension_sizes,
    dimensionality,
    error_call
  );

  const r_ssize capacity =
    rray_capacity_from_dimension_sizes(v_dimension_sizes, dimensionality);

  r_obj* out = KEEP(r_alloc_vector(RRAY_R_TYPE, capacity));
  r_attrib_poke_dim(out, dimension_sizes);

  struct rray_broadcast_iterator it;
  rray_broadcast_iterator_init(
    &it,
    v_x_dimension_sizes,
    x_dimensionality,
    v_dimension_sizes,
    dimensionality
  );

  RRAY_X_CONST_DEREF
  RRAY_OUT_DEREF

  for (r_ssize i = 0; i < capacity; ++i) {
    RRAY_ASSIGN(i, it.flat_location);
    rray_broadcast_iterator_next(&it);
  }

  FREE(2);
  return out;
}

#undef RRAY_TYPE
#undef RRAY_FN
#undef RRAY_R_TYPE
#undef RRAY_X_CONST_DEREF
#undef RRAY_OUT_DEREF
#undef RRAY_ASSIGN
