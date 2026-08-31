#include "broadcast.h"

#include "broadcast-iterator.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "names.h"
#include "size.h"
#include "utils.h"

#include "decl/broadcast-decl.h"

r_obj* ffi_rray_broadcast(
  r_obj* ffi_x,
  r_obj* ffi_dimensions,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_broadcast(ffi_x, ffi_dimensions, error_call);
}

r_obj* rray_broadcast(r_obj* x, r_obj* dimensions, struct r_lazy error_call) {
  check_unclassed(x, "x", error_call);
  x = KEEP(arg_as_array(x, "x", error_call));

  dimensions = KEEP(arg_as_dimensions(dimensions, dimensions_chr, error_call));

  r_obj* x_dimensions = KEEP(rray_dimensions(x, error_call));

  const int* v_x_dimensions = r_int_cbegin(x_dimensions);
  const int* v_dimensions = r_int_cbegin(dimensions);

  const int x_dimensionality =
    rray_dimensionality_from_dimensions(x_dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(dimensions);

  const bool dimensions_are_equal = rray_dimensions_are_equal(
    v_x_dimensions,
    x_dimensionality,
    v_dimensions,
    dimensionality
  );

  if (dimensions_are_equal) {
    FREE(3);
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

  r_obj* out = KEEP(r_alloc_vector(r_typeof(x), size));
  r_attrib_poke_dim(out, dimensions);

  struct rray_iterator it;
  rray_broadcast_iterator_init(
    &it,
    v_x_dimensions,
    x_dimensionality,
    v_dimensions,
    dimensionality
  );

  switch (r_typeof(x)) {
  case R_TYPE_logical:
    rray_broadcast_lgl(x, out, &it);
    break;
  case R_TYPE_integer:
    rray_broadcast_int(x, out, &it);
    break;
  case R_TYPE_double:
    rray_broadcast_dbl(x, out, &it);
    break;
  case R_TYPE_complex:
    rray_broadcast_cpl(x, out, &it);
    break;
  case R_TYPE_raw:
    rray_broadcast_raw(x, out, &it);
    break;
  case R_TYPE_character:
    rray_broadcast_chr(x, out, &it);
    break;
  case R_TYPE_list:
    rray_broadcast_list(x, out, &it);
    break;
  default:
    r_stop_unreachable();
  }

  r_obj* x_names = rray_names(x, error_call);
  if (x_names != r_null) {
    KEEP(x_names);
    r_obj* const* v_x_names = r_list_cbegin(x_names);

    r_obj* out_names = KEEP(rray_broadcast_names(
      v_x_names,
      v_x_dimensions,
      x_dimensionality,
      v_dimensions,
      dimensionality
    ));

    if (out_names != r_null) {
      r_attrib_poke_dim_names(out, out_names);
    }

    FREE(2);
  }

  FREE(4);
  return out;
}

#define RRAY_BROADCAST_ATOMIC(CTYPE, CONST_DEREF, DEREF)                       \
  const r_ssize size = r_length(out);                                          \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
  CTYPE* v_out = DEREF(out);                                                   \
                                                                               \
  for (r_ssize i = 0; i < size; ++i) {                                         \
    v_out[i] = v_x[rray_iterator_location(it)];                                \
    rray_iterator_next(it);                                                    \
  }

#define RRAY_BROADCAST_BARRIER(CONST_DEREF, POKE)                              \
  const r_ssize size = r_length(out);                                          \
  r_obj* const* v_x = CONST_DEREF(x);                                          \
                                                                               \
  for (r_ssize i = 0; i < size; ++i) {                                         \
    POKE(out, i, v_x[rray_iterator_location(it)]);                             \
    rray_iterator_next(it);                                                    \
  }

static void rray_broadcast_lgl(r_obj* x, r_obj* out, struct rray_iterator* it) {
  RRAY_BROADCAST_ATOMIC(int, r_lgl_cbegin, r_lgl_begin);
}

static void rray_broadcast_int(r_obj* x, r_obj* out, struct rray_iterator* it) {
  RRAY_BROADCAST_ATOMIC(int, r_int_cbegin, r_int_begin);
}

static void rray_broadcast_dbl(r_obj* x, r_obj* out, struct rray_iterator* it) {
  RRAY_BROADCAST_ATOMIC(double, r_dbl_cbegin, r_dbl_begin);
}

static void rray_broadcast_cpl(r_obj* x, r_obj* out, struct rray_iterator* it) {
  RRAY_BROADCAST_ATOMIC(r_complex, r_cpl_cbegin, r_cpl_begin);
}

static void rray_broadcast_raw(r_obj* x, r_obj* out, struct rray_iterator* it) {
  RRAY_BROADCAST_ATOMIC(Rbyte, r_raw_cbegin, r_raw_begin);
}

static void rray_broadcast_chr(r_obj* x, r_obj* out, struct rray_iterator* it) {
  RRAY_BROADCAST_BARRIER(r_chr_cbegin, r_chr_poke);
}

static void rray_broadcast_list(
  r_obj* x,
  r_obj* out,
  struct rray_iterator* it
) {
  RRAY_BROADCAST_BARRIER(r_list_cbegin, r_list_poke);
}

#undef RRAY_BROADCAST_ATOMIC
#undef RRAY_BROADCAST_BARRIER

static r_obj* rray_broadcast_names(
  r_obj* const* v_names,
  const int* v_dimensions,
  int dimensionality,
  const int* v_out_dimensions,
  int out_dimensionality
) {
  r_obj* out = r_null;
  r_keep_loc out_loc;
  KEEP_HERE(out, &out_loc);

  for (int i = 0; i < dimensionality; ++i) {
    if (v_names[i] == r_null) {
      // `out` stays `r_null` when there were no names before
      continue;
    }
    if (v_dimensions[i] != v_out_dimensions[i]) {
      // `out` is "cleared" to `r_null` when dimension changes
      continue;
    }
    if (out == r_null) {
      out = r_alloc_list(out_dimensionality);
      KEEP_AT(out, out_loc);
    }
    r_list_poke(out, i, v_names[i]);
  }

  FREE(1);
  return out;
}

void check_broadcastable(
  const int* v_x_dimensions,
  int x_dimensionality,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
) {
  if (x_dimensionality > dimensionality) {
    r_abort_lazy_call(
      error_call,
      "Can't broadcast from dimensionality %d to %d. "
      "Can't decrease dimensionality.",
      x_dimensionality,
      dimensionality
    );
  }

  for (int i = 0; i < x_dimensionality; ++i) {
    const int x_dimension = v_x_dimensions[i];
    const int dimension = v_dimensions[i];

    if (x_dimension == dimension || x_dimension == 1) {
      continue;
    }

    r_abort_lazy_call(
      error_call,
      "Can't broadcast axis %d from dimension %d to %d.",
      i + 1,
      x_dimension,
      dimension
    );
  }
}
