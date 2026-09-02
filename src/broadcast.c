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
  return rray_broadcast(ffi_x, ffi_dimensions, rray_args.x, error_call);
}

r_obj* rray_broadcast(
  r_obj* x,
  r_obj* dimensions,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));

  dimensions =
    KEEP(arg_as_dimensions(dimensions, rray_args.dimensions, error_call));

  r_obj* x_dimensions = KEEP(rray_dimensions(x, arg, error_call));

  const int* v_x_dimensions = r_int_cbegin(x_dimensions);
  const int* v_dimensions = r_int_cbegin(dimensions);

  const int x_dimensionality =
    rray_dimensionality_from_dimensions(x_dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(dimensions);

  if (
    rray_dimensions_are_equal(
      v_x_dimensions,
      x_dimensionality,
      v_dimensions,
      dimensionality
    )
  ) {
    FREE(3);
    return x;
  }

  check_broadcastable(
    v_x_dimensions,
    x_dimensionality,
    v_dimensions,
    dimensionality,
    arg,
    error_call
  );

  const r_ssize size = rray_size_from_dimensions(v_dimensions, dimensionality);

  struct rray_iterator it;
  rray_broadcast_iterator_init(
    &it,
    v_x_dimensions,
    x_dimensionality,
    v_dimensions,
    dimensionality
  );

  r_obj* out;

  switch (r_typeof(x)) {
  case R_TYPE_logical:
    out = rray_broadcast_lgl(x, size, &it);
    break;
  case R_TYPE_integer:
    out = rray_broadcast_int(x, size, &it);
    break;
  case R_TYPE_double:
    out = rray_broadcast_dbl(x, size, &it);
    break;
  case R_TYPE_complex:
    out = rray_broadcast_cpl(x, size, &it);
    break;
  case R_TYPE_raw:
    out = rray_broadcast_raw(x, size, &it);
    break;
  case R_TYPE_character:
    out = rray_broadcast_chr(x, size, &it);
    break;
  case R_TYPE_list:
    out = rray_broadcast_list(x, size, &it);
    break;
  default:
    r_stop_unreachable();
  }

  KEEP(out);
  r_attrib_poke_dim(out, dimensions);

  r_obj* x_names = rray_names(x, arg, error_call);
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

#define RRAY_BROADCAST_ATOMIC(RTYPE, CTYPE, CONST_DEREF, DEREF)                \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, size));                              \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
  CTYPE* v_out = DEREF(out);                                                   \
                                                                               \
  for (r_ssize i = 0; i < size; ++i) {                                         \
    v_out[i] = v_x[rray_iterator_location(it)];                                \
    rray_iterator_next(it);                                                    \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

#define RRAY_BROADCAST_BARRIER(RTYPE, CONST_DEREF, POKE)                       \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, size));                              \
  r_obj* const* v_x = CONST_DEREF(x);                                          \
                                                                               \
  for (r_ssize i = 0; i < size; ++i) {                                         \
    POKE(out, i, v_x[rray_iterator_location(it)]);                             \
    rray_iterator_next(it);                                                    \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

static r_obj* rray_broadcast_lgl(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
) {
  RRAY_BROADCAST_ATOMIC(R_TYPE_logical, int, r_lgl_cbegin, r_lgl_begin);
}

static r_obj* rray_broadcast_int(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
) {
  RRAY_BROADCAST_ATOMIC(R_TYPE_integer, int, r_int_cbegin, r_int_begin);
}

static r_obj* rray_broadcast_dbl(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
) {
  RRAY_BROADCAST_ATOMIC(R_TYPE_double, double, r_dbl_cbegin, r_dbl_begin);
}

static r_obj* rray_broadcast_cpl(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
) {
  RRAY_BROADCAST_ATOMIC(R_TYPE_complex, r_complex, r_cpl_cbegin, r_cpl_begin);
}

static r_obj* rray_broadcast_raw(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
) {
  RRAY_BROADCAST_ATOMIC(R_TYPE_raw, Rbyte, r_raw_cbegin, r_raw_begin);
}

static r_obj* rray_broadcast_chr(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
) {
  RRAY_BROADCAST_BARRIER(R_TYPE_character, r_chr_cbegin, r_chr_poke);
}

static r_obj* rray_broadcast_list(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
) {
  RRAY_BROADCAST_BARRIER(R_TYPE_list, r_list_cbegin, r_list_poke);
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

r_obj* ffi_rray_broadcast_common(
  r_obj* ffi_xs,
  r_obj* ffi_dimensions,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_broadcast_common(ffi_xs, ffi_dimensions, error_call);
}

r_obj* rray_broadcast_common(
  r_obj* xs,
  r_obj* dimensions,
  struct r_lazy error_call
) {
  dimensions = KEEP(rray_dimensions_common(xs, dimensions, error_call));

  const r_ssize n = r_length(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);
  r_obj* xs_names = KEEP(r_names(xs));

  r_obj* out = KEEP(r_alloc_list(n));
  r_attrib_poke_names(out, xs_names);

  r_ssize i = 0;
  struct rray_arg* p_x_arg = new_subscript_arg(NULL, xs_names, n, &i);
  KEEP(p_x_arg->shelter);

  for (; i < n; ++i) {
    r_list_poke(
      out,
      i,
      rray_broadcast(v_xs[i], dimensions, p_x_arg, error_call)
    );
  }

  FREE(4);
  return out;
}

void check_broadcastable(
  const int* v_x_dimensions,
  int x_dimensionality,
  const int* v_dimensions,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  if (x_dimensionality > dimensionality) {
    r_abort_lazy_call(
      error_call,
      "Can't broadcast %s from dimensionality %d to %d. "
      "Can't decrease dimensionality.",
      rray_arg_format(arg),
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
      "Can't broadcast axis %d of %s from dimension %d to %d.",
      i + 1,
      rray_arg_format(arg),
      x_dimension,
      dimension
    );
  }
}
