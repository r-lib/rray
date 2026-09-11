#include "broadcast.h"

#include "broadcast-names.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "gather.h"
#include "iterator.h"
#include "size.h"
#include "utils.h"

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
  rray_iterator_init(
    &it,
    size,
    v_dimensions,
    dimensionality,
    v_x_dimensions,
    x_dimensionality
  );

  r_obj* out = KEEP(rray_gather(x, size, &it));
  r_attrib_poke_dim(out, dimensions);

  r_obj* out_names = KEEP(rray_broadcast_names(x, dimensions));

  if (out_names != r_null) {
    r_attrib_poke_dim_names(out, out_names);
  }

  FREE(5);
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
  struct rray_arg* x_arg = new_subscript_arg(NULL, xs_names, n, &i);
  KEEP(x_arg->shelter);

  for (; i < n; ++i) {
    r_list_poke(out, i, rray_broadcast(v_xs[i], dimensions, x_arg, error_call));
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
