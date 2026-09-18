#include "dimensionality.h"

#include "dimensions.h"

r_obj* ffi_rray_dimensionality(r_obj* ffi_x, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return r_int(rray_dimensionality(ffi_x, rray_args.x, error_call));
}

int rray_dimensionality(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  r_obj* dimensions = rray_dimensions(x, arg, error_call);
  return rray_dimensionality_from_dimensions(dimensions);
}

int rray_dimensionality_from_dimensions(r_obj* dimensions) {
  return (int) r_length(dimensions);
}

int list_max_dimensionality(
  r_obj* xs,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  const r_ssize xs_size = r_length(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);

  r_obj* xs_names = KEEP(r_names(xs));

  r_ssize x_i = 0;
  struct rray_arg* x_arg = new_subscript_arg(arg, xs_names, xs_size, &x_i);
  KEEP(x_arg->shelter);

  int out = 1;

  for (; x_i < xs_size; ++x_i) {
    const int x_dimensionality =
      rray_dimensionality(v_xs[x_i], x_arg, error_call);

    if (x_dimensionality > out) {
      out = x_dimensionality;
    }
  }

  FREE(2);
  return out;
}

void check_max_dimensionality(int dimensionality) {
  if (dimensionality > RRAY_MAX_DIMENSIONALITY) {
    r_abort(
      "rray can't support arrays with a dimensionality greater than %d. "
      "A dimensionality of %d was requested.",
      RRAY_MAX_DIMENSIONALITY,
      dimensionality
    );
  }
}
