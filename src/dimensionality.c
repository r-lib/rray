#include "dimensionality.h"

#include "dimensions.h"
#include "utils.h"
#include "wrapper.h"

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

r_obj* rray_expand_dimensionality(
  r_obj* x,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));

  r_obj* x_dimensions = KEEP(rray_dimensions(x, arg, error_call));
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);
  const int x_dimensionality =
    rray_dimensionality_from_dimensions(x_dimensions);

  if (dimensionality <= x_dimensionality) {
    r_stop_internal(
      "`dimensionality` of %d must be greater than the dimensionality of %d.",
      dimensionality,
      x_dimensionality
    );
  }

  check_max_dimensionality(dimensionality);

  r_obj* out_dimensions = KEEP(r_alloc_integer(dimensionality));
  int* v_out_dimensions = r_int_begin(out_dimensions);
  r_memcpy(v_out_dimensions, v_x_dimensions, sizeof(int) * x_dimensionality);

  for (int i = x_dimensionality; i < dimensionality; ++i) {
    v_out_dimensions[i] = 1;
  }

  r_obj* out = KEEP(r_wrap(x));
  r_attrib_poke_dim(out, out_dimensions);

  r_obj* x_names = r_dim_names(x);

  if (x_names != r_null) {
    r_obj* out_names = KEEP(r_alloc_list(dimensionality));
    r_obj* const* v_x_names = r_list_cbegin(x_names);

    for (int i = 0; i < x_dimensionality; ++i) {
      r_list_poke(out_names, i, v_x_names[i]);
    }

    r_attrib_poke_dim_names(out, out_names);
    FREE(1);
  }

  FREE(4);
  return out;
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
