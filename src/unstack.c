#include "unstack.h"

#include "axes.h"
#include "dimensionality.h"
#include "names.h"
#include "remove-axes.h"
#include "split.h"
#include "utils.h"

r_obj* ffi_rray_unstack(r_obj* ffi_x, r_obj* ffi_axis, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const int axis = arg_as_int(ffi_axis, rray_args.axis, error_call);
  return rray_unstack(ffi_x, axis, rray_args.x, error_call);
}

r_obj* rray_unstack(
  r_obj* x,
  int axis,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));

  const int dimensionality = rray_dimensionality(x, arg, error_call);
  check_dimensionality(dimensionality);

  if (dimensionality < 2) {
    r_abort_lazy_call(
      error_call,
      "%s must have a dimensionality of at least 2, not %d.",
      rray_arg_format(arg),
      dimensionality
    );
  }

  check_axis(axis, dimensionality, rray_args.axis, error_call);

  r_obj* dimensions = KEEP(r_int(1));
  r_obj* out = KEEP(rray_split(x, axis, dimensions, arg, error_call));

  r_obj* axes = KEEP(r_int(axis));
  const r_ssize out_size = r_length(out);

  for (r_ssize i = 0; i < out_size; ++i) {
    r_obj* elt = r_list_get(out, i);
    r_list_poke(out, i, rray_remove_axes(elt, axes, arg, error_call));
  }

  r_attrib_poke_names(out, rray_axis_names(x, axis, arg, error_call));

  FREE(4);
  return out;
}
