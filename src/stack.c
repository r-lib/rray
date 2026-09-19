#include "stack.h"

#include "axes.h"
#include "combine.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "insert-axes.h"
#include "names.h"
#include "utils.h"

#include "decl/stack-decl.h"

r_obj* ffi_rray_stack(r_obj* ffi_xs, r_obj* ffi_axis, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const int axis = arg_as_int(ffi_axis, rray_args.dot_axis, error_call);
  return rray_stack(
    ffi_xs,
    axis,
    r_null,
    rray_args.empty,
    rray_args.empty,
    error_call
  );
}

r_obj* rray_stack(
  r_obj* xs,
  int axis,
  r_obj* ptype,
  struct rray_arg* arg,
  struct rray_arg* ptype_arg,
  struct r_lazy error_call
) {
  if (r_length(xs) == 0) {
    r_abort_lazy_call(error_call, "Must supply at least one array to `...`.");
  }

  // This does two important things:
  // - Computes dimensions we pull dimensionality from
  // - Throws broadcast errors before axes have been inserted, so the error
  //   message numbering is meaningful
  r_obj* dimensions = KEEP(rray_dimensions_common(xs, r_null, arg, error_call));

  const int dimensionality =
    int_add_checked(rray_dimensionality_from_dimensions(dimensions), 1);
  check_dimensionality(dimensionality);

  check_axis(axis, dimensionality, rray_args.dot_axis, error_call);

  xs = KEEP(rray_stack_prepare(xs, axis, arg, error_call));

  r_obj* out = KEEP(rray_combine(xs, axis, ptype, arg, ptype_arg, error_call));

  r_obj* names = KEEP(r_names(xs));
  if (names != r_null) {
    out = rray_set_axis_names(out, axis, names, arg, error_call);
  }

  FREE(4);
  return out;
}

static r_obj* rray_stack_prepare(
  r_obj* xs,
  int axis,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  const r_ssize xs_size = r_length(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);
  r_obj* xs_names = KEEP(r_names(xs));

  r_obj* out = KEEP(r_alloc_list(xs_size));
  r_attrib_poke_names(out, xs_names);

  r_ssize i = 0;
  struct rray_arg* x_arg = new_subscript_arg(arg, xs_names, xs_size, &i);
  KEEP(x_arg->shelter);

  r_obj* axes = KEEP(r_int(axis));

  for (; i < xs_size; ++i) {
    r_obj* x = v_xs[i];

    if (rray_dimensionality(x, x_arg, error_call) < axis - 1) {
      x = rray_expand_dimensionality(x, axis - 1, x_arg, error_call);
    }
    KEEP(x);

    r_list_poke(out, i, rray_insert_axes(x, axes, x_arg, error_call));
    FREE(1);
  }

  FREE(4);
  return out;
}
