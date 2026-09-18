#include "stack.h"

#include "axes.h"
#include "combine.h"
#include "dimensionality.h"
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

  const int dimensionality =
    int_add_checked(list_max_dimensionality(xs, arg, error_call), 1);
  check_max_dimensionality(dimensionality);

  check_axis(axis, dimensionality, rray_args.dot_axis, error_call);

  xs = KEEP(rray_stack_prepare(xs, axis, arg, error_call));

  r_obj* out = KEEP(rray_combine(xs, axis, ptype, arg, ptype_arg, error_call));

  r_obj* names = KEEP(r_names(xs));

  if (names != r_null) {
    out = rray_set_axis_names(out, axis, names, arg, error_call);
  }

  FREE(3);
  return out;
}

static r_obj* rray_stack_prepare(
  r_obj* xs,
  int axis,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  const r_ssize n = r_length(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);
  r_obj* xs_names = KEEP(r_names(xs));

  r_obj* out = KEEP(r_alloc_list(n));
  r_attrib_poke_names(out, xs_names);

  r_ssize i = 0;
  struct rray_arg* x_arg = new_subscript_arg(arg, xs_names, n, &i);
  KEEP(x_arg->shelter);

  r_obj* axes = KEEP(r_int(axis));

  for (; i < n; ++i) {
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
