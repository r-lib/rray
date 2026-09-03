#include "ptype.h"

#include "utils.h"

#include "decl/ptype-decl.h"

r_obj* ffi_rray_ptype2(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};

  check_unclassed(ffi_x, rray_args.x, error_call);
  check_unclassed(ffi_y, rray_args.y, error_call);

  const enum r_type x = arg_as_ptype(ffi_x, rray_args.x, error_call);
  const enum r_type y = arg_as_ptype(ffi_y, rray_args.y, error_call);

  const enum r_type out =
    rray_ptype2(x, y, rray_args.x, rray_args.y, error_call);

  return r_alloc_vector(out, 0);
}

enum r_type rray_ptype2(
  enum r_type x,
  enum r_type y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  if (x == y) {
    return x;
  }

  const int x_rank = rray_ptype_rank(x);
  const int y_rank = rray_ptype_rank(y);

  if (x_rank == -1 || y_rank == -1) {
    stop_incompatible_ptype(x, y, x_arg, y_arg, error_call);
  }

  return (x_rank > y_rank) ? x : y;
}

r_obj* ffi_rray_ptype_common(r_obj* ffi_xs, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const enum r_type out = rray_ptype_common(ffi_xs, error_call);
  return r_alloc_vector(out, 0);
}

enum r_type rray_ptype_common(r_obj* xs, struct r_lazy error_call) {
  const r_ssize n = r_length(xs);

  if (n == 0) {
    r_abort_lazy_call(error_call, "Must supply at least one array to `...`.");
  }

  r_obj* const* v_xs = r_list_cbegin(xs);
  r_obj* xs_names = KEEP(r_names(xs));

  r_ssize x_i = 0;
  struct rray_arg* p_x_arg = new_subscript_arg(NULL, xs_names, n, &x_i);
  KEEP(p_x_arg->shelter);

  r_ssize out_i = 0;
  struct rray_arg* p_out_arg = new_subscript_arg(NULL, xs_names, n, &out_i);
  KEEP(p_out_arg->shelter);

  check_unclassed(v_xs[0], p_x_arg, error_call);
  enum r_type out = arg_as_ptype(v_xs[0], p_x_arg, error_call);

  for (x_i = 1; x_i < n; ++x_i) {
    r_obj* x = v_xs[x_i];

    check_unclassed(x, p_x_arg, error_call);
    const enum r_type type = arg_as_ptype(x, p_x_arg, error_call);

    const enum r_type common =
      rray_ptype2(out, type, p_out_arg, p_x_arg, error_call);

    if (common != out) {
      out = common;
      out_i = x_i;
    }
  }

  FREE(3);
  return out;
}

static int rray_ptype_rank(enum r_type type) {
  switch (type) {
  case R_TYPE_logical:
    return 0;
  case R_TYPE_integer:
    return 1;
  case R_TYPE_double:
    return 2;
  case R_TYPE_complex:
    return 3;
  default:
    return -1;
  }
}

static r_no_return void stop_incompatible_ptype(
  enum r_type x,
  enum r_type y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  r_abort_lazy_call(
    error_call,
    "Can't combine %s <%s> and %s <%s>.",
    rray_arg_format(x_arg),
    r_type_as_c_string(x),
    rray_arg_format(y_arg),
    r_type_as_c_string(y)
  );
}
