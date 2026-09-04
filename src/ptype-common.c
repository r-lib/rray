#include "ptype-common.h"

#include "ptype.h"
#include "utils.h"

r_obj* ffi_rray_ptype_common(
  r_obj* ffi_xs,
  r_obj* ffi_ptype,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_ptype_common(ffi_xs, ffi_ptype, rray_args.dot_ptype, error_call);
}

r_obj* rray_ptype_common(
  r_obj* xs,
  r_obj* ptype,
  struct rray_arg* ptype_arg,
  struct r_lazy error_call
) {
  if (ptype != r_null) {
    check_unclassed(ptype, ptype_arg, error_call);
    return rray_ptype_from_type(arg_as_type(ptype, ptype_arg, error_call));
  }

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
  r_obj* out = rray_ptype_from_type(arg_as_type(v_xs[0], p_x_arg, error_call));

  for (x_i = 1; x_i < n; ++x_i) {
    r_obj* common = rray_ptype2(out, v_xs[x_i], p_out_arg, p_x_arg, error_call);

    if (common != out) {
      out = common;
      out_i = x_i;
    }
  }

  FREE(3);
  return out;
}
