#include "cast-common.h"

#include "cast.h"
#include "ptype-common.h"

r_obj* ffi_rray_cast_common(r_obj* ffi_xs, r_obj* ffi_to, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_cast_common(ffi_xs, ffi_to, error_call);
}

r_obj* rray_cast_common(r_obj* xs, r_obj* to, struct r_lazy error_call) {
  to = KEEP(rray_ptype_common(xs, to, rray_args.dot_to, error_call));

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
      rray_cast(v_xs[i], to, p_x_arg, rray_args.dot_to, error_call)
    );
  }

  FREE(4);
  return out;
}
