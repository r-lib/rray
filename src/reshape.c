#include "reshape.h"

#include "dimensions.h"
#include "size.h"
#include "utils.h"
#include "wrapper.h"

r_obj* ffi_rray_reshape(r_obj* x, r_obj* dimensions, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  return rray_reshape(x, dimensions, error_call);
}

r_obj* rray_reshape(r_obj* x, r_obj* dimensions, struct r_lazy error_call) {
  x = KEEP(arg_as_array(x, "x", error_call));
  dimensions = KEEP(arg_as_dimensions(dimensions, error_call));

  const r_ssize x_size = rray_size(x, error_call);

  const r_ssize dimensionality = r_length(dimensions);
  const int* v_dimensions = r_int_cbegin(dimensions);

  const r_ssize size = rray_size_from_dimensions(v_dimensions, dimensionality);

  if (x_size != size) {
    r_abort_lazy_call(
      error_call,
      "Can't reshape to these dimensions. "
      "Can't change from a size of %td to a size of %td.",
      (ptrdiff_t) x_size,
      (ptrdiff_t) size
    );
  }

  r_obj* out = KEEP(r_wrap(x));

  r_attrib_poke_dim_names(out, r_null);
  r_attrib_poke_dim(out, dimensions);

  FREE(3);
  return out;
}
