#include "reshape.h"

#include "dimension-sizes.h"
#include "size.h"
#include "utils.h"
#include "wrapper.h"

r_obj* ffi_rray_reshape(r_obj* x, r_obj* dimension_sizes, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  return rray_reshape(x, dimension_sizes, error_call);
}

r_obj* rray_reshape(
  r_obj* x,
  r_obj* dimension_sizes,
  struct r_lazy error_call
) {
  x = KEEP(arg_as_array(x, "x", error_call));
  dimension_sizes = KEEP(arg_as_dimension_sizes(dimension_sizes, error_call));

  const r_ssize x_size = rray_size(x, error_call);

  const r_ssize dimensionality = r_length(dimension_sizes);
  const int* v_dimension_sizes = r_int_cbegin(dimension_sizes);

  const r_ssize size =
    rray_size_from_dimensions(v_dimension_sizes, dimensionality);

  if (x_size != size) {
    r_abort_lazy_call(
      error_call,
      "Can't reshape to these dimension sizes. "
      "Can't change from a size of %td to a size of %td.",
      (ptrdiff_t) x_size,
      (ptrdiff_t) size
    );
  }

  r_obj* out = KEEP(r_wrap(x));

  r_attrib_poke_dim_names(out, r_null);
  r_attrib_poke_dim(out, dimension_sizes);

  FREE(3);
  return out;
}
