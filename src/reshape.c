#include "reshape.h"

#include "capacity.h"
#include "dimension-sizes.h"
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
  check_array(x, error_call);
  check_dimension_sizes(dimension_sizes, error_call);

  const r_ssize x_capacity = rray_capacity(x, error_call);

  const r_ssize dimensionality = r_length(dimension_sizes);
  const int* v_dimension_sizes = r_int_cbegin(dimension_sizes);

  const r_ssize capacity =
    rray_capacity_from_dimension_sizes(v_dimension_sizes, dimensionality);

  if (x_capacity != capacity) {
    r_abort_lazy_call(
      error_call,
      "Can't reshape to these dimension sizes. "
      "Can't change from a capacity of %td to a capacity of %td.",
      (ptrdiff_t) x_capacity,
      (ptrdiff_t) capacity
    );
  }

  r_obj* out = KEEP(r_wrap(x));

  // TODO: Maybe `check_array()` should become `as_array()` and handle
  // upgrading vectors to arrays (by adding dim and promoting names to
  // dimnames), so then we only set one of these.
  r_attrib_poke_dim_names(out, r_null);
  r_attrib_poke_names(out, r_null);

  r_attrib_poke_dim(out, dimension_sizes);

  FREE(1);
  return out;
}
