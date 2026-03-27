#include "axes.h"

#include "utils.h"

r_obj* arg_as_axes(
  r_obj* axes,
  r_ssize dimensionality,
  struct r_lazy error_call
) {
  if (r_typeof(axes) != R_TYPE_integer) {
    axes = vec_cast(axes, r_globals.empty_int, axes_chr, r_null);
  }
  KEEP(axes);

  if (r_attrib_has_any(axes)) {
    r_abort_lazy_call(error_call, "`axes` can't have attributes.");
  }

  const r_ssize axes_size = r_length(axes);

  const int* v_axes = r_int_cbegin(axes);

  for (r_ssize i = 0; i < axes_size; ++i) {
    const int axis = v_axes[i];

    if (axis == r_globals.na_int) {
      r_abort_lazy_call(error_call, "`axes` must not contain missing values.");
    }

    if (axis < 1) {
      r_abort_lazy_call(
        error_call,
        "`axes` must contain values greater than or equal to 1, not %d.",
        axis
      );
    }

    if (axis > dimensionality) {
      r_abort_lazy_call(
        error_call,
        "`axes` must contain values less than or equal to the "
        "dimensionality of %td, not %d.",
        (ptrdiff_t) dimensionality,
        axis
      );
    }

    if (i > 0 && axis <= v_axes[i - 1]) {
      r_abort_lazy_call(
        error_call,
        "`axes` must be in strictly increasing order."
      );
    }
  }

  FREE(1);
  return axes;
}
