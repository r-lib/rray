#include "axes.h"

#include "utils.h"

r_obj* arg_as_axes(
  r_obj* axes,
  r_ssize dimensionality,
  r_obj* arg,
  struct r_lazy error_call
) {
  if (r_typeof(axes) != R_TYPE_integer) {
    axes = vec_cast(axes, r_globals.empty_int, arg, r_null);
  }
  KEEP(axes);

  if (r_attrib_has_any(axes)) {
    r_abort_lazy_call(
      error_call,
      "`%s` can't have attributes.",
      r_chr_get_c_string(arg, 0)
    );
  }

  const r_ssize axes_size = r_length(axes);

  const int* v_axes = r_int_cbegin(axes);

  for (r_ssize i = 0; i < axes_size; ++i) {
    const int axis = v_axes[i];

    if (axis == r_globals.na_int) {
      r_abort_lazy_call(
        error_call,
        "`%s` must not contain missing values.",
        r_chr_get_c_string(arg, 0)
      );
    }

    if (axis < 1) {
      r_abort_lazy_call(
        error_call,
        "`%s` must contain values greater than or equal to 1, not %d.",
        r_chr_get_c_string(arg, 0),
        axis
      );
    }

    if (axis > dimensionality) {
      r_abort_lazy_call(
        error_call,
        "`%s` must contain values less than or equal to the "
        "dimensionality of %td, not %d.",
        r_chr_get_c_string(arg, 0),
        (ptrdiff_t) dimensionality,
        axis
      );
    }

    if (i > 0 && axis <= v_axes[i - 1]) {
      r_abort_lazy_call(
        error_call,
        "`%s` must be in strictly increasing order.",
        r_chr_get_c_string(arg, 0)
      );
    }
  }

  FREE(1);
  return axes;
}

r_ssize arg_as_axis(
  r_obj* axis,
  r_ssize dimensionality,
  r_obj* arg,
  struct r_lazy error_call
) {
  axis = KEEP(arg_as_axes(axis, dimensionality, arg, error_call));

  if (r_length(axis) != 1) {
    r_abort_lazy_call(
      error_call,
      "`%s` must be a single axis, not length %td.",
      r_chr_get_c_string(arg, 0),
      (ptrdiff_t) r_length(axis)
    );
  }

  const r_ssize out = r_int_get(axis, 0);

  FREE(1);
  return out;
}
