#include "axes.h"

#include "utils.h"

r_obj* arg_as_axes(
  r_obj* axes,
  r_ssize dimensionality,
  const char* arg,
  struct r_lazy error_call
) {
  if (r_typeof(axes) != R_TYPE_integer) {
    r_obj* arg_chr = KEEP(r_chr(arg));
    axes = vec_cast(axes, r_globals.empty_int, arg_chr, r_null);
    FREE(1);
  }
  KEEP(axes);

  if (r_attrib_has_any(axes)) {
    r_abort_lazy_call(error_call, "`%s` can't have attributes.", arg);
  }

  const r_ssize axes_size = r_length(axes);

  const int* v_axes = r_int_cbegin(axes);

  for (r_ssize i = 0; i < axes_size; ++i) {
    const int axis = v_axes[i];

    if (axis == r_globals.na_int) {
      r_abort_lazy_call(
        error_call,
        "`%s` must not contain missing values.",
        arg
      );
    }

    if (axis < 1) {
      r_abort_lazy_call(
        error_call,
        "`%s` must contain values greater than or equal to 1, not %d.",
        arg,
        axis
      );
    }

    if (axis > dimensionality) {
      r_abort_lazy_call(
        error_call,
        "`%s` must contain values less than or equal to the "
        "dimensionality of %td, not %d.",
        arg,
        (ptrdiff_t) dimensionality,
        axis
      );
    }

    if (i > 0 && axis <= v_axes[i - 1]) {
      r_abort_lazy_call(
        error_call,
        "`%s` must be in strictly increasing order.",
        arg
      );
    }
  }

  FREE(1);
  return axes;
}
