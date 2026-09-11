#include "axes.h"

#include "dimensionality.h"
#include "utils.h"

r_obj* arg_as_axes(
  r_obj* axes,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  if (r_typeof(axes) != R_TYPE_integer) {
    axes = vec_cast(axes, r_globals.empty_int, arg, NULL);
  }
  KEEP(axes);

  if (r_attrib_has_any(axes)) {
    r_abort_lazy_call(
      error_call,
      "%s can't have attributes.",
      rray_arg_format(arg)
    );
  }

  const r_ssize axes_size = r_length(axes);

  const int* v_axes = r_int_cbegin(axes);

  for (r_ssize i = 0; i < axes_size; ++i) {
    const int axis = v_axes[i];

    if (axis == r_globals.na_int) {
      r_abort_lazy_call(
        error_call,
        "%s must not contain missing values.",
        rray_arg_format(arg)
      );
    }

    if (axis < 1) {
      r_abort_lazy_call(
        error_call,
        "%s must contain values greater than or equal to 1, not %d.",
        rray_arg_format(arg),
        axis
      );
    }

    if (axis > dimensionality) {
      r_abort_lazy_call(
        error_call,
        "%s must contain values less than or equal to the "
        "dimensionality of %d, not %d.",
        rray_arg_format(arg),
        dimensionality,
        axis
      );
    }

    if (i > 0 && axis <= v_axes[i - 1]) {
      r_abort_lazy_call(
        error_call,
        "%s must be in strictly increasing order.",
        rray_arg_format(arg)
      );
    }
  }

  FREE(1);
  return axes;
}

r_obj* arg_as_axes_permutation(
  r_obj* axes,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_max_dimensionality(dimensionality);

  if (r_typeof(axes) != R_TYPE_integer) {
    axes = vec_cast(axes, r_globals.empty_int, arg, NULL);
  }
  KEEP(axes);

  if (r_attrib_has_any(axes)) {
    r_abort_lazy_call(
      error_call,
      "%s can't have attributes.",
      rray_arg_format(arg)
    );
  }

  const r_ssize axes_size = r_length(axes);

  if (axes_size != dimensionality) {
    r_abort_lazy_call(
      error_call,
      "%s must have length %d to match the dimensionality of the array, "
      "not length %" R_PRIdXLEN_T ".",
      rray_arg_format(arg),
      dimensionality,
      axes_size
    );
  }

  bool v_seen[RRAY_MAX_DIMENSIONALITY] = {false};

  const int* v_axes = r_int_cbegin(axes);

  for (int i = 0; i < dimensionality; ++i) {
    const int axis = v_axes[i];

    if (axis == r_globals.na_int) {
      r_abort_lazy_call(
        error_call,
        "%s must not contain missing values.",
        rray_arg_format(arg)
      );
    }

    if (axis < 1) {
      r_abort_lazy_call(
        error_call,
        "%s must contain values greater than or equal to 1, not %d.",
        rray_arg_format(arg),
        axis
      );
    }

    if (axis > dimensionality) {
      r_abort_lazy_call(
        error_call,
        "%s must contain values less than or equal to the "
        "dimensionality of %d, not %d.",
        rray_arg_format(arg),
        dimensionality,
        axis
      );
    }

    if (v_seen[axis - 1]) {
      r_abort_lazy_call(
        error_call,
        "%s must not contain axis %d more than once.",
        rray_arg_format(arg),
        axis
      );
    }

    v_seen[axis - 1] = true;
  }

  FREE(1);
  return axes;
}

void check_axis(
  int axis,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  if (axis == r_globals.na_int) {
    r_abort_lazy_call(
      error_call,
      "%s must not be missing.",
      rray_arg_format(arg)
    );
  }

  if (axis < 1) {
    r_abort_lazy_call(
      error_call,
      "%s must be greater than or equal to 1, not %d.",
      rray_arg_format(arg),
      axis
    );
  }

  if (axis > dimensionality) {
    r_abort_lazy_call(
      error_call,
      "%s must be less than or equal to the dimensionality of %d, "
      "not %d.",
      rray_arg_format(arg),
      dimensionality,
      axis
    );
  }
}

r_obj* rray_axes_complement(
  const int* v_axes,
  r_ssize axes_size,
  int dimensionality
) {
  r_obj* out = KEEP(r_alloc_integer(dimensionality - axes_size));
  int* v_out = r_int_begin(out);

  r_ssize j = 0;
  r_ssize k = 0;

  for (int axis = 1; axis <= dimensionality; ++axis) {
    if (j < axes_size && v_axes[j] == axis) {
      ++j;
    } else {
      v_out[k] = axis;
      ++k;
    }
  }

  FREE(1);
  return out;
}
