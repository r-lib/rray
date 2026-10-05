#include "axes.h"

#include "dimensionality.h"
#include "utils.h"

r_obj* arg_as_axes(
  r_obj* axes,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  axes = KEEP(arg_as_bare_integer(axes, arg, error_call));
  check_axes(axes, dimensionality, arg, error_call);
  FREE(1);
  return axes;
}

void check_axes(
  r_obj* axes,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
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
}

r_obj* arg_as_axes_unsorted(
  r_obj* axes,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_dimensionality(dimensionality);

  axes = KEEP(arg_as_bare_integer(axes, arg, error_call));

  const r_ssize axes_size = r_length(axes);

  bool v_seen[RRAY_MAX_DIMENSIONALITY] = {false};

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

    if (v_seen[axis - 1]) {
      r_abort_lazy_call(
        error_call,
        "%s must not contain %d more than once.",
        rray_arg_format(arg),
        axis
      );
    }

    v_seen[axis - 1] = true;
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
  check_dimensionality(dimensionality);

  axes = KEEP(arg_as_bare_integer(axes, arg, error_call));

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
