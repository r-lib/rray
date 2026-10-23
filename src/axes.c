#include "axes.h"

#include "dimensionality.h"
#include "utils.h"

#include "decl/axes-decl.h"

r_obj* arg_as_axes(
  r_obj* axes,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  axes = KEEP(arg_as_bare_integer(axes, arg, error_call));
  axes = axes_normalize(axes, dimensionality, arg, error_call);
  FREE(1);
  return axes;
}

r_obj* axes_normalize(
  r_obj* axes,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_axes_unique(axes, dimensionality, arg, error_call);

  const r_ssize axes_size = r_length(axes);
  const int* v_axes = r_int_cbegin(axes);

  bool increasing = true;

  for (r_ssize i = 1; i < axes_size; ++i) {
    if (v_axes[i] < v_axes[i - 1]) {
      increasing = false;
      break;
    }
  }

  if (increasing) {
    return axes;
  }

  bool v_seen[RRAY_MAX_DIMENSIONALITY] = {false};

  for (r_ssize i = 0; i < axes_size; ++i) {
    v_seen[v_axes[i] - 1] = true;
  }

  r_obj* out = KEEP(r_alloc_integer(axes_size));
  int* v_out = r_int_begin(out);

  r_ssize out_i = 0;

  for (int i = 0; i < dimensionality; ++i) {
    if (v_seen[i]) {
      v_out[out_i] = i + 1;
      ++out_i;
    }
  }

  FREE(1);
  return out;
}

r_obj* arg_as_axes_unsorted(
  r_obj* axes,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_dimensionality(dimensionality);

  axes = KEEP(arg_as_bare_integer(axes, arg, error_call));
  check_axes_unique(axes, dimensionality, arg, error_call);

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
      "not length %" R_PRI_SSIZE ".",
      rray_arg_format(arg),
      dimensionality,
      axes_size
    );
  }

  check_axes_unique(axes, dimensionality, arg, error_call);

  FREE(1);
  return axes;
}

static void check_axes_unique(
  r_obj* axes,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  bool v_seen[RRAY_MAX_DIMENSIONALITY] = {false};

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
