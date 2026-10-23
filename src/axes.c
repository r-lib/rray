#include "axes.h"

#include "dimensionality.h"
#include "utils.h"

#include "decl/axes-decl.h"

int arg_as_axis(
  r_obj* axis,
  r_obj* titles,
  int dimensionality,
  struct rray_arg* arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  check_dimensionality(dimensionality);
  check_axes_type(axis, arg, error_call);

  if (r_length(axis) != 1) {
    r_abort_lazy_call(
      error_call,
      "%s must be a single integer or string, not length %" R_PRI_SSIZE ".",
      rray_arg_format(arg),
      r_length(axis)
    );
  }

  if (r_typeof(axis) == R_TYPE_character) {
    r_obj* title = r_chr_get(axis, 0);

    if (title == r_globals.na_str) {
      r_abort_lazy_call(
        error_call,
        "%s must not be missing.",
        rray_arg_format(arg)
      );
    }

    if (title == r_strs.empty) {
      r_abort_lazy_call(
        error_call,
        "%s can't be an empty string.",
        rray_arg_format(arg)
      );
    }

    return axis_from_title(title, titles, arg, x_arg, error_call);
  }

  const int out = arg_as_int(axis, arg, error_call);
  check_axis(out, dimensionality, arg, error_call);
  return out;
}

r_obj* arg_as_axes(
  r_obj* axes,
  r_obj* titles,
  int dimensionality,
  struct rray_arg* arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  check_dimensionality(dimensionality);

  r_obj* out =
    KEEP(arg_as_axes_positions(axes, titles, arg, x_arg, error_call));
  check_axes_unique(out, axes, dimensionality, arg, error_call);
  out = axes_sort(out, dimensionality);
  FREE(1);
  return out;
}

r_obj* arg_as_axes_unsorted(
  r_obj* axes,
  r_obj* titles,
  int dimensionality,
  struct rray_arg* arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  check_dimensionality(dimensionality);

  r_obj* out =
    KEEP(arg_as_axes_positions(axes, titles, arg, x_arg, error_call));
  check_axes_unique(out, axes, dimensionality, arg, error_call);

  FREE(1);
  return out;
}

r_obj* arg_as_axes_unsorted_integer(
  r_obj* axes,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_dimensionality(dimensionality);

  axes = KEEP(arg_as_bare_integer(axes, arg, error_call));
  check_axes_unique(axes, axes, dimensionality, arg, error_call);

  FREE(1);
  return axes;
}

r_obj* arg_as_axes_permutation(
  r_obj* axes,
  r_obj* titles,
  int dimensionality,
  struct rray_arg* arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  check_dimensionality(dimensionality);

  r_obj* out =
    KEEP(arg_as_axes_positions(axes, titles, arg, x_arg, error_call));

  const r_ssize out_size = r_length(out);

  if (out_size != dimensionality) {
    r_abort_lazy_call(
      error_call,
      "%s must have length %d to match the dimensionality of the array, "
      "not length %" R_PRI_SSIZE ".",
      rray_arg_format(arg),
      dimensionality,
      out_size
    );
  }

  check_axes_unique(out, axes, dimensionality, arg, error_call);

  FREE(1);
  return out;
}

r_obj* axes_normalize(
  r_obj* axes,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_axes_unique(axes, axes, dimensionality, arg, error_call);
  return axes_sort(axes, dimensionality);
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

static void check_axes_type(
  r_obj* axes,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  if (r_attrib_has_any(axes)) {
    r_abort_lazy_call(
      error_call,
      "%s can't have attributes.",
      rray_arg_format(arg)
    );
  }

  switch (r_typeof(axes)) {
  case R_TYPE_integer:
  case R_TYPE_double:
  case R_TYPE_character:
    return;
  default:
    r_abort_lazy_call(
      error_call,
      "%s must be an integer, double, or character vector, not %s.",
      rray_arg_format(arg),
      r_obj_type_friendly(axes)
    );
  }
}

static int axis_from_title(
  r_obj* title,
  r_obj* titles,
  struct rray_arg* arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  check_has_titles(titles, arg, x_arg, error_call);

  const r_ssize titles_size = r_length(titles);
  r_obj* const* v_titles = r_chr_cbegin(titles);

  for (r_ssize i = 0; i < titles_size; ++i) {
    if (v_titles[i] == title) {
      return (int) i + 1;
    }
  }

  r_abort_lazy_call(
    error_call,
    "Can't find an axis of %s titled \"%s\".",
    rray_arg_format(x_arg),
    r_str_c_string(title)
  );
}

static void check_has_titles(
  r_obj* titles,
  struct rray_arg* arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  if (titles == r_null) {
    r_abort_lazy_call(
      error_call,
      "Can't use a title in %s because %s has no titles.",
      rray_arg_format(arg),
      rray_arg_format(x_arg)
    );
  }
}

static r_obj* arg_as_axes_positions(
  r_obj* axes,
  r_obj* titles,
  struct rray_arg* arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  check_axes_type(axes, arg, error_call);

  if (r_typeof(axes) == R_TYPE_character) {
    return axes_from_titles(axes, titles, arg, x_arg, error_call);
  }

  return arg_as_bare_integer(axes, arg, error_call);
}

static r_obj* axes_from_titles(
  r_obj* axes,
  r_obj* titles,
  struct rray_arg* arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  check_has_titles(titles, arg, x_arg, error_call);

  const r_ssize axes_size = r_length(axes);
  r_obj* const* v_axes = r_chr_cbegin(axes);

  r_obj* out = KEEP(r_alloc_integer(axes_size));
  int* v_out = r_int_begin(out);

  for (r_ssize i = 0; i < axes_size; ++i) {
    r_obj* title = v_axes[i];

    if (title == r_globals.na_str) {
      r_abort_lazy_call(
        error_call,
        "%s must not contain missing values.",
        rray_arg_format(arg)
      );
    }

    if (title == r_strs.empty) {
      r_abort_lazy_call(
        error_call,
        "%s can't contain empty strings.",
        rray_arg_format(arg)
      );
    }

    v_out[i] = axis_from_title(title, titles, arg, x_arg, error_call);
  }

  FREE(1);
  return out;
}

static void check_axes_unique(
  r_obj* axes,
  r_obj* input,
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
      if (r_typeof(input) == R_TYPE_character) {
        r_abort_lazy_call(
          error_call,
          "%s must not contain \"%s\" more than once.",
          rray_arg_format(arg),
          r_chr_get_c_string(input, i)
        );
      }

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

static r_obj* axes_sort(r_obj* axes, int dimensionality) {
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
