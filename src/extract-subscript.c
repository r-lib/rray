#include "extract-subscript.h"

#include <math.h>

#include "dimensionality.h"
#include "dimensions.h"
#include "size.h"
#include "utils.h"

struct rray_subscript_summary {
  double min;
  double max;
  r_ssize zeros;
  bool any_missing;
  bool any_fractional;
};

#include "decl/extract-subscript-decl.h"

r_obj* ffi_rray_as_extract_subscript(
  r_obj* ffi_i,
  r_obj* ffi_dimensions,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};

  r_obj* dimensions =
    KEEP(arg_as_dimensions(ffi_dimensions, rray_args.dimensions, error_call));

  const struct rray_extract_subscript subscript = rray_as_extract_subscript(
    ffi_i,
    r_int_cbegin(dimensions),
    rray_dimensionality_from_dimensions(dimensions),
    rray_args.i,
    error_call
  );
  KEEP(subscript.i);

  const char* v_names[] = {"i", "kind", "size"};
  r_obj* names = KEEP(r_chr_n(v_names, 3));

  r_obj* out = KEEP(r_alloc_list(3));
  r_attrib_poke_names(out, names);

  r_list_poke(out, 0, subscript.i);
  r_list_poke(out, 1, r_chr(rray_extract_subscript_kind_name(subscript.kind)));
  r_list_poke(out, 2, r_int(r_ssize_as_integer(subscript.size)));

  FREE(4);
  return out;
}

struct rray_extract_subscript rray_as_extract_subscript(
  r_obj* i,
  const int* v_dimensions,
  int dimensionality,
  struct rray_arg* i_arg,
  struct r_lazy error_call
) {
  check_unclassed(i, i_arg, error_call);

  const r_ssize size = rray_size_from_dimensions(v_dimensions, dimensionality);

  switch (r_typeof(i)) {
  case R_TYPE_logical:
    return rray_as_extract_mask(
      i,
      v_dimensions,
      dimensionality,
      size,
      i_arg,
      error_call
    );
  case R_TYPE_integer:
  case R_TYPE_double: {
    r_obj* i_dimensions = r_dim(i);
    const int i_dimensionality = i_dimensions == r_null
      ? 1
      : rray_dimensionality_from_dimensions(i_dimensions);

    switch (i_dimensionality) {
    case 1:
      return rray_as_extract_locations(i, size, i_arg, error_call);
    case 2:
      return rray_as_extract_points(
        i,
        v_dimensions,
        dimensionality,
        i_arg,
        error_call
      );
    default:
      r_abort_lazy_call(
        error_call,
        "Numeric %s must be a vector or a matrix, not an array with "
        "%d dimensions.",
        rray_arg_format(i_arg),
        i_dimensionality
      );
    }
  }
  default:
    r_abort_lazy_call(
      error_call,
      "%s must be logical, integer, or double, not %s.",
      rray_arg_format(i_arg),
      r_obj_type_friendly(i)
    );
  }
}

static struct rray_extract_subscript rray_as_extract_mask(
  r_obj* i,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  struct rray_arg* i_arg,
  struct r_lazy error_call
) {
  r_obj* i_dimensions = r_dim(i);
  const int i_dimensionality = i_dimensions == r_null
    ? 1
    : rray_dimensionality_from_dimensions(i_dimensions);

  if (i_dimensionality != 1) {
    const bool equal = rray_dimensions_are_equal(
      r_int_cbegin(i_dimensions),
      i_dimensionality,
      v_dimensions,
      dimensionality
    );

    if (!equal) {
      r_abort_lazy_call(
        error_call,
        "Logical %s must be a vector or have the same dimensions as `x`.",
        rray_arg_format(i_arg)
      );
    }
  }

  const r_ssize i_size = r_length(i);

  if (i_size != 1 && i_size != size) {
    r_abort_lazy_call(
      error_call,
      "Logical %s must be size 1 or %" R_PRI_SSIZE ", not %" R_PRI_SSIZE ".",
      rray_arg_format(i_arg),
      size,
      i_size
    );
  }

  return (struct rray_extract_subscript){
    .i = i,
    .kind = RRAY_EXTRACT_SUBSCRIPT_KIND_mask,
    .size = rray_mask_size(i, size)
  };
}

static struct rray_extract_subscript rray_as_extract_locations(
  r_obj* i,
  r_ssize size,
  struct rray_arg* i_arg,
  struct r_lazy error_call
) {
  const r_ssize i_size = r_length(i);
  const struct rray_subscript_summary summary =
    rray_subscript_summarise(i, 0, i_size);

  if (summary.any_fractional) {
    stop_subscript_fractional(i_arg, error_call);
  }
  if (summary.max > size) {
    r_abort_lazy_call(
      error_call,
      "%s must not contain values greater than %" R_PRI_SSIZE ".",
      rray_arg_format(i_arg),
      size
    );
  }
  if (summary.min < -size) {
    r_abort_lazy_call(
      error_call,
      "%s must not contain values less than -%" R_PRI_SSIZE ".",
      rray_arg_format(i_arg),
      size
    );
  }

  const bool any_negative = summary.min < 0;

  if (any_negative && summary.max > 0) {
    r_abort_lazy_call(
      error_call,
      "%s can't mix positive and negative values.",
      rray_arg_format(i_arg)
    );
  }
  if (any_negative && summary.any_missing) {
    r_abort_lazy_call(
      error_call,
      "%s can't mix negative and missing values.",
      rray_arg_format(i_arg)
    );
  }

  if (any_negative) {
    return rray_as_extract_complement(i, size);
  }
  if (summary.zeros != 0) {
    return rray_as_extract_nonzero(i, i_size - summary.zeros);
  }

  return (struct rray_extract_subscript){
    .i = i,
    .kind = r_typeof(i) == R_TYPE_integer
      ? RRAY_EXTRACT_SUBSCRIPT_KIND_locations_int
      : RRAY_EXTRACT_SUBSCRIPT_KIND_locations_dbl,
    .size = i_size
  };
}

static struct rray_extract_subscript rray_as_extract_points(
  r_obj* i,
  const int* v_dimensions,
  int dimensionality,
  struct rray_arg* i_arg,
  struct r_lazy error_call
) {
  const int* v_i_dimensions = r_int_cbegin(r_dim(i));
  const r_ssize size = v_i_dimensions[0];
  const int columns = v_i_dimensions[1];

  if (columns != dimensionality) {
    r_abort_lazy_call(
      error_call,
      "Numeric matrix %s must have %d column%s, one for each axis of `x`, "
      "not %d.",
      rray_arg_format(i_arg),
      dimensionality,
      dimensionality == 1 ? "" : "s",
      columns
    );
  }

  for (int axis = 0; axis < columns; ++axis) {
    const int dimension = v_dimensions[axis];
    const struct rray_subscript_summary summary =
      rray_subscript_summarise(i, axis * size, size);

    if (summary.any_fractional) {
      stop_subscript_fractional(i_arg, error_call);
    }
    if (summary.min < 1) {
      r_abort_lazy_call(
        error_call,
        "Column %d of %s must only contain positive values or missing "
        "values.",
        axis + 1,
        rray_arg_format(i_arg)
      );
    }
    if (summary.max > dimension) {
      r_abort_lazy_call(
        error_call,
        "Column %d of %s must not contain values greater than %d.",
        axis + 1,
        rray_arg_format(i_arg),
        dimension
      );
    }
  }

  return (struct rray_extract_subscript){
    .i = i,
    .kind = r_typeof(i) == R_TYPE_integer
      ? RRAY_EXTRACT_SUBSCRIPT_KIND_points_int
      : RRAY_EXTRACT_SUBSCRIPT_KIND_points_dbl,
    .size = size
  };
}

static struct rray_extract_subscript rray_as_extract_complement(
  r_obj* i,
  r_ssize size
) {
  const r_ssize i_size = r_length(i);

  r_obj* out = KEEP(r_alloc_logical(size));
  int* v_out = r_lgl_begin(out);

  for (r_ssize j = 0; j < size; ++j) {
    v_out[j] = 1;
  }

  switch (r_typeof(i)) {
  case R_TYPE_integer: {
    const int* v_i = r_int_cbegin(i);

    for (r_ssize j = 0; j < i_size; ++j) {
      const int elt = v_i[j];

      if (elt != 0) {
        v_out[-(r_ssize) elt - 1] = 0;
      }
    }

    break;
  }
  case R_TYPE_double: {
    const double* v_i = r_dbl_cbegin(i);

    for (r_ssize j = 0; j < i_size; ++j) {
      const double elt = v_i[j];

      if (elt != 0) {
        v_out[-(r_ssize) elt - 1] = 0;
      }
    }

    break;
  }
  default:
    r_stop_unreachable();
  }

  const struct rray_extract_subscript subscript = {
    .i = out,
    .kind = RRAY_EXTRACT_SUBSCRIPT_KIND_mask,
    .size = rray_mask_size(out, size)
  };

  FREE(1);
  return subscript;
}

static struct rray_extract_subscript rray_as_extract_nonzero(
  r_obj* i,
  r_ssize size
) {
  const r_ssize i_size = r_length(i);

  switch (r_typeof(i)) {
  case R_TYPE_integer: {
    const int* v_i = r_int_cbegin(i);

    r_obj* out = KEEP(r_alloc_integer(size));
    int* v_out = r_int_begin(out);

    r_ssize k = 0;

    for (r_ssize j = 0; j < i_size; ++j) {
      const int elt = v_i[j];

      if (elt != 0) {
        v_out[k] = elt;
        ++k;
      }
    }

    const struct rray_extract_subscript subscript = {
      .i = out,
      .kind = RRAY_EXTRACT_SUBSCRIPT_KIND_locations_int,
      .size = size
    };

    FREE(1);
    return subscript;
  }
  case R_TYPE_double: {
    const double* v_i = r_dbl_cbegin(i);

    r_obj* out = KEEP(r_alloc_double(size));
    double* v_out = r_dbl_begin(out);

    r_ssize k = 0;

    for (r_ssize j = 0; j < i_size; ++j) {
      const double elt = v_i[j];

      if (elt != 0) {
        v_out[k] = elt;
        ++k;
      }
    }

    const struct rray_extract_subscript subscript = {
      .i = out,
      .kind = RRAY_EXTRACT_SUBSCRIPT_KIND_locations_dbl,
      .size = size
    };

    FREE(1);
    return subscript;
  }
  default:
    r_stop_unreachable();
  }
}

static r_ssize rray_mask_size(r_obj* mask, r_ssize size) {
  const int* v_mask = r_lgl_cbegin(mask);
  const r_ssize mask_step = r_length(mask) == 1 ? 0 : 1;

  r_ssize out = 0;

  for (r_ssize j = 0; j < size; ++j) {
    out += v_mask[j * mask_step] != 0;
  }

  return out;
}

static struct rray_subscript_summary rray_subscript_summarise(
  r_obj* i,
  r_ssize start,
  r_ssize size
) {
  switch (r_typeof(i)) {
  case R_TYPE_integer:
    return rray_subscript_summarise_int(r_int_cbegin(i) + start, size);
  case R_TYPE_double:
    return rray_subscript_summarise_dbl(r_dbl_cbegin(i) + start, size);
  default:
    r_stop_unreachable();
  }
}

static struct rray_subscript_summary rray_subscript_summarise_int(
  const int* v_i,
  r_ssize size
) {
  struct rray_subscript_summary out = {
    .min = INFINITY,
    .max = -INFINITY,
    .zeros = 0,
    .any_missing = false,
    .any_fractional = false
  };

  for (r_ssize j = 0; j < size; ++j) {
    const int elt = v_i[j];

    if (elt == r_globals.na_int) {
      out.any_missing = true;
      continue;
    }

    out.min = fmin(out.min, elt);
    out.max = fmax(out.max, elt);
    out.zeros += elt == 0;
  }

  return out;
}

static struct rray_subscript_summary rray_subscript_summarise_dbl(
  const double* v_i,
  r_ssize size
) {
  struct rray_subscript_summary out = {
    .min = INFINITY,
    .max = -INFINITY,
    .zeros = 0,
    .any_missing = false,
    .any_fractional = false
  };

  for (r_ssize j = 0; j < size; ++j) {
    const double elt = v_i[j];

    if (isnan(elt)) {
      out.any_missing = true;
      continue;
    }

    out.min = fmin(out.min, elt);
    out.max = fmax(out.max, elt);
    out.zeros += elt == 0;
    out.any_fractional |= elt != trunc(elt);
  }

  return out;
}

static r_no_return void stop_subscript_fractional(
  struct rray_arg* i_arg,
  struct r_lazy error_call
) {
  r_abort_lazy_call(
    error_call,
    "Can't convert from %s <double> to <integer> due to loss of precision.",
    rray_arg_format(i_arg)
  );
}

static const char* rray_extract_subscript_kind_name(
  enum rray_extract_subscript_kind kind
) {
  switch (kind) {
  case RRAY_EXTRACT_SUBSCRIPT_KIND_locations_int:
    return "locations_int";
  case RRAY_EXTRACT_SUBSCRIPT_KIND_locations_dbl:
    return "locations_dbl";
  case RRAY_EXTRACT_SUBSCRIPT_KIND_mask:
    return "mask";
  case RRAY_EXTRACT_SUBSCRIPT_KIND_points_int:
    return "points_int";
  case RRAY_EXTRACT_SUBSCRIPT_KIND_points_dbl:
    return "points_dbl";
  }

  r_stop_unreachable();
}
