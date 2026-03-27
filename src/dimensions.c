#include "dimensions.h"

#include "decl/dimensions-decl.h"
#include "size.h"
#include "utils.h"
#include "wrapper.h"

r_obj* ffi_rray_dimensions(r_obj* x, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  return rray_dimensions(x, error_call);
}

r_obj* rray_dimensions(r_obj* x, struct r_lazy error_call) {
  x = KEEP(arg_as_array(x, "x", error_call));
  r_obj* out = r_dim(x);
  FREE(1);
  return out;
}

bool rray_dimensions_are_equal(
  const int* v_x_dimensions,
  r_ssize x_dimensionality,
  const int* v_y_dimensions,
  r_ssize y_dimensionality
) {
  if (x_dimensionality != y_dimensionality) {
    return false;
  }

  for (r_ssize i = 0; i < x_dimensionality; ++i) {
    if (v_x_dimensions[i] != v_y_dimensions[i]) {
      return false;
    }
  }

  return true;
}

r_obj* ffi_rray_set_dimensions(r_obj* x, r_obj* dimensions, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  return rray_set_dimensions(x, dimensions, error_call);
}

r_obj* rray_set_dimensions(
  r_obj* x,
  r_obj* dimensions,
  struct r_lazy error_call
) {
  x = KEEP(arg_as_array(x, "x", error_call));
  dimensions = KEEP(arg_as_dimensions(dimensions, error_call));

  const r_ssize x_size = rray_size(x, error_call);

  const r_ssize dimensionality = r_length(dimensions);
  const int* v_dimensions = r_int_cbegin(dimensions);

  const r_ssize size = rray_size_from_dimensions(v_dimensions, dimensionality);

  if (x_size != size) {
    r_abort_lazy_call(
      error_call,
      "Can't set these dimensions. "
      "Can't change from a size of %td to a size of %td.",
      (ptrdiff_t) x_size,
      (ptrdiff_t) size
    );
  }

  r_obj* out = KEEP(r_wrap(x));

  r_attrib_poke_dim_names(out, r_null);
  r_attrib_poke_dim(out, dimensions);

  FREE(3);
  return out;
}

r_obj* ffi_rray_dimensions_common(r_obj* xs, r_obj* dimensions, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  return rray_dimensions_common(xs, dimensions, error_call);
}

r_obj* rray_dimensions_common(
  r_obj* xs,
  r_obj* dimensions,
  struct r_lazy error_call
) {
  if (dimensions != r_null) {
    return arg_as_dimensions(dimensions, error_call);
  }

  const r_ssize n = r_length(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);

  r_obj* out = r_null;
  KEEP(out);

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* x = v_xs[i];

    if (x == r_null) {
      continue;
    }

    r_obj* x_dimensions = KEEP(rray_dimensions(x, error_call));

    if (out == r_null) {
      KEEP_AT(x_dimensions, 0);
      out = x_dimensions;
      FREE(1);
      continue;
    }

    out = rray_dimensions2(out, x_dimensions, error_call);
    KEEP_AT(out, 0);
    FREE(1);
  }

  if (out == r_null) {
    r_abort_lazy_call(error_call, "Must supply at least one array to `...`.");
  }

  FREE(1);
  return out;
}

static inline r_obj* rray_dimensions2(
  r_obj* x_dimensions,
  r_obj* y_dimensions,
  struct r_lazy error_call
) {
  const r_ssize x_dimensionality = r_length(x_dimensions);
  const r_ssize y_dimensionality = r_length(y_dimensions);
  const r_ssize out_dimensionality =
    (x_dimensionality > y_dimensionality) ? x_dimensionality : y_dimensionality;

  const int* v_x_dimensions = r_int_cbegin(x_dimensions);
  const int* v_y_dimensions = r_int_cbegin(y_dimensions);

  r_obj* out = KEEP(r_alloc_integer(out_dimensionality));
  int* v_out = r_int_begin(out);

  for (r_ssize i = 0; i < out_dimensionality; ++i) {
    const int x_dimension = (i < x_dimensionality) ? v_x_dimensions[i] : 1;
    const int y_dimension = (i < y_dimensionality) ? v_y_dimensions[i] : 1;

    if (x_dimension == y_dimension) {
      v_out[i] = x_dimension;
    } else if (x_dimension == 1) {
      v_out[i] = y_dimension;
    } else if (y_dimension == 1) {
      v_out[i] = x_dimension;
    } else {
      r_abort_lazy_call(
        error_call,
        "Can't find common dimensions at axis %td. "
        "Dimensions %d and %d are incompatible.",
        (ptrdiff_t) (i + 1),
        x_dimension,
        y_dimension
      );
    }
  }

  FREE(1);
  return out;
}

r_obj* arg_as_dimensions(r_obj* dimensions, struct r_lazy error_call) {
  if (r_typeof(dimensions) != R_TYPE_integer) {
    dimensions =
      vec_cast(dimensions, r_globals.empty_int, dimensions_chr, r_null);
  }
  KEEP(dimensions);

  if (r_attrib_has_any(dimensions)) {
    r_abort_lazy_call(error_call, "`dimensions` can't have attributes.");
  }

  const r_ssize dimensionality = r_length(dimensions);

  if (dimensionality == 0) {
    r_abort_lazy_call(
      error_call,
      "`dimensions` must have at least one element."
    );
  }

  const int* v_dimensions = r_int_cbegin(dimensions);

  for (r_ssize i = 0; i < dimensionality; ++i) {
    const int dimension = v_dimensions[i];

    if (dimension == r_globals.na_int) {
      r_abort_lazy_call(
        error_call,
        "`dimensions` must not contain missing values."
      );
    }

    if (dimension < 0) {
      r_abort_lazy_call(
        error_call,
        "`dimensions` must not contain negative values."
      );
    }
  }

  FREE(1);
  return dimensions;
}
