#include "dimensions.h"

#include "decl/dimensions-decl.h"
#include "dimensionality.h"
#include "size.h"
#include "utils.h"
#include "wrapper.h"

r_obj* ffi_rray_dimensions(r_obj* x, r_obj* frame) {
  struct r_lazy error_call = {.x = frame, .env = r_null};
  return rray_dimensions(x, error_call);
}

r_obj* rray_dimensions(r_obj* x, struct r_lazy error_call) {
  check_unclassed(x, "x", error_call);
  x = KEEP(arg_as_array(x, "x", error_call));
  r_obj* out = r_dim(x);
  FREE(1);
  return out;
}

int rray_dimension(r_obj* x, int axis, struct r_lazy error_call) {
  r_obj* dimensions = rray_dimensions(x, error_call);
  return r_int_get(dimensions, axis - 1);
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
  struct r_lazy error_call = {.x = frame, .env = r_null};
  return rray_set_dimensions(x, dimensions, error_call);
}

r_obj* rray_set_dimensions(
  r_obj* x,
  r_obj* dimensions,
  struct r_lazy error_call
) {
  check_unclassed(x, "x", error_call);
  x = KEEP(arg_as_array(x, "x", error_call));
  dimensions = KEEP(arg_as_dimensions(dimensions, dimensions_chr, error_call));

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
  struct r_lazy error_call = {.x = frame, .env = r_null};
  return rray_dimensions_common(xs, dimensions, error_call);
}

r_obj* rray_dimensions_common(
  r_obj* xs,
  r_obj* dimensions,
  struct r_lazy error_call
) {
  if (dimensions != r_null) {
    return arg_as_dimensions(dimensions, dot_dimensions_chr, error_call);
  }

  const r_ssize n = r_length(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);

  bool any = false;

  r_ssize out_dimensionality = 1;

  // Stack allocated array of known max size that we accumulate the common
  // dimensions in. Initialized to 1, which works very nicely with broadcasting.
  int v_out_dimensions[RRAY_MAX_DIMENSIONALITY];
  for (r_ssize i = 0; i < RRAY_MAX_DIMENSIONALITY; ++i) {
    v_out_dimensions[i] = 1;
  }

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* x = v_xs[i];

    if (x == r_null) {
      continue;
    }

    any = true;

    r_obj* x_dimensions = KEEP(rray_dimensions(x, error_call));
    const int* v_x_dimensions = r_int_cbegin(x_dimensions);
    const r_ssize x_dimensionality =
      rray_dimensionality_from_dimensions(x_dimensions);
    check_max_dimensionality(x_dimensionality);

    // Update `v_out_dimensions` and `out_dimensionality` in place
    // with common dimensions
    rray_dimensions2(
      v_out_dimensions,
      &out_dimensionality,
      v_x_dimensions,
      x_dimensionality,
      error_call
    );

    FREE(1);
  }

  if (!any) {
    r_abort_lazy_call(error_call, "Must supply at least one array to `...`.");
  }

  r_obj* out = KEEP(r_alloc_integer(out_dimensionality));
  int* v_out = r_int_begin(out);
  memcpy(v_out, v_out_dimensions, sizeof(int) * out_dimensionality);

  FREE(1);
  return out;
}

static inline void rray_dimensions2(
  int* v_out_dimensions,
  r_ssize* p_out_dimensionality,
  const int* v_x_dimensions,
  r_ssize x_dimensionality,
  struct r_lazy error_call
) {
  const r_ssize out_dimensionality = *p_out_dimensionality;

  const r_ssize common_dimensionality = (out_dimensionality > x_dimensionality)
    ? out_dimensionality
    : x_dimensionality;

  *p_out_dimensionality = common_dimensionality;

  for (r_ssize i = 0; i < common_dimensionality; ++i) {
    const int out_dimension =
      (i < out_dimensionality) ? v_out_dimensions[i] : 1;
    const int x_dimension = (i < x_dimensionality) ? v_x_dimensions[i] : 1;

    if (out_dimension == x_dimension) {
      // Nothing to do
      // v_out_dimensions[i] = out_dimension;
    } else if (out_dimension == 1) {
      v_out_dimensions[i] = x_dimension;
    } else if (x_dimension == 1) {
      // Nothing to do
      // v_out_dimensions[i] = out_dimension;
    } else {
      r_abort_lazy_call(
        error_call,
        "Can't find common dimensions at axis %td. "
        "Dimensions %d and %d are incompatible.",
        (ptrdiff_t) (i + 1),
        out_dimension,
        x_dimension
      );
    }
  }
}

r_obj* arg_as_dimensions(
  r_obj* dimensions,
  r_obj* arg,
  struct r_lazy error_call
) {
  if (r_typeof(dimensions) != R_TYPE_integer) {
    dimensions = vec_cast(dimensions, r_globals.empty_int, arg, r_null);
  }
  KEEP(dimensions);

  if (r_attrib_has_any(dimensions)) {
    r_abort_lazy_call(
      error_call,
      "`%s` can't have attributes.",
      r_chr_get_c_string(arg, 0)
    );
  }

  const r_ssize dimensionality = r_length(dimensions);

  if (dimensionality == 0) {
    r_abort_lazy_call(
      error_call,
      "`%s` must have at least one element.",
      r_chr_get_c_string(arg, 0)
    );
  }

  const int* v_dimensions = r_int_cbegin(dimensions);

  for (r_ssize i = 0; i < dimensionality; ++i) {
    const int dimension = v_dimensions[i];

    if (dimension == r_globals.na_int) {
      r_abort_lazy_call(
        error_call,
        "`%s` must not contain missing values.",
        r_chr_get_c_string(arg, 0)
      );
    }

    if (dimension < 0) {
      r_abort_lazy_call(
        error_call,
        "`%s` must not contain negative values.",
        r_chr_get_c_string(arg, 0)
      );
    }
  }

  FREE(1);
  return dimensions;
}
