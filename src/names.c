#include "names.h"

#include "axes.h"
#include "decl/names-decl.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "utils.h"
#include "wrapper.h"

r_obj* ffi_rray_names(r_obj* x, r_obj* frame) {
  struct r_lazy error_call = {.x = frame, .env = r_null};
  return rray_names(x, error_call);
}

r_obj* rray_names(r_obj* x, struct r_lazy error_call) {
  check_unclassed(x, "x", error_call);
  x = KEEP(arg_as_array(x, "x", error_call));
  r_obj* out = r_dim_names(x);
  FREE(1);
  return out;
}

r_obj* ffi_rray_axis_names(r_obj* x, r_obj* axis, r_obj* frame) {
  struct r_lazy error_call = {.x = frame, .env = r_null};
  return rray_axis_names(x, axis, error_call);
}

r_obj* rray_axis_names(r_obj* x, r_obj* axis, struct r_lazy error_call) {
  check_unclassed(x, "x", error_call);
  x = KEEP(arg_as_array(x, "x", error_call));

  const r_ssize dimensionality = rray_dimensionality(x, error_call);
  const r_ssize c_axis =
    arg_as_axis(axis, dimensionality, axis_chr, error_call);

  r_obj* names = rray_names(x, error_call);

  r_obj* out = (names == r_null) ? r_null : r_list_get(names, c_axis - 1);

  FREE(1);
  return out;
}

r_obj* ffi_rray_row_names(r_obj* x, r_obj* frame) {
  struct r_lazy error_call = {.x = frame, .env = r_null};
  return rray_axis_names(x, row_axis, error_call);
}

r_obj* ffi_rray_col_names(r_obj* x, r_obj* frame) {
  struct r_lazy error_call = {.x = frame, .env = r_null};
  return rray_axis_names(x, col_axis, error_call);
}

r_obj* ffi_rray_set_names(r_obj* x, r_obj* names, r_obj* frame) {
  struct r_lazy error_call = {.x = frame, .env = r_null};
  return rray_set_names(x, names, error_call);
}

r_obj* rray_set_names(r_obj* x, r_obj* names, struct r_lazy error_call) {
  check_unclassed(x, "x", error_call);
  x = KEEP(arg_as_array(x, "x", error_call));

  if (names != r_null) {
    if (r_typeof(names) != R_TYPE_list) {
      r_abort_lazy_call(
        error_call,
        "`names` must be a list or `NULL`, not %s.",
        r_obj_type_friendly(names)
      );
    }

    r_obj* dimensions = KEEP(rray_dimensions(x, error_call));
    const int* v_dimensions = r_int_cbegin(dimensions);
    const r_ssize dimensionality =
      rray_dimensionality_from_dimensions(dimensions);

    if (r_length(names) != dimensionality) {
      r_abort_lazy_call(
        error_call,
        "`names` must have length %td to match the dimensionality of `x`, "
        "not length %td.",
        (ptrdiff_t) dimensionality,
        (ptrdiff_t) r_length(names)
      );
    }

    r_obj* const* v_names = r_list_cbegin(names);
    for (r_ssize i = 0; i < dimensionality; ++i) {
      check_axis_names(v_names[i], i + 1, v_dimensions[i], error_call);
    }

    FREE(1);
  }

  r_obj* out = KEEP(r_wrap(x));
  r_attrib_poke_dim_names(out, names);

  FREE(2);
  return out;
}

r_obj* ffi_rray_set_axis_names(
  r_obj* x,
  r_obj* axis,
  r_obj* names,
  r_obj* frame
) {
  struct r_lazy error_call = {.x = frame, .env = r_null};
  return rray_set_axis_names(x, axis, names, error_call);
}

r_obj* rray_set_axis_names(
  r_obj* x,
  r_obj* axis,
  r_obj* names,
  struct r_lazy error_call
) {
  check_unclassed(x, "x", error_call);
  x = KEEP(arg_as_array(x, "x", error_call));

  r_obj* dimensions = KEEP(rray_dimensions(x, error_call));
  const int* v_dimensions = r_int_cbegin(dimensions);
  const r_ssize dimensionality =
    rray_dimensionality_from_dimensions(dimensions);

  const r_ssize c_axis =
    arg_as_axis(axis, dimensionality, axis_chr, error_call);

  check_axis_names(names, c_axis, v_dimensions[c_axis - 1], error_call);

  r_obj* old_names = rray_names(x, error_call);

  r_obj* new_names = KEEP(r_alloc_list(dimensionality));
  if (old_names != r_null) {
    r_obj* const* v_old_names = r_list_cbegin(old_names);
    for (r_ssize i = 0; i < dimensionality; ++i) {
      r_list_poke(new_names, i, v_old_names[i]);
    }
  }
  r_list_poke(new_names, c_axis - 1, names);

  r_obj* out = KEEP(r_wrap(x));
  r_attrib_poke_dim_names(out, new_names);

  FREE(4);
  return out;
}

r_obj* ffi_rray_set_row_names(r_obj* x, r_obj* names, r_obj* frame) {
  struct r_lazy error_call = {.x = frame, .env = r_null};
  return rray_set_axis_names(x, row_axis, names, error_call);
}

r_obj* ffi_rray_set_col_names(r_obj* x, r_obj* names, r_obj* frame) {
  struct r_lazy error_call = {.x = frame, .env = r_null};
  return rray_set_axis_names(x, col_axis, names, error_call);
}

static inline void check_axis_names(
  r_obj* names,
  r_ssize axis,
  int dimension,
  struct r_lazy error_call
) {
  if (names == r_null) {
    return;
  }

  if (r_typeof(names) != R_TYPE_character) {
    r_abort_lazy_call(
      error_call,
      "Names for axis %td must be a character vector or `NULL`, not %s.",
      (ptrdiff_t) axis,
      r_obj_type_friendly(names)
    );
  }

  if (r_length(names) != dimension) {
    r_abort_lazy_call(
      error_call,
      "Names for axis %td must have length %d, not length %td.",
      (ptrdiff_t) axis,
      dimension,
      (ptrdiff_t) r_length(names)
    );
  }
}

r_obj* row_axis = NULL;
r_obj* col_axis = NULL;

void rray_init_names(r_obj* ns) {
  row_axis = r_int(1);
  r_preserve(row_axis);

  col_axis = r_int(2);
  r_preserve(col_axis);
}
