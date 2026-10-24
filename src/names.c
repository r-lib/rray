#include "names.h"

#include "axes.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "globals.h"
#include "utils.h"
#include "view.h"

#include "decl/names-decl.h"

r_obj* ffi_rray_names(r_obj* ffi_x, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_names(ffi_x, rray_args.x, error_call);
}

r_obj* rray_names(r_obj* x, struct rray_arg* arg, struct r_lazy error_call) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));
  r_obj* out = r_dim_names(x);
  FREE(1);
  return out;
}

r_obj* ffi_rray_axis_names(r_obj* ffi_x, r_obj* ffi_axis, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_axis_names(ffi_x, ffi_axis, rray_args.x, error_call);
}

r_obj* ffi_rray_row_names(r_obj* ffi_x, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_axis_names(ffi_x, rray_globals.one_int, rray_args.x, error_call);
}

r_obj* ffi_rray_column_names(r_obj* ffi_x, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_axis_names(ffi_x, rray_globals.two_int, rray_args.x, error_call);
}

r_obj* rray_axis_names(
  r_obj* x,
  r_obj* axis,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));

  const int dimensionality = rray_dimensionality(x, arg, error_call);

  r_obj* names = rray_names(x, arg, error_call);
  r_obj* titles = (names == r_null) ? r_null : r_names(names);

  const int c_axis =
    arg_as_axis(axis, titles, dimensionality, rray_args.axis, arg, error_call);

  r_obj* out = (names == r_null) ? r_null : r_list_get(names, c_axis - 1);

  FREE(1);
  return out;
}

r_obj* ffi_rray_set_names(r_obj* ffi_x, r_obj* ffi_names, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_set_names(ffi_x, ffi_names, rray_args.x, error_call);
}

r_obj* rray_set_names(
  r_obj* x,
  r_obj* names,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));

  if (names != r_null) {
    if (r_typeof(names) != R_TYPE_list) {
      r_abort_lazy_call(
        error_call,
        "%s must be a list or `NULL`, not %s.",
        rray_arg_format(rray_args.names),
        r_obj_type_friendly(names)
      );
    }

    r_obj* dimensions = KEEP(rray_dimensions(x, arg, error_call));
    const int* v_dimensions = r_int_cbegin(dimensions);
    const int dimensionality = rray_dimensionality_from_dimensions(dimensions);

    if (r_length(names) != dimensionality) {
      r_abort_lazy_call(
        error_call,
        "%s must have length %d to match the dimensionality of %s, "
        "not length %" R_PRI_SSIZE ".",
        rray_arg_format(rray_args.names),
        dimensionality,
        rray_arg_format(arg),
        r_length(names)
      );
    }

    r_obj* const* v_names = r_list_cbegin(names);
    for (int i = 0; i < dimensionality; ++i) {
      check_axis_names(v_names[i], i + 1, v_dimensions[i], error_call);
    }

    FREE(1);
  }

  r_obj* out = KEEP(r_view(x));
  r_attrib_poke_dim(out, r_dim(x));
  r_attrib_poke_dim_names(out, names);

  FREE(2);
  return out;
}

r_obj* ffi_rray_set_axis_names(
  r_obj* ffi_x,
  r_obj* ffi_axis,
  r_obj* ffi_names,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_set_axis_names(
    ffi_x,
    ffi_axis,
    ffi_names,
    rray_args.x,
    error_call
  );
}

r_obj* ffi_rray_set_row_names(
  r_obj* ffi_x,
  r_obj* ffi_names,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_set_axis_names(
    ffi_x,
    rray_globals.one_int,
    ffi_names,
    rray_args.x,
    error_call
  );
}

r_obj* ffi_rray_set_column_names(
  r_obj* ffi_x,
  r_obj* ffi_names,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_set_axis_names(
    ffi_x,
    rray_globals.two_int,
    ffi_names,
    rray_args.x,
    error_call
  );
}

r_obj* rray_set_axis_names(
  r_obj* x,
  r_obj* axis,
  r_obj* names,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));

  const int dimensionality = rray_dimensionality(x, arg, error_call);

  r_obj* old_names = r_dim_names(x);
  r_obj* titles = (old_names == r_null) ? r_null : r_names(old_names);

  const int c_axis =
    arg_as_axis(axis, titles, dimensionality, rray_args.axis, arg, error_call);

  const int dimension = rray_dimension(x, c_axis, arg, error_call);
  check_axis_names(names, c_axis, dimension, error_call);

  r_obj* new_names = KEEP(
    old_names == r_null ? r_alloc_list(dimensionality) : r_clone(old_names)
  );
  r_list_poke(new_names, c_axis - 1, names);

  r_obj* out = KEEP(r_view(x));
  r_attrib_poke_dim(out, r_dim(x));
  r_attrib_poke_dim_names(out, new_names);

  FREE(3);
  return out;
}

static inline void check_axis_names(
  r_obj* names,
  int axis,
  int dimension,
  struct r_lazy error_call
) {
  if (names == r_null) {
    return;
  }

  if (r_typeof(names) != R_TYPE_character) {
    r_abort_lazy_call(
      error_call,
      "Names for axis %d must be a character vector or `NULL`, not %s.",
      axis,
      r_obj_type_friendly(names)
    );
  }

  if (r_length(names) != dimension) {
    r_abort_lazy_call(
      error_call,
      "Names for axis %d must have length %d, not length %" R_PRI_SSIZE ".",
      axis,
      dimension,
      r_length(names)
    );
  }
}
