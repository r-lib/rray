#include "names.h"

#include "axes.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "utils.h"
#include "wrapper.h"

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
  const int axis = arg_as_int(ffi_axis, rray_args.axis, error_call);
  return rray_axis_names(ffi_x, axis, rray_args.x, error_call);
}

r_obj* ffi_rray_row_names(r_obj* ffi_x, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_axis_names(ffi_x, 1, rray_args.x, error_call);
}

r_obj* ffi_rray_col_names(r_obj* ffi_x, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_axis_names(ffi_x, 2, rray_args.x, error_call);
}

r_obj* rray_axis_names(
  r_obj* x,
  int axis,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));

  const int dimensionality = rray_dimensionality(x, arg, error_call);
  check_axis(axis, dimensionality, rray_args.axis, error_call);

  r_obj* names = rray_names(x, arg, error_call);

  r_obj* out = (names == r_null) ? r_null : r_list_get(names, axis - 1);

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
        "not length %" R_PRIdXLEN_T ".",
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

  r_obj* out = KEEP(r_wrap(x));
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
  const int axis = arg_as_int(ffi_axis, rray_args.axis, error_call);
  return rray_set_axis_names(ffi_x, axis, ffi_names, rray_args.x, error_call);
}

r_obj* ffi_rray_set_row_names(
  r_obj* ffi_x,
  r_obj* ffi_names,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_set_axis_names(ffi_x, 1, ffi_names, rray_args.x, error_call);
}

r_obj* ffi_rray_set_col_names(
  r_obj* ffi_x,
  r_obj* ffi_names,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_set_axis_names(ffi_x, 2, ffi_names, rray_args.x, error_call);
}

r_obj* rray_set_axis_names(
  r_obj* x,
  int axis,
  r_obj* names,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));

  const int dimensionality = rray_dimensionality(x, arg, error_call);
  check_axis(axis, dimensionality, rray_args.axis, error_call);

  const int dimension = rray_dimension(x, axis, arg, error_call);
  check_axis_names(names, axis, dimension, error_call);

  r_obj* old_names = KEEP(rray_names(x, arg, error_call));

  r_obj* new_names = KEEP(r_alloc_list(dimensionality));
  if (old_names != r_null) {
    r_obj* const* v_old_names = r_list_cbegin(old_names);
    for (int i = 0; i < dimensionality; ++i) {
      r_list_poke(new_names, i, v_old_names[i]);
    }
  }
  r_list_poke(new_names, axis - 1, names);

  r_obj* out = KEEP(r_wrap(x));
  r_attrib_poke_dim_names(out, new_names);

  FREE(4);
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
      "Names for axis %d must have length %d, not length %" R_PRIdXLEN_T ".",
      axis,
      dimension,
      r_length(names)
    );
  }
}

r_obj* ffi_rray_broadcast_names2(
  r_obj* ffi_x,
  r_obj* ffi_y,
  r_obj* ffi_dimensions,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_broadcast_names2(
    ffi_x,
    ffi_y,
    ffi_dimensions,
    rray_args.x,
    rray_args.y,
    error_call
  );
}

r_obj* rray_broadcast_names2(
  r_obj* x,
  r_obj* y,
  r_obj* dimensions,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  r_obj* x_names = KEEP(rray_names(x, x_arg, error_call));
  r_obj* y_names = KEEP(rray_names(y, y_arg, error_call));

  dimensions =
    KEEP(arg_as_dimensions(dimensions, rray_args.dimensions, error_call));

  if (x_names == r_null && y_names == r_null) {
    FREE(3);
    return r_null;
  }

  r_obj* x_dimensions = KEEP(rray_dimensions(x, x_arg, error_call));
  r_obj* y_dimensions = KEEP(rray_dimensions(y, y_arg, error_call));

  const int* v_dimensions = r_int_cbegin(dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(dimensions);

  x_names = KEEP(rray_broadcast_names(
    x_names,
    r_int_cbegin(x_dimensions),
    rray_dimensionality_from_dimensions(x_dimensions),
    v_dimensions,
    dimensionality
  ));

  y_names = KEEP(rray_broadcast_names(
    y_names,
    r_int_cbegin(y_dimensions),
    rray_dimensionality_from_dimensions(y_dimensions),
    v_dimensions,
    dimensionality
  ));

  r_obj* out = rray_names_coalesce(x_names, y_names, dimensionality);

  FREE(7);
  return out;
}

r_obj* rray_broadcast_names(
  r_obj* names,
  const int* v_x_dimensions,
  int x_dimensionality,
  const int* v_dimensions,
  int dimensionality
) {
  if (names == r_null) {
    return r_null;
  }

  r_obj* const* v_names = r_list_cbegin(names);

  const int n =
    (x_dimensionality < dimensionality) ? x_dimensionality : dimensionality;

  r_obj* out = r_null;
  r_keep_loc out_loc;
  KEEP_HERE(out, &out_loc);

  for (int i = 0; i < n; ++i) {
    if (v_names[i] == r_null) {
      // `out` stays `r_null` when there were no names before
      continue;
    }
    if (v_x_dimensions[i] != v_dimensions[i]) {
      // `out` is "cleared" to `r_null` when dimension changes
      continue;
    }
    if (out == r_null) {
      out = r_alloc_list(dimensionality);
      KEEP_AT(out, out_loc);
    }
    r_list_poke(out, i, v_names[i]);
  }

  FREE(1);
  return out;
}

static r_obj* rray_names_coalesce(
  r_obj* x_names,
  r_obj* y_names,
  int dimensionality
) {
  if (x_names == r_null) {
    return y_names;
  }
  if (y_names == r_null) {
    return x_names;
  }

  r_obj* const* v_x_names = r_list_cbegin(x_names);
  r_obj* const* v_y_names = r_list_cbegin(y_names);

  r_obj* out = KEEP(r_alloc_list(dimensionality));

  for (int i = 0; i < dimensionality; ++i) {
    r_list_poke(out, i, (v_x_names[i] == r_null) ? v_y_names[i] : v_x_names[i]);
  }

  FREE(1);
  return out;
}
