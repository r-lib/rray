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

r_obj* ffi_rray_broadcast_names(r_obj* ffi_x, r_obj* ffi_dimensions) {
  return rray_broadcast_names(ffi_x, ffi_dimensions);
}

r_obj* rray_broadcast_names(r_obj* x, r_obj* dimensions) {
  return rray_broadcast_names_fill(r_null, x, dimensions);
}

r_obj* ffi_rray_broadcast_names2(
  r_obj* ffi_x,
  r_obj* ffi_y,
  r_obj* ffi_dimensions
) {
  return rray_broadcast_names2(ffi_x, ffi_y, ffi_dimensions);
}

r_obj* rray_broadcast_names2(r_obj* x, r_obj* y, r_obj* dimensions) {
  r_obj* out = KEEP(rray_broadcast_names(x, dimensions));
  out = rray_broadcast_names_fill(out, y, dimensions);
  FREE(1);
  return out;
}

r_obj* ffi_rray_broadcast_names_common(r_obj* ffi_xs, r_obj* ffi_dimensions) {
  return rray_broadcast_names_common(ffi_xs, ffi_dimensions);
}

r_obj* rray_broadcast_names_common(r_obj* xs, r_obj* dimensions) {
  const r_ssize n = r_length(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);

  r_obj* out = r_null;
  r_keep_loc out_loc;
  KEEP_HERE(out, &out_loc);

  for (r_ssize i = 0; i < n; ++i) {
    out = rray_broadcast_names_fill(out, v_xs[i], dimensions);
    KEEP_AT(out, out_loc);
  }

  FREE(1);
  return out;
}

static r_obj* rray_broadcast_names_fill(
  r_obj* out,
  r_obj* x,
  r_obj* dimensions
) {
  r_obj* x_names = r_dim_names(x);

  if (x_names == r_null) {
    return out;
  }

  r_obj* const* v_x_names = r_list_cbegin(x_names);

  r_obj* x_dimensions = r_dim(x);
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);
  const int x_dimensionality =
    rray_dimensionality_from_dimensions(x_dimensions);

  const int* v_dimensions = r_int_cbegin(dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(dimensions);

  r_keep_loc out_loc;
  KEEP_HERE(out, &out_loc);

  for (int i = 0; i < x_dimensionality; ++i) {
    if (v_x_names[i] == r_null) {
      // `out` stays `r_null` when there were no names before
      continue;
    }
    if (v_x_dimensions[i] != v_dimensions[i]) {
      // `out` is "cleared" to `r_null` when dimension changes
      continue;
    }
    if (out != r_null && r_list_get(out, i) != r_null) {
      continue;
    }
    if (out == r_null) {
      out = r_alloc_list(dimensionality);
      KEEP_AT(out, out_loc);
    }
    r_list_poke(out, i, v_x_names[i]);
  }

  FREE(1);
  return out;
}
