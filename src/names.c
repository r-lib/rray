#include "names.h"

#include "axes.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "reduction-iterator.h"
#include "utils.h"
#include "wrapper.h"

#include "decl/names-decl.h"

r_obj* ffi_rray_names(r_obj* ffi_x, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_names(ffi_x, error_call);
}

r_obj* rray_names(r_obj* x, struct r_lazy error_call) {
  check_unclassed(x, "x", error_call);
  x = KEEP(arg_as_array(x, "x", error_call));
  r_obj* out = r_dim_names(x);
  FREE(1);
  return out;
}

r_obj* ffi_rray_axis_names(r_obj* ffi_x, r_obj* ffi_axis, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const int axis = arg_as_int(ffi_axis, axis_chr, error_call);
  return rray_axis_names(ffi_x, axis, error_call);
}

r_obj* ffi_rray_row_names(r_obj* ffi_x, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_axis_names(ffi_x, 1, error_call);
}

r_obj* ffi_rray_col_names(r_obj* ffi_x, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_axis_names(ffi_x, 2, error_call);
}

r_obj* rray_axis_names(r_obj* x, int axis, struct r_lazy error_call) {
  check_unclassed(x, "x", error_call);
  x = KEEP(arg_as_array(x, "x", error_call));

  const int dimensionality = rray_dimensionality(x, error_call);
  check_axis(axis, dimensionality, "axis", error_call);

  r_obj* names = rray_names(x, error_call);

  r_obj* out = (names == r_null) ? r_null : r_list_get(names, axis - 1);

  FREE(1);
  return out;
}

r_obj* ffi_rray_set_names(r_obj* ffi_x, r_obj* ffi_names, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_set_names(ffi_x, ffi_names, error_call);
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
    const int dimensionality = rray_dimensionality_from_dimensions(dimensions);

    if (r_length(names) != dimensionality) {
      r_abort_lazy_call(
        error_call,
        "`names` must have length %d to match the dimensionality of `x`, "
        "not length %" R_PRIdXLEN_T ".",
        dimensionality,
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
  const int axis = arg_as_int(ffi_axis, axis_chr, error_call);
  return rray_set_axis_names(ffi_x, axis, ffi_names, error_call);
}

r_obj* ffi_rray_set_row_names(
  r_obj* ffi_x,
  r_obj* ffi_names,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_set_axis_names(ffi_x, 1, ffi_names, error_call);
}

r_obj* ffi_rray_set_col_names(
  r_obj* ffi_x,
  r_obj* ffi_names,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_set_axis_names(ffi_x, 2, ffi_names, error_call);
}

r_obj* rray_set_axis_names(
  r_obj* x,
  int axis,
  r_obj* names,
  struct r_lazy error_call
) {
  check_unclassed(x, "x", error_call);
  x = KEEP(arg_as_array(x, "x", error_call));

  const int dimensionality = rray_dimensionality(x, error_call);
  check_axis(axis, dimensionality, "axis", error_call);

  const int dimension = rray_dimension(x, axis, error_call);
  check_axis_names(names, axis, dimension, error_call);

  r_obj* old_names = KEEP(rray_names(x, error_call));

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

r_obj* rray_broadcast_names(
  r_obj* const* v_names,
  const int* v_dimensions,
  int dimensionality,
  const int* v_out_dimensions,
  int out_dimensionality
) {
  r_obj* out = r_null;
  r_keep_loc out_loc;
  KEEP_HERE(out, &out_loc);

  for (int i = 0; i < dimensionality; ++i) {
    if (v_names[i] == r_null) {
      continue;
    }
    if (v_dimensions[i] != v_out_dimensions[i]) {
      continue;
    }

    if (out == r_null) {
      out = r_alloc_list(out_dimensionality);
      KEEP_AT(out, out_loc);
    }

    r_list_poke(out, i, v_names[i]);
  }

  FREE(1);
  return out;
}

r_obj* rray_reduce_names(
  r_obj* const* v_names,
  int dimensionality,
  const int* v_axes,
  r_ssize axes_size
) {
  r_obj* out = r_null;
  r_keep_loc out_loc;
  KEEP_HERE(out, &out_loc);

  for (int i = 0; i < dimensionality; ++i) {
    if (v_names[i] == r_null) {
      continue;
    }
    if (axis_is_reduced(i, v_axes, axes_size)) {
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

void rray_split_names(
  r_obj* out,
  r_obj* const* v_x_names,
  const int* v_out_dimensions,
  int dimensionality,
  r_ssize out_size
) {
  if (!any_axis_has_names(v_x_names, dimensionality)) {
    return;
  }

  r_obj* const* v_out = r_list_cbegin(out);

  struct rray_iterator it;
  rray_reduction_iterator_init(
    &it,
    v_out_dimensions,
    v_out_dimensions,
    dimensionality
  );
  const r_ssize* v_point = rray_iterator_point(&it);

  for (r_ssize i = 0; i < out_size; ++i) {
    r_obj* names = KEEP(r_alloc_list(dimensionality));

    for (int j = 0; j < dimensionality; ++j) {
      r_obj* x_axis_names = v_x_names[j];

      if (x_axis_names == r_null) {
        continue;
      }

      if (v_out_dimensions[j] == 1) {
        // Axis isn't split, its names carry over whole
        r_list_poke(names, j, x_axis_names);
        continue;
      }

      r_obj* axis_names = KEEP(r_alloc_character(1));
      r_chr_poke(axis_names, 0, r_chr_get(x_axis_names, v_point[j]));
      r_list_poke(names, j, axis_names);
      FREE(1);
    }

    r_attrib_poke_dim_names(v_out[i], names);

    FREE(1);
    rray_iterator_next(&it);
  }
}

static inline bool axis_is_reduced(
  int axis,
  const int* v_axes,
  r_ssize axes_size
) {
  for (r_ssize i = 0; i < axes_size; ++i) {
    if (v_axes[i] - 1 == axis) {
      return true;
    }
  }

  return false;
}

static inline bool any_axis_has_names(
  r_obj* const* v_names,
  int dimensionality
) {
  for (int i = 0; i < dimensionality; ++i) {
    if (v_names[i] != r_null) {
      return true;
    }
  }

  return false;
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
