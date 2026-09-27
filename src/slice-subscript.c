#include "slice-subscript.h"

#include "dimensionality.h"
#include "missing.h"
#include "utils.h"

#include "decl/slice-subscript-decl.h"

r_obj* ffi_rray_as_slice_subscript(
  r_obj* ffi_i,
  r_obj* ffi_dimension,
  r_obj* ffi_names,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};

  const int dimension =
    arg_as_int(ffi_dimension, rray_args.dimension, error_call);

  const struct rray_subscript subscript = rray_as_slice_subscript(
    ffi_i,
    dimension,
    ffi_names,
    rray_args.i,
    error_call
  );
  KEEP(subscript.index);

  r_obj* out = rray_subscript_as_list(subscript);

  FREE(1);
  return out;
}

struct rray_subscript rray_as_slice_subscript(
  r_obj* index,
  int dimension,
  r_obj* names,
  struct rray_arg* index_arg,
  struct r_lazy error_call
) {
  check_unclassed(index, index_arg, error_call);

  r_obj* index_dimensions = r_dim(index);

  if (index_dimensions != r_null) {
    const int index_dimensionality =
      rray_dimensionality_from_dimensions(index_dimensions);

    if (index_dimensionality != 1) {
      r_abort_lazy_call(
        error_call,
        "%s must be a vector or a 1D array, not an array with a "
        "dimensionality of %d.",
        rray_arg_format(index_arg),
        index_dimensionality
      );
    }
  }

  switch (r_typeof(index)) {
  case R_TYPE_null:
    return (struct rray_subscript){
      .index = r_globals.empty_int,
      .kind = RRAY_SUBSCRIPT_KIND_locations_int,
      .size = 0
    };
  case R_TYPE_logical:
    return rray_as_subscript_mask(index, dimension, index_arg, error_call);
  case R_TYPE_integer:
  case R_TYPE_double:
    return rray_as_subscript_locations(index, dimension, index_arg, error_call);
  case R_TYPE_character:
    return rray_as_subscript_names(index, names, index_arg, error_call);
  default:
    r_abort_lazy_call(
      error_call,
      "%s must be logical, integer, double, character, or `NULL`, not %s.",
      rray_arg_format(index_arg),
      r_obj_type_friendly(index)
    );
  }
}

static struct rray_subscript rray_as_subscript_names(
  r_obj* index,
  r_obj* names,
  struct rray_arg* index_arg,
  struct r_lazy error_call
) {
  if (names == r_null) {
    r_abort_lazy_call(
      error_call,
      "Character %s can't select from an axis without names.",
      rray_arg_format(index_arg)
    );
  }

  const r_ssize size = r_length(index);
  r_obj* const* v_index = r_chr_cbegin(index);

  r_obj* out = KEEP(Rf_match(names, index, 0));
  int* v_out = r_int_begin(out);

  for (r_ssize i = 0; i < size; ++i) {
    r_obj* elt = v_index[i];

    if (elt == r_globals.na_str) {
      v_out[i] = r_globals.na_int;
      continue;
    }
    if (elt == r_strs.empty) {
      r_abort_lazy_call(
        error_call,
        "%s must not contain the empty string.",
        rray_arg_format(index_arg)
      );
    }
    if (v_out[i] == 0) {
      r_abort_lazy_call(
        error_call,
        "%s must only contain names of the axis, not \"%s\".",
        rray_arg_format(index_arg),
        r_str_c_string(elt)
      );
    }
  }

  const struct rray_subscript subscript =
    {.index = out, .kind = RRAY_SUBSCRIPT_KIND_locations_int, .size = size};

  FREE(1);
  return subscript;
}

void check_slice_indices_unnamed(r_obj* indices, struct r_lazy error_call) {
  if (r_names(indices) != r_null) {
    r_abort_lazy_call(error_call, "All elements of `...` must be unnamed.");
  }
}

void check_slice_indices_size(
  r_obj* indices,
  int dimensionality,
  struct r_lazy error_call
) {
  const r_ssize indices_size = r_length(indices);

  if (indices_size != dimensionality) {
    r_abort_lazy_call(
      error_call,
      "Must supply exactly %d subscript%s to `...`, not %" R_PRI_SSIZE ".",
      dimensionality,
      dimensionality == 1 ? "" : "s",
      indices_size
    );
  }
}

r_obj* rray_slice_as_locations(struct rray_subscript subscript) {
  switch (subscript.kind) {
  case RRAY_SUBSCRIPT_KIND_locations_int:
    return subscript.index;
  case RRAY_SUBSCRIPT_KIND_locations_dbl: {
    const double* v_index = r_dbl_cbegin(subscript.index);

    r_obj* out = KEEP(r_alloc_integer(subscript.size));
    int* v_out = r_int_begin(out);

    for (r_ssize i = 0; i < subscript.size; ++i) {
      const double location = v_index[i];
      v_out[i] =
        rray_dbl_is_missing(location) ? r_globals.na_int : (int) location;
    }

    FREE(1);
    return out;
  }
  case RRAY_SUBSCRIPT_KIND_mask: {
    const int* v_index = r_lgl_cbegin(subscript.index);

    r_obj* out = KEEP(r_alloc_integer(subscript.size));
    int* v_out = r_int_begin(out);

    if (r_length(subscript.index) == 1) {
      const int elt = v_index[0];

      if (elt == 1) {
        r_stop_internal("A scalar `TRUE` should have been handled already.");
      } else if (elt == 0) {
        // Nothing to do
      } else if (elt == r_globals.na_lgl) {
        for (r_ssize i = 0; i < subscript.size; ++i) {
          v_out[i] = r_globals.na_int;
        }
      } else {
        r_stop_unreachable();
      }
    } else {
      r_ssize i = 0;
      r_ssize location = 0;

      while (i < subscript.size) {
        const int elt = v_index[location];
        v_out[i] =
          elt == r_globals.na_lgl ? r_globals.na_int : (int) location + 1;
        i += elt != 0;
        ++location;
      }
    }

    FREE(1);
    return out;
  }
  case RRAY_SUBSCRIPT_KIND_points_int:
  case RRAY_SUBSCRIPT_KIND_points_dbl:
    r_stop_unreachable();
  }

  r_stop_unreachable();
}

bool rray_slice_locations_any_missing(
  const int* const* v_v_locations,
  const int* v_dimensions,
  int dimensionality
) {
  for (int axis = 0; axis < dimensionality; ++axis) {
    const int* v_locations = v_v_locations[axis];
    if (v_locations == NULL) {
      continue;
    }

    const int dimension = v_dimensions[axis];

    for (int i = 0; i < dimension; ++i) {
      if (v_locations[i] == r_globals.na_int) {
        return true;
      }
    }
  }

  return false;
}
