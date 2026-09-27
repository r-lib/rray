#include "slice-subscript.h"

#include "dimensionality.h"
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
