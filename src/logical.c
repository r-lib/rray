#include "logical.h"

#include "binary.h"
#include "broadcast-names.h"
#include "cast.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "strided-iterator.h"
#include "type.h"

enum rray_logical_op {
  RRAY_LOGICAL_and,
  RRAY_LOGICAL_or,
  RRAY_LOGICAL_xor
};

#include "decl/logical-decl.h"

r_obj* ffi_rray_and(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_and(ffi_x, ffi_y, rray_args.x, rray_args.y, error_call);
}

r_obj* rray_and(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  return rray_logical(x, y, RRAY_LOGICAL_and, x_arg, y_arg, error_call);
}

r_obj* ffi_rray_or(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_or(ffi_x, ffi_y, rray_args.x, rray_args.y, error_call);
}

r_obj* rray_or(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  return rray_logical(x, y, RRAY_LOGICAL_or, x_arg, y_arg, error_call);
}

r_obj* ffi_rray_xor(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_xor(ffi_x, ffi_y, rray_args.x, rray_args.y, error_call);
}

r_obj* rray_xor(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  return rray_logical(x, y, RRAY_LOGICAL_xor, x_arg, y_arg, error_call);
}

static r_obj* rray_logical(
  r_obj* x,
  r_obj* y,
  enum rray_logical_op op,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  check_unclassed(x, x_arg, error_call);
  check_unclassed(y, y_arg, error_call);

  x = KEEP(arg_as_array(x, x_arg, error_call));
  y = KEEP(arg_as_array(y, y_arg, error_call));

  check_logical(x, x_arg, error_call);
  check_logical(y, y_arg, error_call);

  r_obj* x_dimensions = r_dim(x);
  r_obj* y_dimensions = r_dim(y);

  const int* v_x_dimensions = r_int_cbegin(x_dimensions);
  const int* v_y_dimensions = r_int_cbegin(y_dimensions);

  const int x_dimensionality =
    rray_dimensionality_from_dimensions(x_dimensions);
  const int y_dimensionality =
    rray_dimensionality_from_dimensions(y_dimensions);

  r_obj* dimensions = KEEP(rray_dimensions2(
    v_x_dimensions,
    x_dimensionality,
    v_y_dimensions,
    y_dimensionality,
    x_arg,
    y_arg,
    error_call
  ));
  const int* v_dimensions = r_int_cbegin(dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(dimensions);

  struct rray_strided_iterator2_plan plan = rray_broadcast_iterator2_plan(
    v_x_dimensions,
    x_dimensionality,
    v_y_dimensions,
    y_dimensionality,
    v_dimensions,
    dimensionality
  );

  r_obj* out = KEEP(rray_logical_lgl_lgl(x, y, &plan, op));
  r_attrib_poke_dim(out, dimensions);

  r_obj* out_names = KEEP(rray_broadcast_names2(x, y, dimensions));

  if (out_names != r_null) {
    r_attrib_poke_dim_names(out, out_names);
  }

  FREE(5);
  return out;
}

void check_logical(r_obj* x, struct rray_arg* arg, struct r_lazy error_call) {
  switch (rray_typeof(x)) {
  case RRAY_TYPE_logical:
    return;

  case RRAY_TYPE_integer:
  case RRAY_TYPE_double:
  case RRAY_TYPE_complex:
  case RRAY_TYPE_character:
  case RRAY_TYPE_raw:
  case RRAY_TYPE_list:
    stop_non_logical(x, arg, error_call);

  case RRAY_TYPE_scalar:
    stop_scalar_input(x, arg, error_call);
  }

  r_stop_unreachable();
}

static r_no_return void stop_non_logical(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  r_abort_lazy_call(
    error_call,
    "%s must be a logical array, not %s.",
    rray_arg_format_input(arg),
    r_obj_type_friendly(x)
  );
}

static r_obj* rray_logical_lgl_lgl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  enum rray_logical_op op
) {
  switch (op) {
  case RRAY_LOGICAL_and: {
    RRAY_BINARY(
      int,
      r_lgl_cbegin,
      rray_cast_lgl_to_lgl_one,
      int,
      r_lgl_cbegin,
      rray_cast_lgl_to_lgl_one,
      R_TYPE_logical,
      int,
      r_lgl_begin,
      rray_and_lgl_one,
      RRAY_BINARY_NO_ARGS
    );
  }
  case RRAY_LOGICAL_or: {
    RRAY_BINARY(
      int,
      r_lgl_cbegin,
      rray_cast_lgl_to_lgl_one,
      int,
      r_lgl_cbegin,
      rray_cast_lgl_to_lgl_one,
      R_TYPE_logical,
      int,
      r_lgl_begin,
      rray_or_lgl_one,
      RRAY_BINARY_NO_ARGS
    );
  }
  case RRAY_LOGICAL_xor: {
    RRAY_BINARY(
      int,
      r_lgl_cbegin,
      rray_cast_lgl_to_lgl_one,
      int,
      r_lgl_cbegin,
      rray_cast_lgl_to_lgl_one,
      R_TYPE_logical,
      int,
      r_lgl_begin,
      rray_xor_lgl_one,
      RRAY_BINARY_NO_ARGS
    );
  }
  }

  r_stop_unreachable();
}
