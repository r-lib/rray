#include "arithmetic.h"

#include <limits.h>

#include "arithmetic-ptype.h"
#include "broadcast-names.h"
#include "cast.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "iterator.h"
#include "size.h"

#include "decl/arithmetic-decl.h"

static r_obj* rray_binary_arithmetic(
  enum rray_binary_arithmetic_op op,
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  r_obj* ptype =
    rray_binary_arithmetic_ptype(op, x, y, x_arg, y_arg, error_call);

  x = KEEP(rray_cast(x, ptype, x_arg, rray_args.empty, error_call));
  y = KEEP(rray_cast(y, ptype, y_arg, rray_args.empty, error_call));

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

  const r_ssize size = rray_size_from_dimensions(v_dimensions, dimensionality);

  struct rray_iterator2 it;
  rray_iterator2_init(
    &it,
    v_dimensions,
    dimensionality,
    v_x_dimensions,
    x_dimensionality,
    v_y_dimensions,
    y_dimensionality
  );

  r_obj* out;

  switch (op) {
  case RRAY_BINARY_ARITHMETIC_OP_add:
    out = rray_add_switch(x, y, size, &it, error_call);
    break;
  case RRAY_BINARY_ARITHMETIC_OP_subtract:
  case RRAY_BINARY_ARITHMETIC_OP_multiply:
  case RRAY_BINARY_ARITHMETIC_OP_divide:
  case RRAY_BINARY_ARITHMETIC_OP_power:
  case RRAY_BINARY_ARITHMETIC_OP_modulo:
  case RRAY_BINARY_ARITHMETIC_OP_integer_divide:
    r_stop_unreachable();
  }

  KEEP(out);
  r_attrib_poke_dim(out, dimensions);

  r_obj* out_names = KEEP(rray_broadcast_names2(x, y, dimensions));

  if (out_names != r_null) {
    r_attrib_poke_dim_names(out, out_names);
  }

  FREE(5);
  return out;
}

#define RRAY_ARITHMETIC(RTYPE, CTYPE, CONST_DEREF, DEREF, ONE)                 \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, size));                              \
  CTYPE* v_out = DEREF(out);                                                   \
                                                                               \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
  const CTYPE* v_y = CONST_DEREF(y);                                           \
                                                                               \
  for (r_ssize i = 0; i < size; ++i) {                                         \
    v_out[i] = ONE(                                                            \
      v_x[rray_iterator2_location1(it)],                                       \
      v_y[rray_iterator2_location2(it)],                                       \
      error_call                                                               \
    );                                                                         \
    rray_iterator2_next(it);                                                   \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

static r_no_return void stop_int_overflow(struct r_lazy error_call) {
  r_abort_lazy_call(error_call, "Integer overflow.");
}

// --------------------------------------------------------------------------

r_obj* ffi_rray_add(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_add(ffi_x, ffi_y, rray_args.x, rray_args.y, error_call);
}

r_obj* rray_add(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  return rray_binary_arithmetic(
    RRAY_BINARY_ARITHMETIC_OP_add,
    x,
    y,
    x_arg,
    y_arg,
    error_call
  );
}

static r_obj* rray_add_switch(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
) {
  switch (r_typeof(x)) {
  case R_TYPE_integer:
    return rray_add_int(x, y, size, it, error_call);
  case R_TYPE_double:
    return rray_add_dbl(x, y, size, it, error_call);
  case R_TYPE_complex:
    return rray_add_cpl(x, y, size, it, error_call);
  default:
    r_stop_unreachable();
  }
}

static r_obj* rray_add_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
) {
  RRAY_ARITHMETIC(
    R_TYPE_integer,
    int,
    r_int_cbegin,
    r_int_begin,
    rray_add_int_one
  );
}

static r_obj* rray_add_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
) {
  RRAY_ARITHMETIC(
    R_TYPE_double,
    double,
    r_dbl_cbegin,
    r_dbl_begin,
    rray_add_dbl_one
  );
}

static r_obj* rray_add_cpl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
) {
  RRAY_ARITHMETIC(
    R_TYPE_complex,
    r_complex,
    r_cpl_cbegin,
    r_cpl_begin,
    rray_add_cpl_one
  );
}

static inline int rray_add_int_one(int x, int y, struct r_lazy error_call) {
  if (x == r_globals.na_int || y == r_globals.na_int) {
    return r_globals.na_int;
  }

  if ((y > 0 && x > INT_MAX - y) || (y < 0 && x < -INT_MAX - y)) {
    stop_int_overflow(error_call);
  }

  return x + y;
}

static inline double rray_add_dbl_one(
  double x,
  double y,
  struct r_lazy error_call
) {
  return x + y;
}

static inline r_complex rray_add_cpl_one(
  r_complex x,
  r_complex y,
  struct r_lazy error_call
) {
  return (r_complex){.r = x.r + y.r, .i = x.i + y.i};
}

#undef RRAY_ARITHMETIC
