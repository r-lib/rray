#include "compare.h"

#include "binary.h"
#include "broadcast-names.h"
#include "cast.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "strided-iterator.h"
#include "missing.h"
#include "type.h"
#include "typeof2.h"
#include "utils.h"

enum rray_compare_op {
  RRAY_COMPARE_greater_than,
  RRAY_COMPARE_greater_than_or_equal,
  RRAY_COMPARE_less_than,
  RRAY_COMPARE_less_than_or_equal
};

#include "decl/compare-decl.h"

r_obj* ffi_rray_greater_than(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_greater_than(ffi_x, ffi_y, rray_args.x, rray_args.y, error_call);
}

r_obj* rray_greater_than(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  return rray_compare(
    x,
    y,
    RRAY_COMPARE_greater_than,
    x_arg,
    y_arg,
    error_call
  );
}

r_obj* ffi_rray_greater_than_or_equal(
  r_obj* ffi_x,
  r_obj* ffi_y,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_greater_than_or_equal(
    ffi_x,
    ffi_y,
    rray_args.x,
    rray_args.y,
    error_call
  );
}

r_obj* rray_greater_than_or_equal(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  return rray_compare(
    x,
    y,
    RRAY_COMPARE_greater_than_or_equal,
    x_arg,
    y_arg,
    error_call
  );
}

r_obj* ffi_rray_less_than(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_less_than(ffi_x, ffi_y, rray_args.x, rray_args.y, error_call);
}

r_obj* rray_less_than(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  return rray_compare(x, y, RRAY_COMPARE_less_than, x_arg, y_arg, error_call);
}

r_obj* ffi_rray_less_than_or_equal(
  r_obj* ffi_x,
  r_obj* ffi_y,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_less_than_or_equal(
    ffi_x,
    ffi_y,
    rray_args.x,
    rray_args.y,
    error_call
  );
}

r_obj* rray_less_than_or_equal(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  return rray_compare(
    x,
    y,
    RRAY_COMPARE_less_than_or_equal,
    x_arg,
    y_arg,
    error_call
  );
}

static r_obj* rray_compare(
  r_obj* x,
  r_obj* y,
  enum rray_compare_op op,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  check_unclassed(x, x_arg, error_call);
  check_unclassed(y, y_arg, error_call);

  x = KEEP(arg_as_array(x, x_arg, error_call));
  y = KEEP(arg_as_array(y, y_arg, error_call));

  const rray_compare_fn fn =
    rray_compare_switch(x, y, op, x_arg, y_arg, error_call);

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

  r_obj* out = KEEP(fn(x, y, &plan, op));
  r_attrib_poke_dim(out, dimensions);

  r_obj* out_names = KEEP(rray_broadcast_names2(x, y, dimensions));

  if (out_names != r_null) {
    r_attrib_poke_dim_names(out, out_names);
  }

  FREE(5);
  return out;
}

static rray_compare_fn rray_compare_switch(
  r_obj* x,
  r_obj* y,
  enum rray_compare_op op,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  const enum rray_type x_type = rray_typeof(x);
  const enum rray_type y_type = rray_typeof(y);

  enum rray_side side;

  switch (rray_typeof2(x_type, y_type, &side)) {
  case RRAY_TYPE2_logical_logical:
    return rray_compare_lgl_lgl;
  case RRAY_TYPE2_logical_integer:
    return side == RRAY_SIDE_right ? rray_compare_lgl_int
                                   : rray_compare_int_lgl;
  case RRAY_TYPE2_logical_double:
    return side == RRAY_SIDE_right ? rray_compare_lgl_dbl
                                   : rray_compare_dbl_lgl;
  case RRAY_TYPE2_integer_integer:
    return rray_compare_int_int;
  case RRAY_TYPE2_integer_double:
    return side == RRAY_SIDE_right ? rray_compare_int_dbl
                                   : rray_compare_dbl_int;
  case RRAY_TYPE2_double_double:
    return rray_compare_dbl_dbl;

  case RRAY_TYPE2_logical_complex:
  case RRAY_TYPE2_logical_character:
  case RRAY_TYPE2_logical_raw:
  case RRAY_TYPE2_logical_list:
  case RRAY_TYPE2_integer_complex:
  case RRAY_TYPE2_integer_character:
  case RRAY_TYPE2_integer_raw:
  case RRAY_TYPE2_integer_list:
  case RRAY_TYPE2_double_complex:
  case RRAY_TYPE2_double_character:
  case RRAY_TYPE2_double_raw:
  case RRAY_TYPE2_double_list:
  case RRAY_TYPE2_complex_complex:
  case RRAY_TYPE2_complex_character:
  case RRAY_TYPE2_complex_raw:
  case RRAY_TYPE2_complex_list:
  case RRAY_TYPE2_character_character:
  case RRAY_TYPE2_character_raw:
  case RRAY_TYPE2_character_list:
  case RRAY_TYPE2_raw_raw:
  case RRAY_TYPE2_raw_list:
  case RRAY_TYPE2_list_list:
    stop_unsupported_compare(
      rray_compare_op_as_c_string(op),
      x,
      y,
      x_arg,
      y_arg,
      error_call
    );

  case RRAY_TYPE2_logical_scalar:
  case RRAY_TYPE2_integer_scalar:
  case RRAY_TYPE2_double_scalar:
  case RRAY_TYPE2_complex_scalar:
  case RRAY_TYPE2_character_scalar:
  case RRAY_TYPE2_raw_scalar:
  case RRAY_TYPE2_list_scalar:
  case RRAY_TYPE2_scalar_scalar:
    if (x_type == RRAY_TYPE_scalar) {
      stop_scalar_input(x, x_arg, error_call);
    } else {
      stop_scalar_input(y, y_arg, error_call);
    }
  }

  r_stop_unreachable();
}

static const char* rray_compare_op_as_c_string(enum rray_compare_op op) {
  switch (op) {
  case RRAY_COMPARE_greater_than:
    return ">";
  case RRAY_COMPARE_greater_than_or_equal:
    return ">=";
  case RRAY_COMPARE_less_than:
    return "<";
  case RRAY_COMPARE_less_than_or_equal:
    return "<=";
  }

  r_stop_unreachable();
}

static r_no_return void stop_unsupported_compare(
  const char* op,
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  r_abort_lazy_call(
    error_call,
    "Can't apply `%s` to %s and %s.",
    op,
    rray_arg_type_format(x_arg, rray_typeof(x)),
    rray_arg_type_format(y_arg, rray_typeof(y))
  );
}

#define RRAY_COMPARE(                                                          \
  X_CTYPE,                                                                     \
  X_CONST_DEREF,                                                               \
  X_CAST,                                                                      \
  Y_CTYPE,                                                                     \
  Y_CONST_DEREF,                                                               \
  Y_CAST,                                                                      \
  GREATER_THAN_ONE,                                                            \
  GREATER_THAN_OR_EQUAL_ONE,                                                   \
  LESS_THAN_ONE,                                                               \
  LESS_THAN_OR_EQUAL_ONE                                                       \
)                                                                              \
  switch (op) {                                                                \
  case RRAY_COMPARE_greater_than: {                                            \
    RRAY_BINARY(                                                               \
      X_CTYPE,                                                                 \
      X_CONST_DEREF,                                                           \
      X_CAST,                                                                  \
      Y_CTYPE,                                                                 \
      Y_CONST_DEREF,                                                           \
      Y_CAST,                                                                  \
      R_TYPE_logical,                                                          \
      int,                                                                     \
      r_lgl_begin,                                                             \
      GREATER_THAN_ONE,                                                        \
      RRAY_BINARY_NO_ARGS                                                      \
    );                                                                         \
  }                                                                            \
  case RRAY_COMPARE_greater_than_or_equal: {                                   \
    RRAY_BINARY(                                                               \
      X_CTYPE,                                                                 \
      X_CONST_DEREF,                                                           \
      X_CAST,                                                                  \
      Y_CTYPE,                                                                 \
      Y_CONST_DEREF,                                                           \
      Y_CAST,                                                                  \
      R_TYPE_logical,                                                          \
      int,                                                                     \
      r_lgl_begin,                                                             \
      GREATER_THAN_OR_EQUAL_ONE,                                               \
      RRAY_BINARY_NO_ARGS                                                      \
    );                                                                         \
  }                                                                            \
  case RRAY_COMPARE_less_than: {                                               \
    RRAY_BINARY(                                                               \
      X_CTYPE,                                                                 \
      X_CONST_DEREF,                                                           \
      X_CAST,                                                                  \
      Y_CTYPE,                                                                 \
      Y_CONST_DEREF,                                                           \
      Y_CAST,                                                                  \
      R_TYPE_logical,                                                          \
      int,                                                                     \
      r_lgl_begin,                                                             \
      LESS_THAN_ONE,                                                           \
      RRAY_BINARY_NO_ARGS                                                      \
    );                                                                         \
  }                                                                            \
  case RRAY_COMPARE_less_than_or_equal: {                                      \
    RRAY_BINARY(                                                               \
      X_CTYPE,                                                                 \
      X_CONST_DEREF,                                                           \
      X_CAST,                                                                  \
      Y_CTYPE,                                                                 \
      Y_CONST_DEREF,                                                           \
      Y_CAST,                                                                  \
      R_TYPE_logical,                                                          \
      int,                                                                     \
      r_lgl_begin,                                                             \
      LESS_THAN_OR_EQUAL_ONE,                                                  \
      RRAY_BINARY_NO_ARGS                                                      \
    );                                                                         \
  }                                                                            \
  }                                                                            \
                                                                               \
  r_stop_unreachable();

static r_obj* rray_compare_lgl_lgl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  enum rray_compare_op op
) {
  RRAY_COMPARE(
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_int_one,
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_int_one,
    rray_greater_than_int_int_one,
    rray_greater_than_or_equal_int_int_one,
    rray_less_than_int_int_one,
    rray_less_than_or_equal_int_int_one
  );
}

static r_obj* rray_compare_lgl_int(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  enum rray_compare_op op
) {
  RRAY_COMPARE(
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_int_one,
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    rray_greater_than_int_int_one,
    rray_greater_than_or_equal_int_int_one,
    rray_less_than_int_int_one,
    rray_less_than_or_equal_int_int_one
  );
}

static r_obj* rray_compare_int_lgl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  enum rray_compare_op op
) {
  RRAY_COMPARE(
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_int_one,
    rray_greater_than_int_int_one,
    rray_greater_than_or_equal_int_int_one,
    rray_less_than_int_int_one,
    rray_less_than_or_equal_int_int_one
  );
}

static r_obj* rray_compare_lgl_dbl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  enum rray_compare_op op
) {
  RRAY_COMPARE(
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_int_one,
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    rray_greater_than_int_dbl_one,
    rray_greater_than_or_equal_int_dbl_one,
    rray_less_than_int_dbl_one,
    rray_less_than_or_equal_int_dbl_one
  );
}

static r_obj* rray_compare_dbl_lgl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  enum rray_compare_op op
) {
  RRAY_COMPARE(
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_int_one,
    rray_greater_than_dbl_int_one,
    rray_greater_than_or_equal_dbl_int_one,
    rray_less_than_dbl_int_one,
    rray_less_than_or_equal_dbl_int_one
  );
}

static r_obj* rray_compare_int_int(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  enum rray_compare_op op
) {
  RRAY_COMPARE(
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    rray_greater_than_int_int_one,
    rray_greater_than_or_equal_int_int_one,
    rray_less_than_int_int_one,
    rray_less_than_or_equal_int_int_one
  );
}

static r_obj* rray_compare_int_dbl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  enum rray_compare_op op
) {
  RRAY_COMPARE(
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    rray_greater_than_int_dbl_one,
    rray_greater_than_or_equal_int_dbl_one,
    rray_less_than_int_dbl_one,
    rray_less_than_or_equal_int_dbl_one
  );
}

static r_obj* rray_compare_dbl_int(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  enum rray_compare_op op
) {
  RRAY_COMPARE(
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    rray_greater_than_dbl_int_one,
    rray_greater_than_or_equal_dbl_int_one,
    rray_less_than_dbl_int_one,
    rray_less_than_or_equal_dbl_int_one
  );
}

static r_obj* rray_compare_dbl_dbl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  enum rray_compare_op op
) {
  RRAY_COMPARE(
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    rray_greater_than_dbl_dbl_one,
    rray_greater_than_or_equal_dbl_dbl_one,
    rray_less_than_dbl_dbl_one,
    rray_less_than_or_equal_dbl_dbl_one
  );
}

#undef RRAY_COMPARE

#define RRAY_COMPARE_ONE(X_IS_MISSING, Y_IS_MISSING, OPERATOR)                 \
  const bool missing = (int) X_IS_MISSING(x) | (int) Y_IS_MISSING(y);          \
  const int elt = x OPERATOR y;                                                \
  return missing ? r_globals.na_lgl : elt;

static inline int rray_greater_than_int_int_one(int x, int y) {
  RRAY_COMPARE_ONE(rray_int_is_missing, rray_int_is_missing, >);
}
static inline int rray_greater_than_or_equal_int_int_one(int x, int y) {
  RRAY_COMPARE_ONE(rray_int_is_missing, rray_int_is_missing, >=);
}
static inline int rray_less_than_int_int_one(int x, int y) {
  RRAY_COMPARE_ONE(rray_int_is_missing, rray_int_is_missing, <);
}
static inline int rray_less_than_or_equal_int_int_one(int x, int y) {
  RRAY_COMPARE_ONE(rray_int_is_missing, rray_int_is_missing, <=);
}

static inline int rray_greater_than_int_dbl_one(int x, double y) {
  RRAY_COMPARE_ONE(rray_int_is_missing, rray_dbl_is_missing, >);
}
static inline int rray_greater_than_or_equal_int_dbl_one(int x, double y) {
  RRAY_COMPARE_ONE(rray_int_is_missing, rray_dbl_is_missing, >=);
}
static inline int rray_less_than_int_dbl_one(int x, double y) {
  RRAY_COMPARE_ONE(rray_int_is_missing, rray_dbl_is_missing, <);
}
static inline int rray_less_than_or_equal_int_dbl_one(int x, double y) {
  RRAY_COMPARE_ONE(rray_int_is_missing, rray_dbl_is_missing, <=);
}

static inline int rray_greater_than_dbl_int_one(double x, int y) {
  RRAY_COMPARE_ONE(rray_dbl_is_missing, rray_int_is_missing, >);
}
static inline int rray_greater_than_or_equal_dbl_int_one(double x, int y) {
  RRAY_COMPARE_ONE(rray_dbl_is_missing, rray_int_is_missing, >=);
}
static inline int rray_less_than_dbl_int_one(double x, int y) {
  RRAY_COMPARE_ONE(rray_dbl_is_missing, rray_int_is_missing, <);
}
static inline int rray_less_than_or_equal_dbl_int_one(double x, int y) {
  RRAY_COMPARE_ONE(rray_dbl_is_missing, rray_int_is_missing, <=);
}

static inline int rray_greater_than_dbl_dbl_one(double x, double y) {
  RRAY_COMPARE_ONE(rray_dbl_is_missing, rray_dbl_is_missing, >);
}
static inline int rray_greater_than_or_equal_dbl_dbl_one(double x, double y) {
  RRAY_COMPARE_ONE(rray_dbl_is_missing, rray_dbl_is_missing, >=);
}
static inline int rray_less_than_dbl_dbl_one(double x, double y) {
  RRAY_COMPARE_ONE(rray_dbl_is_missing, rray_dbl_is_missing, <);
}
static inline int rray_less_than_or_equal_dbl_dbl_one(double x, double y) {
  RRAY_COMPARE_ONE(rray_dbl_is_missing, rray_dbl_is_missing, <=);
}

#undef RRAY_COMPARE_ONE
