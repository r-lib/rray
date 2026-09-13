#include "equal.h"

#include "broadcast-names.h"
#include "cast.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "strided-iterator.h"
#include "missing.h"
#include "size.h"
#include "type.h"
#include "typeof2.h"
#include "utils.h"

enum rray_equality_op {
  RRAY_EQUAL_equal,
  RRAY_EQUAL_not_equal
};

#include "decl/equal-decl.h"

r_obj* ffi_rray_equal(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_equal(ffi_x, ffi_y, rray_args.x, rray_args.y, error_call);
}

r_obj* rray_equal(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  return rray_equality(x, y, RRAY_EQUAL_equal, x_arg, y_arg, error_call);
}

r_obj* ffi_rray_not_equal(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_not_equal(ffi_x, ffi_y, rray_args.x, rray_args.y, error_call);
}

r_obj* rray_not_equal(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  return rray_equality(x, y, RRAY_EQUAL_not_equal, x_arg, y_arg, error_call);
}

static r_obj* rray_equality(
  r_obj* x,
  r_obj* y,
  enum rray_equality_op op,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  check_unclassed(x, x_arg, error_call);
  check_unclassed(y, y_arg, error_call);

  x = KEEP(arg_as_array(x, x_arg, error_call));
  y = KEEP(arg_as_array(y, y_arg, error_call));

  const rray_equality_fn fn =
    rray_equality_switch(x, y, op, x_arg, y_arg, error_call);

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

  struct rray_strided_iterator2 it = rray_broadcast_iterator2(
    v_x_dimensions,
    x_dimensionality,
    v_y_dimensions,
    y_dimensionality,
    v_dimensions,
    dimensionality
  );

  r_obj* out = KEEP(fn(x, y, size, &it, op));
  r_attrib_poke_dim(out, dimensions);

  r_obj* out_names = KEEP(rray_broadcast_names2(x, y, dimensions));

  if (out_names != r_null) {
    r_attrib_poke_dim_names(out, out_names);
  }

  FREE(5);
  return out;
}

static rray_equality_fn rray_equality_switch(
  r_obj* x,
  r_obj* y,
  enum rray_equality_op op,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  const enum rray_type x_type = rray_typeof(x);
  const enum rray_type y_type = rray_typeof(y);

  enum rray_side side;

  switch (rray_typeof2(x_type, y_type, &side)) {
  case RRAY_TYPE2_logical_logical:
    return rray_equality_lgl_lgl;
  case RRAY_TYPE2_logical_integer:
    return side == RRAY_SIDE_right ? rray_equality_lgl_int
                                   : rray_equality_int_lgl;
  case RRAY_TYPE2_logical_double:
    return side == RRAY_SIDE_right ? rray_equality_lgl_dbl
                                   : rray_equality_dbl_lgl;
  case RRAY_TYPE2_logical_complex:
    return side == RRAY_SIDE_right ? rray_equality_lgl_cpl
                                   : rray_equality_cpl_lgl;
  case RRAY_TYPE2_integer_integer:
    return rray_equality_int_int;
  case RRAY_TYPE2_integer_double:
    return side == RRAY_SIDE_right ? rray_equality_int_dbl
                                   : rray_equality_dbl_int;
  case RRAY_TYPE2_integer_complex:
    return side == RRAY_SIDE_right ? rray_equality_int_cpl
                                   : rray_equality_cpl_int;
  case RRAY_TYPE2_double_double:
    return rray_equality_dbl_dbl;
  case RRAY_TYPE2_double_complex:
    return side == RRAY_SIDE_right ? rray_equality_dbl_cpl
                                   : rray_equality_cpl_dbl;
  case RRAY_TYPE2_complex_complex:
    return rray_equality_cpl_cpl;

  case RRAY_TYPE2_logical_character:
  case RRAY_TYPE2_logical_raw:
  case RRAY_TYPE2_logical_list:
  case RRAY_TYPE2_integer_character:
  case RRAY_TYPE2_integer_raw:
  case RRAY_TYPE2_integer_list:
  case RRAY_TYPE2_double_character:
  case RRAY_TYPE2_double_raw:
  case RRAY_TYPE2_double_list:
  case RRAY_TYPE2_complex_character:
  case RRAY_TYPE2_complex_raw:
  case RRAY_TYPE2_complex_list:
  case RRAY_TYPE2_character_character:
  case RRAY_TYPE2_character_raw:
  case RRAY_TYPE2_character_list:
  case RRAY_TYPE2_raw_raw:
  case RRAY_TYPE2_raw_list:
  case RRAY_TYPE2_list_list:
    stop_unsupported_equality(
      rray_equality_op_as_c_string(op),
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

static const char* rray_equality_op_as_c_string(enum rray_equality_op op) {
  switch (op) {
  case RRAY_EQUAL_equal:
    return "==";
  case RRAY_EQUAL_not_equal:
    return "!=";
  }

  r_stop_unreachable();
}

static r_no_return void stop_unsupported_equality(
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

#define RRAY_EQUALITY_IMPL(                                                    \
  X_CTYPE,                                                                     \
  X_CONST_DEREF,                                                               \
  X_CAST,                                                                      \
  Y_CTYPE,                                                                     \
  Y_CONST_DEREF,                                                               \
  Y_CAST,                                                                      \
  ONE                                                                          \
)                                                                              \
  r_obj* out = KEEP(r_alloc_vector(R_TYPE_logical, size));                     \
  int* v_out = r_lgl_begin(out);                                               \
                                                                               \
  const X_CTYPE* v_x = X_CONST_DEREF(x);                                       \
  const Y_CTYPE* v_y = Y_CONST_DEREF(y);                                       \
                                                                               \
  struct rray_strided_iterator2_cursor cursor =                                \
    rray_strided_iterator2_begin(it);                                          \
  for (; !rray_strided_iterator2_finished(&cursor);                            \
       rray_strided_iterator2_next(&cursor)) {                                 \
    const r_ssize index = rray_strided_iterator2_index(&cursor);               \
    const r_ssize end = index + rray_strided_iterator2_run_size(&cursor);      \
    const r_ssize x_loc = rray_strided_iterator2_location1(&cursor);           \
    const r_ssize y_loc = rray_strided_iterator2_location2(&cursor);           \
    const r_ssize x_stride = rray_strided_iterator2_run_stride1(&cursor);      \
    const r_ssize y_stride = rray_strided_iterator2_run_stride2(&cursor);      \
                                                                               \
    if (x_stride == 0) {                                                       \
      if (y_stride == 0) {                                                     \
        for (r_ssize i = index; i < end; ++i) {                                \
          v_out[i] = ONE(X_CAST(v_x[x_loc]), Y_CAST(v_y[y_loc]));              \
        }                                                                      \
      } else {                                                                 \
        for (r_ssize i = index, y_loc_ = y_loc; i < end;                       \
             ++i, y_loc_ += y_stride) {                                        \
          v_out[i] = ONE(X_CAST(v_x[x_loc]), Y_CAST(v_y[y_loc_]));             \
        }                                                                      \
      }                                                                        \
    } else if (y_stride == 0) {                                                \
      for (r_ssize i = index, x_loc_ = x_loc; i < end;                         \
           ++i, x_loc_ += x_stride) {                                          \
        v_out[i] = ONE(X_CAST(v_x[x_loc_]), Y_CAST(v_y[y_loc]));               \
      }                                                                        \
    } else {                                                                   \
      for (r_ssize i = index, x_loc_ = x_loc, y_loc_ = y_loc; i < end;         \
           ++i, x_loc_ += x_stride, y_loc_ += y_stride) {                      \
        v_out[i] = ONE(X_CAST(v_x[x_loc_]), Y_CAST(v_y[y_loc_]));              \
      }                                                                        \
    }                                                                          \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

#define RRAY_EQUALITY(                                                         \
  X_CTYPE,                                                                     \
  X_CONST_DEREF,                                                               \
  X_CAST,                                                                      \
  Y_CTYPE,                                                                     \
  Y_CONST_DEREF,                                                               \
  Y_CAST,                                                                      \
  EQUAL_ONE,                                                                   \
  NOT_EQUAL_ONE                                                                \
)                                                                              \
  if (op == RRAY_EQUAL_equal) {                                                \
    RRAY_EQUALITY_IMPL(                                                        \
      X_CTYPE,                                                                 \
      X_CONST_DEREF,                                                           \
      X_CAST,                                                                  \
      Y_CTYPE,                                                                 \
      Y_CONST_DEREF,                                                           \
      Y_CAST,                                                                  \
      EQUAL_ONE                                                                \
    );                                                                         \
  } else {                                                                     \
    RRAY_EQUALITY_IMPL(                                                        \
      X_CTYPE,                                                                 \
      X_CONST_DEREF,                                                           \
      X_CAST,                                                                  \
      Y_CTYPE,                                                                 \
      Y_CONST_DEREF,                                                           \
      Y_CAST,                                                                  \
      NOT_EQUAL_ONE                                                            \
    );                                                                         \
  }

static r_obj* rray_equality_lgl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  enum rray_equality_op op
) {
  RRAY_EQUALITY(
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_int_one,
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_int_one,
    rray_equal_int_one,
    rray_not_equal_int_one
  );
}

static r_obj* rray_equality_lgl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  enum rray_equality_op op
) {
  RRAY_EQUALITY(
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_int_one,
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    rray_equal_int_one,
    rray_not_equal_int_one
  );
}

static r_obj* rray_equality_int_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  enum rray_equality_op op
) {
  RRAY_EQUALITY(
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_int_one,
    rray_equal_int_one,
    rray_not_equal_int_one
  );
}

static r_obj* rray_equality_lgl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  enum rray_equality_op op
) {
  RRAY_EQUALITY(
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_dbl_one,
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    rray_equal_dbl_one,
    rray_not_equal_dbl_one
  );
}

static r_obj* rray_equality_dbl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  enum rray_equality_op op
) {
  RRAY_EQUALITY(
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_dbl_one,
    rray_equal_dbl_one,
    rray_not_equal_dbl_one
  );
}

static r_obj* rray_equality_lgl_cpl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  enum rray_equality_op op
) {
  RRAY_EQUALITY(
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_cpl_one,
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    rray_equal_cpl_one,
    rray_not_equal_cpl_one
  );
}

static r_obj* rray_equality_cpl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  enum rray_equality_op op
) {
  RRAY_EQUALITY(
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_cpl_one,
    rray_equal_cpl_one,
    rray_not_equal_cpl_one
  );
}

static r_obj* rray_equality_int_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  enum rray_equality_op op
) {
  RRAY_EQUALITY(
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    rray_equal_int_one,
    rray_not_equal_int_one
  );
}

static r_obj* rray_equality_int_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  enum rray_equality_op op
) {
  RRAY_EQUALITY(
    int,
    r_int_cbegin,
    rray_cast_int_to_dbl_one,
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    rray_equal_dbl_one,
    rray_not_equal_dbl_one
  );
}

static r_obj* rray_equality_dbl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  enum rray_equality_op op
) {
  RRAY_EQUALITY(
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    int,
    r_int_cbegin,
    rray_cast_int_to_dbl_one,
    rray_equal_dbl_one,
    rray_not_equal_dbl_one
  );
}

static r_obj* rray_equality_int_cpl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  enum rray_equality_op op
) {
  RRAY_EQUALITY(
    int,
    r_int_cbegin,
    rray_cast_int_to_cpl_one,
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    rray_equal_cpl_one,
    rray_not_equal_cpl_one
  );
}

static r_obj* rray_equality_cpl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  enum rray_equality_op op
) {
  RRAY_EQUALITY(
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    int,
    r_int_cbegin,
    rray_cast_int_to_cpl_one,
    rray_equal_cpl_one,
    rray_not_equal_cpl_one
  );
}

static r_obj* rray_equality_dbl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  enum rray_equality_op op
) {
  RRAY_EQUALITY(
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    rray_equal_dbl_one,
    rray_not_equal_dbl_one
  );
}

static r_obj* rray_equality_dbl_cpl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  enum rray_equality_op op
) {
  RRAY_EQUALITY(
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_cpl_one,
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    rray_equal_cpl_one,
    rray_not_equal_cpl_one
  );
}

static r_obj* rray_equality_cpl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  enum rray_equality_op op
) {
  RRAY_EQUALITY(
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_cpl_one,
    rray_equal_cpl_one,
    rray_not_equal_cpl_one
  );
}

static r_obj* rray_equality_cpl_cpl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  enum rray_equality_op op
) {
  RRAY_EQUALITY(
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    r_complex,
    r_cpl_cbegin,
    rray_cast_cpl_to_cpl_one,
    rray_equal_cpl_one,
    rray_not_equal_cpl_one
  );
}

#undef RRAY_EQUALITY
#undef RRAY_EQUALITY_IMPL

static inline int rray_equal_int_one(int x, int y) {
  const bool missing = rray_int_is_missing(x) | rray_int_is_missing(y);
  const int elt = x == y;
  return missing ? r_globals.na_lgl : elt;
}

static inline int rray_not_equal_int_one(int x, int y) {
  const bool missing = rray_int_is_missing(x) | rray_int_is_missing(y);
  const int elt = x != y;
  return missing ? r_globals.na_lgl : elt;
}

static inline int rray_equal_dbl_one(double x, double y) {
  const bool missing = rray_dbl_is_missing(x) | rray_dbl_is_missing(y);
  const int elt = x == y;
  return missing ? r_globals.na_lgl : elt;
}

static inline int rray_not_equal_dbl_one(double x, double y) {
  const bool missing = rray_dbl_is_missing(x) | rray_dbl_is_missing(y);
  const int elt = x != y;
  return missing ? r_globals.na_lgl : elt;
}

// Purposeful usage of bitwise operators to encourage loop vectorization
static inline int rray_equal_cpl_one(r_complex x, r_complex y) {
  const bool missing = rray_cpl_is_missing(x) | rray_cpl_is_missing(y);
  const int elt = (x.r == y.r) & (x.i == y.i);
  return missing ? r_globals.na_lgl : elt;
}

static inline int rray_not_equal_cpl_one(r_complex x, r_complex y) {
  const bool missing = rray_cpl_is_missing(x) | rray_cpl_is_missing(y);
  const int elt = (x.r != y.r) | (x.i != y.i);
  return missing ? r_globals.na_lgl : elt;
}
