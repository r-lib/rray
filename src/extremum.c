#include "extremum.h"

#include "broadcast-names.h"
#include "cast.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "iterator.h"
#include "size.h"
#include "type.h"
#include "typeof2.h"
#include "utils.h"

typedef r_obj* (*rray_extremum_fn)(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);

enum rray_extremum_op {
  RRAY_EXTREMUM_max,
  RRAY_EXTREMUM_min
};

#include "decl/extremum-decl.h"

r_obj* ffi_rray_pmax(
  r_obj* ffi_x,
  r_obj* ffi_y,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const bool na_rm = r_arg_as_bool(ffi_na_rm, "na_rm");
  return rray_pmax(ffi_x, ffi_y, na_rm, rray_args.x, rray_args.y, error_call);
}

r_obj* rray_pmax(
  r_obj* x,
  r_obj* y,
  bool na_rm,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  return rray_extremum(
    x,
    y,
    na_rm,
    RRAY_EXTREMUM_max,
    x_arg,
    y_arg,
    error_call
  );
}

r_obj* ffi_rray_pmin(
  r_obj* ffi_x,
  r_obj* ffi_y,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const bool na_rm = r_arg_as_bool(ffi_na_rm, "na_rm");
  return rray_pmin(ffi_x, ffi_y, na_rm, rray_args.x, rray_args.y, error_call);
}

r_obj* rray_pmin(
  r_obj* x,
  r_obj* y,
  bool na_rm,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  return rray_extremum(
    x,
    y,
    na_rm,
    RRAY_EXTREMUM_min,
    x_arg,
    y_arg,
    error_call
  );
}

static r_obj* rray_extremum(
  r_obj* x,
  r_obj* y,
  bool na_rm,
  enum rray_extremum_op op,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  check_unclassed(x, x_arg, error_call);
  check_unclassed(y, y_arg, error_call);

  x = KEEP(arg_as_array(x, x_arg, error_call));
  y = KEEP(arg_as_array(y, y_arg, error_call));

  const rray_extremum_fn fn =
    rray_extremum_switch(x, y, op, x_arg, y_arg, error_call);

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
    size,
    v_dimensions,
    dimensionality,
    v_x_dimensions,
    x_dimensionality,
    v_y_dimensions,
    y_dimensionality
  );

  r_obj* out = KEEP(fn(x, y, size, &it, na_rm, error_call));
  r_attrib_poke_dim(out, dimensions);

  r_obj* out_names = KEEP(rray_broadcast_names2(x, y, dimensions));

  if (out_names != r_null) {
    r_attrib_poke_dim_names(out, out_names);
  }

  FREE(5);
  return out;
}

static rray_extremum_fn rray_extremum_switch(
  r_obj* x,
  r_obj* y,
  enum rray_extremum_op op,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  const enum rray_type x_type = rray_typeof(x);
  const enum rray_type y_type = rray_typeof(y);

  enum rray_side side;

  switch (rray_typeof2(x_type, y_type, &side)) {
  case RRAY_TYPE2_logical_logical:
    return op == RRAY_EXTREMUM_max ? rray_pmax_lgl_lgl : rray_pmin_lgl_lgl;
  case RRAY_TYPE2_logical_integer:
    if (side == RRAY_SIDE_right) {
      return op == RRAY_EXTREMUM_max ? rray_pmax_lgl_int : rray_pmin_lgl_int;
    } else {
      return op == RRAY_EXTREMUM_max ? rray_pmax_int_lgl : rray_pmin_int_lgl;
    }
  case RRAY_TYPE2_logical_double:
    if (side == RRAY_SIDE_right) {
      return op == RRAY_EXTREMUM_max ? rray_pmax_lgl_dbl : rray_pmin_lgl_dbl;
    } else {
      return op == RRAY_EXTREMUM_max ? rray_pmax_dbl_lgl : rray_pmin_dbl_lgl;
    }
  case RRAY_TYPE2_integer_integer:
    return op == RRAY_EXTREMUM_max ? rray_pmax_int_int : rray_pmin_int_int;
  case RRAY_TYPE2_integer_double:
    if (side == RRAY_SIDE_right) {
      return op == RRAY_EXTREMUM_max ? rray_pmax_int_dbl : rray_pmin_int_dbl;
    } else {
      return op == RRAY_EXTREMUM_max ? rray_pmax_dbl_int : rray_pmin_dbl_int;
    }
  case RRAY_TYPE2_double_double:
    return op == RRAY_EXTREMUM_max ? rray_pmax_dbl_dbl : rray_pmin_dbl_dbl;

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
    stop_unsupported_extremum(
      op == RRAY_EXTREMUM_max ? "pmax" : "pmin",
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

static r_no_return void stop_unsupported_extremum(
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

#define RRAY_EXTREMUM(                                                         \
  X_CTYPE,                                                                     \
  X_CONST_DEREF,                                                               \
  X_CAST,                                                                      \
  Y_CTYPE,                                                                     \
  Y_CONST_DEREF,                                                               \
  Y_CAST,                                                                      \
  OUT_RTYPE,                                                                   \
  OUT_CTYPE,                                                                   \
  OUT_DEREF,                                                                   \
  ONE                                                                          \
)                                                                              \
  r_obj* out = KEEP(r_alloc_vector(OUT_RTYPE, size));                          \
  OUT_CTYPE* v_out = OUT_DEREF(out);                                           \
                                                                               \
  const X_CTYPE* v_x = X_CONST_DEREF(x);                                       \
  const Y_CTYPE* v_y = Y_CONST_DEREF(y);                                       \
                                                                               \
  RRAY_ITERATOR2_FOR_EACH(it, i, x_loc, y_loc, {                               \
    v_out[i] = ONE(X_CAST(v_x[x_loc]), Y_CAST(v_y[y_loc]), na_rm);             \
  });                                                                          \
                                                                               \
  FREE(1);                                                                     \
  return out;

static r_obj* rray_pmax_lgl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
) {
  RRAY_EXTREMUM(
    int,
    r_lgl_cbegin,
    rray_cast_int_to_int_one,
    int,
    r_lgl_cbegin,
    rray_cast_int_to_int_one,
    R_TYPE_logical,
    int,
    r_lgl_begin,
    rray_pmax_int_one
  );
}

static r_obj* rray_pmax_lgl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
) {
  RRAY_EXTREMUM(
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_int_one,
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    R_TYPE_integer,
    int,
    r_int_begin,
    rray_pmax_int_one
  );
}

static r_obj* rray_pmax_int_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
) {
  RRAY_EXTREMUM(
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_int_one,
    R_TYPE_integer,
    int,
    r_int_begin,
    rray_pmax_int_one
  );
}

static r_obj* rray_pmax_lgl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
) {
  RRAY_EXTREMUM(
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_dbl_one,
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_pmax_dbl_one
  );
}

static r_obj* rray_pmax_dbl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
) {
  RRAY_EXTREMUM(
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_pmax_dbl_one
  );
}

static r_obj* rray_pmax_int_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
) {
  RRAY_EXTREMUM(
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    R_TYPE_integer,
    int,
    r_int_begin,
    rray_pmax_int_one
  );
}

static r_obj* rray_pmax_int_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
) {
  RRAY_EXTREMUM(
    int,
    r_int_cbegin,
    rray_cast_int_to_dbl_one,
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_pmax_dbl_one
  );
}

static r_obj* rray_pmax_dbl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
) {
  RRAY_EXTREMUM(
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    int,
    r_int_cbegin,
    rray_cast_int_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_pmax_dbl_one
  );
}

static r_obj* rray_pmax_dbl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
) {
  RRAY_EXTREMUM(
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_pmax_dbl_one
  );
}

static r_obj* rray_pmin_lgl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
) {
  RRAY_EXTREMUM(
    int,
    r_lgl_cbegin,
    rray_cast_int_to_int_one,
    int,
    r_lgl_cbegin,
    rray_cast_int_to_int_one,
    R_TYPE_logical,
    int,
    r_lgl_begin,
    rray_pmin_int_one
  );
}

static r_obj* rray_pmin_lgl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
) {
  RRAY_EXTREMUM(
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_int_one,
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    R_TYPE_integer,
    int,
    r_int_begin,
    rray_pmin_int_one
  );
}

static r_obj* rray_pmin_int_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
) {
  RRAY_EXTREMUM(
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_int_one,
    R_TYPE_integer,
    int,
    r_int_begin,
    rray_pmin_int_one
  );
}

static r_obj* rray_pmin_lgl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
) {
  RRAY_EXTREMUM(
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_dbl_one,
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_pmin_dbl_one
  );
}

static r_obj* rray_pmin_dbl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
) {
  RRAY_EXTREMUM(
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    int,
    r_lgl_cbegin,
    rray_cast_lgl_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_pmin_dbl_one
  );
}

static r_obj* rray_pmin_int_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
) {
  RRAY_EXTREMUM(
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    int,
    r_int_cbegin,
    rray_cast_int_to_int_one,
    R_TYPE_integer,
    int,
    r_int_begin,
    rray_pmin_int_one
  );
}

static r_obj* rray_pmin_int_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
) {
  RRAY_EXTREMUM(
    int,
    r_int_cbegin,
    rray_cast_int_to_dbl_one,
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_pmin_dbl_one
  );
}

static r_obj* rray_pmin_dbl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
) {
  RRAY_EXTREMUM(
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    int,
    r_int_cbegin,
    rray_cast_int_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_pmin_dbl_one
  );
}

static r_obj* rray_pmin_dbl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
) {
  RRAY_EXTREMUM(
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    double,
    r_dbl_cbegin,
    rray_cast_dbl_to_dbl_one,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_pmin_dbl_one
  );
}

#undef RRAY_EXTREMUM

static inline int rray_pmax_int_one(int x, int y, bool na_rm) {
  if (x == r_globals.na_int) {
    return na_rm ? y : x;
  }
  if (y == r_globals.na_int) {
    return na_rm ? x : y;
  }
  return x < y ? y : x;
}

static inline double rray_pmax_dbl_one(double x, double y, bool na_rm) {
  if (ISNAN(y) && !na_rm) {
    return y;
  }
  if (ISNAN(x)) {
    return na_rm ? y : x;
  }
  if (ISNAN(y)) {
    return x;
  }
  return x < y ? y : x;
}

static inline int rray_pmin_int_one(int x, int y, bool na_rm) {
  if (x == r_globals.na_int) {
    return na_rm ? y : x;
  }
  if (y == r_globals.na_int) {
    return na_rm ? x : y;
  }
  return x > y ? y : x;
}

static inline double rray_pmin_dbl_one(double x, double y, bool na_rm) {
  if (ISNAN(y) && !na_rm) {
    return y;
  }
  if (ISNAN(x)) {
    return na_rm ? y : x;
  }
  if (ISNAN(y)) {
    return x;
  }
  return x > y ? y : x;
}
