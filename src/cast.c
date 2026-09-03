#include "cast.h"

#include <limits.h>

#include "utils.h"

#include "decl/cast-decl.h"

r_obj* ffi_rray_cast(r_obj* ffi_x, r_obj* ffi_to, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};

  check_unclassed(ffi_to, rray_args.to, error_call);
  const enum r_type to = arg_as_ptype(ffi_to, rray_args.to, error_call);

  return rray_cast(ffi_x, to, rray_args.x, error_call);
}

r_obj* rray_cast(
  r_obj* x,
  enum r_type to,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));

  const enum r_type type = r_typeof(x);

  if (type == to) {
    FREE(1);
    return x;
  }

  r_obj* out;

  switch (type) {
  case R_TYPE_logical:
    switch (to) {
    case R_TYPE_integer:
      out = rray_cast_lgl_to_int(x);
      break;
    case R_TYPE_double:
      out = rray_cast_lgl_to_dbl(x);
      break;
    case R_TYPE_complex:
      out = rray_cast_lgl_to_cpl(x);
      break;
    default:
      stop_incompatible_cast(type, to, arg, error_call);
    }
    break;
  case R_TYPE_integer:
    switch (to) {
    case R_TYPE_logical:
      out = rray_cast_int_to_lgl(x, arg, error_call);
      break;
    case R_TYPE_double:
      out = rray_cast_int_to_dbl(x);
      break;
    case R_TYPE_complex:
      out = rray_cast_int_to_cpl(x);
      break;
    default:
      stop_incompatible_cast(type, to, arg, error_call);
    }
    break;
  case R_TYPE_double:
    switch (to) {
    case R_TYPE_logical:
      out = rray_cast_dbl_to_lgl(x, arg, error_call);
      break;
    case R_TYPE_integer:
      out = rray_cast_dbl_to_int(x, arg, error_call);
      break;
    case R_TYPE_complex:
      out = rray_cast_dbl_to_cpl(x);
      break;
    default:
      stop_incompatible_cast(type, to, arg, error_call);
    }
    break;
  default:
    stop_incompatible_cast(type, to, arg, error_call);
  }

  KEEP(out);

  r_attrib_poke_dim(out, r_dim(x));

  r_obj* names = r_dim_names(x);

  if (names != r_null) {
    r_attrib_poke_dim_names(out, names);
  }

  FREE(2);
  return out;
}

#define RRAY_CAST(FROM_CTYPE, FROM_CBEGIN, TO_RTYPE, TO_CTYPE, TO_BEGIN, ONE)  \
  const r_ssize size = r_length(x);                                            \
  const FROM_CTYPE* v_x = FROM_CBEGIN(x);                                      \
                                                                               \
  r_obj* out = KEEP(r_alloc_vector(TO_RTYPE, size));                           \
  TO_CTYPE* v_out = TO_BEGIN(out);                                             \
                                                                               \
  for (r_ssize i = 0; i < size; ++i) {                                         \
    v_out[i] = ONE(v_x[i]);                                                    \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

#define RRAY_CAST_LOSSY(                                                       \
  FROM_CTYPE,                                                                  \
  FROM_CBEGIN,                                                                 \
  TO_RTYPE,                                                                    \
  TO_CTYPE,                                                                    \
  TO_BEGIN,                                                                    \
  ONE                                                                          \
)                                                                              \
  const r_ssize size = r_length(x);                                            \
  const FROM_CTYPE* v_x = FROM_CBEGIN(x);                                      \
                                                                               \
  r_obj* out = KEEP(r_alloc_vector(TO_RTYPE, size));                           \
  TO_CTYPE* v_out = TO_BEGIN(out);                                             \
                                                                               \
  for (r_ssize i = 0; i < size; ++i) {                                         \
    v_out[i] = ONE(v_x[i], i, arg, error_call);                                \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

static r_obj* rray_cast_lgl_to_int(r_obj* x) {
  const r_ssize size = r_length(x);
  r_obj* out = KEEP(r_alloc_integer(size));
  r_memcpy(r_int_begin(out), r_lgl_cbegin(x), sizeof(int) * size);
  FREE(1);
  return out;
}

static r_obj* rray_cast_lgl_to_dbl(r_obj* x) {
  RRAY_CAST(
    int,
    r_lgl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_cast_lgl_to_dbl_one
  );
}

static r_obj* rray_cast_lgl_to_cpl(r_obj* x) {
  RRAY_CAST(
    int,
    r_lgl_cbegin,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    rray_cast_lgl_to_cpl_one
  );
}

static r_obj* rray_cast_int_to_dbl(r_obj* x) {
  RRAY_CAST(
    int,
    r_int_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_cast_int_to_dbl_one
  );
}

static r_obj* rray_cast_int_to_cpl(r_obj* x) {
  RRAY_CAST(
    int,
    r_int_cbegin,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    rray_cast_int_to_cpl_one
  );
}

static r_obj* rray_cast_dbl_to_cpl(r_obj* x) {
  RRAY_CAST(
    double,
    r_dbl_cbegin,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    rray_cast_dbl_to_cpl_one
  );
}

static r_obj* rray_cast_int_to_lgl(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  RRAY_CAST_LOSSY(
    int,
    r_int_cbegin,
    R_TYPE_logical,
    int,
    r_lgl_begin,
    rray_cast_int_to_lgl_one
  );
}

static r_obj* rray_cast_dbl_to_lgl(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  RRAY_CAST_LOSSY(
    double,
    r_dbl_cbegin,
    R_TYPE_logical,
    int,
    r_lgl_begin,
    rray_cast_dbl_to_lgl_one
  );
}

static r_obj* rray_cast_dbl_to_int(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  RRAY_CAST_LOSSY(
    double,
    r_dbl_cbegin,
    R_TYPE_integer,
    int,
    r_int_begin,
    rray_cast_dbl_to_int_one
  );
}

#undef RRAY_CAST
#undef RRAY_CAST_LOSSY

static inline double rray_cast_lgl_to_dbl_one(int x) {
  if (x == r_globals.na_lgl) {
    return r_globals.na_dbl;
  }

  return (double) x;
}

static inline r_complex rray_cast_lgl_to_cpl_one(int x) {
  if (x == r_globals.na_lgl) {
    return r_globals.na_cpl;
  }

  return (r_complex){.r = (double) x, .i = 0};
}

static inline double rray_cast_int_to_dbl_one(int x) {
  if (x == r_globals.na_int) {
    return r_globals.na_dbl;
  }

  return (double) x;
}

static inline r_complex rray_cast_int_to_cpl_one(int x) {
  if (x == r_globals.na_int) {
    return r_globals.na_cpl;
  }

  return (r_complex){.r = (double) x, .i = 0};
}

static inline r_complex rray_cast_dbl_to_cpl_one(double x) {
  if (R_IsNA(x)) {
    return r_globals.na_cpl;
  }

  return (r_complex){.r = x, .i = 0};
}

static inline int rray_cast_int_to_lgl_one(
  int x,
  r_ssize i,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  if (x == r_globals.na_int) {
    return r_globals.na_lgl;
  }

  if (x == 0 || x == 1) {
    return x;
  }

  stop_lossy_cast(R_TYPE_integer, R_TYPE_logical, i, arg, error_call);
}

static inline int rray_cast_dbl_to_lgl_one(
  double x,
  r_ssize i,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  if (ISNAN(x)) {
    return r_globals.na_lgl;
  }

  if (x == 0) {
    return 0;
  }

  if (x == 1) {
    return 1;
  }

  stop_lossy_cast(R_TYPE_double, R_TYPE_logical, i, arg, error_call);
}

static inline int rray_cast_dbl_to_int_one(
  double x,
  r_ssize i,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  if (ISNAN(x)) {
    return r_globals.na_int;
  }

  if (x <= INT_MIN || x > INT_MAX) {
    stop_lossy_cast(R_TYPE_double, R_TYPE_integer, i, arg, error_call);
  }

  const int out = (int) x;

  if ((double) out != x) {
    stop_lossy_cast(R_TYPE_double, R_TYPE_integer, i, arg, error_call);
  }

  return out;
}

r_obj* ffi_rray_cast_common(r_obj* ffi_xs, r_obj* ffi_to, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};

  check_unclassed(ffi_to, rray_args.dot_to, error_call);
  const enum r_type to = arg_as_ptype(ffi_to, rray_args.dot_to, error_call);

  return rray_cast_common(ffi_xs, to, error_call);
}

r_obj* rray_cast_common(r_obj* xs, enum r_type to, struct r_lazy error_call) {
  const r_ssize n = r_length(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);
  r_obj* xs_names = KEEP(r_names(xs));

  r_obj* out = KEEP(r_alloc_list(n));
  r_attrib_poke_names(out, xs_names);

  r_ssize i = 0;
  struct rray_arg* p_x_arg = new_subscript_arg(NULL, xs_names, n, &i);
  KEEP(p_x_arg->shelter);

  for (; i < n; ++i) {
    r_list_poke(out, i, rray_cast(v_xs[i], to, p_x_arg, error_call));
  }

  FREE(3);
  return out;
}

static r_no_return void stop_incompatible_cast(
  enum r_type x,
  enum r_type to,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  r_abort_lazy_call(
    error_call,
    "Can't convert %s from <%s> to <%s>.",
    rray_arg_format(arg),
    r_type_as_c_string(x),
    r_type_as_c_string(to)
  );
}

static r_no_return void stop_lossy_cast(
  enum r_type x,
  enum r_type to,
  r_ssize i,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  r_abort_lazy_call(
    error_call,
    "Can't convert %s from <%s> to <%s> due to loss of precision at "
    "location %" R_PRI_SSIZE ".",
    rray_arg_format(arg),
    r_type_as_c_string(x),
    r_type_as_c_string(to),
    i + 1
  );
}
