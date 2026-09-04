#include "cast.h"

#include <limits.h>

#include "syms.h"
#include "type.h"
#include "utils.h"

#include "decl/cast-decl.h"

r_obj* ffi_rray_cast(r_obj* ffi_x, r_obj* ffi_to, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = rray_syms.call, .env = ffi_frame};

  struct r_lazy x_arg_lazy = {.x = rray_syms.x_arg, .env = ffi_frame};
  struct rray_arg x_arg = new_lazy_arg(&x_arg_lazy);

  struct r_lazy to_arg_lazy = {.x = rray_syms.to_arg, .env = ffi_frame};
  struct rray_arg to_arg = new_lazy_arg(&to_arg_lazy);

  return rray_cast(ffi_x, ffi_to, &x_arg, &to_arg, error_call);
}

r_obj* rray_cast(
  r_obj* x,
  r_obj* to,
  struct rray_arg* x_arg,
  struct rray_arg* to_arg,
  struct r_lazy error_call
) {
  check_unclassed(x, x_arg, error_call);
  x = KEEP(arg_as_array(x, x_arg, error_call));

  check_unclassed(to, to_arg, error_call);
  const enum rray_type to_type = arg_as_type(to, to_arg, error_call);

  const enum rray_type type = rray_typeof(x);

  r_obj* out;

  switch (type) {
  case RRAY_TYPE_logical:
    switch (to_type) {
    case RRAY_TYPE_logical:
      out = x;
      break;
    case RRAY_TYPE_integer:
      out = rray_cast_lgl_to_int(x, x_arg, error_call);
      break;
    case RRAY_TYPE_double:
      out = rray_cast_lgl_to_dbl(x, x_arg, error_call);
      break;
    case RRAY_TYPE_complex:
      out = rray_cast_lgl_to_cpl(x, x_arg, error_call);
      break;
    default:
      stop_incompatible_cast(type, to_type, x_arg, error_call);
    }
    break;
  case RRAY_TYPE_integer:
    switch (to_type) {
    case RRAY_TYPE_logical:
      out = rray_cast_int_to_lgl(x, x_arg, error_call);
      break;
    case RRAY_TYPE_integer:
      out = x;
      break;
    case RRAY_TYPE_double:
      out = rray_cast_int_to_dbl(x, x_arg, error_call);
      break;
    case RRAY_TYPE_complex:
      out = rray_cast_int_to_cpl(x, x_arg, error_call);
      break;
    default:
      stop_incompatible_cast(type, to_type, x_arg, error_call);
    }
    break;
  case RRAY_TYPE_double:
    switch (to_type) {
    case RRAY_TYPE_logical:
      out = rray_cast_dbl_to_lgl(x, x_arg, error_call);
      break;
    case RRAY_TYPE_integer:
      out = rray_cast_dbl_to_int(x, x_arg, error_call);
      break;
    case RRAY_TYPE_double:
      out = x;
      break;
    case RRAY_TYPE_complex:
      out = rray_cast_dbl_to_cpl(x, x_arg, error_call);
      break;
    default:
      stop_incompatible_cast(type, to_type, x_arg, error_call);
    }
    break;
  case RRAY_TYPE_complex:
    switch (to_type) {
    case RRAY_TYPE_complex:
      out = x;
      break;
    default:
      stop_incompatible_cast(type, to_type, x_arg, error_call);
    }
    break;
  case RRAY_TYPE_character:
    switch (to_type) {
    case RRAY_TYPE_character:
      out = x;
      break;
    default:
      stop_incompatible_cast(type, to_type, x_arg, error_call);
    }
    break;
  case RRAY_TYPE_raw:
    switch (to_type) {
    case RRAY_TYPE_raw:
      out = x;
      break;
    default:
      stop_incompatible_cast(type, to_type, x_arg, error_call);
    }
    break;
  case RRAY_TYPE_list:
    switch (to_type) {
    case RRAY_TYPE_list:
      out = x;
      break;
    default:
      stop_incompatible_cast(type, to_type, x_arg, error_call);
    }
    break;
  }

  FREE(1);
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
    v_out[i] = ONE(v_x[i], i, x_arg, error_call);                              \
  }                                                                            \
                                                                               \
  poke_dimensions_and_names(out, x);                                           \
                                                                               \
  FREE(1);                                                                     \
  return out;

static r_obj* rray_cast_lgl_to_int(
  r_obj* x,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  const r_ssize size = r_length(x);

  r_obj* out = KEEP(r_alloc_integer(size));
  r_memcpy(r_int_begin(out), r_lgl_cbegin(x), sizeof(int) * size);

  poke_dimensions_and_names(out, x);

  FREE(1);
  return out;
}

static r_obj* rray_cast_lgl_to_dbl(
  r_obj* x,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  RRAY_CAST(
    int,
    r_lgl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_cast_lgl_to_dbl_one
  );
}

static r_obj* rray_cast_lgl_to_cpl(
  r_obj* x,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  RRAY_CAST(
    int,
    r_lgl_cbegin,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    rray_cast_lgl_to_cpl_one
  );
}

static r_obj* rray_cast_int_to_lgl(
  r_obj* x,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  RRAY_CAST(
    int,
    r_int_cbegin,
    R_TYPE_logical,
    int,
    r_lgl_begin,
    rray_cast_int_to_lgl_one
  );
}

static r_obj* rray_cast_int_to_dbl(
  r_obj* x,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  RRAY_CAST(
    int,
    r_int_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    rray_cast_int_to_dbl_one
  );
}

static r_obj* rray_cast_int_to_cpl(
  r_obj* x,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  RRAY_CAST(
    int,
    r_int_cbegin,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    rray_cast_int_to_cpl_one
  );
}

static r_obj* rray_cast_dbl_to_lgl(
  r_obj* x,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  RRAY_CAST(
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
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  RRAY_CAST(
    double,
    r_dbl_cbegin,
    R_TYPE_integer,
    int,
    r_int_begin,
    rray_cast_dbl_to_int_one
  );
}

static r_obj* rray_cast_dbl_to_cpl(
  r_obj* x,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  RRAY_CAST(
    double,
    r_dbl_cbegin,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    rray_cast_dbl_to_cpl_one
  );
}

#undef RRAY_CAST

static inline double rray_cast_lgl_to_dbl_one(
  int x,
  r_ssize i,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  if (x == r_globals.na_lgl) {
    return r_globals.na_dbl;
  }

  return (double) x;
}

static inline r_complex rray_cast_lgl_to_cpl_one(
  int x,
  r_ssize i,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  if (x == r_globals.na_lgl) {
    return r_globals.na_cpl;
  }

  return (r_complex){.r = (double) x, .i = 0};
}

static inline int rray_cast_int_to_lgl_one(
  int x,
  r_ssize i,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  if (x == r_globals.na_int) {
    return r_globals.na_lgl;
  }

  if (x == 0 || x == 1) {
    return x;
  }

  stop_lossy_cast(RRAY_TYPE_integer, RRAY_TYPE_logical, i, x_arg, error_call);
}

static inline double rray_cast_int_to_dbl_one(
  int x,
  r_ssize i,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  if (x == r_globals.na_int) {
    return r_globals.na_dbl;
  }

  return (double) x;
}

static inline r_complex rray_cast_int_to_cpl_one(
  int x,
  r_ssize i,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  if (x == r_globals.na_int) {
    return r_globals.na_cpl;
  }

  return (r_complex){.r = (double) x, .i = 0};
}

static inline int rray_cast_dbl_to_lgl_one(
  double x,
  r_ssize i,
  struct rray_arg* x_arg,
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

  stop_lossy_cast(RRAY_TYPE_double, RRAY_TYPE_logical, i, x_arg, error_call);
}

static inline int rray_cast_dbl_to_int_one(
  double x,
  r_ssize i,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  if (ISNAN(x)) {
    return r_globals.na_int;
  }

  if (x <= INT_MIN || x > INT_MAX) {
    stop_lossy_cast(RRAY_TYPE_double, RRAY_TYPE_integer, i, x_arg, error_call);
  }

  const int out = (int) x;

  if ((double) out != x) {
    stop_lossy_cast(RRAY_TYPE_double, RRAY_TYPE_integer, i, x_arg, error_call);
  }

  return out;
}

static inline r_complex rray_cast_dbl_to_cpl_one(
  double x,
  r_ssize i,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  if (R_IsNA(x)) {
    return r_globals.na_cpl;
  }

  return (r_complex){.r = x, .i = 0};
}

static void poke_dimensions_and_names(r_obj* out, r_obj* x) {
  r_attrib_poke_dim(out, r_dim(x));

  r_obj* names = r_dim_names(x);

  if (names != r_null) {
    r_attrib_poke_dim_names(out, names);
  }
}

static r_no_return void stop_incompatible_cast(
  enum rray_type x,
  enum rray_type to,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  r_abort_lazy_call(
    error_call,
    "Can't convert from %s to <%s>.",
    rray_arg_type_format(x_arg, x),
    rray_type_as_c_string(to)
  );
}

static r_no_return void stop_lossy_cast(
  enum rray_type x,
  enum rray_type to,
  r_ssize i,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  r_abort_lazy_call(
    error_call,
    "Can't convert from %s to <%s> due to loss of precision at "
    "location %" R_PRI_SSIZE ".",
    rray_arg_type_format(x_arg, x),
    rray_type_as_c_string(to),
    i + 1
  );
}
