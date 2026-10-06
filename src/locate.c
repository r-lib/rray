#include "locate.h"

#include "axes.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "missing.h"
#include "reduce-names.h"
#include "size.h"
#include "strided-iterator.h"
#include "strides.h"
#include "type.h"
#include "utils.h"

enum rray_locate_op {
  RRAY_LOCATE_max,
  RRAY_LOCATE_min
};

typedef void (*rray_locate_fn)(
  r_obj* x,
  r_obj* best,
  bool na_rm,
  struct rray_run_iterator* it,
  int* v_out
);

#include "decl/locate-decl.h"

r_obj* ffi_rray_locate_max(
  r_obj* ffi_x,
  r_obj* ffi_axis,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const int axis = arg_as_int(ffi_axis, rray_args.axis, error_call);
  const bool na_rm = r_arg_as_bool(ffi_na_rm, "na_rm");
  return rray_locate_max(ffi_x, axis, na_rm, rray_args.x, error_call);
}

r_obj* rray_locate_max(
  r_obj* x,
  int axis,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_locate(x, axis, na_rm, RRAY_LOCATE_max, arg, error_call);
}

r_obj* ffi_rray_locate_min(
  r_obj* ffi_x,
  r_obj* ffi_axis,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const int axis = arg_as_int(ffi_axis, rray_args.axis, error_call);
  const bool na_rm = r_arg_as_bool(ffi_na_rm, "na_rm");
  return rray_locate_min(ffi_x, axis, na_rm, rray_args.x, error_call);
}

r_obj* rray_locate_min(
  r_obj* x,
  int axis,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_locate(x, axis, na_rm, RRAY_LOCATE_min, arg, error_call);
}

static r_obj* rray_locate(
  r_obj* x,
  int axis,
  bool na_rm,
  enum rray_locate_op op,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));

  r_obj* x_dimensions = KEEP(rray_dimensions(x, arg, error_call));
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);

  const int dimensionality = rray_dimensionality_from_dimensions(x_dimensions);
  check_dimensionality(dimensionality);

  check_axis(axis, dimensionality, rray_args.axis, error_call);

  const rray_locate_fn fn = rray_locate_switch(x, op, arg, error_call);

  r_obj* out_dimensions = KEEP(r_alloc_integer(dimensionality));
  int* v_out_dimensions = r_int_begin(out_dimensions);
  r_memcpy(v_out_dimensions, v_x_dimensions, sizeof(int) * dimensionality);
  v_out_dimensions[axis - 1] = 1;

  const r_ssize out_size =
    rray_size_from_dimensions(v_out_dimensions, dimensionality);

  r_obj* out = KEEP(r_alloc_integer(out_size));
  int* v_out = r_int_begin(out);

  if (v_x_dimensions[axis - 1] == 0) {
    for (r_ssize i = 0; i < out_size; ++i) {
      v_out[i] = r_globals.na_int;
    }
  } else {
    r_ssize v_out_broadcast_strides[RRAY_MAX_DIMENSIONALITY];
    rray_fill_broadcast_strides_from_dimensions(
      v_out_dimensions,
      dimensionality,
      dimensionality,
      v_out_broadcast_strides
    );

    r_ssize v_position_strides[RRAY_MAX_DIMENSIONALITY] = {0};
    v_position_strides[axis - 1] = 1;

    struct rray_run_iterator it;
    rray_run_iterator_init2(
      &it,
      v_x_dimensions,
      dimensionality,
      v_out_broadcast_strides,
      v_position_strides
    );

    r_obj* best = KEEP(r_alloc_vector(r_typeof(x), out_size));
    fn(x, best, na_rm, &it, v_out);
    FREE(1);
  }

  r_attrib_poke_dim(out, out_dimensions);

  r_obj* axes = KEEP(r_int(axis));
  r_obj* out_names = KEEP(rray_reduce_names(x, axes));

  if (out_names != r_null) {
    r_attrib_poke_dim_names(out, out_names);
  }

  FREE(6);
  return out;
}

static rray_locate_fn rray_locate_switch(
  r_obj* x,
  enum rray_locate_op op,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  switch (rray_typeof(x)) {
  case RRAY_TYPE_logical:
    switch (op) {
    case RRAY_LOCATE_max:
      return rray_locate_max_lgl;
    case RRAY_LOCATE_min:
      return rray_locate_min_lgl;
    }
  case RRAY_TYPE_integer:
    switch (op) {
    case RRAY_LOCATE_max:
      return rray_locate_max_int;
    case RRAY_LOCATE_min:
      return rray_locate_min_int;
    }
  case RRAY_TYPE_double:
    switch (op) {
    case RRAY_LOCATE_max:
      return rray_locate_max_dbl;
    case RRAY_LOCATE_min:
      return rray_locate_min_dbl;
    }

  case RRAY_TYPE_complex:
  case RRAY_TYPE_character:
  case RRAY_TYPE_raw:
  case RRAY_TYPE_list:
    switch (op) {
    case RRAY_LOCATE_max:
      stop_unsupported_locate("maximum", x, arg, error_call);
    case RRAY_LOCATE_min:
      stop_unsupported_locate("minimum", x, arg, error_call);
    }

  case RRAY_TYPE_scalar:
    stop_scalar_input(x, arg, error_call);
  }

  r_stop_unreachable();
}

static r_no_return void stop_unsupported_locate(
  const char* op,
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  r_abort_lazy_call(
    error_call,
    "Can't locate the %s of %s.",
    op,
    rray_arg_type_format(arg, rray_typeof(x))
  );
}

#define RRAY_LOCATE(CTYPE, CONST_DEREF, DEREF, IS_MISSING, ONE)                \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
  CTYPE* v_best = DEREF(best);                                                 \
                                                                               \
  for (; !rray_run_iterator_done(it); rray_run_iterator_next2(it)) {           \
    r_ssize start = rray_run_iterator_start(it);                               \
    const r_ssize end = rray_run_iterator_end(it);                             \
                                                                               \
    r_ssize out_loc = rray_run_iterator_loc(it, 0);                            \
    const r_ssize out_stride = rray_run_iterator_stride(it, 0);                \
                                                                               \
    int position = (int) rray_run_iterator_loc(it, 1) + 1;                     \
    const int position_stride = (int) rray_run_iterator_stride(it, 1);         \
                                                                               \
    if (out_stride == 0) {                                                     \
      if (position == 1) {                                                     \
        v_best[out_loc] = v_x[start];                                          \
        v_out[out_loc] = 1;                                                    \
        ++start;                                                               \
        position += position_stride;                                           \
      }                                                                        \
                                                                               \
      CTYPE best_elt = v_best[out_loc];                                        \
      int best_position = v_out[out_loc];                                      \
                                                                               \
      for (r_ssize i = start; i < end; ++i) {                                  \
        const CTYPE x_elt = v_x[i];                                            \
        const bool is_better = ONE(x_elt, best_elt);                           \
        best_elt = is_better ? x_elt : best_elt;                               \
        best_position = is_better ? position : best_position;                  \
        position += position_stride;                                           \
      }                                                                        \
                                                                               \
      v_best[out_loc] = best_elt;                                              \
      v_out[out_loc] = best_position;                                          \
    } else if (position == 1) {                                                \
      for (r_ssize i = start; i < end; ++i) {                                  \
        v_best[out_loc] = v_x[i];                                              \
        v_out[out_loc] = 1;                                                    \
        out_loc += out_stride;                                                 \
      }                                                                        \
    } else {                                                                   \
      for (r_ssize i = start; i < end; ++i) {                                  \
        const CTYPE x_elt = v_x[i];                                            \
        const bool is_better = ONE(x_elt, v_best[out_loc]);                    \
        v_best[out_loc] = is_better ? x_elt : v_best[out_loc];                 \
        v_out[out_loc] = is_better ? position : v_out[out_loc];                \
        out_loc += out_stride;                                                 \
      }                                                                        \
    }                                                                          \
  }                                                                            \
                                                                               \
  if (na_rm) {                                                                 \
    const r_ssize out_size = r_length(best);                                   \
                                                                               \
    for (r_ssize i = 0; i < out_size; ++i) {                                   \
      v_out[i] = IS_MISSING(v_best[i]) ? r_globals.na_int : v_out[i];          \
    }                                                                          \
  }

static void rray_locate_max_lgl(
  r_obj* x,
  r_obj* best,
  bool na_rm,
  struct rray_run_iterator* it,
  int* v_out
) {
  if (na_rm) {
    RRAY_LOCATE(
      int,
      r_lgl_cbegin,
      r_lgl_begin,
      rray_lgl_is_missing,
      rray_locate_max_lgl_one_na_rm
    );
  } else {
    RRAY_LOCATE(
      int,
      r_lgl_cbegin,
      r_lgl_begin,
      rray_lgl_is_missing,
      rray_locate_max_lgl_one
    );
  }
}

static void rray_locate_max_int(
  r_obj* x,
  r_obj* best,
  bool na_rm,
  struct rray_run_iterator* it,
  int* v_out
) {
  if (na_rm) {
    RRAY_LOCATE(
      int,
      r_int_cbegin,
      r_int_begin,
      rray_int_is_missing,
      rray_locate_max_int_one_na_rm
    );
  } else {
    RRAY_LOCATE(
      int,
      r_int_cbegin,
      r_int_begin,
      rray_int_is_missing,
      rray_locate_max_int_one
    );
  }
}

static void rray_locate_max_dbl(
  r_obj* x,
  r_obj* best,
  bool na_rm,
  struct rray_run_iterator* it,
  int* v_out
) {
  if (na_rm) {
    RRAY_LOCATE(
      double,
      r_dbl_cbegin,
      r_dbl_begin,
      rray_dbl_is_missing,
      rray_locate_max_dbl_one_na_rm
    );
  } else {
    RRAY_LOCATE(
      double,
      r_dbl_cbegin,
      r_dbl_begin,
      rray_dbl_is_missing,
      rray_locate_max_dbl_one
    );
  }
}

static void rray_locate_min_lgl(
  r_obj* x,
  r_obj* best,
  bool na_rm,
  struct rray_run_iterator* it,
  int* v_out
) {
  if (na_rm) {
    RRAY_LOCATE(
      int,
      r_lgl_cbegin,
      r_lgl_begin,
      rray_lgl_is_missing,
      rray_locate_min_lgl_one_na_rm
    );
  } else {
    RRAY_LOCATE(
      int,
      r_lgl_cbegin,
      r_lgl_begin,
      rray_lgl_is_missing,
      rray_locate_min_lgl_one
    );
  }
}

static void rray_locate_min_int(
  r_obj* x,
  r_obj* best,
  bool na_rm,
  struct rray_run_iterator* it,
  int* v_out
) {
  if (na_rm) {
    RRAY_LOCATE(
      int,
      r_int_cbegin,
      r_int_begin,
      rray_int_is_missing,
      rray_locate_min_int_one_na_rm
    );
  } else {
    RRAY_LOCATE(
      int,
      r_int_cbegin,
      r_int_begin,
      rray_int_is_missing,
      rray_locate_min_int_one
    );
  }
}

static void rray_locate_min_dbl(
  r_obj* x,
  r_obj* best,
  bool na_rm,
  struct rray_run_iterator* it,
  int* v_out
) {
  if (na_rm) {
    RRAY_LOCATE(
      double,
      r_dbl_cbegin,
      r_dbl_begin,
      rray_dbl_is_missing,
      rray_locate_min_dbl_one_na_rm
    );
  } else {
    RRAY_LOCATE(
      double,
      r_dbl_cbegin,
      r_dbl_begin,
      rray_dbl_is_missing,
      rray_locate_min_dbl_one
    );
  }
}

static inline bool rray_locate_max_lgl_one(int x, int best) {
  return !rray_lgl_is_missing(best) && (rray_lgl_is_missing(x) || x > best);
}

static inline bool rray_locate_max_lgl_one_na_rm(int x, int best) {
  return x > best;
}

static inline bool rray_locate_max_int_one(int x, int best) {
  return !rray_int_is_missing(best) && (rray_int_is_missing(x) || x > best);
}

static inline bool rray_locate_max_int_one_na_rm(int x, int best) {
  return x > best;
}

static inline bool rray_locate_max_dbl_one(double x, double best) {
  switch (rray_dbl_classify(best)) {
  case RRAY_DBL_number:
    return rray_dbl_is_missing(x) || x > best;
  case RRAY_DBL_missing:
    return false;
  case RRAY_DBL_nan:
    return rray_dbl_classify(x) == RRAY_DBL_missing;
  }
  r_stop_unreachable();
}

static inline bool rray_locate_max_dbl_one_na_rm(double x, double best) {
  return !rray_dbl_is_missing(x) && !(x <= best);
}

static inline bool rray_locate_min_lgl_one(int x, int best) {
  return !rray_lgl_is_missing(best) && x < best;
}

static inline bool rray_locate_min_lgl_one_na_rm(int x, int best) {
  return !rray_lgl_is_missing(x) && (rray_lgl_is_missing(best) || x < best);
}

static inline bool rray_locate_min_int_one(int x, int best) {
  return !rray_int_is_missing(best) && x < best;
}

static inline bool rray_locate_min_int_one_na_rm(int x, int best) {
  return !rray_int_is_missing(x) && (rray_int_is_missing(best) || x < best);
}

static inline bool rray_locate_min_dbl_one(double x, double best) {
  switch (rray_dbl_classify(best)) {
  case RRAY_DBL_number:
    return rray_dbl_is_missing(x) || x < best;
  case RRAY_DBL_missing:
    return false;
  case RRAY_DBL_nan:
    return rray_dbl_classify(x) == RRAY_DBL_missing;
  }
  r_stop_unreachable();
}

static inline bool rray_locate_min_dbl_one_na_rm(double x, double best) {
  return !rray_dbl_is_missing(x) && !(x >= best);
}
