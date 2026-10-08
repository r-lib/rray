#include "if-else.h"

#include "broadcast.h"
#include "cast.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "ptype.h"
#include "size.h"
#include "strided-iterator.h"
#include "strides.h"
#include "utils.h"

#include "decl/if-else-decl.h"

r_obj* ffi_rray_if_else(
  r_obj* ffi_condition,
  r_obj* ffi_true,
  r_obj* ffi_false,
  r_obj* ffi_missing,
  r_obj* ffi_dimensions,
  r_obj* ffi_frame
) {
  struct rray_arg condition_arg = new_wrapper_arg(NULL, "condition");
  struct rray_arg true_arg = new_wrapper_arg(NULL, "true");
  struct rray_arg false_arg = new_wrapper_arg(NULL, "false");
  struct rray_arg missing_arg = new_wrapper_arg(NULL, "missing");
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};

  return rray_if_else(
    ffi_condition,
    ffi_true,
    ffi_false,
    ffi_missing,
    ffi_dimensions,
    &condition_arg,
    &true_arg,
    &false_arg,
    &missing_arg,
    error_call
  );
}

r_obj* rray_if_else(
  r_obj* condition,
  r_obj* true_,
  r_obj* false_,
  r_obj* missing,
  r_obj* dimensions,
  struct rray_arg* condition_arg,
  struct rray_arg* true_arg,
  struct rray_arg* false_arg,
  struct rray_arg* missing_arg,
  struct r_lazy error_call
) {
  int n_prot = 0;

  check_unclassed(condition, condition_arg, error_call);
  if (r_typeof(condition) != R_TYPE_logical) {
    r_abort_lazy_call(
      error_call,
      "%s must be a logical array, not %s.",
      rray_arg_format(condition_arg),
      r_obj_type_friendly(condition)
    );
  }

  condition =
    KEEP_N(arg_as_array(condition, condition_arg, error_call), &n_prot);

  check_unclassed(true_, true_arg, error_call);
  true_ = KEEP_N(arg_as_array(true_, true_arg, error_call), &n_prot);

  check_unclassed(false_, false_arg, error_call);
  false_ = KEEP_N(arg_as_array(false_, false_arg, error_call), &n_prot);

  const bool has_missing = missing != r_null;
  if (has_missing) {
    check_unclassed(missing, missing_arg, error_call);
    missing = KEEP_N(arg_as_array(missing, missing_arg, error_call), &n_prot);
  }

  enum rray_side side;
  r_obj* ptype = KEEP_N(
    rray_ptype2(true_, false_, &side, true_arg, false_arg, error_call),
    &n_prot
  );

  if (has_missing) {
    struct rray_arg* ptype_arg = side == RRAY_SIDE_right ? false_arg : true_arg;
    ptype = KEEP_N(
      rray_ptype2(ptype, missing, &side, ptype_arg, missing_arg, error_call),
      &n_prot
    );
  }

  true_ = KEEP_N(
    rray_cast(true_, ptype, true_arg, rray_args.empty, error_call),
    &n_prot
  );
  false_ = KEEP_N(
    rray_cast(false_, ptype, false_arg, rray_args.empty, error_call),
    &n_prot
  );
  if (has_missing) {
    missing = KEEP_N(
      rray_cast(missing, ptype, missing_arg, rray_args.empty, error_call),
      &n_prot
    );
  }

  dimensions = KEEP_N(
    rray_if_else_dimensions_common(
      condition,
      true_,
      false_,
      missing,
      dimensions,
      error_call
    ),
    &n_prot
  );

  const int* v_dimensions = r_int_cbegin(dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(dimensions);
  check_dimensionality(dimensionality);

  r_obj* condition_dimensions = r_dim(condition);
  const int* v_condition_dimensions = r_int_cbegin(condition_dimensions);
  const int condition_dimensionality =
    rray_dimensionality_from_dimensions(condition_dimensions);
  check_broadcastable(
    v_condition_dimensions,
    condition_dimensionality,
    v_dimensions,
    dimensionality,
    condition_arg,
    error_call
  );

  r_obj* true_dimensions = r_dim(true_);
  const int* v_true_dimensions = r_int_cbegin(true_dimensions);
  const int true_dimensionality =
    rray_dimensionality_from_dimensions(true_dimensions);
  check_broadcastable(
    v_true_dimensions,
    true_dimensionality,
    v_dimensions,
    dimensionality,
    true_arg,
    error_call
  );

  r_obj* false_dimensions = r_dim(false_);
  const int* v_false_dimensions = r_int_cbegin(false_dimensions);
  const int false_dimensionality =
    rray_dimensionality_from_dimensions(false_dimensions);
  check_broadcastable(
    v_false_dimensions,
    false_dimensionality,
    v_dimensions,
    dimensionality,
    false_arg,
    error_call
  );

  r_ssize v_condition_strides[RRAY_MAX_DIMENSIONALITY];
  rray_fill_broadcast_strides_from_dimensions(
    v_condition_dimensions,
    condition_dimensionality,
    dimensionality,
    v_condition_strides
  );

  r_ssize v_true_strides[RRAY_MAX_DIMENSIONALITY];
  rray_fill_broadcast_strides_from_dimensions(
    v_true_dimensions,
    true_dimensionality,
    dimensionality,
    v_true_strides
  );

  r_ssize v_false_strides[RRAY_MAX_DIMENSIONALITY];
  rray_fill_broadcast_strides_from_dimensions(
    v_false_dimensions,
    false_dimensionality,
    dimensionality,
    v_false_strides
  );

  r_ssize v_missing_strides[RRAY_MAX_DIMENSIONALITY];
  if (has_missing) {
    r_obj* missing_dimensions = r_dim(missing);
    const int* v_missing_dimensions = r_int_cbegin(missing_dimensions);
    const int missing_dimensionality =
      rray_dimensionality_from_dimensions(missing_dimensions);
    check_broadcastable(
      v_missing_dimensions,
      missing_dimensionality,
      v_dimensions,
      dimensionality,
      missing_arg,
      error_call
    );
    rray_fill_broadcast_strides_from_dimensions(
      v_missing_dimensions,
      missing_dimensionality,
      dimensionality,
      v_missing_strides
    );
  }

  const r_ssize size =
    rray_size_from_dimensions_checked(v_dimensions, dimensionality, error_call);

  r_obj* out = KEEP_N(
    rray_if_else_fill(
      condition,
      true_,
      false_,
      missing,
      v_dimensions,
      dimensionality,
      size,
      v_condition_strides,
      v_true_strides,
      v_false_strides,
      v_missing_strides
    ),
    &n_prot
  );
  r_attrib_poke_dim(out, dimensions);

  FREE(n_prot);
  return out;
}

static r_obj* rray_if_else_dimensions_common(
  r_obj* condition,
  r_obj* true_,
  r_obj* false_,
  r_obj* missing,
  r_obj* dimensions,
  struct r_lazy error_call
) {
  if (dimensions != r_null) {
    return arg_as_dimensions(dimensions, rray_args.dimensions, error_call);
  }

  const bool has_missing = missing != r_null;
  const r_ssize n_inputs = has_missing ? 4 : 3;
  r_obj* inputs = KEEP(r_alloc_list(n_inputs));
  r_obj* input_names = KEEP(r_alloc_character(n_inputs));

  r_list_poke(inputs, 0, condition);
  r_list_poke(inputs, 1, true_);
  r_list_poke(inputs, 2, false_);
  r_chr_poke(input_names, 0, r_str("condition"));
  r_chr_poke(input_names, 1, r_str("true"));
  r_chr_poke(input_names, 2, r_str("false"));

  if (has_missing) {
    r_list_poke(inputs, 3, missing);
    r_chr_poke(input_names, 3, r_str("missing"));
  }

  r_attrib_poke_names(inputs, input_names);
  r_obj* out =
    KEEP(rray_dimensions_common(inputs, r_null, rray_args.empty, error_call));

  FREE(3);
  return out;
}

#define RRAY_IF_ELSE_RUN2_INNER(                                               \
  CTYPE,                                                                       \
  POKE,                                                                        \
  MISSING,                                                                     \
  TRUE_VALUE,                                                                  \
  FALSE_VALUE,                                                                 \
  CONDITION_VALUE                                                              \
)                                                                              \
  do {                                                                         \
    for (r_ssize i = start; i < end; ++i) {                                    \
      const int cnd = CONDITION_VALUE;                                         \
      CTYPE const elt = cnd == 1 ? TRUE_VALUE                                  \
        : cnd == 0               ? FALSE_VALUE                                 \
                                 : MISSING;                                    \
      POKE;                                                                    \
      condition_loc += condition_stride;                                       \
      true_loc += true_stride;                                                 \
      false_loc += false_stride;                                               \
    }                                                                          \
  } while (0)

#define RRAY_IF_ELSE_RUN2(CTYPE, POKE, MISSING, TRUE_VALUE, FALSE_VALUE)       \
  do {                                                                         \
    if (condition_stride == 0) {                                               \
      const int condition_elt = v_condition[condition_loc];                    \
      RRAY_IF_ELSE_RUN2_INNER(                                                 \
        CTYPE,                                                                 \
        POKE,                                                                  \
        MISSING,                                                               \
        TRUE_VALUE,                                                            \
        FALSE_VALUE,                                                           \
        condition_elt                                                          \
      );                                                                       \
    } else {                                                                   \
      RRAY_IF_ELSE_RUN2_INNER(                                                 \
        CTYPE,                                                                 \
        POKE,                                                                  \
        MISSING,                                                               \
        TRUE_VALUE,                                                            \
        FALSE_VALUE,                                                           \
        v_condition[condition_loc]                                             \
      );                                                                       \
    }                                                                          \
  } while (0)

#define RRAY_IF_ELSE_RUN3_INNER(                                               \
  CTYPE,                                                                       \
  POKE,                                                                        \
  TRUE_VALUE,                                                                  \
  FALSE_VALUE,                                                                 \
  MISSING_VALUE,                                                               \
  CONDITION_VALUE                                                              \
)                                                                              \
  do {                                                                         \
    for (r_ssize i = start; i < end; ++i) {                                    \
      const int cnd = CONDITION_VALUE;                                         \
      CTYPE const elt = cnd == 1 ? TRUE_VALUE                                  \
        : cnd == 0               ? FALSE_VALUE                                 \
                                 : MISSING_VALUE;                              \
      POKE;                                                                    \
      condition_loc += condition_stride;                                       \
      true_loc += true_stride;                                                 \
      false_loc += false_stride;                                               \
      missing_loc += missing_stride;                                           \
    }                                                                          \
  } while (0)

#define RRAY_IF_ELSE_RUN3(CTYPE, POKE, TRUE_VALUE, FALSE_VALUE, MISSING_VALUE) \
  do {                                                                         \
    if (condition_stride == 0) {                                               \
      const int condition_elt = v_condition[condition_loc];                    \
      RRAY_IF_ELSE_RUN3_INNER(                                                 \
        CTYPE,                                                                 \
        POKE,                                                                  \
        TRUE_VALUE,                                                            \
        FALSE_VALUE,                                                           \
        MISSING_VALUE,                                                         \
        condition_elt                                                          \
      );                                                                       \
    } else {                                                                   \
      RRAY_IF_ELSE_RUN3_INNER(                                                 \
        CTYPE,                                                                 \
        POKE,                                                                  \
        TRUE_VALUE,                                                            \
        FALSE_VALUE,                                                           \
        MISSING_VALUE,                                                         \
        v_condition[condition_loc]                                             \
      );                                                                       \
    }                                                                          \
  } while (0)

#define RRAY_IF_ELSE_RUN3_MISSING(CTYPE, POKE, TRUE_VALUE, FALSE_VALUE)        \
  do {                                                                         \
    if (missing_stride == 0) {                                                 \
      CTYPE const missing_elt = v_missing[missing_loc];                        \
      RRAY_IF_ELSE_RUN3(CTYPE, POKE, TRUE_VALUE, FALSE_VALUE, missing_elt);    \
    } else {                                                                   \
      RRAY_IF_ELSE_RUN3(                                                       \
        CTYPE,                                                                 \
        POKE,                                                                  \
        TRUE_VALUE,                                                            \
        FALSE_VALUE,                                                           \
        v_missing[missing_loc]                                                 \
      );                                                                       \
    }                                                                          \
  } while (0)

#define RRAY_IF_ELSE_FILL(RTYPE, CTYPE, CBEGIN, OUT_DEREF, POKE, MISSING)      \
  do {                                                                         \
    const int* v_condition = r_lgl_cbegin(condition);                          \
                                                                               \
    CTYPE const* v_true = CBEGIN(true_);                                       \
    CTYPE const* v_false = CBEGIN(false_);                                     \
                                                                               \
    r_obj* out = KEEP(r_alloc_vector(RTYPE, size));                            \
    OUT_DEREF;                                                                 \
                                                                               \
    if (missing != r_null) {                                                   \
      CTYPE const* v_missing = CBEGIN(missing);                                \
                                                                               \
      struct rray_run_iterator it;                                             \
      rray_run_iterator_init4(                                                 \
        &it,                                                                   \
        v_dimensions,                                                          \
        dimensionality,                                                        \
        v_condition_strides,                                                   \
        v_true_strides,                                                        \
        v_false_strides,                                                       \
        v_missing_strides                                                      \
      );                                                                       \
                                                                               \
      for (; !rray_run_iterator_done(&it); rray_run_iterator_next4(&it)) {     \
        const r_ssize start = rray_run_iterator_start(&it);                    \
        const r_ssize end = rray_run_iterator_end(&it);                        \
                                                                               \
        r_ssize condition_loc = rray_run_iterator_loc(&it, 0);                 \
        const r_ssize condition_stride = rray_run_iterator_stride(&it, 0);     \
                                                                               \
        r_ssize true_loc = rray_run_iterator_loc(&it, 1);                      \
        const r_ssize true_stride = rray_run_iterator_stride(&it, 1);          \
                                                                               \
        r_ssize false_loc = rray_run_iterator_loc(&it, 2);                     \
        const r_ssize false_stride = rray_run_iterator_stride(&it, 2);         \
                                                                               \
        r_ssize missing_loc = rray_run_iterator_loc(&it, 3);                   \
        const r_ssize missing_stride = rray_run_iterator_stride(&it, 3);       \
                                                                               \
        if (true_stride == 0 && false_stride == 0) {                           \
          CTYPE const true_elt = v_true[true_loc];                             \
          CTYPE const false_elt = v_false[false_loc];                          \
                                                                               \
          RRAY_IF_ELSE_RUN3_MISSING(CTYPE, POKE, true_elt, false_elt);         \
        } else if (true_stride == 0) {                                         \
          CTYPE const true_elt = v_true[true_loc];                             \
                                                                               \
          RRAY_IF_ELSE_RUN3_MISSING(                                           \
            CTYPE,                                                             \
            POKE,                                                              \
            true_elt,                                                          \
            v_false[false_loc]                                                 \
          );                                                                   \
        } else if (false_stride == 0) {                                        \
          CTYPE const false_elt = v_false[false_loc];                          \
                                                                               \
          RRAY_IF_ELSE_RUN3_MISSING(CTYPE, POKE, v_true[true_loc], false_elt); \
        } else {                                                               \
          RRAY_IF_ELSE_RUN3_MISSING(                                           \
            CTYPE,                                                             \
            POKE,                                                              \
            v_true[true_loc],                                                  \
            v_false[false_loc]                                                 \
          );                                                                   \
        }                                                                      \
      }                                                                        \
    } else {                                                                   \
      struct rray_run_iterator it;                                             \
      rray_run_iterator_init3(                                                 \
        &it,                                                                   \
        v_dimensions,                                                          \
        dimensionality,                                                        \
        v_condition_strides,                                                   \
        v_true_strides,                                                        \
        v_false_strides                                                        \
      );                                                                       \
                                                                               \
      for (; !rray_run_iterator_done(&it); rray_run_iterator_next3(&it)) {     \
        const r_ssize start = rray_run_iterator_start(&it);                    \
        const r_ssize end = rray_run_iterator_end(&it);                        \
                                                                               \
        r_ssize condition_loc = rray_run_iterator_loc(&it, 0);                 \
        const r_ssize condition_stride = rray_run_iterator_stride(&it, 0);     \
                                                                               \
        r_ssize true_loc = rray_run_iterator_loc(&it, 1);                      \
        const r_ssize true_stride = rray_run_iterator_stride(&it, 1);          \
                                                                               \
        r_ssize false_loc = rray_run_iterator_loc(&it, 2);                     \
        const r_ssize false_stride = rray_run_iterator_stride(&it, 2);         \
                                                                               \
        if (true_stride == 0 && false_stride == 0) {                           \
          CTYPE const true_elt = v_true[true_loc];                             \
          CTYPE const false_elt = v_false[false_loc];                          \
                                                                               \
          RRAY_IF_ELSE_RUN2(CTYPE, POKE, MISSING, true_elt, false_elt);        \
        } else if (true_stride == 0) {                                         \
          CTYPE const true_elt = v_true[true_loc];                             \
                                                                               \
          RRAY_IF_ELSE_RUN2(                                                   \
            CTYPE,                                                             \
            POKE,                                                              \
            MISSING,                                                           \
            true_elt,                                                          \
            v_false[false_loc]                                                 \
          );                                                                   \
        } else if (false_stride == 0) {                                        \
          CTYPE const false_elt = v_false[false_loc];                          \
                                                                               \
          RRAY_IF_ELSE_RUN2(                                                   \
            CTYPE,                                                             \
            POKE,                                                              \
            MISSING,                                                           \
            v_true[true_loc],                                                  \
            false_elt                                                          \
          );                                                                   \
        } else {                                                               \
          RRAY_IF_ELSE_RUN2(                                                   \
            CTYPE,                                                             \
            POKE,                                                              \
            MISSING,                                                           \
            v_true[true_loc],                                                  \
            v_false[false_loc]                                                 \
          );                                                                   \
        }                                                                      \
      }                                                                        \
    }                                                                          \
                                                                               \
    FREE(1);                                                                   \
    return out;                                                                \
  } while (0)

#define RRAY_IF_ELSE_NO_DEREF

static r_obj* rray_if_else_fill(
  r_obj* condition,
  r_obj* true_,
  r_obj* false_,
  r_obj* missing,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  const r_ssize* v_condition_strides,
  const r_ssize* v_true_strides,
  const r_ssize* v_false_strides,
  const r_ssize* v_missing_strides
) {
  switch (r_typeof(true_)) {
  case R_TYPE_logical:
    RRAY_IF_ELSE_FILL(
      R_TYPE_logical,
      int,
      r_lgl_cbegin,
      int* v_out = r_lgl_begin(out),
      v_out[i] = elt,
      r_globals.na_lgl
    );
  case R_TYPE_integer:
    RRAY_IF_ELSE_FILL(
      R_TYPE_integer,
      int,
      r_int_cbegin,
      int* v_out = r_int_begin(out),
      v_out[i] = elt,
      r_globals.na_int
    );
  case R_TYPE_double:
    RRAY_IF_ELSE_FILL(
      R_TYPE_double,
      double,
      r_dbl_cbegin,
      double* v_out = r_dbl_begin(out),
      v_out[i] = elt,
      r_globals.na_dbl
    );
  case R_TYPE_complex:
    RRAY_IF_ELSE_FILL(
      R_TYPE_complex,
      r_complex,
      r_cpl_cbegin,
      r_complex* v_out = r_cpl_begin(out),
      v_out[i] = elt,
      r_globals.na_cpl
    );
  case R_TYPE_raw:
    RRAY_IF_ELSE_FILL(
      R_TYPE_raw,
      Rbyte,
      r_raw_cbegin,
      Rbyte* v_out = r_raw_begin(out),
      v_out[i] = elt,
      (Rbyte) 0
    );
  case R_TYPE_character:
    RRAY_IF_ELSE_FILL(
      R_TYPE_character,
      r_obj*,
      r_chr_cbegin,
      RRAY_IF_ELSE_NO_DEREF,
      r_chr_poke(out, i, elt),
      r_globals.na_str
    );
  case R_TYPE_list:
    RRAY_IF_ELSE_FILL(
      R_TYPE_list,
      r_obj*,
      r_list_cbegin,
      RRAY_IF_ELSE_NO_DEREF,
      r_list_poke(out, i, elt),
      r_null
    );
  default:
    r_stop_unreachable();
  }
}

#undef RRAY_IF_ELSE_RUN2
#undef RRAY_IF_ELSE_RUN2_INNER
#undef RRAY_IF_ELSE_RUN3
#undef RRAY_IF_ELSE_RUN3_INNER
#undef RRAY_IF_ELSE_RUN3_MISSING
#undef RRAY_IF_ELSE_FILL
#undef RRAY_IF_ELSE_NO_DEREF
