#include "index.h"

#include "dimensionality.h"
#include "dimensions.h"
#include "strided-iterator2.h"
#include "strides.h"
#include "utils.h"

#include "decl/index-decl.h"

r_obj* ffi_rray_index(r_obj* ffi_x, r_obj* ffi_indices, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_index(
    ffi_x,
    ffi_indices,
    rray_args.x,
    rray_args.empty,
    error_call
  );
}

r_obj* rray_index(
  r_obj* x,
  r_obj* indices,
  struct rray_arg* x_arg,
  struct rray_arg* indices_arg,
  struct r_lazy error_call
) {
  check_unclassed(x, x_arg, error_call);
  x = KEEP(arg_as_array(x, x_arg, error_call));

  r_obj* x_dimensions = KEEP(r_dim(x));
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);
  const int x_dimensionality =
    rray_dimensionality_from_dimensions(x_dimensions);
  check_dimensionality(x_dimensionality);

  r_ssize v_x_strides[RRAY_MAX_DIMENSIONALITY];
  rray_fill_strides_from_dimensions(
    v_x_dimensions,
    x_dimensionality,
    v_x_strides
  );

  indices = KEEP(rray_as_index_arrays(
    indices,
    v_x_dimensions,
    x_dimensionality,
    indices_arg,
    error_call
  ));
  r_obj* const* v_indices = r_list_cbegin(indices);
  const r_ssize indices_size = r_length(indices);

  const bool any_missing = rray_any_missing_index(v_indices, indices_size);

  r_obj* dimensions =
    KEEP(rray_dimensions_common(indices, r_null, indices_arg, error_call));
  const int* v_dimensions = r_int_cbegin(dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(dimensions);
  check_dimensionality(dimensionality);

  // `...` can take up to `RRAY_MAX_DIMENSIONALITY` inputs, bounded by the
  // dimensionality of `x`. Each input can have dimensionality up to
  // `RRAY_MAX_DIMENSIONALITY`, so the maximum size is known.
  r_ssize v_indices_strides[RRAY_MAX_DIMENSIONALITY][RRAY_MAX_DIMENSIONALITY];
  const r_ssize* v_v_indices_strides[RRAY_MAX_DIMENSIONALITY];
  for (r_ssize i = 0; i < indices_size; ++i) {
    r_obj* index_dimensions = r_dim(v_indices[i]);
    const int* v_index_dimensions = r_int_cbegin(index_dimensions);
    const int index_dimensionality =
      rray_dimensionality_from_dimensions(index_dimensions);

    rray_fill_broadcast_strides_from_dimensions(
      v_index_dimensions,
      index_dimensionality,
      dimensionality,
      v_indices_strides[i]
    );

    v_v_indices_strides[i] = v_indices_strides[i];
  }

  // `...` can have up to `RRAY_MAX_DIMENSIONALITY` inputs, bounded by the
  // dimensionality of `x`.
  const int* v_v_index[RRAY_MAX_DIMENSIONALITY];
  for (r_ssize i = 0; i < indices_size; ++i) {
    v_v_index[i] = r_int_cbegin(v_indices[i]);
  }

  r_obj* out;

  switch (r_typeof(x)) {
  case R_TYPE_logical:
    out = rray_index_lgl(
      x,
      v_x_strides,
      v_v_index,
      indices_size,
      any_missing,
      v_dimensions,
      dimensionality,
      v_v_indices_strides
    );
    break;
  case R_TYPE_integer:
    out = rray_index_int(
      x,
      v_x_strides,
      v_v_index,
      indices_size,
      any_missing,
      v_dimensions,
      dimensionality,
      v_v_indices_strides
    );
    break;
  case R_TYPE_double:
    out = rray_index_dbl(
      x,
      v_x_strides,
      v_v_index,
      indices_size,
      any_missing,
      v_dimensions,
      dimensionality,
      v_v_indices_strides
    );
    break;
  case R_TYPE_complex:
    out = rray_index_cpl(
      x,
      v_x_strides,
      v_v_index,
      indices_size,
      any_missing,
      v_dimensions,
      dimensionality,
      v_v_indices_strides
    );
    break;
  case R_TYPE_raw:
    out = rray_index_raw(
      x,
      v_x_strides,
      v_v_index,
      indices_size,
      any_missing,
      v_dimensions,
      dimensionality,
      v_v_indices_strides
    );
    break;
  case R_TYPE_character:
    out = rray_index_chr(
      x,
      v_x_strides,
      v_v_index,
      indices_size,
      any_missing,
      v_dimensions,
      dimensionality,
      v_v_indices_strides
    );
    break;
  case R_TYPE_list:
    out = rray_index_list(
      x,
      v_x_strides,
      v_v_index,
      indices_size,
      any_missing,
      v_dimensions,
      dimensionality,
      v_v_indices_strides
    );
    break;
  default:
    r_stop_unreachable();
  }

  KEEP(out);
  r_attrib_poke_dim(out, dimensions);

  FREE(5);
  return out;
}

static r_obj* rray_as_index_arrays(
  r_obj* indices,
  const int* v_dimensions,
  int dimensionality,
  struct rray_arg* indices_arg,
  struct r_lazy error_call
) {
  r_obj* const* v_indices = r_list_cbegin(indices);
  const r_ssize indices_size = r_length(indices);

  if (indices_size != dimensionality) {
    r_abort_lazy_call(
      error_call,
      "Must supply exactly %d coordinate array%s to `...`, not %" R_PRI_SSIZE
      ".",
      dimensionality,
      dimensionality == 1 ? "" : "s",
      indices_size
    );
  }

  if (r_names(indices) != r_null) {
    r_abort_lazy_call(error_call, "All elements of `...` must be unnamed.");
  }

  r_obj* out = KEEP(r_alloc_list(indices_size));

  r_ssize i = 0;
  struct rray_arg* index_arg =
    new_subscript_arg(indices_arg, r_null, indices_size, &i);
  KEEP(index_arg->shelter);

  for (; i < indices_size; ++i) {
    r_obj* index =
      rray_as_index_array(v_indices[i], v_dimensions[i], index_arg, error_call);
    r_list_poke(out, i, index);
  }

  FREE(2);
  return out;
}

r_obj* ffi_rray_as_index_array(
  r_obj* ffi_x,
  r_obj* ffi_dimension,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const int dimension =
    arg_as_int(ffi_dimension, rray_args.dimension, error_call);
  return rray_as_index_array(ffi_x, dimension, rray_args.x, error_call);
}

r_obj* rray_as_index_array(
  r_obj* x,
  int dimension,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);

  if (r_typeof(x) != R_TYPE_integer) {
    r_abort_lazy_call(
      error_call,
      "%s must be an integer array, not %s.",
      rray_arg_format_input(arg),
      r_obj_type_friendly(x)
    );
  }

  x = KEEP(vec_as_array(x));

  const r_ssize size = r_length(x);
  const int* v_x = r_int_cbegin(x);

  bool any_problems = false;

  // It's 50% faster to do a branchless check for any problems, then fall back
  // to the slow loop if we actually need to locate and report the problem
  for (r_ssize i = 0; i < size; ++i) {
    const int elt = v_x[i];
    any_problems |= (elt != r_globals.na_int) & ((elt < 1) | (elt > dimension));
  }

  if (any_problems) {
    stop_index_array_problem(v_x, size, dimension, arg, error_call);
  }

  FREE(1);
  return x;
}

static r_no_return void stop_index_array_problem(
  const int* v_x,
  r_ssize size,
  int dimension,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  for (r_ssize i = 0; i < size; ++i) {
    const int elt = v_x[i];

    if (elt == r_globals.na_int) {
      continue;
    }
    if (elt < 1) {
      r_abort_lazy_call(
        error_call,
        "%s must only contain positive values or missing values.",
        rray_arg_format_input(arg)
      );
    }
    if (elt > dimension) {
      r_abort_lazy_call(
        error_call,
        "%s must not contain values greater than %d.",
        rray_arg_format_input(arg),
        dimension
      );
    }
  }

  r_stop_unreachable();
}

static bool rray_any_missing_index(
  r_obj* const* v_indices,
  r_ssize indices_size
) {
  bool out = false;

  for (r_ssize i = 0; i < indices_size; ++i) {
    r_obj* index = v_indices[i];
    const r_ssize size = r_length(index);
    const int* v_index = r_int_cbegin(index);

    for (r_ssize j = 0; j < size; ++j) {
      out |= v_index[j] == r_globals.na_int;
    }

    if (out) {
      break;
    }
  }

  return out;
}

// Builds a flat location into `x` from the current multidimensional point
// represented by the indices.
static inline r_ssize rray_index_location(
  const r_ssize* v_x_strides,
  const int* const* v_v_index,
  const r_ssize* v_location,
  const r_ssize* v_strides,
  r_ssize run_i,
  r_ssize indices_size
) {
  r_ssize out = 0;

  for (r_ssize i = 0; i < indices_size; ++i) {
    const int* v_index = v_v_index[i];
    const r_ssize location = v_location[i] + run_i * v_strides[i];
    const int index = v_index[location];
    out += (r_ssize) (index - 1) * v_x_strides[i];
  }

  return out;
}

static inline r_ssize rray_index_location_missing(
  const r_ssize* v_x_strides,
  const int* const* v_v_index,
  const r_ssize* v_location,
  const r_ssize* v_strides,
  r_ssize run_i,
  r_ssize indices_size
) {
  r_ssize out = 0;

  for (r_ssize i = 0; i < indices_size; ++i) {
    const int* v_index = v_v_index[i];
    const r_ssize location = v_location[i] + run_i * v_strides[i];
    const int index = v_index[location];
    if (index == r_globals.na_int) {
      return -1;
    }
    out += (r_ssize) (index - 1) * v_x_strides[i];
  }

  return out;
}

#define RRAY_INDEX_LOOP(POKE, NEXT, INDICES_SIZE)                              \
  for (; !rray_run_iterator_done(&it); NEXT(&it)) {                            \
    const r_ssize start = rray_run_iterator_start(&it);                        \
    const r_ssize end = rray_run_iterator_end(&it);                            \
                                                                               \
    const r_ssize* v_location = rray_run_iterator_v_loc(&it);                  \
    const r_ssize* v_strides = rray_run_iterator_v_strides(&it);               \
                                                                               \
    for (r_ssize i = start; i < end; ++i) {                                    \
      const r_ssize location = rray_index_location(                            \
        v_x_strides,                                                           \
        v_v_index,                                                             \
        v_location,                                                            \
        v_strides,                                                             \
        i - start,                                                             \
        INDICES_SIZE                                                           \
      );                                                                       \
      POKE(out, i, v_x[location]);                                             \
    }                                                                          \
  }

#define RRAY_INDEX_LOOP_MISSING(POKE, MISSING, NEXT, INDICES_SIZE)             \
  for (; !rray_run_iterator_done(&it); NEXT(&it)) {                            \
    const r_ssize start = rray_run_iterator_start(&it);                        \
    const r_ssize end = rray_run_iterator_end(&it);                            \
                                                                               \
    const r_ssize* v_location = rray_run_iterator_v_loc(&it);                  \
    const r_ssize* v_strides = rray_run_iterator_v_strides(&it);               \
                                                                               \
    for (r_ssize i = start; i < end; ++i) {                                    \
      const r_ssize location = rray_index_location_missing(                    \
        v_x_strides,                                                           \
        v_v_index,                                                             \
        v_location,                                                            \
        v_strides,                                                             \
        i - start,                                                             \
        INDICES_SIZE                                                           \
      );                                                                       \
      POKE(out, i, location == -1 ? MISSING : v_x[location]);                  \
    }                                                                          \
  }

#define RRAY_INDEX_ITERATE(POKE, MISSING)                                      \
  if (any_missing) {                                                           \
    switch (indices_size) {                                                    \
    case 1:                                                                    \
      RRAY_INDEX_LOOP_MISSING(POKE, MISSING, rray_run_iterator_next1, 1);      \
      break;                                                                   \
    case 2:                                                                    \
      RRAY_INDEX_LOOP_MISSING(POKE, MISSING, rray_run_iterator_next2, 2);      \
      break;                                                                   \
    case 3:                                                                    \
      RRAY_INDEX_LOOP_MISSING(POKE, MISSING, rray_run_iterator_next3, 3);      \
      break;                                                                   \
    case 4:                                                                    \
      RRAY_INDEX_LOOP_MISSING(POKE, MISSING, rray_run_iterator_next4, 4);      \
      break;                                                                   \
    default:                                                                   \
      RRAY_INDEX_LOOP_MISSING(                                                 \
        POKE,                                                                  \
        MISSING,                                                               \
        rray_run_iterator_next,                                                \
        indices_size                                                           \
      );                                                                       \
      break;                                                                   \
    }                                                                          \
  } else {                                                                     \
    switch (indices_size) {                                                    \
    case 1:                                                                    \
      RRAY_INDEX_LOOP(POKE, rray_run_iterator_next1, 1);                       \
      break;                                                                   \
    case 2:                                                                    \
      RRAY_INDEX_LOOP(POKE, rray_run_iterator_next2, 2);                       \
      break;                                                                   \
    case 3:                                                                    \
      RRAY_INDEX_LOOP(POKE, rray_run_iterator_next3, 3);                       \
      break;                                                                   \
    case 4:                                                                    \
      RRAY_INDEX_LOOP(POKE, rray_run_iterator_next4, 4);                       \
      break;                                                                   \
    default:                                                                   \
      RRAY_INDEX_LOOP(POKE, rray_run_iterator_next, indices_size);             \
      break;                                                                   \
    }                                                                          \
  }

#define RRAY_INDEX_ATOMIC_POKE(OUT, I, VALUE) v_out[I] = (VALUE)

#define RRAY_INDEX_ATOMIC(RTYPE, CTYPE, CONST_DEREF, DEREF, MISSING)           \
  struct rray_run_iterator it = rray_run_iterator(                             \
    v_dimensions,                                                              \
    dimensionality,                                                            \
    v_v_indices_strides,                                                       \
    indices_size                                                               \
  );                                                                           \
                                                                               \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, rray_run_iterator_size(&it)));       \
  CTYPE* v_out = DEREF(out);                                                   \
                                                                               \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
                                                                               \
  RRAY_INDEX_ITERATE(RRAY_INDEX_ATOMIC_POKE, MISSING);                         \
                                                                               \
  FREE(1);                                                                     \
  return out;

#define RRAY_INDEX_BARRIER(RTYPE, CONST_DEREF, POKE, MISSING)                  \
  struct rray_run_iterator it = rray_run_iterator(                             \
    v_dimensions,                                                              \
    dimensionality,                                                            \
    v_v_indices_strides,                                                       \
    indices_size                                                               \
  );                                                                           \
                                                                               \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, rray_run_iterator_size(&it)));       \
                                                                               \
  r_obj* const* v_x = CONST_DEREF(x);                                          \
                                                                               \
  RRAY_INDEX_ITERATE(POKE, MISSING);                                           \
                                                                               \
  FREE(1);                                                                     \
  return out;

static r_obj* rray_index_lgl(
  r_obj* x,
  const r_ssize* v_x_strides,
  const int* const* v_v_index,
  r_ssize indices_size,
  bool any_missing,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_indices_strides
) {
  RRAY_INDEX_ATOMIC(
    R_TYPE_logical,
    int,
    r_lgl_cbegin,
    r_lgl_begin,
    r_globals.na_lgl
  );
}

static r_obj* rray_index_int(
  r_obj* x,
  const r_ssize* v_x_strides,
  const int* const* v_v_index,
  r_ssize indices_size,
  bool any_missing,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_indices_strides
) {
  RRAY_INDEX_ATOMIC(
    R_TYPE_integer,
    int,
    r_int_cbegin,
    r_int_begin,
    r_globals.na_int
  );
}

static r_obj* rray_index_dbl(
  r_obj* x,
  const r_ssize* v_x_strides,
  const int* const* v_v_index,
  r_ssize indices_size,
  bool any_missing,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_indices_strides
) {
  RRAY_INDEX_ATOMIC(
    R_TYPE_double,
    double,
    r_dbl_cbegin,
    r_dbl_begin,
    r_globals.na_dbl
  );
}

static r_obj* rray_index_cpl(
  r_obj* x,
  const r_ssize* v_x_strides,
  const int* const* v_v_index,
  r_ssize indices_size,
  bool any_missing,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_indices_strides
) {
  RRAY_INDEX_ATOMIC(
    R_TYPE_complex,
    r_complex,
    r_cpl_cbegin,
    r_cpl_begin,
    r_globals.na_cpl
  );
}

static r_obj* rray_index_raw(
  r_obj* x,
  const r_ssize* v_x_strides,
  const int* const* v_v_index,
  r_ssize indices_size,
  bool any_missing,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_indices_strides
) {
  RRAY_INDEX_ATOMIC(R_TYPE_raw, Rbyte, r_raw_cbegin, r_raw_begin, 0);
}

static r_obj* rray_index_chr(
  r_obj* x,
  const r_ssize* v_x_strides,
  const int* const* v_v_index,
  r_ssize indices_size,
  bool any_missing,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_indices_strides
) {
  RRAY_INDEX_BARRIER(
    R_TYPE_character,
    r_chr_cbegin,
    r_chr_poke,
    r_globals.na_str
  );
}

static r_obj* rray_index_list(
  r_obj* x,
  const r_ssize* v_x_strides,
  const int* const* v_v_index,
  r_ssize indices_size,
  bool any_missing,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_indices_strides
) {
  RRAY_INDEX_BARRIER(R_TYPE_list, r_list_cbegin, r_list_poke, r_null);
}

#undef RRAY_INDEX_LOOP
#undef RRAY_INDEX_LOOP_MISSING
#undef RRAY_INDEX_ITERATE
#undef RRAY_INDEX_ATOMIC_POKE
#undef RRAY_INDEX_ATOMIC
#undef RRAY_INDEX_BARRIER
