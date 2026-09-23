#include "index.h"

#include "dimensionality.h"
#include "dimensions.h"
#include "size.h"
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
  const int x_dimensionality =
    rray_dimensionality_from_dimensions(x_dimensions);
  check_dimensionality(x_dimensionality);

  const r_ssize indices_size = r_length(indices);
  check_index_argument_count(indices_size, x_dimensionality, error_call);

  if (r_names(indices) != r_null) {
    r_abort_lazy_call(error_call, "All elements of `...` must be unnamed.");
  }

  r_obj* normalized = KEEP(r_alloc_list(indices_size));
  r_obj* const* v_indices = r_list_cbegin(indices);
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);

  r_ssize i = 0;
  struct rray_arg* index_arg =
    new_subscript_arg(indices_arg, r_null, indices_size, &i);
  KEEP(index_arg->shelter);

  for (; i < indices_size; ++i) {
    r_obj* index = rray_as_index_array(
      v_indices[i],
      v_x_dimensions[i],
      index_arg,
      error_call
    );
    r_list_poke(normalized, i, index);
  }

  r_obj* dimensions = KEEP(
    rray_dimensions_common_opts(normalized, NULL, 0, indices_arg, error_call)
  );

  const struct rray_index_plan plan =
    rray_index_plan(x_dimensions, normalized, dimensions, error_call);

  r_obj* out;

  switch (r_typeof(x)) {
  case R_TYPE_logical:
    out = rray_index_lgl(x, &plan);
    break;
  case R_TYPE_integer:
    out = rray_index_int(x, &plan);
    break;
  case R_TYPE_double:
    out = rray_index_dbl(x, &plan);
    break;
  case R_TYPE_complex:
    out = rray_index_cpl(x, &plan);
    break;
  case R_TYPE_raw:
    out = rray_index_raw(x, &plan);
    break;
  case R_TYPE_character:
    out = rray_index_chr(x, &plan);
    break;
  case R_TYPE_list:
    out = rray_index_list(x, &plan);
    break;
  default:
    r_stop_unreachable();
  }

  KEEP(out);
  r_attrib_poke_dim(out, dimensions);

  FREE(6);
  return out;
}

r_obj* ffi_rray_as_index_array(
  r_obj* ffi_x,
  r_obj* ffi_dimension,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const int dimension =
    rray_as_index_dimension(ffi_dimension, rray_args.dimension, error_call);
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

  FREE(1);
  return x;
}

static int rray_as_index_dimension(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  const int out = arg_as_int(x, arg, error_call);

  if (out == r_globals.na_int) {
    r_abort_lazy_call(
      error_call,
      "%s must not be missing.",
      rray_arg_format(arg)
    );
  }
  if (out < 0) {
    r_abort_lazy_call(
      error_call,
      "%s must not be negative.",
      rray_arg_format(arg)
    );
  }

  return out;
}

static void check_index_argument_count(
  r_ssize size,
  int dimensionality,
  struct r_lazy error_call
) {
  if (size == dimensionality) {
    return;
  }

  r_abort_lazy_call(
    error_call,
    "Must supply exactly %d coordinate array%s to `...`, not %" R_PRI_SSIZE ".",
    dimensionality,
    dimensionality == 1 ? "" : "s",
    size
  );
}

static struct rray_index_plan rray_index_plan(
  r_obj* x_dimensions,
  r_obj* indices,
  r_obj* dimensions,
  struct r_lazy error_call
) {
  struct rray_index_plan plan;

  const int* v_x_dimensions = r_int_cbegin(x_dimensions);
  const int* v_dimensions = r_int_cbegin(dimensions);
  const int x_dimensionality =
    rray_dimensionality_from_dimensions(x_dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(dimensions);

  plan.size =
    rray_size_from_dimensions_checked(v_dimensions, dimensionality, error_call);
  plan.x_dimensionality = x_dimensionality;
  plan.dimensionality = dimensionality;

  rray_fill_strides_from_dimensions(
    v_x_dimensions,
    x_dimensionality,
    plan.v_x_strides
  );

  for (int axis = 0; axis < dimensionality; ++axis) {
    plan.v_dimensions[axis] = v_dimensions[axis];
  }

  r_obj* const* v_indices = r_list_cbegin(indices);

  for (int index_axis = 0; index_axis < x_dimensionality; ++index_axis) {
    r_obj* index = v_indices[index_axis];
    r_obj* index_dimensions = r_dim(index);
    const int* v_index_dimensions = r_int_cbegin(index_dimensions);
    const int index_dimensionality =
      rray_dimensionality_from_dimensions(index_dimensions);
    r_ssize v_strides[RRAY_MAX_DIMENSIONALITY];

    rray_fill_broadcast_strides_from_dimensions(
      v_index_dimensions,
      index_dimensionality,
      dimensionality,
      v_strides
    );

    for (int axis = 0; axis < dimensionality; ++axis) {
      plan.v_index_strides[axis][index_axis] = v_strides[axis];
    }

    plan.v_indices[index_axis] = r_int_cbegin(index);
  }

  return plan;
}

static inline bool rray_index_plan_source_location(
  const struct rray_index_plan* plan,
  const r_ssize* v_index_locations,
  r_ssize* p_source_location
) {
  r_ssize out = 0;

  for (int axis = 0; axis < plan->x_dimensionality; ++axis) {
    const int index = plan->v_indices[axis][v_index_locations[axis]];

    if (index == r_globals.na_int) {
      return false;
    }

    out += (r_ssize) (index - 1) * plan->v_x_strides[axis];
  }

  *p_source_location = out;
  return true;
}

static inline void rray_index_plan_next(
  const struct rray_index_plan* plan,
  int* v_point,
  r_ssize* v_index_locations
) {
  for (int axis = 0; axis < plan->dimensionality; ++axis) {
    ++v_point[axis];

    if (v_point[axis] < plan->v_dimensions[axis]) {
      for (int index_axis = 0; index_axis < plan->x_dimensionality;
           ++index_axis) {
        v_index_locations[index_axis] +=
          plan->v_index_strides[axis][index_axis];
      }
      break;
    }

    v_point[axis] = 0;

    for (int index_axis = 0; index_axis < plan->x_dimensionality;
         ++index_axis) {
      v_index_locations[index_axis] -= (plan->v_dimensions[axis] - 1) *
        plan->v_index_strides[axis][index_axis];
    }
  }
}

#define RRAY_INDEX_ATOMIC(RTYPE, CTYPE, CONST_DEREF, DEREF, MISSING)           \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, plan->size));                        \
  CTYPE* v_out = DEREF(out);                                                   \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
  int v_point[RRAY_MAX_DIMENSIONALITY] = {0};                                  \
  r_ssize v_index_locations[RRAY_MAX_DIMENSIONALITY] = {0};                    \
                                                                               \
  for (r_ssize i = 0; i < plan->size; ++i) {                                   \
    r_ssize source_location = 0;                                               \
    const bool found = rray_index_plan_source_location(                        \
      plan,                                                                    \
      v_index_locations,                                                       \
      &source_location                                                         \
    );                                                                         \
    v_out[i] = found ? v_x[source_location] : MISSING;                         \
    rray_index_plan_next(plan, v_point, v_index_locations);                    \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

#define RRAY_INDEX_BARRIER(RTYPE, CONST_DEREF, POKE, MISSING)                  \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, plan->size));                        \
  r_obj* const* v_x = CONST_DEREF(x);                                          \
  int v_point[RRAY_MAX_DIMENSIONALITY] = {0};                                  \
  r_ssize v_index_locations[RRAY_MAX_DIMENSIONALITY] = {0};                    \
                                                                               \
  for (r_ssize i = 0; i < plan->size; ++i) {                                   \
    r_ssize source_location = 0;                                               \
    const bool found = rray_index_plan_source_location(                        \
      plan,                                                                    \
      v_index_locations,                                                       \
      &source_location                                                         \
    );                                                                         \
    POKE(out, i, found ? v_x[source_location] : MISSING);                      \
    rray_index_plan_next(plan, v_point, v_index_locations);                    \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

static r_obj* rray_index_lgl(r_obj* x, const struct rray_index_plan* plan) {
  RRAY_INDEX_ATOMIC(
    R_TYPE_logical,
    int,
    r_lgl_cbegin,
    r_lgl_begin,
    r_globals.na_lgl
  );
}

static r_obj* rray_index_int(r_obj* x, const struct rray_index_plan* plan) {
  RRAY_INDEX_ATOMIC(
    R_TYPE_integer,
    int,
    r_int_cbegin,
    r_int_begin,
    r_globals.na_int
  );
}

static r_obj* rray_index_dbl(r_obj* x, const struct rray_index_plan* plan) {
  RRAY_INDEX_ATOMIC(
    R_TYPE_double,
    double,
    r_dbl_cbegin,
    r_dbl_begin,
    r_globals.na_dbl
  );
}

static r_obj* rray_index_cpl(r_obj* x, const struct rray_index_plan* plan) {
  RRAY_INDEX_ATOMIC(
    R_TYPE_complex,
    r_complex,
    r_cpl_cbegin,
    r_cpl_begin,
    r_globals.na_cpl
  );
}

static r_obj* rray_index_raw(r_obj* x, const struct rray_index_plan* plan) {
  RRAY_INDEX_ATOMIC(R_TYPE_raw, Rbyte, r_raw_cbegin, r_raw_begin, 0);
}

static r_obj* rray_index_chr(r_obj* x, const struct rray_index_plan* plan) {
  RRAY_INDEX_BARRIER(
    R_TYPE_character,
    r_chr_cbegin,
    r_chr_poke,
    r_globals.na_str
  );
}

static r_obj* rray_index_list(r_obj* x, const struct rray_index_plan* plan) {
  RRAY_INDEX_BARRIER(R_TYPE_list, r_list_cbegin, r_list_poke, r_null);
}

#undef RRAY_INDEX_ATOMIC
#undef RRAY_INDEX_BARRIER
