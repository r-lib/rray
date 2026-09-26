#include "subscript.h"

#include <math.h>

#include "decl/subscript-decl.h"

struct rray_subscript rray_as_locations_subscript(
  r_obj* index,
  r_ssize size,
  struct rray_arg* index_arg,
  struct r_lazy error_call
) {
  const r_ssize index_size = r_length(index);
  const struct rray_subscript_summary summary =
    rray_subscript_summarise(index, 0, index_size);

  if (summary.any_fractional) {
    stop_subscript_fractional(index_arg, error_call);
  }
  if (summary.max > size) {
    r_abort_lazy_call(
      error_call,
      "%s must not contain values greater than %" R_PRI_SSIZE ".",
      rray_arg_format(index_arg),
      size
    );
  }
  if (summary.min < -size) {
    r_abort_lazy_call(
      error_call,
      "%s must not contain values less than -%" R_PRI_SSIZE ".",
      rray_arg_format(index_arg),
      size
    );
  }

  const bool any_negative = summary.min < 0;

  if (any_negative && summary.max > 0) {
    r_abort_lazy_call(
      error_call,
      "%s can't mix positive and negative values.",
      rray_arg_format(index_arg)
    );
  }
  if (any_negative && summary.any_missing) {
    r_abort_lazy_call(
      error_call,
      "%s can't mix negative and missing values.",
      rray_arg_format(index_arg)
    );
  }

  if (any_negative) {
    return rray_as_complement_subscript(index, size);
  }
  if (summary.zeros != 0) {
    return rray_as_nonzero_subscript(index, index_size - summary.zeros);
  }

  return (struct rray_subscript){
    .index = index,
    .kind = r_typeof(index) == R_TYPE_integer
      ? RRAY_SUBSCRIPT_KIND_locations_int
      : RRAY_SUBSCRIPT_KIND_locations_dbl,
    .size = index_size
  };
}

static struct rray_subscript rray_as_complement_subscript(
  r_obj* index,
  r_ssize size
) {
  const r_ssize index_size = r_length(index);

  r_obj* out = KEEP(r_alloc_logical(size));
  int* v_out = r_lgl_begin(out);

  for (r_ssize i = 0; i < size; ++i) {
    v_out[i] = 1;
  }

  switch (r_typeof(index)) {
  case R_TYPE_integer: {
    const int* v_index = r_int_cbegin(index);

    for (r_ssize i = 0; i < index_size; ++i) {
      const int elt = v_index[i];

      if (elt != 0) {
        v_out[-(r_ssize) elt - 1] = 0;
      }
    }

    break;
  }
  case R_TYPE_double: {
    const double* v_index = r_dbl_cbegin(index);

    for (r_ssize i = 0; i < index_size; ++i) {
      const double elt = v_index[i];

      if (elt != 0) {
        v_out[-(r_ssize) elt - 1] = 0;
      }
    }

    break;
  }
  default:
    r_stop_unreachable();
  }

  const struct rray_subscript subscript = {
    .index = out,
    .kind = RRAY_SUBSCRIPT_KIND_mask,
    .size = rray_mask_size(out, size)
  };

  FREE(1);
  return subscript;
}

static struct rray_subscript rray_as_nonzero_subscript(
  r_obj* index,
  r_ssize size
) {
  const r_ssize index_size = r_length(index);

  switch (r_typeof(index)) {
  case R_TYPE_integer: {
    const int* v_index = r_int_cbegin(index);

    r_obj* out = KEEP(r_alloc_integer(size));
    int* v_out = r_int_begin(out);

    r_ssize j = 0;

    for (r_ssize i = 0; i < index_size; ++i) {
      const int elt = v_index[i];

      if (elt != 0) {
        v_out[j] = elt;
        ++j;
      }
    }

    const struct rray_subscript subscript =
      {.index = out, .kind = RRAY_SUBSCRIPT_KIND_locations_int, .size = size};

    FREE(1);
    return subscript;
  }
  case R_TYPE_double: {
    const double* v_index = r_dbl_cbegin(index);

    r_obj* out = KEEP(r_alloc_double(size));
    double* v_out = r_dbl_begin(out);

    r_ssize j = 0;

    for (r_ssize i = 0; i < index_size; ++i) {
      const double elt = v_index[i];

      if (elt != 0) {
        v_out[j] = elt;
        ++j;
      }
    }

    const struct rray_subscript subscript =
      {.index = out, .kind = RRAY_SUBSCRIPT_KIND_locations_dbl, .size = size};

    FREE(1);
    return subscript;
  }
  default:
    r_stop_unreachable();
  }
}

struct rray_subscript rray_as_mask_subscript(
  r_obj* index,
  r_ssize size,
  struct rray_arg* index_arg,
  struct r_lazy error_call
) {
  // Check for size 1 or size `size`. Note we don't expand a single `TRUE`, we
  // have native support for that.
  const r_ssize index_size = r_length(index);

  if (index_size != 1 && index_size != size) {
    r_abort_lazy_call(
      error_call,
      "Logical %s must be size 1 or %" R_PRI_SSIZE ", not %" R_PRI_SSIZE ".",
      rray_arg_format(index_arg),
      size,
      index_size
    );
  }

  return (struct rray_subscript){
    .index = index,
    .kind = RRAY_SUBSCRIPT_KIND_mask,
    .size = rray_mask_size(index, size)
  };
}

// Computes number of used values in `x`. Includes both `TRUE` and `NA` logical
// values!
r_ssize rray_mask_size(r_obj* mask, r_ssize size) {
  const int* v_mask = r_lgl_cbegin(mask);
  const r_ssize mask_step = r_length(mask) == 1 ? 0 : 1;

  r_ssize out = 0;

  for (r_ssize i = 0; i < size; ++i) {
    out += v_mask[i * mask_step] != 0;
  }

  return out;
}

struct rray_subscript_summary rray_subscript_summarise(
  r_obj* index,
  r_ssize start,
  r_ssize size
) {
  switch (r_typeof(index)) {
  case R_TYPE_integer:
    return rray_subscript_summarise_int(r_int_cbegin(index) + start, size);
  case R_TYPE_double:
    return rray_subscript_summarise_dbl(r_dbl_cbegin(index) + start, size);
  default:
    r_stop_unreachable();
  }
}

static struct rray_subscript_summary rray_subscript_summarise_int(
  const int* v_index,
  r_ssize size
) {
  struct rray_subscript_summary out = {
    .min = INFINITY,
    .max = -INFINITY,
    .zeros = 0,
    .any_missing = false,
    .any_fractional = false
  };

  for (r_ssize i = 0; i < size; ++i) {
    const int elt = v_index[i];

    if (elt == r_globals.na_int) {
      out.any_missing = true;
      continue;
    }

    out.min = fmin(out.min, elt);
    out.max = fmax(out.max, elt);
    out.zeros += elt == 0;
  }

  return out;
}

static struct rray_subscript_summary rray_subscript_summarise_dbl(
  const double* v_index,
  r_ssize size
) {
  struct rray_subscript_summary out = {
    .min = INFINITY,
    .max = -INFINITY,
    .zeros = 0,
    .any_missing = false,
    .any_fractional = false
  };

  for (r_ssize i = 0; i < size; ++i) {
    const double elt = v_index[i];

    if (isnan(elt)) {
      out.any_missing = true;
      continue;
    }

    out.min = fmin(out.min, elt);
    out.max = fmax(out.max, elt);
    out.zeros += elt == 0;
    out.any_fractional |= elt != trunc(elt);
  }

  return out;
}

r_no_return void stop_subscript_fractional(
  struct rray_arg* index_arg,
  struct r_lazy error_call
) {
  r_abort_lazy_call(
    error_call,
    "Can't convert from %s <double> to <integer> due to loss of precision.",
    rray_arg_format(index_arg)
  );
}

r_obj* rray_subscript_as_list(struct rray_subscript subscript) {
  const char* v_names[] = {"index", "kind", "size"};
  r_obj* names = KEEP(r_chr_n(v_names, 3));

  r_obj* out = KEEP(r_alloc_list(3));
  r_attrib_poke_names(out, names);

  r_list_poke(out, 0, subscript.index);
  r_list_poke(out, 1, r_chr(rray_subscript_kind_name(subscript.kind)));
  r_list_poke(out, 2, r_int(r_ssize_as_integer(subscript.size)));

  FREE(2);
  return out;
}

static const char* rray_subscript_kind_name(enum rray_subscript_kind kind) {
  switch (kind) {
  case RRAY_SUBSCRIPT_KIND_locations_int:
    return "locations_int";
  case RRAY_SUBSCRIPT_KIND_locations_dbl:
    return "locations_dbl";
  case RRAY_SUBSCRIPT_KIND_mask:
    return "mask";
  case RRAY_SUBSCRIPT_KIND_points_int:
    return "points_int";
  case RRAY_SUBSCRIPT_KIND_points_dbl:
    return "points_dbl";
  }

  r_stop_unreachable();
}
