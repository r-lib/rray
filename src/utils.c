#include "utils.h"

#include <limits.h>
#include <math.h>

#include "syms.h"
#include "wrapper.h"

#include "decl/utils-decl.h"

// Normalize a vector into an array
//
// - Turns length into `dim`
// - Turns `names` into `dimnames`
// - Clears `names`
//
// Since we are only modifying attributes,
// we use a lightweight wrapper
r_obj* vec_as_array(r_obj* x) {
  if (r_dim(x) != r_null) {
    return x;
  }

  r_obj* out = KEEP(r_wrap(x));

  const r_ssize size = r_length(x);
  r_obj* dimensions = r_int(r_ssize_as_integer(size));
  r_attrib_poke_dim(out, dimensions);

  r_obj* axis_names = r_names(x);
  if (axis_names != r_null) {
    KEEP(axis_names);
    r_obj* names = KEEP(r_alloc_list(1));
    r_list_poke(names, 0, axis_names);
    r_attrib_poke_dim_names(out, names);
    r_attrib_zap(out, r_syms.names);
    FREE(2);
  }

  FREE(1);
  return out;
}

void check_unclassed(r_obj* x, struct rray_arg* arg, struct r_lazy error_call) {
  if (r_is_object(x)) {
    r_abort_lazy_call(
      error_call,
      "%s must be a bare array, not %s.",
      rray_arg_format_input(arg),
      r_obj_type_friendly(x)
    );
  }
}

r_obj* arg_as_array(r_obj* x, struct rray_arg* arg, struct r_lazy error_call) {
  if (rray_typeof(x) == RRAY_TYPE_scalar) {
    stop_scalar_input(x, arg, error_call);
  }

  return vec_as_array(x);
}

r_no_return void stop_scalar_input(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  r_abort_lazy_call(
    error_call,
    "%s must be an array, not %s.",
    rray_arg_format_input(arg),
    r_obj_type_friendly(x)
  );
}

r_no_return void stop_int_overflow(struct r_lazy error_call) {
  r_abort_lazy_call(error_call, "Integer overflow.");
}

r_obj* arg_as_bare_integer(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  if (r_attrib_has_any(x)) {
    r_abort_lazy_call(
      error_call,
      "%s can't have attributes.",
      rray_arg_format(arg)
    );
  }

  return arg_as_integer(x, arg, error_call);
}

static r_obj* arg_as_integer(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  switch (r_typeof(x)) {
  case R_TYPE_integer:
    return x;
  case R_TYPE_double:
    return arg_as_integer_from_double(x, arg, error_call);
  default:
    r_abort_lazy_call(
      error_call,
      "%s must be an integer or double vector, not %s.",
      rray_arg_format(arg),
      r_obj_type_friendly(x)
    );
  }
}

static r_obj* arg_as_integer_from_double(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  const r_ssize size = r_length(x);
  const double* v_x = r_dbl_cbegin(x);

  r_obj* out = KEEP(r_alloc_integer(size));
  int* v_out = r_int_begin(out);

  for (r_ssize i = 0; i < size; ++i) {
    const double elt = v_x[i];

    if (isnan(elt)) {
      v_out[i] = r_globals.na_int;
      continue;
    }

    if (elt <= INT_MIN || elt >= INT_MAX + 1.0 || (double) (int) elt != elt) {
      r_abort_lazy_call(
        error_call,
        "%s must contain whole numbers that fit in an integer. Problem at "
        "location %" R_PRI_SSIZE ".",
        rray_arg_format(arg),
        i + 1
      );
    }

    v_out[i] = (int) elt;
  }

  r_obj* names = r_names(x);
  if (names != r_null) {
    r_attrib_poke_names(out, names);
  }

  FREE(1);
  return out;
}

int arg_as_int(r_obj* x, struct rray_arg* arg, struct r_lazy error_call) {
  x = KEEP(arg_as_bare_integer(x, arg, error_call));

  if (r_length(x) != 1) {
    r_abort_lazy_call(
      error_call,
      "%s must be a single integer, not length %" R_PRI_SSIZE ".",
      rray_arg_format(arg),
      r_length(x)
    );
  }

  const int out = r_int_get(x, 0);

  FREE(1);
  return out;
}

r_obj* arg_as_non_negative_bare_integer(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  x = KEEP(arg_as_bare_integer(x, arg, error_call));

  const r_ssize size = r_length(x);
  const int* v_x = r_int_cbegin(x);

  for (r_ssize i = 0; i < size; ++i) {
    const int elt = v_x[i];

    if (elt == r_globals.na_int) {
      r_abort_lazy_call(
        error_call,
        "%s must not contain missing values.",
        rray_arg_format(arg)
      );
    }

    if (elt < 0) {
      r_abort_lazy_call(
        error_call,
        "%s must not contain negative values.",
        rray_arg_format(arg)
      );
    }
  }

  FREE(1);
  return x;
}

bool r_has_name_at(r_obj* names, r_ssize i) {
  if (r_typeof(names) != R_TYPE_character) {
    return false;
  }

  const r_ssize n = r_length(names);

  if (i >= n) {
    r_stop_internal(
      "`i` of %" R_PRI_SSIZE " can't be past the end of %" R_PRI_SSIZE ".",
      i,
      n
    );
  }

  return r_str_is_name(r_chr_get(names, i));
}

int int_add_checked(int x, int y) {
  if ((y > 0 && x > INT_MAX - y) || (y < 0 && x < INT_MIN - y)) {
    r_abort("Can't add %d and %d, the result doesn't fit in an integer.", x, y);
  }

  return x + y;
}
