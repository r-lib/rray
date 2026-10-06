#include "utils.h"

#include <limits.h>

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
  if (r_typeof(x) != R_TYPE_integer) {
    x = vec_cast(x, r_globals.empty_int, arg, NULL);
  }
  KEEP(x);

  if (r_attrib_has_any(x)) {
    r_abort_lazy_call(
      error_call,
      "%s can't have attributes.",
      rray_arg_format(arg)
    );
  }

  FREE(1);
  return x;
}

int arg_as_int(r_obj* x, struct rray_arg* arg, struct r_lazy error_call) {
  x = KEEP(arg_as_bare_integer(x, arg, error_call));

  if (r_length(x) != 1) {
    r_abort_lazy_call(
      error_call,
      "%s must be a single integer, not length %" R_PRIdXLEN_T ".",
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

r_obj* vec_cast(
  r_obj* x,
  r_obj* to,
  struct rray_arg* x_arg,
  struct rray_arg* to_arg
) {
  r_obj* x_arg_chr = KEEP(rray_arg(x_arg));
  r_obj* to_arg_chr = KEEP(rray_arg(to_arg));

  r_obj* mask = KEEP(r_alloc_environment(4, r_envs.global));

  r_env_bind(mask, r_syms.x, x);
  r_env_bind(mask, rray_syms.to, to);
  r_env_bind(mask, rray_syms.x_arg, x_arg_chr);
  r_env_bind(mask, rray_syms.to_arg, to_arg_chr);

  r_obj* out = r_eval(vec_cast_call, mask);

  FREE(3);
  return out;
}

r_obj* vec_cast_call = NULL;

void rray_init_utils(r_obj* ns) {
  vec_cast_call =
    r_parse("vctrs::vec_cast(x, to, x_arg = x_arg, to_arg = to_arg)");
  r_preserve(vec_cast_call);
}
