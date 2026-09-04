#include "utils.h"

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
static inline r_obj* vec_as_array(r_obj* x) {
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

void check_unclassed(
  r_obj* x,
  struct rray_arg* p_arg,
  struct r_lazy error_call
) {
  if (r_is_object(x)) {
    r_abort_lazy_call(
      error_call,
      "%s must be a bare array, not %s.",
      rray_arg_format_input(p_arg),
      r_obj_type_friendly(x)
    );
  }
}

r_obj* arg_as_array(
  r_obj* x,
  struct rray_arg* p_arg,
  struct r_lazy error_call
) {
  check_array(x, p_arg, error_call);

  if (r_dim(x) == r_null) {
    return vec_as_array(x);
  }

  return x;
}

enum rray_type arg_as_type(
  r_obj* x,
  struct rray_arg* p_arg,
  struct r_lazy error_call
) {
  check_array(x, p_arg, error_call);
  return rray_typeof(x);
}

static void check_array(
  r_obj* x,
  struct rray_arg* p_arg,
  struct r_lazy error_call
) {
  switch (r_typeof(x)) {
  case R_TYPE_logical:
  case R_TYPE_integer:
  case R_TYPE_double:
  case R_TYPE_complex:
  case R_TYPE_character:
  case R_TYPE_raw:
  case R_TYPE_list:
    return;
  default:
    r_abort_lazy_call(
      error_call,
      "%s must be an array, not %s.",
      rray_arg_format_input(p_arg),
      r_obj_type_friendly(x)
    );
  }
}

int arg_as_int(r_obj* x, struct rray_arg* p_arg, struct r_lazy error_call) {
  if (r_typeof(x) != R_TYPE_integer) {
    x = KEEP(vec_cast(x, r_globals.empty_int, p_arg, NULL));
  } else {
    KEEP(x);
  }

  if (r_attrib_has_any(x)) {
    r_abort_lazy_call(
      error_call,
      "%s can't have attributes.",
      rray_arg_format(p_arg)
    );
  }

  if (r_length(x) != 1) {
    r_abort_lazy_call(
      error_call,
      "%s must be a single integer, not length %" R_PRIdXLEN_T ".",
      rray_arg_format(p_arg),
      r_length(x)
    );
  }

  const int out = r_int_get(x, 0);

  FREE(1);
  return out;
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

r_obj* vec_cast(
  r_obj* x,
  r_obj* to,
  struct rray_arg* p_x_arg,
  struct rray_arg* p_to_arg
) {
  r_obj* x_arg_chr = KEEP(rray_arg(p_x_arg));
  r_obj* to_arg_chr = KEEP(rray_arg(p_to_arg));

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
