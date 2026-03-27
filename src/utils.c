#include "utils.h"

#include "decl/utils-decl.h"
#include "wrapper.h"

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

  r_obj* names = r_names(x);
  if (names != r_null) {
    KEEP(names);
    r_obj* dimension_names = KEEP(r_alloc_list(1));
    r_list_poke(dimension_names, 0, names);
    r_attrib_poke_dim_names(out, dimension_names);
    r_attrib_zap(out, r_syms.names);
    FREE(2);
  }

  FREE(1);
  return out;
}

r_obj* arg_as_array(r_obj* x, const char* arg, struct r_lazy error_call) {
  switch (r_typeof(x)) {
    case R_TYPE_logical:
    case R_TYPE_integer:
    case R_TYPE_double:
    case R_TYPE_complex:
    case R_TYPE_character:
    case R_TYPE_raw:
    case R_TYPE_list:
      break;
    default:
      r_abort_lazy_call(
        error_call,
        "`x` must be an array, not %s.",
        r_obj_type_friendly(x)
      );
  }

  if (r_dim(x) == r_null) {
    return vec_as_array(x);
  }

  return x;
}

r_obj* vec_cast(r_obj* x, r_obj* to, r_obj* x_arg, r_obj* to_arg) {
  r_obj* mask = KEEP(r_alloc_environment(4, r_envs.global));

  r_env_bind(mask, r_syms.x, x);
  r_env_bind(mask, to_sym, to);
  r_env_bind(mask, x_arg_sym, x_arg);
  r_env_bind(mask, to_arg_sym, to_arg);

  r_obj* out = r_eval(vec_cast_call, mask);

  FREE(1);
  return out;
}

r_obj* to_sym = NULL;
r_obj* to_arg_sym = NULL;
r_obj* x_arg_sym = NULL;

r_obj* axes_chr = NULL;
r_obj* dimensions_chr = NULL;

r_obj* vec_cast_call = NULL;

void rray_init_utils(r_obj* ns) {
  to_sym = r_sym("to");
  to_arg_sym = r_sym("to_arg");
  x_arg_sym = r_sym("x_arg");

  axes_chr = r_chr("axes");
  r_preserve(axes_chr);

  dimensions_chr = r_chr("dimensions");
  r_preserve(dimensions_chr);

  vec_cast_call =
    r_parse("vctrs::vec_cast(x, to, x_arg = x_arg, to_arg = to_arg)");
  r_preserve(vec_cast_call);
}
