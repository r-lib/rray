#include "utils.h"

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
  r_obj* dimension_sizes = r_int(r_ssize_as_integer(size));
  r_attrib_poke_dim(out, dimension_sizes);

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
