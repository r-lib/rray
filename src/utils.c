#include "utils.h"

void check_array(r_obj* x) {
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
        r_lazy_null,
        "`x` must be an array, not %s.",
        r_obj_type_friendly(x)
      );
  }
}
