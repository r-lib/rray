#include "sum.h"

#include "types.h"
#include "utils.h"

r_obj* ffi_rray_sum(r_obj* x, r_obj* axes, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  return rray_sum(x, axes, error_call);
}

#define RRAY_TYPE RRAY_TYPE_LOGICAL
#include "sum-template.h"

#define RRAY_TYPE RRAY_TYPE_INTEGER
#include "sum-template.h"

#define RRAY_TYPE RRAY_TYPE_DOUBLE
#include "sum-template.h"

#define RRAY_TYPE RRAY_TYPE_COMPLEX
#include "sum-template.h"

r_obj* rray_sum(r_obj* x, r_obj* axes, struct r_lazy error_call) {
  x = KEEP(arg_as_array(x, "x", error_call));

  r_obj* out;

  switch (r_typeof(x)) {
    case R_TYPE_logical:
      out = rray_sum_lgl(x, axes, error_call);
      break;
    case R_TYPE_integer:
      out = rray_sum_int(x, axes, error_call);
      break;
    case R_TYPE_double:
      out = rray_sum_dbl(x, axes, error_call);
      break;
    case R_TYPE_complex:
      out = rray_sum_cpl(x, axes, error_call);
      break;
    default:
      r_abort_lazy_call(
        error_call,
        "`x` must be a logical, integer, double, or complex array, not %s.",
        r_obj_type_friendly(x)
      );
  }

  FREE(1);
  return out;
}
