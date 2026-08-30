#include "split.h"

#include "types.h"
#include "utils.h"

r_obj* ffi_rray_split(r_obj* ffi_x, r_obj* ffi_axes, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_split(ffi_x, ffi_axes, error_call);
}

#define RRAY_TYPE RRAY_TYPE_LOGICAL
#include "split-template.h"

#define RRAY_TYPE RRAY_TYPE_INTEGER
#include "split-template.h"

#define RRAY_TYPE RRAY_TYPE_DOUBLE
#include "split-template.h"

#define RRAY_TYPE RRAY_TYPE_COMPLEX
#include "split-template.h"

#define RRAY_TYPE RRAY_TYPE_RAW
#include "split-template.h"

#define RRAY_TYPE RRAY_TYPE_CHARACTER
#include "split-template.h"

#define RRAY_TYPE RRAY_TYPE_LIST
#include "split-template.h"

r_obj* rray_split(r_obj* x, r_obj* axes, struct r_lazy error_call) {
  check_unclassed(x, "x", error_call);
  x = KEEP(arg_as_array(x, "x", error_call));

  r_obj* out;

  switch (r_typeof(x)) {
  case R_TYPE_logical:
    out = rray_split_lgl(x, axes, error_call);
    break;
  case R_TYPE_integer:
    out = rray_split_int(x, axes, error_call);
    break;
  case R_TYPE_double:
    out = rray_split_dbl(x, axes, error_call);
    break;
  case R_TYPE_complex:
    out = rray_split_cpl(x, axes, error_call);
    break;
  case R_TYPE_raw:
    out = rray_split_raw(x, axes, error_call);
    break;
  case R_TYPE_character:
    out = rray_split_chr(x, axes, error_call);
    break;
  case R_TYPE_list:
    out = rray_split_list(x, axes, error_call);
    break;
  default:
    r_stop_unreachable();
  }

  FREE(1);
  return out;
}
