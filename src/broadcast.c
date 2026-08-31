#include "broadcast.h"

#include "types.h"
#include "utils.h"

r_obj* ffi_rray_broadcast(
  r_obj* ffi_x,
  r_obj* ffi_dimensions,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_broadcast(ffi_x, ffi_dimensions, error_call);
}

#define RRAY_TYPE RRAY_TYPE_LOGICAL
#include "broadcast-template.h"

#define RRAY_TYPE RRAY_TYPE_INTEGER
#include "broadcast-template.h"

#define RRAY_TYPE RRAY_TYPE_DOUBLE
#include "broadcast-template.h"

#define RRAY_TYPE RRAY_TYPE_COMPLEX
#include "broadcast-template.h"

#define RRAY_TYPE RRAY_TYPE_RAW
#include "broadcast-template.h"

#define RRAY_TYPE RRAY_TYPE_CHARACTER
#include "broadcast-template.h"

#define RRAY_TYPE RRAY_TYPE_LIST
#include "broadcast-template.h"

r_obj* rray_broadcast(r_obj* x, r_obj* dimensions, struct r_lazy error_call) {
  check_unclassed(x, "x", error_call);
  x = KEEP(arg_as_array(x, "x", error_call));

  r_obj* out;

  switch (r_typeof(x)) {
  case R_TYPE_logical:
    out = rray_broadcast_lgl(x, dimensions, error_call);
    break;
  case R_TYPE_integer:
    out = rray_broadcast_int(x, dimensions, error_call);
    break;
  case R_TYPE_double:
    out = rray_broadcast_dbl(x, dimensions, error_call);
    break;
  case R_TYPE_complex:
    out = rray_broadcast_cpl(x, dimensions, error_call);
    break;
  case R_TYPE_raw:
    out = rray_broadcast_raw(x, dimensions, error_call);
    break;
  case R_TYPE_character:
    out = rray_broadcast_chr(x, dimensions, error_call);
    break;
  case R_TYPE_list:
    out = rray_broadcast_list(x, dimensions, error_call);
    break;
  default:
    r_stop_unreachable();
  }

  FREE(1);
  return out;
}

void check_broadcastable(
  const int* v_x_dimensions,
  int x_dimensionality,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
) {
  if (x_dimensionality > dimensionality) {
    r_abort_lazy_call(
      error_call,
      "Can't broadcast from dimensionality %d to %d. "
      "Can't decrease dimensionality.",
      x_dimensionality,
      dimensionality
    );
  }

  for (int i = 0; i < x_dimensionality; ++i) {
    const int x_dimension = v_x_dimensions[i];
    const int dimension = v_dimensions[i];

    if (x_dimension == dimension || x_dimension == 1) {
      continue;
    }

    r_abort_lazy_call(
      error_call,
      "Can't broadcast axis %d from dimension %d to %d.",
      i + 1,
      x_dimension,
      dimension
    );
  }
}
