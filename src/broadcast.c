#include "broadcast.h"

#include "types.h"
#include "utils.h"

r_obj* ffi_rray_broadcast(r_obj* x, r_obj* dimensions, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  return rray_broadcast(x, dimensions, error_call);
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
  r_ssize x_dimensionality,
  const int* v_dimensions,
  r_ssize dimensionality,
  struct r_lazy error_call
) {
  if (x_dimensionality > dimensionality) {
    r_abort_lazy_call(
      error_call,
      "Can't broadcast from dimensionality %td to %td. "
      "Can't decrease dimensionality.",
      (ptrdiff_t) x_dimensionality,
      (ptrdiff_t) dimensionality
    );
  }

  for (r_ssize i = 0; i < x_dimensionality; ++i) {
    const int x_dimension = v_x_dimensions[i];
    const int dimension = v_dimensions[i];

    if (x_dimension == dimension || x_dimension == 1) {
      continue;
    }

    r_abort_lazy_call(
      error_call,
      "Can't broadcast dimension %td from size %d to %d.",
      (ptrdiff_t) (i + 1),
      x_dimension,
      dimension
    );
  }
}
