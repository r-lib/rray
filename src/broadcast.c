#include "broadcast.h"

#include "types.h"
#include "utils.h"

r_obj* ffi_rray_broadcast(r_obj* x, r_obj* dimension_sizes, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  return rray_broadcast(x, dimension_sizes, error_call);
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

r_obj* rray_broadcast(
  r_obj* x,
  r_obj* dimension_sizes,
  struct r_lazy error_call
) {
  check_array(x, error_call);

  switch (r_typeof(x)) {
    case R_TYPE_logical:
      return rray_broadcast_lgl(x, dimension_sizes, error_call);
    case R_TYPE_integer:
      return rray_broadcast_int(x, dimension_sizes, error_call);
    case R_TYPE_double:
      return rray_broadcast_dbl(x, dimension_sizes, error_call);
    case R_TYPE_complex:
      return rray_broadcast_cpl(x, dimension_sizes, error_call);
    case R_TYPE_raw:
      return rray_broadcast_raw(x, dimension_sizes, error_call);
    case R_TYPE_character:
      return rray_broadcast_chr(x, dimension_sizes, error_call);
    case R_TYPE_list:
      return rray_broadcast_list(x, dimension_sizes, error_call);
    default:
      r_stop_unreachable();
  }
}

void check_broadcastable(
  const int* v_x_dimension_sizes,
  r_ssize x_dimensionality,
  const int* v_dimension_sizes,
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
    const int x_dimension_size = v_x_dimension_sizes[i];
    const int dimension_size = v_dimension_sizes[i];

    if (x_dimension_size == dimension_size || x_dimension_size == 1) {
      continue;
    }

    r_abort_lazy_call(
      error_call,
      "Can't broadcast dimension %td from size %d to %d.",
      (ptrdiff_t) (i + 1),
      x_dimension_size,
      dimension_size
    );
  }
}
