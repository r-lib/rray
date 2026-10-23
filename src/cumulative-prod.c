#include "cumulative-prod.h"

#include "cumulative.h"
#include "one-multiply.h"
#include "type.h"
#include "utils.h"

#include "decl/cumulative-prod-decl.h"

r_obj* ffi_rray_cumulative_prod(
  r_obj* ffi_x,
  r_obj* ffi_axis,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_cumulative_prod(ffi_x, ffi_axis, rray_args.x, error_call);
}

r_obj* rray_cumulative_prod(
  r_obj* x,
  r_obj* axis,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_cumulative(x, axis, rray_cumulative_prod_switch, arg, error_call);
}

static rray_cumulative_fn rray_cumulative_prod_switch(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  switch (rray_typeof(x)) {
  case RRAY_TYPE_logical:
    return rray_cumulative_prod_lgl;
  case RRAY_TYPE_integer:
    return rray_cumulative_prod_int;
  case RRAY_TYPE_double:
    return rray_cumulative_prod_dbl;
  case RRAY_TYPE_complex:
    return rray_cumulative_prod_cpl;

  case RRAY_TYPE_character:
  case RRAY_TYPE_raw:
  case RRAY_TYPE_list:
    stop_unsupported_cumulative("product", x, arg, error_call);

  case RRAY_TYPE_scalar:
    stop_scalar_input(x, arg, error_call);
  }

  r_stop_unreachable();
}

static r_obj* rray_cumulative_prod_lgl(
  r_obj* x,
  r_ssize state_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_state_broadcast_strides,
  struct r_lazy error_call
) {
  RRAY_CUMULATIVE(
    int,
    r_lgl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    1.0,
    rray_prod_lgl_one,
    RRAY_CUMULATIVE_NO_ARGS
  );
}

static r_obj* rray_cumulative_prod_int(
  r_obj* x,
  r_ssize state_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_state_broadcast_strides,
  struct r_lazy error_call
) {
  RRAY_CUMULATIVE(
    int,
    r_int_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    1.0,
    rray_prod_int_one,
    RRAY_CUMULATIVE_NO_ARGS
  );
}

static r_obj* rray_cumulative_prod_dbl(
  r_obj* x,
  r_ssize state_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_state_broadcast_strides,
  struct r_lazy error_call
) {
  RRAY_CUMULATIVE(
    double,
    r_dbl_cbegin,
    R_TYPE_double,
    double,
    r_dbl_begin,
    1.0,
    rray_prod_dbl_one,
    RRAY_CUMULATIVE_NO_ARGS
  );
}

static r_obj* rray_cumulative_prod_cpl(
  r_obj* x,
  r_ssize state_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_state_broadcast_strides,
  struct r_lazy error_call
) {
  RRAY_CUMULATIVE(
    r_complex,
    r_cpl_cbegin,
    R_TYPE_complex,
    r_complex,
    r_cpl_begin,
    ((r_complex){.r = 1, .i = 0}),
    rray_prod_cpl_one,
    RRAY_CUMULATIVE_NO_ARGS
  );
}
