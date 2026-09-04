#include "ptype.h"

#include "syms.h"
#include "typeof2.h"
#include "utils.h"

#include "decl/ptype-decl.h"

r_obj* ffi_rray_ptype2(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = rray_syms.call, .env = ffi_frame};

  struct r_lazy x_arg_lazy = {.x = rray_syms.x_arg, .env = ffi_frame};
  struct rray_arg x_arg = new_lazy_arg(&x_arg_lazy);

  struct r_lazy y_arg_lazy = {.x = rray_syms.y_arg, .env = ffi_frame};
  struct rray_arg y_arg = new_lazy_arg(&y_arg_lazy);

  return rray_ptype2(ffi_x, ffi_y, &x_arg, &y_arg, error_call);
}

r_obj* rray_ptype2(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  check_unclassed(x, x_arg, error_call);
  check_unclassed(y, y_arg, error_call);

  const enum rray_type x_type = arg_as_type(x, x_arg, error_call);
  const enum rray_type y_type = arg_as_type(y, y_arg, error_call);

  switch (rray_typeof2(x_type, y_type)) {
  case RRAY_TYPE2_logical_logical:
    return r_globals.empty_lgl;

  case RRAY_TYPE2_logical_integer:
  case RRAY_TYPE2_integer_integer:
    return r_globals.empty_int;

  case RRAY_TYPE2_logical_double:
  case RRAY_TYPE2_integer_double:
  case RRAY_TYPE2_double_double:
    return r_globals.empty_dbl;

  case RRAY_TYPE2_logical_complex:
  case RRAY_TYPE2_integer_complex:
  case RRAY_TYPE2_double_complex:
  case RRAY_TYPE2_complex_complex:
    return r_globals.empty_cpl;

  case RRAY_TYPE2_character_character:
    return r_globals.empty_chr;

  case RRAY_TYPE2_raw_raw:
    return r_globals.empty_raw;

  case RRAY_TYPE2_list_list:
    return r_globals.empty_list;

  case RRAY_TYPE2_logical_character:
  case RRAY_TYPE2_logical_raw:
  case RRAY_TYPE2_logical_list:
  case RRAY_TYPE2_integer_character:
  case RRAY_TYPE2_integer_raw:
  case RRAY_TYPE2_integer_list:
  case RRAY_TYPE2_double_character:
  case RRAY_TYPE2_double_raw:
  case RRAY_TYPE2_double_list:
  case RRAY_TYPE2_complex_character:
  case RRAY_TYPE2_complex_raw:
  case RRAY_TYPE2_complex_list:
  case RRAY_TYPE2_character_raw:
  case RRAY_TYPE2_character_list:
  case RRAY_TYPE2_raw_list:
    stop_incompatible_ptype(x_type, y_type, x_arg, y_arg, error_call);
  }

  r_stop_unreachable();
}

r_obj* rray_ptype_from_type(enum rray_type type) {
  switch (type) {
  case RRAY_TYPE_logical:
    return r_globals.empty_lgl;
  case RRAY_TYPE_integer:
    return r_globals.empty_int;
  case RRAY_TYPE_double:
    return r_globals.empty_dbl;
  case RRAY_TYPE_complex:
    return r_globals.empty_cpl;
  case RRAY_TYPE_character:
    return r_globals.empty_chr;
  case RRAY_TYPE_raw:
    return r_globals.empty_raw;
  case RRAY_TYPE_list:
    return r_globals.empty_list;
  }

  r_stop_unreachable();
}

static r_no_return void stop_incompatible_ptype(
  enum rray_type x,
  enum rray_type y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  r_abort_lazy_call(
    error_call,
    "Can't combine %s and %s.",
    rray_arg_type_format(x_arg, x),
    rray_arg_type_format(y_arg, y)
  );
}
