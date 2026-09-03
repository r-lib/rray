#include "ptype.h"

#include "utils.h"

#include "decl/ptype-decl.h"

r_obj* ffi_rray_ptype2(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};

  check_unclassed(ffi_x, rray_args.x, error_call);
  check_unclassed(ffi_y, rray_args.y, error_call);

  const enum r_type x = arg_as_ptype(ffi_x, rray_args.x, error_call);
  const enum r_type y = arg_as_ptype(ffi_y, rray_args.y, error_call);

  const enum r_type out =
    rray_ptype2(x, y, rray_args.x, rray_args.y, error_call);

  return r_alloc_vector(out, 0);
}

enum r_type rray_ptype2(
  enum r_type x,
  enum r_type y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  switch (rray_typeof2(x, y)) {
  case RRAY_TYPE2_logical_logical:
    return R_TYPE_logical;

  case RRAY_TYPE2_logical_integer:
  case RRAY_TYPE2_integer_integer:
    return R_TYPE_integer;

  case RRAY_TYPE2_logical_double:
  case RRAY_TYPE2_integer_double:
  case RRAY_TYPE2_double_double:
    return R_TYPE_double;

  case RRAY_TYPE2_logical_complex:
  case RRAY_TYPE2_integer_complex:
  case RRAY_TYPE2_double_complex:
  case RRAY_TYPE2_complex_complex:
    return R_TYPE_complex;

  case RRAY_TYPE2_character_character:
    return R_TYPE_character;

  case RRAY_TYPE2_raw_raw:
    return R_TYPE_raw;

  case RRAY_TYPE2_list_list:
    return R_TYPE_list;

  default:
    stop_incompatible_ptype(x, y, x_arg, y_arg, error_call);
  }
}

enum rray_type2 rray_typeof2(enum r_type x, enum r_type y) {
  switch (x) {
  case R_TYPE_logical:
    switch (y) {
    case R_TYPE_logical:
      return RRAY_TYPE2_logical_logical;
    case R_TYPE_integer:
      return RRAY_TYPE2_logical_integer;
    case R_TYPE_double:
      return RRAY_TYPE2_logical_double;
    case R_TYPE_complex:
      return RRAY_TYPE2_logical_complex;
    case R_TYPE_character:
      return RRAY_TYPE2_logical_character;
    case R_TYPE_raw:
      return RRAY_TYPE2_logical_raw;
    case R_TYPE_list:
      return RRAY_TYPE2_logical_list;
    default:
      break;
    }
    break;
  case R_TYPE_integer:
    switch (y) {
    case R_TYPE_logical:
      return RRAY_TYPE2_logical_integer;
    case R_TYPE_integer:
      return RRAY_TYPE2_integer_integer;
    case R_TYPE_double:
      return RRAY_TYPE2_integer_double;
    case R_TYPE_complex:
      return RRAY_TYPE2_integer_complex;
    case R_TYPE_character:
      return RRAY_TYPE2_integer_character;
    case R_TYPE_raw:
      return RRAY_TYPE2_integer_raw;
    case R_TYPE_list:
      return RRAY_TYPE2_integer_list;
    default:
      break;
    }
    break;
  case R_TYPE_double:
    switch (y) {
    case R_TYPE_logical:
      return RRAY_TYPE2_logical_double;
    case R_TYPE_integer:
      return RRAY_TYPE2_integer_double;
    case R_TYPE_double:
      return RRAY_TYPE2_double_double;
    case R_TYPE_complex:
      return RRAY_TYPE2_double_complex;
    case R_TYPE_character:
      return RRAY_TYPE2_double_character;
    case R_TYPE_raw:
      return RRAY_TYPE2_double_raw;
    case R_TYPE_list:
      return RRAY_TYPE2_double_list;
    default:
      break;
    }
    break;
  case R_TYPE_complex:
    switch (y) {
    case R_TYPE_logical:
      return RRAY_TYPE2_logical_complex;
    case R_TYPE_integer:
      return RRAY_TYPE2_integer_complex;
    case R_TYPE_double:
      return RRAY_TYPE2_double_complex;
    case R_TYPE_complex:
      return RRAY_TYPE2_complex_complex;
    case R_TYPE_character:
      return RRAY_TYPE2_complex_character;
    case R_TYPE_raw:
      return RRAY_TYPE2_complex_raw;
    case R_TYPE_list:
      return RRAY_TYPE2_complex_list;
    default:
      break;
    }
    break;
  case R_TYPE_character:
    switch (y) {
    case R_TYPE_logical:
      return RRAY_TYPE2_logical_character;
    case R_TYPE_integer:
      return RRAY_TYPE2_integer_character;
    case R_TYPE_double:
      return RRAY_TYPE2_double_character;
    case R_TYPE_complex:
      return RRAY_TYPE2_complex_character;
    case R_TYPE_character:
      return RRAY_TYPE2_character_character;
    case R_TYPE_raw:
      return RRAY_TYPE2_character_raw;
    case R_TYPE_list:
      return RRAY_TYPE2_character_list;
    default:
      break;
    }
    break;
  case R_TYPE_raw:
    switch (y) {
    case R_TYPE_logical:
      return RRAY_TYPE2_logical_raw;
    case R_TYPE_integer:
      return RRAY_TYPE2_integer_raw;
    case R_TYPE_double:
      return RRAY_TYPE2_double_raw;
    case R_TYPE_complex:
      return RRAY_TYPE2_complex_raw;
    case R_TYPE_character:
      return RRAY_TYPE2_character_raw;
    case R_TYPE_raw:
      return RRAY_TYPE2_raw_raw;
    case R_TYPE_list:
      return RRAY_TYPE2_raw_list;
    default:
      break;
    }
    break;
  case R_TYPE_list:
    switch (y) {
    case R_TYPE_logical:
      return RRAY_TYPE2_logical_list;
    case R_TYPE_integer:
      return RRAY_TYPE2_integer_list;
    case R_TYPE_double:
      return RRAY_TYPE2_double_list;
    case R_TYPE_complex:
      return RRAY_TYPE2_complex_list;
    case R_TYPE_character:
      return RRAY_TYPE2_character_list;
    case R_TYPE_raw:
      return RRAY_TYPE2_raw_list;
    case R_TYPE_list:
      return RRAY_TYPE2_list_list;
    default:
      break;
    }
    break;
  default:
    break;
  }

  r_stop_unreachable();
}

r_obj* ffi_rray_ptype_common(r_obj* ffi_xs, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const enum r_type out = rray_ptype_common(ffi_xs, error_call);
  return r_alloc_vector(out, 0);
}

enum r_type rray_ptype_common(r_obj* xs, struct r_lazy error_call) {
  const r_ssize n = r_length(xs);

  if (n == 0) {
    r_abort_lazy_call(error_call, "Must supply at least one array to `...`.");
  }

  r_obj* const* v_xs = r_list_cbegin(xs);
  r_obj* xs_names = KEEP(r_names(xs));

  r_ssize x_i = 0;
  struct rray_arg* p_x_arg = new_subscript_arg(NULL, xs_names, n, &x_i);
  KEEP(p_x_arg->shelter);

  r_ssize out_i = 0;
  struct rray_arg* p_out_arg = new_subscript_arg(NULL, xs_names, n, &out_i);
  KEEP(p_out_arg->shelter);

  check_unclassed(v_xs[0], p_x_arg, error_call);
  enum r_type out = arg_as_ptype(v_xs[0], p_x_arg, error_call);

  for (x_i = 1; x_i < n; ++x_i) {
    r_obj* x = v_xs[x_i];

    check_unclassed(x, p_x_arg, error_call);
    const enum r_type type = arg_as_ptype(x, p_x_arg, error_call);

    const enum r_type common =
      rray_ptype2(out, type, p_out_arg, p_x_arg, error_call);

    if (common != out) {
      out = common;
      out_i = x_i;
    }
  }

  FREE(3);
  return out;
}

static r_no_return void stop_incompatible_ptype(
  enum r_type x,
  enum r_type y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  r_abort_lazy_call(
    error_call,
    "Can't combine %s <%s> and %s <%s>.",
    rray_arg_format(x_arg),
    r_type_as_c_string(x),
    rray_arg_format(y_arg),
    r_type_as_c_string(y)
  );
}
