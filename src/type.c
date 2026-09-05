#include "type.h"

#include <stdio.h>
#include <string.h>

enum rray_type rray_typeof(r_obj* x) {
  switch (r_typeof(x)) {
  case R_TYPE_logical:
    return RRAY_TYPE_logical;
  case R_TYPE_integer:
    return RRAY_TYPE_integer;
  case R_TYPE_double:
    return RRAY_TYPE_double;
  case R_TYPE_complex:
    return RRAY_TYPE_complex;
  case R_TYPE_character:
    return RRAY_TYPE_character;
  case R_TYPE_raw:
    return RRAY_TYPE_raw;
  case R_TYPE_list:
    return RRAY_TYPE_list;
  default:
    return RRAY_TYPE_scalar;
  }
}

const char* rray_type_as_c_string(enum rray_type type) {
  switch (type) {
  case RRAY_TYPE_logical:
    return "logical";
  case RRAY_TYPE_integer:
    return "integer";
  case RRAY_TYPE_double:
    return "double";
  case RRAY_TYPE_complex:
    return "complex";
  case RRAY_TYPE_character:
    return "character";
  case RRAY_TYPE_raw:
    return "raw";
  case RRAY_TYPE_list:
    return "list";
  case RRAY_TYPE_scalar:
    return "scalar";
  }

  r_stop_unreachable();
}

const char* rray_arg_type_format(struct rray_arg* arg, enum rray_type type) {
  const char* type_str = rray_type_as_c_string(type);

  if (rray_arg_is_empty(arg)) {
    const size_t size = strlen(type_str) + 3;
    char* out = R_alloc(size, sizeof(char));
    snprintf(out, size, "<%s>", type_str);
    return out;
  }

  const char* arg_str = rray_arg_format(arg);

  const size_t size = strlen(arg_str) + strlen(type_str) + 4;
  char* out = R_alloc(size, sizeof(char));
  snprintf(out, size, "%s <%s>", arg_str, type_str);
  return out;
}
