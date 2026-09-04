#ifndef RRAY_TYPE_H
#define RRAY_TYPE_H

#include "rlang.h"

enum rray_type {
  RRAY_TYPE_logical,
  RRAY_TYPE_integer,
  RRAY_TYPE_double,
  RRAY_TYPE_complex,
  RRAY_TYPE_character,
  RRAY_TYPE_raw,
  RRAY_TYPE_list
};

enum rray_type rray_typeof(r_obj* x);

enum r_type rray_type_to_r_type(enum rray_type type);

const char* rray_type_as_c_string(enum rray_type type);

#endif
