#ifndef RRAY_TYPEOF2_H
#define RRAY_TYPEOF2_H

#include "rlang.h"

#include "type.h"

enum rray_type2 {
  RRAY_TYPE2_logical_logical,
  RRAY_TYPE2_logical_integer,
  RRAY_TYPE2_logical_double,
  RRAY_TYPE2_logical_complex,
  RRAY_TYPE2_logical_character,
  RRAY_TYPE2_logical_raw,
  RRAY_TYPE2_logical_list,

  RRAY_TYPE2_integer_integer,
  RRAY_TYPE2_integer_double,
  RRAY_TYPE2_integer_complex,
  RRAY_TYPE2_integer_character,
  RRAY_TYPE2_integer_raw,
  RRAY_TYPE2_integer_list,

  RRAY_TYPE2_double_double,
  RRAY_TYPE2_double_complex,
  RRAY_TYPE2_double_character,
  RRAY_TYPE2_double_raw,
  RRAY_TYPE2_double_list,

  RRAY_TYPE2_complex_complex,
  RRAY_TYPE2_complex_character,
  RRAY_TYPE2_complex_raw,
  RRAY_TYPE2_complex_list,

  RRAY_TYPE2_character_character,
  RRAY_TYPE2_character_raw,
  RRAY_TYPE2_character_list,

  RRAY_TYPE2_raw_raw,
  RRAY_TYPE2_raw_list,

  RRAY_TYPE2_list_list
};

enum rray_type2 rray_typeof2(enum rray_type x, enum rray_type y);

#endif
