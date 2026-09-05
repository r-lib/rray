#include "typeof2.h"

enum rray_type2 rray_typeof2(
  enum rray_type x,
  enum rray_type y,
  enum rray_side* side
) {
  switch (x) {
  case RRAY_TYPE_logical:
    switch (y) {
    case RRAY_TYPE_logical:
      *side = RRAY_SIDE_both;
      return RRAY_TYPE2_logical_logical;
    case RRAY_TYPE_integer:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_logical_integer;
    case RRAY_TYPE_double:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_logical_double;
    case RRAY_TYPE_complex:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_logical_complex;
    case RRAY_TYPE_character:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_logical_character;
    case RRAY_TYPE_raw:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_logical_raw;
    case RRAY_TYPE_list:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_logical_list;
    case RRAY_TYPE_scalar:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_logical_scalar;
    }
    break;
  case RRAY_TYPE_integer:
    switch (y) {
    case RRAY_TYPE_logical:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_logical_integer;
    case RRAY_TYPE_integer:
      *side = RRAY_SIDE_both;
      return RRAY_TYPE2_integer_integer;
    case RRAY_TYPE_double:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_integer_double;
    case RRAY_TYPE_complex:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_integer_complex;
    case RRAY_TYPE_character:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_integer_character;
    case RRAY_TYPE_raw:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_integer_raw;
    case RRAY_TYPE_list:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_integer_list;
    case RRAY_TYPE_scalar:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_integer_scalar;
    }
    break;
  case RRAY_TYPE_double:
    switch (y) {
    case RRAY_TYPE_logical:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_logical_double;
    case RRAY_TYPE_integer:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_integer_double;
    case RRAY_TYPE_double:
      *side = RRAY_SIDE_both;
      return RRAY_TYPE2_double_double;
    case RRAY_TYPE_complex:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_double_complex;
    case RRAY_TYPE_character:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_double_character;
    case RRAY_TYPE_raw:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_double_raw;
    case RRAY_TYPE_list:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_double_list;
    case RRAY_TYPE_scalar:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_double_scalar;
    }
    break;
  case RRAY_TYPE_complex:
    switch (y) {
    case RRAY_TYPE_logical:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_logical_complex;
    case RRAY_TYPE_integer:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_integer_complex;
    case RRAY_TYPE_double:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_double_complex;
    case RRAY_TYPE_complex:
      *side = RRAY_SIDE_both;
      return RRAY_TYPE2_complex_complex;
    case RRAY_TYPE_character:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_complex_character;
    case RRAY_TYPE_raw:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_complex_raw;
    case RRAY_TYPE_list:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_complex_list;
    case RRAY_TYPE_scalar:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_complex_scalar;
    }
    break;
  case RRAY_TYPE_character:
    switch (y) {
    case RRAY_TYPE_logical:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_logical_character;
    case RRAY_TYPE_integer:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_integer_character;
    case RRAY_TYPE_double:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_double_character;
    case RRAY_TYPE_complex:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_complex_character;
    case RRAY_TYPE_character:
      *side = RRAY_SIDE_both;
      return RRAY_TYPE2_character_character;
    case RRAY_TYPE_raw:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_character_raw;
    case RRAY_TYPE_list:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_character_list;
    case RRAY_TYPE_scalar:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_character_scalar;
    }
    break;
  case RRAY_TYPE_raw:
    switch (y) {
    case RRAY_TYPE_logical:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_logical_raw;
    case RRAY_TYPE_integer:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_integer_raw;
    case RRAY_TYPE_double:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_double_raw;
    case RRAY_TYPE_complex:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_complex_raw;
    case RRAY_TYPE_character:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_character_raw;
    case RRAY_TYPE_raw:
      *side = RRAY_SIDE_both;
      return RRAY_TYPE2_raw_raw;
    case RRAY_TYPE_list:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_raw_list;
    case RRAY_TYPE_scalar:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_raw_scalar;
    }
    break;
  case RRAY_TYPE_list:
    switch (y) {
    case RRAY_TYPE_logical:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_logical_list;
    case RRAY_TYPE_integer:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_integer_list;
    case RRAY_TYPE_double:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_double_list;
    case RRAY_TYPE_complex:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_complex_list;
    case RRAY_TYPE_character:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_character_list;
    case RRAY_TYPE_raw:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_raw_list;
    case RRAY_TYPE_list:
      *side = RRAY_SIDE_both;
      return RRAY_TYPE2_list_list;
    case RRAY_TYPE_scalar:
      *side = RRAY_SIDE_right;
      return RRAY_TYPE2_list_scalar;
    }
    break;
  case RRAY_TYPE_scalar:
    switch (y) {
    case RRAY_TYPE_logical:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_logical_scalar;
    case RRAY_TYPE_integer:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_integer_scalar;
    case RRAY_TYPE_double:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_double_scalar;
    case RRAY_TYPE_complex:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_complex_scalar;
    case RRAY_TYPE_character:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_character_scalar;
    case RRAY_TYPE_raw:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_raw_scalar;
    case RRAY_TYPE_list:
      *side = RRAY_SIDE_left;
      return RRAY_TYPE2_list_scalar;
    case RRAY_TYPE_scalar:
      *side = RRAY_SIDE_both;
      return RRAY_TYPE2_scalar_scalar;
    }
    break;
  }

  r_stop_unreachable();
}
