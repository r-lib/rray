#include "typeof2.h"

enum rray_type2 rray_typeof2(enum rray_type x, enum rray_type y) {
  switch (x) {
  case RRAY_TYPE_logical:
    switch (y) {
    case RRAY_TYPE_logical:
      return RRAY_TYPE2_logical_logical;
    case RRAY_TYPE_integer:
      return RRAY_TYPE2_logical_integer;
    case RRAY_TYPE_double:
      return RRAY_TYPE2_logical_double;
    case RRAY_TYPE_complex:
      return RRAY_TYPE2_logical_complex;
    case RRAY_TYPE_character:
      return RRAY_TYPE2_logical_character;
    case RRAY_TYPE_raw:
      return RRAY_TYPE2_logical_raw;
    case RRAY_TYPE_list:
      return RRAY_TYPE2_logical_list;
    }
    break;
  case RRAY_TYPE_integer:
    switch (y) {
    case RRAY_TYPE_logical:
      return RRAY_TYPE2_logical_integer;
    case RRAY_TYPE_integer:
      return RRAY_TYPE2_integer_integer;
    case RRAY_TYPE_double:
      return RRAY_TYPE2_integer_double;
    case RRAY_TYPE_complex:
      return RRAY_TYPE2_integer_complex;
    case RRAY_TYPE_character:
      return RRAY_TYPE2_integer_character;
    case RRAY_TYPE_raw:
      return RRAY_TYPE2_integer_raw;
    case RRAY_TYPE_list:
      return RRAY_TYPE2_integer_list;
    }
    break;
  case RRAY_TYPE_double:
    switch (y) {
    case RRAY_TYPE_logical:
      return RRAY_TYPE2_logical_double;
    case RRAY_TYPE_integer:
      return RRAY_TYPE2_integer_double;
    case RRAY_TYPE_double:
      return RRAY_TYPE2_double_double;
    case RRAY_TYPE_complex:
      return RRAY_TYPE2_double_complex;
    case RRAY_TYPE_character:
      return RRAY_TYPE2_double_character;
    case RRAY_TYPE_raw:
      return RRAY_TYPE2_double_raw;
    case RRAY_TYPE_list:
      return RRAY_TYPE2_double_list;
    }
    break;
  case RRAY_TYPE_complex:
    switch (y) {
    case RRAY_TYPE_logical:
      return RRAY_TYPE2_logical_complex;
    case RRAY_TYPE_integer:
      return RRAY_TYPE2_integer_complex;
    case RRAY_TYPE_double:
      return RRAY_TYPE2_double_complex;
    case RRAY_TYPE_complex:
      return RRAY_TYPE2_complex_complex;
    case RRAY_TYPE_character:
      return RRAY_TYPE2_complex_character;
    case RRAY_TYPE_raw:
      return RRAY_TYPE2_complex_raw;
    case RRAY_TYPE_list:
      return RRAY_TYPE2_complex_list;
    }
    break;
  case RRAY_TYPE_character:
    switch (y) {
    case RRAY_TYPE_logical:
      return RRAY_TYPE2_logical_character;
    case RRAY_TYPE_integer:
      return RRAY_TYPE2_integer_character;
    case RRAY_TYPE_double:
      return RRAY_TYPE2_double_character;
    case RRAY_TYPE_complex:
      return RRAY_TYPE2_complex_character;
    case RRAY_TYPE_character:
      return RRAY_TYPE2_character_character;
    case RRAY_TYPE_raw:
      return RRAY_TYPE2_character_raw;
    case RRAY_TYPE_list:
      return RRAY_TYPE2_character_list;
    }
    break;
  case RRAY_TYPE_raw:
    switch (y) {
    case RRAY_TYPE_logical:
      return RRAY_TYPE2_logical_raw;
    case RRAY_TYPE_integer:
      return RRAY_TYPE2_integer_raw;
    case RRAY_TYPE_double:
      return RRAY_TYPE2_double_raw;
    case RRAY_TYPE_complex:
      return RRAY_TYPE2_complex_raw;
    case RRAY_TYPE_character:
      return RRAY_TYPE2_character_raw;
    case RRAY_TYPE_raw:
      return RRAY_TYPE2_raw_raw;
    case RRAY_TYPE_list:
      return RRAY_TYPE2_raw_list;
    }
    break;
  case RRAY_TYPE_list:
    switch (y) {
    case RRAY_TYPE_logical:
      return RRAY_TYPE2_logical_list;
    case RRAY_TYPE_integer:
      return RRAY_TYPE2_integer_list;
    case RRAY_TYPE_double:
      return RRAY_TYPE2_double_list;
    case RRAY_TYPE_complex:
      return RRAY_TYPE2_complex_list;
    case RRAY_TYPE_character:
      return RRAY_TYPE2_character_list;
    case RRAY_TYPE_raw:
      return RRAY_TYPE2_raw_list;
    case RRAY_TYPE_list:
      return RRAY_TYPE2_list_list;
    }
    break;
  }

  r_stop_unreachable();
}
