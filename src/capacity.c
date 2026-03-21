#include "capacity.h"

#include "utils.h"

r_obj* ffi_rray_capacity(r_obj* x) {
  return r_dbl((double) rray_capacity(x));
}

r_ssize rray_capacity(r_obj* x) {
  check_array(x);
  return r_length(x);
}
