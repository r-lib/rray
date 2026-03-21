#include "capacity.h"

#include "utils.h"

r_obj* ffi_rray_capacity(r_obj* x, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  return r_dbl((double) rray_capacity(x, error_call));
}

r_ssize rray_capacity(r_obj* x, struct r_lazy error_call) {
  check_array(x, error_call);
  return r_length(x);
}
