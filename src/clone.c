#include "clone.h"

// Shallow duplicates only the data, not the attributes
r_obj* r_clone_data(r_obj* x) {
  switch (r_typeof(x)) {
  case R_TYPE_logical: {
    const int* v_x = r_lgl_cbegin(x);
    const r_ssize size = r_length(x);
    r_obj* out = KEEP(r_alloc_logical(size));
    int* v_out = r_lgl_begin(out);
    r_memcpy(v_out, v_x, sizeof(int) * size);
    FREE(1);
    return out;
  }
  case R_TYPE_integer: {
    const int* v_x = r_int_cbegin(x);
    const r_ssize size = r_length(x);
    r_obj* out = KEEP(r_alloc_integer(size));
    int* v_out = r_int_begin(out);
    r_memcpy(v_out, v_x, sizeof(int) * size);
    FREE(1);
    return out;
  }
  case R_TYPE_double: {
    const double* v_x = r_dbl_cbegin(x);
    const r_ssize size = r_length(x);
    r_obj* out = KEEP(r_alloc_double(size));
    double* v_out = r_dbl_begin(out);
    r_memcpy(v_out, v_x, sizeof(double) * size);
    FREE(1);
    return out;
  }
  case R_TYPE_complex: {
    const r_complex* v_x = r_cpl_cbegin(x);
    const r_ssize size = r_length(x);
    r_obj* out = KEEP(r_alloc_complex(size));
    r_complex* v_out = r_cpl_begin(out);
    r_memcpy(v_out, v_x, sizeof(r_complex) * size);
    FREE(1);
    return out;
  }
  case R_TYPE_raw: {
    const Rbyte* v_x = r_raw_cbegin(x);
    const r_ssize size = r_length(x);
    r_obj* out = KEEP(r_alloc_raw(size));
    Rbyte* v_out = r_raw_begin(out);
    r_memcpy(v_out, v_x, sizeof(Rbyte) * size);
    FREE(1);
    return out;
  }
  case R_TYPE_character: {
    r_obj* const* v_x = r_chr_cbegin(x);
    const r_ssize size = r_length(x);
    r_obj* out = KEEP(r_alloc_character(size));
    for (r_ssize i = 0; i < size; ++i) {
      r_chr_poke(out, i, v_x[i]);
    }
    FREE(1);
    return out;
  }
  case R_TYPE_list: {
    r_obj* const* v_x = r_list_cbegin(x);
    const r_ssize size = r_length(x);
    r_obj* out = KEEP(r_alloc_list(size));
    for (r_ssize i = 0; i < size; ++i) {
      r_list_poke(out, i, v_x[i]);
    }
    FREE(1);
    return out;
  }
  default: {
    r_stop_unimplemented_type(r_typeof(x));
  }
  }
}
