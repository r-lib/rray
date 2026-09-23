#include "size.h"

#include <stdio.h>

#include "dimensionality.h"
#include "utils.h"

#include "decl/size-decl.h"

r_obj* ffi_rray_size(r_obj* ffi_x, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return r_dbl((double) rray_size(ffi_x, rray_args.x, error_call));
}

r_ssize rray_size(r_obj* x, struct rray_arg* arg, struct r_lazy error_call) {
  check_unclassed(x, arg, error_call);
  x = arg_as_array(x, arg, error_call);
  return r_length(x);
}

r_ssize rray_size_from_dimensions(const int* v_dimensions, int dimensionality) {
  r_ssize out = 1;

  for (int i = 0; i < dimensionality; ++i) {
    out *= v_dimensions[i];
  }

  return out;
}

// Sometimes dimensions are built from pieces rather than being pulled from an
// existing array, and must be checked for overflow
r_ssize rray_size_from_dimensions_checked(
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
) {
  r_ssize out = 1;

  for (int i = 0; i < dimensionality; ++i) {
    const int dimension = v_dimensions[i];

    if (dimension != 0 && out > R_SSIZE_MAX / dimension) {
      stop_size_too_large(v_dimensions, dimensionality, error_call);
    }

    out *= dimension;
  }

  return out;
}

void check_size_from_dimensions(r_obj* dimensions, struct r_lazy error_call) {
  const int* v_dimensions = r_int_cbegin(dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(dimensions);
  rray_size_from_dimensions_checked(v_dimensions, dimensionality, error_call);
}

static r_no_return void stop_size_too_large(
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
) {
  double size = 1;

  for (int i = 0; i < dimensionality; ++i) {
    size *= v_dimensions[i];
  }

  r_abort_lazy_call(
    error_call,
    "Size (%g) computed from dimensions `%s` is too large.",
    size,
    format_error_dimensions(v_dimensions, dimensionality)
  );
}

static const char* format_error_dimensions(
  const int* v_dimensions,
  int dimensionality
) {
  // 11 characters for the widest `int` and 2 for its `, ` separator, then 3
  // more for the `(`, the `)`, and the null terminator
  const size_t size = (size_t) dimensionality * 13 + 3;
  char* out = R_alloc(size, sizeof(char));

  size_t loc = 0;
  out[loc++] = '(';

  for (int i = 0; i < dimensionality; ++i) {
    if (i != 0) {
      out[loc++] = ',';
      out[loc++] = ' ';
    }

    loc += (size_t) snprintf(out + loc, size - loc, "%d", v_dimensions[i]);
  }

  out[loc++] = ')';
  out[loc] = '\0';

  return out;
}
