#include "extract.h"

#include "dimensionality.h"
#include "extract-subscript.h"
#include "size.h"
#include "strides.h"
#include "utils.h"

#include "decl/extract-decl.h"

r_obj* ffi_rray_extract(r_obj* ffi_x, r_obj* ffi_i, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_extract(ffi_x, ffi_i, rray_args.x, rray_args.i, error_call);
}

r_obj* rray_extract(
  r_obj* x,
  r_obj* i,
  struct rray_arg* x_arg,
  struct rray_arg* i_arg,
  struct r_lazy error_call
) {
  check_unclassed(x, x_arg, error_call);
  x = KEEP(arg_as_array(x, x_arg, error_call));

  r_obj* dimensions = r_dim(x);
  const int* v_dimensions = r_int_cbegin(dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(dimensions);
  check_dimensionality(dimensionality);

  i = KEEP(rray_as_extract_subscript(
    i,
    v_dimensions,
    dimensionality,
    i_arg,
    error_call
  ));

  r_obj* offsets = KEEP(rray_extract_offsets(i, v_dimensions, dimensionality));
  const r_ssize* v_offsets = r_raw_cbegin(offsets);
  const r_ssize size = r_length(offsets) / (r_ssize) sizeof(r_ssize);

  r_obj* out;

  switch (r_typeof(x)) {
  case R_TYPE_logical:
    out = rray_extract_lgl(x, v_offsets, size);
    break;
  case R_TYPE_integer:
    out = rray_extract_int(x, v_offsets, size);
    break;
  case R_TYPE_double:
    out = rray_extract_dbl(x, v_offsets, size);
    break;
  case R_TYPE_complex:
    out = rray_extract_cpl(x, v_offsets, size);
    break;
  case R_TYPE_raw:
    out = rray_extract_raw(x, v_offsets, size);
    break;
  case R_TYPE_character:
    out = rray_extract_chr(x, v_offsets, size);
    break;
  case R_TYPE_list:
    out = rray_extract_list(x, v_offsets, size);
    break;
  default:
    r_stop_unreachable();
  }

  KEEP(out);
  r_attrib_poke_dim(out, r_int(r_ssize_as_integer(size)));

  FREE(4);
  return out;
}

static r_obj* rray_extract_offsets(
  r_obj* i,
  const int* v_dimensions,
  int dimensionality
) {
  if (r_typeof(i) == R_TYPE_logical) {
    return rray_extract_mask_offsets(i, v_dimensions, dimensionality);
  }
  if (r_dim(i) == r_null) {
    return rray_extract_positions_offsets(i);
  }
  return rray_extract_points_offsets(i, v_dimensions, dimensionality);
}

static r_obj* rray_extract_mask_offsets(
  r_obj* i,
  const int* v_dimensions,
  int dimensionality
) {
  const r_ssize size = rray_size_from_dimensions(v_dimensions, dimensionality);
  const int* v_i = r_lgl_cbegin(i);
  const r_ssize i_step = r_length(i) == 1 ? 0 : 1;

  r_ssize out_size = 0;

  for (r_ssize j = 0; j < size; ++j) {
    out_size += v_i[j * i_step] != 0;
  }

  r_obj* out = r_alloc_raw(out_size * (r_ssize) sizeof(r_ssize));
  r_ssize* v_out = r_raw_begin(out);

  r_ssize k = 0;

  for (r_ssize j = 0; j < size; ++j) {
    const int elt = v_i[j * i_step];

    if (elt == 0) {
      continue;
    }

    v_out[k] = elt == r_globals.na_lgl ? -1 : j;
    ++k;
  }

  return out;
}

static r_obj* rray_extract_positions_offsets(r_obj* i) {
  const r_ssize size = r_length(i);
  const int* v_i = r_int_cbegin(i);

  r_obj* out = r_alloc_raw(size * (r_ssize) sizeof(r_ssize));
  r_ssize* v_out = r_raw_begin(out);

  for (r_ssize j = 0; j < size; ++j) {
    const int elt = v_i[j];
    v_out[j] = elt == r_globals.na_int ? -1 : (r_ssize) elt - 1;
  }

  return out;
}

static r_obj* rray_extract_points_offsets(
  r_obj* i,
  const int* v_dimensions,
  int dimensionality
) {
  const r_ssize size = r_int_cbegin(r_dim(i))[0];
  const int* v_i = r_int_cbegin(i);

  r_ssize v_strides[RRAY_MAX_DIMENSIONALITY];
  rray_fill_strides_from_dimensions(v_dimensions, dimensionality, v_strides);

  r_obj* out = r_alloc_raw(size * (r_ssize) sizeof(r_ssize));
  r_ssize* v_out = r_raw_begin(out);

  for (r_ssize j = 0; j < size; ++j) {
    r_ssize offset = 0;

    for (int axis = 0; axis < dimensionality; ++axis) {
      const int elt = v_i[axis * size + j];

      if (elt == r_globals.na_int) {
        offset = -1;
        break;
      }

      offset += (r_ssize) (elt - 1) * v_strides[axis];
    }

    v_out[j] = offset;
  }

  return out;
}

#define RRAY_EXTRACT_ATOMIC(RTYPE, CTYPE, CONST_DEREF, DEREF, MISSING)         \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, size));                              \
  CTYPE* v_out = DEREF(out);                                                   \
                                                                               \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
                                                                               \
  for (r_ssize j = 0; j < size; ++j) {                                         \
    const r_ssize offset = v_offsets[j];                                       \
    v_out[j] = offset == -1 ? MISSING : v_x[offset];                           \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

#define RRAY_EXTRACT_BARRIER(RTYPE, CONST_DEREF, POKE, MISSING)                \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, size));                              \
                                                                               \
  r_obj* const* v_x = CONST_DEREF(x);                                          \
                                                                               \
  for (r_ssize j = 0; j < size; ++j) {                                         \
    const r_ssize offset = v_offsets[j];                                       \
    POKE(out, j, offset == -1 ? MISSING : v_x[offset]);                        \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

static r_obj* rray_extract_lgl(
  r_obj* x,
  const r_ssize* v_offsets,
  r_ssize size
) {
  RRAY_EXTRACT_ATOMIC(
    R_TYPE_logical,
    int,
    r_lgl_cbegin,
    r_lgl_begin,
    r_globals.na_lgl
  );
}

static r_obj* rray_extract_int(
  r_obj* x,
  const r_ssize* v_offsets,
  r_ssize size
) {
  RRAY_EXTRACT_ATOMIC(
    R_TYPE_integer,
    int,
    r_int_cbegin,
    r_int_begin,
    r_globals.na_int
  );
}

static r_obj* rray_extract_dbl(
  r_obj* x,
  const r_ssize* v_offsets,
  r_ssize size
) {
  RRAY_EXTRACT_ATOMIC(
    R_TYPE_double,
    double,
    r_dbl_cbegin,
    r_dbl_begin,
    r_globals.na_dbl
  );
}

static r_obj* rray_extract_cpl(
  r_obj* x,
  const r_ssize* v_offsets,
  r_ssize size
) {
  RRAY_EXTRACT_ATOMIC(
    R_TYPE_complex,
    r_complex,
    r_cpl_cbegin,
    r_cpl_begin,
    r_globals.na_cpl
  );
}

static r_obj* rray_extract_raw(
  r_obj* x,
  const r_ssize* v_offsets,
  r_ssize size
) {
  RRAY_EXTRACT_ATOMIC(R_TYPE_raw, Rbyte, r_raw_cbegin, r_raw_begin, 0);
}

static r_obj* rray_extract_chr(
  r_obj* x,
  const r_ssize* v_offsets,
  r_ssize size
) {
  RRAY_EXTRACT_BARRIER(
    R_TYPE_character,
    r_chr_cbegin,
    r_chr_poke,
    r_globals.na_str
  );
}

static r_obj* rray_extract_list(
  r_obj* x,
  const r_ssize* v_offsets,
  r_ssize size
) {
  RRAY_EXTRACT_BARRIER(R_TYPE_list, r_list_cbegin, r_list_poke, r_null);
}

#undef RRAY_EXTRACT_ATOMIC
#undef RRAY_EXTRACT_BARRIER
