#include "gather.h"

#include "decl/gather-decl.h"

r_obj* rray_gather(r_obj* x, r_ssize size, struct rray_iterator* it) {
  switch (r_typeof(x)) {
  case R_TYPE_logical:
    return rray_gather_lgl(x, size, it);
  case R_TYPE_integer:
    return rray_gather_int(x, size, it);
  case R_TYPE_double:
    return rray_gather_dbl(x, size, it);
  case R_TYPE_complex:
    return rray_gather_cpl(x, size, it);
  case R_TYPE_raw:
    return rray_gather_raw(x, size, it);
  case R_TYPE_character:
    return rray_gather_chr(x, size, it);
  case R_TYPE_list:
    return rray_gather_list(x, size, it);
  default:
    r_stop_unreachable();
  }
}

#define RRAY_GATHER_ATOMIC(RTYPE, CTYPE, CONST_DEREF, DEREF)                   \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, size));                              \
  const CTYPE* v_x = CONST_DEREF(x);                                           \
  CTYPE* v_out = DEREF(out);                                                   \
                                                                               \
  RRAY_ITERATOR_FOR_EACH(it, i, loc, { v_out[i] = v_x[loc]; });                \
                                                                               \
  FREE(1);                                                                     \
  return out;

#define RRAY_GATHER_BARRIER(RTYPE, CONST_DEREF, POKE)                          \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, size));                              \
  r_obj* const* v_x = CONST_DEREF(x);                                          \
                                                                               \
  RRAY_ITERATOR_FOR_EACH(it, i, loc, { POKE(out, i, v_x[loc]); });             \
                                                                               \
  FREE(1);                                                                     \
  return out;

static r_obj* rray_gather_lgl(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
) {
  RRAY_GATHER_ATOMIC(R_TYPE_logical, int, r_lgl_cbegin, r_lgl_begin);
}

static r_obj* rray_gather_int(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
) {
  RRAY_GATHER_ATOMIC(R_TYPE_integer, int, r_int_cbegin, r_int_begin);
}

static r_obj* rray_gather_dbl(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
) {
  RRAY_GATHER_ATOMIC(R_TYPE_double, double, r_dbl_cbegin, r_dbl_begin);
}

static r_obj* rray_gather_cpl(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
) {
  RRAY_GATHER_ATOMIC(R_TYPE_complex, r_complex, r_cpl_cbegin, r_cpl_begin);
}

static r_obj* rray_gather_raw(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
) {
  RRAY_GATHER_ATOMIC(R_TYPE_raw, Rbyte, r_raw_cbegin, r_raw_begin);
}

static r_obj* rray_gather_chr(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
) {
  RRAY_GATHER_BARRIER(R_TYPE_character, r_chr_cbegin, r_chr_poke);
}

static r_obj* rray_gather_list(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
) {
  RRAY_GATHER_BARRIER(R_TYPE_list, r_list_cbegin, r_list_poke);
}

#undef RRAY_GATHER_ATOMIC
#undef RRAY_GATHER_BARRIER
