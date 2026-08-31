#ifndef RRAY_BROADCAST_DECL_H
#define RRAY_BROADCAST_DECL_H

#include "rlang.h"

static void rray_broadcast_lgl(r_obj* x, r_obj* out, struct rray_iterator* it);
static void rray_broadcast_int(r_obj* x, r_obj* out, struct rray_iterator* it);
static void rray_broadcast_dbl(r_obj* x, r_obj* out, struct rray_iterator* it);
static void rray_broadcast_cpl(r_obj* x, r_obj* out, struct rray_iterator* it);
static void rray_broadcast_raw(r_obj* x, r_obj* out, struct rray_iterator* it);
static void rray_broadcast_chr(r_obj* x, r_obj* out, struct rray_iterator* it);
static void rray_broadcast_list(r_obj* x, r_obj* out, struct rray_iterator* it);

static r_obj* rray_broadcast_names(
  r_obj* const* v_names,
  const int* v_dimensions,
  int dimensionality,
  const int* v_out_dimensions,
  int out_dimensionality
);

#endif
