static r_obj* rray_broadcast_lgl(
  r_obj* x,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides
);
static r_obj* rray_broadcast_int(
  r_obj* x,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides
);
static r_obj* rray_broadcast_dbl(
  r_obj* x,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides
);
static r_obj* rray_broadcast_cpl(
  r_obj* x,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides
);
static r_obj* rray_broadcast_raw(
  r_obj* x,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides
);
static r_obj* rray_broadcast_chr(
  r_obj* x,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides
);
static r_obj* rray_broadcast_list(
  r_obj* x,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides
);
