static inline r_ssize rray_slice_assign_value_start(
  const r_ssize* v_value_strides,
  const int* v_point,
  int dimensionality
);

static void rray_slice_assign_lgl(
  r_obj* out,
  r_obj* value,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const r_ssize* v_value_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static void rray_slice_assign_int(
  r_obj* out,
  r_obj* value,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const r_ssize* v_value_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static void rray_slice_assign_dbl(
  r_obj* out,
  r_obj* value,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const r_ssize* v_value_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static void rray_slice_assign_cpl(
  r_obj* out,
  r_obj* value,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const r_ssize* v_value_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static void rray_slice_assign_raw(
  r_obj* out,
  r_obj* value,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const r_ssize* v_value_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static void rray_slice_assign_chr(
  r_obj* out,
  r_obj* value,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const r_ssize* v_value_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static void rray_slice_assign_list(
  r_obj* out,
  r_obj* value,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const r_ssize* v_value_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
