static r_obj* rray_slice_as_locations(struct rray_subscript subscript);

static r_obj* rray_slice_names(
  r_obj* const* v_x_names,
  const int* v_dimensions,
  int dimensionality,
  const int* const* v_v_locations
);

static r_obj* rray_slice_axis_names(
  r_obj* x_axis_names,
  const int* v_locations,
  int dimension
);

static bool rray_slice_locations_any_missing(
  const int* const* v_v_locations,
  const int* v_dimensions,
  int dimensionality
);

static inline r_ssize rray_slice_start(
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const int* v_point,
  int dimensionality,
  r_ssize size,
  bool any_missing
);

static inline r_ssize rray_slice_offset(
  const int* v_locations,
  r_ssize stride,
  int point
);

static r_obj* rray_slice_lgl(
  r_obj* x,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static r_obj* rray_slice_int(
  r_obj* x,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static r_obj* rray_slice_dbl(
  r_obj* x,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static r_obj* rray_slice_cpl(
  r_obj* x,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static r_obj* rray_slice_raw(
  r_obj* x,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static r_obj* rray_slice_chr(
  r_obj* x,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static r_obj* rray_slice_list(
  r_obj* x,
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
