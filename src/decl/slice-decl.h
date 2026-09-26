static void rray_slice_fill_locations(
  struct rray_subscript subscript,
  r_ssize* v_locations
);

static r_obj* rray_slice_names(
  r_obj* const* v_x_names,
  const int* v_dimensions,
  int dimensionality,
  const struct rray_slice_axis* v_axes
);

static r_obj* rray_slice_axis_names(
  r_obj* x_axis_names,
  bool identity,
  const r_ssize* v_locations,
  int dimension
);

static bool rray_slice_locations_any_missing(
  const r_ssize* v_locations,
  int dimension
);

static void rray_slice_locations_as_offsets(
  r_ssize* v_locations,
  int dimension,
  r_ssize stride,
  bool any_missing
);

static inline r_ssize rray_slice_start(
  const struct rray_slice_axis* v_axes,
  int dimensionality,
  r_ssize size
);

static inline r_ssize rray_slice_offset(
  const struct rray_slice_axis* v_axes,
  int axis,
  r_ssize point
);

static r_obj* rray_slice_lgl(
  r_obj* x,
  const struct rray_slice_axis* v_axes,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static r_obj* rray_slice_int(
  r_obj* x,
  const struct rray_slice_axis* v_axes,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static r_obj* rray_slice_dbl(
  r_obj* x,
  const struct rray_slice_axis* v_axes,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static r_obj* rray_slice_cpl(
  r_obj* x,
  const struct rray_slice_axis* v_axes,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static r_obj* rray_slice_raw(
  r_obj* x,
  const struct rray_slice_axis* v_axes,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static r_obj* rray_slice_chr(
  r_obj* x,
  const struct rray_slice_axis* v_axes,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static r_obj* rray_slice_list(
  r_obj* x,
  const struct rray_slice_axis* v_axes,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
