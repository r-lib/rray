static void check_slice_indices(
  r_obj* indices,
  int dimensionality,
  struct r_lazy error_call
);

static void rray_slice_fill_locations(
  struct rray_subscript subscript,
  r_ssize* v_locations
);

static r_obj* rray_slice_names(
  r_obj* x_names,
  const int* v_x_dimensions,
  const int* v_dimensions,
  int dimensionality,
  r_ssize* const* v_v_locations
);

static r_obj* rray_slice_axis_names(
  r_obj* x_axis_names,
  int x_dimension,
  const r_ssize* v_locations,
  int dimension
);

static bool rray_slice_locations_are_identity(
  const r_ssize* v_locations,
  int dimension,
  int x_dimension
);

static bool rray_slice_locations_as_offsets(
  r_ssize* v_locations,
  int dimension,
  r_ssize stride
);

static inline r_ssize rray_slice_start(
  r_ssize* const* v_v_offsets,
  int dimensionality,
  r_ssize size
);

static r_obj* rray_slice_lgl(
  r_obj* x,
  r_ssize* const* v_v_offsets,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static r_obj* rray_slice_int(
  r_obj* x,
  r_ssize* const* v_v_offsets,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static r_obj* rray_slice_dbl(
  r_obj* x,
  r_ssize* const* v_v_offsets,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static r_obj* rray_slice_cpl(
  r_obj* x,
  r_ssize* const* v_v_offsets,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static r_obj* rray_slice_raw(
  r_obj* x,
  r_ssize* const* v_v_offsets,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static r_obj* rray_slice_chr(
  r_obj* x,
  r_ssize* const* v_v_offsets,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
static r_obj* rray_slice_list(
  r_obj* x,
  r_ssize* const* v_v_offsets,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  bool any_missing
);
