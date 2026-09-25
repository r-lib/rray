static r_obj* rray_extract_lgl(
  r_obj* x,
  struct rray_extract_subscript subscript,
  const int* v_dimensions,
  int dimensionality
);
static r_obj* rray_extract_int(
  r_obj* x,
  struct rray_extract_subscript subscript,
  const int* v_dimensions,
  int dimensionality
);
static r_obj* rray_extract_dbl(
  r_obj* x,
  struct rray_extract_subscript subscript,
  const int* v_dimensions,
  int dimensionality
);
static r_obj* rray_extract_cpl(
  r_obj* x,
  struct rray_extract_subscript subscript,
  const int* v_dimensions,
  int dimensionality
);
static r_obj* rray_extract_raw(
  r_obj* x,
  struct rray_extract_subscript subscript,
  const int* v_dimensions,
  int dimensionality
);
static r_obj* rray_extract_chr(
  r_obj* x,
  struct rray_extract_subscript subscript,
  const int* v_dimensions,
  int dimensionality
);
static r_obj* rray_extract_list(
  r_obj* x,
  struct rray_extract_subscript subscript,
  const int* v_dimensions,
  int dimensionality
);

static inline r_ssize rray_position_offset_int(int position);
static inline r_ssize rray_position_offset_dbl(double position);

static inline r_ssize rray_point_offset_int(
  const int* v_point,
  r_ssize size,
  const r_ssize* v_strides,
  int dimensionality
);
static inline r_ssize rray_point_offset_dbl(
  const double* v_point,
  r_ssize size,
  const r_ssize* v_strides,
  int dimensionality
);
