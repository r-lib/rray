static r_obj* rray_extract_offsets(
  r_obj* i,
  const int* v_dimensions,
  int dimensionality
);

static r_obj* rray_extract_mask_offsets(
  r_obj* i,
  const int* v_dimensions,
  int dimensionality
);

static r_obj* rray_extract_positions_offsets(r_obj* i);

static r_obj* rray_extract_points_offsets(
  r_obj* i,
  const int* v_dimensions,
  int dimensionality
);

static r_obj* rray_extract_lgl(
  r_obj* x,
  const r_ssize* v_offsets,
  r_ssize size
);
static r_obj* rray_extract_int(
  r_obj* x,
  const r_ssize* v_offsets,
  r_ssize size
);
static r_obj* rray_extract_dbl(
  r_obj* x,
  const r_ssize* v_offsets,
  r_ssize size
);
static r_obj* rray_extract_cpl(
  r_obj* x,
  const r_ssize* v_offsets,
  r_ssize size
);
static r_obj* rray_extract_raw(
  r_obj* x,
  const r_ssize* v_offsets,
  r_ssize size
);
static r_obj* rray_extract_chr(
  r_obj* x,
  const r_ssize* v_offsets,
  r_ssize size
);
static r_obj* rray_extract_list(
  r_obj* x,
  const r_ssize* v_offsets,
  r_ssize size
);
