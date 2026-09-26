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
