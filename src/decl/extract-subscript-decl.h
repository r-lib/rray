static r_obj* rray_as_extract_mask(
  r_obj* i,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  struct rray_arg* i_arg,
  struct r_lazy error_call
);

static r_obj* rray_as_extract_positions(
  r_obj* i,
  r_ssize size,
  struct rray_arg* i_arg,
  struct r_lazy error_call
);

static r_obj* rray_as_extract_points(
  r_obj* i,
  const int* v_dimensions,
  int dimensionality,
  struct rray_arg* i_arg,
  struct r_lazy error_call
);

static r_obj* rray_as_extract_integer(
  r_obj* i,
  struct rray_arg* i_arg,
  struct r_lazy error_call
);

static r_obj* rray_as_extract_complement(
  const int* v_i,
  r_ssize i_size,
  r_ssize size
);

static r_obj* rray_as_extract_nonzero(const int* v_i, r_ssize i_size);

static r_obj* vec_bare(r_obj* x);
