static struct rray_subscript rray_as_extract_mask(
  r_obj* index,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  struct rray_arg* index_arg,
  struct r_lazy error_call
);

static struct rray_subscript rray_as_extract_points(
  r_obj* index,
  const int* v_dimensions,
  int dimensionality,
  struct rray_arg* index_arg,
  struct r_lazy error_call
);
