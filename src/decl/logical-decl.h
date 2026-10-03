static r_obj* rray_logical(
  r_obj* x,
  r_obj* y,
  enum rray_logical_op op,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

static r_obj* rray_logical_lgl_lgl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  enum rray_logical_op op
);
