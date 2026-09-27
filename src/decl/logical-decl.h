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
  const struct rray_strided_iterator2_plan* plan,
  enum rray_logical_op op
);
