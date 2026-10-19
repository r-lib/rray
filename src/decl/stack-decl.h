static r_obj* rray_stack_prepare(
  r_obj* xs,
  int axis,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* rray_stack_axes(int x_dimensionality, int axis);
