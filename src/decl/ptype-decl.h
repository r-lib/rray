static r_no_return void stop_incompatible_ptype(
  enum rray_type x,
  enum rray_type y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

static r_no_return void stop_unsupported_binary_op(
  enum rray_binary_op op,
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);
