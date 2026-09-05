static r_no_return void stop_unsupported_binary_arithmetic_op(
  enum rray_binary_arithmetic_op op,
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

static const char* rray_binary_arithmetic_op_as_c_string(
  enum rray_binary_arithmetic_op op
);
