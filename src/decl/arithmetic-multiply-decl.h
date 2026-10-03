static rray_binary_arithmetic_fn rray_multiply_switch(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

static r_obj* rray_multiply_lgl_lgl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_multiply_lgl_int(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_multiply_int_lgl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_multiply_lgl_dbl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_multiply_dbl_lgl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_multiply_lgl_cpl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_multiply_cpl_lgl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_multiply_int_int(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_multiply_int_dbl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_multiply_dbl_int(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_multiply_int_cpl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_multiply_cpl_int(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_multiply_dbl_dbl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_multiply_dbl_cpl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_multiply_cpl_dbl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_multiply_cpl_cpl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  struct r_lazy error_call
);
