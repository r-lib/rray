static rray_binary_arithmetic_run_fn rray_add_switch(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

static r_obj* rray_add_lgl_lgl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
);
static r_obj* rray_add_lgl_int(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
);
static r_obj* rray_add_int_lgl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
);
static r_obj* rray_add_lgl_dbl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
);
static r_obj* rray_add_dbl_lgl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
);
static r_obj* rray_add_lgl_cpl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
);
static r_obj* rray_add_cpl_lgl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
);
static r_obj* rray_add_int_int(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
);
static r_obj* rray_add_int_dbl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
);
static r_obj* rray_add_dbl_int(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
);
static r_obj* rray_add_int_cpl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
);
static r_obj* rray_add_cpl_int(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
);
static r_obj* rray_add_dbl_dbl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
);
static r_obj* rray_add_dbl_cpl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
);
static r_obj* rray_add_cpl_dbl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
);
static r_obj* rray_add_cpl_cpl(
  r_obj* x,
  const r_ssize* v_x_broadcast_strides,
  r_obj* y,
  const r_ssize* v_y_broadcast_strides,
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
);
