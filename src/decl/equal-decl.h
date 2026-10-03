typedef r_obj* (*rray_equality_fn)(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  enum rray_equality_op op
);

static r_obj* rray_equality(
  r_obj* x,
  r_obj* y,
  enum rray_equality_op op,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);
static rray_equality_fn rray_equality_switch(
  r_obj* x,
  r_obj* y,
  enum rray_equality_op op,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);
static const char* rray_equality_op_as_c_string(enum rray_equality_op op);

static r_no_return void stop_unsupported_equality(
  const char* op,
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

static r_obj* rray_equality_lgl_lgl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  enum rray_equality_op op
);
static r_obj* rray_equality_lgl_int(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  enum rray_equality_op op
);
static r_obj* rray_equality_int_lgl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  enum rray_equality_op op
);
static r_obj* rray_equality_lgl_dbl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  enum rray_equality_op op
);
static r_obj* rray_equality_dbl_lgl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  enum rray_equality_op op
);
static r_obj* rray_equality_lgl_cpl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  enum rray_equality_op op
);
static r_obj* rray_equality_cpl_lgl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  enum rray_equality_op op
);
static r_obj* rray_equality_int_int(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  enum rray_equality_op op
);
static r_obj* rray_equality_int_dbl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  enum rray_equality_op op
);
static r_obj* rray_equality_dbl_int(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  enum rray_equality_op op
);
static r_obj* rray_equality_int_cpl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  enum rray_equality_op op
);
static r_obj* rray_equality_cpl_int(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  enum rray_equality_op op
);
static r_obj* rray_equality_dbl_dbl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  enum rray_equality_op op
);
static r_obj* rray_equality_dbl_cpl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  enum rray_equality_op op
);
static r_obj* rray_equality_cpl_dbl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  enum rray_equality_op op
);
static r_obj* rray_equality_cpl_cpl(
  r_obj* x,
  r_obj* y,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_x_broadcast_strides,
  const r_ssize* v_y_broadcast_strides,
  enum rray_equality_op op
);

static inline int rray_equal_int_one(int x, int y);
static inline int rray_not_equal_int_one(int x, int y);
static inline int rray_equal_dbl_one(double x, double y);
static inline int rray_not_equal_dbl_one(double x, double y);
static inline int rray_equal_cpl_one(r_complex x, r_complex y);
static inline int rray_not_equal_cpl_one(r_complex x, r_complex y);
