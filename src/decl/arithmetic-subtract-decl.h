static rray_binary_arithmetic_fn rray_subtract_switch(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

static r_obj* rray_subtract_lgl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_subtract_lgl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_subtract_int_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_subtract_lgl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_subtract_dbl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_subtract_lgl_cpl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_subtract_cpl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_subtract_int_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_subtract_int_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_subtract_dbl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_subtract_int_cpl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_subtract_cpl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_subtract_dbl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_subtract_dbl_cpl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_subtract_cpl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_subtract_cpl_cpl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);

static inline int rray_subtract_int_one(
  int x,
  int y,
  struct r_lazy error_call
);
static inline double rray_subtract_dbl_one(
  double x,
  double y,
  struct r_lazy error_call
);
static inline r_complex rray_subtract_cpl_one(
  r_complex x,
  r_complex y,
  struct r_lazy error_call
);
