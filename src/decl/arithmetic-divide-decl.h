static rray_binary_arithmetic_fn rray_divide_switch(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

static r_obj* rray_divide_lgl_lgl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_divide_lgl_int(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_divide_int_lgl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_divide_lgl_dbl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_divide_dbl_lgl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_divide_lgl_cpl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_divide_cpl_lgl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_divide_int_int(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_divide_int_dbl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_divide_dbl_int(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_divide_int_cpl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_divide_cpl_int(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_divide_dbl_dbl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_divide_dbl_cpl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_divide_cpl_dbl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_divide_cpl_cpl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
);

static inline double rray_divide_dbl_one(
  double x,
  double y,
  struct r_lazy error_call
);
static inline r_complex rray_divide_cpl_one(
  r_complex x,
  r_complex y,
  struct r_lazy error_call
);
