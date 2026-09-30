static r_obj* rray_extremum(
  r_obj* x,
  r_obj* y,
  bool na_rm,
  enum rray_extremum_op op,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);
static r_obj* rray_extremum_switch(
  r_obj* x,
  r_obj* y,
  enum rray_extremum_op op,
  const struct rray_strided_iterator2_plan* plan,
  bool na_rm,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

static r_no_return void stop_unsupported_extremum(
  const char* op,
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

static r_obj* rray_pmax_lgl_lgl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmax_lgl_int(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmax_int_lgl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmax_lgl_dbl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmax_dbl_lgl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmax_int_int(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmax_int_dbl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmax_dbl_int(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmax_dbl_dbl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  bool na_rm,
  struct r_lazy error_call
);

static r_obj* rray_pmin_lgl_lgl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmin_lgl_int(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmin_int_lgl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmin_lgl_dbl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmin_dbl_lgl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmin_int_int(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmin_int_dbl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmin_dbl_int(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmin_dbl_dbl(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  bool na_rm,
  struct r_lazy error_call
);
