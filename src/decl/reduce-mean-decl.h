static rray_reduce_nested_fn rray_mean_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* rray_mean_lgl(
  r_obj* x,
  const struct rray_strided_iterator_plan* outer_plan,
  const struct rray_strided_iterator_plan* inner_plan,
  struct r_lazy error_call
);
static r_obj* rray_mean_lgl_na_rm(
  r_obj* x,
  const struct rray_strided_iterator_plan* outer_plan,
  const struct rray_strided_iterator_plan* inner_plan,
  struct r_lazy error_call
);
static r_obj* rray_mean_int(
  r_obj* x,
  const struct rray_strided_iterator_plan* outer_plan,
  const struct rray_strided_iterator_plan* inner_plan,
  struct r_lazy error_call
);
static r_obj* rray_mean_int_na_rm(
  r_obj* x,
  const struct rray_strided_iterator_plan* outer_plan,
  const struct rray_strided_iterator_plan* inner_plan,
  struct r_lazy error_call
);
static r_obj* rray_mean_dbl(
  r_obj* x,
  const struct rray_strided_iterator_plan* outer_plan,
  const struct rray_strided_iterator_plan* inner_plan,
  struct r_lazy error_call
);
static r_obj* rray_mean_dbl_na_rm(
  r_obj* x,
  const struct rray_strided_iterator_plan* outer_plan,
  const struct rray_strided_iterator_plan* inner_plan,
  struct r_lazy error_call
);

static inline double rray_mean_lgl_one(
  const int* v_x,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* inner_plan
);
static inline double rray_mean_lgl_one_na_rm(
  const int* v_x,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* inner_plan
);
static inline double rray_mean_int_one(
  const int* v_x,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* inner_plan
);
static inline double rray_mean_int_one_na_rm(
  const int* v_x,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* inner_plan
);
static inline double rray_mean_dbl_one(
  const double* v_x,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* inner_plan
);
static inline double rray_mean_dbl_one_na_rm(
  const double* v_x,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* inner_plan
);
