static rray_reduce_grouped_fn rray_mean_along_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* rray_mean_along_lgl(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* outer_plan,
  const struct rray_strided_iterator_plan* inner_plan
);
static r_obj* rray_mean_along_lgl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* outer_plan,
  const struct rray_strided_iterator_plan* inner_plan
);
static r_obj* rray_mean_along_int(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* outer_plan,
  const struct rray_strided_iterator_plan* inner_plan
);
static r_obj* rray_mean_along_int_na_rm(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* outer_plan,
  const struct rray_strided_iterator_plan* inner_plan
);
static r_obj* rray_mean_along_dbl(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* outer_plan,
  const struct rray_strided_iterator_plan* inner_plan
);
static r_obj* rray_mean_along_dbl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* outer_plan,
  const struct rray_strided_iterator_plan* inner_plan
);

static inline double rray_mean_along_lgl_one(
  const int* v_x,
  r_ssize x_base,
  const struct rray_strided_iterator_plan* inner_plan
);
static inline double rray_mean_along_lgl_one_na_rm(
  const int* v_x,
  r_ssize x_base,
  const struct rray_strided_iterator_plan* inner_plan
);
static inline double rray_mean_along_int_one(
  const int* v_x,
  r_ssize x_base,
  const struct rray_strided_iterator_plan* inner_plan
);
static inline double rray_mean_along_int_one_na_rm(
  const int* v_x,
  r_ssize x_base,
  const struct rray_strided_iterator_plan* inner_plan
);
static inline double rray_mean_along_dbl_one(
  const double* v_x,
  r_ssize x_base,
  const struct rray_strided_iterator_plan* inner_plan
);
static inline double rray_mean_along_dbl_one_na_rm(
  const double* v_x,
  r_ssize x_base,
  const struct rray_strided_iterator_plan* inner_plan
);

static inline double rray_mean_along_dbl_missing(
  const double* v_x,
  r_ssize x_base,
  const struct rray_strided_iterator_plan* inner_plan
);
