static rray_reduce_fn rray_sum_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* rray_sum_lgl(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
);
static r_obj* rray_sum_lgl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
);
static r_obj* rray_sum_int(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
);
static r_obj* rray_sum_int_na_rm(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
);
static r_obj* rray_sum_dbl(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
);
static r_obj* rray_sum_dbl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
);
static r_obj* rray_sum_cpl(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
);
static r_obj* rray_sum_cpl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
);

static inline int rray_sum_lgl_one(int out, int x);
static inline int rray_sum_lgl_one_na_rm(int out, int x);
static inline int rray_sum_int_one(int out, int x);
static inline int rray_sum_int_one_na_rm(int out, int x);
static inline double rray_sum_dbl_one(double out, double x);
static inline double rray_sum_dbl_one_na_rm(double out, double x);
static inline r_complex rray_sum_cpl_one(r_complex out, r_complex x);
static inline r_complex rray_sum_cpl_one_na_rm(
  r_complex out,
  r_complex x
);

static inline void check_sum_int_overflow(int out, int x);
