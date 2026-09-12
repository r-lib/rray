static rray_reduce_fn rray_sum_along_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* rray_sum_along_lgl(
  r_obj* x,
  r_ssize out_size,
  struct rray_strided_iterator* it
);
static r_obj* rray_sum_along_lgl_na_rm(
  r_obj* x,
  r_ssize out_size,
  struct rray_strided_iterator* it
);
static r_obj* rray_sum_along_int(
  r_obj* x,
  r_ssize out_size,
  struct rray_strided_iterator* it
);
static r_obj* rray_sum_along_int_na_rm(
  r_obj* x,
  r_ssize out_size,
  struct rray_strided_iterator* it
);
static r_obj* rray_sum_along_dbl(
  r_obj* x,
  r_ssize out_size,
  struct rray_strided_iterator* it
);
static r_obj* rray_sum_along_dbl_na_rm(
  r_obj* x,
  r_ssize out_size,
  struct rray_strided_iterator* it
);
static r_obj* rray_sum_along_cpl(
  r_obj* x,
  r_ssize out_size,
  struct rray_strided_iterator* it
);
static r_obj* rray_sum_along_cpl_na_rm(
  r_obj* x,
  r_ssize out_size,
  struct rray_strided_iterator* it
);

static inline int rray_sum_along_lgl_one(int out, int x);
static inline int rray_sum_along_lgl_one_na_rm(int out, int x);
static inline int rray_sum_along_int_one(int out, int x);
static inline int rray_sum_along_int_one_na_rm(int out, int x);
static inline double rray_sum_along_dbl_one(double out, double x);
static inline double rray_sum_along_dbl_one_na_rm(double out, double x);
static inline r_complex rray_sum_along_cpl_one(r_complex out, r_complex x);
static inline r_complex rray_sum_along_cpl_one_na_rm(
  r_complex out,
  r_complex x
);

static inline void check_sum_along_int_overflow(int out, int x);
