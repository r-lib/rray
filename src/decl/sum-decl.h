static r_obj* rray_sum_lgl(
  r_obj* x,
  r_ssize out_size,
  bool na_rm,
  struct rray_iterator* it
);
static r_obj* rray_sum_int(
  r_obj* x,
  r_ssize out_size,
  bool na_rm,
  struct rray_iterator* it
);
static r_obj* rray_sum_dbl(
  r_obj* x,
  r_ssize out_size,
  bool na_rm,
  struct rray_iterator* it
);
static r_obj* rray_sum_cpl(
  r_obj* x,
  r_ssize out_size,
  bool na_rm,
  struct rray_iterator* it
);

static void check_sum_type(r_obj* x, struct r_lazy error_call);

static inline int rray_sum_lgl_one(int out, int x);
static inline int rray_sum_lgl_one_na_rm(int out, int x);
static inline int rray_sum_int_one(int out, int x);
static inline int rray_sum_int_one_na_rm(int out, int x);
static inline double rray_sum_dbl_one(double out, double x);
static inline double rray_sum_dbl_one_na_rm(double out, double x);
static inline r_complex rray_sum_cpl_one(r_complex out, r_complex x);
static inline r_complex rray_sum_cpl_one_na_rm(r_complex out, r_complex x);

static inline void check_sum_int_overflow(int out, int x);
