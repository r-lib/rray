static rray_reduce_fn rray_prod_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* rray_prod_lgl(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
);
static r_obj* rray_prod_lgl_na_rm(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
);
static r_obj* rray_prod_int(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
);
static r_obj* rray_prod_int_na_rm(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
);
static r_obj* rray_prod_dbl(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
);
static r_obj* rray_prod_dbl_na_rm(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
);
static r_obj* rray_prod_cpl(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
);
static r_obj* rray_prod_cpl_na_rm(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
);

static inline double rray_prod_lgl_one(double out, int x);
static inline double rray_prod_lgl_one_na_rm(double out, int x);
static inline double rray_prod_int_one(double out, int x);
static inline double rray_prod_int_one_na_rm(double out, int x);
static inline double rray_prod_dbl_one(double out, double x);
static inline double rray_prod_dbl_one_na_rm(double out, double x);
static inline r_complex rray_prod_cpl_one(r_complex out, r_complex x);
static inline r_complex rray_prod_cpl_one_na_rm(r_complex out, r_complex x);
