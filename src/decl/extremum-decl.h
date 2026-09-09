static r_obj* rray_extremum(
  r_obj* x,
  r_obj* y,
  bool na_rm,
  rray_extremum_switch_fn fn_switch,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

static rray_extremum_fn rray_pmax_switch(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);
static rray_extremum_fn rray_pmin_switch(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);
static rray_extremum_fn rray_extremum_switch(
  r_obj* x,
  r_obj* y,
  enum rray_extremum_op op,
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
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmax_lgl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmax_int_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmax_lgl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmax_dbl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmax_int_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmax_int_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmax_dbl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmax_dbl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);

static r_obj* rray_pmin_lgl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmin_lgl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmin_int_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmin_lgl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmin_dbl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmin_int_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmin_int_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmin_dbl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_pmin_dbl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);

static inline int rray_pmax_int_one(int x, int y, bool na_rm);
static inline double rray_pmax_dbl_one(double x, double y, bool na_rm);
static inline int rray_pmin_int_one(int x, int y, bool na_rm);
static inline double rray_pmin_dbl_one(double x, double y, bool na_rm);
