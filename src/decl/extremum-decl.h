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
  r_ssize size,
  struct rray_strided_iterator2* it,
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

static r_obj* rray_max_lgl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_max_lgl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_max_int_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_max_lgl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_max_dbl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_max_int_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_max_int_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_max_dbl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_max_dbl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);

static r_obj* rray_min_lgl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_min_lgl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_min_int_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_min_lgl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_min_dbl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_min_int_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_min_int_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_min_dbl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);
static r_obj* rray_min_dbl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  bool na_rm,
  struct r_lazy error_call
);

static inline int rray_max_int_one_propagate_na(int x, int y);
static inline int rray_max_int_one_remove_na(int x, int y);
static inline double rray_max_dbl_one_propagate_na(double x, double y);
static inline double rray_max_dbl_one_remove_na(double x, double y);
static inline int rray_min_int_one_propagate_na(int x, int y);
static inline int rray_min_int_one_remove_na(int x, int y);
static inline double rray_min_dbl_one_propagate_na(double x, double y);
static inline double rray_min_dbl_one_remove_na(double x, double y);
