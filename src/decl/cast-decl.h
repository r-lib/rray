static r_obj* rray_cast_lgl_to_int(r_obj* x);
static r_obj* rray_cast_lgl_to_dbl(r_obj* x);
static r_obj* rray_cast_lgl_to_cpl(r_obj* x);
static r_obj* rray_cast_int_to_dbl(r_obj* x);
static r_obj* rray_cast_int_to_cpl(r_obj* x);
static r_obj* rray_cast_dbl_to_cpl(r_obj* x);
static r_obj* rray_cast_int_to_lgl(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);
static r_obj* rray_cast_dbl_to_lgl(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);
static r_obj* rray_cast_dbl_to_int(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static inline double rray_cast_lgl_to_dbl_one(int x);
static inline r_complex rray_cast_lgl_to_cpl_one(int x);
static inline double rray_cast_int_to_dbl_one(int x);
static inline r_complex rray_cast_int_to_cpl_one(int x);
static inline r_complex rray_cast_dbl_to_cpl_one(double x);
static inline int rray_cast_int_to_lgl_one(
  int x,
  r_ssize i,
  struct rray_arg* arg,
  struct r_lazy error_call
);
static inline int rray_cast_dbl_to_lgl_one(
  double x,
  r_ssize i,
  struct rray_arg* arg,
  struct r_lazy error_call
);
static inline int rray_cast_dbl_to_int_one(
  double x,
  r_ssize i,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_no_return void stop_incompatible_cast(
  enum r_type x,
  enum r_type to,
  struct rray_arg* arg,
  struct r_lazy error_call
);
static r_no_return void stop_lossy_cast(
  enum r_type x,
  enum r_type to,
  r_ssize i,
  struct rray_arg* arg,
  struct r_lazy error_call
);
