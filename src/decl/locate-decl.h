static r_obj* rray_locate(
  r_obj* x,
  int axis,
  bool na_rm,
  enum rray_locate_op op,
  struct rray_arg* arg,
  struct r_lazy error_call
);
static rray_locate_fn rray_locate_switch(
  r_obj* x,
  enum rray_locate_op op,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_no_return void stop_unsupported_locate(
  const char* op,
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* rray_locate_max_lgl(
  r_obj* x,
  bool na_rm,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis
);
static r_obj* rray_locate_max_int(
  r_obj* x,
  bool na_rm,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis
);
static r_obj* rray_locate_max_dbl(
  r_obj* x,
  bool na_rm,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis
);

static r_obj* rray_locate_min_lgl(
  r_obj* x,
  bool na_rm,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis
);
static r_obj* rray_locate_min_int(
  r_obj* x,
  bool na_rm,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis
);
static r_obj* rray_locate_min_dbl(
  r_obj* x,
  bool na_rm,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis
);

static inline bool rray_locate_max_lgl_one(int x, int best);
static inline bool rray_locate_max_lgl_one_na_rm(int x, int best);
static inline bool rray_locate_max_int_one(int x, int best);
static inline bool rray_locate_max_int_one_na_rm(int x, int best);
static inline bool rray_locate_max_dbl_one(double x, double best);
static inline bool rray_locate_max_dbl_one_na_rm(double x, double best);

static inline bool rray_locate_min_lgl_one(int x, int best);
static inline bool rray_locate_min_lgl_one_na_rm(int x, int best);
static inline bool rray_locate_min_int_one(int x, int best);
static inline bool rray_locate_min_int_one_na_rm(int x, int best);
static inline bool rray_locate_min_dbl_one(double x, double best);
static inline bool rray_locate_min_dbl_one_na_rm(double x, double best);
