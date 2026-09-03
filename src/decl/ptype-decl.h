static int rray_ptype_rank(enum r_type type);

static r_no_return void stop_incompatible_ptype(
  enum r_type x,
  enum r_type y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);
