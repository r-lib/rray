static r_obj* rray_extract_flat(
  r_obj* x,
  r_obj* i,
  struct rray_arg* x_arg,
  struct rray_arg* i_arg,
  struct r_lazy error_call
);

static r_obj* rray_extract_mask(
  r_obj* x,
  r_obj* i,
  struct rray_arg* x_arg,
  struct rray_arg* i_arg,
  struct r_lazy error_call
);

static r_obj* rray_extract_points(
  r_obj* x,
  r_obj* i,
  struct rray_arg* x_arg,
  struct rray_arg* i_arg,
  struct r_lazy error_call
);

static r_obj* rray_extract_column(r_obj* x, r_ssize size, r_ssize j);
