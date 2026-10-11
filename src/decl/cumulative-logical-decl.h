static rray_cumulative_fn rray_cumulative_all_switch(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);
static rray_cumulative_fn rray_cumulative_any_switch(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* rray_cumulative_all_lgl(
  r_obj* x,
  r_ssize stride,
  r_ssize axis_dimension,
  struct r_lazy error_call
);
static r_obj* rray_cumulative_any_lgl(
  r_obj* x,
  r_ssize stride,
  r_ssize axis_dimension,
  struct r_lazy error_call
);
