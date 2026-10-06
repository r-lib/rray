static rray_reduce_fn rray_all_switch(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);
static rray_reduce_fn rray_any_switch(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* rray_all_lgl(
  r_obj* x,
  bool na_rm,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_any_lgl(
  r_obj* x,
  bool na_rm,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
);
