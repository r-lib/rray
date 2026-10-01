static rray_reduce_fn rray_prod_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* rray_prod_lgl(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_prod_lgl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_prod_int(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_prod_int_na_rm(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_prod_dbl(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_prod_dbl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_prod_cpl(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan,
  struct r_lazy error_call
);
static r_obj* rray_prod_cpl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan,
  struct r_lazy error_call
);
