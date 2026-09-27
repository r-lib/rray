static rray_reduce_fn rray_all_along_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
);
static rray_reduce_fn rray_any_along_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* rray_all_along_lgl(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
);
static r_obj* rray_all_along_lgl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
);
static r_obj* rray_any_along_lgl(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
);
static r_obj* rray_any_along_lgl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
);

static inline int rray_all_along_lgl_one_na_rm(int out, int x);
static inline int rray_any_along_lgl_one_na_rm(int out, int x);
