static r_obj* rray_broadcast_lgl(
  r_obj* x,
  r_ssize size,
  struct rray_strided_iterator_plan* plan
);
static r_obj* rray_broadcast_int(
  r_obj* x,
  r_ssize size,
  struct rray_strided_iterator_plan* plan
);
static r_obj* rray_broadcast_dbl(
  r_obj* x,
  r_ssize size,
  struct rray_strided_iterator_plan* plan
);
static r_obj* rray_broadcast_cpl(
  r_obj* x,
  r_ssize size,
  struct rray_strided_iterator_plan* plan
);
static r_obj* rray_broadcast_raw(
  r_obj* x,
  r_ssize size,
  struct rray_strided_iterator_plan* plan
);
static r_obj* rray_broadcast_chr(
  r_obj* x,
  r_ssize size,
  struct rray_strided_iterator_plan* plan
);
static r_obj* rray_broadcast_list(
  r_obj* x,
  r_ssize size,
  struct rray_strided_iterator_plan* plan
);
