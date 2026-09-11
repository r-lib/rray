static r_obj* rray_gather_lgl(r_obj* x, r_ssize size, struct rray_iterator* it);
static r_obj* rray_gather_int(r_obj* x, r_ssize size, struct rray_iterator* it);
static r_obj* rray_gather_dbl(r_obj* x, r_ssize size, struct rray_iterator* it);
static r_obj* rray_gather_cpl(r_obj* x, r_ssize size, struct rray_iterator* it);
static r_obj* rray_gather_raw(r_obj* x, r_ssize size, struct rray_iterator* it);
static r_obj* rray_gather_chr(r_obj* x, r_ssize size, struct rray_iterator* it);
static r_obj* rray_gather_list(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
);
