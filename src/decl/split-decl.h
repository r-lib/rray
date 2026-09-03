static void rray_split_lgl(
  r_obj* x,
  r_obj* out,
  struct rray_iterator* out_it,
  struct rray_iterator* out_elt_it
);
static void rray_split_int(
  r_obj* x,
  r_obj* out,
  struct rray_iterator* out_it,
  struct rray_iterator* out_elt_it
);
static void rray_split_dbl(
  r_obj* x,
  r_obj* out,
  struct rray_iterator* out_it,
  struct rray_iterator* out_elt_it
);
static void rray_split_cpl(
  r_obj* x,
  r_obj* out,
  struct rray_iterator* out_it,
  struct rray_iterator* out_elt_it
);
static void rray_split_raw(
  r_obj* x,
  r_obj* out,
  struct rray_iterator* out_it,
  struct rray_iterator* out_elt_it
);
static void rray_split_chr(
  r_obj* x,
  r_obj* out,
  struct rray_iterator* out_it,
  struct rray_iterator* out_elt_it
);
static void rray_split_list(
  r_obj* x,
  r_obj* out,
  struct rray_iterator* out_it,
  struct rray_iterator* out_elt_it
);
