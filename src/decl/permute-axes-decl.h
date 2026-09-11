static r_obj* rray_permute_axes_lgl(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
);
static r_obj* rray_permute_axes_int(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
);
static r_obj* rray_permute_axes_dbl(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
);
static r_obj* rray_permute_axes_cpl(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
);
static r_obj* rray_permute_axes_raw(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
);
static r_obj* rray_permute_axes_chr(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
);
static r_obj* rray_permute_axes_list(
  r_obj* x,
  r_ssize size,
  struct rray_iterator* it
);

static r_obj* rray_permute_axes_names(
  r_obj* x,
  const int* v_axes,
  int dimensionality
);
