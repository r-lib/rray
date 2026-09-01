static r_obj* rray_split_lgl(
  r_obj* x,
  r_obj* out_elt_dimensions,
  r_ssize out_size,
  r_ssize out_elt_size,
  struct rray_iterator* out_it,
  struct rray_iterator* out_elt_it
);
static r_obj* rray_split_int(
  r_obj* x,
  r_obj* out_elt_dimensions,
  r_ssize out_size,
  r_ssize out_elt_size,
  struct rray_iterator* out_it,
  struct rray_iterator* out_elt_it
);
static r_obj* rray_split_dbl(
  r_obj* x,
  r_obj* out_elt_dimensions,
  r_ssize out_size,
  r_ssize out_elt_size,
  struct rray_iterator* out_it,
  struct rray_iterator* out_elt_it
);
static r_obj* rray_split_cpl(
  r_obj* x,
  r_obj* out_elt_dimensions,
  r_ssize out_size,
  r_ssize out_elt_size,
  struct rray_iterator* out_it,
  struct rray_iterator* out_elt_it
);
static r_obj* rray_split_raw(
  r_obj* x,
  r_obj* out_elt_dimensions,
  r_ssize out_size,
  r_ssize out_elt_size,
  struct rray_iterator* out_it,
  struct rray_iterator* out_elt_it
);
static r_obj* rray_split_chr(
  r_obj* x,
  r_obj* out_elt_dimensions,
  r_ssize out_size,
  r_ssize out_elt_size,
  struct rray_iterator* out_it,
  struct rray_iterator* out_elt_it
);
static r_obj* rray_split_list(
  r_obj* x,
  r_obj* out_elt_dimensions,
  r_ssize out_size,
  r_ssize out_elt_size,
  struct rray_iterator* out_it,
  struct rray_iterator* out_elt_it
);

static r_obj* rray_split_dimensions(
  const int* v_dimensions,
  int dimensionality,
  const int* v_axes,
  r_ssize axes_size
);

static void rray_split_names(
  r_obj* out,
  r_obj* const* v_x_names,
  const int* v_out_dimensions,
  int dimensionality,
  r_ssize out_size
);
