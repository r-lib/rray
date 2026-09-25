static struct rray_extract_subscript rray_as_extract_mask(
  r_obj* i,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  struct rray_arg* i_arg,
  struct r_lazy error_call
);

static struct rray_extract_subscript rray_as_extract_positions(
  r_obj* i,
  r_ssize size,
  struct rray_arg* i_arg,
  struct r_lazy error_call
);

static struct rray_extract_subscript rray_as_extract_points(
  r_obj* i,
  const int* v_dimensions,
  int dimensionality,
  struct rray_arg* i_arg,
  struct r_lazy error_call
);

static struct rray_extract_subscript rray_as_extract_complement(
  r_obj* i,
  r_ssize size
);

static struct rray_extract_subscript rray_as_extract_nonzero(
  r_obj* i,
  r_ssize size
);

static r_ssize rray_mask_size(r_obj* mask, r_ssize size);

static struct rray_subscript_summary rray_subscript_summarise(
  r_obj* i,
  r_ssize start,
  r_ssize size
);

static struct rray_subscript_summary rray_subscript_summarise_int(
  const int* v_i,
  r_ssize size
);

static struct rray_subscript_summary rray_subscript_summarise_dbl(
  const double* v_i,
  r_ssize size
);

static r_no_return void stop_subscript_fractional(
  struct rray_arg* i_arg,
  struct r_lazy error_call
);

static const char* rray_extract_subscript_kind_name(
  enum rray_extract_subscript_kind kind
);
