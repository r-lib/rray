static struct rray_subscript rray_as_complement_subscript(
  r_obj* index,
  r_ssize size
);

static struct rray_subscript rray_as_nonzero_subscript(
  r_obj* index,
  r_ssize size
);

static struct rray_subscript_summary rray_subscript_summarise_int(
  const int* v_index,
  r_ssize size
);

static struct rray_subscript_summary rray_subscript_summarise_dbl(
  const double* v_index,
  r_ssize size
);

static const char* rray_subscript_kind_name(enum rray_subscript_kind kind);
