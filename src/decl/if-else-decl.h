static r_obj* rray_if_else_dimensions_common(
  r_obj* condition,
  r_obj* true_,
  r_obj* false_,
  r_obj* missing,
  r_obj* dimensions,
  struct r_lazy error_call
);

static r_obj* rray_if_else_fill(
  r_obj* condition,
  r_obj* true_,
  r_obj* false_,
  r_obj* missing,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  const r_ssize* v_condition_strides,
  const r_ssize* v_true_strides,
  const r_ssize* v_false_strides,
  const r_ssize* v_missing_strides
);
