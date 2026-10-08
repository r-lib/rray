static r_obj* rray_if_else_fill(
  r_obj* condition,
  r_obj* true_,
  r_obj* false_,
  r_obj* missing,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_true_strides,
  const r_ssize* v_false_strides,
  const r_ssize* v_missing_strides
);
