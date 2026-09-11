static r_obj* rray_transpose_permutation(
  r_obj* permutation,
  int dimensionality,
  struct r_lazy error_call
);

static void rray_transpose_strides(
  r_ssize* v_strides,
  const int* v_x_dimensions,
  const int* v_permutation,
  int dimensionality
);

static r_obj* rray_transpose_names(
  r_obj* x,
  const int* v_permutation,
  int dimensionality
);
