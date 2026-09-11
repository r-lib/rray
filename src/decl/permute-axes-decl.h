static void rray_permute_axes_strides(
  r_ssize* v_strides,
  const int* v_x_dimensions,
  const int* v_axes,
  int dimensionality
);

static r_obj* rray_permute_axes_names(
  r_obj* x,
  const int* v_axes,
  int dimensionality
);
