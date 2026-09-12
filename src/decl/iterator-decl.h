static inline void rray__location_strides_init(
  r_ssize* v_location_strides,
  const int* v_point_dimensions,
  int point_dimensionality,
  const int* v_location_dimensions,
  int location_dimensionality,
  const char* location_arg
);

static inline bool rray__iterator_axes_coalescible(
  r_ssize left_dimension,
  r_ssize left_stride,
  r_ssize right_dimension,
  r_ssize right_stride
);

static inline int rray__iterator_axes_coalesce2(
  r_ssize* v_dimensions,
  r_ssize* v_strides1,
  r_ssize* v_strides2,
  int dimensionality
);
