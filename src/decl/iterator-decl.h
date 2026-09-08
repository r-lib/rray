static inline void rray__location_strides_init(
  r_ssize* v_location_strides,
  const int* v_point_dimensions,
  int point_dimensionality,
  const int* v_location_dimensions,
  int location_dimensionality,
  const char* location_arg
);

static inline bool rray__iterator_axes_coalescible(
  int left_dimension,
  r_ssize left_stride,
  int right_dimension,
  r_ssize right_stride
);

static inline int rray__iterator_axes_coalesce(
  int* v_dimensions,
  r_ssize* v_strides,
  int dimensionality
);

static inline int rray__iterator_axes_coalesce2(
  int* v_dimensions,
  r_ssize* v_strides1,
  r_ssize* v_strides2,
  int dimensionality
);
