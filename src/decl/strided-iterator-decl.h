static inline int rray__strided_iterator_axes_coalesce(
  r_ssize* v_dimensions,
  r_ssize* v_strides,
  int dimensionality
);

static inline int rray__strided_iterator_axes_coalesce2(
  r_ssize* v_dimensions,
  r_ssize* v_strides1,
  r_ssize* v_strides2,
  int dimensionality
);

static inline int rray__strided_iterator_axes_coalescen(
  r_ssize* v_dimensions,
  r_ssize* v_strides,
  int dimensionality,
  r_ssize n
);

static inline bool rray__strided_iterator_axes_coalescible(
  r_ssize left_dimension,
  r_ssize left_stride,
  r_ssize right_dimension,
  r_ssize right_stride
);

static inline void rray__check_broadcast_dimensions(
  const int* v_from_dimensions,
  int from_dimensionality,
  const int* v_to_dimensions,
  int to_dimensionality
);
