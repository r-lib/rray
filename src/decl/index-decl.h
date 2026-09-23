struct rray_index_plan {
  r_ssize size;
  int x_dimensionality;
  int dimensionality;
  r_ssize v_dimensions[RRAY_MAX_DIMENSIONALITY];
  r_ssize v_x_strides[RRAY_MAX_DIMENSIONALITY];
  r_ssize v_index_strides[RRAY_MAX_DIMENSIONALITY]
                          [RRAY_MAX_DIMENSIONALITY];
  const int* v_indices[RRAY_MAX_DIMENSIONALITY];
};

static struct rray_index_plan rray_index_plan(
  r_obj* x_dimensions,
  r_obj* indices,
  r_obj* dimensions,
  struct r_lazy error_call
);

static inline bool rray_index_plan_source_location(
  const struct rray_index_plan* plan,
  const r_ssize* v_index_locations,
  r_ssize* p_source_location
);

static inline void rray_index_plan_next(
  const struct rray_index_plan* plan,
  int* v_point,
  r_ssize* v_index_locations
);

static r_obj* rray_index_lgl(
  r_obj* x,
  const struct rray_index_plan* plan
);
static r_obj* rray_index_int(
  r_obj* x,
  const struct rray_index_plan* plan
);
static r_obj* rray_index_dbl(
  r_obj* x,
  const struct rray_index_plan* plan
);
static r_obj* rray_index_cpl(
  r_obj* x,
  const struct rray_index_plan* plan
);
static r_obj* rray_index_raw(
  r_obj* x,
  const struct rray_index_plan* plan
);
static r_obj* rray_index_chr(
  r_obj* x,
  const struct rray_index_plan* plan
);
static r_obj* rray_index_list(
  r_obj* x,
  const struct rray_index_plan* plan
);
