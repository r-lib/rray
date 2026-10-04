#include "reduce-mean.h"

#include "reduce.h"
#include "type.h"
#include "utils.h"

#include "decl/reduce-mean-decl.h"

#define RRAY_MEAN_INT64_MAX_COUNT ((r_ssize) 1 << 32)
#define RRAY_MEAN_INT64_MAX_EXACT ((int64_t) 1 << 53)

r_obj* ffi_rray_mean(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const bool na_rm = r_arg_as_bool(ffi_na_rm, "na_rm");
  return rray_mean(ffi_x, ffi_axes, na_rm, rray_args.x, error_call);
}

r_obj* rray_mean(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_reduce(x, axes, na_rm, rray_mean_switch, arg, error_call);
}

static rray_reduce_fn rray_mean_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  switch (rray_typeof(x)) {
  case RRAY_TYPE_logical:
    return na_rm ? rray_mean_lgl_na_rm : rray_mean_lgl;
  case RRAY_TYPE_integer:
    return na_rm ? rray_mean_int_na_rm : rray_mean_int;
  case RRAY_TYPE_double:
    return na_rm ? rray_mean_dbl_na_rm : rray_mean_dbl;

  case RRAY_TYPE_complex:
  case RRAY_TYPE_character:
  case RRAY_TYPE_raw:
  case RRAY_TYPE_list:
    stop_unsupported_reduce("mean", x, arg, error_call);

  case RRAY_TYPE_scalar:
    stop_scalar_input(x, arg, error_call);
  }

  r_stop_unreachable();
}

static r_obj* rray_mean_lgl(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  const r_ssize count = rray_mean_count(x, out_size);
  const int na_lgl = r_globals.na_lgl;

  r_obj* sums = KEEP(r_alloc_raw0(out_size * sizeof(int64_t)));
  int64_t* v_sums = (int64_t*) r_raw_begin(sums);

  const int* v_x = r_lgl_cbegin(x);

  bool any_na = false;

  struct rray_run_iterator it;
  rray_run_iterator_init1(
    &it,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );

  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
    const r_ssize start = rray_run_iterator_start(&it);
    const r_ssize end = rray_run_iterator_end(&it);

    r_ssize out_loc = rray_run_iterator_loc(&it, 0);
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

    if (out_stride == 0) {
      int64_t sum = v_sums[out_loc];

      for (r_ssize i = start; i < end; ++i) {
        const int x_elt = v_x[i];
        const bool na = x_elt == na_lgl;
        sum += na ? 0 : x_elt;
        any_na |= na;
      }

      v_sums[out_loc] = sum;
    } else {
      for (r_ssize i = start; i < end; ++i) {
        const int x_elt = v_x[i];
        const bool na = x_elt == na_lgl;
        v_sums[out_loc] += na ? 0 : x_elt;
        any_na |= na;
        out_loc += out_stride;
      }
    }
  }

  r_obj* out = KEEP(r_alloc_double(out_size));
  double* v_out = r_dbl_begin(out);

  for (r_ssize i = 0; i < out_size; ++i) {
    v_out[i] = rray_mean_int64(v_sums[i], count);
  }

  if (any_na) {
    rray_mean_lgl_propagate_na(
      v_x,
      v_out,
      v_dimensions,
      dimensionality,
      v_out_broadcast_strides
    );
  }

  FREE(2);
  return out;
}

static r_obj* rray_mean_lgl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  const int na_lgl = r_globals.na_lgl;

  r_obj* sums = KEEP(r_alloc_raw0(out_size * sizeof(int64_t)));
  int64_t* v_sums = (int64_t*) r_raw_begin(sums);

  r_obj* counts = KEEP(r_alloc_raw0(out_size * sizeof(r_ssize)));
  r_ssize* v_counts = (r_ssize*) r_raw_begin(counts);

  const int* v_x = r_lgl_cbegin(x);

  struct rray_run_iterator it;
  rray_run_iterator_init1(
    &it,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );

  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
    const r_ssize start = rray_run_iterator_start(&it);
    const r_ssize end = rray_run_iterator_end(&it);

    r_ssize out_loc = rray_run_iterator_loc(&it, 0);
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

    if (out_stride == 0) {
      int64_t sum = v_sums[out_loc];
      r_ssize count = 0;

      for (r_ssize i = start; i < end; ++i) {
        const int x_elt = v_x[i];
        const bool na = x_elt == na_lgl;
        sum += na ? 0 : x_elt;
        count += !na;
      }

      v_sums[out_loc] = sum;
      v_counts[out_loc] += count;
    } else {
      for (r_ssize i = start; i < end; ++i) {
        const int x_elt = v_x[i];
        const bool na = x_elt == na_lgl;
        v_sums[out_loc] += na ? 0 : x_elt;
        v_counts[out_loc] += !na;
        out_loc += out_stride;
      }
    }
  }

  r_obj* out = KEEP(r_alloc_double(out_size));
  double* v_out = r_dbl_begin(out);

  for (r_ssize i = 0; i < out_size; ++i) {
    v_out[i] = rray_mean_int64(v_sums[i], v_counts[i]);
  }

  FREE(3);
  return out;
}

static r_obj* rray_mean_int(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  const r_ssize count = rray_mean_count(x, out_size);

  if (count > RRAY_MEAN_INT64_MAX_COUNT) {
    return rray_mean_int_fallback(
      x,
      out_size,
      v_dimensions,
      dimensionality,
      v_out_broadcast_strides
    );
  }

  const int na_int = r_globals.na_int;

  r_obj* sums = KEEP(r_alloc_raw0(out_size * sizeof(int64_t)));
  int64_t* v_sums = (int64_t*) r_raw_begin(sums);

  const int* v_x = r_int_cbegin(x);

  bool any_na = false;

  struct rray_run_iterator it;
  rray_run_iterator_init1(
    &it,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );

  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
    const r_ssize start = rray_run_iterator_start(&it);
    const r_ssize end = rray_run_iterator_end(&it);

    r_ssize out_loc = rray_run_iterator_loc(&it, 0);
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

    if (out_stride == 0) {
      int64_t sum = v_sums[out_loc];

      for (r_ssize i = start; i < end; ++i) {
        const int x_elt = v_x[i];
        const bool na = x_elt == na_int;
        sum += na ? 0 : x_elt;
        any_na |= na;
      }

      v_sums[out_loc] = sum;
    } else {
      for (r_ssize i = start; i < end; ++i) {
        const int x_elt = v_x[i];
        const bool na = x_elt == na_int;
        v_sums[out_loc] += na ? 0 : x_elt;
        any_na |= na;
        out_loc += out_stride;
      }
    }
  }

  r_obj* out = KEEP(r_alloc_double(out_size));
  double* v_out = r_dbl_begin(out);

  for (r_ssize i = 0; i < out_size; ++i) {
    v_out[i] = rray_mean_int64(v_sums[i], count);
  }

  if (any_na) {
    rray_mean_int_propagate_na(
      v_x,
      v_out,
      v_dimensions,
      dimensionality,
      v_out_broadcast_strides
    );
  }

  FREE(2);
  return out;
}

static r_obj* rray_mean_int_na_rm(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  if (rray_mean_count(x, out_size) > RRAY_MEAN_INT64_MAX_COUNT) {
    return rray_mean_int_na_rm_fallback(
      x,
      out_size,
      v_dimensions,
      dimensionality,
      v_out_broadcast_strides
    );
  }

  const int na_int = r_globals.na_int;

  r_obj* sums = KEEP(r_alloc_raw0(out_size * sizeof(int64_t)));
  int64_t* v_sums = (int64_t*) r_raw_begin(sums);

  r_obj* counts = KEEP(r_alloc_raw0(out_size * sizeof(r_ssize)));
  r_ssize* v_counts = (r_ssize*) r_raw_begin(counts);

  const int* v_x = r_int_cbegin(x);

  struct rray_run_iterator it;
  rray_run_iterator_init1(
    &it,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );

  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
    const r_ssize start = rray_run_iterator_start(&it);
    const r_ssize end = rray_run_iterator_end(&it);

    r_ssize out_loc = rray_run_iterator_loc(&it, 0);
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

    if (out_stride == 0) {
      int64_t sum = v_sums[out_loc];
      r_ssize count = 0;

      for (r_ssize i = start; i < end; ++i) {
        const int x_elt = v_x[i];
        const bool na = x_elt == na_int;
        sum += na ? 0 : x_elt;
        count += !na;
      }

      v_sums[out_loc] = sum;
      v_counts[out_loc] += count;
    } else {
      for (r_ssize i = start; i < end; ++i) {
        const int x_elt = v_x[i];
        const bool na = x_elt == na_int;
        v_sums[out_loc] += na ? 0 : x_elt;
        v_counts[out_loc] += !na;
        out_loc += out_stride;
      }
    }
  }

  r_obj* out = KEEP(r_alloc_double(out_size));
  double* v_out = r_dbl_begin(out);

  for (r_ssize i = 0; i < out_size; ++i) {
    v_out[i] = rray_mean_int64(v_sums[i], v_counts[i]);
  }

  FREE(3);
  return out;
}

static r_obj* rray_mean_dbl(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  const r_ssize count = rray_mean_count(x, out_size);

  r_obj* out = KEEP(r_alloc_double(out_size));
  double* v_out = r_dbl_begin(out);

  r_obj* correction = KEEP(r_alloc_double(out_size));
  double* v_correction = r_dbl_begin(correction);

  for (r_ssize i = 0; i < out_size; ++i) {
    v_out[i] = 0.0;
    v_correction[i] = 0.0;
  }

  const double* v_x = r_dbl_cbegin(x);

  struct rray_run_iterator it;
  rray_run_iterator_init1(
    &it,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );

  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
    const r_ssize start = rray_run_iterator_start(&it);
    const r_ssize end = rray_run_iterator_end(&it);

    r_ssize out_loc = rray_run_iterator_loc(&it, 0);
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

    if (out_stride == 0) {
      double sum = v_out[out_loc];

      for (r_ssize i = start; i < end; ++i) {
        sum += v_x[i];
      }

      v_out[out_loc] = sum;
    } else {
      for (r_ssize i = start; i < end; ++i) {
        v_out[out_loc] += v_x[i];
        out_loc += out_stride;
      }
    }
  }

  for (r_ssize i = 0; i < out_size; ++i) {
    v_out[i] /= count;
  }

  rray_run_iterator_reset1(&it);

  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
    const r_ssize start = rray_run_iterator_start(&it);
    const r_ssize end = rray_run_iterator_end(&it);

    r_ssize out_loc = rray_run_iterator_loc(&it, 0);
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

    if (out_stride == 0) {
      const double mean = v_out[out_loc];
      double correction = v_correction[out_loc];

      for (r_ssize i = start; i < end; ++i) {
        correction += v_x[i] - mean;
      }

      v_correction[out_loc] = correction;
    } else {
      for (r_ssize i = start; i < end; ++i) {
        v_correction[out_loc] += v_x[i] - v_out[out_loc];
        out_loc += out_stride;
      }
    }
  }

  bool any_nan = false;
  bool any_infinite = false;

  for (r_ssize i = 0; i < out_size; ++i) {
    const double mean = v_out[i];

    if (R_FINITE(mean)) {
      v_out[i] = mean + v_correction[i] / count;
    } else if (ISNAN(mean)) {
      any_nan = true;
    } else {
      any_infinite = true;
    }
  }

  if (any_infinite) {
    rray_mean_dbl_rescale(
      v_x,
      v_out,
      out_size,
      count,
      v_dimensions,
      dimensionality,
      v_out_broadcast_strides
    );
  }

  if (any_nan) {
    rray_mean_dbl_propagate_na(
      v_x,
      v_out,
      v_dimensions,
      dimensionality,
      v_out_broadcast_strides
    );
  }

  FREE(2);
  return out;
}

static r_obj* rray_mean_dbl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  r_obj* out = KEEP(r_alloc_double(out_size));
  double* v_out = r_dbl_begin(out);

  r_obj* correction = KEEP(r_alloc_double(out_size));
  double* v_correction = r_dbl_begin(correction);

  for (r_ssize i = 0; i < out_size; ++i) {
    v_out[i] = 0.0;
    v_correction[i] = 0.0;
  }

  r_obj* counts = KEEP(r_alloc_raw0(out_size * sizeof(r_ssize)));
  r_ssize* v_counts = (r_ssize*) r_raw_begin(counts);

  const double* v_x = r_dbl_cbegin(x);

  struct rray_run_iterator it;
  rray_run_iterator_init1(
    &it,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );

  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
    const r_ssize start = rray_run_iterator_start(&it);
    const r_ssize end = rray_run_iterator_end(&it);

    r_ssize out_loc = rray_run_iterator_loc(&it, 0);
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

    if (out_stride == 0) {
      double sum = v_out[out_loc];
      r_ssize count = 0;

      for (r_ssize i = start; i < end; ++i) {
        const double x_elt = v_x[i];
        const bool na = ISNAN(x_elt);
        sum += na ? 0 : x_elt;
        count += !na;
      }

      v_out[out_loc] = sum;
      v_counts[out_loc] += count;
    } else {
      for (r_ssize i = start; i < end; ++i) {
        const double x_elt = v_x[i];
        const bool na = ISNAN(x_elt);
        v_out[out_loc] += na ? 0 : x_elt;
        v_counts[out_loc] += !na;
        out_loc += out_stride;
      }
    }
  }

  for (r_ssize i = 0; i < out_size; ++i) {
    v_out[i] /= v_counts[i];
  }

  rray_run_iterator_reset1(&it);

  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
    const r_ssize start = rray_run_iterator_start(&it);
    const r_ssize end = rray_run_iterator_end(&it);

    r_ssize out_loc = rray_run_iterator_loc(&it, 0);
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

    if (out_stride == 0) {
      const double mean = v_out[out_loc];
      double correction = v_correction[out_loc];

      for (r_ssize i = start; i < end; ++i) {
        const double x_elt = v_x[i];
        correction += ISNAN(x_elt) ? 0 : x_elt - mean;
      }

      v_correction[out_loc] = correction;
    } else {
      for (r_ssize i = start; i < end; ++i) {
        const double x_elt = v_x[i];
        v_correction[out_loc] += ISNAN(x_elt) ? 0 : x_elt - v_out[out_loc];
        out_loc += out_stride;
      }
    }
  }

  bool any_infinite = false;

  for (r_ssize i = 0; i < out_size; ++i) {
    const double mean = v_out[i];

    if (R_FINITE(mean)) {
      v_out[i] = mean + v_correction[i] / v_counts[i];
    } else if (!ISNAN(mean)) {
      any_infinite = true;
    }
  }

  if (any_infinite) {
    rray_mean_dbl_rescale_na_rm(
      v_x,
      v_out,
      v_counts,
      out_size,
      v_dimensions,
      dimensionality,
      v_out_broadcast_strides
    );
  }

  FREE(3);
  return out;
}

static inline r_ssize rray_mean_count(r_obj* x, r_ssize out_size) {
  return out_size == 0 ? 0 : r_length(x) / out_size;
}

static inline double rray_mean_int64(int64_t sum, r_ssize count) {
  if (count == 0) {
    return R_NaN;
  }

  if (-RRAY_MEAN_INT64_MAX_EXACT <= sum && sum <= RRAY_MEAN_INT64_MAX_EXACT) {
    return (double) sum / count;
  }

  return (double) (sum / count) + (double) (sum % count) / count;
}

static void rray_mean_lgl_propagate_na(
  const int* v_x,
  double* v_out,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
) {
  const int na_lgl = r_globals.na_lgl;
  const double na_dbl = r_globals.na_dbl;

  struct rray_run_iterator it;
  rray_run_iterator_init1(
    &it,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );

  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
    const r_ssize start = rray_run_iterator_start(&it);
    const r_ssize end = rray_run_iterator_end(&it);

    r_ssize out_loc = rray_run_iterator_loc(&it, 0);
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

    for (r_ssize i = start; i < end; ++i) {
      if (v_x[i] == na_lgl) {
        v_out[out_loc] = na_dbl;
      }
      out_loc += out_stride;
    }
  }
}

static r_obj* rray_mean_int_fallback(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
) {
  const r_ssize count = rray_mean_count(x, out_size);
  const int na_int = r_globals.na_int;
  const double na_dbl = r_globals.na_dbl;

  r_obj* out = KEEP(r_alloc_double(out_size));
  double* v_out = r_dbl_begin(out);

  for (r_ssize i = 0; i < out_size; ++i) {
    v_out[i] = 0.0;
  }

  const int* v_x = r_int_cbegin(x);

  struct rray_run_iterator it;
  rray_run_iterator_init1(
    &it,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );

  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
    const r_ssize start = rray_run_iterator_start(&it);
    const r_ssize end = rray_run_iterator_end(&it);

    r_ssize out_loc = rray_run_iterator_loc(&it, 0);
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

    if (out_stride == 0) {
      double sum = v_out[out_loc];

      for (r_ssize i = start; i < end; ++i) {
        const int x_elt = v_x[i];
        sum += x_elt == na_int ? na_dbl : x_elt;
      }

      v_out[out_loc] = sum;
    } else {
      for (r_ssize i = start; i < end; ++i) {
        const int x_elt = v_x[i];
        v_out[out_loc] += x_elt == na_int ? na_dbl : x_elt;
        out_loc += out_stride;
      }
    }
  }

  for (r_ssize i = 0; i < out_size; ++i) {
    const double sum = v_out[i];
    v_out[i] = ISNAN(sum) ? na_dbl : sum / count;
  }

  FREE(1);
  return out;
}

static void rray_mean_int_propagate_na(
  const int* v_x,
  double* v_out,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
) {
  const int na_int = r_globals.na_int;
  const double na_dbl = r_globals.na_dbl;

  struct rray_run_iterator it;
  rray_run_iterator_init1(
    &it,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );

  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
    const r_ssize start = rray_run_iterator_start(&it);
    const r_ssize end = rray_run_iterator_end(&it);

    r_ssize out_loc = rray_run_iterator_loc(&it, 0);
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

    for (r_ssize i = start; i < end; ++i) {
      if (v_x[i] == na_int) {
        v_out[out_loc] = na_dbl;
      }
      out_loc += out_stride;
    }
  }
}

static r_obj* rray_mean_int_na_rm_fallback(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
) {
  const int na_int = r_globals.na_int;

  r_obj* out = KEEP(r_alloc_double(out_size));
  double* v_out = r_dbl_begin(out);

  for (r_ssize i = 0; i < out_size; ++i) {
    v_out[i] = 0.0;
  }

  r_obj* counts = KEEP(r_alloc_raw0(out_size * sizeof(r_ssize)));
  r_ssize* v_counts = (r_ssize*) r_raw_begin(counts);

  const int* v_x = r_int_cbegin(x);

  struct rray_run_iterator it;
  rray_run_iterator_init1(
    &it,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );

  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
    const r_ssize start = rray_run_iterator_start(&it);
    const r_ssize end = rray_run_iterator_end(&it);

    r_ssize out_loc = rray_run_iterator_loc(&it, 0);
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

    if (out_stride == 0) {
      double sum = v_out[out_loc];
      r_ssize count = 0;

      for (r_ssize i = start; i < end; ++i) {
        const int x_elt = v_x[i];
        const bool na = x_elt == na_int;
        sum += na ? 0 : x_elt;
        count += !na;
      }

      v_out[out_loc] = sum;
      v_counts[out_loc] += count;
    } else {
      for (r_ssize i = start; i < end; ++i) {
        const int x_elt = v_x[i];
        const bool na = x_elt == na_int;
        v_out[out_loc] += na ? 0 : x_elt;
        v_counts[out_loc] += !na;
        out_loc += out_stride;
      }
    }
  }

  for (r_ssize i = 0; i < out_size; ++i) {
    v_out[i] /= v_counts[i];
  }

  FREE(2);
  return out;
}

static void rray_mean_dbl_rescale(
  const double* v_x,
  double* v_out,
  r_ssize out_size,
  r_ssize count,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
) {
  r_obj* sum = KEEP(r_alloc_double(out_size));
  double* v_sum = r_dbl_begin(sum);

  r_obj* correction = KEEP(r_alloc_double(out_size));
  double* v_correction = r_dbl_begin(correction);

  for (r_ssize i = 0; i < out_size; ++i) {
    v_sum[i] = 0.0;
    v_correction[i] = 0.0;
  }

  struct rray_run_iterator it;
  rray_run_iterator_init1(
    &it,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );

  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
    const r_ssize start = rray_run_iterator_start(&it);
    const r_ssize end = rray_run_iterator_end(&it);

    r_ssize out_loc = rray_run_iterator_loc(&it, 0);
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

    for (r_ssize i = start; i < end; ++i) {
      if (isinf(v_out[out_loc])) {
        v_sum[out_loc] += v_x[i] / count;
      }
      out_loc += out_stride;
    }
  }

  rray_run_iterator_reset1(&it);

  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
    const r_ssize start = rray_run_iterator_start(&it);
    const r_ssize end = rray_run_iterator_end(&it);

    r_ssize out_loc = rray_run_iterator_loc(&it, 0);
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

    for (r_ssize i = start; i < end; ++i) {
      if (isinf(v_out[out_loc])) {
        v_correction[out_loc] += (v_x[i] - v_sum[out_loc]) / count;
      }
      out_loc += out_stride;
    }
  }

  for (r_ssize i = 0; i < out_size; ++i) {
    if (isinf(v_out[i])) {
      const double mean = v_sum[i];
      v_out[i] = R_FINITE(mean) ? mean + v_correction[i] : mean;
    }
  }

  FREE(2);
}

static void rray_mean_dbl_propagate_na(
  const double* v_x,
  double* v_out,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
) {
  const double na_dbl = r_globals.na_dbl;

  struct rray_run_iterator it;
  rray_run_iterator_init1(
    &it,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );

  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
    const r_ssize start = rray_run_iterator_start(&it);
    const r_ssize end = rray_run_iterator_end(&it);

    r_ssize out_loc = rray_run_iterator_loc(&it, 0);
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

    for (r_ssize i = start; i < end; ++i) {
      if (R_IsNA(v_x[i])) {
        v_out[out_loc] = na_dbl;
      }
      out_loc += out_stride;
    }
  }
}

static void rray_mean_dbl_rescale_na_rm(
  const double* v_x,
  double* v_out,
  const r_ssize* v_counts,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
) {
  r_obj* sum = KEEP(r_alloc_double(out_size));
  double* v_sum = r_dbl_begin(sum);

  r_obj* correction = KEEP(r_alloc_double(out_size));
  double* v_correction = r_dbl_begin(correction);

  for (r_ssize i = 0; i < out_size; ++i) {
    v_sum[i] = 0.0;
    v_correction[i] = 0.0;
  }

  struct rray_run_iterator it;
  rray_run_iterator_init1(
    &it,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );

  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
    const r_ssize start = rray_run_iterator_start(&it);
    const r_ssize end = rray_run_iterator_end(&it);

    r_ssize out_loc = rray_run_iterator_loc(&it, 0);
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

    for (r_ssize i = start; i < end; ++i) {
      const double x_elt = v_x[i];
      if (isinf(v_out[out_loc]) && !ISNAN(x_elt)) {
        v_sum[out_loc] += x_elt / v_counts[out_loc];
      }
      out_loc += out_stride;
    }
  }

  rray_run_iterator_reset1(&it);

  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
    const r_ssize start = rray_run_iterator_start(&it);
    const r_ssize end = rray_run_iterator_end(&it);

    r_ssize out_loc = rray_run_iterator_loc(&it, 0);
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

    for (r_ssize i = start; i < end; ++i) {
      const double x_elt = v_x[i];
      if (isinf(v_out[out_loc]) && !ISNAN(x_elt)) {
        v_correction[out_loc] += (x_elt - v_sum[out_loc]) / v_counts[out_loc];
      }
      out_loc += out_stride;
    }
  }

  for (r_ssize i = 0; i < out_size; ++i) {
    if (isinf(v_out[i])) {
      const double mean = v_sum[i];
      v_out[i] = R_FINITE(mean) ? mean + v_correction[i] : mean;
    }
  }

  FREE(2);
}
