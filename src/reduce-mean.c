#include "reduce-mean.h"

#include "reduce.h"
#include "type.h"
#include "utils.h"

#include "decl/reduce-mean-decl.h"

// Max count is 2^32. Each integer is at most about 2^31 in size, so 2^32 of
// them add up to at most 2^63, which still fits in `int64_t`. If more than 2^32
// inputs make up a single slot in the output there is a risk of overflow (from
// reducing over two 2^16+1 length axes, all INT_MAX). `rray_mean_int()`
// switches to `rray_mean_int_fallback()`, which sums directly in a double
// instead, which is less precise but won't overflow.
#define RRAY_MEAN_INT64_MAX_COUNT ((r_ssize) 1 << 32)

// For anything outside the bounds of this number, casting from `int64_t` to
// `double` may do inexact rounding. If the `sum` exceeds this bound, we split
// its division by `count` into an exact integer division plus a possibly lossy
// (but much less so!) double division of the remainder by `count`, which
// greatly limits the overall error.
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
  const int* v_x = r_lgl_cbegin(x);
  const r_ssize x_size = r_length(x);
  const r_ssize count = rray_mean_count(x_size, out_size);

  return rray_mean_lgl_or_int(
    v_x,
    r_globals.na_lgl,
    out_size,
    count,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );
}

static r_obj* rray_mean_lgl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  const int* v_x = r_lgl_cbegin(x);

  return rray_mean_lgl_or_int_na_rm(
    v_x,
    r_globals.na_lgl,
    out_size,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );
}

static r_obj* rray_mean_int(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  const int* v_x = r_int_cbegin(x);
  const r_ssize x_size = r_length(x);
  const r_ssize count = rray_mean_count(x_size, out_size);

  if (count > RRAY_MEAN_INT64_MAX_COUNT) {
    return rray_mean_int_fallback(
      v_x,
      out_size,
      count,
      v_dimensions,
      dimensionality,
      v_out_broadcast_strides
    );
  }

  return rray_mean_lgl_or_int(
    v_x,
    r_globals.na_int,
    out_size,
    count,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );
}

static r_obj* rray_mean_int_na_rm(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  const int* v_x = r_int_cbegin(x);
  const r_ssize x_size = r_length(x);
  const r_ssize count = rray_mean_count(x_size, out_size);

  if (count > RRAY_MEAN_INT64_MAX_COUNT) {
    return rray_mean_int_na_rm_fallback(
      v_x,
      out_size,
      v_dimensions,
      dimensionality,
      v_out_broadcast_strides
    );
  }

  return rray_mean_lgl_or_int_na_rm(
    v_x,
    r_globals.na_int,
    out_size,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );
}

static r_obj* rray_mean_dbl(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  const double* v_x = r_dbl_cbegin(x);
  const r_ssize x_size = r_length(x);
  const r_ssize count = rray_mean_count(x_size, out_size);

  r_obj* out = KEEP(r_alloc_double(out_size));
  double* v_out = r_dbl_begin(out);

  r_obj* corrections = KEEP(r_alloc_double(out_size));
  double* v_corrections = r_dbl_begin(corrections);

  for (r_ssize i = 0; i < out_size; ++i) {
    v_out[i] = 0.0;
    v_corrections[i] = 0.0;
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
      double correction = v_corrections[out_loc];

      for (r_ssize i = start; i < end; ++i) {
        correction += v_x[i] - mean;
      }

      v_corrections[out_loc] = correction;
    } else {
      for (r_ssize i = start; i < end; ++i) {
        v_corrections[out_loc] += v_x[i] - v_out[out_loc];
        out_loc += out_stride;
      }
    }
  }

  bool any_nan = false;
  bool any_infinite = false;

  for (r_ssize i = 0; i < out_size; ++i) {
    const double mean = v_out[i];

    if (R_FINITE(mean)) {
      const double correction = v_corrections[i];
      v_out[i] = R_FINITE(correction) ? mean + correction / count : mean;
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
      v_corrections,
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
  const double* v_x = r_dbl_cbegin(x);

  r_obj* out = KEEP(r_alloc_double(out_size));
  double* v_out = r_dbl_begin(out);

  r_obj* corrections = KEEP(r_alloc_double(out_size));
  double* v_corrections = r_dbl_begin(corrections);

  for (r_ssize i = 0; i < out_size; ++i) {
    v_out[i] = 0.0;
    v_corrections[i] = 0.0;
  }

  r_obj* counts = KEEP(r_alloc_raw0(out_size * sizeof(r_ssize)));
  r_ssize* v_counts = (r_ssize*) r_raw_begin(counts);

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
      double correction = v_corrections[out_loc];

      for (r_ssize i = start; i < end; ++i) {
        const double x_elt = v_x[i];
        correction += ISNAN(x_elt) ? 0 : x_elt - mean;
      }

      v_corrections[out_loc] = correction;
    } else {
      for (r_ssize i = start; i < end; ++i) {
        const double x_elt = v_x[i];
        v_corrections[out_loc] += ISNAN(x_elt) ? 0 : x_elt - v_out[out_loc];
        out_loc += out_stride;
      }
    }
  }

  bool any_infinite = false;

  for (r_ssize i = 0; i < out_size; ++i) {
    const double mean = v_out[i];

    if (R_FINITE(mean)) {
      const double correction = v_corrections[i];
      v_out[i] = R_FINITE(correction) ? mean + correction / v_counts[i] : mean;
    } else if (!ISNAN(mean)) {
      any_infinite = true;
    }
  }

  if (any_infinite) {
    rray_mean_dbl_rescale_na_rm(
      v_x,
      v_out,
      v_corrections,
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

static r_obj* rray_mean_lgl_or_int(
  const int* v_x,
  int na_value,
  r_ssize out_size,
  r_ssize count,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
) {
  r_obj* sums = KEEP(r_alloc_raw0(out_size * sizeof(int64_t)));
  int64_t* v_sums = (int64_t*) r_raw_begin(sums);

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
        const bool na = x_elt == na_value;
        sum += na ? 0 : x_elt;
        any_na |= na;
      }

      v_sums[out_loc] = sum;
    } else {
      for (r_ssize i = start; i < end; ++i) {
        const int x_elt = v_x[i];
        const bool na = x_elt == na_value;
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
    rray_mean_lgl_or_int_propagate_na(
      v_x,
      na_value,
      v_out,
      v_dimensions,
      dimensionality,
      v_out_broadcast_strides
    );
  }

  FREE(2);
  return out;
}

static r_obj* rray_mean_lgl_or_int_na_rm(
  const int* v_x,
  int na_value,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
) {
  r_obj* sums = KEEP(r_alloc_raw0(out_size * sizeof(int64_t)));
  int64_t* v_sums = (int64_t*) r_raw_begin(sums);

  r_obj* counts = KEEP(r_alloc_raw0(out_size * sizeof(r_ssize)));
  r_ssize* v_counts = (r_ssize*) r_raw_begin(counts);

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
        const bool na = x_elt == na_value;
        sum += na ? 0 : x_elt;
        count += !na;
      }

      v_sums[out_loc] = sum;
      v_counts[out_loc] += count;
    } else {
      for (r_ssize i = start; i < end; ++i) {
        const int x_elt = v_x[i];
        const bool na = x_elt == na_value;
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

static void rray_mean_lgl_or_int_propagate_na(
  const int* v_x,
  int na_value,
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
      if (v_x[i] == na_value) {
        v_out[out_loc] = na_dbl;
      }
      out_loc += out_stride;
    }
  }
}

static r_obj* rray_mean_int_fallback(
  const int* v_x,
  r_ssize out_size,
  r_ssize count,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
) {
  const int na_int = r_globals.na_int;
  const double na_dbl = r_globals.na_dbl;

  r_obj* out = KEEP(r_alloc_double(out_size));
  double* v_out = r_dbl_begin(out);

  r_obj* corrections = KEEP(r_alloc_double(out_size));
  double* v_corrections = r_dbl_begin(corrections);

  for (r_ssize i = 0; i < out_size; ++i) {
    v_out[i] = 0.0;
    v_corrections[i] = 0.0;
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

  rray_run_iterator_reset1(&it);

  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
    const r_ssize start = rray_run_iterator_start(&it);
    const r_ssize end = rray_run_iterator_end(&it);

    r_ssize out_loc = rray_run_iterator_loc(&it, 0);
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

    if (out_stride == 0) {
      const double mean = v_out[out_loc];
      double correction = v_corrections[out_loc];

      for (r_ssize i = start; i < end; ++i) {
        correction += v_x[i] - mean;
      }

      v_corrections[out_loc] = correction;
    } else {
      for (r_ssize i = start; i < end; ++i) {
        v_corrections[out_loc] += v_x[i] - v_out[out_loc];
        out_loc += out_stride;
      }
    }
  }

  for (r_ssize i = 0; i < out_size; ++i) {
    const double mean = v_out[i];

    if (R_FINITE(mean)) {
      v_out[i] = mean + v_corrections[i] / count;
    }
  }

  FREE(2);
  return out;
}

static r_obj* rray_mean_int_na_rm_fallback(
  const int* v_x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
) {
  const int na_int = r_globals.na_int;

  r_obj* out = KEEP(r_alloc_double(out_size));
  double* v_out = r_dbl_begin(out);

  r_obj* corrections = KEEP(r_alloc_double(out_size));
  double* v_corrections = r_dbl_begin(corrections);

  for (r_ssize i = 0; i < out_size; ++i) {
    v_out[i] = 0.0;
    v_corrections[i] = 0.0;
  }

  r_obj* counts = KEEP(r_alloc_raw0(out_size * sizeof(r_ssize)));
  r_ssize* v_counts = (r_ssize*) r_raw_begin(counts);

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

  rray_run_iterator_reset1(&it);

  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
    const r_ssize start = rray_run_iterator_start(&it);
    const r_ssize end = rray_run_iterator_end(&it);

    r_ssize out_loc = rray_run_iterator_loc(&it, 0);
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

    if (out_stride == 0) {
      const double mean = v_out[out_loc];
      double correction = v_corrections[out_loc];

      for (r_ssize i = start; i < end; ++i) {
        const int x_elt = v_x[i];
        correction += x_elt == na_int ? 0 : x_elt - mean;
      }

      v_corrections[out_loc] = correction;
    } else {
      for (r_ssize i = start; i < end; ++i) {
        const int x_elt = v_x[i];
        v_corrections[out_loc] += x_elt == na_int ? 0 : x_elt - v_out[out_loc];
        out_loc += out_stride;
      }
    }
  }

  for (r_ssize i = 0; i < out_size; ++i) {
    const double mean = v_out[i];

    if (R_FINITE(mean)) {
      v_out[i] = mean + v_corrections[i] / v_counts[i];
    }
  }

  FREE(3);
  return out;
}

static void rray_mean_dbl_rescale(
  const double* v_x,
  double* v_out,
  double* v_corrections,
  r_ssize out_size,
  r_ssize count,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
) {
  r_obj* means = KEEP(r_alloc_double(out_size));
  double* v_means = r_dbl_begin(means);

  for (r_ssize i = 0; i < out_size; ++i) {
    v_means[i] = 0.0;
    v_corrections[i] = 0.0;
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
        v_means[out_loc] += v_x[i] / count;
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
        v_corrections[out_loc] += (v_x[i] - v_means[out_loc]) / count;
      }
      out_loc += out_stride;
    }
  }

  for (r_ssize i = 0; i < out_size; ++i) {
    if (isinf(v_out[i])) {
      const double mean = v_means[i];
      const double correction = v_corrections[i];
      v_out[i] =
        R_FINITE(mean) && R_FINITE(correction) ? mean + correction : mean;
    }
  }

  FREE(1);
}

static void rray_mean_dbl_rescale_na_rm(
  const double* v_x,
  double* v_out,
  double* v_corrections,
  const r_ssize* v_counts,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
) {
  r_obj* means = KEEP(r_alloc_double(out_size));
  double* v_means = r_dbl_begin(means);

  for (r_ssize i = 0; i < out_size; ++i) {
    v_means[i] = 0.0;
    v_corrections[i] = 0.0;
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
        v_means[out_loc] += x_elt / v_counts[out_loc];
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
        v_corrections[out_loc] +=
          (x_elt - v_means[out_loc]) / v_counts[out_loc];
      }
      out_loc += out_stride;
    }
  }

  for (r_ssize i = 0; i < out_size; ++i) {
    if (isinf(v_out[i])) {
      const double mean = v_means[i];
      const double correction = v_corrections[i];
      v_out[i] =
        R_FINITE(mean) && R_FINITE(correction) ? mean + correction : mean;
    }
  }

  FREE(1);
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

static inline r_ssize rray_mean_count(r_ssize x_size, r_ssize out_size) {
  return out_size == 0 ? 0 : x_size / out_size;
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
