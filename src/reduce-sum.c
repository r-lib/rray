#include "reduce-sum.h"

#include <stdint.h>

#include "int-128.h"
#include "reduce.h"
#include "type.h"
#include "utils.h"

#include "decl/reduce-sum-decl.h"

// Above an input length of 2^32 for a single output location, we can't
// guarantee that an `int` sum will fit in an `int64_t`. However, it is
// guaranteed to fit in an `int128_t`. So we fall back to a slower sum that
// relies on `int128_t` summing. That type isn't actually portable, so we mock
// our own minimal version.
#define RRAY_SUM_INT64_MAX_COUNT ((r_ssize) 1 << 32)

r_obj* ffi_rray_sum(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const bool na_rm = r_arg_as_bool(ffi_na_rm, "na_rm");
  return rray_sum(ffi_x, ffi_axes, na_rm, rray_args.x, error_call);
}

r_obj* rray_sum(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  return rray_reduce2(x, axes, na_rm, rray_sum_switch, arg, error_call);
}

static rray_reduce2_fn rray_sum_switch(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  switch (rray_typeof(x)) {
  case RRAY_TYPE_logical:
    return rray_sum_lgl;
  case RRAY_TYPE_integer:
    return rray_sum_int;
  case RRAY_TYPE_double:
    return rray_sum_dbl;
  case RRAY_TYPE_complex:
    return rray_sum_cpl;

  case RRAY_TYPE_character:
  case RRAY_TYPE_raw:
  case RRAY_TYPE_list:
    stop_unsupported_reduce("sum", x, arg, error_call);

  case RRAY_TYPE_scalar:
    stop_scalar_input(x, arg, error_call);
  }

  r_stop_unreachable();
}

static r_obj* rray_sum_lgl(
  r_obj* x,
  bool na_rm,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  const int* v_x = r_lgl_cbegin(x);

  return rray_sum_lgl_or_int(
    v_x,
    r_globals.na_lgl,
    na_rm,
    out_size,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides,
    error_call
  );
}

static r_obj* rray_sum_int(
  r_obj* x,
  bool na_rm,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  const int* v_x = r_int_cbegin(x);
  const r_ssize x_size = r_length(x);
  const r_ssize count = rray_sum_count(x_size, out_size);

  if (count > RRAY_SUM_INT64_MAX_COUNT) {
    return rray_sum_int_fallback(
      v_x,
      na_rm,
      out_size,
      v_dimensions,
      dimensionality,
      v_out_broadcast_strides,
      error_call
    );
  }

  return rray_sum_lgl_or_int(
    v_x,
    r_globals.na_int,
    na_rm,
    out_size,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides,
    error_call
  );
}

static r_obj* rray_sum_dbl(
  r_obj* x,
  bool na_rm,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  const double* v_x = r_dbl_cbegin(x);

  r_obj* out = KEEP(r_alloc_double(out_size));
  double* v_out = r_dbl_begin(out);

  for (r_ssize i = 0; i < out_size; ++i) {
    v_out[i] = 0.0;
  }

  struct rray_run_iterator it;
  rray_run_iterator_init1(
    &it,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );

  if (na_rm) {
    for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
      const r_ssize start = rray_run_iterator_start(&it);
      const r_ssize end = rray_run_iterator_end(&it);

      r_ssize out_loc = rray_run_iterator_loc(&it, 0);
      const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

      if (out_stride == 0) {
        double sum = v_out[out_loc];

        for (r_ssize i = start; i < end; ++i) {
          const double x_elt = v_x[i];
          sum += ISNAN(x_elt) ? 0 : x_elt;
        }

        v_out[out_loc] = sum;
      } else {
        for (r_ssize i = start; i < end; ++i) {
          const double x_elt = v_x[i];
          v_out[out_loc] += ISNAN(x_elt) ? 0 : x_elt;
          out_loc += out_stride;
        }
      }
    }
  } else {
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
  }

  FREE(1);
  return out;
}

static r_obj* rray_sum_cpl(
  r_obj* x,
  bool na_rm,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  const r_complex* v_x = r_cpl_cbegin(x);

  r_obj* out = KEEP(r_alloc_complex(out_size));
  r_complex* v_out = r_cpl_begin(out);

  for (r_ssize i = 0; i < out_size; ++i) {
    v_out[i] = (r_complex){.r = 0.0, .i = 0.0};
  }

  struct rray_run_iterator it;
  rray_run_iterator_init1(
    &it,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );

  if (na_rm) {
    for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
      const r_ssize start = rray_run_iterator_start(&it);
      const r_ssize end = rray_run_iterator_end(&it);

      r_ssize out_loc = rray_run_iterator_loc(&it, 0);
      const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

      if (out_stride == 0) {
        r_complex sum = v_out[out_loc];

        for (r_ssize i = start; i < end; ++i) {
          const r_complex x_elt = v_x[i];
          sum.r += ISNAN(x_elt.r) ? 0 : x_elt.r;
          sum.i += ISNAN(x_elt.i) ? 0 : x_elt.i;
        }

        v_out[out_loc] = sum;
      } else {
        for (r_ssize i = start; i < end; ++i) {
          const r_complex x_elt = v_x[i];
          const r_complex out_elt = v_out[out_loc];
          v_out[out_loc] = (r_complex){
            .r = out_elt.r + (ISNAN(x_elt.r) ? 0 : x_elt.r),
            .i = out_elt.i + (ISNAN(x_elt.i) ? 0 : x_elt.i),
          };
          out_loc += out_stride;
        }
      }
    }
  } else {
    for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
      const r_ssize start = rray_run_iterator_start(&it);
      const r_ssize end = rray_run_iterator_end(&it);

      r_ssize out_loc = rray_run_iterator_loc(&it, 0);
      const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

      if (out_stride == 0) {
        r_complex sum = v_out[out_loc];

        for (r_ssize i = start; i < end; ++i) {
          const r_complex x_elt = v_x[i];
          sum.r += x_elt.r;
          sum.i += x_elt.i;
        }

        v_out[out_loc] = sum;
      } else {
        for (r_ssize i = start; i < end; ++i) {
          const r_complex x_elt = v_x[i];
          const r_complex out_elt = v_out[out_loc];
          v_out[out_loc] = (r_complex){
            .r = out_elt.r + x_elt.r,
            .i = out_elt.i + x_elt.i,
          };
          out_loc += out_stride;
        }
      }
    }
  }

  FREE(1);
  return out;
}

static r_obj* rray_sum_lgl_or_int(
  const int* v_x,
  int na_value,
  bool na_rm,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  int n_prot = 0;

  r_obj* sums = KEEP_N(r_alloc_raw0(out_size * sizeof(int64_t)), &n_prot);
  int64_t* v_sums = (int64_t*) r_raw_begin(sums);

  bool* v_missings = NULL;

  if (!na_rm) {
    r_obj* missings = KEEP_N(r_alloc_raw0(out_size * sizeof(bool)), &n_prot);
    v_missings = (bool*) r_raw_begin(missings);
  }

  struct rray_run_iterator it;
  rray_run_iterator_init1(
    &it,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );

  if (na_rm) {
    for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
      const r_ssize start = rray_run_iterator_start(&it);
      const r_ssize end = rray_run_iterator_end(&it);

      r_ssize out_loc = rray_run_iterator_loc(&it, 0);
      const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

      if (out_stride == 0) {
        int64_t sum = v_sums[out_loc];

        for (r_ssize i = start; i < end; ++i) {
          const int x_elt = v_x[i];
          sum += x_elt == na_value ? 0 : x_elt;
        }

        v_sums[out_loc] = sum;
      } else {
        for (r_ssize i = start; i < end; ++i) {
          const int x_elt = v_x[i];
          v_sums[out_loc] += x_elt == na_value ? 0 : x_elt;
          out_loc += out_stride;
        }
      }
    }
  } else {
    for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
      const r_ssize start = rray_run_iterator_start(&it);
      const r_ssize end = rray_run_iterator_end(&it);

      r_ssize out_loc = rray_run_iterator_loc(&it, 0);
      const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

      if (out_stride == 0) {
        int64_t sum = v_sums[out_loc];
        bool missing = v_missings[out_loc];

        for (r_ssize i = start; i < end; ++i) {
          const int x_elt = v_x[i];
          const bool na = x_elt == na_value;
          sum += na ? 0 : x_elt;
          missing |= na;
        }

        v_sums[out_loc] = sum;
        v_missings[out_loc] = missing;
      } else {
        for (r_ssize i = start; i < end; ++i) {
          const int x_elt = v_x[i];
          const bool na = x_elt == na_value;
          v_sums[out_loc] += na ? 0 : x_elt;
          v_missings[out_loc] |= na;
          out_loc += out_stride;
        }
      }
    }
  }

  r_obj* out = KEEP_N(r_alloc_integer(out_size), &n_prot);
  int* v_out = r_int_begin(out);

  if (na_rm) {
    for (r_ssize i = 0; i < out_size; ++i) {
      const int64_t sum = v_sums[i];

      if (sum > INT_MAX || sum < -INT_MAX) {
        stop_int_overflow(error_call);
      }

      v_out[i] = (int) sum;
    }
  } else {
    for (r_ssize i = 0; i < out_size; ++i) {
      if (v_missings[i]) {
        v_out[i] = r_globals.na_int;
        continue;
      }

      const int64_t sum = v_sums[i];

      if (sum > INT_MAX || sum < -INT_MAX) {
        stop_int_overflow(error_call);
      }

      v_out[i] = (int) sum;
    }
  }

  FREE(n_prot);
  return out;
}

r_obj* ffi_test_rray_sum_forced_fallback(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const bool na_rm = r_arg_as_bool(ffi_na_rm, "na_rm");
  return rray_reduce2(
    ffi_x,
    ffi_axes,
    na_rm,
    rray_sum_forced_fallback_switch,
    rray_args.x,
    error_call
  );
}

static rray_reduce2_fn rray_sum_forced_fallback_switch(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  if (rray_typeof(x) != RRAY_TYPE_integer) {
    r_stop_internal("`x` must be an integer array.");
  }

  return rray_sum_int_forced_fallback;
}

static r_obj* rray_sum_int_forced_fallback(
  r_obj* x,
  bool na_rm,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  const int* v_x = r_int_cbegin(x);

  return rray_sum_int_fallback(
    v_x,
    na_rm,
    out_size,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides,
    error_call
  );
}

static r_obj* rray_sum_int_fallback(
  const int* v_x,
  bool na_rm,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  const int na_int = r_globals.na_int;

  int n_prot = 0;

  r_obj* sums =
    KEEP_N(r_alloc_raw0(out_size * sizeof(struct rray_int128)), &n_prot);
  struct rray_int128* v_sums = (struct rray_int128*) r_raw_begin(sums);

  bool* v_missings = NULL;

  if (!na_rm) {
    r_obj* missings = KEEP_N(r_alloc_raw0(out_size * sizeof(bool)), &n_prot);
    v_missings = (bool*) r_raw_begin(missings);
  }

  struct rray_run_iterator it;
  rray_run_iterator_init1(
    &it,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );

  if (na_rm) {
    for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
      const r_ssize start = rray_run_iterator_start(&it);
      const r_ssize end = rray_run_iterator_end(&it);

      r_ssize out_loc = rray_run_iterator_loc(&it, 0);
      const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

      if (out_stride == 0) {
        struct rray_int128 sum = v_sums[out_loc];

        for (r_ssize i = start; i < end; ++i) {
          const int x_elt = v_x[i];
          sum = rray_int128_add(sum, x_elt == na_int ? 0 : x_elt);
        }

        v_sums[out_loc] = sum;
      } else {
        for (r_ssize i = start; i < end; ++i) {
          const int x_elt = v_x[i];
          v_sums[out_loc] =
            rray_int128_add(v_sums[out_loc], x_elt == na_int ? 0 : x_elt);
          out_loc += out_stride;
        }
      }
    }
  } else {
    for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
      const r_ssize start = rray_run_iterator_start(&it);
      const r_ssize end = rray_run_iterator_end(&it);

      r_ssize out_loc = rray_run_iterator_loc(&it, 0);
      const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

      if (out_stride == 0) {
        struct rray_int128 sum = v_sums[out_loc];
        bool missing = v_missings[out_loc];

        for (r_ssize i = start; i < end; ++i) {
          const int x_elt = v_x[i];
          const bool na = x_elt == na_int;
          sum = rray_int128_add(sum, na ? 0 : x_elt);
          missing |= na;
        }

        v_sums[out_loc] = sum;
        v_missings[out_loc] = missing;
      } else {
        for (r_ssize i = start; i < end; ++i) {
          const int x_elt = v_x[i];
          const bool na = x_elt == na_int;
          v_sums[out_loc] = rray_int128_add(v_sums[out_loc], na ? 0 : x_elt);
          v_missings[out_loc] |= na;
          out_loc += out_stride;
        }
      }
    }
  }

  r_obj* out = KEEP_N(r_alloc_integer(out_size), &n_prot);
  int* v_out = r_int_begin(out);

  if (na_rm) {
    for (r_ssize i = 0; i < out_size; ++i) {
      v_out[i] = rray_int128_as_int(v_sums[i], error_call);
    }
  } else {
    for (r_ssize i = 0; i < out_size; ++i) {
      v_out[i] =
        v_missings[i] ? na_int : rray_int128_as_int(v_sums[i], error_call);
    }
  }

  FREE(n_prot);
  return out;
}

static inline r_ssize rray_sum_count(r_ssize x_size, r_ssize out_size) {
  return out_size == 0 ? 0 : x_size / out_size;
}
