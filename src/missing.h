#ifndef RRAY_MISSING_H
#define RRAY_MISSING_H

#include "rlang.h"

#include "utils.h"

static inline bool rray_lgl_is_missing(int x) {
  return x == r_globals.na_lgl;
}

static inline bool rray_int_is_missing(int x) {
  return x == r_globals.na_int;
}

static inline bool rray_dbl_is_missing(double x) {
  return ISNAN(x);
}

static inline bool rray_cpl_is_missing(r_complex x) {
  // Purposefully bitwise, can help compiler perform vectorization
  return bool_bitwise_or(ISNAN(x.r), ISNAN(x.i));
}

// -----------------------------------------------------------------------------

#ifdef WORDS_BIGENDIAN
static const int rray_dbl_indicator_pos = 1;
#else
static const int rray_dbl_indicator_pos = 0;
#endif

union rray_dbl_indicator {
  double value;
  unsigned int key[2];
};

enum rray_dbl_class {
  RRAY_DBL_number,
  RRAY_DBL_missing,
  RRAY_DBL_nan
};

static inline enum rray_dbl_class rray_dbl_classify(double x) {
  if (!isnan(x)) {
    return RRAY_DBL_number;
  }

  union rray_dbl_indicator indicator;
  indicator.value = x;

  if (indicator.key[rray_dbl_indicator_pos] == 1954) {
    return RRAY_DBL_missing;
  } else {
    return RRAY_DBL_nan;
  }
}

#endif
