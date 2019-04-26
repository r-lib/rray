#ifndef rray_r_api_h
#define rray_r_api_h

// Required for SEXP typename and R_MissingArg to work
#include <Rcpp.h>

bool r_identical(SEXP x, SEXP y);

bool r_is_null(SEXP x);

bool r_is_missing(SEXP x);

#endif
