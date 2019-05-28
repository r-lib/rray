#ifndef rray_utils_h
#define rray_utils_h

// Required for SEXP typename
#include <Rcpp.h>

extern SEXP rray_ns_env;

extern SEXP syms_x;
extern SEXP syms_y;
extern SEXP syms_to;

extern SEXP fns_vec_cast_inner;
extern SEXP fns_vec_type_inner2;

SEXP r_new_environment(SEXP parent, R_len_t size);

#endif
