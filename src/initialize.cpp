#include <Rcpp.h>

// utils.cpp
void rray_init_utils(SEXP ns);
void rray_init_cast(SEXP ns);
void rray_init_type2(SEXP ns);

// [[Rcpp::export]]
SEXP rray_init(SEXP ns) {
  rray_init_utils(ns);
  rray_init_cast(ns);
  rray_init_type2(ns);
  return R_NilValue;
}
