#ifndef rray_errors_h
#define rray_errors_h

#include <Rcpp.h>

[[ noreturn ]] inline void error_unknown_type() {
  Rcpp::stop("Incompatible SEXP encountered; only accepts doubles, integers, and logicals.");
}

#endif
