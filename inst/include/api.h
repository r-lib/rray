#ifndef rray_api_h
#define rray_api_h

#include <Rcpp.h>

// -----------------------------------------------------------------------------
// Dimensions / dimensionality

Rcpp::IntegerVector rray_dim(Rcpp::RObject x);

Rcpp::IntegerVector rray_increase_dims(const Rcpp::IntegerVector& dim, const int& dims);

// -----------------------------------------------------------------------------
// Re-exposed R API

bool r_identical(SEXP x, SEXP y);

#endif
