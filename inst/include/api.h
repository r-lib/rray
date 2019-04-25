#ifndef rray_api_h
#define rray_api_h

#include <Rcpp.h>

// -----------------------------------------------------------------------------
// Dimensions / dimensionality

Rcpp::IntegerVector rray__dim(const Rcpp::RObject& x);

int rray__dims(const Rcpp::RObject& x);

Rcpp::IntegerVector rray__increase_dims(const Rcpp::IntegerVector& dim,
                                        const int& dims);

// -----------------------------------------------------------------------------
// Miscellaneous

int rray__prod(Rcpp::IntegerVector x);

// -----------------------------------------------------------------------------
// Validation

void rray__validate_dim(Rcpp::IntegerVector dim);

void rray__validate_reshape(Rcpp::RObject x, Rcpp::IntegerVector dim);

// -----------------------------------------------------------------------------
// Re-exposed R API

bool r_identical(SEXP x, SEXP y);

bool r_is_null(SEXP x);

bool r_is_missing(SEXP x);

#endif
