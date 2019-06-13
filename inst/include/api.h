#ifndef rray_api_h
#define rray_api_h

#include <Rcpp.h>

// -----------------------------------------------------------------------------
// Dimensions / dimensionality

Rcpp::IntegerVector rray__dim(const Rcpp::RObject& x);

Rcpp::IntegerVector rray__dim2(Rcpp::IntegerVector x_dim,
                               Rcpp::IntegerVector y_dim);

int rray__dim_n(const Rcpp::RObject& x);

Rcpp::IntegerVector rray__increase_dims(const Rcpp::IntegerVector& dim,
                                        const int& dim_n);

bool rray__has_dim(const Rcpp::RObject& x);

// -----------------------------------------------------------------------------
// Dimension names

Rcpp::List rray__new_empty_dim_names(int n);

Rcpp::List rray__dim_names(const Rcpp::RObject& x);

// -----------------------------------------------------------------------------
// Common dimension names

Rcpp::List rray__resize_dim_names(Rcpp::List dim_names,
                                  Rcpp::IntegerVector dim);

Rcpp::List rray__coalesce_dim_names(Rcpp::List x_dim_names,
                                    Rcpp::List y_dim_names);

Rcpp::List rray__dim_names2(Rcpp::RObject x, Rcpp::RObject y);

void rray__resize_and_set_dim_names(Rcpp::RObject res, Rcpp::RObject x);

void rray__set_dim_names(Rcpp::RObject x, const Rcpp::List& dim_names);

// -----------------------------------------------------------------------------
// Miscellaneous

int rray__prod(Rcpp::IntegerVector x);

// -----------------------------------------------------------------------------
// Validation

void rray__validate_dim(Rcpp::IntegerVector dim);

void rray__validate_reshape(Rcpp::RObject x, Rcpp::IntegerVector dim);

void rray__validate_broadcastable_to_dim(Rcpp::IntegerVector x_dim,
                                         Rcpp::IntegerVector dim);

std::string rray__dim_to_string(Rcpp::IntegerVector dim);

// -----------------------------------------------------------------------------
// Re-exposed R API

#include <r-api.h>

#endif
