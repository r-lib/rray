#ifndef rray_dispatch_h
#define rray_dispatch_h

#include <rray.h>
#include <tools/tools.h>

// -----------------------------------------------------------------------------
// Unary + 1 argument

#define DISPATCH_UNARY_ONE(FUN, X, ARG)                        \
  if (Rf_isNull(X)) {                                          \
    return Rcpp::as<Rcpp::RObject>(R_NilValue);                \
  }                                                            \
                                                               \
  int x_type = TYPEOF(X);                                      \
                                                               \
  if (x_type == REALSXP) {                                     \
    return Rcpp::as<Rcpp::RObject>(                            \
      FUN(xt::rarray<double>(X), ARG)                          \
    );                                                         \
  }                                                            \
  else if (x_type == INTSXP) {                                 \
    return Rcpp::as<Rcpp::RObject>(                            \
      FUN(xt::rarray<int>(X), ARG)                             \
    );                                                         \
  }                                                            \
  else if (x_type == LGLSXP) {                                 \
    return Rcpp::as<Rcpp::RObject>(                            \
      FUN(xt::rarray<rlogical>(X), ARG)                        \
    );                                                         \
  }                                                            \
                                                               \
  error_unknown_type()

// -----------------------------------------------------------------------------
// Unary + 1 argument

#define DISPATCH_UNARY_TWO(FUN, X, ARG_1, ARG_2)                 \
  if (Rf_isNull(X)) {                                            \
    return Rcpp::as<Rcpp::RObject>(R_NilValue);                  \
  }                                                              \
                                                                 \
  int x_type = TYPEOF(X);                                        \
                                                                 \
  if (x_type == REALSXP) {                                       \
    return Rcpp::as<Rcpp::RObject>(                              \
      FUN(xt::rarray<double>(X), ARG_1, ARG_2)                   \
    );                                                           \
  }                                                              \
  else if (x_type == INTSXP) {                                   \
    return Rcpp::as<Rcpp::RObject>(                              \
      FUN(xt::rarray<int>(X), ARG_1, ARG_2)                      \
    );                                                           \
  }                                                              \
  else if (x_type == LGLSXP) {                                   \
    return Rcpp::as<Rcpp::RObject>(                              \
      FUN(xt::rarray<rlogical>(X), ARG_1, ARG_2)                 \
    );                                                           \
  }                                                              \
                                                                 \
  error_unknown_type()

// -----------------------------------------------------------------------------

#endif
