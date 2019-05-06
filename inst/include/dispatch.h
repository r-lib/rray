#ifndef rray_dispatch_h
#define rray_dispatch_h

#include <rray.h>
#include <tools/tools.h>

// -----------------------------------------------------------------------------
// Unary + 1 argument

#define DISPATCH_UNARY(FUN, X)                                 \
  if (Rf_isNull(X)) {                                          \
    return Rcpp::as<Rcpp::RObject>(R_NilValue);                \
  }                                                            \
                                                               \
  int x_type = TYPEOF(X);                                      \
                                                               \
  if (x_type == REALSXP) {                                     \
    return Rcpp::as<Rcpp::RObject>(                            \
      FUN(xt::rarray<double>(X))                               \
    );                                                         \
  }                                                            \
  else if (x_type == INTSXP) {                                 \
    return Rcpp::as<Rcpp::RObject>(                            \
      FUN(xt::rarray<int>(X))                                  \
    );                                                         \
  }                                                            \
  else if (x_type == LGLSXP) {                                 \
    return Rcpp::as<Rcpp::RObject>(                            \
      FUN(xt::rarray<rlogical>(X))                             \
    );                                                         \
  }                                                            \
                                                               \
  error_unknown_type()                                         \

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
// Unary + 2 argument

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
// Unary + 3 argument

#define DISPATCH_UNARY_THREE(FUN, X, ARG_1, ARG_2, ARG_3)        \
  if (Rf_isNull(X)) {                                            \
    return Rcpp::as<Rcpp::RObject>(R_NilValue);                  \
  }                                                              \
                                                                 \
  int x_type = TYPEOF(X);                                        \
                                                                 \
  if (x_type == REALSXP) {                                       \
    return Rcpp::as<Rcpp::RObject>(                              \
      FUN(xt::rarray<double>(X), ARG_1, ARG_2, ARG_3)            \
    );                                                           \
  }                                                              \
  else if (x_type == INTSXP) {                                   \
    return Rcpp::as<Rcpp::RObject>(                              \
      FUN(xt::rarray<int>(X), ARG_1, ARG_2, ARG_3)               \
    );                                                           \
  }                                                              \
  else if (x_type == LGLSXP) {                                   \
    return Rcpp::as<Rcpp::RObject>(                              \
      FUN(xt::rarray<rlogical>(X), ARG_1, ARG_2, ARG_3)          \
    );                                                           \
  }                                                              \
                                                                 \
  error_unknown_type()

// -----------------------------------------------------------------------------
// Binary + 0 argument

#define DISPATCH_BINARY(FUN, X, Y)                             \
  if (Rf_isNull(X) || Rf_isNull(Y)) {                          \
    return Rcpp::as<Rcpp::RObject>(R_NilValue);                \
  }                                                            \
                                                               \
  int x_type = TYPEOF(X);                                      \
  int y_type = TYPEOF(Y);                                      \
                                                               \
  if (x_type != y_type) {                                      \
    Rcpp::stop("`x` and `y` must have the same type.");        \
  }                                                            \
                                                               \
  if (x_type == REALSXP) {                                     \
    return Rcpp::as<Rcpp::RObject>(                            \
      FUN(xt::rarray<double>(X), xt::rarray<double>(Y))        \
    );                                                         \
  }                                                            \
  else if (x_type == INTSXP) {                                 \
    return Rcpp::as<Rcpp::RObject>(                            \
      FUN(xt::rarray<int>(X), xt::rarray<int>(Y))              \
    );                                                         \
  }                                                            \
  else if (x_type == LGLSXP) {                                 \
    return Rcpp::as<Rcpp::RObject>(                            \
      FUN(xt::rarray<rlogical>(X), xt::rarray<rlogical>(Y))    \
    );                                                         \
  }                                                            \
                                                               \
  error_unknown_type()

// -----------------------------------------------------------------------------
// Binary + 1 argument

#define DISPATCH_BINARY_ONE(FUN, X, Y, ARG)                      \
  if (Rf_isNull(X) || Rf_isNull(Y)) {                            \
    return Rcpp::as<Rcpp::RObject>(R_NilValue);                  \
  }                                                              \
                                                                 \
  int x_type = TYPEOF(X);                                        \
  int y_type = TYPEOF(Y);                                        \
                                                                 \
  if (x_type != y_type) {                                        \
    Rcpp::stop("`x` and `y` must have the same type.");          \
  }                                                              \
                                                                 \
  if (x_type == REALSXP) {                                       \
    return Rcpp::as<Rcpp::RObject>(                              \
      FUN(xt::rarray<double>(X), xt::rarray<double>(Y), ARG)     \
    );                                                           \
  }                                                              \
  else if (x_type == INTSXP) {                                   \
    return Rcpp::as<Rcpp::RObject>(                              \
      FUN(xt::rarray<int>(X), xt::rarray<int>(Y), ARG)           \
    );                                                           \
  }                                                              \
  else if (x_type == LGLSXP) {                                   \
    return Rcpp::as<Rcpp::RObject>(                              \
      FUN(xt::rarray<rlogical>(X), xt::rarray<rlogical>(Y), ARG) \
    );                                                           \
  }                                                              \
                                                                 \
  error_unknown_type()                                           \

// -----------------------------------------------------------------------------
// Trinary + 0 argument + logicals converted to integer

#define DISPATCH_TRINARY_NO_LOGICAL(FUN, X, Y, Z)                     \
  if (Rf_isNull(X) || Rf_isNull(Y) || Rf_isNull(Z)) {                 \
    return Rcpp::as<Rcpp::RObject>(R_NilValue);                       \
  }                                                                   \
                                                                      \
  int x_type = TYPEOF(X);                                             \
  int y_type = TYPEOF(Y);                                             \
  int z_type = TYPEOF(Z);                                             \
                                                                      \
  if (x_type != y_type || x_type != z_type) {                         \
    Rcpp::stop("`x`, `y`, and `z` must have the same type.");         \
  }                                                                   \
                                                                      \
  if (x_type == REALSXP) {                                            \
    return Rcpp::as<Rcpp::RObject>(                                   \
      FUN(                                                            \
        xt::rarray<double>(X),                                        \
        xt::rarray<double>(Y),                                        \
        xt::rarray<double>(Z)                                         \
      )                                                               \
    );                                                                \
  }                                                                   \
  else if (x_type == INTSXP) {                                        \
    return Rcpp::as<Rcpp::RObject>(                                   \
      FUN(                                                            \
        xt::rarray<int>(X),                                           \
        xt::rarray<int>(Y),                                           \
        xt::rarray<int>(Z)                                            \
      )                                                               \
    );                                                                \
  }                                                                   \
  else if (x_type == LGLSXP) {                                        \
    xt::rarray<int> X_int = xt::cast<int>(xt::rarray<rlogical>(X));   \
    xt::rarray<int> Y_int = xt::cast<int>(xt::rarray<rlogical>(Y));   \
    xt::rarray<int> Z_int = xt::cast<int>(xt::rarray<rlogical>(Z));   \
    return Rcpp::as<Rcpp::RObject>(                                   \
      FUN(                                                            \
        X_int,                                                        \
        Y_int,                                                        \
        Z_int                                                         \
      )                                                               \
    );                                                                \
  }                                                                   \
                                                                      \
  error_unknown_type()

// -----------------------------------------------------------------------------

#endif
