#ifndef rray_dispatch_h
#define rray_dispatch_h

#include <rray.h>
#include <tools/tools.h>

// -----------------------------------------------------------------------------
// Trinary + 0 argument + logicals converted to integer

#define DISPATCH_TRINARY_NO_LOGICAL(OUT, FUN, X, Y, Z)                \
  if (Rf_isNull(X) || Rf_isNull(Y) || Rf_isNull(Z)) {                 \
    return Rcpp::as<Rcpp::RObject>(R_NilValue);                       \
  }                                                                   \
                                                                      \
  int x_type = TYPEOF(X);                                             \
                                                                      \
  if (x_type == REALSXP) {                                            \
    OUT = FUN(                                                        \
      xt::rarray<double>(X),                                          \
      xt::rarray<double>(Y),                                          \
      xt::rarray<double>(Z)                                           \
    );                                                                \
  }                                                                   \
  else if (x_type == INTSXP) {                                        \
    OUT = FUN(                                                        \
      xt::rarray<int>(X),                                             \
      xt::rarray<int>(Y),                                             \
      xt::rarray<int>(Z)                                              \
    );                                                                \
  }                                                                   \
  else if (x_type == LGLSXP) {                                        \
    xt::rarray<int> X_int = xt::cast<int>(xt::rarray<rlogical>(X));   \
    xt::rarray<int> Y_int = xt::cast<int>(xt::rarray<rlogical>(Y));   \
    xt::rarray<int> Z_int = xt::cast<int>(xt::rarray<rlogical>(Z));   \
    OUT = FUN(                                                        \
      X_int,                                                          \
      Y_int,                                                          \
      Z_int                                                           \
    );                                                                \
  }                                                                   \
  else {                                                              \
    error_unknown_type();                                             \
  }

// -----------------------------------------------------------------------------

#define DISPATCH_BINARY(OUT, FUN, X, Y)                          \
  int x_type = TYPEOF(X);                                        \
                                                                 \
  if (x_type == REALSXP) {                                       \
    OUT = FUN(                                                   \
      xt::rarray<double>(X),                                     \
      xt::rarray<double>(Y)                                      \
    );                                                           \
  }                                                              \
  else if (x_type == INTSXP) {                                   \
    OUT = FUN(                                                   \
      xt::rarray<int>(X),                                        \
      xt::rarray<int>(Y)                                         \
    );                                                           \
  }                                                              \
  else if (x_type == LGLSXP) {                                   \
    OUT = FUN(                                                   \
      xt::rarray<rlogical>(X),                                   \
      xt::rarray<rlogical>(Y)                                    \
    );                                                           \
  }                                                              \
  else {                                                         \
    error_unknown_type();                                        \
  }

// -----------------------------------------------------------------------------

#define DISPATCH_BINARY_ONE(OUT, FUN, X, Y, ARG)               \
  int x_type = TYPEOF(X);                                      \
                                                               \
  if (x_type == REALSXP) {                                     \
    OUT = FUN(                                                 \
      xt::rarray<double>(X),                                   \
      xt::rarray<double>(Y),                                   \
      ARG                                                      \
    );                                                         \
  }                                                            \
  else if (x_type == INTSXP) {                                 \
    OUT = FUN(                                                 \
      xt::rarray<int>(X),                                      \
      xt::rarray<int>(Y),                                      \
      ARG                                                      \
    );                                                         \
  }                                                            \
  else if (x_type == LGLSXP) {                                 \
    OUT = FUN(                                                 \
      xt::rarray<rlogical>(X),                                 \
      xt::rarray<rlogical>(Y),                                 \
      ARG                                                      \
    );                                                         \
  }                                                            \
  else {                                                       \
    error_unknown_type();                                      \
  }

// -----------------------------------------------------------------------------

#define DISPATCH_BINARY_TWO(OUT, FUN, X, Y, ARG1, ARG2)          \
  int x_type = TYPEOF(X);                                        \
                                                                 \
  if (x_type == REALSXP) {                                       \
    OUT = FUN(                                                   \
      xt::rarray<double>(X),                                     \
      xt::rarray<double>(Y),                                     \
      ARG1, ARG2                                                 \
    );                                                           \
  }                                                              \
  else if (x_type == INTSXP) {                                   \
    OUT = FUN(                                                   \
      xt::rarray<int>(X),                                        \
      xt::rarray<int>(Y),                                        \
      ARG1, ARG2                                                 \
    );                                                           \
  }                                                              \
  else if (x_type == LGLSXP) {                                   \
    OUT = FUN(                                                   \
      xt::rarray<rlogical>(X),                                   \
      xt::rarray<rlogical>(Y),                                   \
      ARG1, ARG2                                                 \
    );                                                           \
  }                                                              \
  else {                                                         \
    error_unknown_type();                                        \
  }


// -----------------------------------------------------------------------------

#define DISPATCH_UNARY(OUT, FUN, X)                                \
  int x_type = TYPEOF(X);                                          \
                                                                   \
  if (x_type == REALSXP) {                                         \
    OUT = FUN(                                                     \
      xt::rarray<double>(X)                                        \
    );                                                             \
  }                                                                \
  else if (x_type == INTSXP) {                                     \
    OUT = FUN(                                                     \
      xt::rarray<int>(X)                                           \
    );                                                             \
  }                                                                \
  else if (x_type == LGLSXP) {                                     \
    OUT = FUN(                                                     \
      xt::rarray<rlogical>(X)                                      \
    );                                                             \
  }                                                                \
  else {                                                           \
    error_unknown_type();                                          \
  }

// -----------------------------------------------------------------------------

#define DISPATCH_UNARY_ONE(OUT, FUN, X, ARG)                      \
  int x_type = TYPEOF(X);                                         \
                                                                  \
  if (x_type == REALSXP) {                                        \
    OUT = FUN(                                                    \
      xt::rarray<double>(X),                                      \
      ARG                                                         \
    );                                                            \
  }                                                               \
  else if (x_type == INTSXP) {                                    \
    OUT = FUN(                                                    \
      xt::rarray<int>(X),                                         \
      ARG                                                         \
    );                                                            \
  }                                                               \
  else if (x_type == LGLSXP) {                                    \
    OUT = FUN(                                                    \
      xt::rarray<rlogical>(X),                                    \
      ARG                                                         \
    );                                                            \
  }                                                               \
  else {                                                          \
    error_unknown_type();                                         \
  }

// -----------------------------------------------------------------------------

#define DISPATCH_UNARY_TWO(OUT, FUN, X, ARG1, ARG2)                    \
  int x_type = TYPEOF(X);                                              \
                                                                       \
  if (x_type == REALSXP) {                                             \
    OUT = FUN(                                                         \
      xt::rarray<double>(X),                                           \
      ARG1, ARG2                                                       \
    );                                                                 \
  }                                                                    \
  else if (x_type == INTSXP) {                                         \
    OUT = FUN(                                                         \
      xt::rarray<int>(X),                                              \
      ARG1, ARG2                                                       \
    );                                                                 \
  }                                                                    \
  else if (x_type == LGLSXP) {                                         \
    OUT = FUN(                                                         \
      xt::rarray<rlogical>(X),                                         \
      ARG1, ARG2                                                       \
    );                                                                 \
  }                                                                    \
  else {                                                               \
    error_unknown_type();                                              \
  }

// -----------------------------------------------------------------------------

#define DISPATCH_UNARY_THREE(OUT, FUN, X, ARG1, ARG2, ARG3)          \
  int x_type = TYPEOF(X);                                            \
                                                                     \
  if (x_type == REALSXP) {                                           \
    OUT = FUN(                                                       \
      xt::rarray<double>(X),                                         \
      ARG1, ARG2, ARG3                                               \
    );                                                               \
  }                                                                  \
  else if (x_type == INTSXP) {                                       \
    OUT = FUN(                                                       \
      xt::rarray<int>(X),                                            \
      ARG1, ARG2, ARG3                                               \
    );                                                               \
  }                                                                  \
  else if (x_type == LGLSXP) {                                       \
    OUT = FUN(                                                       \
      xt::rarray<rlogical>(X),                                       \
      ARG1, ARG2, ARG3                                               \
    );                                                               \
  }                                                                  \
  else {                                                             \
    error_unknown_type();                                            \
  }

// -----------------------------------------------------------------------------

#define DISPATCH_UNARY_MATH(FUN, X)                              \
  if (r_is_null(X)) {                                            \
    return X;                                                    \
  }                                                              \
                                                                 \
  Rcpp::RObject out;                                             \
  DISPATCH_UNARY(out, FUN, X);                                   \
                                                                 \
  rray__set_dim_names(out, rray__dim_names(X));                  \
  return out

// -----------------------------------------------------------------------------

#define DISPATCH_BINARY_MATH(FUN, X, Y)                          \
  if (r_is_null(X) || r_is_null(Y)) {                            \
    return R_NilValue;                                           \
  }                                                              \
                                                                 \
  Rcpp::List new_dim_names = rray__dim_names2(X, Y);             \
                                                                 \
  Rcpp::RObject type = vec__ptype_inner2(X, Y);                  \
  X = vec__cast_inner(X, type);                                  \
  Y = vec__cast_inner(Y, type);                                  \
                                                                 \
  Rcpp::RObject out;                                             \
  DISPATCH_BINARY(out, FUN, X, Y);                               \
                                                                 \
  rray__set_dim_names(out, new_dim_names);                       \
  return out

// -----------------------------------------------------------------------------

#endif
