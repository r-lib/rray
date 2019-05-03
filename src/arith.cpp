#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<T> rray__add_impl(const xt::rarray<T>& x, const xt::rarray<T>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));

  // If any dimension is size 0, return an empty logical array with common dim
  if (Rcpp::is_true(Rcpp::any(dim == 0))) {
    xt::rarray<T> res(Rcpp::as<std::vector<std::size_t>>(dim));
    return(res);
  }

  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return x_view + y_view;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__add(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__add_impl, x, y);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<T> rray__subtract_impl(const xt::rarray<T>& x, const xt::rarray<T>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));

  // If any dimension is size 0, return an empty logical array with common dim
  if (Rcpp::is_true(Rcpp::any(dim == 0))) {
    xt::rarray<T> res(Rcpp::as<std::vector<std::size_t>>(dim));
    return(res);
  }

  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return x_view - y_view;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__subtract(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__subtract_impl, x, y);
}

// -----------------------------------------------------------------------------

// Should always take and return a numeric result

xt::rarray<double> rray__divide_impl(const xt::rarray<double>& x, const xt::rarray<double>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));

  // If any dimension is size 0, return an empty logical array with common dim
  if (Rcpp::is_true(Rcpp::any(dim == 0))) {
    xt::rarray<double> res(Rcpp::as<std::vector<std::size_t>>(dim));
    return(res);
  }

  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return x_view / y_view;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__divide(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__divide_impl, x, y);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<T> rray__multiply_impl(const xt::rarray<T>& x, const xt::rarray<T>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));

  // If any dimension is size 0, return an empty logical array with common dim
  if (Rcpp::is_true(Rcpp::any(dim == 0))) {
    xt::rarray<T> res(Rcpp::as<std::vector<std::size_t>>(dim));
    return(res);
  }

  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return x_view * y_view;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__multiply(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__multiply_impl, x, y);
}

// -----------------------------------------------------------------------------

// TODO somehow use the faster template version? pow<N>(x)

template <typename T>
xt::rarray<T> rray__pow_impl(const xt::rarray<T>& x, const xt::rarray<T>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));

  // If any dimension is size 0, return an empty logical array with common dim
  if (Rcpp::is_true(Rcpp::any(dim == 0))) {
    xt::rarray<T> res(Rcpp::as<std::vector<std::size_t>>(dim));
    return(res);
  }

  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return xt::pow(x_view, y_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__pow(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__pow_impl, x, y);
}

// -----------------------------------------------------------------------------

// c++ modulus ONLY works with integers. see fmod() as well for the floating
// point version

xt::rarray<int> rray__modulus_impl(const xt::rarray<int>& x, const xt::rarray<int>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));

  // If any dimension is size 0, return an empty logical array with common dim
  if (Rcpp::is_true(Rcpp::any(dim == 0))) {
    xt::rarray<int> res(Rcpp::as<std::vector<std::size_t>>(dim));
    return(res);
  }

  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return x_view % y_view;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__modulus(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__modulus_impl, x, y);
}

// -----------------------------------------------------------------------------

// Logicals need to be coerced to integers so we need to dispatch manually
// with a cast to int if it is a logical

template <typename T>
xt::rarray<T> rray__identity_impl(const xt::rarray<T>& x) {
  return xt::operator+(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__identity(Rcpp::RObject x) {

  if (Rf_isNull(x)) {
    return Rcpp::as<Rcpp::RObject>(R_NilValue);
  }

  int x_type = TYPEOF(x);

  if (x_type == REALSXP) {
    return Rcpp::as<Rcpp::RObject>(
      rray__identity_impl(xt::rarray<double>(x))
    );
  }
  else if (x_type == INTSXP) {
    return Rcpp::as<Rcpp::RObject>(
      rray__identity_impl(xt::rarray<int>(x))
    );
  }
  else if (x_type == LGLSXP) {

    // Coerce logical to integer!
    xt::rarray<rlogical> x_logical(x);
    xt::rarray<int> x_int = xt::cast<int>(x_logical);

    return Rcpp::as<Rcpp::RObject>(
      rray__identity_impl(x_int)
    );
  }

  error_unknown_type();
}

// -----------------------------------------------------------------------------

// Logicals need to be coerced to integers so we need to dispatch manually
// with a cast to int if it is a logical

template <typename T>
xt::rarray<T> rray__opposite_impl(const xt::rarray<T>& x) {
  return xt::operator-(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__opposite(Rcpp::RObject x) {

  if (Rf_isNull(x)) {
    return Rcpp::as<Rcpp::RObject>(R_NilValue);
  }

  int x_type = TYPEOF(x);

  if (x_type == REALSXP) {
    return Rcpp::as<Rcpp::RObject>(
      rray__opposite_impl(xt::rarray<double>(x))
    );
  }
  else if (x_type == INTSXP) {
    return Rcpp::as<Rcpp::RObject>(
      rray__opposite_impl(xt::rarray<int>(x))
    );
  }
  else if (x_type == LGLSXP) {

    // Coerce logical to integer!
    xt::rarray<rlogical> x_logical(x);
    xt::rarray<int> x_int = xt::cast<int>(x_logical);

    return Rcpp::as<Rcpp::RObject>(
      rray__opposite_impl(x_int)
    );
  }

  error_unknown_type();
}
