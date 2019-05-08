#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------

// TODO - handle any integer overflow somehow?

template <typename T>
xt::rarray<T> rray__multiply_add_impl(const xt::rarray<T>& x,
                                      const xt::rarray<T>& y,
                                      const xt::rarray<T>& z) {

  // Common dim
  Rcpp::IntegerVector tmp_dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  Rcpp::IntegerVector dim = rray__dim2(tmp_dim, rray__dim(SEXP(z)));

  // If any dimension is size 0, return an empty T array with common dim
  if (Rcpp::is_true(Rcpp::any(dim == 0))) {
    xt::rarray<T> res(Rcpp::as<std::vector<std::size_t>>(dim));
    return(res);
  }

  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);
  auto z_view = rray__increase_dims_view(z, dims);

  return xt::fma(x_view, y_view, z_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__multiply_add(Rcpp::RObject x, Rcpp::RObject y, Rcpp::RObject z) {
  DISPATCH_TRINARY_NO_LOGICAL(rray__multiply_add_impl, x, y, z);
}

// -----------------------------------------------------------------------------

// logicals return integers. this does so with only 1 allocation (so does base R)

template <typename T>
Rcpp::RObject rray__abs_impl(const xt::rarray<T>& x) {
  xt::rarray<T> res = xt::abs(x);
  return Rcpp::as<Rcpp::RObject>(res);
}

Rcpp::RObject rray__abs_impl(const xt::rarray<rlogical>& x) {
  xt::rarray<int> res = xt::abs(x);
  return Rcpp::as<Rcpp::RObject>(res);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__abs(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__abs_impl, x);
}

// -----------------------------------------------------------------------------

// - for doubles, this should return a double, not an integer.
// this is so that `rray_sign(NaN)` correctly returns `NaN`
// - for ints, we can return an int. base R does not.
//   e.g. `storage.mode(sign(1L))`

template <typename T>
Rcpp::RObject rray__sign_impl(const xt::rarray<T>& x) {
  xt::rarray<T> res = xt::sign(x);
  return Rcpp::as<Rcpp::RObject>(res);
}

Rcpp::RObject rray__sign_impl(const xt::rarray<rlogical>& x) {
  xt::rarray<int> res = xt::sign(x);
  return Rcpp::as<Rcpp::RObject>(res);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__sign(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__sign_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__fmod_impl(const xt::rarray<T>& x,
                                   const xt::rarray<T>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));

  // If any dimension is size 0, return an empty double array with common dim
  if (Rcpp::is_true(Rcpp::any(dim == 0))) {
    xt::rarray<double> res(Rcpp::as<std::vector<std::size_t>>(dim));
    return(res);
  }

  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return xt::fmod(x_view, y_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__fmod(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__fmod_impl, x, y);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__remainder_impl(const xt::rarray<T>& x,
                                        const xt::rarray<T>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));

  // If any dimension is size 0, return an empty double array with common dim
  if (Rcpp::is_true(Rcpp::any(dim == 0))) {
    xt::rarray<double> res(Rcpp::as<std::vector<std::size_t>>(dim));
    return(res);
  }

  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return xt::remainder(x_view, y_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__remainder(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__remainder_impl, x, y);
}


