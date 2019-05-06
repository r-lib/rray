#include <rray.h>
#include <dispatch.h>

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
