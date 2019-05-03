#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::List rray__split_impl(const xt::rarray<T>& x,
                            std::size_t n,
                            std::size_t axis) {

  auto res = xt::split(x, n, axis);

  Rcpp::List out(n);

  // `res` is a vector of strided view
  // we have to cast each element to an rarray before
  // assigning into `out`
  for (int i = 0; i < n; ++i) {
    xt::rarray<T> res_i = res[i];
    out[i] = res_i;
  }

  return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__split(Rcpp::RObject x, std::size_t n, std::size_t axis) {
  DISPATCH_UNARY_TWO(rray__split_impl, x, n, axis);
}

// -----------------------------------------------------------------------------
