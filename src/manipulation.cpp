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

template <typename T>
xt::rarray<T> rray__rotate_impl(const xt::rarray<T>& x,
                                std::ptrdiff_t from,
                                std::ptrdiff_t to,
                                int n) {

  // Axes
  std::array<std::ptrdiff_t, 2> axes = {from, to};

  if (n == 1) {
    xt::rarray<T> res = xt::rot90<1>(x, axes);
    return res;
  }
  else if (n == 2) {
    xt::rarray<T> res = xt::rot90<2>(x, axes);
    return res;
  }
  else if (n == 3) {
    xt::rarray<T> res = xt::rot90<3>(x, axes);
    return res;
  }
  else {
    Rcpp::stop("`n` must be 1, 2, or 3.");
  }

}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__rotate(Rcpp::RObject x, std::ptrdiff_t from, std::ptrdiff_t to, int n) {
  DISPATCH_UNARY_THREE(rray__rotate_impl, x, from, to, n);
}

// -----------------------------------------------------------------------------
