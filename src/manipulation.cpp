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

template <typename T>
xt::rarray<T> rray__transpose_impl(const xt::rarray<T>& x,
                                   Rcpp::RObject permutation) {

  using ptrdiff_vec_t = typename std::vector<std::ptrdiff_t>;

  if (r_is_null(permutation)) {
    xt::rarray<T> res = xt::transpose(x);
    return res;
  }

  ptrdiff_vec_t xt_permutation = Rcpp::as<ptrdiff_vec_t>(permutation);

  return xt::transpose(x, xt_permutation, xt::check_policy::full());
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__transpose(Rcpp::RObject x, Rcpp::RObject permutation) {
  DISPATCH_UNARY_ONE(rray__transpose_impl, x, permutation);
}

// -----------------------------------------------------------------------------

// Call xt::squeeze() but always use xt::check_policy::full()
// which throws an error if you are trying to drop a dimension
// with >1 element. You pretty much never want this so we don't
// expose that option.

// xt::squeeze() docs say it takes `axis` but its really `axes`

template <typename T>
xt::rarray<T> rray__squeeze_impl(const xt::rarray<T>& x, std::vector<std::size_t> axes) {
  return xt::squeeze(x, axes, xt::check_policy::full());
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__squeeze(Rcpp::RObject x, std::vector<std::size_t> axes) {
  DISPATCH_UNARY_ONE(rray__squeeze_impl, x, axes);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<T> rray__expand_dims_impl(const xt::rarray<T>& x, std::size_t axis) {
  return xt::expand_dims(x, axis);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__expand_dims(Rcpp::RObject x, std::size_t axis) {
  DISPATCH_UNARY_ONE(rray__expand_dims_impl, x, axis);
}
