#include <rray.h>
#include <dispatch.h>
#include <tools/tools.h>

// -----------------------------------------------------------------------------

// guard against integer overflow

template <typename T>
xt::rarray<double> rray__sum_impl(const xt::rarray<T>& x, Rcpp::RObject axes) {

  if (r_is_null(axes)) {
    return xt::sum(x, xt::keep_dims);
  }

  using size_vec = typename std::vector<std::size_t>;
  size_vec xt_axes = Rcpp::as<size_vec>(axes);

  return xt::sum(x, xt_axes, xt::keep_dims);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__sum(Rcpp::RObject x, Rcpp::RObject axes) {
  DISPATCH_UNARY_ONE(rray__sum_impl, x, axes);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__prod_impl(const xt::rarray<T>& x, Rcpp::RObject axes) {

  if (r_is_null(axes)) {
    return xt::prod(x, xt::keep_dims);
  }

  using size_vec = typename std::vector<std::size_t>;
  size_vec xt_axes = Rcpp::as<size_vec>(axes);

  return xt::prod(x, xt_axes, xt::keep_dims);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__prod(Rcpp::RObject x, Rcpp::RObject axes) {
  DISPATCH_UNARY_ONE(rray__prod_impl, x, axes);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__mean_impl(const xt::rarray<T>& x, Rcpp::RObject axes) {

  if (r_is_null(axes)) {
    return xt::mean(x, xt::keep_dims);
  }

  using size_vec = typename std::vector<std::size_t>;
  size_vec xt_axes = Rcpp::as<size_vec>(axes);

  return xt::mean(x, xt_axes, xt::keep_dims);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__mean(Rcpp::RObject x, Rcpp::RObject axes) {
  DISPATCH_UNARY_ONE(rray__mean_impl, x, axes);
}

// -----------------------------------------------------------------------------

// TODO - Blocked by
// https://github.com/DavisVaughan/rray/issues/42

// template <typename T>
// xt::rarray<double> rray__variance_impl(const xt::rarray<T>& x, Rcpp::RObject axes) {
//
//   if (r_is_null(axes)) {
//     return xt::variance(x, xt::keep_dims);
//   }
//
//   using size_vec = typename std::vector<std::size_t>;
//   size_vec xt_axes = Rcpp::as<size_vec>(axes);
//
//   return xt::variance(x, xt_axes, xt::keep_dims);
// }
//
// // [[Rcpp::export(rng = false)]]
// Rcpp::RObject rray__variance(Rcpp::RObject x, Rcpp::RObject axes) {
//   DISPATCH_UNARY_ONE(rray__variance_impl, x, axes);
// }

// -----------------------------------------------------------------------------

// TODO - Blocked by
// https://github.com/DavisVaughan/rray/issues/42

// template <typename T>
// xt::rarray<double> rray__std_dev_impl(const xt::rarray<T>& x, Rcpp::RObject axes) {
//
//   if (r_is_null(axes)) {
//     return xt::stddev(x, xt::keep_dims);
//   }
//
//   using size_vec = typename std::vector<std::size_t>;
//   size_vec xt_axes = Rcpp::as<size_vec>(axes);
//
//   return xt::stddev(x, xt_axes, xt::keep_dims);
// }
//
// // [[Rcpp::export(rng = false)]]
// Rcpp::RObject rray__std_dev(Rcpp::RObject x, Rcpp::RObject axes) {
//   DISPATCH_UNARY_ONE(rray__std_dev_impl, x, axes);
// }

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<T> rray__amax_impl(const xt::rarray<T>& x, Rcpp::RObject axes) {

  if (r_is_null(axes)) {
    return xt::amax(x, xt::keep_dims);
  }

  using size_vec = typename std::vector<std::size_t>;
  size_vec xt_axes = Rcpp::as<size_vec>(axes);

  return xt::amax(x, xt_axes, xt::keep_dims);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__amax(Rcpp::RObject x, Rcpp::RObject axes) {
  DISPATCH_UNARY_ONE(rray__amax_impl, x, axes);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<T> rray__amin_impl(const xt::rarray<T>& x, Rcpp::RObject axes) {

  if (r_is_null(axes)) {
    return xt::amin(x, xt::keep_dims);
  }

  using size_vec = typename std::vector<std::size_t>;
  size_vec xt_axes = Rcpp::as<size_vec>(axes);

  return xt::amin(x, xt_axes, xt::keep_dims);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__amin(Rcpp::RObject x, Rcpp::RObject axes) {
  DISPATCH_UNARY_ONE(rray__amin_impl, x, axes);
}

// -----------------------------------------------------------------------------
