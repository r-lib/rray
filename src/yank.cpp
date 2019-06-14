#include <rray.h>
#include <tools/tools.h>
#include <dispatch.h>
#include <subset-tools.h>

// For `filter()` and `index_view()`
#include <xtensor/xindex_view.hpp>

// For `i`
#include <xtensor-r/rtensor.hpp>

// For `unravel_indices()`
#include <xtensor/xtensor.hpp>

// -----------------------------------------------------------------------------

// filter() view. `rray_dim_n(i) == rray_dim_n(x)`

template <typename T>
auto rray__yank_filter_impl(const xt::rarray<T>& x, const Rcpp::RObject& i) {
  xt::rarray<rlogical> xt_i = Rcpp::as<xt::rarray<rlogical>>(i);
  return xt::filter<xt::layout_type::column_major>(x, xt_i);
}

template <typename T>
auto rray__yank_index_impl(const xt::rarray<T>& x, const Rcpp::RObject& i) {

  const xt::rtensor<int, 1> xt_i = Rcpp::as<xt::rtensor<int, 1>>(i);

  // Ideally wouldn't have to do this, but `x.shape()` isn't recognized by `unravel_indices()`
  const std::vector<std::size_t> shape(x.shape().begin(), x.shape().end());

  // Convert flat indices -> array indices
  auto array_indices = xt::unravel_indices(xt_i, shape, xt::layout_type::column_major);

  return xt::index_view(x, array_indices);
}


template <typename T>
Rcpp::RObject rray__yank_impl(const xt::rarray<T>& x, Rcpp::RObject i) {

  xt::rarray<T> out;

  if (TYPEOF(i) == LGLSXP) {
    out = rray__yank_filter_impl(x, i);
  }
  else if (TYPEOF(i) == INTSXP) {
    out = rray__yank_index_impl(x, i);
  }
  else {
    Rcpp::stop("Internal error: `i` is somehow not a logical or integer.");
  }

  return Rcpp::as<Rcpp::RObject>(out);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__yank(Rcpp::RObject x, Rcpp::RObject i) {
  Rcpp::RObject out;
  DISPATCH_UNARY_ONE(out, rray__yank_impl, x, i);

  out.attr("dimnames") = rray__new_empty_dim_names(1);

  return out;
}
