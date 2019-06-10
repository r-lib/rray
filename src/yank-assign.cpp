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

#include <xtensor/xarray.hpp>

// -----------------------------------------------------------------------------

// filter() view. `rray_dims(i) == rray_dims(x)`

// Would like to use `xt::filter()` for the logical `i` case
// Waiting on https://github.com/QuantStack/xtensor/issues/1663

// template <typename T>
// auto rray__yank_filter_impl(const xt::rarray<T>& x, const Rcpp::RObject& i) {
//   xt::rarray<rlogical> xt_i = Rcpp::as<xt::rarray<rlogical>>(i);
//   return xt::filter(x, xt_i);
// }

// For the assignment step to work, `x` cannot be a const reference

template <typename T>
auto rray__yank_non_const_index_impl(xt::rarray<T>& x, const Rcpp::RObject& i) {

  const xt::rtensor<int, 1> xt_i = Rcpp::as<xt::rtensor<int, 1>>(i);

  // Ideally wouldn't have to do this, but `x.shape()` isn't recognized by `unravel_indices()`
  const std::vector<std::size_t> shape(x.shape().begin(), x.shape().end());

  // Convert flat indices -> array indices
  auto array_indices = xt::unravel_indices(xt_i, shape, xt::layout_type::column_major);

  return xt::index_view(x, array_indices);
}


template <typename T>
Rcpp::RObject rray__yank_assign_impl(const xt::rarray<T>& x, Rcpp::RObject i, Rcpp::RObject value_) {

  // Copy `x`
  xt::rarray<T> out = x;

  xt::rarray<T> value(value_);

  if (TYPEOF(i) == LGLSXP) {
    // Waiting on https://github.com/QuantStack/xtensor/issues/1663
    //x_view = rray__yank_filter_impl(x, i);
    //rray__validate_broadcastable_to(value, x_view);
    //xt::noalias(x_view) = value;
  }
  else if (TYPEOF(i) == INTSXP) {
    auto x_view = rray__yank_non_const_index_impl(out, i);
    rray__validate_broadcastable_to(value, x_view);
    x_view = value;
  }
  else {
    Rcpp::stop("Internal error: `i` is somehow not a logical or integer.");
  }

  return Rcpp::as<Rcpp::RObject>(out);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__yank_assign(Rcpp::RObject x, Rcpp::RObject i, Rcpp::RObject value) {
  Rcpp::RObject out;
  DISPATCH_UNARY_TWO(out, rray__yank_assign_impl, x, i, value);

  rray__set_dim_names(out, rray__dim_names(x));

  return out;
}
