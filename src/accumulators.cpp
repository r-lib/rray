#include <rray.h>
#include <dispatch.h>

// Both required for cumsum() / cumprod()
#include <xtensor/xarray.hpp>
#include <xtensor/xview.hpp>

// -----------------------------------------------------------------------------

// BLOCKED until
// https://github.com/QuantStack/xtensor/issues/1334

// TODO - detect integer overflow, maybe have to write a custom accumulator?
// https://github.com/wch/r-source/blob/5a156a0865362bb8381dcd69ac335f5174a4f60c/src/main/cum.c#L40
// https://github.com/QuantStack/xtensor/blob/94931618b695dbbae9155637dd7a43c8e64121f4/include/xtensor/xmath.hpp#L2042

// TODO - convert logicals to integers

// To prevent overflow, return doubles

// template <typename T>
// xt::rarray<double> rray__cumsum_impl(const xt::rarray<T>& x,
//                                      Rcpp::RObject axis) {
//
//   if (r_is_null(axis)) {
//     xt::rarray<double> res = xt::cumsum(x);
//     return res;
//   }
//
//   std::ptrdiff_t xt_axis = Rcpp::as<std::ptrdiff_t>(axis);
//   xt::rarray<double> res = xt::cumsum(x, xt_axis);
//
//   return res;
// }
//
// // [[Rcpp::export(rng = false)]]
// Rcpp::RObject rray__cumsum(Rcpp::RObject x, Rcpp::RObject axis) {
//   DISPATCH_UNARY_ONE(rray__cumsum_impl, x, axis);
// }

// -----------------------------------------------------------------------------

// BLOCKED until
// https://github.com/QuantStack/xtensor/issues/1334

// template <typename T>
// xt::rarray<T> rray__cumprod_impl(const xt::rarray<T>& x,
//                                  Rcpp::RObject axis) {
//
//   if (r_is_null(axis)) {
//     xt::rarray<double> res = xt::cumprod(x);
//     return res;
//   }
//
//   std::ptrdiff_t xt_axis = Rcpp::as<std::ptrdiff_t>(axis);
//   xt::rarray<double> res = xt::cumprod(x, xt_axis);
//   return res;
// }
//
// // [[Rcpp::export(rng = false)]]
// Rcpp::RObject rray__cumprod(Rcpp::RObject x, Rcpp::RObject axis) {
//   DISPATCH_UNARY_ONE(rray__cumprod_impl, x, axis);
// }

// -----------------------------------------------------------------------------
