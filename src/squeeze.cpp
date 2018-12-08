// #include <rray_types.h>
// #include <Rcpp.h>
// using namespace Rcpp;
//
// // I don't think this is working quite right so deprecate for now and come back
// // later
//
// // this also has an argument for axis to squeeze so it doesnt do them all
//
// // [[Rcpp::export]]
// xt::rarray<double> rray_squeeze_cpp(xt::rarray<double> x, IntegerVector axis) {
//
//   // better way?
//   LogicalVector axis_na = LogicalVector::create(IntegerVector::is_na(axis[0]));
//   bool axis_na_bool = as<bool>(axis_na);
//
//   xt::rarray<double> x_sq;
//
//   if(axis_na_bool) {
//     x_sq = xt::squeeze(x);
//   }
//   else {
//     x_sq = xt::squeeze(x, axis);
//   }
//
//   return x_sq;
// }
