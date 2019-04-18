#include <rray.h>

// This registers ALL exported functions as callable from other C++
// packages. It is done on a per-source file basis.

// [[Rcpp::interfaces(r, cpp)]]

// [[Rcpp::export]]
Rcpp::IntegerVector rray_dim(Rcpp::RObject x) {

  Rcpp::IntegerVector out;

  if (x.hasAttribute("dim")) {
    out = x.attr("dim");
  }
  else {
    out = Rf_xlength(x);
  }

  return(out);
}
