#include <api.h>

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
