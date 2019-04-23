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

// [[Rcpp::export]]
Rcpp::IntegerVector rray_increase_dims(const Rcpp::IntegerVector& dim,
                                       const int& dims) {

  int current_dims = dim.size();

  // Early exit
  if (current_dims == dims) {
    return dim;
  }

  if (current_dims > dims) {
    Rcpp::stop("Cannot decrease dimensions.");
  }

  // At this point, we know we are missing dims
  int n_missing_dims = dims - current_dims;

  // Copy dim since we change it
  Rcpp::IntegerVector out = Rcpp::clone(dim);

  for (int i = 0; i < n_missing_dims; ++i) {
    out.push_back(1);
  }

  return out;
}
