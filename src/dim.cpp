#include <api.h>

// [[Rcpp::export]]
Rcpp::IntegerVector rray__dim(const Rcpp::RObject& x) {

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
int rray__dims(const Rcpp::RObject& x) {

  Rcpp::RObject d = x.attr("dim");

  if (Rf_isNull(d)) {
    return 1;
  }
  else {
    return Rf_length(d);
  }

}

// [[Rcpp::export]]
int rray__dims2(const int& x_dims, const int& y_dims) {
  return std::max(x_dims, y_dims);
}

// [[Rcpp::export]]
Rcpp::IntegerVector rray__increase_dims(const Rcpp::IntegerVector& dim,
                                        const int& dims) {

  int current_dims = dim.size();

  // Early exit
  if (current_dims == dims) {
    return dim;
  }

  if (current_dims > dims) {
    Rcpp::stop(
      "Cannot decrease dimensionality from %i to %i.",
      current_dims, dims
    );
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
