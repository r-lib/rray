#include <api.h>

// -----------------------------------------------------------------------------

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

// -----------------------------------------------------------------------------

// c(x, y)
Rcpp::IntegerVector combine_int(Rcpp::IntegerVector x, Rcpp::IntegerVector y) {
  Rcpp::IntegerVector out(x.size() + y.size());

  std::copy(x.begin(), x.end(), out.begin());
  std::copy(y.begin(), y.end(), out.begin() + x.size());

  return out;
}

// This returns a list of x_dim / y_dim, but extends them as required
// so that the dimensionality matches. It does so by appending 1's as needed
Rcpp::List dims_match2(Rcpp::IntegerVector x_dim, Rcpp::IntegerVector y_dim) {
  int n_x = x_dim.size();
  int n_y = y_dim.size();

  Rcpp::IntegerVector x_matched;
  Rcpp::IntegerVector y_matched;

  if (n_x == n_y) {
    x_matched = x_dim;
    y_matched = y_dim;
  }
  else if (n_x < n_y) {
    x_matched = combine_int(x_dim, Rcpp::rep(1, n_y - n_x));
    y_matched = y_dim;
  }
  else {
    y_matched = combine_int(y_dim, Rcpp::rep(1, n_x - n_y));
    x_matched = x_dim;
  }

  Rcpp::List out = Rcpp::List::create(
    Rcpp::Named("x") = x_matched,
    Rcpp::Named("y") = y_matched
  );

  return out;
}

// Determine the "common dimensions" between two sets of dimensions by
// applying broadcasting rules.

// [[Rcpp::export]]
Rcpp::IntegerVector rray__dim2(Rcpp::IntegerVector x_dim,
                               Rcpp::IntegerVector y_dim) {

  Rcpp::List matched = dims_match2(x_dim, y_dim);

  Rcpp::IntegerVector x = matched["x"];
  Rcpp::IntegerVector y = matched["y"];

  int n = x.size();

  Rcpp::IntegerVector out(n);

  for (int i = 0; i < n; ++i) {

    int x_i = x[i];
    int y_i = y[i];

    if (x_i == y_i) {
      out[i] = x_i;
    }
    else if (x_i == 0 || y_i == 0) {
      out[i] = 0;
    }
    else if (x_i == 1) {
      out[i] = y_i;
    }
    else if (y_i == 1) {
      out[i] = x_i;
    }
    else {
      Rcpp::stop(
        "Non-broadcastable dimensions: %s and %s.",
        rray__dim_to_string(x),
        rray__dim_to_string(y)
      );
    }

  }

  return out;
}

// -----------------------------------------------------------------------------

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

// -----------------------------------------------------------------------------

// [[Rcpp::export]]
int rray__dims2(const int& x_dims, const int& y_dims) {
  return std::max(x_dims, y_dims);
}

// -----------------------------------------------------------------------------

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
