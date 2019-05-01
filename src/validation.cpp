#include <api.h>

// [[Rcpp::export(rng = false)]]
void rray__validate_dim(Rcpp::IntegerVector dim) {
  bool not_ok = Rcpp::is_true(Rcpp::any(dim < 0));

  if (not_ok) {
    Rcpp::stop("`dim` must be a positive integer vector.");
  }
}

// [[Rcpp::export(rng = false)]]
void rray__validate_reshape(Rcpp::RObject x, Rcpp::IntegerVector dim) {

  rray__validate_dim(dim);

  int x_size = rray__prod(rray__dim(x));
  int new_size = rray__prod(dim);

  if (x_size != new_size) {
    Rcpp::stop(
      "The size you are reshaping from (%i) must be equal to the size you are reshaping to (%i).",
      x_size, new_size
    );
  }

}

// Convert integer dim to something like '(1, 2, 3)'
std::string rray__dim_to_string(Rcpp::IntegerVector dim) {

  std::vector<int> dim_vec = Rcpp::as<std::vector<int>>(dim);

  std::ostringstream oss;

  oss << "(";

  if (!dim_vec.empty())
  {
    // Convert all but the last element to avoid a trailing ","
    std::copy(
      dim_vec.begin(),
      dim_vec.end() - 1,
      std::ostream_iterator<int>(oss, ", ")
    );

    // Now add the last element with no delimiter
    oss << dim_vec.back();
  }

  oss << ")";

  return oss.str();
}

// 4 cases where broadcasting works:
// - Dimensions are the same (no change is made)
// - Dimension of x is 1 (broadcast to new dimension)
// - New dimension is 0 (no change is made)
// - Dimensions of x are shorter than `dim` (i.e. x is 3D but dim is 4D)
//   in that case, we check the 3 first dimensions and assume the 4th will
//   be reshape-viewed to work

// [[Rcpp::export(rng = false)]]
void rray__validate_broadcastable_to_dim(Rcpp::IntegerVector x_dim,
                                         Rcpp::IntegerVector dim) {

  int n_x = x_dim.size();
  int n_to = dim.size();

  if (n_x > n_to) {
    Rcpp::stop(
      "Cannot decrease dimensionality from %i to %i.",
      n_x, n_to
    );
  }

  for (int i = 0; i < n_x; ++i) {
    int x_dim_i = x_dim[i];
    int dim_i = dim[i];

    bool ok = (x_dim_i == dim_i || x_dim_i == 1);

    if (!ok) {
      Rcpp::stop(
        "Cannot broadcast from %s to %s due to dimension %i.",
        rray__dim_to_string(x_dim),
        rray__dim_to_string(dim),
        i + 1
      );
    }
  }
}
