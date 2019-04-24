#include <api.h>

// [[Rcpp::export]]
void rray__validate_dim(Rcpp::IntegerVector dim) {
  bool not_ok = Rcpp::is_true(Rcpp::any(dim < 0));

  if (not_ok) {
    Rcpp::stop("`dim` must be a positive integer vector.");
  }
}

// [[Rcpp::export]]
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
