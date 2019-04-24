#include <api.h>

int rray__prod(Rcpp::IntegerVector x) {
  int prod = 1;

  for (int i = 0; i < x.size(); ++i) {
    prod *= x[i];
  }

  return prod;
}
