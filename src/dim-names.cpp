#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------

// [[Rcpp::export(rng = false)]]
Rcpp::List rray__new_empty_dim_names(int n) {
  return Rcpp::List(n);
}

// -----------------------------------------------------------------------------

// [[Rcpp::export(rng = false)]]
Rcpp::List rray__dim_names(const Rcpp::RObject& x) {

  Rcpp::RObject x_dim_names;

  if (rray__has_dim(x)) {
    x_dim_names = x.attr("dimnames");

    if (x_dim_names.isNULL()) {
      x_dim_names = rray__new_empty_dim_names(rray__dim_n(x));
    }
  }
  else {
    x_dim_names = x.attr("names");

    if (x_dim_names.isNULL()) {
      x_dim_names = rray__new_empty_dim_names(rray__dim_n(x));
    }
    else {
      // character vector -> list
      x_dim_names = Rcpp::List::create(x_dim_names);
    }
  }

  return Rcpp::as<Rcpp::List>(x_dim_names);
}

// -----------------------------------------------------------------------------
