#include <rray.h>
#include <dispatch.h>
#include <type2.h>
#include <cast.h>

// -----------------------------------------------------------------------------

// TODO - handle any integer overflow somehow?

template <typename T>
xt::rarray<T> rray__multiply_add_impl(const xt::rarray<T>& x,
                                      const xt::rarray<T>& y,
                                      const xt::rarray<T>& z) {

  // Common dim
  Rcpp::IntegerVector x_dim = rray__dim(SEXP(x));
  Rcpp::IntegerVector y_dim = rray__dim(SEXP(y));
  Rcpp::IntegerVector z_dim = rray__dim(SEXP(z));

  Rcpp::IntegerVector dim = rray__dim2(rray__dim2(x_dim, y_dim), z_dim);

  const int& dim_n = dim.size();

  auto x_view = rray__increase_dims_view(x, dim_n);
  auto y_view = rray__increase_dims_view(y, dim_n);
  auto z_view = rray__increase_dims_view(z, dim_n);

  return xt::fma(x_view, y_view, z_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__multiply_add(Rcpp::RObject x, Rcpp::RObject y, Rcpp::RObject z) {

  Rcpp::IntegerVector x_dim = rray__dim(x);
  Rcpp::IntegerVector y_dim = rray__dim(y);
  Rcpp::IntegerVector z_dim = rray__dim(z);

  Rcpp::IntegerVector dim = rray__dim2(rray__dim2(x_dim, y_dim), z_dim);

  Rcpp::List resized_x_dim_names = rray__resize_dim_names(rray__dim_names(x), dim);
  Rcpp::List resized_y_dim_names = rray__resize_dim_names(rray__dim_names(y), dim);
  Rcpp::List resized_z_dim_names = rray__resize_dim_names(rray__dim_names(z), dim);

  Rcpp::List new_dim_names;

  new_dim_names = rray__coalesce_dim_names(
    resized_x_dim_names,
    resized_y_dim_names
  );

  new_dim_names = rray__coalesce_dim_names(
    new_dim_names,
    resized_z_dim_names
  );

  Rcpp::RObject tmp_type = vec__ptype_inner2(x, y);
  Rcpp::RObject type = vec__ptype_inner2(tmp_type, z);
  x = vec__cast_inner(x, type);
  y = vec__cast_inner(y, type);
  z = vec__cast_inner(z, type);

  Rcpp::RObject out;
  DISPATCH_TRINARY_NO_LOGICAL(out, rray__multiply_add_impl, x, y, z);

  rray__set_dim_names(out, new_dim_names);

  return out;
}
