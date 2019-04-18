#include <rray.h>
#include <tools/tools.h>
using namespace rray;

template <typename T>
xt::rarray<T> rray_sum_cpp(const xt::rarray<T>& x, SEXP axes) {

  if (Rf_isNull(axes)) {
    return xt::sum(x);
  }

  std::vector<std::size_t> axes_cpp = Rcpp::as<std::vector<std::size_t>>(axes);
  return xt::sum(x, axes_cpp);
}

template <typename T>
xt::rarray<T> rray_prod_cpp(const xt::rarray<T>& x, SEXP axes) {

  if (Rf_isNull(axes)) {
    return xt::prod(x);
  }

  std::vector<std::size_t> axes_cpp = Rcpp::as<std::vector<std::size_t>>(axes);
  return xt::prod(x, axes_cpp);
}


template <typename T>
xt::rarray<T> rray_mean_cpp(const xt::rarray<T>& x, SEXP axes) {

  if (Rf_isNull(axes)) {
    return xt::mean(x);
  }

  std::vector<std::size_t> axes_cpp = Rcpp::as<std::vector<std::size_t>>(axes);
  return xt::mean(x, axes_cpp);
}

// template <typename T>
// xt::rarray<T> rray_variance_cpp(const xt::rarray<T>& x, SEXP axes) {
//
//   if (Rf_isNull(axes)) {
//     return xt::variance(x);
//   }
//
//   std::vector<std::size_t> axes_cpp = as<std::vector<std::size_t>>(axes);
//   return xt::variance(x, axes_cpp);
// }
//
// template <typename T>
// xt::rarray<T> rray_stddev_cpp(const xt::rarray<T>& x, SEXP axes) {
//
//   if (Rf_isNull(axes)) {
//     return xt::stddev(x);
//   }
//
//   std::vector<std::size_t> axes_cpp = as<std::vector<std::size_t>>(axes);
//   return xt::stddev(x, axes_cpp);
// }

template <typename T>
xt::rarray<T> rray_amax_cpp(const xt::rarray<T>& x, SEXP axes) {

  if (Rf_isNull(axes)) {
    return xt::amax(x);
  }

  std::vector<std::size_t> axes_cpp = Rcpp::as<std::vector<std::size_t>>(axes);
  return xt::amax(x, axes_cpp);
}

template <typename T>
xt::rarray<T> rray_amin_cpp(const xt::rarray<T>& x, SEXP axes) {

  if (Rf_isNull(axes)) {
    return xt::amin(x);
  }

  std::vector<std::size_t> axes_cpp = Rcpp::as<std::vector<std::size_t>>(axes);
  return xt::amin(x, axes_cpp);
}

// -----------------------------------------------------------------------------
// Switch on the op

template <typename T>
SEXP rray_reducer_cpp_impl(std::string op, xt::rarray<T> x, SEXP axes) {

  switch(str2int(op.c_str())) {

  case str2int("sum"): {
    return rray_sum_cpp(x, axes);
  }

  case str2int("prod"): {
    return rray_prod_cpp(x, axes);
  }

  case str2int("mean"): {
    return rray_mean_cpp(x, axes);
  }

  // case str2int("variance"): {
  //   return rray_variance_cpp(x, axes);
  // }
  //
  // case str2int("stddev"): {
  //   return rray_stddev_cpp(x, axes);
  // }

  case str2int("amax"): {
    return rray_amax_cpp(x, axes);
  }

  case str2int("amin"): {
    return rray_amin_cpp(x, axes);
  }

  default: {
    Rcpp::stop("Unknown reducing operation.");
  }

  }

}

// -----------------------------------------------------------------------------
// Integer input is special cased

// Some reducers need to return a double for stability. This aligns with
// what base R does as well. (Except for sum, which in base R sometimes
// gives an int and sometimes a double based on whether overflow is going
// to happen or not)

SEXP rray_int_reducer_cpp_impl(std::string op, xt::rarray<int> x, SEXP axes) {

  switch(str2int(op.c_str())) {

  case str2int("sum"): {
    return rray_sum_cpp(xt::rarray<double>(x), axes);
  }

  case str2int("prod"): {
    return rray_prod_cpp(xt::rarray<double>(x), axes);
  }

  case str2int("mean"): {
    return rray_mean_cpp(xt::rarray<double>(x), axes);
  }

  // case str2int("variance"): {
  //   return rray_variance_cpp(xt::rarray<double>(x), axes);
  // }
  //
  // case str2int("stddev"): {
  //   return rray_stddev_cpp(xt::rarray<double>(x), axes);
  // }

  case str2int("amax"): {
    return rray_amax_cpp(x, axes);
  }

  case str2int("amin"): {
    return rray_amin_cpp(x, axes);
  }

  default: {
    Rcpp::stop("Unknown reducing operation.");
  }

  }

}

// -----------------------------------------------------------------------------
// Switch on the type of x

// [[Rcpp::export]]
SEXP rray_reducer_cpp(std::string op, SEXP x, SEXP axes) {

  // Switch on X
  switch(TYPEOF(x)) {

    case REALSXP: {
      auto x_rray = Rcpp::as<xt::rarray<double>>(x);
      return rray_reducer_cpp_impl(op, x_rray, axes);
    }

    case INTSXP: {
      auto x_rray = Rcpp::as<xt::rarray<int>>(x);
      return rray_int_reducer_cpp_impl(op, x_rray, axes);
    }

    case LGLSXP: {
      auto x_rray = Rcpp::as<xt::rarray<rlogical>>(x);
      return rray_reducer_cpp_impl(op, x_rray, axes);
    }

    default: {
      rray::error_unknown_type();
    }

  }

}
