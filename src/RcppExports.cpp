// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/rray.h"
#include <Rcpp.h>

using namespace Rcpp;

// rray__add
Rcpp::RObject rray__add(Rcpp::RObject x, Rcpp::RObject y);
RcppExport SEXP _rray_rray__add(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(rray__add(x, y));
    return rcpp_result_gen;
END_RCPP
}
// rray__subtract
Rcpp::RObject rray__subtract(Rcpp::RObject x, Rcpp::RObject y);
RcppExport SEXP _rray_rray__subtract(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(rray__subtract(x, y));
    return rcpp_result_gen;
END_RCPP
}
// rray__divide
Rcpp::RObject rray__divide(Rcpp::RObject x, Rcpp::RObject y);
RcppExport SEXP _rray_rray__divide(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(rray__divide(x, y));
    return rcpp_result_gen;
END_RCPP
}
// rray__multiply
Rcpp::RObject rray__multiply(Rcpp::RObject x, Rcpp::RObject y);
RcppExport SEXP _rray_rray__multiply(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(rray__multiply(x, y));
    return rcpp_result_gen;
END_RCPP
}
// rray__pow
Rcpp::RObject rray__pow(Rcpp::RObject x, Rcpp::RObject y);
RcppExport SEXP _rray_rray__pow(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(rray__pow(x, y));
    return rcpp_result_gen;
END_RCPP
}
// rray__modulus
Rcpp::RObject rray__modulus(Rcpp::RObject x, Rcpp::RObject y);
RcppExport SEXP _rray_rray__modulus(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(rray__modulus(x, y));
    return rcpp_result_gen;
END_RCPP
}
// rray__identity
Rcpp::RObject rray__identity(Rcpp::RObject x);
RcppExport SEXP _rray_rray__identity(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rray__identity(x));
    return rcpp_result_gen;
END_RCPP
}
// rray__opposite
Rcpp::RObject rray__opposite(Rcpp::RObject x);
RcppExport SEXP _rray_rray__opposite(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rray__opposite(x));
    return rcpp_result_gen;
END_RCPP
}
// rray__broadcast
Rcpp::RObject rray__broadcast(Rcpp::RObject x, Rcpp::IntegerVector dim);
RcppExport SEXP _rray_rray__broadcast(SEXP xSEXP, SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(rray__broadcast(x, dim));
    return rcpp_result_gen;
END_RCPP
}
// rray__full_like
Rcpp::RObject rray__full_like(Rcpp::RObject x, Rcpp::RObject value);
RcppExport SEXP _rray_rray__full_like(SEXP xSEXP, SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type value(valueSEXP);
    rcpp_result_gen = Rcpp::wrap(rray__full_like(x, value));
    return rcpp_result_gen;
END_RCPP
}
// rray__diag
Rcpp::RObject rray__diag(Rcpp::RObject x, int k);
RcppExport SEXP _rray_rray__diag(SEXP xSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(rray__diag(x, k));
    return rcpp_result_gen;
END_RCPP
}
// rray_ones_cpp
SEXP rray_ones_cpp(const std::vector<std::size_t>& shape);
RcppExport SEXP _rray_rray_ones_cpp(SEXP shapeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const std::vector<std::size_t>& >::type shape(shapeSEXP);
    rcpp_result_gen = Rcpp::wrap(rray_ones_cpp(shape));
    return rcpp_result_gen;
END_RCPP
}
// rray_zeros_cpp
SEXP rray_zeros_cpp(const std::vector<std::size_t>& shape);
RcppExport SEXP _rray_rray_zeros_cpp(SEXP shapeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const std::vector<std::size_t>& >::type shape(shapeSEXP);
    rcpp_result_gen = Rcpp::wrap(rray_zeros_cpp(shape));
    return rcpp_result_gen;
END_RCPP
}
// rray_eye_cpp
SEXP rray_eye_cpp(const std::vector<std::size_t> shape, int k);
RcppExport SEXP _rray_rray_eye_cpp(SEXP shapeSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const std::vector<std::size_t> >::type shape(shapeSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(rray_eye_cpp(shape, k));
    return rcpp_result_gen;
END_RCPP
}
// rray_eye_square_cpp
SEXP rray_eye_square_cpp(std::size_t n, int k);
RcppExport SEXP _rray_rray_eye_square_cpp(SEXP nSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< std::size_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(rray_eye_square_cpp(n, k));
    return rcpp_result_gen;
END_RCPP
}
// rray__greater
Rcpp::RObject rray__greater(Rcpp::RObject x, Rcpp::RObject y);
RcppExport SEXP _rray_rray__greater(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(rray__greater(x, y));
    return rcpp_result_gen;
END_RCPP
}
// rray__greater_equal
Rcpp::RObject rray__greater_equal(Rcpp::RObject x, Rcpp::RObject y);
RcppExport SEXP _rray_rray__greater_equal(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(rray__greater_equal(x, y));
    return rcpp_result_gen;
END_RCPP
}
// rray__lesser
Rcpp::RObject rray__lesser(Rcpp::RObject x, Rcpp::RObject y);
RcppExport SEXP _rray_rray__lesser(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(rray__lesser(x, y));
    return rcpp_result_gen;
END_RCPP
}
// rray__lesser_equal
Rcpp::RObject rray__lesser_equal(Rcpp::RObject x, Rcpp::RObject y);
RcppExport SEXP _rray_rray__lesser_equal(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(rray__lesser_equal(x, y));
    return rcpp_result_gen;
END_RCPP
}
// rray__equal
Rcpp::RObject rray__equal(Rcpp::RObject x, Rcpp::RObject y);
RcppExport SEXP _rray_rray__equal(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(rray__equal(x, y));
    return rcpp_result_gen;
END_RCPP
}
// rray__not_equal
Rcpp::RObject rray__not_equal(Rcpp::RObject x, Rcpp::RObject y);
RcppExport SEXP _rray_rray__not_equal(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(rray__not_equal(x, y));
    return rcpp_result_gen;
END_RCPP
}
// rray__dim
Rcpp::IntegerVector rray__dim(const Rcpp::RObject& x);
RcppExport SEXP _rray_rray__dim(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::RObject& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rray__dim(x));
    return rcpp_result_gen;
END_RCPP
}
// rray__dim2
Rcpp::IntegerVector rray__dim2(Rcpp::IntegerVector x_dim, Rcpp::IntegerVector y_dim);
RcppExport SEXP _rray_rray__dim2(SEXP x_dimSEXP, SEXP y_dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type x_dim(x_dimSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type y_dim(y_dimSEXP);
    rcpp_result_gen = Rcpp::wrap(rray__dim2(x_dim, y_dim));
    return rcpp_result_gen;
END_RCPP
}
// rray__dims
int rray__dims(const Rcpp::RObject& x);
RcppExport SEXP _rray_rray__dims(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::RObject& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rray__dims(x));
    return rcpp_result_gen;
END_RCPP
}
// rray__dims2
int rray__dims2(const int& x_dims, const int& y_dims);
RcppExport SEXP _rray_rray__dims2(SEXP x_dimsSEXP, SEXP y_dimsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const int& >::type x_dims(x_dimsSEXP);
    Rcpp::traits::input_parameter< const int& >::type y_dims(y_dimsSEXP);
    rcpp_result_gen = Rcpp::wrap(rray__dims2(x_dims, y_dims));
    return rcpp_result_gen;
END_RCPP
}
// rray__increase_dims
Rcpp::IntegerVector rray__increase_dims(const Rcpp::IntegerVector& dim, const int& dims);
RcppExport SEXP _rray_rray__increase_dims(SEXP dimSEXP, SEXP dimsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector& >::type dim(dimSEXP);
    Rcpp::traits::input_parameter< const int& >::type dims(dimsSEXP);
    rcpp_result_gen = Rcpp::wrap(rray__increase_dims(dim, dims));
    return rcpp_result_gen;
END_RCPP
}
// rray__reshape
Rcpp::RObject rray__reshape(Rcpp::RObject x, Rcpp::IntegerVector dim);
RcppExport SEXP _rray_rray__reshape(SEXP xSEXP, SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(rray__reshape(x, dim));
    return rcpp_result_gen;
END_RCPP
}
// rray__logical_and
Rcpp::RObject rray__logical_and(Rcpp::RObject x, Rcpp::RObject y);
RcppExport SEXP _rray_rray__logical_and(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(rray__logical_and(x, y));
    return rcpp_result_gen;
END_RCPP
}
// rray__logical_or
Rcpp::RObject rray__logical_or(Rcpp::RObject x, Rcpp::RObject y);
RcppExport SEXP _rray_rray__logical_or(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(rray__logical_or(x, y));
    return rcpp_result_gen;
END_RCPP
}
// rray__logical_not
Rcpp::RObject rray__logical_not(const xt::rarray<rlogical>& x);
RcppExport SEXP _rray_rray__logical_not(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const xt::rarray<rlogical>& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rray__logical_not(x));
    return rcpp_result_gen;
END_RCPP
}
// rray__any
Rcpp::RObject rray__any(const xt::rarray<rlogical>& x, Rcpp::RObject axes);
RcppExport SEXP _rray_rray__any(SEXP xSEXP, SEXP axesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const xt::rarray<rlogical>& >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type axes(axesSEXP);
    rcpp_result_gen = Rcpp::wrap(rray__any(x, axes));
    return rcpp_result_gen;
END_RCPP
}
// rray_op_binary_cpp
SEXP rray_op_binary_cpp(const std::string& op, SEXP x, SEXP y);
RcppExport SEXP _rray_rray_op_binary_cpp(SEXP opSEXP, SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const std::string& >::type op(opSEXP);
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(rray_op_binary_cpp(op, x, y));
    return rcpp_result_gen;
END_RCPP
}
// rray_op_trinary_cpp
SEXP rray_op_trinary_cpp(const std::string& op, SEXP x, SEXP y, SEXP z);
RcppExport SEXP _rray_rray_op_trinary_cpp(SEXP opSEXP, SEXP xSEXP, SEXP ySEXP, SEXP zSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const std::string& >::type op(opSEXP);
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type y(ySEXP);
    Rcpp::traits::input_parameter< SEXP >::type z(zSEXP);
    rcpp_result_gen = Rcpp::wrap(rray_op_trinary_cpp(op, x, y, z));
    return rcpp_result_gen;
END_RCPP
}
// rray_op_unary_one_cpp
SEXP rray_op_unary_one_cpp(std::string op, SEXP x, SEXP arg);
RcppExport SEXP _rray_rray_op_unary_one_cpp(SEXP opSEXP, SEXP xSEXP, SEXP argSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< std::string >::type op(opSEXP);
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type arg(argSEXP);
    rcpp_result_gen = Rcpp::wrap(rray_op_unary_one_cpp(op, x, arg));
    return rcpp_result_gen;
END_RCPP
}
// rray_op_unary_three_cpp
SEXP rray_op_unary_three_cpp(std::string op, SEXP x, SEXP arg1, SEXP arg2, SEXP arg3);
RcppExport SEXP _rray_rray_op_unary_three_cpp(SEXP opSEXP, SEXP xSEXP, SEXP arg1SEXP, SEXP arg2SEXP, SEXP arg3SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< std::string >::type op(opSEXP);
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type arg1(arg1SEXP);
    Rcpp::traits::input_parameter< SEXP >::type arg2(arg2SEXP);
    Rcpp::traits::input_parameter< SEXP >::type arg3(arg3SEXP);
    rcpp_result_gen = Rcpp::wrap(rray_op_unary_three_cpp(op, x, arg1, arg2, arg3));
    return rcpp_result_gen;
END_RCPP
}
// rray_op_unary_two_cpp
SEXP rray_op_unary_two_cpp(std::string op, SEXP x, SEXP arg1, SEXP arg2);
RcppExport SEXP _rray_rray_op_unary_two_cpp(SEXP opSEXP, SEXP xSEXP, SEXP arg1SEXP, SEXP arg2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< std::string >::type op(opSEXP);
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type arg1(arg1SEXP);
    Rcpp::traits::input_parameter< SEXP >::type arg2(arg2SEXP);
    rcpp_result_gen = Rcpp::wrap(rray_op_unary_two_cpp(op, x, arg1, arg2));
    return rcpp_result_gen;
END_RCPP
}
// rray_op_unary_cpp
SEXP rray_op_unary_cpp(std::string op, SEXP x);
RcppExport SEXP _rray_rray_op_unary_cpp(SEXP opSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< std::string >::type op(opSEXP);
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rray_op_unary_cpp(op, x));
    return rcpp_result_gen;
END_RCPP
}
// rray__sort
Rcpp::RObject rray__sort(Rcpp::RObject x, Rcpp::RObject axis);
RcppExport SEXP _rray_rray__sort(SEXP xSEXP, SEXP axisSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type axis(axisSEXP);
    rcpp_result_gen = Rcpp::wrap(rray__sort(x, axis));
    return rcpp_result_gen;
END_RCPP
}
// rray__max_pos
Rcpp::RObject rray__max_pos(Rcpp::RObject x, Rcpp::RObject axis);
RcppExport SEXP _rray_rray__max_pos(SEXP xSEXP, SEXP axisSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type axis(axisSEXP);
    rcpp_result_gen = Rcpp::wrap(rray__max_pos(x, axis));
    return rcpp_result_gen;
END_RCPP
}
// rray__min_pos
Rcpp::RObject rray__min_pos(Rcpp::RObject x, Rcpp::RObject axis);
RcppExport SEXP _rray_rray__min_pos(SEXP xSEXP, SEXP axisSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type axis(axisSEXP);
    rcpp_result_gen = Rcpp::wrap(rray__min_pos(x, axis));
    return rcpp_result_gen;
END_RCPP
}
// rray_reducer_cpp
SEXP rray_reducer_cpp(std::string op, SEXP x, SEXP axes);
RcppExport SEXP _rray_rray_reducer_cpp(SEXP opSEXP, SEXP xSEXP, SEXP axesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< std::string >::type op(opSEXP);
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type axes(axesSEXP);
    rcpp_result_gen = Rcpp::wrap(rray_reducer_cpp(op, x, axes));
    return rcpp_result_gen;
END_RCPP
}
// rray__subset_assign
Rcpp::RObject rray__subset_assign(Rcpp::RObject x, Rcpp::List indexer, Rcpp::RObject value);
RcppExport SEXP _rray_rray__subset_assign(SEXP xSEXP, SEXP indexerSEXP, SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type indexer(indexerSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type value(valueSEXP);
    rcpp_result_gen = Rcpp::wrap(rray__subset_assign(x, indexer, value));
    return rcpp_result_gen;
END_RCPP
}
// is_any_na_int
bool is_any_na_int(Rcpp::List x);
RcppExport SEXP _rray_is_any_na_int(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(is_any_na_int(x));
    return rcpp_result_gen;
END_RCPP
}
// is_contiguous_increasing
bool is_contiguous_increasing(Rcpp::RObject x);
RcppExport SEXP _rray_is_contiguous_increasing(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(is_contiguous_increasing(x));
    return rcpp_result_gen;
END_RCPP
}
// subset_dim_names
Rcpp::List subset_dim_names(Rcpp::List dim_names, Rcpp::List indexer);
RcppExport SEXP _rray_subset_dim_names(SEXP dim_namesSEXP, SEXP indexerSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type dim_names(dim_namesSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type indexer(indexerSEXP);
    rcpp_result_gen = Rcpp::wrap(subset_dim_names(dim_names, indexer));
    return rcpp_result_gen;
END_RCPP
}
// rray__subset
Rcpp::RObject rray__subset(Rcpp::RObject x, Rcpp::List indexer);
RcppExport SEXP _rray_rray__subset(SEXP xSEXP, SEXP indexerSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type indexer(indexerSEXP);
    rcpp_result_gen = Rcpp::wrap(rray__subset(x, indexer));
    return rcpp_result_gen;
END_RCPP
}
// rray__validate_dim
void rray__validate_dim(Rcpp::IntegerVector dim);
RcppExport SEXP _rray_rray__validate_dim(SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type dim(dimSEXP);
    rray__validate_dim(dim);
    return R_NilValue;
END_RCPP
}
// rray__validate_reshape
void rray__validate_reshape(Rcpp::RObject x, Rcpp::IntegerVector dim);
RcppExport SEXP _rray_rray__validate_reshape(SEXP xSEXP, SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type dim(dimSEXP);
    rray__validate_reshape(x, dim);
    return R_NilValue;
END_RCPP
}
// rray__validate_broadcastable_to_dim
void rray__validate_broadcastable_to_dim(Rcpp::IntegerVector x_dim, Rcpp::IntegerVector dim);
RcppExport SEXP _rray_rray__validate_broadcastable_to_dim(SEXP x_dimSEXP, SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type x_dim(x_dimSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type dim(dimSEXP);
    rray__validate_broadcastable_to_dim(x_dim, dim);
    return R_NilValue;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rray_rray__add", (DL_FUNC) &_rray_rray__add, 2},
    {"_rray_rray__subtract", (DL_FUNC) &_rray_rray__subtract, 2},
    {"_rray_rray__divide", (DL_FUNC) &_rray_rray__divide, 2},
    {"_rray_rray__multiply", (DL_FUNC) &_rray_rray__multiply, 2},
    {"_rray_rray__pow", (DL_FUNC) &_rray_rray__pow, 2},
    {"_rray_rray__modulus", (DL_FUNC) &_rray_rray__modulus, 2},
    {"_rray_rray__identity", (DL_FUNC) &_rray_rray__identity, 1},
    {"_rray_rray__opposite", (DL_FUNC) &_rray_rray__opposite, 1},
    {"_rray_rray__broadcast", (DL_FUNC) &_rray_rray__broadcast, 2},
    {"_rray_rray__full_like", (DL_FUNC) &_rray_rray__full_like, 2},
    {"_rray_rray__diag", (DL_FUNC) &_rray_rray__diag, 2},
    {"_rray_rray_ones_cpp", (DL_FUNC) &_rray_rray_ones_cpp, 1},
    {"_rray_rray_zeros_cpp", (DL_FUNC) &_rray_rray_zeros_cpp, 1},
    {"_rray_rray_eye_cpp", (DL_FUNC) &_rray_rray_eye_cpp, 2},
    {"_rray_rray_eye_square_cpp", (DL_FUNC) &_rray_rray_eye_square_cpp, 2},
    {"_rray_rray__greater", (DL_FUNC) &_rray_rray__greater, 2},
    {"_rray_rray__greater_equal", (DL_FUNC) &_rray_rray__greater_equal, 2},
    {"_rray_rray__lesser", (DL_FUNC) &_rray_rray__lesser, 2},
    {"_rray_rray__lesser_equal", (DL_FUNC) &_rray_rray__lesser_equal, 2},
    {"_rray_rray__equal", (DL_FUNC) &_rray_rray__equal, 2},
    {"_rray_rray__not_equal", (DL_FUNC) &_rray_rray__not_equal, 2},
    {"_rray_rray__dim", (DL_FUNC) &_rray_rray__dim, 1},
    {"_rray_rray__dim2", (DL_FUNC) &_rray_rray__dim2, 2},
    {"_rray_rray__dims", (DL_FUNC) &_rray_rray__dims, 1},
    {"_rray_rray__dims2", (DL_FUNC) &_rray_rray__dims2, 2},
    {"_rray_rray__increase_dims", (DL_FUNC) &_rray_rray__increase_dims, 2},
    {"_rray_rray__reshape", (DL_FUNC) &_rray_rray__reshape, 2},
    {"_rray_rray__logical_and", (DL_FUNC) &_rray_rray__logical_and, 2},
    {"_rray_rray__logical_or", (DL_FUNC) &_rray_rray__logical_or, 2},
    {"_rray_rray__logical_not", (DL_FUNC) &_rray_rray__logical_not, 1},
    {"_rray_rray__any", (DL_FUNC) &_rray_rray__any, 2},
    {"_rray_rray_op_binary_cpp", (DL_FUNC) &_rray_rray_op_binary_cpp, 3},
    {"_rray_rray_op_trinary_cpp", (DL_FUNC) &_rray_rray_op_trinary_cpp, 4},
    {"_rray_rray_op_unary_one_cpp", (DL_FUNC) &_rray_rray_op_unary_one_cpp, 3},
    {"_rray_rray_op_unary_three_cpp", (DL_FUNC) &_rray_rray_op_unary_three_cpp, 5},
    {"_rray_rray_op_unary_two_cpp", (DL_FUNC) &_rray_rray_op_unary_two_cpp, 4},
    {"_rray_rray_op_unary_cpp", (DL_FUNC) &_rray_rray_op_unary_cpp, 2},
    {"_rray_rray__sort", (DL_FUNC) &_rray_rray__sort, 2},
    {"_rray_rray__max_pos", (DL_FUNC) &_rray_rray__max_pos, 2},
    {"_rray_rray__min_pos", (DL_FUNC) &_rray_rray__min_pos, 2},
    {"_rray_rray_reducer_cpp", (DL_FUNC) &_rray_rray_reducer_cpp, 3},
    {"_rray_rray__subset_assign", (DL_FUNC) &_rray_rray__subset_assign, 3},
    {"_rray_is_any_na_int", (DL_FUNC) &_rray_is_any_na_int, 1},
    {"_rray_is_contiguous_increasing", (DL_FUNC) &_rray_is_contiguous_increasing, 1},
    {"_rray_subset_dim_names", (DL_FUNC) &_rray_subset_dim_names, 2},
    {"_rray_rray__subset", (DL_FUNC) &_rray_rray__subset, 2},
    {"_rray_rray__validate_dim", (DL_FUNC) &_rray_rray__validate_dim, 1},
    {"_rray_rray__validate_reshape", (DL_FUNC) &_rray_rray__validate_reshape, 2},
    {"_rray_rray__validate_broadcastable_to_dim", (DL_FUNC) &_rray_rray__validate_broadcastable_to_dim, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_rray(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
