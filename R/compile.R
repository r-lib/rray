# TODO:
# This is nice, but doesn't work right with broadcasting because
# it _prepends_ dimensions rather than _appends_ them
# this works in rray because we manaully append 1s first
# but doesnt run when we compile it
# x <- matrix(1, nrow = 100)
# y <- matrix(1, ncol = 100)
# z <- array(1, c(100, 100, 2))
# rray(x) + rray(y) / rray(z)

#' Compile expressions on the fly
#'
#' These functions allow you to compile more complex operations in C++
#' on the fly. This can result in massive speed and memory improvements.
#'
#' rray operations are not as "lazy" as they could be.
#' They perform the operation in C++ and instantly return the result. Consider
#' the following set of operations:
#'
#' ```
#' x + y + z
#' ```
#'
#' First, `x + y` is computed, then the result of that is added to `z`. This
#' is essentially identical to:
#'
#' ```
#' temp <- x + y
#' temp + z
#' ```
#'
#' If we were able to pass the entire expression of `x + y + z` down to the
#' xtensor library at once, it would actually perform the entire expression
#' in 1 loop, with no temporary object created. This can result in incredible
#' speed improvements.
#'
#' These compilation functions allow you to do just that by constructing a
#' C++ function containing `x + y + z` and compiling it on the fly.
#'
#' @param expr An expression to be run at the C++ level using arguments
#' specified in `args`. The result of the expression is assigned directly
#' to a contain of type `return`, and then returned as an R object.
#'
#' @param body A character vector containing the entire body of the resulting
#' function. It must include a call to `return` at the end and should return
#' an object that can be converted to a `SEXP` automatically by Rcpp.
#'
#' @param args A character vector for the names of the arguments of the created
#' C++ function.
#'
#' @param arg_types A character vector of the types of `args`. Should be length
#' 1 or as long as `args` is.
#'
#' @param name A single character for the name of the created C++ function.
#'
#' @param return A single character for the container type of the
#' result.
#'
#' @param dry_run A single logical specifying whether or not to actually compile
#' the function. If set to `TRUE`, the function is not compiled, and the code
#' that would be compiled is printed instead.
#'
#' @examples
#'
#' # Compile an expression at the cpp level, and
#' # return the function as "rray_func()"
#' rray_compile_expr(x + y / z, args = c("x", "y", "z"))
#'
#' rray_func(1, 2, 3)
#'
#' # Full support for broadcasting is there automatically
#' # but you have to check that the dimensions are
#' # broadcastable yourself! Otherwise R will crash!
#' rray_func(1, 2, matrix(1:5))
#'
#' # -----------------------------------------------------------
#'
#' # A function that adds 3 objects together, storing
#' # them as intermediate variables using `auto`.
#' # These intermediate variables don't hold the result
#' # of the operation, but instead hold efficient xexpressions
#' # and the full operation is only computed when the result
#' # is assigned to `res`.
#' body <- '
#'   auto res1 = x + y;
#'   auto res2 = res1 + z;
#'   xt::rarray<double> res = res2;
#'   return res;
#' '
#'
#' rray_compile_body(
#'   body = body,
#'   args = c("x", "y", "z"),
#'   name = "custom_func"
#' )
#'
#' custom_func(1, matrix(6:10), matrix(1:5))
#'
#'
#' @name rray-compilation

#' @export
#' @rdname rray-compilation
rray_compile_expr <- function(expr,
                              args = NULL,
                              arg_types = "xt::rarray<double>",
                              name = "rray_func",
                              return = "xt::rarray<double>",
                              dry_run = FALSE) {

  expr <- rlang::expr_name(rlang::enexpr(expr))
  args <- construct_args(args, arg_types)

  code <- '
   // [[Rcpp::plugins(cpp14)]]
   // [[Rcpp::depends(xtensorrr)]]

   #include <xtensor-r/rarray.hpp>
   #include <xtensor-r/rtensor.hpp>
   #include <xtensor/xtensor.hpp>
   #include <Rcpp.h>
   using namespace Rcpp;

   // [[Rcpp::export]]
   SEXP {{name}}({{args}}) {

     {{return}} res = {{expr}};
     return res;

   }
  '

  code_glued <- glue::glue(code, .open = "{{", .close = "}}")

  if (dry_run) {
    dry_print(code_glued)
  }
  else {
    Rcpp::sourceCpp(code = code_glued)
  }

}

#' @export
#' @rdname rray-compilation
rray_compile_body <- function(body,
                              args = NULL,
                              arg_types = "xt::rarray<double>",
                              name = "rray_func",
                              dry_run = FALSE) {

  args <- construct_args(args, arg_types)

  code <- '
   // [[Rcpp::plugins(cpp14)]]
   // [[Rcpp::depends(xtensorrr)]]

   #include <xtensor-r/rarray.hpp>
   #include <xtensor-r/rtensor.hpp>
   #include <xtensor/xtensor.hpp>
   #include <Rcpp.h>
   using namespace Rcpp;

   // [[Rcpp::export]]
   SEXP {{name}}({{args}}) {

    {{body}}

   }
  '

  code_glued <- glue::glue(code, .open = "{{", .close = "}}")

  if (dry_run) {
    dry_print(code_glued)
  }
  else {
    Rcpp::sourceCpp(code = code_glued)
  }

}


construct_args <- function(args, arg_types) {

  if (is.null(args)) {
    args <- ""
  }
  else {

    # Recycle arg_types as needed
    n_args <- length(args)
    n_types <- length(arg_types)

    if (n_args != n_types) {
      if (n_types == 1L) {
        arg_types <- rep(arg_types, times = n_args)
      }
      else {
        abort("`arg_types` must be the same length as `args` or length 1.")
      }
    }

    # Collapse
    args <- paste(arg_types, args, sep = " ", collapse = ", ")
  }

  args
}

dry_print <- function(code) {
  cat("Auto generated template:\n")
  cat("------------------------")
  cat("\n\n")
  cat(code)
  cat("\n\n")
  cat("------------------------\n")
  invisible(NULL)
}
