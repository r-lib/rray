# nocov start

#' @importFrom vctrs vec_size
#' @importFrom vctrs vec_data
#' @importFrom vctrs vec_slice
#' @importFrom vctrs vec_init
#' @importFrom vctrs vec_c
#' @importFrom vctrs vec_ptype
#' @importFrom vctrs vec_ptype_common
#' @importFrom vctrs vec_size_common
#' @importFrom vctrs vec_math
#' @importFrom vctrs vec_math_base
#' @importFrom vctrs vec_unique
#' @importFrom vctrs vec_unique_loc
#' @importFrom vctrs vec_unique_count
#' @importFrom vctrs vec_duplicate_any
#' @importFrom vctrs vec_duplicate_detect
#' @importFrom vctrs vec_duplicate_id
#' @importFrom vctrs vec_ptype_finalise
#' @importFrom vctrs vec_assert
#' @importFrom vctrs new_rcrd
#' @importFrom vctrs field
#' @importFrom vctrs vec_split
#' @importFrom vctrs vec_proxy_compare
#' @importFrom vctrs vec_as_index
#'
#' @importFrom vctrs vec_ptype_full
#' @importFrom vctrs vec_ptype_abbr
#'
#' @importFrom vctrs stop_incompatible_cast
#' @importFrom vctrs stop_incompatible_type
#' @importFrom vctrs stop_incompatible_op
#'
#' @importFrom vctrs new_vctr
#' @importFrom vctrs obj_print_data
#' @importFrom vctrs obj_str_data
#'
#' @importFrom vctrs vec_arith
#' @importFrom vctrs vec_arith.logical
#' @importFrom vctrs vec_arith.numeric
#'
#' @importFrom vctrs vec_cast
#' @importFrom vctrs vec_cast.double
#' @importFrom vctrs vec_cast.integer
#' @importFrom vctrs vec_cast.logical
#'
#' @importFrom vctrs vec_ptype2
#' @importFrom vctrs vec_ptype2.double
#' @importFrom vctrs vec_ptype2.integer
#' @importFrom vctrs vec_ptype2.logical
NULL

# Required for R 3.1

#' @importFrom vctrs vec_cast.character
NULL

#' @importFrom rlang abort
#' @importFrom rlang is_integer
#' @importFrom rlang is_character
#' @importFrom rlang is_double
#' @importFrom rlang is_logical
#' @importFrom rlang is_bare_integer
#' @importFrom rlang is_scalar_integer
#' @importFrom rlang is_integerish
#' @importFrom rlang is_null
#' @importFrom rlang is_missing
#' @importFrom rlang is_empty
#' @importFrom rlang is_true
#' @importFrom rlang %||%
#' @importFrom rlang dots_list
#' @importFrom rlang missing_arg
#' @importFrom rlang eval_bare
#' @importFrom rlang expr
#' @importFrom rlang list2
#' @importFrom rlang set_names
#' @importFrom rlang maybe_missing
#' @importFrom rlang names2
#' @importFrom rlang new_list
#' @importFrom rlang is_vector
NULL

#' @importFrom utils head tail
NULL

#' @useDynLib rray, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

# Holder for empty objects so we don't have to repeatedly spend time creating them
shared <- rlang::new_environment()

.onLoad <- function(libname, pkgname) {
  shared$empty_rray_int <- new_rray(integer())
  shared$empty_rray_lgl <- new_rray(logical())
  shared$empty_rray_dbl <- new_rray(double())

  rray_init(rlang::ns_env())
}


# nocov end
