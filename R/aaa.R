#' @importFrom vctrs vec_size
#' @importFrom vctrs vec_dim
#' @importFrom vctrs vec_dims
#' @importFrom vctrs vec_data
#' @importFrom vctrs vec_restore
#' @importFrom vctrs vec_slice
#' @importFrom vctrs vec_type
#' @importFrom vctrs vec_type_common
#' @importFrom vctrs vec_math
#'
#' @importFrom vctrs vec_ptype_full
#' @importFrom vctrs vec_ptype_abbr
#'
#' @importFrom vctrs stop_incompatible_cast
#' @importFrom vctrs stop_incompatible_type
#'
#' @importFrom vctrs new_vctr
#' @importFrom vctrs obj_print_data
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
#' @importFrom vctrs vec_type2
#' @importFrom vctrs vec_type2.double
#' @importFrom vctrs vec_type2.integer
#' @importFrom vctrs vec_type2.logical
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
NULL

#' @importFrom utils head tail
NULL

#' @useDynLib rray, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL
