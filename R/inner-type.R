vec_type_inner <- function(x) {

  if (is.null(x)) {
    return(NULL)
  }

  UseMethod("vec_type_inner")
}

vec_type_inner.logical <- function(x) {
  logical()
}

vec_type_inner.integer <- function(x) {
  integer()
}

vec_type_inner.double <- function(x) {
  double()
}

vec_type_inner.character <- function(x) {
  character()
}

vec_type_inner.vctrs_rray_lgl <- function(x) {
  logical()
}

vec_type_inner.vctrs_rray_int <- function(x) {
  integer()
}

vec_type_inner.vctrs_rray_dbl <- function(x) {
  double()
}

# ------------------------------------------------------------------------------

vec_type_inner2 <- function(x, y) {
  vec_type2(vec_type_inner(x), vec_type_inner(y))
}

# ------------------------------------------------------------------------------

vec_type_inner_common <- function(..., .ptype = NULL) {

  if (!is.null(.ptype)) {
    return(vec_type_inner(.ptype))
  }

  args <- compact(list2(...))
  n_args <- length(args)

  if (n_args == 0L) {
    return(NULL)
  }

  if (n_args == 1L) {
    return(vec_type_inner(args[[1]]))
  }

  reduce(args, vec_type_inner2)
}
