vec_type_container <- function(x) {

  if (is.null(x)) {
    return(NULL)
  }

  UseMethod("vec_type_container")
}

vec_type_container.logical <- function(x) {
  logical()
}

vec_type_container.integer <- function(x) {
  integer()
}

vec_type_container.double <- function(x) {
  double()
}

vec_type_container.vctrs_rray_lgl <- function(x) {
  shared$empty_rray_lgl
}

vec_type_container.vctrs_rray_int <- function(x) {
  shared$empty_rray_int
}

vec_type_container.vctrs_rray_dbl <- function(x) {
  shared$empty_rray_dbl
}

vec_type_container.vctrs_unspecified <- function(x) {
  logical()
}

# ------------------------------------------------------------------------------

vec_type_container2 <- function(x, y) {
  vec_type2(vec_type_container(x), vec_type_container(y))
}

# ------------------------------------------------------------------------------

vec_type_container_common <- function(..., .ptype = NULL) {

  if (!is.null(.ptype)) {
    return(vec_type_container(.ptype))
  }

  args <- compact(list2(...))
  n_args <- length(args)

  if (n_args == 0L) {
    return(NULL)
  }

  if (n_args == 1L) {
    return(vec_type_container(args[[1]]))
  }

  reduce(args, vec_type_container2)
}
