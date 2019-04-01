# Printing seems to be broken if I try and use the
# format method for array objects (it right aligns characters but not column
# headers). In the long run, we
# probably want our own format method that doesn't use the array
# one. For now, call print(as_array(x)) in obj_print_data() to have
# the alignment be correct. For an example of bad behavior, print
# as_rray(sh8) from ?solve with the format method

# format.vctrs_rray <- function(x, ...) {
#   format(as_array(x))
# }

#' @export
obj_print_data.vctrs_rray <- function(x, ...) {
  if (length(x) == 0) {
    return()
  }

  # vctrs sets names() here which is problematic
  # for 1D arrays

  print(as_array(x))
  invisible(x)
}

#' @export
t.vctrs_rray <- function(x) {
  as_rray(t(as_array(x)))
}

#' @export
vec_restore.vctrs_rray <- function(x, to, ..., i = NULL) {
  x_dim <- vec_dim(x)

  new_rray(
    .data = vec_data(x),
    size = x_dim[1],
    shape = x_dim[-1],
    dim_names = dim_names(x)
  )
}

#' @export
`dim<-.vctrs_rray` <- function(x, value) {
  rray_broadcast(x, value)
}

#' @export
vec_ptype_abbr.vctrs_rray <- function(x) {
  "rray"
}

#' @export
vec_ptype_full.vctrs_rray <- function(x) {
  paste0("vctrs_rray<", typeof(x), ">", vec_ptype_shape(x))
}

# from vctrs
vec_ptype_shape <- function(x) {
  dim <- vec_dim(x)
  if (length(dim) == 1) {
    ""
  } else {
    paste0("[,", paste(dim[-1], collapse = ","), "]")
  }
}

# Because vctrs uses x[1:length] in obj_str_leaf() which
# is not allowed for rray objects. This also has a more informative
# title

#' @export
obj_str_data.vctrs_rray <- function(x, ...) {

  width <- getOption("width") - 2
  out <- vec_data(x)

  # Avoid spending too much time formatting elements that won't see
  length <- ceiling(width / 2)
  if (vec_size(out) > length) {
    out <- vec_slice(out, 1:length)
  } else {
    out <- out
  }

  title <- glue::glue(" {vec_ptype_abbr(x)} {vec_ptype_shape(x)}[{vec_size(x)}] ")
  cat_line(inline_list(title, format(out), width = width))

  invisible(x)
}

# Print helper stolen from vctrs
cat_line <- function(...) {
  cat(paste0(..., "\n", collapse = ""))
}

# Print helper stolen from vctrs
inline_list <- function(title, x, width = getOption("width"), quote = "") {
  label_width <- width - nchar(title)
  x <- glue::glue_collapse(
    encodeString(x, quote = quote),
    sep = ", ",
    width = label_width
  )
  paste0(title, x)
}

# Override the vctrs `[<-` because it does not allow you to pass in more than
# just i AND it calls vec_cast() where `to` is the full x obj, not just on the slice
# you are casting to
# If vctrs did: function(x, ..., value) and vec_cast(value, x[...]) then
# all would be good

#' @export
`[<-.vctrs_rray` <- function(x, ..., value) {
  value <- vec_cast(value, x[...])
  x_array <- as_array(x)
  x_array[...] <- value
  res <- vec_restore(x_array, x)
  dim_names(res) <- dim_names(x)
  res
}

#' @export
`[[<-.vctrs_rray` <- function(x, ..., value) {
  value <- vec_cast(value, x[[...]])
  x_array <- as_array(x)
  x_array[[...]] <- value
  res <- vec_restore(x_array, x)
  dim_names(res) <- dim_names(x)
  res
}

# vec_type2 boilerplate --------------------------------------------------------

#' vctrs compatibility functions
#'
#' These functions are the extensions that allow rray objects to
#' work with vctrs.
#'
#' @param x,y Objects.
#' @param to Type to cast to.
#' @param op An arithmetic operator as a string.
#'
#' @name vctrs-compat
#'
NULL

#' @export
#' @rdname vctrs-compat
#' @method vec_type2 vctrs_rray
#' @export vec_type2.vctrs_rray
vec_type2.vctrs_rray <- function(x, y) UseMethod("vec_type2.vctrs_rray", y)

#' @method vec_type2.vctrs_rray default
#' @export
vec_type2.vctrs_rray.default <- function(x, y) stop_incompatible_type(x, y)

#' @method vec_type2.vctrs_rray vctrs_unspecified
#' @export
vec_type2.vctrs_rray.vctrs_unspecified <- function(x, y) x

# vec_type2 vctrs_rray <-> vctrs_rray ------------------------------------------

# vec_type2 makes no attempt to recover dim names, as they are not part of the type.
# type = class + shape (at least for rray objects)

#' @method vec_type2.vctrs_rray vctrs_rray
#' @export
vec_type2.vctrs_rray.vctrs_rray <- function(x, y) {
  inner_ptype <- vec_type(vec_type2(vec_data(x), vec_data(y)))
  new_rray(inner_ptype, shape = rray_shape2(x, y))
}

# vec_type2 vctrs_rray <-> double/matrix/array ---------------------------------

#' @method vec_type2.vctrs_rray double
#' @export
vec_type2.vctrs_rray.double <- vec_type2.vctrs_rray.vctrs_rray

#' @method vec_type2.double vctrs_rray
#' @export
vec_type2.double.vctrs_rray <- vec_type2.vctrs_rray.vctrs_rray

# vec_type2 vctrs_rray <-> integer/matrix/array --------------------------------

#' @method vec_type2.vctrs_rray integer
#' @export
vec_type2.vctrs_rray.integer <- vec_type2.vctrs_rray.vctrs_rray

#' @method vec_type2.integer vctrs_rray
#' @export
vec_type2.integer.vctrs_rray <- vec_type2.vctrs_rray.vctrs_rray

# vec_type2 vctrs_rray <-> logical/matrix/array --------------------------------

#' @method vec_type2.vctrs_rray logical
#' @export
vec_type2.vctrs_rray.logical <- vec_type2.vctrs_rray.vctrs_rray

#' @method vec_type2.logical vctrs_rray
#' @export
vec_type2.logical.vctrs_rray <- vec_type2.vctrs_rray.vctrs_rray

# vec_cast boilerplate ---------------------------------------------------------

#' @export
#' @rdname vctrs-compat
#' @method vec_cast vctrs_rray
#' @export vec_cast.vctrs_rray
vec_cast.vctrs_rray <- function(x, to) UseMethod("vec_cast.vctrs_rray")

#' @method vec_cast.vctrs_rray default
#' @export
vec_cast.vctrs_rray.default <- function(x, to) stop_incompatible_cast(x, to)

#' @method vec_cast.vctrs_rray logical
#' @export
vec_cast.vctrs_rray.logical <- function(x, to) vec_unspecified_cast(x, to)

# vec_cast vctrs_rray <-> vctrs_rray -------------------------------------------

# like vec_type2, vec_cast is ONLY about casting to a new type (class + shape)
# and has no regard for names

#' @method vec_cast.vctrs_rray vctrs_rray
#' @export
vec_cast.vctrs_rray.vctrs_rray <- function(x, to) {
  dim <- c(vec_size(x), rray_shape(to))
  res <- rray_broadcast_impl(x, dim)
  new_rray(
    .data = vec_cast(vec_data(res), vec_data(to)),
    size = vec_size(res),
    shape = rray_shape(res)
  )
}

# vec_cast vctrs_rray <-> double -----------------------------------------------

# double to vctrs_rray

#' @method vec_cast.vctrs_rray double
#' @export
vec_cast.vctrs_rray.double <- vec_cast.vctrs_rray.vctrs_rray

# vctrs_rray to double

#' @method vec_cast.double vctrs_rray
#' @export
vec_cast.double.vctrs_rray <- function(x, to) {
  dim <- c(vec_size(x), rray_shape(to))
  x <- rray_broadcast_impl(x, dim)
  x <- vec_cast(vec_data(x), double())
  array(x, dim = dim)
}

# vec_cast vctrs_rray <-> integer -----------------------------------------------

#' @method vec_cast.vctrs_rray integer
#' @export
vec_cast.vctrs_rray.integer <- vec_cast.vctrs_rray.vctrs_rray

#' @method vec_cast.integer vctrs_rray
#' @export
vec_cast.integer.vctrs_rray <- function(x, to) {
  dim <- c(vec_size(x), rray_shape(to))
  x <- rray_broadcast_impl(x, dim)
  x <- vec_cast(vec_data(x), integer())
  array(x, dim = dim)
}

# vec_cast vctrs_rray <-> logical -----------------------------------------------

#' @method vec_cast.vctrs_rray logical
#' @export
vec_cast.vctrs_rray.logical <- vec_cast.vctrs_rray.vctrs_rray

#' @method vec_cast.logical vctrs_rray
#' @export
vec_cast.logical.vctrs_rray <- function(x, to) {
  dim <- c(vec_size(x), rray_shape(to))
  x <- rray_broadcast_impl(x, dim)
  x <- vec_cast(vec_data(x), logical())
  array(x, dim = dim)
}
