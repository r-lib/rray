# Printing seems to be broken if I try and use the
# format method for array objects (it right aligns characters but not column
# headers). In the long run, we
# probably want our own format method that doesn't use the array
# one. For now, call print(vec_data(x)) in obj_print_data() to have
# the alignment be correct. For an example of bad behavior, print
# as_rray(sh8) from ?solve with the format method

# format.vctrs_rray <- function(x, ...) {
#   format(as_array(x))
# }

#' @export
obj_print_data.vctrs_rray <- function(x, ...) {
  # vctrs sets names() here which is problematic for 1D arrays
  print(vec_data(x))
  invisible(x)
}

#' @export
vec_ptype_abbr.vctrs_rray <- function(x, ...) {
  "rray"
}

#' @export
vec_ptype_full.vctrs_rray <- function(x, ...) {
  paste0("rray<", rray_inner_ptype_abbr(x), ">", vec_ptype_shape(x))
}

# from vctrs
vec_ptype_shape <- function(x) {
  dim <- rray_dim(x)
  if (length(dim) == 1) {
    ""
  } else {
    paste0("[,", paste(dim[-1], collapse = ","), "]")
  }
}

rray_inner_ptype_abbr <- function(x) {
  if (is_rray_int(x)) {
    "int"
  }
  else if (is_rray_dbl(x)) {
    "dbl"
  }
  else if (is_rray_lgl(x)) {
    "lgl"
  }
  else {
    "unknown"
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
