compact <- function (x) {
  is_null <- map_lgl(x, is.null)
  x[!is_null]
}

glubort <- function(..., .sep = "", .envir = parent.frame()) {
  abort(glue::glue(..., .sep = .sep, .envir = .envir))
}

as_cpp_idx <- function(x) {

  if (is.null(x)) {
    return(x)
  }

  x - 1L
}
