compact <- function (x) {
  is_null <- map_lgl(x, is.null)
  x[!is_null]
}

glubort <- function(..., .sep = "", .envir = parent.frame()) {
  abort(glue::glue(..., .sep = .sep, .envir = .envir))
}

# we need a helper to just set the dim because we
# override dim<- to broadcast. Otherwise without this
# we get an infinite loop because broadcast would call dim<-
set_dim <- function(x, dim) {

  # potentially avoid copy
  if (!identical(vec_dim(x), dim)) {
    attr(x, "dim") <- dim
  }

  x
}
