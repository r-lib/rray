new_vctr2 <- function(.data, ..., class = character()) {
  if (!is_vector(.data)) {
    abort("`.data` must be a vector type.")
  }

  class <- c(class, "vctrs_vctr")
  names <- validate_names(.data)

  vec_structure(.data, names = names, ..., class = class)
}

validate_names <- function(.data) {
  names <- names(.data)

  if (is.null(names)) {
    return(NULL)
  }

  if (!all(names != "" & !is.na(names))) {
    abort("If any elements of `.data` are named, all must be named")
  }

  names
}

# ------------------------------------------------------------------------------

# TODO - remove this if vctrs improves

vec_data_fast <- function(x) {
  if (!is_vector(x)) {
    x
  }
  # else if (is_record(x)) {
  #   attributes(x) <- list(names = fields(x))
  # }
  else if (has_dim(x)) {
    `attributes<-`(x, value = list(dim = dim(x), dimnames = dimnames(x)))
  }
  else {
    `attributes<-`(x, value = list(names = names(x)))
  }
}

has_dim <- function (x) {
  !is.null(attr(x, "dim"))
}

# ------------------------------------------------------------------------------

# For R < 3.6.0, a shim around `structure()` that doesn't make two
# copies of `.Data` while setting attributes. It calls `attributes<-()`
# using the functional form directly rather than `attributes(x) <- list(...)`
# to avoid the double copy. Otherwise it is identical to `structure()`.
structure_shim <- function (.Data, ...) {

  if (is.null(.Data)) {
    warning("Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.\n  Consider 'structure(list(), *)' instead.")
  }

  attrib <- list(...)

  if (length(attrib)) {
    specials <- c(".Dim", ".Dimnames", ".Names", ".Tsp", ".Label")
    replace <- c("dim", "dimnames", "names", "tsp", "levels")

    m <- match(names(attrib), specials)
    ok <- !is.na(m)
    names(attrib)[ok] <- replace[m[ok]]

    if ("factor" %in% attrib[["class", exact = TRUE]] && typeof(.Data) == "double") {
      storage.mode(.Data) <- "integer"
    }

    `attributes<-`(.Data, value = c(attributes(.Data), attrib))
  }
  else {
    .Data
  }

}
