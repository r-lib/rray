native_ptypes <- list(
  lgl = logical(),
  int = integer(),
  dbl = double(),
  cpl = complex(),
  chr = character(),
  raw = raw(),
  list = list()
)

native_ptype_matrix <- function(fn, labels) {
  types <- names(native_ptypes)

  dimnames <- list(types, types)
  names(dimnames) <- labels

  out <- matrix(
    NA_character_,
    length(types),
    length(types),
    dimnames = dimnames
  )

  for (x in types) {
    for (y in types) {
      out[x, y] <- tryCatch(
        typeof(fn(native_ptypes[[x]], native_ptypes[[y]])),
        error = function(cnd) NA_character_
      )
    }
  }

  out
}
