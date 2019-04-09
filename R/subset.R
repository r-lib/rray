
rray_subset <- function(x, ...) {
  out <- vec_data(x)

  indexer <- rray_as_index(x, ...)

  out <- eval_bare(expr(out[!!!indexer]))

  vec_restore(out, x)
}

`rray_subset<-` <- function(x, ..., value) {
  x_subset <- x[...]
  value <- vec_cast(value, x_subset)
  value <- rray_broadcast(value, vec_dim(x_subset))

  indexer <- rray_as_index(x, ..., with_drop = FALSE)

  x_data <- vec_data(x)

  eval_bare(expr(x_data[!!!indexer] <- value))

  res <- vec_restore(x_data, x)

  res
}

rray_element <- function(x, ...) {
  out <- vec_data(x)

  indexer <- rray_as_index(x, ..., with_drop = FALSE)

  validate_element_indexer(indexer)

  out <- eval_bare(expr(out[[!!!indexer]]))

  vec_restore(out, x)
}

#' @export
`[.vctrs_rray` <- function(x, ..., drop = FALSE) {
  maybe_warn_drop(drop)
  rray_subset(x, ...)
}

#' @export
`[[.vctrs_rray` <- function(x, ..., exact = TRUE) {
  maybe_warn_exact(exact)
  rray_element(x, ...)
}

# - turn the dots into something to slice x with using expr(x[!!!dots])
# - optionally add `drop = FALSE` to the end
rray_as_index <- function(x, ..., with_drop = TRUE) {
  dots <- dots_list(..., .preserve_empty = TRUE, .ignore_empty = "none")
  n_dots <- length(dots)

  # 1D subsetting allowed, always flattens
  if (n_dots == 1L) {
    indexer <- as_flat_indexer(dots, x)
  }
  else {
    indexer <- as_indexer(dots, x)
  }

  if (with_drop) {
    indexer <- c(indexer, drop = FALSE)
  }

  indexer
}

as_indexer <- function(dots, x) {
  proxies <- map(vec_dim(x), seq_len)
  proxy_names <- dim_names(x)
  dots <- pad_missing(dots, x)

  # Set names on the proxy if the indexer is by name
  for (i in seq_along(proxies)) {
    if (is.character(dots[[i]])) {
      names(proxies[[i]]) <- proxy_names[[i]]
    }
  }

  indexer <- map2(dots, proxies, vec_as_index_wrapper)

  indexer
}

pad_missing <- function(dots, x) {
  x_dims <- vec_dims(x)
  requested_dims <- length(dots)

  if (requested_dims > x_dims) {
    glubort(
      "The dimensionality of `x` is {x_dims}. ",
      "Cannot subset into dimension {requested_dims}."
    )
  }

  # n_dots < d, need to pad with missing args
  # (if n_dots == d this does nothing)
  n_missing <- x_dims - requested_dims
  padding <- rep(list(missing_arg()), times = n_missing)
  dots <- c(dots, padding)

  dots
}

vec_as_index_wrapper <- function(i, x) {
  if (is_missing(i)) {
    missing_arg()
  }
  else {
    vctrs:::vec_as_index(i, x)
  }
}

as_flat_indexer <- function(dots, x) {

  i <- dots[[1]]

  if (is_missing(i)) {
    return(list2(missing_arg()))
  }

  if (is.logical(i)) {
    i <- as_flat_indexer_lgl(i, x)
  }
  else {
    i <- as_flat_indexer_default(i, x)
  }

  list(i)
}

as_flat_indexer_default <- function(i, x) {
  if (vec_dims(i) > 1L) {
    glubort("Subscript 1 can only have >1 dimensions if it is a logical.")
  }

  # Not looking at vctrs "size" here
  proxy <- seq_len(rray_elems(x))

  vctrs:::vec_as_index(i, proxy)
}

as_flat_indexer_lgl <- function(i, x) {
  i <- rray_broadcast(i, vec_dim(x))
  i <- as.vector(i)
  as_flat_indexer_default(i, x)
}

validate_element_indexer <- function(indexer) {

  missing_indexes <- map_lgl(indexer, is_missing)

  if (any(missing_indexes)) {
    missing_indexes <- glue::glue_collapse(which(missing_indexes), ", ")
    glubort(
      "Subscript(s) {missing_indexes} must not ",
      "be missing in single element subsetting."
    )
  }

  lengths <- map_int(indexer, length)
  is_one <- map_lgl(lengths, identical, 1L)

  # Allow for multiple bad subscripts
  if (any(!is_one)) {
    bad_subscript <- which(!is_one)
    bad_lengths <- lengths[!is_one]
    msg <- glue::glue(
      "Subscript {bad_subscript} must have size 1, not {bad_lengths}."
    )
    msg <- glue::glue_collapse(msg, sep ="\n")
    glubort(msg)
  }

  invisible(indexer)
}

maybe_warn_exact <- function(exact) {
  if (!exact) {
    warn_exact()
  }
}

warn_exact <- function() {
  rlang::warn("`exact` ignored.")
}

maybe_warn_drop <- function(drop) {
  if (drop) {
    warn_drop()
  }
  invisible(drop)
}

warn_drop <- function() {
  rlang::warn("`drop` ignored.")
}

# ------------------------------------------------------------------------------

# Override the vctrs `[<-` because it does not allow you to pass in more than
# just i AND it calls vec_cast() where `to` is the full x obj, not just on the slice
# you are casting to
# If vctrs did: function(x, ..., value) and vec_cast(value, x[...]) then
# all would be good

#' @export
`[<-.vctrs_rray` <- function(x, ..., value) {
  rray_subset(x, ...) <- value
  x
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

# ------------------------------------------------------------------------------

#' @export
head.vctrs_rray <- function (x, n = 6L, ...) {

  n_size <- vec_size(n)
  if (vec_size(n) != 1L) {
    glubort("`n` must be size 1, not {n_size}.")
  }

  n <- vec_cast(n, integer())

  x_size <- vec_size(x)

  if (n < 0L) {
    n <- max(x_size + n, 0L)
  }
  else {
    n <- min(n, x_size)
  }

  x[seq_len(n),]
}

#' @export
tail.vctrs_rray <- function(x, n = 6L, ...) {

  n_size <- vec_size(n)
  if (vec_size(n) != 1L) {
    glubort("`n` must be size 1, not {n_size}.")
  }

  n <- vec_cast(n, integer())

  x_size <- vec_size(x)

  if (n < 0L) {
    n <- max(x_size + n, 0L)
  }
  else {
    n <- min(n, x_size)
  }

  x[seq.int(to = x_size, length.out = n),]
}
