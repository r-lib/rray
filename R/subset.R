
rray_subset <- function(x, ...) {
  out <- vec_data(x)

  indexer <- rray_as_index(x, ...)

  out <- eval_bare(expr(out[!!!indexer]))

  vec_restore(out, x)
}

#' @export
`[.vctrs_rray` <- function(x, ..., drop = FALSE) {
  maybe_warn_drop(drop)
  rray_subset(x, ...)
}

`rray_subset<-` <- function(x, ..., value) {
  x_subset <- rray_subset(x, ...)
  value <- vec_cast(value, x_subset)
  value <- rray_broadcast(value, vec_dim(x_subset))

  indexer <- rray_as_index(x, ..., with_drop = FALSE)

  x_data <- vec_data(x)

  eval_bare(expr(x_data[!!!indexer] <- value))

  res <- vec_restore(x_data, x)

  res
}

#' @export
`[<-.vctrs_rray` <- function(x, ..., value) {
  rray_subset(x, ...) <- value
  x
}

# ------------------------------------------------------------------------------

rray_slice <- function(x, i, axis) {
  validate_axis(axis, x)
  indexer <- front_pad(i, axis)
  rray_subset(x, !!!indexer)
}

`rray_slice<-` <- function(x, i, axis, value) {
  validate_axis(axis, x)
  indexer <- front_pad(i, axis)
  rray_subset(x, !!!indexer) <- value
  x
}

front_pad <- function(i, axis) {
  padding <- rep(list(missing_arg()), times = axis - 1L)
  c(padding, list(i))
}

# ------------------------------------------------------------------------------

rray_yank <- function(x, i) {
  i <- maybe_missing(i, TRUE)

  out <- vec_data(x)

  indexer <- as_yank_indexer(i, x)

  out <- eval_bare(expr(out[!!!indexer]))

  vec_restore(out, x)
}

`rray_yank<-` <- function(x, i, value) {
  i <- maybe_missing(i, TRUE)

  x_yank <- rray_yank(x, i)
  value <- vec_cast(value, x_yank)
  value <- rray_broadcast(value, vec_dim(x_yank))

  out <- vec_data(x)

  indexer <- as_yank_indexer(i, x)

  eval_bare(expr(out[!!!indexer] <- value))

  vec_restore(out, x)
}

# ------------------------------------------------------------------------------

# can only be used like (with 2D for example)
# x[[1, 1]] not like x[[1]]
# (i.e. must fully qualify pluck indices)

rray_pluck <- function(x, ...) {
  out <- vec_data(x)

  indexer <- rray_as_index(x, ..., with_drop = FALSE)

  validate_pluck_indexer(indexer)

  out <- eval_bare(expr(out[[!!!indexer]]))

  vec_restore(out, x)
}

#' @export
`[[.vctrs_rray` <- function(x, ..., exact = TRUE) {
  maybe_warn_exact(exact)
  rray_pluck(x, ...)
}

`rray_pluck<-` <- function(x, ..., value) {

  x_pluck <- rray_pluck(x, ...)
  value <- vec_cast(value, x_pluck)

  value_size <- vec_size(value)
  if (value_size != 1L) {
    glubort("The size of `value` must be 1, not {value_size}.")
  }

  out <- vec_data(x)

  indexer <- rray_as_index(x, ..., with_drop = FALSE)

  eval_bare(expr(out[!!!indexer] <- value))

  vec_restore(out, x)
}

#' @export
`[[<-.vctrs_rray` <- function(x, ..., value) {
  rray_pluck(x, ...) <- value
  x
}

# ------------------------------------------------------------------------------

rray_as_index <- function(x, ..., with_drop = TRUE) {
  dots <- dots_list(..., .preserve_empty = TRUE, .ignore_empty = "trailing")

  indexer <- as_indexer(dots, x)

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

# ------------------------------------------------------------------------------

as_yank_indexer <- function(i, x) {

  if (is.logical(i)) {
    i <- as_yank_indexer_lgl(i, x)
  }
  else if (is.character(i)) {
    glubort("Cannot yank with a character `i`.")
  }
  else {
    i <- as_yank_indexer_default(i, x)
  }

  list(i)
}

as_yank_indexer_default <- function(i, x) {

  if (vec_dims(i) > 1L) {
    glubort("`i` can only have >1 dimensions if it is a logical.")
  }

  # Not looking at vctrs "size" here
  proxy <- seq_len(rray_elems(x))

  vctrs:::vec_as_index(i, proxy)
}

as_yank_indexer_lgl <- function(i, x) {

  ok <- vec_dims(i) == 1L || identical(vec_dim(i), vec_dim(x))
  if (!ok) {
    glubort("A logical `i` must be 1D or have dimensions identical to `x`.")
  }

  i <- as.vector(i)

  as_yank_indexer_default(i, x)
}

# ------------------------------------------------------------------------------

validate_pluck_indexer <- function(indexer) {

  missing_indexes <- map_lgl(indexer, is_missing)

  if (any(missing_indexes)) {
    missing_indexes <- glue::glue_collapse(which(missing_indexes), ", ")
    glubort(
      "Subscript(s) {missing_indexes} must not ",
      "be missing in a pluck."
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

# ------------------------------------------------------------------------------

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
