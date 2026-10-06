devtools::load_all(quiet = TRUE)

size <- as.integer(Sys.getenv("RRAY_BENCH_SIZE", "1000000"))
iterations <- as.integer(Sys.getenv("RRAY_BENCH_ITERATIONS", "15"))
output <- Sys.getenv("RRAY_BENCH_OUTPUT", "")

side <- as.integer(sqrt(size))
if (side * side != size) {
  stop("`RRAY_BENCH_SIZE` must be a perfect square.", call. = FALSE)
}

dimensions <- c(side, side)

cases <- expand.grid(
  implementation = c("mean", "sum", "base"),
  type = c("lgl", "int", "int128", "dbl", "inf"),
  na_rm = c(FALSE, TRUE),
  missing = c("none", "sparse"),
  axes = c("axis1", "axis2", "both"),
  stringsAsFactors = FALSE
)

add_missing <- function(x, missing) {
  if (missing == "none") {
    return(x)
  }

  x[seq.int(1L, length(x), by = 101L)] <- NA
  x
}

make_input <- function(type, missing) {
  x <- switch(
    type,
    lgl = rep_len(c(TRUE, FALSE, TRUE), size),
    int = rep_len(1:1000, size),
    int128 = rep_len(1:1000, size),
    dbl = as.double(seq_len(size)) / 7,
    inf = rep_len(c(.Machine$double.xmax, 1), size)
  )
  x <- add_missing(x, missing)
  array(x, dimensions)
}

mean_fn <- function(type) {
  if (type != "int128") {
    return(rray_mean)
  }

  function(x, axes, na_rm) {
    .Call(ffi_test_rray_mean_forced_fallback, x, axes, na_rm, environment())
  }
}

sum_fn <- function(type) {
  if (type != "int128") {
    return(rray_sum)
  }

  function(x, axes, na_rm) {
    .Call(ffi_test_rray_sum_forced_fallback, x, axes, na_rm, environment())
  }
}

base_fn <- function(axes) {
  switch(
    axes,
    axis1 = colMeans,
    axis2 = rowMeans,
    both = function(x, na.rm) mean(x, na.rm = na.rm)
  )
}

results <- vector("list", nrow(cases))

for (i in seq_len(nrow(cases))) {
  case <- cases[i, ]
  x <- make_input(case$type, case$missing)
  axes <- switch(case$axes, axis1 = 1L, axis2 = 2L, both = c(1L, 2L))
  rray_mean_impl <- mean_fn(case$type)
  rray_sum_impl <- sum_fn(case$type)
  base <- base_fn(case$axes)

  measurement <- switch(
    case$implementation,
    mean = bench::mark(
      rray_mean_impl(x, axes, na_rm = case$na_rm),
      iterations = iterations,
      check = FALSE,
      memory = FALSE,
      filter_gc = FALSE
    ),
    sum = bench::mark(
      rray_sum_impl(x, axes, na_rm = case$na_rm),
      iterations = iterations,
      check = FALSE,
      memory = FALSE,
      filter_gc = FALSE
    ),
    base = bench::mark(
      base(x, na.rm = case$na_rm),
      iterations = iterations,
      check = FALSE,
      memory = FALSE,
      filter_gc = FALSE
    )
  )

  results[[i]] <- cbind(
    case,
    median_ns = as.numeric(measurement$median) * 1e9,
    itr_per_sec = as.numeric(measurement$`itr/sec`)
  )
}

results <- do.call(rbind, results)
row.names(results) <- NULL

print(results, row.names = FALSE)

if (nzchar(output)) {
  saveRDS(
    list(
      results = results,
      size = size,
      iterations = iterations,
      revision = system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
    ),
    output
  )
}
