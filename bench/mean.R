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

make_input <- function(missing) {
  x <- as.double(seq_len(size)) / 7
  x <- add_missing(x, missing)
  array(x, dimensions)
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
  x <- make_input(case$missing)
  axes <- switch(case$axes, axis1 = 1L, axis2 = 2L, both = c(1L, 2L))
  base <- base_fn(case$axes)

  measurement <- switch(
    case$implementation,
    mean = bench::mark(
      rray_mean_along(x, axes, na_rm = case$na_rm),
      iterations = iterations,
      check = FALSE,
      memory = FALSE,
      filter_gc = FALSE
    ),
    sum = bench::mark(
      rray_sum_along(x, axes, na_rm = case$na_rm),
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
