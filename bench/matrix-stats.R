devtools::load_all(quiet = TRUE)

if (!requireNamespace("matrixStats", quietly = TRUE)) {
  stop("`matrixStats` must be installed to run this benchmark.", call. = FALSE)
}

size <- as.integer(Sys.getenv("RRAY_BENCH_SIZE", "1000000"))
iterations <- as.integer(Sys.getenv("RRAY_BENCH_ITERATIONS", "15"))
output <- Sys.getenv("RRAY_BENCH_OUTPUT", "")

side <- as.integer(sqrt(size))
if (side * side != size) {
  stop("`RRAY_BENCH_SIZE` must be a perfect square.", call. = FALSE)
}

dimensions <- c(side, side)

cases <- expand.grid(
  operation = c("sum", "product", "all", "any"),
  na_rm = c(FALSE, TRUE),
  missing = c("none", "sparse", "dense"),
  direction = c("columns", "rows"),
  stringsAsFactors = FALSE
)

make_input <- function(operation, missing) {
  x <- switch(
    operation,
    sum = as.double(seq_len(size) %% 1000L),
    product = 1 + as.double(seq_len(size) %% 10L) / 1000,
    all = rep(TRUE, size),
    any = rep(FALSE, size)
  )

  if (missing != "none") {
    step <- if (missing == "sparse") 101L else 2L
    x[seq.int(1L, size, by = step)] <- NA
  }

  array(x, dimensions)
}

functions <- list(
  sum = list(
    rray = rray_sum_along,
    columns = matrixStats::colSums2,
    rows = matrixStats::rowSums2
  ),
  product = list(
    rray = rray_product_along,
    columns = matrixStats::colProds,
    rows = matrixStats::rowProds
  ),
  all = list(
    rray = rray_all_along,
    columns = matrixStats::colAlls,
    rows = matrixStats::rowAlls
  ),
  any = list(
    rray = rray_any_along,
    columns = matrixStats::colAnys,
    rows = matrixStats::rowAnys
  )
)

results <- vector("list", nrow(cases))

for (i in seq_len(nrow(cases))) {
  case <- cases[i, ]
  x <- make_input(case$operation, case$missing)
  fns <- functions[[case$operation]]
  axis <- if (case$direction == "columns") 1L else 2L
  matrix_stats <- fns[[case$direction]]

  rray <- fns$rray(x, axis, na_rm = case$na_rm)
  reference <- matrix_stats(x, na.rm = case$na_rm, useNames = FALSE)
  if (!isTRUE(all.equal(as.vector(rray), reference))) {
    stop("The rray and matrixStats results differ.", call. = FALSE)
  }

  measurement <- bench::mark(
    rray = fns$rray(x, axis, na_rm = case$na_rm),
    matrix_stats = matrix_stats(x, na.rm = case$na_rm, useNames = FALSE),
    iterations = iterations,
    check = FALSE,
    memory = FALSE,
    filter_gc = FALSE
  )

  results[[i]] <- cbind(
    case,
    rray_median_ns = as.numeric(measurement$median[[1L]]) * 1e9,
    matrix_stats_median_ns = as.numeric(measurement$median[[2L]]) * 1e9,
    ratio = as.numeric(measurement$median[[1L]]) /
      as.numeric(measurement$median[[2L]])
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
      rray_revision = system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
      matrix_stats_version = as.character(utils::packageVersion("matrixStats"))
    ),
    output
  )
}
