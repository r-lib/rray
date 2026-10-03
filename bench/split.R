devtools::load_all(quiet = TRUE)

cat("\nRows\n")

local({
  x <- array(runif(1e6L), c(1000L, 1000L))

  gc()
  print(bench::mark(
    rray = rray_split(x, axis = 1L, dimensions = 100L),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nColumns\n")

local({
  x <- array(runif(1e6L), c(1000L, 1000L))

  gc()
  print(bench::mark(
    rray = rray_split(x, axis = 2L, dimensions = 100L),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nRows with short runs\n")

local({
  x <- array(runif(1e6L), c(10L, 100000L))

  gc()
  print(bench::mark(
    rray = rray_split(x, axis = 1L, dimensions = 5L),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nMany small columns\n")

local({
  x <- array(runif(1e6L), c(10L, 100000L))

  gc()
  print(bench::mark(
    rray = rray_split(x, axis = 2L, dimensions = 1L),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nMany small rows\n")

local({
  x <- array(runif(1e6L), c(100000L, 10L))

  gc()
  print(bench::mark(
    rray = rray_split(x, axis = 1L, dimensions = 1L),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nMany small columns with row names\n")

local({
  x <- array(runif(1e6L), c(10L, 100000L))
  dimnames(x) <- list(letters[1:10], NULL)

  gc()
  print(bench::mark(
    rray = rray_split(x, axis = 2L, dimensions = 1L),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nMany small columns with column names\n")

local({
  x <- array(runif(1e6L), c(10L, 100000L))
  dimnames(x) <- list(NULL, as.character(seq_len(100000L)))

  gc()
  print(bench::mark(
    rray = rray_split(x, axis = 2L, dimensions = 1L),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nExplicit dimensions\n")

local({
  x <- array(runif(1e6L), c(1000L, 1000L))
  dimensions <- rep(c(1L, 3L), times = 250L)

  gc()
  print(bench::mark(
    rray = rray_split(x, axis = 2L, dimensions = dimensions),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nThree-dimensional\n")

local({
  x <- array(runif(1e6L), c(100L, 100L, 100L))

  gc()
  print(bench::mark(
    rray = rray_split(x, axis = 2L, dimensions = 10L),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nInteger storage\n")

local({
  x <- array(seq_len(1e6L), c(1000L, 1000L))

  gc()
  print(bench::mark(
    rray = rray_split(x, axis = 1L, dimensions = 100L),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nCharacter storage\n")

local({
  x <- array(as.character(seq_len(100000L)), c(1000L, 100L))

  gc()
  print(bench::mark(
    rray = rray_split(x, axis = 1L, dimensions = 100L),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nList storage\n")

local({
  x <- array(as.list(seq_len(100000L)), c(1000L, 100L))

  gc()
  print(bench::mark(
    rray = rray_split(x, axis = 1L, dimensions = 100L),
    iterations = 20L,
    memory = FALSE
  ))
})
