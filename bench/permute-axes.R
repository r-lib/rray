devtools::load_all(quiet = TRUE)

cat("\nTranspose\n")

local({
  x <- array(runif(16e6L), c(4000L, 4000L))

  gc()
  print(bench::mark(
    rray = rray_permute_axes(x, c(2L, 1L)),
    base = aperm(x, c(2L, 1L)),
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nTranspose, short runs\n")

local({
  x <- array(runif(1e7L), c(1e6L, 10L))

  gc()
  print(bench::mark(
    rray = rray_permute_axes(x, c(2L, 1L)),
    base = aperm(x, c(2L, 1L)),
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nTranspose, long runs\n")

local({
  x <- array(runif(1e7L), c(10L, 1e6L))

  gc()
  print(bench::mark(
    rray = rray_permute_axes(x, c(2L, 1L)),
    base = aperm(x, c(2L, 1L)),
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nTranspose, integer\n")

local({
  x <- array(sample(16e6L), c(4000L, 4000L))

  gc()
  print(bench::mark(
    rray = rray_permute_axes(x, c(2L, 1L)),
    base = aperm(x, c(2L, 1L)),
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nTranspose, character\n")

local({
  x <- array(as.character(sample(1000L, 1e6L, TRUE)), c(1000L, 1000L))

  gc()
  print(bench::mark(
    rray = rray_permute_axes(x, c(2L, 1L)),
    base = aperm(x, c(2L, 1L)),
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nKeep the first axis\n")

local({
  x <- array(runif(8e6L), c(200L, 200L, 200L))

  gc()
  print(bench::mark(
    rray = rray_permute_axes(x, c(1L, 3L, 2L)),
    base = aperm(x, c(1L, 3L, 2L)),
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nSwap the first two axes\n")

local({
  x <- array(runif(8e6L), c(200L, 200L, 200L))

  gc()
  print(bench::mark(
    rray = rray_permute_axes(x, c(2L, 1L, 3L)),
    base = aperm(x, c(2L, 1L, 3L)),
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nReverse the axes\n")

local({
  x <- array(runif(8e6L), c(200L, 200L, 200L))

  gc()
  print(bench::mark(
    rray = rray_permute_axes(x, c(3L, 2L, 1L)),
    base = aperm(x, c(3L, 2L, 1L)),
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nReverse the axes, short runs\n")

local({
  x <- array(runif(8e6L), c(2000L, 2000L, 2L))

  gc()
  print(bench::mark(
    rray = rray_permute_axes(x, c(3L, 2L, 1L)),
    base = aperm(x, c(3L, 2L, 1L)),
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nUnit axes\n")

local({
  x <- array(runif(1e7L), c(1e7L, 1L))

  gc()
  print(bench::mark(
    rray = rray_permute_axes(x, c(2L, 1L)),
    base = aperm(x, c(2L, 1L)),
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})
