devtools::load_all(quiet = TRUE)

cat("\nOne-dimensional\n")

local({
  x <- array(seq_len(1e6L), 1e6L)
  i <- rev(seq_len(1e6L))

  gc()
  print(bench::mark(
    rray = rray_index(x, i),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nOne-dimensional with missing locations\n")

local({
  x <- array(seq_len(1e6L), 1e6L)
  i <- rev(seq_len(1e6L))
  i[[1L]] <- NA_integer_

  gc()
  print(bench::mark(
    rray = rray_index(x, i),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nTwo-dimensional pointwise\n")

local({
  x <- array(seq_len(1e6L), c(1000L, 1000L))
  i <- rep(rev(seq_len(1000L)), times = 1000L)
  j <- rep(rev(seq_len(1000L)), each = 1000L)

  gc()
  print(bench::mark(
    rray = rray_index(x, i, j),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nTwo-dimensional pointwise with missing locations\n")

local({
  x <- array(seq_len(1e6L), c(1000L, 1000L))
  i <- rep(rev(seq_len(1000L)), times = 1000L)
  i[[1L]] <- NA_integer_
  j <- rep(rev(seq_len(1000L)), each = 1000L)

  gc()
  print(bench::mark(
    rray = rray_index(x, i, j),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nTwo-dimensional scalar\n")

local({
  x <- array(seq_len(1e6L), c(1000L, 1000L))
  i <- 500L
  j <- rep(rev(seq_len(1000L)), times = 1000L)

  gc()
  print(bench::mark(
    rray = rray_index(x, i, j),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nTwo-dimensional product\n")

local({
  x <- array(seq_len(1e6L), c(1000L, 1000L))
  i <- array(rev(seq_len(1000L)), c(1000L, 1L))
  j <- array(rev(seq_len(1000L)), c(1L, 1000L))

  gc()
  print(bench::mark(
    rray = rray_index(x, i, j),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nTwo-dimensional product with missing locations\n")

local({
  x <- array(seq_len(1e6L), c(1000L, 1000L))
  i <- array(rev(seq_len(1000L)), c(1000L, 1L))
  i[[1L]] <- NA_integer_
  j <- array(rev(seq_len(1000L)), c(1L, 1000L))

  gc()
  print(bench::mark(
    rray = rray_index(x, i, j),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nTwo-dimensional product with short runs\n")

local({
  x <- array(seq_len(1e6L), c(10L, 100000L))
  i <- array(rev(seq_len(10L)), c(10L, 1L))
  j <- array(rev(seq_len(100000L)), c(1L, 100000L))

  gc()
  print(bench::mark(
    rray = rray_index(x, i, j),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nTwo-dimensional product with leading unit axis\n")

local({
  x <- array(seq_len(1e6L), c(1000L, 1000L))
  i <- array(rev(seq_len(1000L)), c(1L, 1000L, 1L))
  j <- array(rev(seq_len(1000L)), c(1L, 1L, 1000L))

  gc()
  print(bench::mark(
    rray = rray_index(x, i, j),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nThree-dimensional pointwise\n")

local({
  x <- array(seq_len(1e6L), c(100L, 100L, 100L))
  i <- rep(rev(seq_len(100L)), times = 10000L)
  j <- rep(rep(rev(seq_len(100L)), each = 100L), times = 100L)
  k <- rep(rev(seq_len(100L)), each = 10000L)

  gc()
  print(bench::mark(
    rray = rray_index(x, i, j, k),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nThree-dimensional product\n")

local({
  x <- array(seq_len(1e6L), c(100L, 100L, 100L))
  i <- array(rev(seq_len(100L)), c(100L, 1L, 1L))
  j <- array(rev(seq_len(100L)), c(1L, 100L, 1L))
  k <- array(rev(seq_len(100L)), c(1L, 1L, 100L))

  gc()
  print(bench::mark(
    rray = rray_index(x, i, j, k),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nThree-dimensional product with missing locations\n")

local({
  x <- array(seq_len(1e6L), c(100L, 100L, 100L))
  i <- array(rev(seq_len(100L)), c(100L, 1L, 1L))
  j <- array(rev(seq_len(100L)), c(1L, 100L, 1L))
  j[[1L]] <- NA_integer_
  k <- array(rev(seq_len(100L)), c(1L, 1L, 100L))

  gc()
  print(bench::mark(
    rray = rray_index(x, i, j, k),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nThree-dimensional mixed\n")

local({
  x <- array(seq_len(1e6L), c(100L, 100L, 100L))
  i <- array(rep(rev(seq_len(100L)), times = 100L), c(100L, 100L, 1L))
  j <- array(rep(seq_len(100L), each = 100L), c(100L, 100L, 1L))
  k <- array(rev(seq_len(100L)), c(1L, 1L, 100L))

  gc()
  print(bench::mark(
    rray = rray_index(x, i, j, k),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nFour-dimensional product\n")

local({
  x <- array(seq_len(1048576L), c(32L, 32L, 32L, 32L))
  i <- array(rev(seq_len(32L)), c(32L, 1L, 1L, 1L))
  j <- array(rev(seq_len(32L)), c(1L, 32L, 1L, 1L))
  k <- array(rev(seq_len(32L)), c(1L, 1L, 32L, 1L))
  l <- array(rev(seq_len(32L)), c(1L, 1L, 1L, 32L))

  gc()
  print(bench::mark(
    rray = rray_index(x, i, j, k, l),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nFour-dimensional product with missing locations\n")

local({
  x <- array(seq_len(1048576L), c(32L, 32L, 32L, 32L))
  i <- array(rev(seq_len(32L)), c(32L, 1L, 1L, 1L))
  j <- array(rev(seq_len(32L)), c(1L, 32L, 1L, 1L))
  k <- array(rev(seq_len(32L)), c(1L, 1L, 32L, 1L))
  l <- array(rev(seq_len(32L)), c(1L, 1L, 1L, 32L))
  l[[1L]] <- NA_integer_

  gc()
  print(bench::mark(
    rray = rray_index(x, i, j, k, l),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nFive-dimensional fallback\n")

local({
  x <- array(seq_len(1e6L), c(10L, 10L, 10L, 10L, 100L))
  i <- array(rev(seq_len(10L)), c(10L, 1L, 1L, 1L, 1L))
  j <- array(rev(seq_len(10L)), c(1L, 10L, 1L, 1L, 1L))
  k <- array(rev(seq_len(10L)), c(1L, 1L, 10L, 1L, 1L))
  l <- array(rev(seq_len(10L)), c(1L, 1L, 1L, 10L, 1L))
  m <- array(rev(seq_len(100L)), c(1L, 1L, 1L, 1L, 100L))

  gc()
  print(bench::mark(
    rray = rray_index(x, i, j, k, l, m),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nFive-dimensional fallback with missing locations\n")

local({
  x <- array(seq_len(1e6L), c(10L, 10L, 10L, 10L, 100L))
  i <- array(rev(seq_len(10L)), c(10L, 1L, 1L, 1L, 1L))
  j <- array(rev(seq_len(10L)), c(1L, 10L, 1L, 1L, 1L))
  k <- array(rev(seq_len(10L)), c(1L, 1L, 10L, 1L, 1L))
  l <- array(rev(seq_len(10L)), c(1L, 1L, 1L, 10L, 1L))
  m <- array(rev(seq_len(100L)), c(1L, 1L, 1L, 1L, 100L))
  m[[1L]] <- NA_integer_

  gc()
  print(bench::mark(
    rray = rray_index(x, i, j, k, l, m),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nLogical storage\n")

local({
  x <- array(rep(c(TRUE, FALSE, NA), length.out = 100000L), c(100L, 1000L))
  i <- array(rev(seq_len(100L)), c(100L, 1L))
  j <- array(rev(seq_len(1000L)), c(1L, 1000L))

  gc()
  print(bench::mark(
    rray = rray_index(x, i, j),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nDouble storage\n")

local({
  x <- array(seq_len(100000L) / 3, c(100L, 1000L))
  i <- array(rev(seq_len(100L)), c(100L, 1L))
  j <- array(rev(seq_len(1000L)), c(1L, 1000L))

  gc()
  print(bench::mark(
    rray = rray_index(x, i, j),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nComplex storage\n")

local({
  x <- array(as.complex(seq_len(100000L)), c(100L, 1000L))
  i <- array(rev(seq_len(100L)), c(100L, 1L))
  j <- array(rev(seq_len(1000L)), c(1L, 1000L))

  gc()
  print(bench::mark(
    rray = rray_index(x, i, j),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nRaw storage\n")

local({
  x <- array(as.raw(seq_len(100000L) %% 256L), c(100L, 1000L))
  i <- array(rev(seq_len(100L)), c(100L, 1L))
  j <- array(rev(seq_len(1000L)), c(1L, 1000L))

  gc()
  print(bench::mark(
    rray = rray_index(x, i, j),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nCharacter storage\n")

local({
  x <- array(as.character(seq_len(100000L)), c(100L, 1000L))
  i <- array(rev(seq_len(100L)), c(100L, 1L))
  j <- array(rev(seq_len(1000L)), c(1L, 1000L))

  gc()
  print(bench::mark(
    rray = rray_index(x, i, j),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nList storage\n")

local({
  x <- array(as.list(seq_len(100000L)), c(100L, 1000L))
  i <- array(rev(seq_len(100L)), c(100L, 1L))
  j <- array(rev(seq_len(1000L)), c(1L, 1000L))

  gc()
  print(bench::mark(
    rray = rray_index(x, i, j),
    iterations = 20L,
    memory = FALSE
  ))
})
