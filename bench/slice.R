devtools::load_all(quiet = TRUE)

cat("\nOne-dimensional selection\n")

local({
  x <- array(seq_len(1e6L), 1e6L)
  index <- seq(1L, 1e6L, by = 2L)

  gc()
  print(bench::mark(
    rray = rray_slice(x, index),
    base = x[index, drop = FALSE],
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nLeading unit matrix\n")

local({
  x <- array(seq_len(1e6L), c(1L, 1e6L))
  index <- rev(seq_len(1e6L))

  gc()
  print(bench::mark(
    rray = rray_slice(x, TRUE, index),
    base = x[, index, drop = FALSE],
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nLeading unit matrix with missing locations\n")

local({
  x <- array(seq_len(1e6L), c(1L, 1e6L))
  index <- rev(seq_len(1e6L))
  index[[1L]] <- NA_integer_

  gc()
  print(bench::mark(
    rray = rray_slice(x, TRUE, index),
    base = x[, index, drop = FALSE],
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nThree-dimensional carry\n")

local({
  x <- array(seq_len(1e6L), c(1L, 1000L, 1000L))
  j <- rev(seq_len(1000L))
  k <- rev(seq_len(1000L))

  gc()
  print(bench::mark(
    rray = rray_slice(x, TRUE, j, k),
    base = x[, j, k, drop = FALSE],
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nThree-dimensional carry with missing locations\n")

local({
  x <- array(seq_len(1e6L), c(1L, 1000L, 1000L))
  j <- rev(seq_len(1000L))
  j[[1L]] <- NA_integer_
  k <- rev(seq_len(1000L))

  gc()
  print(bench::mark(
    rray = rray_slice(x, TRUE, j, k),
    base = x[, j, k, drop = FALSE],
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nFour-dimensional carry\n")

local({
  x <- array(seq_len(1e6L), c(1L, 100L, 100L, 100L))
  j <- rev(seq_len(100L))
  k <- rev(seq_len(100L))
  l <- rev(seq_len(100L))

  gc()
  print(bench::mark(
    rray = rray_slice(x, TRUE, j, k, l),
    base = x[, j, k, l, drop = FALSE],
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nFour-dimensional carry with missing locations\n")

local({
  x <- array(seq_len(1e6L), c(1L, 100L, 100L, 100L))
  j <- rev(seq_len(100L))
  j[[1L]] <- NA_integer_
  k <- rev(seq_len(100L))
  l <- rev(seq_len(100L))

  gc()
  print(bench::mark(
    rray = rray_slice(x, TRUE, j, k, l),
    base = x[, j, k, l, drop = FALSE],
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nLong first axis\n")

local({
  x <- array(seq_len(1024000L), c(256L, 125L, 32L))
  j <- rev(seq_len(125L))
  k <- rev(seq_len(32L))

  gc()
  print(bench::mark(
    rray = rray_slice(x, TRUE, j, k),
    base = x[, j, k, drop = FALSE],
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nFive-dimensional fallback\n")

local({
  x <- array(seq_len(1e6L), c(1L, 10L, 10L, 10L, 1000L))
  j <- rev(seq_len(10L))
  k <- rev(seq_len(10L))
  l <- rev(seq_len(10L))
  m <- rev(seq_len(1000L))

  gc()
  print(bench::mark(
    rray = rray_slice(x, TRUE, j, k, l, m),
    base = x[, j, k, l, m, drop = FALSE],
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nNamed axes\n")

local({
  x <- array(
    seq_len(1e6L),
    c(100L, 100L, 100L),
    dimnames = list(
      paste0("r", seq_len(100L)),
      paste0("c", seq_len(100L)),
      paste0("s", seq_len(100L))
    )
  )
  i <- rev(seq_len(100L))
  k <- rev(seq_len(100L))

  gc()
  print(bench::mark(
    rray = rray_slice(x, i, TRUE, k),
    base = x[i, , k, drop = FALSE],
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nLogical storage\n")

local({
  x <- array(rep(c(TRUE, FALSE, NA), length.out = 100000L), c(1L, 100000L))
  index <- rev(seq_len(100000L))

  gc()
  print(bench::mark(
    rray = rray_slice(x, TRUE, index),
    base = x[, index, drop = FALSE],
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nDouble storage\n")

local({
  x <- array(seq_len(100000L) / 3, c(1L, 100000L))
  index <- rev(seq_len(100000L))

  gc()
  print(bench::mark(
    rray = rray_slice(x, TRUE, index),
    base = x[, index, drop = FALSE],
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nComplex storage\n")

local({
  x <- array(as.complex(seq_len(100000L)), c(1L, 100000L))
  index <- rev(seq_len(100000L))

  gc()
  print(bench::mark(
    rray = rray_slice(x, TRUE, index),
    base = x[, index, drop = FALSE],
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nRaw storage\n")

local({
  x <- array(as.raw(seq_len(100000L) %% 256L), c(1L, 100000L))
  index <- rev(seq_len(100000L))

  gc()
  print(bench::mark(
    rray = rray_slice(x, TRUE, index),
    base = x[, index, drop = FALSE],
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nCharacter storage\n")

local({
  x <- array(as.character(seq_len(100000L)), c(1L, 100000L))
  index <- rev(seq_len(100000L))

  gc()
  print(bench::mark(
    rray = rray_slice(x, TRUE, index),
    base = x[, index, drop = FALSE],
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nList storage\n")

local({
  x <- array(as.list(seq_len(100000L)), c(1L, 100000L))
  index <- rev(seq_len(100000L))

  gc()
  print(bench::mark(
    rray = rray_slice(x, TRUE, index),
    base = x[, index, drop = FALSE],
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})
