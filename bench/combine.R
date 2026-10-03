devtools::load_all(quiet = TRUE)

cat("\nRows\n")

local({
  x <- array(runif(1e6L), c(1000L, 1000L))
  y <- array(runif(1e6L), c(1000L, 1000L))

  gc()
  print(bench::mark(
    rray = rray_combine(x, y, .axis = 1L),
    base = rbind(x, y),
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nColumns\n")

local({
  x <- array(runif(1e6L), c(1000L, 1000L))
  y <- array(runif(1e6L), c(1000L, 1000L))

  gc()
  print(bench::mark(
    rray = rray_combine(x, y, .axis = 2L),
    base = cbind(x, y),
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nRows with short runs\n")

local({
  x <- array(runif(1e6L), c(10L, 100000L))
  y <- array(runif(1e6L), c(10L, 100000L))

  gc()
  print(bench::mark(
    rray = rray_combine(x, y, .axis = 1L),
    base = rbind(x, y),
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nUnit rows\n")

local({
  x <- array(runif(1e6L), c(1L, 1e6L))
  y <- array(runif(1e6L), c(1L, 1e6L))

  gc()
  print(bench::mark(
    rray = rray_combine(x, y, .axis = 1L),
    base = rbind(x, y),
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nMany inputs\n")

local({
  xs <- lapply(seq_len(1000L), \(i) array(runif(1000L), c(1L, 1000L)))

  gc()
  print(bench::mark(
    rray = rray_combine(!!!xs, .axis = 1L),
    base = do.call(rbind, xs),
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nThree-dimensional\n")

local({
  x <- array(runif(1e6L), c(100L, 100L, 100L))
  y <- array(runif(1e6L), c(100L, 100L, 100L))

  gc()
  print(bench::mark(
    rray = rray_combine(x, y, .axis = 2L),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nBroadcast scalar\n")

local({
  x <- array(runif(1e6L), c(1000L, 1000L))
  y <- array(0, c(1L, 1L))

  gc()
  print(bench::mark(
    rray = rray_combine(x, y, .axis = 2L),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nBroadcast row\n")

local({
  x <- array(runif(1e6L), c(1000L, 1000L))
  y <- array(runif(1000L), c(1L, 1000L))

  gc()
  print(bench::mark(
    rray = rray_combine(x, y, .axis = 2L),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nBroadcast row with short runs\n")

local({
  x <- array(runif(1e6L), c(10L, 100000L))
  y <- array(runif(100000L), c(1L, 100000L))

  gc()
  print(bench::mark(
    rray = rray_combine(x, y, .axis = 2L),
    iterations = 20L,
    memory = FALSE
  ))
})

cat("\nLogical storage\n")

local({
  x <- array(rep(c(TRUE, FALSE, NA), length.out = 1e6L), c(1000L, 1000L))
  y <- array(rep(c(NA, TRUE, FALSE), length.out = 1e6L), c(1000L, 1000L))

  gc()
  print(bench::mark(
    rray = rray_combine(x, y, .axis = 1L),
    base = rbind(x, y),
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nInteger storage\n")

local({
  x <- array(seq_len(1e6L), c(1000L, 1000L))
  y <- array(rev(seq_len(1e6L)), c(1000L, 1000L))

  gc()
  print(bench::mark(
    rray = rray_combine(x, y, .axis = 1L),
    base = rbind(x, y),
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nComplex storage\n")

local({
  x <- array(as.complex(seq_len(1e6L)), c(1000L, 1000L))
  y <- array(as.complex(rev(seq_len(1e6L))), c(1000L, 1000L))

  gc()
  print(bench::mark(
    rray = rray_combine(x, y, .axis = 1L),
    base = rbind(x, y),
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nRaw storage\n")

local({
  x <- array(as.raw(seq_len(1e6L) %% 256L), c(1000L, 1000L))
  y <- array(as.raw(rev(seq_len(1e6L)) %% 256L), c(1000L, 1000L))

  gc()
  print(bench::mark(
    rray = rray_combine(x, y, .axis = 1L),
    base = rbind(x, y),
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nCharacter storage\n")

local({
  x <- array(as.character(seq_len(100000L)), c(1000L, 100L))
  y <- array(as.character(rev(seq_len(100000L))), c(1000L, 100L))

  gc()
  print(bench::mark(
    rray = rray_combine(x, y, .axis = 1L),
    base = rbind(x, y),
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})

cat("\nList storage\n")

local({
  x <- array(as.list(seq_len(100000L)), c(1000L, 100L))
  y <- array(as.list(rev(seq_len(100000L))), c(1000L, 100L))

  gc()
  print(bench::mark(
    rray = rray_combine(x, y, .axis = 1L),
    base = rbind(x, y),
    iterations = 20L,
    check = TRUE,
    memory = FALSE
  ))
})
