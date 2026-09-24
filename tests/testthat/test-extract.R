# ------------------------------------------------------------------------------
# rray_extract()

test_that("extracts flat positions in column-major order", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_identical(rray_extract(x, c(1L, 4L, 24L)), array(c(1L, 4L, 24L), 3L))
  expect_identical(rray_extract(x, c(24, 1)), array(c(24L, 1L), 2L))
})

test_that("flat positions support duplicates, zero, and missing values", {
  x <- array(1:6, c(2L, 3L))

  expect_identical(
    rray_extract(x, c(2L, 0L, 2L, NA, 6L)),
    array(c(2L, 2L, NA, 6L), 4L)
  )
})

test_that("negative flat positions select the complement", {
  x <- array(1:6, c(2L, 3L))

  expect_identical(rray_extract(x, -(1:4)), array(5:6, 2L))
  expect_identical(rray_extract(x, c(-1, 0, -6)), array(2:5, 4L))
})

test_that("empty subscripts give an empty result", {
  x <- array(1:6, c(2L, 3L))

  expect_identical(rray_extract(x, integer()), array(integer(), 0L))
  expect_identical(rray_extract(x, double()), array(integer(), 0L))
  expect_identical(rray_extract(x, 0L), array(integer(), 0L))
  expect_identical(rray_extract(x, FALSE), array(integer(), 0L))
})

test_that("a logical vector is a flat mask", {
  x <- array(1:6, c(2L, 3L))

  expect_identical(
    rray_extract(x, c(TRUE, FALSE, NA, FALSE, FALSE, TRUE)),
    array(c(1L, NA, 6L), 3L)
  )
})

test_that("a scalar logical applies to every element", {
  x <- array(1:6, c(2L, 3L))

  expect_identical(rray_extract(x, TRUE), array(1:6, 6L))
  expect_identical(rray_extract(x, FALSE), array(integer(), 0L))
  expect_identical(rray_extract(x, NA), array(rep(NA_integer_, 6L), 6L))
})

test_that("a logical array with the dimensions of `x` is a flat mask", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_identical(
    rray_extract(x, x %% 5L == 0L),
    array(c(5L, 10L, 15L, 20L), 4L)
  )
})

test_that("a numeric matrix holds coordinate points", {
  x <- array(1:24, c(2L, 3L, 4L))
  points <- rbind(
    c(1L, 1L, 1L),
    c(2L, 3L, 4L),
    c(1L, 2L, 3L)
  )

  expect_identical(rray_extract(x, points), array(c(1L, 24L, 15L), 3L))

  storage.mode(points) <- "double"
  expect_identical(rray_extract(x, points), array(c(1L, 24L, 15L), 3L))
})

test_that("points support missing and repeated coordinates", {
  x <- array(1:6, c(2L, 3L))
  points <- rbind(
    c(2L, 3L),
    c(NA, 1L),
    c(1L, NA),
    c(2L, 3L)
  )

  expect_identical(rray_extract(x, points), array(c(6L, NA, NA, 6L), 4L))
})

test_that("points match `rray_index()` over the matrix columns", {
  x <- array(1:24, c(2L, 3L, 4L))
  points <- rbind(
    c(2L, 1L, 4L),
    c(1L, NA, 2L),
    c(2L, 3L, 1L)
  )

  expect_identical(
    rray_extract(x, points),
    rray_index(x, points[, 1], points[, 2], points[, 3])
  )
})

test_that("a zero-row point matrix gives an empty result", {
  x <- array(1:6, c(2L, 3L))

  expect_identical(
    rray_extract(x, matrix(integer(), 0L, 2L)),
    array(integer(), 0L)
  )
})

test_that("points work against a one-dimensional `x`", {
  x <- c(10L, 20L, 30L)

  expect_identical(
    rray_extract(x, matrix(c(3L, 1L), ncol = 1L)),
    array(c(30L, 10L), 2L)
  )
})

test_that("points work against the maximum dimensionality", {
  x <- array(1:2, c(rep(1L, 63L), 2L))
  points <- rbind(c(rep(1L, 63L), 2L), c(rep(1L, 63L), 1L))

  expect_identical(rray_extract(x, points), array(c(2L, 1L), 2L))
})

test_that("works with zero dimensions", {
  x <- array(integer(), c(2L, 0L, 3L))

  expect_identical(rray_extract(x, TRUE), array(integer(), 0L))
  expect_identical(rray_extract(x, logical()), array(integer(), 0L))
  expect_identical(rray_extract(x, integer()), array(integer(), 0L))
  expect_identical(
    rray_extract(x, matrix(integer(), 0L, 3L)),
    array(integer(), 0L)
  )
})

test_that("matches base R for every kind of subscript", {
  x <- array(1:24, c(2L, 3L, 4L))
  points <- rbind(c(2, 1, 4), c(1, NA, 2), c(2, 3, 1))
  subscripts <- list(
    c(24L, 1L, NA, 1L, 0L),
    c(-1, -24, 0),
    x > 20L,
    rep(c(TRUE, NA, FALSE), 8L),
    points
  )

  for (i in subscripts) {
    expect_identical(rray_extract(x, i), extract_base(x, i))
  }
})

test_that("returns every native storage type", {
  xs <- list(
    c(TRUE, FALSE, NA),
    1:3,
    c(1.5, 2.5, 3.5),
    c(1i, 2i, 3i),
    c("a", "b", "c"),
    as.raw(1:3),
    list("a", 2L, NULL)
  )
  flat <- c(3L, NA, 1L)
  mask <- c(TRUE, NA, TRUE)
  points <- matrix(c(3L, NA, 1L), ncol = 1L)

  for (x in xs) {
    expect_identical(rray_extract(x, flat), extract_base(x, flat))
    expect_identical(rray_extract(x, mask), extract_base(x, mask))
    expect_identical(rray_extract(x, points), extract_base(x, points))
  }
})

test_that("drops all names", {
  x <- array(
    1:4,
    c(2L, 2L),
    dimnames = list(c("r1", "r2"), c("c1", "c2"))
  )

  expect_identical(rray_extract(x, c(a = 4L, b = 1L)), array(c(4L, 1L), 2L))
  expect_identical(rray_extract(x, x > 2L), array(3:4, 2L))
  expect_identical(rray_extract(x, rbind(c(2L, 1L))), array(2L, 1L))

  x <- c(a = 1L, b = 2L)
  expect_identical(rray_extract(x, 2L), array(2L, 1L))
  expect_identical(rray_extract(x, matrix(2L)), array(2L, 1L))
})

test_that("reports subscript errors from `rray_extract()`", {
  x <- array(1:6, c(2L, 3L))

  expect_snapshot(error = TRUE, {
    rray_extract(x, 7L)
    rray_extract(x, rbind(c(1L, 4L)))
  })
})

test_that("errors on unsupported `x` inputs", {
  expect_snapshot(error = TRUE, {
    rray_extract(NULL, 1L)
    rray_extract(mean, 1L)
    rray_extract(structure(1:2, class = "foo"), 1L)
  })
})
