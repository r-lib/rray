test_that("`rray_as_index_array()` normalizes vectors", {
  x <- c(a = 1L, b = NA_integer_, c = 3L)
  out <- rray_as_index_array(x, 3L)

  expect_identical(out, array(x, 3L, dimnames = list(names(x))))
})

test_that("`rray_as_index_array()` preserves arrays", {
  x <- array(
    c(1L, NA_integer_, 2L, 1L),
    c(2L, 2L),
    dimnames = list(c("a", "b"), c("c", "d"))
  )

  expect_identical(rray_as_index_array(x, 2L), x)
})

test_that("`rray_as_index_array()` requires bare integer input", {
  expect_snapshot(error = TRUE, {
    rray_as_index_array(NULL, 2L)
    rray_as_index_array(c(TRUE, FALSE), 2L)
    rray_as_index_array(c(1, 2), 2L)
    rray_as_index_array(c("a", "b"), 2L)
    rray_as_index_array(factor(c("a", "b")), 2L)
    rray_as_index_array(structure(1:2, class = "foo"), 2L)
  })
})

test_that("`rray_as_index_array()` checks coordinates", {
  expect_snapshot(error = TRUE, {
    rray_as_index_array(c(0L, 1L), 2L)
    rray_as_index_array(c(-1L, 1L), 2L)
    rray_as_index_array(c(1L, 3L), 2L)
    rray_as_index_array(1L, 0L)
  })

  expect_identical(
    rray_as_index_array(NA_integer_, 0L),
    array(NA_integer_, 1L)
  )
})

test_that("`rray_as_index_array()` checks `dimension`", {
  expect_identical(rray_as_index_array(integer(), 0), array(integer(), 0L))

  expect_snapshot(error = TRUE, {
    rray_as_index_array(1L, NULL)
    rray_as_index_array(1L, integer())
    rray_as_index_array(1L, c(1L, 2L))
    rray_as_index_array(1L, NA_integer_)
    rray_as_index_array(1L, -1L)
    rray_as_index_array(1L, 1.5)
  })
})

test_that("indexes vectors", {
  x <- c(10L, 20L, 30L)

  expect_identical(
    rray_index(x, c(3L, 1L, NA_integer_, 2L)),
    array(c(30L, 10L, NA_integer_, 20L), 4L)
  )
})

test_that("pairs equal coordinate dimensions pointwise", {
  x <- array(1:6, c(2L, 3L))
  rows <- array(c(1L, 2L, 1L, 2L), c(2L, 2L))
  columns <- array(c(1L, 2L, 3L, 1L), c(2L, 2L))

  expect_identical(
    rray_index(x, rows, columns),
    array(c(x[1L, 1L], x[2L, 2L], x[1L, 3L], x[2L, 1L]), c(2L, 2L))
  )
})

test_that("broadcasts coordinate arrays", {
  x <- array(1:6, c(2L, 3L))
  rows <- array(1:2, c(2L, 1L))
  columns <- array(1:3, c(1L, 3L))

  expect_identical(rray_index(x, rows, columns), x)

  rows <- 1:2
  columns <- array(1:3, c(1L, 3L))
  expect_identical(rray_index(x, rows, columns), x)
})

test_that("matches base R across mixed coordinate shapes", {
  x <- array(seq_len(2L * 3L * 4L * 2L), c(2L, 3L, 4L, 2L))
  axis1 <- array(c(1L, 2L), c(2L, 1L, 1L))
  axis2 <- array(c(3L, NA_integer_, 1L), c(1L, 3L, 1L))
  axis3 <- array(c(4L, 2L), c(1L, 1L, 2L))
  axis4 <- 2L

  expect_identical(
    rray_index(x, axis1, axis2, axis3, axis4),
    index_oracle(x, axis1, axis2, axis3, axis4)
  )
})

test_that("matches base R for pointwise coordinates", {
  x <- array(seq_len(2L * 3L * 4L), c(2L, 3L, 4L))
  axis1 <- array(c(2L, 1L, NA_integer_, 2L, 1L, 1L), c(2L, 3L))
  axis2 <- array(c(1L, 3L, 2L, 1L, 2L, 3L), c(2L, 3L))
  axis3 <- array(c(4L, 1L, 2L, 3L, 1L, 4L), c(2L, 3L))

  expect_identical(
    rray_index(x, axis1, axis2, axis3),
    index_oracle(x, axis1, axis2, axis3)
  )
})

test_that("supports repeated coordinates", {
  x <- array(1:6, c(2L, 3L))
  rows <- c(2L, 2L, 2L, 1L)
  columns <- c(3L, 3L, 3L, 1L)

  expect_identical(
    rray_index(x, rows, columns),
    array(c(6L, 6L, 6L, 1L), 4L)
  )
})

test_that("result dimensions come from coordinates", {
  x <- array(1:6, c(2L, 3L))

  expect_identical(
    rray_index(x, 1L, 2L),
    array(x[1L, 2L], 1L)
  )

  rows <- array(rep(1:2, 6L), c(2L, 3L, 2L))
  columns <- array(rep(rep(1:3, each = 2L), 2L), c(2L, 3L, 2L))
  expect_identical(
    rray_index(x, rows, columns),
    array(rep(x, 2L), c(2L, 3L, 2L))
  )
})

test_that("supports the maximum source dimensionality", {
  dimensions <- rep(1L, 64L)
  x <- array(42L, dimensions)
  indices <- rep(list(1L), 64L)

  expect_identical(do.call(rray_index, c(list(x), indices)), array(42L, 1L))
})

test_that("supports the maximum result dimensionality", {
  dimensions <- rep(1L, 64L)
  index <- array(1L, dimensions)

  expect_identical(rray_index(42L, index), array(42L, dimensions))
})

test_that("errors above the maximum result dimensionality", {
  index <- array(1L, rep(1L, 65L))

  expect_snapshot(rray_index(42L, index), error = TRUE)
})

test_that("returns every native storage type", {
  indices <- c(2L, NA_integer_, 1L)

  expect_identical(
    rray_index(c(TRUE, FALSE), indices),
    array(c(FALSE, NA, TRUE), 3L)
  )
  expect_identical(
    rray_index(c(1L, 2L), indices),
    array(c(2L, NA_integer_, 1L), 3L)
  )
  expect_identical(
    rray_index(c(1, 2), indices),
    array(c(2, NA_real_, 1), 3L)
  )
  expect_identical(
    rray_index(c(1 + 1i, 2 + 2i), indices),
    array(c(2 + 2i, NA_complex_, 1 + 1i), 3L)
  )
  expect_identical(
    rray_index(c("a", "b"), indices),
    array(c("b", NA_character_, "a"), 3L)
  )
  expect_identical(
    rray_index(as.raw(1:2), indices),
    array(as.raw(c(2, 0, 1)), 3L)
  )
  expect_identical(
    rray_index(list("a", 2L), indices),
    array(list(2L, NULL, "a"), 3L)
  )
})

test_that("missing in any coordinate produces missing output", {
  x <- array(1:4, c(2L, 2L))
  rows <- c(1L, NA_integer_, 1L, 2L)
  columns <- c(NA_integer_, 1L, 2L, 2L)

  expect_identical(
    rray_index(x, rows, columns),
    array(c(NA_integer_, NA_integer_, 3L, 4L), 4L)
  )
})

test_that("requires one coordinate per source axis", {
  x <- array(1:6, c(2L, 3L))

  expect_snapshot(error = TRUE, {
    rray_index(x)
    rray_index(x, 1L)
    rray_index(x, 1L, 1L, 1L)
  })
})

test_that("requires unnamed coordinates", {
  x <- array(1:6, c(2L, 3L))

  expect_snapshot(error = TRUE, {
    rray_index(x, rows = 1L, 1L)
    rray_index(x, 1L, columns = 1L)
  })
})

test_that("supports dynamic splicing", {
  x <- array(1:6, c(2L, 3L))
  indices <- list(array(1:2, c(2L, 1L)), array(1:3, c(1L, 3L)))

  expect_identical(rray_index(x, !!!indices), x)
})

test_that("validates every coordinate before broadcasting", {
  x <- array(1:6, c(2L, 3L))

  expect_snapshot(error = TRUE, {
    rray_index(x, integer(), 0L)
    rray_index(x, integer(), 4L)
    rray_index(x, integer(), NULL)
    rray_index(x, 1L, factor("a"))
  })
})

test_that("errors on incompatible coordinate dimensions", {
  x <- array(1:6, c(2L, 3L))
  rows <- array(1L, c(2L, 3L))
  columns <- array(1L, c(3L, 2L))

  expect_snapshot(rray_index(x, rows, columns), error = TRUE)
})

test_that("combines zero dimensions by broadcasting rules", {
  x <- array(1:6, c(2L, 3L))

  expect_identical(
    rray_index(x, array(integer(), c(0L, 2L)), array(1L, c(1L, 2L))),
    array(integer(), c(0L, 2L))
  )

  expect_snapshot(
    rray_index(x, array(integer(), c(0L, 2L)), array(1L, c(3L, 2L))),
    error = TRUE
  )
})

test_that("supports missing coordinates against a zero source dimension", {
  x <- array(integer(), c(0L, 2L))

  expect_identical(
    rray_index(x, NA_integer_, 1L),
    array(NA_integer_, 1L)
  )
})

test_that("drops all names", {
  x <- array(
    1:4,
    c(2L, 2L),
    dimnames = list(c("r1", "r2"), c("c1", "c2"))
  )
  rows <- c(a = 1L, b = 2L)
  columns <- c(a = 1L, b = 2L)

  expect_null(dimnames(rray_index(x, rows, columns)))
})

test_that("errors on unsupported `x` inputs", {
  expect_snapshot(error = TRUE, {
    rray_index(NULL, 1L)
    rray_index(mean, 1L)
    rray_index(structure(1:2, class = "foo"), 1L)
  })
})
