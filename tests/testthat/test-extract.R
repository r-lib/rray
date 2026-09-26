# ------------------------------------------------------------------------------
# rray_extract()

test_that("extracts 1D locations in column-major order", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_identical(rray_extract(x, c(1L, 4L, 24L)), array(c(1L, 4L, 24L), 3L))
  expect_identical(rray_extract(x, c(24, 1)), array(c(24L, 1L), 2L))
})

test_that("1D locations support duplicates, zero, and missing values", {
  x <- array(1:6, c(2L, 3L))

  expect_identical(
    rray_extract(x, c(2L, 0L, 2L, NA, 6L)),
    array(c(2L, 2L, NA, 6L), 4L)
  )
})

test_that("negative 1D locations select the complement", {
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
    c(24, 1, NA, 1, 0),
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
  locations <- c(3L, NA, 1L)
  mask <- c(TRUE, NA, TRUE)
  points <- matrix(c(3L, NA, 1L), ncol = 1L)

  for (x in xs) {
    expect_identical(rray_extract(x, locations), extract_base(x, locations))
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

# ------------------------------------------------------------------------------
# rray_extract_assign()

test_that("assigns 1D locations in column-major order", {
  x <- array(1:6, c(2L, 3L))

  expect_identical(
    rray_extract_assign(x, c(4L, 1L), c(40L, 10L)),
    array(c(10L, 2L, 3L, 40L, 5L, 6L), c(2L, 3L))
  )
  expect_identical(
    rray_extract_assign(x, c(6, 0, 2), 0L),
    array(c(1L, 0L, 3L, 4L, 5L, 0L), c(2L, 3L))
  )
})

test_that("negative 1D locations assign to the complement", {
  x <- array(1:6, c(2L, 3L))

  expect_identical(
    rray_extract_assign(x, -(1:4), c(50L, 60L)),
    array(c(1:4, 50L, 60L), c(2L, 3L))
  )
})

test_that("the last assignment to a repeated location wins", {
  x <- array(1:6, c(2L, 3L))

  expect_identical(
    rray_extract_assign(x, c(2L, 2L, 2L), c(10L, 20L, 30L)),
    array(c(1L, 30L, 3:6), c(2L, 3L))
  )
  expect_identical(
    rray_extract_assign(x, rbind(c(1L, 2L), c(1L, 2L)), c(10L, 20L)),
    array(c(1:2, 20L, 4:6), c(2L, 3L))
  )
})

test_that("assigns through a logical mask", {
  x <- array(1:6, c(2L, 3L))

  expect_identical(
    rray_extract_assign(x, c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE), 0L),
    array(c(0L, 2:5, 0L), c(2L, 3L))
  )
  expect_identical(
    rray_extract_assign(x, x > 4L, c(50L, 60L)),
    array(c(1:4, 50L, 60L), c(2L, 3L))
  )
  expect_identical(rray_extract_assign(x, TRUE, 0L), array(0L, c(2L, 3L)))
  expect_identical(rray_extract_assign(x, TRUE, 6:1), array(6:1, c(2L, 3L)))
  expect_identical(rray_extract_assign(x, FALSE, 0L), x)
})

test_that("assigns through coordinate points", {
  x <- array(1:24, c(2L, 3L, 4L))
  points <- rbind(
    c(1L, 1L, 1L),
    c(2L, 3L, 4L)
  )
  expect <- x
  expect[1L, 1L, 1L] <- 100L
  expect[2L, 3L, 4L] <- 200L

  expect_identical(rray_extract_assign(x, points, c(100L, 200L)), expect)

  storage.mode(points) <- "double"
  expect_identical(rray_extract_assign(x, points, c(100L, 200L)), expect)
})

test_that("assigns points against the maximum dimensionality", {
  x <- array(1:2, c(rep(1L, 63L), 2L))
  points <- rbind(c(rep(1L, 63L), 2L))

  expect_identical(
    rray_extract_assign(x, points, 0L),
    array(c(1L, 0L), c(rep(1L, 63L), 2L))
  )
})

test_that("agrees with a read of the same locations", {
  x <- array(1:24, c(2L, 3L, 4L))
  value <- 101:104
  subscripts <- list(
    c(24L, 1L, 7L, 12L),
    -(5:24),
    x %% 6L == 0L,
    rbind(c(2, 1, 4), c(1, 3, 2), c(2, 3, 1), c(1, 1, 1))
  )

  for (i in subscripts) {
    out <- rray_extract_assign(x, i, value)
    expect_identical(rray_extract(out, i), array(value, 4L))
  }
})

test_that("assignment matches base R for every kind of subscript", {
  x <- array(1:24, c(2L, 3L, 4L))
  subscripts <- list(
    c(24L, 1L, 1L, 0L),
    c(24, 1, 1, 0),
    c(-1, -24, 0),
    x > 20L,
    rep(c(TRUE, FALSE, FALSE), 8L),
    rbind(c(2, 1, 4), c(1, 3, 2), c(2, 1, 4))
  )

  for (i in subscripts) {
    expect <- x
    expect[i] <- 0L
    expect_identical(rray_extract_assign(x, i, 0L), expect)
  }
})

test_that("assigns every native storage type", {
  xs <- list(
    c(TRUE, FALSE, NA),
    1:3,
    c(1.5, 2.5, 3.5),
    c(1i, 2i, 3i),
    c("a", "b", "c"),
    as.raw(1:3),
    list("a", 2L, NULL)
  )
  locations <- c(3L, 1L)
  mask <- c(TRUE, FALSE, TRUE)
  points <- matrix(c(3L, 1L), ncol = 1L)

  for (x in xs) {
    x <- array(x)
    value <- x[c(2L, 2L)]
    expect <- x
    expect[c(3L, 1L)] <- value

    expect_identical(rray_extract_assign(x, locations, value), expect)
    expect_identical(rray_extract_assign(x, mask, rev(value)), expect)
    expect_identical(rray_extract_assign(x, points, value), expect)
  }
})

test_that("empty targets accept a value of size 0 or 1", {
  x <- array(1:6, c(2L, 3L))

  expect_identical(rray_extract_assign(x, integer(), integer()), x)
  expect_identical(rray_extract_assign(x, integer(), 0L), x)
  expect_identical(rray_extract_assign(x, FALSE, 0L), x)
  expect_identical(rray_extract_assign(x, matrix(integer(), 0L, 2L), 0L), x)

  x <- array(integer(), c(2L, 0L, 3L))
  expect_identical(rray_extract_assign(x, TRUE, 0L), x)
  expect_identical(rray_extract_assign(x, logical(), integer()), x)
})

test_that("keeps the type, dimensions, and names of `x`", {
  x <- array(
    c(1, 2, 3, 4),
    c(2L, 2L),
    dimnames = list(c("r1", "r2"), c("c1", "c2"))
  )
  value <- array(c(TRUE, FALSE), 2L, dimnames = list(c("a", "b")))

  expect_identical(
    rray_extract_assign(x, c(a = 4L, b = 1L), value),
    array(
      c(0, 2, 3, 1),
      c(2L, 2L),
      dimnames = list(c("r1", "r2"), c("c1", "c2"))
    )
  )
})

test_that("drops every other attribute of `x`", {
  x <- array(1:4, c(2L, 2L), dimnames = list(c("r1", "r2"), NULL))
  names(x) <- c("a", "b", "c", "d")
  attr(x, "foo") <- "bar"

  expect_identical(
    rray_extract_assign(x, 1L, 0L),
    array(c(0L, 2:4), c(2L, 2L), dimnames = list(c("r1", "r2"), NULL))
  )

  x <- structure(1:2, foo = "bar")
  expect_identical(rray_extract_assign(x, 1L, 0L), array(c(0L, 2L)))
})

test_that("a bare vector `x` becomes a 1D array", {
  expect_identical(rray_extract_assign(1:3, 2L, 0L), array(c(1L, 0L, 3L)))
  expect_identical(
    rray_extract_assign(c(a = 1L, b = 2L), 2L, 0L),
    array(c(1L, 0L), dimnames = list(c("a", "b")))
  )
})

test_that("does not modify `x` or `value`", {
  xs <- list(
    1:6,
    array(c(1, 2, 3, 4, 5, 6), c(2L, 3L)),
    letters[1:6],
    array(as.list(1:6), c(2L, 3L))
  )

  for (x in xs) {
    original <- rlang::duplicate(x)
    value <- x[6:1]
    original_value <- rlang::duplicate(value)

    rray_extract_assign(x, TRUE, value)

    expect_identical(x, original)
    expect_identical(value, original_value)
  }
})

test_that("`value` can be `x`", {
  x <- array(1:6)

  expect_identical(rray_extract_assign(x, 6:1, x), array(6:1))
  expect_identical(rray_extract_assign(x, TRUE, x), x)

  x <- c(1L, 2L, 3L)
  expect_identical(rray_extract_assign(x, 3:1, x), array(3:1))
  expect_identical(x, c(1L, 2L, 3L))
})

test_that("casts `value` to the type of `x`", {
  x <- array(c(1.5, 2.5, 3.5))

  expect_identical(
    rray_extract_assign(x, 2L, TRUE),
    array(c(1.5, 1, 3.5))
  )
  expect_identical(
    rray_extract_assign(array(1:3), 2L, 5),
    array(c(1L, 5L, 3L))
  )

  expect_snapshot(error = TRUE, {
    rray_extract_assign(array(1:3), 2L, 1.5)
    rray_extract_assign(array(1:3), 2L, "a")
    rray_extract_assign(array(1:3), 2L, NULL)
    rray_extract_assign(array(1:3), 2L, factor("a"))
  })
})

test_that("broadcasts `value` to the number of selected values", {
  x <- array(1:6, c(2L, 3L))

  expect_identical(
    rray_extract_assign(x, c(1L, 6L), array(0L, 1L)),
    array(c(0L, 2:5, 0L), c(2L, 3L))
  )

  expect_snapshot(error = TRUE, {
    rray_extract_assign(x, 1:4, 1:2)
    rray_extract_assign(x, 1L, integer())
    rray_extract_assign(x, 1L, matrix(0L))
  })
})

test_that("rejects missing locations", {
  x <- array(1:6, c(2L, 3L))

  expect_snapshot(error = TRUE, {
    rray_extract_assign(x, c(1L, NA), 0L)
    rray_extract_assign(x, NA, 0L)
    rray_extract_assign(x, x > NA, 0L)
    rray_extract_assign(x, rbind(c(1L, NA)), 0L)
  })
})

test_that("reports subscript errors from `rray_extract_assign()`", {
  x <- array(1:6, c(2L, 3L))

  expect_snapshot(error = TRUE, {
    rray_extract_assign(x, 7L, 0L)
    rray_extract_assign(x, rbind(c(0L, 1L)), 0L)
    rray_extract_assign(x, "a", 0L)
  })
})

test_that("errors on unsupported `x` inputs to `rray_extract_assign()`", {
  expect_snapshot(error = TRUE, {
    rray_extract_assign(NULL, 1L, 0L)
    rray_extract_assign(structure(1:2, class = "foo"), 1L, 0L)
  })
})
