test_that("can accumulate along axis 1", {
  x <- array(c(1L, 2L, 3L, 4L, 5L, 6L), c(3L, 2L))

  expect_identical(
    rray_cumulative_sum(x, 1L),
    array(c(1L, 3L, 6L, 4L, 9L, 15L), c(3L, 2L))
  )
})

test_that("can accumulate along axis 2", {
  x <- array(c(1L, 2L, 3L, 4L, 5L, 6L), c(2L, 3L))

  expect_identical(
    rray_cumulative_sum(x, 2L),
    array(c(1L, 2L, 4L, 6L, 9L, 12L), c(2L, 3L))
  )
})

test_that("can accumulate along every axis of a 3D array", {
  x <- array(1:24, c(2L, 3L, 4L))

  for (axis in 1:3) {
    expect_identical(
      rray_cumulative_sum(x, axis),
      cumulative_reference(x, axis, `+`)
    )
  }
})

test_that("each line starts fresh", {
  x <- array(c(1, 2, 3, 4), c(2L, 2L))
  expect_identical(
    rray_cumulative_sum(x, 1L),
    array(c(1, 3, 3, 7), c(2L, 2L))
  )
  expect_identical(
    rray_cumulative_sum(x, 2L),
    array(c(1, 2, 4, 6), c(2L, 2L))
  )
})

test_that("logicals become integers, other types are kept", {
  expect_identical(
    rray_cumulative_sum(c(TRUE, FALSE, TRUE), 1L),
    array(c(1L, 1L, 2L), 3L)
  )
  expect_identical(
    rray_cumulative_sum(c(2L, -3L, 4L), 1L),
    array(c(2L, -1L, 3L), 3L)
  )
  expect_identical(
    rray_cumulative_sum(c(2, 0.5), 1L),
    array(c(2, 2.5), 2L)
  )
  expect_identical(
    rray_cumulative_sum(c(1i, 1 + 1i), 1L),
    array(c(1i, 1 + 2i), 2L)
  )
})

test_that("matches base R for simple lines", {
  lines <- list(
    c(TRUE, TRUE, FALSE, TRUE),
    c(2L, -1L, 3L, 0L, 5L),
    c(0.5, -4, 2, -0.25),
    c(1 + 1i, 2 - 1i, 0 + 1i, -3 + 0i)
  )

  for (x in lines) {
    expect_identical(as.vector(rray_cumulative_sum(x, 1L)), cumsum(x))
  }
})

test_that("errors on integer overflow", {
  x <- c(.Machine$integer.max, 1L)
  expect_snapshot(rray_cumulative_sum(x, 1L), error = TRUE)

  x <- c(-.Machine$integer.max, -1L)
  expect_snapshot(rray_cumulative_sum(x, 1L), error = TRUE)

  x <- array(c(1L, 2L, .Machine$integer.max, 1L), c(2L, 2L))
  expect_snapshot(rray_cumulative_sum(x, 2L), error = TRUE)
})

test_that("errors on integer overflow even if a later value cancels it", {
  x <- c(.Machine$integer.max, 1L, -1L)
  expect_snapshot(rray_cumulative_sum(x, 1L), error = TRUE)
})

test_that("can reach the integer limits without overflowing", {
  x <- c(.Machine$integer.max - 1L, 1L, -1L)
  expect_identical(
    rray_cumulative_sum(x, 1L),
    array(
      c(
        .Machine$integer.max - 1L,
        .Machine$integer.max,
        .Machine$integer.max - 1L
      ),
      3L
    )
  )

  x <- c(-.Machine$integer.max + 1L, -1L)
  expect_identical(
    rray_cumulative_sum(x, 1L),
    array(c(-.Machine$integer.max + 1L, -.Machine$integer.max), 2L)
  )
})

test_that("missing values persist through the rest of a line", {
  expect_identical(
    as.vector(rray_cumulative_sum(c(TRUE, NA, FALSE), 1L)),
    c(1L, NA, NA)
  )
  expect_identical(
    as.vector(rray_cumulative_sum(c(2L, NA, 0L), 1L)),
    c(2L, NA, NA)
  )
  expect_identical(
    as.vector(rray_cumulative_sum(c(2, NA, 0), 1L)),
    c(2, NA, NA)
  )
  expect_identical(
    as.vector(rray_cumulative_sum(c(2, NaN, 0), 1L)),
    c(2, NaN, NaN)
  )
})

test_that("a missing value before an overflow does not error", {
  x <- c(1L, NA, .Machine$integer.max, .Machine$integer.max)
  expect_identical(
    as.vector(rray_cumulative_sum(x, 1L)),
    c(1L, NA, NA, NA)
  )
})

test_that("`NA` and `NaN` together are missing in either order", {
  expect_identical(
    is.na(as.vector(rray_cumulative_sum(c(2, NA, NaN, 3), 1L))),
    c(FALSE, TRUE, TRUE, TRUE)
  )
  expect_identical(
    is.na(as.vector(rray_cumulative_sum(c(2, NaN, NA, 3), 1L))),
    c(FALSE, TRUE, TRUE, TRUE)
  )
})

test_that("opposing infinities give `NaN`", {
  expect_identical(
    as.vector(rray_cumulative_sum(c(Inf, 1, -Inf, 1), 1L)),
    c(Inf, Inf, NaN, NaN)
  )
})

test_that("complex parts are summed separately", {
  x <- c(1 + 1i, complex(real = NA, imaginary = 2), 3 + 3i)
  out <- as.vector(rray_cumulative_sum(x, 1L))
  expect_identical(is.na(Re(out)), c(FALSE, TRUE, TRUE))
  expect_identical(Im(out), c(1, 3, 6))

  x <- c(1 + 1i, complex(real = 2, imaginary = NA), 3 + 3i)
  out <- as.vector(rray_cumulative_sum(x, 1L))
  expect_identical(Re(out), c(1, 3, 6))
  expect_identical(is.na(Im(out)), c(FALSE, TRUE, TRUE))
})

test_that("last position matches the reducer", {
  x <- array(c(2L, -1L, 5L, 4L, -3L, 7L), c(2L, 3L))
  expect_identical(
    rray_cumulative_sum(x, 2L)[, 3L, drop = FALSE],
    rray_sum(x, 2L)
  )

  x <- array(c(2, -1, 0.5, 4, -3, 0.25), c(3L, 2L))
  expect_identical(
    rray_cumulative_sum(x, 1L)[3L, , drop = FALSE],
    rray_sum(x, 1L)
  )

  x <- array(c(1 + 1i, 2i, -1 + 0i, 3 - 1i), c(2L, 2L))
  expect_identical(
    rray_cumulative_sum(x, 1L)[2L, , drop = FALSE],
    rray_sum(x, 1L)
  )
})

test_that("an axis of dimension 1 still casts logicals to integer", {
  x <- array(c(TRUE, FALSE, NA), c(1L, 3L))
  expect_identical(rray_cumulative_sum(x, 1L), array(c(1L, 0L, NA), c(1L, 3L)))

  x <- array(c(-Inf, 2, NA, NaN), c(1L, 4L))
  expect_identical(rray_cumulative_sum(x, 1L), x)
})

test_that("zero-size arrays keep their shape", {
  x <- array(logical(), c(2L, 0L, 3L))
  for (axis in 1:3) {
    expect_identical(
      rray_cumulative_sum(x, axis),
      array(integer(), c(2L, 0L, 3L))
    )
  }

  for (x in list(integer(), double(), complex())) {
    x <- array(x, c(2L, 0L, 3L))

    for (axis in 1:3) {
      expect_identical(rray_cumulative_sum(x, axis), x)
    }
  }
})

test_that("names are kept on every axis", {
  x <- array(
    c(1, 2, 3, 4),
    c(2L, 2L),
    dimnames = list(c("a", "b"), c("c", "d"))
  )
  expect_identical(dimnames(rray_cumulative_sum(x, 1L)), dimnames(x))
  expect_identical(dimnames(rray_cumulative_sum(x, 2L)), dimnames(x))

  x <- array(1L, c(2L, 2L), dimnames = list(NULL, c("c", "d")))
  expect_identical(dimnames(rray_cumulative_sum(x, 2L)), dimnames(x))

  x <- array(1L, c(2L, 2L))
  expect_null(dimnames(rray_cumulative_sum(x, 1L)))
})

test_that("titles are kept", {
  x <- array(
    1L,
    c(2L, 2L),
    dimnames = list(Row = c("a", "b"), Column = NULL)
  )
  expect_identical(
    rray_titles(rray_cumulative_sum(x, 1L)),
    c("Row", "Column")
  )
})

test_that("vectors become 1D arrays", {
  expect_identical(
    rray_cumulative_sum(c(a = 1, b = 3, c = 2), 1L),
    array(c(1, 4, 6), 3L, dimnames = list(c("a", "b", "c")))
  )
  expect_identical(
    rray_cumulative_sum(c(3L, 1L, 2L), 1L),
    array(c(3L, 4L, 6L), 3L)
  )
})

test_that("`axis` can be a whole double", {
  x <- array(c(1, 2), c(2L, 2L))
  expect_identical(rray_cumulative_sum(x, 2), rray_cumulative_sum(x, 2L))
})

test_that("`axis` is validated", {
  x <- array(1, c(2L, 2L))

  expect_snapshot(rray_cumulative_sum(x, integer()), error = TRUE)
  expect_snapshot(rray_cumulative_sum(x, 1:2), error = TRUE)
  expect_snapshot(rray_cumulative_sum(x, NA_integer_), error = TRUE)
  expect_snapshot(rray_cumulative_sum(x, 0L), error = TRUE)
  expect_snapshot(rray_cumulative_sum(x, -1L), error = TRUE)
  expect_snapshot(rray_cumulative_sum(x, 1.5), error = TRUE)
  expect_snapshot(rray_cumulative_sum(x, 3L), error = TRUE)
  expect_snapshot(rray_cumulative_sum(x, structure(1L, foo = 1)), error = TRUE)
  expect_snapshot(rray_cumulative_sum(x, "a"), error = TRUE)
})

test_that("errors on unsupported input types", {
  expect_snapshot(rray_cumulative_sum("a", 1L), error = TRUE)
  expect_snapshot(rray_cumulative_sum(as.raw(1L), 1L), error = TRUE)
  expect_snapshot(rray_cumulative_sum(list(1), 1L), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(1L, class = "foo")
  expect_snapshot(rray_cumulative_sum(x, 1L), error = TRUE)
})
