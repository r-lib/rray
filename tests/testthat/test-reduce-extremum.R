test_that("can reduce along axis 1", {
  x <- array(c(3L, 1L, 2L, 4L, 6L, 5L), c(3L, 2L))
  expect_identical(rray_max(x, axes = 1L), array(c(3L, 6L), c(1L, 2L)))
  expect_identical(rray_min(x, axes = 1L), array(c(1L, 4L), c(1L, 2L)))
})

test_that("can reduce along axis 2", {
  x <- array(c(3L, 1L, 2L, 4L, 6L, 5L), c(3L, 2L))
  expect_identical(rray_max(x, axes = 2L), array(c(4L, 6L, 5L), c(3L, 1L)))
  expect_identical(rray_min(x, axes = 2L), array(c(3L, 1L, 2L), c(3L, 1L)))
})

test_that("can reduce over all axes", {
  x <- array(c(3L, 1L, 2L, 4L, 6L, 5L), c(3L, 2L))
  expect_identical(rray_max(x, axes = c(1L, 2L)), array(6L, c(1L, 1L)))
  expect_identical(rray_min(x, axes = c(1L, 2L)), array(1L, c(1L, 1L)))
})

test_that("reducing over no axes returns the input unchanged", {
  x <- array(c(3, NA, NaN, -Inf), c(2L, 2L))
  expect_identical(rray_max(x, axes = integer()), x)
  expect_identical(rray_min(x, axes = integer()), x)
})

test_that("can reduce axis 3", {
  x <- array(1:24, c(2L, 3L, 4L))
  expect_identical(rray_max(x, axes = 3L), array(19:24, c(2L, 3L, 1L)))
  expect_identical(rray_min(x, axes = 3L), array(1:6, c(2L, 3L, 1L)))
})

test_that("axes can be in any order", {
  x <- array(1:24, c(2L, 3L, 4L))
  expect_identical(rray_max(x, axes = c(3L, 1L)), rray_max(x, axes = c(1L, 3L)))
  expect_identical(rray_min(x, axes = c(3L, 1L)), rray_min(x, axes = c(1L, 3L)))
})

test_that("coalesces reduction axes", {
  x <- array((1:24 * 7L) %% 11L, c(1L, 3L, 2L, 4L))

  expected <- function(fn, axes) {
    dimensions <- dim(x)
    dimensions[axes] <- 1L
    kept_axes <- setdiff(seq_along(dimensions), axes)

    values <- if (length(kept_axes)) {
      apply(x, kept_axes, fn)
    } else {
      fn(x)
    }

    array(values, dimensions)
  }

  axes <- list(1L, 2L, 3L, 4L, c(1L, 3L), c(2L, 4L), 1:4)

  for (axis in axes) {
    expect_identical(rray_max(x, axes = axis), expected(max, axis))
    expect_identical(rray_min(x, axes = axis), expected(min, axis))
  }
})

test_that("dimension names are kept for non-reduced axes", {
  x <- array(1:10, c(5L, 2L), dimnames = list(letters[1:5], c("c1", "c2")))
  expect_identical(dimnames(rray_max(x, axes = 1L)), list(NULL, c("c1", "c2")))
  expect_identical(dimnames(rray_min(x, axes = 2L)), list(letters[1:5], NULL))
})

test_that("dimension names are dropped when all named axes are reduced", {
  x <- array(1:10, c(5L, 2L), dimnames = list(letters[1:5], NULL))
  expect_null(dimnames(rray_max(x, axes = 1L)))
  expect_null(dimnames(rray_min(x, axes = 1L)))
})

test_that("titles are kept", {
  x <- array(
    1:24,
    c(2L, 3L, 4L),
    dimnames = list(A = c("a", "b"), B = NULL, C = NULL)
  )
  y <- array(1:24, c(2L, 3L, 4L), dimnames = list(A = NULL, B = NULL, C = NULL))

  expect_identical(rray_titles(rray_max(x, axes = 1L)), c("A", "B", "C"))
  expect_identical(rray_titles(rray_min(x, axes = 1L)), c("A", "B", "C"))
  expect_identical(dimnames(rray_max(y, axes = 1L)), dimnames(y))
  expect_identical(dimnames(rray_min(y, axes = 1L)), dimnames(y))
})

test_that("output type is the input type", {
  expect_identical(rray_max(c(TRUE, FALSE), axes = 1L), array(TRUE, 1L))
  expect_identical(rray_min(c(TRUE, FALSE), axes = 1L), array(FALSE, 1L))
  expect_identical(rray_max(c(1L, 2L), axes = 1L), array(2L, 1L))
  expect_identical(rray_min(c(1L, 2L), axes = 1L), array(1L, 1L))
  expect_identical(rray_max(c(1, 2), axes = 1L), array(2, 1L))
  expect_identical(rray_min(c(1, 2), axes = 1L), array(1, 1L))
})

test_that("matches repeated `pmax()` and `pmin()` across special values", {
  cases <- list(
    list(
      values = c(NA, FALSE, TRUE),
      cast = as.logical,
      lowest = FALSE,
      highest = TRUE
    ),
    list(
      values = c(NA, -1L, 0L, 1L),
      cast = as.integer,
      lowest = -.Machine$integer.max,
      highest = .Machine$integer.max
    ),
    list(
      values = c(NA, NaN, -Inf, -1, -0, 0, 1, Inf),
      cast = as.double,
      lowest = -Inf,
      highest = Inf
    )
  )

  for (case in cases) {
    x <- unname(t(as.matrix(expand.grid(rep(list(case$values), 3L)))))
    rows <- lapply(1:3, \(i) x[i, ])

    for (na_rm in c(FALSE, TRUE)) {
      pmax_na_rm <- \(x, y) na_wins(pmax(x, y, na.rm = na_rm), x, y)
      pmin_na_rm <- \(x, y) na_wins(pmin(x, y, na.rm = na_rm), x, y)

      expect_identical(
        as.vector(rray_max(x, axes = 1L, na_rm = na_rm)),
        case$cast(Reduce(pmax_na_rm, rows, case$lowest))
      )
      expect_identical(
        as.vector(rray_min(x, axes = 1L, na_rm = na_rm)),
        case$cast(Reduce(pmin_na_rm, rows, case$highest))
      )
    }
  }
})

test_that("missing values propagate", {
  expect_identical(as.vector(rray_max(c(TRUE, NA), axes = 1L)), NA)
  expect_identical(as.vector(rray_min(c(NA, FALSE), axes = 1L)), NA)
  expect_identical(as.vector(rray_max(c(1L, NA), axes = 1L)), NA_integer_)
  expect_identical(as.vector(rray_min(c(NA, 1L), axes = 1L)), NA_integer_)
  expect_identical(as.vector(rray_max(c(1, NaN), axes = 1L)), NaN)
  expect_identical(as.vector(rray_min(c(NA, 1), axes = 1L)), NA_real_)
})

test_that("`NA` wins over `NaN`, like `max()` and `min()`", {
  expect_identical(as.vector(rray_max(c(NA, NaN), axes = 1L)), NA_real_)
  expect_identical(as.vector(rray_max(c(NaN, NA), axes = 1L)), NA_real_)
  expect_identical(
    as.vector(rray_max(c(NaN, 1, NA, NaN), axes = 1L)),
    NA_real_
  )
  expect_identical(as.vector(rray_min(c(NA, NaN), axes = 1L)), NA_real_)
  expect_identical(as.vector(rray_min(c(NaN, NA), axes = 1L)), NA_real_)
  expect_identical(
    as.vector(rray_min(c(NaN, 1, NA, NaN), axes = 1L)),
    NA_real_
  )
})

test_that("na_rm removes missing values", {
  expect_identical(
    as.vector(rray_max(c(NA, FALSE), axes = 1L, na_rm = TRUE)),
    FALSE
  )
  expect_identical(
    as.vector(rray_min(c(TRUE, NA), axes = 1L, na_rm = TRUE)),
    TRUE
  )
  expect_identical(
    as.vector(rray_max(c(NA, -1L), axes = 1L, na_rm = TRUE)),
    -1L
  )
  expect_identical(
    as.vector(rray_min(c(1L, NA), axes = 1L, na_rm = TRUE)),
    1L
  )
  expect_identical(
    as.vector(rray_max(c(NaN, -1, NA), axes = 1L, na_rm = TRUE)),
    -1
  )
  expect_identical(
    as.vector(rray_min(c(NA, 1, NaN), axes = 1L, na_rm = TRUE)),
    1
  )
})

test_that("na_rm with all missing returns the identity value", {
  expect_identical(
    as.vector(rray_max(c(NA, NA), axes = 1L, na_rm = TRUE)),
    FALSE
  )
  expect_identical(
    as.vector(rray_min(c(NA, NA), axes = 1L, na_rm = TRUE)),
    TRUE
  )

  x <- c(NA_integer_, NA_integer_)
  expect_identical(
    as.vector(rray_max(x, axes = 1L, na_rm = TRUE)),
    -.Machine$integer.max
  )
  expect_identical(
    as.vector(rray_min(x, axes = 1L, na_rm = TRUE)),
    .Machine$integer.max
  )

  x <- c(NA, NaN)
  expect_identical(as.vector(rray_max(x, axes = 1L, na_rm = TRUE)), -Inf)
  expect_identical(as.vector(rray_min(x, axes = 1L, na_rm = TRUE)), Inf)
})

test_that("na_rm with no missing values matches default", {
  x <- array(c(3L, 1L, 2L, 4L, 6L, 5L), c(3L, 2L))
  expect_identical(rray_max(x, axes = 1L, na_rm = TRUE), rray_max(x, axes = 1L))
  expect_identical(rray_min(x, axes = 1L, na_rm = TRUE), rray_min(x, axes = 1L))
})

test_that("the most extreme values of each type are reachable", {
  x <- c(-.Machine$integer.max, -.Machine$integer.max)
  expect_identical(as.vector(rray_max(x, axes = 1L)), -.Machine$integer.max)

  x <- c(.Machine$integer.max, .Machine$integer.max)
  expect_identical(as.vector(rray_min(x, axes = 1L)), .Machine$integer.max)

  expect_identical(as.vector(rray_max(c(-Inf, -Inf), axes = 1L)), -Inf)
  expect_identical(as.vector(rray_min(c(Inf, Inf), axes = 1L)), Inf)
})

test_that("plain vector input works", {
  expect_identical(rray_max(c(1L, 3L, 2L), axes = 1L), array(3L, 1L))
  expect_identical(rray_min(c(1L, 3L, 2L), axes = 1L), array(1L, 1L))
})

test_that("axes are coerced to integer", {
  x <- array(1:4, c(2L, 2L))
  expect_identical(rray_max(x, axes = 1), rray_max(x, axes = 1L))
  expect_identical(rray_min(x, axes = 1), rray_min(x, axes = 1L))
})

test_that("reducing a zero-length axis gives the identity value", {
  zero_size <- function(x) array(x, c(0L, 2L))

  expect_identical(
    rray_max(zero_size(logical()), axes = 1L),
    array(FALSE, c(1L, 2L))
  )
  expect_identical(
    rray_min(zero_size(logical()), axes = 1L),
    array(TRUE, c(1L, 2L))
  )
  expect_identical(
    rray_max(zero_size(integer()), axes = 1L),
    array(-.Machine$integer.max, c(1L, 2L))
  )
  expect_identical(
    rray_min(zero_size(integer()), axes = 1L),
    array(.Machine$integer.max, c(1L, 2L))
  )
  expect_identical(
    rray_max(zero_size(double()), axes = 1L),
    array(-Inf, c(1L, 2L))
  )
  expect_identical(
    rray_min(zero_size(double()), axes = 1L),
    array(Inf, c(1L, 2L))
  )
})

test_that("reducing over a non-zero-length axis with a zero-length axis", {
  x <- matrix(double(), 0L, 2L)
  expect_identical(rray_max(x, axes = 2L), array(double(), c(0L, 1L)))
  expect_identical(rray_min(x, axes = 2L), array(double(), c(0L, 1L)))
})

test_that("reducing all axes of a zero-length array", {
  x <- matrix(double(), 0L, 2L)
  expect_identical(rray_max(x, axes = c(1L, 2L)), array(-Inf, c(1L, 1L)))
  expect_identical(rray_min(x, axes = c(1L, 2L)), array(Inf, c(1L, 1L)))
})

test_that("errors on axes out of range", {
  x <- array(1:4, c(2L, 2L))
  expect_snapshot(rray_max(x, axes = 3L), error = TRUE)
  expect_snapshot(rray_min(x, axes = 3L), error = TRUE)
})

test_that("`axes` must be named", {
  expect_snapshot(rray_max(1:3, 1), error = TRUE)
  expect_snapshot(rray_min(1:3, 1), error = TRUE)
})

test_that("`na_rm` must be `TRUE` or `FALSE`", {
  x <- array(1:4, c(2L, 2L))
  expect_snapshot(rray_max(x, axes = 1L, na_rm = NA), error = TRUE)
  expect_snapshot(rray_min(x, axes = 1L, na_rm = 1), error = TRUE)
})

test_that("errors on unsupported types", {
  expect_snapshot(rray_max(array(1i, c(2L, 2L)), axes = 1L), error = TRUE)
  expect_snapshot(rray_max(array("a", c(2L, 2L)), axes = 1L), error = TRUE)
  expect_snapshot(
    rray_max(array(as.raw(1:4), c(2L, 2L)), axes = 1L),
    error = TRUE
  )
  expect_snapshot(
    rray_max(array(list(1, 2, 3, 4), c(2L, 2L)), axes = 1L),
    error = TRUE
  )
  expect_snapshot(rray_min(array(1i, c(2L, 2L)), axes = 1L), error = TRUE)
})

test_that("errors on scalar input", {
  expect_snapshot(rray_max(quote(x), axes = 1L), error = TRUE)
  expect_snapshot(rray_min(quote(x), axes = 1L), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(array(1:4, c(2L, 2L)), class = "foo")
  expect_snapshot(rray_max(x, axes = 1L), error = TRUE)
  expect_snapshot(rray_min(x, axes = 1L), error = TRUE)
})
