test_that("tests equality elementwise", {
  x <- array(c(1 + 1i, 2 + 2i, 3 + 3i, 4 + 4i), c(2L, 2L))
  y <- array(c(4 + 4i, 2 + 2i, 3 + 3i, 1 + 1i), c(2L, 2L))

  expect_identical(rray_equal(x, y), x == y)
  expect_identical(rray_not_equal(x, y), x != y)
})

test_that("broadcasts both inputs to common dimensions", {
  x <- array(c(1 + 1i, 2 + 2i, 3 + 3i), c(3L, 1L))
  y <- array(c(2 + 2i, 4 + 4i), c(1L, 2L))

  expected_x <- x[, rep(1L, 2L), drop = FALSE]
  expected_y <- y[rep(1L, 3L), , drop = FALSE]

  expect_identical(rray_equal(x, y), expected_x == expected_y)
  expect_identical(rray_not_equal(x, y), expected_x != expected_y)
})

test_that("works with 1D and 3D arrays", {
  expect_identical(rray_equal(1:3, 2L), array(c(FALSE, TRUE, FALSE), 3L))

  x <- array(1:24 + 1i, c(2L, 3L, 4L))
  y <- array(c(1 + 1i, 2 + 1i), c(2L, 1L, 1L))
  expected_y <- y[, rep(1L, 3L), rep(1L, 4L), drop = FALSE]

  expect_identical(rray_not_equal(x, y), x != expected_y)
})

test_that("returns logical output for every supported type pair", {
  expect_snapshot(native_ptype_matrix(rray_equal, c("x", "y")))
})

test_that("missing values and complex edge cases match base R", {
  values <- list(
    logical = c(NA, FALSE, TRUE),
    integer = c(NA_integer_, -1L, 0L, 1L),
    double = c(NA_real_, NaN, -Inf, -1, -0, 0, 1, Inf),
    complex = c(
      NA_complex_,
      complex(real = NaN),
      complex(imaginary = NaN),
      complex(real = NA_real_, imaginary = 1),
      complex(real = 1, imaginary = NA_real_),
      complex(real = -Inf),
      -1 - 1i,
      complex(real = -0),
      0 + 0i,
      1 + 1i,
      complex(real = Inf)
    )
  )

  make_case <- function(x_values, y_values) {
    list(
      x = rep(x_values, each = length(y_values)),
      y = rep(y_values, times = length(x_values))
    )
  }

  cases <- list()
  for (x_name in names(values)) {
    for (y_name in names(values)) {
      name <- paste(x_name, y_name, sep = "_")
      cases[[name]] <- make_case(values[[x_name]], values[[y_name]])
    }
  }

  operations <- list(
    equal = list(rray_equal, `==`),
    not_equal = list(rray_not_equal, `!=`)
  )

  for (operation in operations) {
    for (case in cases) {
      expect_identical(
        as.vector(operation[[1]](case$x, case$y)),
        operation[[2]](case$x, case$y)
      )
    }
  }
})

test_that("coalesces names across inputs", {
  x <- array(
    1:3,
    c(3L, 1L),
    dimnames = list(c("r1", "r2", "r3"), "x")
  )
  y <- array(
    1:2,
    c(1L, 2L),
    dimnames = list("y", c("c1", "c2"))
  )

  expect_identical(
    rray_names(rray_equal(x, y)),
    list(c("r1", "r2", "r3"), c("c1", "c2"))
  )

  x <- array(1:3, 3L, dimnames = list(c("a", "b", "c")))
  y <- array(1:3, 3L, dimnames = list(c("x", "y", "z")))
  expect_identical(
    rray_names(rray_not_equal(x, y)),
    list(c("a", "b", "c"))
  )
})

test_that("zero dimensions broadcast against dimensions of 1", {
  x <- array(1i, c(1L, 2L))
  y <- array(complex(), c(0L, 2L))
  expect_identical(rray_equal(x, y), array(logical(), c(0L, 2L)))

  x <- array(1i, c(1L, 1L))
  y <- array(complex(), c(0L, 0L))
  expect_identical(rray_not_equal(x, y), array(logical(), c(0L, 0L)))

  expect_identical(rray_equal(complex(), 1i), array(logical(), 0L))
})

test_that("errors on incompatible dimensions", {
  x <- array(1i, c(3L, 2L))
  y <- array(1i, c(2L, 2L))
  expect_snapshot(rray_equal(x, y), error = TRUE)
})

test_that("errors on unsupported types", {
  expect_snapshot(rray_equal("a", "b"), error = TRUE)
  expect_snapshot(rray_not_equal(as.raw(1), as.raw(2)), error = TRUE)
})

test_that("a type error beats a dimension error", {
  x <- array("a", c(2L, 2L))
  y <- array("b", c(3L, 3L))
  expect_snapshot(rray_equal(x, y), error = TRUE)
})

test_that("errors on scalar and classed input", {
  expect_snapshot(rray_equal(NULL, 1i), error = TRUE)
  expect_snapshot(rray_not_equal(1i, NULL), error = TRUE)

  x <- structure(1i, class = "foo")
  expect_snapshot(rray_equal(x, 1i), error = TRUE)
  expect_snapshot(rray_not_equal(1i, x), error = TRUE)
})
