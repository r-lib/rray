# ------------------------------------------------------------------------------
# rray_as_extract_subscript()

test_that("returns bare integer positions unchanged", {
  expect_identical(rray_as_extract_subscript(c(3L, 1L, 3L), 3L), c(3L, 1L, 3L))
  expect_identical(rray_as_extract_subscript(integer(), 3L), integer())
})

test_that("drops attributes from positions", {
  expect_identical(rray_as_extract_subscript(c(a = 2L), 3L), 2L)
  expect_identical(
    rray_as_extract_subscript(
      array(c(2L, 1L), 2L, dimnames = list(c("a", "b"))),
      3L
    ),
    c(2L, 1L)
  )
})

test_that("keeps missing positions", {
  expect_identical(
    rray_as_extract_subscript(c(NA, 2L, NA), 3L),
    c(NA, 2L, NA)
  )
})

test_that("drops zero positions", {
  expect_identical(rray_as_extract_subscript(c(0L, 2L, 0L, NA), 3L), c(2L, NA))
  expect_identical(rray_as_extract_subscript(c(0L, 0L), 3L), integer())
})

test_that("converts double positions to integer", {
  expect_identical(rray_as_extract_subscript(c(3, 0, 1), 3L), c(3L, 1L))
  expect_identical(
    rray_as_extract_subscript(c(NA, NaN), 3L),
    c(NA_integer_, NA)
  )
  expect_identical(rray_as_extract_subscript(double(), 3L), integer())
})

test_that("turns negative positions into a complement mask", {
  expect_identical(
    rray_as_extract_subscript(c(-1L, -3L), 4L),
    c(FALSE, TRUE, FALSE, TRUE)
  )
  expect_identical(
    rray_as_extract_subscript(c(-2, 0, -2), 3L),
    c(TRUE, FALSE, TRUE)
  )
  expect_identical(
    rray_as_extract_subscript(-(1:3), 3L),
    c(FALSE, FALSE, FALSE)
  )
})

test_that("checks positions against the size of `dimensions`", {
  expect_identical(rray_as_extract_subscript(6L, c(2L, 3L)), 6L)
  expect_identical(
    rray_as_extract_subscript(-6L, c(2L, 3L)),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
  )

  expect_snapshot(error = TRUE, {
    rray_as_extract_subscript(7L, c(2L, 3L))
    rray_as_extract_subscript(-7L, c(2L, 3L))
    rray_as_extract_subscript(1L, c(2L, 0L))
  })
})

test_that("checks position signs", {
  expect_snapshot(error = TRUE, {
    rray_as_extract_subscript(c(-1L, 2L), 3L)
    rray_as_extract_subscript(c(-1L, NA), 3L)
  })
})

test_that("checks double positions are whole integers", {
  expect_snapshot(error = TRUE, {
    rray_as_extract_subscript(1.5, 3L)
    rray_as_extract_subscript(-1.5, 3L)
    rray_as_extract_subscript(1e10, 3L)
    rray_as_extract_subscript(Inf, 3L)
    rray_as_extract_subscript(-Inf, 3L)
  })
})

test_that("returns a bare logical mask", {
  expect_identical(
    rray_as_extract_subscript(c(TRUE, NA, FALSE), 3L),
    c(TRUE, NA, FALSE)
  )
  expect_identical(
    rray_as_extract_subscript(c(a = TRUE, b = FALSE), 2L),
    c(TRUE, FALSE)
  )
  expect_identical(rray_as_extract_subscript(logical(), 0L), logical())
})

test_that("keeps a scalar logical mask at size 1", {
  expect_identical(rray_as_extract_subscript(TRUE, c(2L, 3L)), TRUE)
  expect_identical(rray_as_extract_subscript(NA, c(2L, 3L)), NA)
  expect_identical(rray_as_extract_subscript(FALSE, 0L), FALSE)
})

test_that("drops the dimensions of a logical mask", {
  expect_identical(
    rray_as_extract_subscript(array(c(TRUE, FALSE), 2L), c(1L, 2L)),
    c(TRUE, FALSE)
  )
  expect_identical(
    rray_as_extract_subscript(
      array(TRUE, c(2L, 3L), dimnames = list(c("a", "b"), NULL)),
      c(2L, 3L)
    ),
    rep(TRUE, 6L)
  )
  expect_identical(
    rray_as_extract_subscript(array(TRUE, c(1L, 2L, 1L)), c(1L, 2L, 1L)),
    c(TRUE, TRUE)
  )
})

test_that("checks the size of a logical mask", {
  expect_snapshot(error = TRUE, {
    rray_as_extract_subscript(c(TRUE, FALSE), 3L)
    rray_as_extract_subscript(logical(), 3L)
    rray_as_extract_subscript(array(TRUE, 3L), c(2L, 3L))
  })
})

test_that("checks the dimensions of a logical array", {
  expect_snapshot(error = TRUE, {
    rray_as_extract_subscript(array(TRUE, c(3L, 2L)), c(2L, 3L))
    rray_as_extract_subscript(array(TRUE, c(2L, 3L, 1L)), c(2L, 3L))
    rray_as_extract_subscript(array(TRUE, c(6L, 1L)), 6L)
  })
})

test_that("returns an integer point matrix", {
  points <- matrix(c(2L, 1L, 3L, NA), 2L)

  expect_identical(rray_as_extract_subscript(points, c(2L, 3L)), points)
})

test_that("drops all point matrix attributes except `dim`", {
  points <- matrix(
    c(2L, 1L, 3L, 1L),
    2L,
    dimnames = list(c("a", "b"), c("c", "d"))
  )

  expect_identical(
    rray_as_extract_subscript(points, c(2L, 3L)),
    matrix(c(2L, 1L, 3L, 1L), 2L)
  )
})

test_that("converts a double point matrix to integer", {
  expect_identical(
    rray_as_extract_subscript(matrix(c(2, NA, 3, 1), 2L), c(2L, 3L)),
    matrix(c(2L, NA, 3L, 1L), 2L)
  )
})

test_that("allows a zero-row point matrix", {
  expect_identical(
    rray_as_extract_subscript(matrix(double(), 0L, 2L), c(2L, 0L)),
    matrix(integer(), 0L, 2L)
  )
})

test_that("requires one point matrix column per axis", {
  expect_identical(
    rray_as_extract_subscript(matrix(3L), 3L),
    matrix(3L)
  )

  expect_snapshot(error = TRUE, {
    rray_as_extract_subscript(matrix(1L, 1L, 3L), c(2L, 3L))
    rray_as_extract_subscript(matrix(1L, 1L, 2L), 3L)
  })
})

test_that("checks point coordinates against each axis", {
  expect_snapshot(error = TRUE, {
    rray_as_extract_subscript(rbind(c(3L, 1L)), c(2L, 3L))
    rray_as_extract_subscript(rbind(c(1L, 4L)), c(2L, 3L))
    rray_as_extract_subscript(rbind(c(0L, 1L)), c(2L, 3L))
    rray_as_extract_subscript(rbind(c(1L, -1L)), c(2L, 3L))
    rray_as_extract_subscript(rbind(c(1L, 1L)), c(2L, 0L))
  })
})

test_that("checks double point coordinates are whole integers", {
  expect_snapshot(error = TRUE, {
    rray_as_extract_subscript(rbind(c(1, 1.5)), c(2L, 3L))
  })
})

test_that("errors on numeric arrays with more than two dimensions", {
  expect_snapshot(error = TRUE, {
    rray_as_extract_subscript(array(1L, c(1L, 1L, 1L)), c(1L, 1L, 1L))
    rray_as_extract_subscript(array(1, c(1L, 1L, 1L, 1L)), 1L)
  })
})

test_that("errors on unsupported types", {
  expect_snapshot(error = TRUE, {
    rray_as_extract_subscript(NULL, 3L)
    rray_as_extract_subscript("a", 3L)
    rray_as_extract_subscript(matrix("a", 1L, 2L), c(2L, 3L))
    rray_as_extract_subscript(1i, 3L)
    rray_as_extract_subscript(as.raw(1L), 3L)
    rray_as_extract_subscript(list(1L), 3L)
  })
})

test_that("errors on classed input", {
  expect_snapshot(error = TRUE, {
    rray_as_extract_subscript(factor("a"), 3L)
    rray_as_extract_subscript(structure(1L, class = "foo"), 3L)
    rray_as_extract_subscript(data.frame(x = 1L), 3L)
  })
})

test_that("checks `dimensions`", {
  expect_snapshot(error = TRUE, {
    rray_as_extract_subscript(1L, -1L)
    rray_as_extract_subscript(1L, "a")
  })
})
