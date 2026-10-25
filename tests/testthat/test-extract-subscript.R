test_that("returns integer locations unchanged", {
  expect_identical(
    rray_as_extract_subscript(c(3L, 1L, 3L), 3L),
    new_subscript(c(3L, 1L, 3L), "locations_int", 3L)
  )
  expect_identical(
    rray_as_extract_subscript(integer(), 3L),
    new_subscript(integer(), "locations_int", 0L)
  )
})

test_that("returns double locations unchanged", {
  expect_identical(
    rray_as_extract_subscript(c(3, 1), 3L),
    new_subscript(c(3, 1), "locations_dbl", 2L)
  )
  expect_identical(
    rray_as_extract_subscript(c(NA, NaN), 3L),
    new_subscript(c(NA, NaN), "locations_dbl", 2L)
  )
  expect_identical(
    rray_as_extract_subscript(double(), 3L),
    new_subscript(double(), "locations_dbl", 0L)
  )
})

test_that("keeps attributes on locations", {
  i <- array(c(2L, 1L), 2L, dimnames = list(c("a", "b")))

  expect_identical(
    rray_as_extract_subscript(i, 3L),
    new_subscript(i, "locations_int", 2L)
  )
})

test_that("keeps missing locations", {
  expect_identical(
    rray_as_extract_subscript(c(NA, 2L, NA), 3L),
    new_subscript(c(NA, 2L, NA), "locations_int", 3L)
  )
})

test_that("drops zero locations", {
  expect_identical(
    rray_as_extract_subscript(c(0L, 2L, 0L, NA), 3L),
    new_subscript(c(2L, NA), "locations_int", 2L)
  )
  expect_identical(
    rray_as_extract_subscript(c(0L, 0L), 3L),
    new_subscript(integer(), "locations_int", 0L)
  )
  expect_identical(
    rray_as_extract_subscript(c(3, 0, NA), 3L),
    new_subscript(c(3, NA), "locations_dbl", 2L)
  )
})

test_that("turns negative locations into a complement mask", {
  expect_identical(
    rray_as_extract_subscript(c(-1L, -3L), 4L),
    new_subscript(c(FALSE, TRUE, FALSE, TRUE), "mask", 2L)
  )
  expect_identical(
    rray_as_extract_subscript(c(-2, 0, -2), 3L),
    new_subscript(c(TRUE, FALSE, TRUE), "mask", 2L)
  )
  expect_identical(
    rray_as_extract_subscript(-(1:3), 3L),
    new_subscript(c(FALSE, FALSE, FALSE), "mask", 0L)
  )
})

test_that("checks locations against the size of `dimensions`", {
  expect_identical(
    rray_as_extract_subscript(6L, c(2L, 3L)),
    new_subscript(6L, "locations_int", 1L)
  )
  expect_identical(
    rray_as_extract_subscript(-6L, c(2L, 3L)),
    new_subscript(c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE), "mask", 5L)
  )

  expect_snapshot(error = TRUE, {
    rray_as_extract_subscript(7L, c(2L, 3L))
    rray_as_extract_subscript(-7L, c(2L, 3L))
    rray_as_extract_subscript(1L, c(2L, 0L))
  })
})

test_that("checks location signs", {
  expect_snapshot(error = TRUE, {
    rray_as_extract_subscript(c(-1L, 2L), 3L)
    rray_as_extract_subscript(c(-1L, NA), 3L)
  })
})

test_that("checks double locations are whole integers", {
  expect_snapshot(error = TRUE, {
    rray_as_extract_subscript(1.5, 3L)
    rray_as_extract_subscript(-1.5, 3L)
  })
})

test_that("checks huge double locations against the size of `dimensions`", {
  expect_snapshot(error = TRUE, {
    rray_as_extract_subscript(1e10, 3L)
    rray_as_extract_subscript(Inf, 3L)
    rray_as_extract_subscript(-Inf, 3L)
  })
})

test_that("returns a logical mask unchanged", {
  expect_identical(
    rray_as_extract_subscript(c(TRUE, NA, FALSE), 3L),
    new_subscript(c(TRUE, NA, FALSE), "mask", 2L)
  )
  expect_identical(
    rray_as_extract_subscript(c(a = TRUE, b = FALSE), 2L),
    new_subscript(c(a = TRUE, b = FALSE), "mask", 1L)
  )
  expect_identical(
    rray_as_extract_subscript(logical(), 0L),
    new_subscript(logical(), "mask", 0L)
  )
})

test_that("keeps a scalar logical mask at size 1", {
  expect_identical(
    rray_as_extract_subscript(TRUE, c(2L, 3L)),
    new_subscript(TRUE, "mask", 6L)
  )
  expect_identical(
    rray_as_extract_subscript(NA, c(2L, 3L)),
    new_subscript(NA, "mask", 6L)
  )
  expect_identical(
    rray_as_extract_subscript(FALSE, c(2L, 3L)),
    new_subscript(FALSE, "mask", 0L)
  )
  expect_identical(
    rray_as_extract_subscript(TRUE, 0L),
    new_subscript(TRUE, "mask", 0L)
  )
})

test_that("accepts a logical array with the dimensions of `x`", {
  i <- array(c(TRUE, FALSE), 2L)
  expect_identical(
    rray_as_extract_subscript(i, c(1L, 2L)),
    new_subscript(i, "mask", 1L)
  )

  i <- array(TRUE, c(2L, 3L), dimnames = list(c("a", "b"), NULL))
  expect_identical(
    rray_as_extract_subscript(i, c(2L, 3L)),
    new_subscript(i, "mask", 6L)
  )

  i <- array(TRUE, c(1L, 2L, 1L))
  expect_identical(
    rray_as_extract_subscript(i, c(1L, 2L, 1L)),
    new_subscript(i, "mask", 2L)
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

test_that("returns an integer point matrix unchanged", {
  points <- matrix(c(2L, 1L, 3L, NA), 2L)

  expect_identical(
    rray_as_extract_subscript(points, c(2L, 3L)),
    new_subscript(points, "points_int", 2L)
  )
})

test_that("returns a double point matrix unchanged", {
  points <- matrix(c(2, NA, 3, 1), 2L)

  expect_identical(
    rray_as_extract_subscript(points, c(2L, 3L)),
    new_subscript(points, "points_dbl", 2L)
  )
})

test_that("keeps point matrix attributes", {
  points <- matrix(
    c(2L, 1L, 3L, 1L),
    2L,
    dimnames = list(c("a", "b"), c("c", "d"))
  )

  expect_identical(
    rray_as_extract_subscript(points, c(2L, 3L)),
    new_subscript(points, "points_int", 2L)
  )
})

test_that("allows a zero-row point matrix", {
  points <- matrix(double(), 0L, 2L)

  expect_identical(
    rray_as_extract_subscript(points, c(2L, 0L)),
    new_subscript(points, "points_dbl", 0L)
  )
})

test_that("requires one point matrix column per axis", {
  expect_identical(
    rray_as_extract_subscript(matrix(3L), 3L),
    new_subscript(matrix(3L), "points_int", 1L)
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
