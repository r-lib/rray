test_that("`NULL` selects nothing", {
  expect_identical(
    rray_as_slice_subscript(NULL, 3L),
    new_subscript(integer(), "locations_int", 0L)
  )
})

test_that("returns integer locations unchanged", {
  expect_identical(
    rray_as_slice_subscript(c(3L, 1L, 3L), 3L),
    new_subscript(c(3L, 1L, 3L), "locations_int", 3L)
  )
  expect_identical(
    rray_as_slice_subscript(c(NA, 2L), 3L),
    new_subscript(c(NA, 2L), "locations_int", 2L)
  )
  expect_identical(
    rray_as_slice_subscript(integer(), 3L),
    new_subscript(integer(), "locations_int", 0L)
  )
})

test_that("returns double locations unchanged", {
  expect_identical(
    rray_as_slice_subscript(c(3, 1, NA), 3L),
    new_subscript(c(3, 1, NA), "locations_dbl", 3L)
  )
  expect_identical(
    rray_as_slice_subscript(double(), 3L),
    new_subscript(double(), "locations_dbl", 0L)
  )
})

test_that("keeps attributes on a 1D array", {
  i <- array(c(2L, 1L), 2L, dimnames = list(c("a", "b")))

  expect_identical(
    rray_as_slice_subscript(i, 3L),
    new_subscript(i, "locations_int", 2L)
  )
})

test_that("drops zero locations", {
  expect_identical(
    rray_as_slice_subscript(c(0L, 2L, 0L, NA), 3L),
    new_subscript(c(2L, NA), "locations_int", 2L)
  )
  expect_identical(
    rray_as_slice_subscript(c(3, 0), 3L),
    new_subscript(3, "locations_dbl", 1L)
  )
})

test_that("turns negative locations into a complement mask", {
  expect_identical(
    rray_as_slice_subscript(c(-1L, -3L), 4L),
    new_subscript(c(FALSE, TRUE, FALSE, TRUE), "mask", 2L)
  )
  expect_identical(
    rray_as_slice_subscript(c(-2, 0, -2), 3L),
    new_subscript(c(TRUE, FALSE, TRUE), "mask", 2L)
  )
})

test_that("checks locations against `dimension`", {
  expect_snapshot(error = TRUE, {
    rray_as_slice_subscript(4L, 3L)
    rray_as_slice_subscript(-4L, 3L)
    rray_as_slice_subscript(1L, 0L)
  })
})

test_that("checks location signs", {
  expect_snapshot(error = TRUE, {
    rray_as_slice_subscript(c(-1L, 2L), 3L)
    rray_as_slice_subscript(c(-1L, NA), 3L)
  })
})

test_that("checks double locations are whole integers", {
  expect_snapshot(error = TRUE, {
    rray_as_slice_subscript(1.5, 3L)
  })
})

test_that("returns a logical mask unchanged", {
  expect_identical(
    rray_as_slice_subscript(c(TRUE, NA, FALSE), 3L),
    new_subscript(c(TRUE, NA, FALSE), "mask", 2L)
  )
})

test_that("keeps a scalar logical mask at size 1", {
  expect_identical(
    rray_as_slice_subscript(TRUE, 3L),
    new_subscript(TRUE, "mask", 3L)
  )
  expect_identical(
    rray_as_slice_subscript(NA, 3L),
    new_subscript(NA, "mask", 3L)
  )
  expect_identical(
    rray_as_slice_subscript(FALSE, 3L),
    new_subscript(FALSE, "mask", 0L)
  )
  expect_identical(
    rray_as_slice_subscript(TRUE, 0L),
    new_subscript(TRUE, "mask", 0L)
  )
})

test_that("checks the size of a logical mask", {
  expect_snapshot(error = TRUE, {
    rray_as_slice_subscript(c(TRUE, FALSE), 3L)
    rray_as_slice_subscript(logical(), 3L)
  })
})

test_that("matches character names against `names`", {
  names <- c("a", "b", "c")

  expect_identical(
    rray_as_slice_subscript(c("c", "a", "c"), 3L, names),
    new_subscript(c(3L, 1L, 3L), "locations_int", 3L)
  )
  expect_identical(
    rray_as_slice_subscript(character(), 3L, names),
    new_subscript(integer(), "locations_int", 0L)
  )
})

test_that("character `NA` is a missing location", {
  expect_identical(
    rray_as_slice_subscript(c("b", NA), 2L, c("a", "b")),
    new_subscript(c(2L, NA), "locations_int", 2L)
  )
  expect_identical(
    rray_as_slice_subscript(NA_character_, 2L, c("a", NA)),
    new_subscript(NA_integer_, "locations_int", 1L)
  )
})

test_that("uses the first match for duplicate names", {
  expect_identical(
    rray_as_slice_subscript("a", 3L, c("b", "a", "a")),
    new_subscript(2L, "locations_int", 1L)
  )
})

test_that("checks character names", {
  expect_snapshot(error = TRUE, {
    rray_as_slice_subscript("a", 2L)
    rray_as_slice_subscript(c("a", ""), 2L, c("a", "b"))
    rray_as_slice_subscript(c("a", "z"), 2L, c("a", "b"))
  })
})

test_that("errors on subscripts with two or more dimensions", {
  expect_snapshot(error = TRUE, {
    rray_as_slice_subscript(matrix(1L), 3L)
    rray_as_slice_subscript(array(TRUE, c(1L, 1L, 1L)), 3L)
    rray_as_slice_subscript(matrix("a"), 3L, c("a", "b", "c"))
  })
})

test_that("errors on unsupported types", {
  expect_snapshot(error = TRUE, {
    rray_as_slice_subscript(1i, 3L)
    rray_as_slice_subscript(as.raw(1L), 3L)
    rray_as_slice_subscript(list(1L), 3L)
  })
})

test_that("errors on classed input", {
  expect_snapshot(error = TRUE, {
    rray_as_slice_subscript(factor("a"), 3L, "a")
    rray_as_slice_subscript(structure(1L, class = "foo"), 3L)
  })
})
