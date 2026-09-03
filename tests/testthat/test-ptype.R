test_that("the common type of every pair of native types", {
  expect_snapshot(native_ptype_matrix(rray_ptype2, c("x", "y")))
})

test_that("the type is read off arrays of any dimensionality or size", {
  expect_identical(rray_ptype2(1:2, array(1, c(2L, 3L))), double())
  expect_identical(rray_ptype2(array(TRUE, c(1L, 1L, 1L)), 1L), integer())
  expect_identical(rray_ptype2(array(integer(), c(0L, 2L)), 1), double())
})

test_that("`rray_ptype_common()` works with one input", {
  expect_identical(rray_ptype_common(1L), integer())
})

test_that("`rray_ptype_common()` climbs to the widest type", {
  expect_identical(rray_ptype_common(TRUE, 1L, 2.5), double())
  expect_identical(rray_ptype_common(2.5, 1L, TRUE), double())
  expect_identical(rray_ptype_common(TRUE, 1i, 1L), complex())
})

test_that("errors on types that don't combine", {
  expect_snapshot(rray_ptype2(character(), integer()), error = TRUE)
  expect_snapshot(rray_ptype2(raw(), integer()), error = TRUE)
  expect_snapshot(rray_ptype2(list(), double()), error = TRUE)
  expect_snapshot(rray_ptype2(character(), list()), error = TRUE)
})

test_that("the error names the input that set the common type", {
  expect_snapshot(rray_ptype_common(1L, "a"), error = TRUE)
  expect_snapshot(rray_ptype_common(1L, 2.5, "a"), error = TRUE)
  expect_snapshot(rray_ptype_common(x = 1L, y = 2.5, z = "a"), error = TRUE)
})

test_that("errors on no inputs", {
  expect_snapshot(rray_ptype_common(), error = TRUE)
})

test_that("errors on non-array input", {
  expect_snapshot(rray_ptype2(NULL, integer()), error = TRUE)
  expect_snapshot(rray_ptype2(integer(), sum), error = TRUE)
  expect_snapshot(rray_ptype_common(1L, NULL), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(1L, class = "foo")
  expect_snapshot(rray_ptype2(x, integer()), error = TRUE)
  expect_snapshot(rray_ptype2(integer(), x), error = TRUE)
  expect_snapshot(rray_ptype_common(1L, x), error = TRUE)
})
