test_that("returns NULL for unnamed plain vectors", {
  expect_null(rray_dimension_names(1:3))
  expect_null(rray_dimension_names("a"))
})

test_that("returns names for named plain vectors", {
  expect_identical(rray_dimension_names(c(a = 1, b = 2)), list(c("a", "b")))
  expect_identical(rray_dimension_names(c(x = 1L)), list("x"))
})

test_that("returns dimnames for arrays", {
  x <- array(1:6, c(2, 3), dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  expect_identical(
    rray_dimension_names(x),
    list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
})

test_that("returns NULL for unnamed arrays", {
  expect_null(rray_dimension_names(array(1:6, c(2, 3))))
  expect_null(rray_dimension_names(array(1:24, c(2, 3, 4))))
})

test_that("handles partially named dimnames", {
  x <- array(1:6, c(2, 3), dimnames = list(c("r1", "r2"), NULL))
  expect_identical(rray_dimension_names(x), list(c("r1", "r2"), NULL))
})

test_that("errors on non-vector types", {
  expect_snapshot(rray_dimension_names(NULL), error = TRUE)
  expect_snapshot(rray_dimension_names(mean), error = TRUE)
})
