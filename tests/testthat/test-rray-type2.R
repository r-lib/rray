context("test-rray-type2")

test_that("vec_type2 with same dimensions", {
  x <- as_rray(array(1, dim = c(2, 2, 2)))

  expect_equal(dim(vec_type2(x, x)), c(0, 2, 2))
  expect_equal(vec_type2(x, x), vec_type(x))

  expect_equal(dim(vec_type(x[0,0])), c(0, 0, 2))

  expect_equal(vec_type_common(x, x), vec_type2(x, x))
})


test_that("Common dim is found", {

  x <- as_rray(array(1, dim = c(2, 2, 2)))
  y <- as_rray(array(1, dim = c(2, 2)))

  # common dim
  expect_equal(dim(vec_type2(x, y)), c(0, 2, 2))
  expect_equal(dim(vec_type2(y, x)), c(0, 2, 2))

  expect_equal(vec_type_common(x, y), vec_type2(x, y))
})

test_that("rray and mtrx = rray", {

  x <- as_rray(array(1, dim = c(2, 1)))
  y <- mtrx(1:2, 1:2)

  # common dim
  expect_is(vec_type2(x, y), "vctrs_rray")
  expect_equal(dim(vec_type2(x, y)), c(0, 2))
  expect_equal(dim(vec_type2(y, x)), c(0, 2))

  expect_equal(vec_type_common(x, y), vec_type2(x, y))
})
