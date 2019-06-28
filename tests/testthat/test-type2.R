context("test-rray-type2")

test_that("vec_ptype2 with same dimensions", {
  x <- as_rray(array(1, dim = c(2, 2, 2)))

  expect_equal(dim(vec_ptype2(x, x)), c(0, 2, 2))
  expect_equal(vec_ptype2(x, x), vec_ptype(x))

  expect_equal(dim(vec_ptype(x[0,0])), c(0, 0, 2))

  expect_equal(vec_ptype_common(x, x), vec_ptype2(x, x))
})


test_that("Common dim is found", {

  x <- as_rray(array(1, dim = c(2, 2, 2)))
  y <- as_rray(array(1, dim = c(2, 2)))

  # common dim
  expect_equal(dim(vec_ptype2(x, y)), c(0, 2, 2))
  expect_equal(dim(vec_ptype2(y, x)), c(0, 2, 2))

  expect_equal(vec_ptype_common(x, y), vec_ptype2(x, y))
})

test_that("Common inner type is found", {

  x <- rray(1)
  y <- rray(1L)
  z <- 1L

  # numeric + integer
  expect_equal(vec_ptype2(x, y), new_rray(numeric()))

  # numeric + integer
  expect_equal(vec_ptype2(x, z), new_rray(numeric()))

  # integer + integer
  expect_equal(vec_ptype2(y, z), new_rray(integer()))
})
