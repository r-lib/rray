
test_that("can flatten input", {
  expect_equal(rray_flatten(1:5), new_array(1:5))

  x <- matrix(1:6, 2)
  expect_equal(rray_flatten(x), new_array(as.vector(x)))

  x <- array(1:8, c(2, 2, 2))
  expect_equal(rray_flatten(as.vector(x)), new_array(as.vector(x)))
})

test_that("rray class is kept", {
  expect_equal(rray_flatten(rray(1)), rray(1))
})

test_that("can keep names with 1D objects", {
  x <- rray(1, dim_names = list("foo"))
  expect_equal(rray_dim_names(rray_flatten(x)), rray_dim_names(x))
})

test_that("can keep names with higher dim objects", {
  x <- rray(1:2, c(2, 1), dim_names = list(c("foo", "foofy"), "bar"))
  expect_equal(rray_dim_names(rray_flatten(x)), list(c("foo", "foofy")))

  # no names
  x_t <- t(x)
  expect_equal(rray_dim_names(rray_flatten(x_t)), list(NULL))
})

test_that("can flatten NULL", {
  expect_equal(rray_flatten(NULL), NULL)
})

test_that("can flatten 0 length input", {
  expect_equal(rray_flatten(numeric()), new_array(numeric()))
})
