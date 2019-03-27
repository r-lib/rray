context("test-reshape")

test_that("can reshape up in dimensions", {

  expect_equal(
    rray_reshape(1, c(1, 1)),
    new_matrix(1)
  )

  expect_equal(
    rray_reshape(1:5, c(5, 1)),
    new_matrix(1:5)
  )

  expect_equal(
    rray_reshape(matrix(1), c(1, 1, 1)),
    new_array(1, c(1, 1, 1))
  )

})

test_that("can reshape down in dimensions", {

  expect_equal(
    rray_reshape(matrix(1), 1),
    new_array(1)
  )

  expect_equal(
    rray_reshape(matrix(1:5), 5),
    new_array(1:5)
  )

  expect_equal(
    rray_reshape(array(1:5, c(5, 1, 1)), 5),
    new_array(1:5)
  )
})

test_that("dimension names are maintained where appropriate", {

  x <- new_matrix(1:5, c(5, 1), list(meta = letters[1:5], "c1"))

  expect_equal(
    dim_names(rray_reshape(x, 5)),
    list(meta = letters[1:5])
  )

  expect_equal(
    dim_names(rray_reshape(x, c(5, 1, 1))),
    c(dim_names(x), new_empty_dim_names(1))
  )

})



