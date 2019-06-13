context("test-tile")

test_that("can tile a vector", {

  x <- new_array(1:5)

  expect_equal(
    rray_tile(x, 2),
    as_array(c(x, x))
  )

  expect_equal(
    rray_tile(x, c(1, 1)),
    new_matrix(x)
  )
})

test_that("can tile a matrix", {

  x <- new_matrix(1:5)

  expect_equal(
    rray_tile(x, 2),
    new_matrix(c(x, x))
  )
})

test_that("can tile a 3D array", {

  x <- new_array(1:8, c(2, 2, 2))

  expect_equal(
    rray_tile(x, 2),
    new_array(c(1:2, 1:2, 3:4, 3:4, 5:6, 5:6, 7:8, 7:8), c(4, 2, 2))
  )

  expect_equal(
    rray_tile(x, c(1, 1, 1, 1)),
    rray_expand(x, 4)
  )

})

test_that("names are carried along when tiling", {

  x <- new_array(1:5, c(5, 1), list(letters[1:5], "c1"))

  expect_equal(
    rray_row_names(rray_tile(x, 2)),
    rep(rray_row_names(x), 2)
  )

  expect_equal(
    rray_col_names(rray_tile(x, 2)),
    rray_col_names(x)
  )

})

test_that("can tile length 0 input", {

  x <- new_array(numeric())

  expect_equal(
    rray_tile(x, 1),
    x
  )

  expect_equal(
    rray_tile(x, c(1, 2)),
    new_array(numeric(), c(0, 2))
  )

  expect_equal(
    rray_tile(x, c(1, 2)),
    rray_tile(x, c(2, 2))
  )

  expect_equal(
    rray_tile(x, c(1, 2, 1)),
    new_array(numeric(), c(0, 2, 1))
  )
})

test_that("can tile 0 column input", {

  x <- new_matrix(numeric(), c(1, 0))

  expect_equal(
    rray_tile(x, 1),
    x
  )

  expect_equal(
    rray_tile(x, 2),
    rbind(x, x)
  )

  expect_equal(
    rray_tile(x, c(1, 2)),
    x
  )
})

test_that("`NULL` input", {
  expect_equal(rray_tile(NULL, c(1, 2)), NULL)
})
