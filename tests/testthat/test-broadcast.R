context("test-broadcast")

test_that("can broadcast dimension of 1 to N", {
  x <- as_rray(array(1, dim = c(1, 2)))
  expect_equal(vec_dim(rray_broadcast(x, c(5, 2))), c(5, 2))
})

test_that("broadcasting keeps original class", {
  x <- as_rray(array(1, dim = c(1, 2)))
  res <- rray_broadcast(x, c(1, 2))
  expect_is(res, "vctrs_rray")
})

test_that("can broadcast up to a new dimension", {
  x <- as_rray(array(1, dim = c(1, 2)))
  new_dim <- 2
  res <- rray_broadcast(x, c(1, 2, new_dim))
  expect_equal(vec_dim(rray_broadcast(x, c(1, 2, new_dim))), c(1, 2, new_dim))
})

# https://github.com/QuantStack/xtensor-r/issues/103
test_that("can broadcast 2x2 to same dimension and not change output", {
  x <- new_matrix(1:4, dim = c(2, 2))

  expect_equal(
    rray_broadcast(x, c(2L, 2L)),
    x
  )
})

test_that("can broadcast with 0 length input", {

  x <- new_array(numeric())

  expect_equal(rray_broadcast(x, 0), x)
  expect_equal(rray_broadcast(x, 1), x)

  expect_equal(
    rray_broadcast(x, c(0, 2)),
    new_array(numeric(), c(0, 2))
  )

  expect_equal(
    rray_broadcast(x, c(0, 1, 2)),
    new_array(numeric(), c(0, 1, 2))
  )

})

test_that("0 row input broadcasting", {

  x <- new_matrix(numeric(), c(0, 2))

  # Same shape
  expect_equal(
    rray_broadcast(x, c(0, 2)),
    x
  )

  # To 0 cols
  expect_equal(
    rray_broadcast(x, c(0, 0)),
    new_matrix(numeric(), c(0, 0))
  )

  # Broadcast to 1 row, but started with 0
  # This is a no-op
  expect_equal(
    rray_broadcast(x, c(1, 2)),
    new_matrix(numeric(), c(0, 2))
  )

  expect_error(rray_broadcast(x, c(0, 4)), "Non-recyclable")

})

test_that("0 col input broadcasting", {

  x <- new_matrix(numeric(), c(2, 0))

  # Same shape
  expect_equal(
    rray_broadcast(x, c(2, 0)),
    x
  )

  # Broadcast to 0 rows
  expect_equal(
    rray_broadcast(x, c(0, 0)),
    new_matrix(numeric(), c(0, 0))
  )

  # Broadcast to 1 col, but started with 0 cols
  # This is a no-op
  expect_equal(
    rray_broadcast(x, c(2, 1)),
    new_matrix(numeric(), c(2, 0))
  )

  expect_error(
    rray_broadcast(x, c(4, 0)),
    "Non-recyclable"
  )

})

test_that("broadcasting with `NULL`", {
  expect_equal(rray_broadcast(NULL, 1), NULL)
  expect_equal(rray_broadcast(NULL, c(1, 2)), NULL)
  expect_equal(rray_broadcast(NULL, c(2, 2, 3)), NULL)
})
