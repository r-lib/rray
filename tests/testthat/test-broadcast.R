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

# TODO - https://github.com/QuantStack/xtensor-r/issues/103
test_that("can broadcast 2x2 to same dimension and not change output", {
  x <- matrix(1:4, ncol = 2)

  expect_failure(
    expect_equal(
      rray_broadcast(x, c(2, 2)),
      x
    )
  )
})
