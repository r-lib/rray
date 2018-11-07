context("test-rray-math")

test_that("transpose works", {
  dat <- c(1, 2, 3, 4)
  x <- rray(dat, dim = c(4, 1))

  expect_equal(dim(t(x)), c(1, 4))
  expect_equal(vec_data(t(x)), vec_data(x))

  y <- rray_broadcast(x, c(4,2))

  expect_equal(dim(t(y)), c(2, 4))
  expect_equal(t(y)[1,], t(y)[2,])
})

test_that("addition works", {
  dat <- c(1, 2, 3, 4)
  x <- rray(dat, dim = c(4, 1))

  # with scalar
  x_sclr <- x + 1
  expect_is(x_sclr, "vctrs_rray")
  expect_equal(vec_data(x_sclr), dat + 1)
  expect_equal(dim(x_sclr), c(4, 1))

  # same dim
  x_col <- x + x
  expect_is(x_col, "vctrs_rray")
  expect_equal(vec_data(x_col), dat + dat)
  expect_equal(dim(x_col), c(4, 1))

  # outer
  x_broad <- x + t(x)
  expect_is(x_broad, "vctrs_rray")
  expect_equal(vec_data(x_broad), unlist(map(1:4, function(x) x + c(1, 2, 3, 4))))
  expect_equal(dim(x_broad), c(4, 4))

  # higher value of same dim
  x_broad2 <- x + rray_broadcast(x, c(4, 2))
  expect_is(x_broad2, "vctrs_rray")
  expect_equal(vec_data(x_broad2), rep(vec_data(x) + vec_data(x), 2))
  expect_equal(dim(x_broad2), c(4, 2))

  # 1 more dim
  y <- rray(dat, dim = c(4, 2, 2))
  x_broad3 <- x + y
  expect_is(x_broad3, "vctrs_rray")
  expect_equal(dim(x_broad3), c(4, 2, 2))
})

test_that("subtraction works", {
  dat <- c(1, 2, 3, 4)
  x <- rray(dat, dim = c(4, 1))

  # with scalar
  x_sclr <- x - 1
  expect_is(x_sclr, "vctrs_rray")
  expect_equal(vec_data(x_sclr), dat - 1)
  expect_equal(dim(x_sclr), c(4, 1))

  # same dim
  x_col <- x - x
  expect_is(x_col, "vctrs_rray")
  expect_equal(vec_data(x_col), dat - dat)
  expect_equal(dim(x_col), c(4, 1))

  # outer
  x_broad <- x - t(x)
  expect_is(x_broad, "vctrs_rray")
  expect_equal(vec_data(x_broad), unlist(map(seq(-1, -4), function(x) x + c(1, 2, 3, 4))))
  expect_equal(dim(x_broad), c(4, 4))

  # higher value of same dim
  x_broad2 <- x - rray_broadcast(x, c(4, 2))
  expect_is(x_broad2, "vctrs_rray")
  expect_equal(vec_data(x_broad2), rep(vec_data(x) - vec_data(x), 2))
  expect_equal(dim(x_broad2), c(4, 2))

  # 1 more dim
  y <- rray(dat, dim = c(4, 2, 2))
  x_broad3 <- x - y
  expect_is(x_broad3, "vctrs_rray")
  expect_equal(dim(x_broad3), c(4, 2, 2))
})

test_that("division works", {
  dat <- c(1, 2, 3, 4)
  x <- rray(dat, dim = c(4, 1))

  # with scalar
  x_sclr <- x / 1
  expect_is(x_sclr, "vctrs_rray")
  expect_equal(vec_data(x_sclr), dat / 1)
  expect_equal(dim(x_sclr), c(4, 1))

  # same dim
  x_col <- x / x
  expect_is(x_col, "vctrs_rray")
  expect_equal(vec_data(x_col), dat / dat)
  expect_equal(dim(x_col), c(4, 1))

  # outer
  x_broad <- x / t(x)
  expect_is(x_broad, "vctrs_rray")
  expect_equal(vec_data(x_broad), unlist(map(1/(1:4), function(x) x * c(1, 2, 3, 4))))
  expect_equal(dim(x_broad), c(4, 4))

  # higher value of same dim
  x_broad2 <- x / rray_broadcast(x, c(4, 2))
  expect_is(x_broad2, "vctrs_rray")
  expect_equal(vec_data(x_broad2), rep(vec_data(x) / vec_data(x), 2))
  expect_equal(dim(x_broad2), c(4, 2))

  # 1 more dim
  y <- rray(dat, dim = c(4, 2, 2))
  x_broad3 <- x / y
  expect_is(x_broad3, "vctrs_rray")
  expect_equal(dim(x_broad3), c(4, 2, 2))
})

test_that("multiplication works", {
  dat <- c(1, 2, 3, 4)
  x <- rray(dat, dim = c(4, 1))

  # with scalar
  x_sclr <- x * 2
  expect_is(x_sclr, "vctrs_rray")
  expect_equal(vec_data(x_sclr), dat * 2)
  expect_equal(dim(x_sclr), c(4, 1))

  # same dim
  x_col <- x * x
  expect_is(x_col, "vctrs_rray")
  expect_equal(vec_data(x_col), dat * dat)
  expect_equal(dim(x_col), c(4, 1))

  # outer
  x_broad <- x * t(x)
  expect_is(x_broad, "vctrs_rray")
  expect_equal(vec_data(x_broad), unlist(map(1:4, function(x) x * c(1, 2, 3, 4))))
  expect_equal(dim(x_broad), c(4, 4))

  # higher value of same dim
  x_broad2 <- x * rray_broadcast(x, c(4, 2))
  expect_is(x_broad2, "vctrs_rray")
  expect_equal(vec_data(x_broad2), rep(vec_data(x) * vec_data(x), 2))
  expect_equal(dim(x_broad2), c(4, 2))

  # 1 more dim
  y <- rray(dat, dim = c(4, 2, 2))
  x_broad3 <- x * y
  expect_is(x_broad3, "vctrs_rray")
  expect_equal(dim(x_broad3), c(4, 2, 2))
})
