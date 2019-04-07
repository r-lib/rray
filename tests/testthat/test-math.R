context("test-rray-math")

test_that("transpose works", {
  dat <- c(1, 2, 3, 4)
  x <- rray(dat, dim = c(4, 1))

  expect_equal(dim(t(x)), c(1, 4))
  expect_equal(vec_data(t(x)), t(vec_data(x)))

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
  expect_equal(vec_data(x_sclr), as_matrix(dat + 1))
  expect_equal(dim(x_sclr), c(4, 1))

  # same dim
  x_col <- x + x
  expect_is(x_col, "vctrs_rray")
  expect_equal(vec_data(x_col), as_matrix(dat + dat))
  expect_equal(dim(x_col), c(4, 1))

  # outer
  x_broad <- x + t(x)
  expect_is(x_broad, "vctrs_rray")
  expect_equal(
    vec_data(x_broad),
    new_matrix(unlist(map(1:4, function(x) x + c(1, 2, 3, 4))), dim = c(4, 4))
  )
  expect_equal(dim(x_broad), c(4, 4))

  # higher value of same dim
  x_broad2 <- x + rray_broadcast(x, c(4, 2))
  expect_is(x_broad2, "vctrs_rray")
  expect_equal(
    vec_data(x_broad2),
    new_matrix(rep(vec_data(x) + vec_data(x), 2), c(4, 2))
  )
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
  expect_equal(vec_data(x_sclr), as_matrix(dat - 1))
  expect_equal(dim(x_sclr), c(4, 1))

  # same dim
  x_col <- x - x
  expect_is(x_col, "vctrs_rray")
  expect_equal(vec_data(x_col), as_matrix(dat - dat))
  expect_equal(dim(x_col), c(4, 1))

  # outer
  x_broad <- x - t(x)
  expect_is(x_broad, "vctrs_rray")
  expect_equal(
    vec_data(x_broad),
    new_matrix(unlist(map(seq(-1, -4), function(x) x + c(1, 2, 3, 4))), c(4, 4))
  )
  expect_equal(dim(x_broad), c(4, 4))

  # higher value of same dim
  x_broad2 <- x - rray_broadcast(x, c(4, 2))
  expect_is(x_broad2, "vctrs_rray")
  expect_equal(
    vec_data(x_broad2),
    new_matrix(rep(vec_data(x) - vec_data(x), 2), c(4, 2))
  )
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
  expect_equal(vec_data(x_sclr), as_matrix(dat / 1))
  expect_equal(dim(x_sclr), c(4, 1))

  # same dim
  x_col <- x / x
  expect_is(x_col, "vctrs_rray")
  expect_equal(vec_data(x_col), as_matrix(dat / dat))
  expect_equal(dim(x_col), c(4, 1))

  # outer
  x_broad <- x / t(x)
  expect_is(x_broad, "vctrs_rray")
  expect_equal(
    vec_data(x_broad),
    new_matrix(unlist(map(1/(1:4), function(x) x * c(1, 2, 3, 4))), c(4, 4))
  )
  expect_equal(dim(x_broad), c(4, 4))

  # higher value of same dim
  x_broad2 <- x / rray_broadcast(x, c(4, 2))
  expect_is(x_broad2, "vctrs_rray")
  expect_equal(
    vec_data(x_broad2),
    new_matrix(rep(vec_data(x) / vec_data(x), 2), c(4, 2))
  )
  expect_equal(dim(x_broad2), c(4, 2))

  # 1 more dim
  y <- rray(dat, dim = c(4, 2, 2))
  x_broad3 <- x / y
  expect_is(x_broad3, "vctrs_rray")
  expect_equal(dim(x_broad3), c(4, 2, 2))
})

test_that("division coerces to double", {
  expect_equal(vec_data(rray(1L) / 2L), new_array(0.5))
})

test_that("multiplication works", {
  dat <- c(1, 2, 3, 4)
  x <- rray(dat, dim = c(4, 1))

  # with scalar
  x_sclr <- x * 2
  expect_is(x_sclr, "vctrs_rray")
  expect_equal(vec_data(x_sclr), as_matrix(dat * 2))
  expect_equal(dim(x_sclr), c(4, 1))

  # same dim
  x_col <- x * x
  expect_is(x_col, "vctrs_rray")
  expect_equal(vec_data(x_col), as_matrix(dat * dat))
  expect_equal(dim(x_col), c(4, 1))

  # outer
  x_broad <- x * t(x)
  expect_is(x_broad, "vctrs_rray")
  expect_equal(
    vec_data(x_broad),
    new_matrix(unlist(map(1:4, function(x) x * c(1, 2, 3, 4))), c(4, 4))
  )
  expect_equal(dim(x_broad), c(4, 4))

  # higher value of same dim
  x_broad2 <- x * rray_broadcast(x, c(4, 2))
  expect_is(x_broad2, "vctrs_rray")
  expect_equal(
    vec_data(x_broad2),
    new_matrix(rep(vec_data(x) * vec_data(x), 2), c(4, 2))
  )
  expect_equal(dim(x_broad2), c(4, 2))

  # 1 more dim
  y <- rray(dat, dim = c(4, 2, 2))
  x_broad3 <- x * y
  expect_is(x_broad3, "vctrs_rray")
  expect_equal(dim(x_broad3), c(4, 2, 2))
})

test_that("Fallthrough operation throws unsupported operation error", {
  expect_error(rray(1) + "a", "is not permitted")
})

test_that("meta dim names are kept", {
  x <- rray(1:2, c(2, 1))
  y <- rray(1:2, c(2, 1))
  dim_names(x) <- list(xR = NULL, xC = NULL)
  dim_names(y) <- list(yR = NULL, yC = NULL)

  expect_equal(
    names(dim_names(x + y)),
    names(dim_names(x))
  )

  expect_equal(
    names(dim_names(y + x)),
    names(dim_names(y))
  )

  names(dim_names(x)) <- NULL

  expect_equal(
    names(dim_names(x + y)),
    names(dim_names(y))
  )

  expect_equal(
    names(dim_names(y + x)),
    names(dim_names(y))
  )
})

test_that("can assign meta names to `NULL` without affecting dim names", {
  x <- rray(1:2, c(2, 1))
  dim_names(x) <- list(R = c("r1", "r2"), C = "c1")

  names(dim_names(x)) <- NULL

  expect_equal(names(dim_names(x)), NULL)
  expect_equal(dim_names(x), list(c("r1", "r2"), "c1"))
})
