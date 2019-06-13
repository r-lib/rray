test_that("first input names are used", {
  x <- array(1, c(1, 1), dimnames = list("r1", "c1"))
  y <- array(1, c(1, 1), dimnames = list(NULL, NULL))

  expect_equal(
    rray_dim_names_common(x, y),
    rray_dim_names(x)
  )
})

test_that("first input names have priorities", {
  x <- array(1, c(1, 1), dimnames = list("r1", "c1"))
  y <- array(1, c(1, 1), dimnames = list("r2", "c2"))

  expect_equal(
    rray_dim_names_common(x, y),
    rray_dim_names(x)
  )
})

test_that("dim names can come from both inputs", {
  x <- array(1, c(1, 1), dimnames = list("r1", NULL))
  y <- array(1, c(1, 1), dimnames = list("r2", "c2"))

  expect_equal(
    rray_dim_names_common(x, y),
    list("r1", "c2")
  )
})

test_that("dim names have to match the common dim to be used", {
  x <- array(1, c(2, 1), dimnames = list(NULL, "c1"))
  y <- array(1, c(1, 2), dimnames = list("r2", NULL))

  expect_equal(
    rray_dim_names_common(x, y),
    list(NULL, NULL)
  )
})

test_that("meta dim names are used even if names aren't", {
  x <- array(1, c(2, 1), dimnames = list(x = NULL, "c1"))
  y <- array(1, c(1, 2), dimnames = list("r2", NULL))

  expect_equal(
    rray_dim_names_common(x, y),
    list(x = NULL, NULL)
  )
})

test_that("meta dim names are used from first input", {
  x <- array(1, dimnames = list(x = NULL))
  y <- array(1, dimnames = list(y = NULL))

  expect_equal(
    rray_dim_names_common(x, y),
    list(x = NULL)
  )
})

test_that("meta dim names can be constructed from both inputs", {
  x <- array(1, c(2, 1), dimnames = list(x = NULL, NULL))
  y <- array(1, c(1, 2), dimnames = list(NULL, y = NULL))

  expect_equal(
    rray_dim_names_common(x, y),
    list(x = NULL, y = NULL)
  )
})

