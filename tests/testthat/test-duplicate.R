context("test-duplicate")

test_that("duplicates along `axis = 1` is equal to vctrs", {
  x <- c(1, 1, 2, 2, 3)
  expect_equal(rray_duplicate_any(x), vec_duplicate_any(x))
  expect_equal(rray_duplicate_detect(x), vec_duplicate_detect(x))
  expect_equal(rray_duplicate_id(x), vec_duplicate_id(x))

  x <- rray(c(1, 1, 2, 2), c(2, 2))
  expect_equal(rray_duplicate_any(x), vec_duplicate_any(x))
  expect_equal(rray_duplicate_detect(x), vec_duplicate_detect(x))
  expect_equal(rray_duplicate_id(x), vec_duplicate_id(x))
})

test_that("can compute duplicates along columns", {

  x <- rray(c(1, 1, 2, 2), c(1, 4))

  expect_true(
    rray_duplicate_any(x, axis = 2L),
  )

  expect_equal(
    rray_duplicate_detect(x, 2),
    rep(TRUE, times = 4)
  )

  expect_equal(
    rray_duplicate_detect(x, 1),
    FALSE
  )

  expect_equal(
    rray_duplicate_id(x, 2),
    c(1, 1, 3, 3)
  )

})

test_that("`axis` is validated", {
  axis <- c(1, 2)
  expect_error(rray_duplicate_any(1, axis), "Invalid `axis`")
  expect_error(rray_duplicate_detect(1, axis), "Invalid `axis`")
  expect_error(rray_duplicate_id(1, axis), "Invalid `axis`")

  axis <- -1
  expect_error(rray_duplicate_any(1, axis), "Invalid `axis`")
  expect_error(rray_duplicate_detect(1, axis), "Invalid `axis`")
  expect_error(rray_duplicate_id(1, axis), "Invalid `axis`")

  axis <- 2
  expect_error(rray_duplicate_any(1, axis), "Invalid `axis`")
  expect_error(rray_duplicate_detect(1, axis), "Invalid `axis`")
  expect_error(rray_duplicate_id(1, axis), "Invalid `axis`")
})

test_that("`NULL` input", {
  expect_equal(rray_duplicate_any(NULL, 2L), FALSE)
  expect_equal(rray_duplicate_detect(NULL, 2L), logical(0))
  expect_equal(rray_duplicate_id(NULL, 2L), integer(0))
})
