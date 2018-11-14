context("test-mtrx-creation")

test_that("Can create mtrxs", {

  # [3, 1] no names
  expect_equal(
    new_mtrx(c(1, 2, 3), size = 3L, shape = 1L),
    structure(
      c(1, 2, 3),
      dim = c(3L, 1L),
      dim_names = list(character(), character()),
      class = c("vctrs_mtrx", "vctrs_rray", "vctrs_vctr")
    )
  )

  # [3, 1] with names
  expect_equal(
    new_mtrx(c(1, 2, 3), size = 3L, shape = 1L, dim_names = list(c("r1", "r2", "r3"), "c1")),
    structure(
      c(1, 2, 3),
      dim = c(3L, 1L),
      dim_names = list(c("r1", "r2", "r3"), "c1"),
      class = c("vctrs_mtrx", "vctrs_rray", "vctrs_vctr")
    )
  )

})

test_that("Error if no dim", {
  expect_error(new_mtrx(c(1, 2, 3)))
})

test_that("Error if bad dim", {
  expect_error(new_mtrx(c(1, 2, 3), "apple"))
  expect_error(new_mtrx(c(1, 2, 3), 3)) # not an integer
})

test_that("Error if bad names", {
  expect_error(new_mtrx(c(1, 2, 3), size = 3L, shape = 1L, dim_names = list("only_one", character())))
  expect_error(new_mtrx(c(1, 2, 3), size = 3L, shape = 1L, dim_names = list(1, character())))
})
