context("test-rray-creation")

test_that("Can create rrays", {

  # [3, 1] no names
  expect_equal(
    new_rray(c(1, 2, 3), 3L, 1L),
    structure(
      c(1, 2, 3),
      dim = c(3L, 1L),
      dim_names = list(character(), character()),
      class = c("vctrs_rray", "vctrs_vctr")
    )
  )

  # [3, 1] with names
  expect_equal(
    new_rray(c(1, 2, 3), 3L, 1L, list(c("r1", "r2", "r3"), "c1")),
    structure(
      c(1, 2, 3),
      dim = c(3L, 1L),
      dim_names = list(c("r1", "r2", "r3"), "c1"),
      class = c("vctrs_rray", "vctrs_vctr")
    )
  )

  # [3, 1, 1]
  expect_equal(
    new_rray(c(1, 2, 3), 3L, c(1L, 1L)),
    structure(
      c(1, 2, 3),
      dim = c(3L, 1L, 1L),
      dim_names = list(character(), character(), character()),
      class = c("vctrs_rray", "vctrs_vctr")
    )
  )

  # [3, 1, 1] with null row names
  expect_equal(
    new_rray(c(1, 2, 3), 3L, c(1L, 1L), list(character(0), "c1", "depth1")),
    structure(
      c(1, 2, 3),
      dim = c(3L, 1L, 1L),
      dim_names = list(character(0), "c1", "depth1"),
      class = c("vctrs_rray", "vctrs_vctr")
    )
  )

})

test_that("Error if no dim", {
  expect_error(new_rray(c(1, 2, 3), dim_names = list(c("r1", "r2", "r3"), "c1")))
})

test_that("Error if bad dim", {
  expect_error(new_rray(c(1, 2, 3), "apple"))
  expect_error(new_rray(c(1, 2, 3), 3)) # not an integer
})

test_that("Error if bad names", {
  expect_error(new_rray(c(1, 2, 3), c(3L, 1L), list("only_one")))
  expect_error(new_rray(c(1, 2, 3), c(3L, 1L), list(1)))
})
