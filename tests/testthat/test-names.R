context("test-names")

test_that("can assign names() on 1D rrays", {

  x <- rray(1:5)

  names(x) <- letters[1:5]

  expect_equal(
    rray_dim_names(x),
    list(letters[1:5])
  )

  expect_is(x, "vctrs_rray")

  x_unnamed <- unname(x)
  expect_equal(attr(x_unnamed, "dimnames"), rray_empty_dim_names(1))

})

test_that("cannot assign names() on a 2D+ rray", {
  x <- rray(1:5, dim = c(5, 1))
  expect_error(names(x) <- letters[1:5], "Cannot set `names`")
})
