context("test-container-type2")

xs <- list(logical(), integer(), double(), character())
ys <- xs

# ------------------------------------------------------------------------------
# Base R container type2

for (x in xs) {
  for (y in ys) {
    test_that(glue::glue("container type2 {typeof(x)} and {typeof(y)}."), {
      expect_equal(vec_ptype_container2(x, y), logical())
    })
  }
}

# ------------------------------------------------------------------------------
# Bad `y`

bad_y <- new_vctr(1, class = "unknown")

for (x in xs) {
  test_that(glue::glue("container type2 {typeof(x)} errors with unknown `y`."), {
    expect_error(vec_ptype_container2(x, bad_y), class = "vctrs_error_incompatible_type")
  })
}

# ------------------------------------------------------------------------------
# Bad `x`

bad_x <- new_vctr(1, class = "unknown")

for (y in ys) {
  test_that("container type2 fails with unknown `x`", {
    expect_error(vec_ptype_container2(bad_x, y), class = "vctrs_error_incompatible_type")
  })
}

# ------------------------------------------------------------------------------
# NULL

test_that("container type2 allows NULL `x`", {
  expect_equal(vec_ptype_container2(NULL, 1), logical())
})

test_that("container type2 allows NULL `y`", {
  expect_equal(vec_ptype_container2(1, NULL), logical())
})

test_that("container type2 allows NULL `y` and `x`", {
  expect_equal(vec_ptype_container2(NULL, NULL), NULL)
})

# ------------------------------------------------------------------------------
# x = rray, y = base

for (y in ys) {
  test_that(glue::glue("container type2 rray and {typeof(y)}."), {
    expect_equal(vec_ptype_container2(rray(1), y), rray(logical()))
  })
}

# ------------------------------------------------------------------------------
# x = base, y = rray

for (x in xs) {
  test_that(glue::glue("container type2 {typeof(x)} and rray."), {
    expect_equal(vec_ptype_container2(x, rray(1)), rray(logical()))
  })
}

# ------------------------------------------------------------------------------
# rray default

bad_x <- new_vctr(1, class = "unknown")

test_that("container type2 rray errors with unknown `x`.", {
  expect_error(vec_ptype_container2(bad_x, rray(1)), class = "vctrs_error_incompatible_type")
})

# ------------------------------------------------------------------------------
# rray and rray

test_that("container type2 rray and rray.", {
  expect_equal(vec_ptype_container2(rray(1), rray(TRUE)), rray(logical()))
})

