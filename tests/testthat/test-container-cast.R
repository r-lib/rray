context("test-container-cast")

xs <- list(logical(), integer(), double())
tos <- xs

# ------------------------------------------------------------------------------
# Base R container casting

for (x in xs) {
  for (to in tos) {
    test_that(glue::glue("container casting {typeof(x)} to {typeof(to)}."), {
      expect_equal(vec_cast_container(x, to), x)
    })
  }
}

# ------------------------------------------------------------------------------
# Bad `to`

bad_to <- new_vctr(1, class = "unknown")

for (x in xs) {
  test_that(glue::glue("container casting {typeof(x)} errors with unknown `to`."), {
    expect_error(vec_cast_container(x, bad_to), class = "vctrs_error_incompatible_cast")
  })
}

# ------------------------------------------------------------------------------
# Bad `x`

bad_x <- new_vctr(1, class = "unknown")

for (to in tos) {
  test_that("container casting fails with unknown `x`", {
    expect_error(vec_cast_container(bad_x, to), class = "vctrs_error_incompatible_cast")
  })
}

# ------------------------------------------------------------------------------
# NULL

test_that("container casting allows NULL `x`", {
  expect_equal(vec_cast_container(NULL, 1), NULL)
})

test_that("container casting allows NULL `to`", {
  expect_equal(vec_cast_container(1, NULL), 1)
})

# ------------------------------------------------------------------------------
# rray to base

for (to in tos) {
  test_that(glue::glue("container casting rray to {typeof(to)}."), {
    expect_equal(vec_cast_container(rray(1), to), vec_data(rray(1)))
  })
}

# ------------------------------------------------------------------------------
# base to rray

for (x in xs) {
  test_that(glue::glue("container casting {typeof(x)} to rray."), {
    expect_equal(vec_cast_container(x, rray(1)), rray(x))
  })
}

# ------------------------------------------------------------------------------
# rray default

bad_x <- new_vctr(1, class = "unknown")

test_that("container casting rray errors with unknown `x`.", {
  expect_error(vec_cast_container(bad_x, rray(1)), class = "vctrs_error_incompatible_cast")
})

# ------------------------------------------------------------------------------
# rray to rray

test_that("container casting rray to rray.", {
  expect_equal(vec_cast_container(rray(1), rray(TRUE)), rray(1))
})

# ------------------------------------------------------------------------------
# Attribute testing

test_that("`x` retains shape", {
  x <- matrix(1:5)
  expect_equal(vec_cast_container(x, TRUE), x)
  expect_equal(vec_cast_container(x, rray(TRUE)), rray(1:5, c(5, 1)))
})

test_that("`x` retains dim names", {
  x <- array(1, dimnames = list("foo"))
  expect_equal(vec_cast_container(x, TRUE), x)
  expect_equal(vec_cast_container(x, rray(TRUE)), rray(1, dim_names = list("foo")))
})


# ------------------------------------------------------------------------------
# Common container cast

test_that("common container cast can be found", {
  expect_equal(vec_cast_container_common(1, 1L), list(1, 1L))
  expect_equal(vec_cast_container_common(1, rray(1L)), list(rray(1), rray(1L)))
})

test_that("common container cast with 1 input", {
  expect_equal(vec_cast_container_common(1), list(1))
})

test_that("common container cast with no input", {
  expect_equal(vec_cast_container_common(), list())
})

test_that("can specify ptype", {
  expect_equal(vec_cast_container_common(1, .to = rray(1L)), list(rray(1)))
})
