context("test-math-trigonometric")

.fs <- c(
  rray_sin,
  rray_cos,
  rray_tan,
  rray_asin,
  rray_acos,
  rray_atan,
  rray_sinpi,
  rray_cospi,
  rray_tanpi
)

.fs_base <- c(
  sin,
  cos,
  tan,
  asin,
  acos,
  atan,
  sinpi,
  cospi,
  tanpi
)

.f_names <- c(
  "sin",
  "cos",
  "tan",
  "asin",
  "acos",
  "atan",
  "sinpi",
  "cospi",
  "tanpi"
)

for (i in seq_along(.fs)) {
  .f <- .fs[[i]]
  .f_base <- .fs_base[[i]]
  .f_name <- .f_names[[i]]

  test_that(glue::glue("basic - {.f_name}"), {
    expect_equal(.f(rray(2)), rray(suppressWarnings(.f_base(2))))
    expect_equal(.f(rray(TRUE)), rray(.f_base(TRUE)))
    expect_equal(.f(rray(matrix(1:5))), rray(suppressWarnings(.f_base(matrix(1:5)))))
  })

  test_that(glue::glue("dim names are kept - {.f_name}"), {
    nms <- list("r1", "c1")
    x <- rray(1, c(1, 1), dim_names = nms)
    expect_equal(dim_names(.f(x)), dim_names(x))
  })

  test_that(glue::glue("corner cases - {.f_name}"), {
    expect_equal(suppressWarnings(.f(Inf)), new_array(suppressWarnings(.f_base(Inf))))
    expect_equal(suppressWarnings(.f(-Inf)), new_array(suppressWarnings(.f_base(-Inf))))

    expect_equal(.f(0L), new_array(.f_base(0)))

    expect_equal(.f(NaN), new_array(.f_base(NaN)))
  })

  test_that(glue::glue("vctrs dispatch works - {.f_name}"), {
    expect_equal(.f_base(rray(1)), .f(rray(1)))
    expect_equal(.f_base(rray(1L)), .f(rray(1L)))
  })
}

# ------------------------------------------------------------------------------
# Extra corner case tests for specific trig functions

test_that("*pi() functions are more accurate", {
  expect_equal(rray_sinpi(1), new_array(sinpi(1)))
  expect_equal(rray_cospi(1), new_array(cospi(1)))
  expect_equal(rray_tanpi(1), new_array(tanpi(1)))
})

# ------------------------------------------------------------------------------
# tests for atan2()

test_that("basic - atan2", {
  expect_equal(rray_atan2(1, 1), new_array(atan2(1, 1)))
  expect_equal(rray_atan2(TRUE, 1), new_array(atan2(TRUE, 1)))
  expect_equal(rray_atan2(matrix(1:5), 1), atan2(new_matrix(1:5), 1))
})

test_that("dim names are kept - atan2", {
  nms <- list("r1", NULL)
  nms2 <- list("rr1", "cc1")

  x <- rray(1, c(1, 1), dim_names = nms)
  y <- rray(1, c(1, 1), dim_names = nms2)

  expect_equal(dim_names(rray_atan2(x, y)), list("r1", "cc1"))
  expect_equal(dim_names(rray_atan2(y, x)), list("rr1", "cc1"))
})

test_that("corner cases - atan2", {
  expect_equal(rray_atan2(Inf, Inf), new_array(atan2(Inf, Inf)))
  expect_equal(rray_atan2(Inf, -Inf), new_array(atan2(Inf, -Inf)))

  expect_equal(rray_atan2(0L, 0L), new_array(atan2(0, 0)))

  expect_equal(rray_atan2(NaN, 1), new_array(atan2(NaN, 1)))
})
