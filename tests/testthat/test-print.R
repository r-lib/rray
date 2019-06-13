test_that("printing works as expected", {
  expect_known_output(print(rray(1)), test_path("out/test-1D-rray.txt"))
  expect_known_output(print(rray(1, c(1, 1))), test_path("out/test-2D-rray.txt"))
})

test_that("printing works when one axis is 0", {
  expect_known_output(print(rray(numeric())), test_path("out/test-0-axis-1D.txt"))
  expect_known_output(print(rray(numeric(), c(0, 1))), test_path("out/test-0-axis-2D.txt"))
  expect_known_output(print(rray(numeric(), c(0, 1, 0))), test_path("out/test-0-axis-3D.txt"))
})

test_that("str() prints correct", {
  x <- rray(1:5)
  expect_known_output(str(x), test_path("out/test-str-1.txt"))

  # test long vector
  y <- rray(1:50)
  expect_known_output(str(y), test_path("out/test-str-2.txt"))
})

test_that("str() prints 2D+ correctly", {
  x <- rray(1, c(1, 1))
  expect_known_output(str(x), test_path("out/test-str-3.txt"))

  y <- rray(1:50, c(1, 1, 50))
  expect_known_output(str(y), test_path("out/test-str-4.txt"))
})

test_that("abbreviation is correct", {
  expect_equal(vec_ptype_abbr(rray()), "rray")
})

test_that("full ptype is correct", {
  expect_equal(vec_ptype_full(rray(1)), "rray<dbl>")
  expect_equal(vec_ptype_full(rray(1L)), "rray<int>")
  expect_equal(vec_ptype_full(rray(TRUE)), "rray<lgl>")

  expect_equal(vec_ptype_full(rray(1, c(1, 1))), "rray<dbl>[,1]")
  expect_equal(vec_ptype_full(rray(1, c(1, 1, 1))), "rray<dbl>[,1,1]")
  expect_equal(vec_ptype_full(rray(numeric(), c(0, 1, 1))), "rray<dbl>[,1,1]")
})
