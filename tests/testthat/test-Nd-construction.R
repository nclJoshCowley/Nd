test_that("Nd(<chr>) handles issues, 'NA' and NA_character_", {
  expect_warning(Nd(testdata_Nd))
  expect_no_error(suppressWarnings(Nd(testdata_Nd)))
})


test_that("Nd(<fct>) fails", {
  expect_error(Nd(factor(1)))
})


test_that("Nd(<dbl>) passes", {
  x2 <- stats::rnorm(n = 2)
  x10 <- stats::rnorm(n = 10)

  expect_no_error(Nd(x10))
  expect_no_warning(Nd(x10))

  expect_error(Nd(x = x10, is_nd = c(FALSE, TRUE)))
  expect_no_error(Nd(x = x2, is_nd = c(FALSE, TRUE)))
})


test_that("Nd(<int>) passes", {
  x2 <- sample.int(2)
  x10 <- sample.int(10)

  expect_no_error(Nd(x10))
  expect_no_warning(Nd(x10))

  expect_error(Nd(x = x10, is_nd = c(FALSE, TRUE)))
  expect_no_error(Nd(x = x2, is_nd = c(FALSE, TRUE)))
})


test_that("Nd(<Nd>) returns self", {
  expect_identical(Nd(Nd_example), Nd_example)
})


test_that("Nd() returns empty Nd object", {
  Nd_empty <- expect_no_error(Nd())

  expect_equal(NROW(Nd_empty), 0L)
  expect_output(print(Nd_empty), "Nd\\(0\\)")
})
