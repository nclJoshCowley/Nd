test_that("is.Nd() as expected", {
  expect_true(is.Nd(Nd_example))
  expect_false(is.Nd(numeric()))
  expect_false(is.Nd(character()))
  expect_false(is.Nd(factor()))
})


test_that("is.na() as expected", {
  x <- suppressWarnings(Nd(testdata_Nd))
  expect_equal(is.na(x), c(rep(FALSE, 4), rep(TRUE, 6)))
})


test_that("is.finite() returns same as is.na()", {
  x <- suppressWarnings(Nd(testdata_Nd))

  expect_error(x$value[4] <- Inf)

  expect_equal(is.finite(x), c(rep(TRUE, 4), rep(FALSE, 6)))
})


test_that("length() as expected", {
  x <- Nd_example

  expect_equal(length(unclass(x)), 36)

  expect_equal(length(x), 18)
  expect_equal(nrow(x), 18)
  expect_equal(NROW(x), 18)
})


test_that("log() and exp() as expected", {
  x <- Nd_example

  expect_equal(log(x)$value, log(x$value))
  expect_error(log(log(x)))
  expect_error(log(Nd(-1.2)))

  expect_equal(exp(x)$value, exp(x$value))
})


test_that("mean() fails on Nd object", {
  expect_error(mean(Nd_example))
})


test_that("cens_ratio() as expected", {
  expect_error(Nd::cens_ratio(1:10))

  expect_equal(Nd::cens_ratio(Nd_example), 0.5)
})
