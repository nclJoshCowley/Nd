test_that("artificial_censor works with single argument", {
  x <- 0:10

  x_quant <- expect_no_error(Nd::artificial_censor(x, quantiles = 0.4))
  expect_equal(unique(x_quant$value[x_quant$is_nd]), 4)
  expect_equal(Nd::cens_ratio(x_quant), mean(x <= stats::quantile(x, 0.4)))

  x_detlim <- expect_no_error(Nd::artificial_censor(x, detlims = 4))
  expect_equal(unique(x_detlim$value[x_detlim$is_nd]), 4)
  expect_equal(Nd::cens_ratio(x_detlim), mean(x <= stats::quantile(x, 0.4)))

  expect_error(Nd::artificial_censor(x, quantiles = 0.4, detlims = 4))
})


test_that("artificial_censor works with multiple arguments", {
  x <- 0:10

  x_quant <- expect_no_error(Nd::artificial_censor(x, quantiles = c(0.2, 0.4)))
  expect_equal(x_quant$value[x_quant$is_nd], c(2, 2, 2, 4, 4))
  expect_equal(Nd::cens_ratio(x_quant), mean(x <= stats::quantile(x, 0.4)))

  x_detlim <- expect_no_error(Nd::artificial_censor(x, detlims = c(2, 4)))
  expect_equal(x_detlim$value[x_detlim$is_nd], c(2, 2, 2, 4, 4))
  expect_equal(Nd::cens_ratio(x_detlim), mean(x <= stats::quantile(x, 0.4)))
})
