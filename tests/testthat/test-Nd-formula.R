test_that("stats::model.frame works with Nd dependent variable", {
  formula <- y ~ x01 + x02
  data <- simulate_censreg_data(10, 1:3, 1, 0.2)

  expect_no_error({
    mf <- stats::model.frame(formula, data = data)
  })

  expect_s3_class(stats::model.response(mf), "Nd")
  expect_true(is.matrix(stats::model.matrix(mf, data = mf)))
})
