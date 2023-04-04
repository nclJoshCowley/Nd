test_that("impute by DL/2, original scale", {
  x <- Nd_example
  x_imputed <- expect_no_error(impute(x, "DL2"))

  expect_equal(x_imputed, ifelse(x$is_nd, x$value / 2, x$value))
})


test_that("impute by DL/2, log scale", {
  x <- Nd::Nd(c(0.01, 0.1, 1), is_nd = c(TRUE, FALSE, FALSE))
  log_x <- log(x)

  expect_error(impute(log_x, "DL2"))
  expect_no_error(impute(log_x, "DL2", delog = TRUE))

  expect_equal(
    impute(log_x, "DL2", delog = TRUE),
    log(ifelse(x$is_nd, x$value / 2, x$value))
  )
})


test_that("impute by ROS (pre-sorted data)", {
  x_ROS <- expect_no_error(
    suppressPackageStartupMessages(impute(Nd_example, "ROS"))
  )

  expect_equal(x_ROS, NADA::ros(Nd_example$value, Nd_example$is_nd)$modeled)
})


test_that("impute by ROS (unsorted data)", {
  x_sorted_by_detlim <- Nd_example[c(1:6, 10:12, 7:9, 13:18)]

  x_imputed <- expect_no_error(impute(x_sorted_by_detlim, "ROS"))

  expect_equal(
    x_imputed,
    c(
      0.479, 0.857, 1.278, 1.766, 2.344, 3.039,
      1.113, 2.506, 4.805,
      3.000, 7.000, 9.000, 12.000, 15.000, 20.000, 27.000, 33.000, 50.000
    ),
    tolerance = 1e-3
  )
})
