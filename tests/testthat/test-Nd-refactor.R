test_that("subset by '[' returns Nd", {
  x <- Nd_example

  expect_s3_class(x[3], "Nd")
  expect_s3_class(x[1:2], "Nd")

  expect_error(x[1, 1])
  expect_error(x[1:2, 1])

  expect_warning(x[1:2, drop = TRUE])
  expect_warning(x[1:2, drop = FALSE])
})


test_that("subset and replace by '[' maintains Nd", {
  x <- Nd_example

  expect_s3_class(local({x[1] <- Nd(200); x}), "Nd")
  expect_s3_class(local({x[1:2] <- Nd(400); x}), "Nd")

  expect_error(x[1] <- 600)
  expect_error(x[1:2] <- "800")
  expect_error(x[1:2] <- survival::Surv(1000, FALSE, type = "left"))
})

