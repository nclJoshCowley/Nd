test_that("subset by '[' returns Nd", {
  x <- Nd_example

  expect_s3_class(x[3], "Nd")
  expect_s3_class(x[1:2], "Nd")

  # Empty arguments and `drop` required for [stats::model.frame()]
  expect_s3_class(x[1, ], "Nd")
  expect_s3_class(x[1, , drop = FALSE], "Nd")
  expect_s3_class(x[1, , drop = TRUE], "Nd")

  expect_error(x[1, 1])
  expect_error(x[1:2, 1])

})


test_that("subset and replace by '[' maintains Nd", {
  x <- Nd_example

  expect_s3_class(local({x[1] <- Nd(200); x}), "Nd")
  expect_s3_class(local({x[1:2] <- Nd(400); x}), "Nd")

  expect_identical(
    local({x[1] <- Nd(200); x[1]}), Nd(200), label = "Post-transform x[1]"
  )

  expect_identical(
    local({x[1] <- Nd(200); x[3]}), x[3], label = "Post-transform x[3]"
  )

  expect_error(x[1] <- 600)
  expect_error(x[1:2] <- "800")
  expect_error(x[1:2] <- survival::Surv(1000, FALSE, type = "left"))
})

