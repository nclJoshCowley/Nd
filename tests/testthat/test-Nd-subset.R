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


test_that("subset by '[' compatible with survival::Surv", {
  x <- Nd_example

  expect_equal(x[, "time"], unclass(x)[, 1])
  expect_equal(x[, "status"], unclass(x)[, 2])

  ind <- c(1, 2, 10)
  expect_equal(x[ind, "time"], unclass(x)[ind, 1])
  expect_equal(x[ind, "status"], unclass(x)[ind, 2])
})


test_that("subset and replace by '[' maintains Nd", {
  x <- Nd_example

  expect_s3_class(local({x[1] <- Nd(200); x}), "Nd")
  expect_s3_class(local({x[1:2] <- Nd(400); x}), "Nd")
  expect_s3_class(local({x[1:2, drop = FALSE] <- Nd(400); x}), "Nd")

  expect_identical(
    local({x[1] <- Nd(200); x[1]}), Nd(200), label = "Post-transform x[1]"
  )

  expect_identical(
    local({x[1] <- Nd(200); x[3]}), x[3], label = "Post-transform x[3]"
  )

  expect_error(x[1] <- 600)
  expect_error(x[1:2, 1] <- "800")
  expect_error(x[1:2] <- survival::Surv(1000, FALSE, type = "left"))
})


test_that("subset by '$' returns correct type", {
  x <- Nd_example

  expect_type(x$value, "double")
  expect_type(x$is_nd, "logical")
  expect_error(x$is_na)
})


test_that("subset and replace by '$' works", {
  x <- Nd_example

  x_partial_change <-
    local({
      x$value[1:2] <- 1.234
      x$is_nd[1:2] <- FALSE
      x
    })

  expect_s3_class(x_partial_change, "Nd")
  expect_equal(x_partial_change[1:2], Nd(c(1.234, 1.234)))

  x_full_change <- local({
    x$value <- 4 * x$value
    x$is_nd <- !x$is_nd
    x
  })

  expect_s3_class(x_full_change, "Nd")
  expect_identical(x_full_change, Nd(4 * x$value, !x$is_nd))
})
