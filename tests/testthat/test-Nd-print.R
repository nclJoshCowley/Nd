test_that("as.character.Nd() deals with NA / emptiness", {
  x_Nd <- suppressWarnings(Nd(testdata_Nd))

  expect_equal(
    as.character(x_Nd),
    c("0.1", "10", "ND<0.5", "ND<2.7", rep(NA_character_, 6))
  )

  expect_identical(as.character(Nd()), character(0))
})


test_that("format.Nd() rounds to correct significant digits", {
  x <- Nd(stats::rnorm(5, mean = 0), c(FALSE, FALSE, TRUE, TRUE, FALSE))

  x_rounded <- x
  x_rounded$value <- signif(x_rounded$value, digits = 3)

  expect_equal(Nd(format(x, digits = 3)), x_rounded)
})



test_that("print.Nd() consistent and works with emptiness", {
  x <- Nd(1:5, c(FALSE, FALSE, TRUE, TRUE, FALSE))

  expect_equal(capture.output(print(x)), "[1] 1    2    ND<3 ND<4 5   ")
  expect_equal(capture.output(print(Nd())), "[1] Nd(0)")
})
