creation_valid <- test_that("constructor (character) works", {
  expect_warning(Nd(testdata_Nd))
  expect_error(suppressWarnings(Nd(testdata_Nd)), NA)
})


test_that("constructor (factor) fails", {
  expect_error(Nd(factor(1)))
})


test_that("constructor (numeric) works", {
  expect_error(Nd(1:10), NA)
  expect_warning(Nd(1:10), NA)
  expect_error(Nd(x = 1:4, is_nd = c(FALSE, TRUE)))
})


test_that("passing Nd returns self", {
  skip_if_not(creation_valid)
  x_Nd <- suppressWarnings(Nd(testdata_Nd))

  expect_error(Nd(x_Nd), NA)
  expect_warning(Nd(x_Nd), NA)
  expect_equal(Nd(x_Nd), x_Nd)
})


test_that("passing no arguments returns empty Nd object", {
  Nd()
  skip_if_not(creation_valid)
  x_Nd <- suppressWarnings(Nd(testdata_Nd))

  expect_error(Nd(x_Nd), NA)
  expect_warning(Nd(x_Nd), NA)
  expect_equal(Nd(x_Nd), x_Nd)
})


subset_valid <- test_that("subset by '$' returns vector", {
  skip_if_not(creation_valid)
  x_Nd <- suppressWarnings(Nd(testdata_Nd))

  expect_type(x_Nd$value, "double")
  expect_type(x_Nd$is_nd, "logical")
  expect_error(x_Nd$is_na)
})


test_that("subset and modify using '$' works", {
  skip_if_not(creation_valid)
  skip_if_not(subset_valid)

  x_Nd <- suppressWarnings(Nd(testdata_Nd))

  x_Nd_modified <- local({
    x_Nd$value <- 4 * x_Nd$value
    x_Nd$is_nd <- !x_Nd$is_nd
    attr(x_Nd, "problems") <- NULL
    x_Nd
  })

  expect_equal(
    x_Nd_modified,
    Nd(c("ND<0.4", "ND<40", "2", "10.8", rep(NA, 6)))
  )
})

method_test <- function(desc, code) {
  test_that(
    paste(desc, "works as expected."),
    {
      skip_if_not(creation_valid)
      x_Nd <<- suppressWarnings(Nd(testdata_Nd))
      eval(code)
    }
  )
}


method_test("is.Nd()", {
  expect_true(is.Nd(x_Nd))
})


method_test("is.na.Nd()", {
  expect_equal(is.na(x_Nd), c(rep(FALSE, 4), rep(TRUE, 6)))
})


method_test("is.finite.Nd()", {
  expect_equal(is.finite(x_Nd), c(rep(TRUE, 4), rep(FALSE, 6)))
})


method_test("length.Nd()", {
  expect_equal(length(x_Nd), 10)
})


method_test("log.Nd()", {
  expect_equal(log(x_Nd)$value[1:2], log(x_Nd$value[1:2]))
  expect_error(log(log(log(x_Nd))))
})


method_test("exp.Nd()", {
  expect_equal(exp(x_Nd)$value[1:2], exp(x_Nd$value[1:2]))
})


method_test("mean.Nd()", {
  expect_error(mean(x_Nd))
})


test_that("impute by DL/2, original scale", {
  skip_if_not(creation_valid)
  x_Nd <- suppressWarnings(Nd(testdata_Nd))

  x_imputed <- expect_error(impute(x_Nd, "DL2"), NA)

  expect_equal(x_imputed, c(0.1, 10, 0.25, 1.35, rep(NA, 6)))
})


test_that("impute by DL/2, log scale", {
  skip_if_not(creation_valid)
  log_x_Nd <- suppressWarnings(log(Nd(testdata_Nd)))

  # FAILS while `delog` = FALSE
  expect_error(impute(log_x_Nd, impute_by_factor, impute_fct = 0.5))

  log_x_imputed <- expect_error(
    impute(log_x_Nd, delog = TRUE, impute_by_factor, impute_fct = 0.5),
    NA
  )

  expect_equal(log_x_imputed, log(c(0.1, 10, 0.25, 1.35, rep(NA, 6))))
})


test_that("impute by ROS (pre-sorted data)", {
  # Estimation of Descriptive Statistics for
  # Multiply Censored Water Quality Data
  Helsel <-
    new_Nd(
      value = c(1, 1, 1, 1, 1, 1, 3, 7, 9, 10, 10, 10, 12, 15, 20, 27, 33, 50),
      is_nd = c(T, T, T, T, T, T, F, F, F, T,  T,  T,  F,  F,  F,  F,  F,  F)
    )

  Helsel_imputed <- expect_error(impute(Helsel, "ROS"), NA)

  expect_equal(
    Helsel_imputed,
    NADA::ros(Helsel$value, Helsel$is_nd)$modeled
  )
})


test_that("impute by ROS (unsorted data)", {
  Helsel <-
    new_Nd(
      value = c(rep(1, 6), rep(10, 3), 3, 7, 9, 12, 15, 20, 27, 33, 50),
      is_nd = c(rep(TRUE, 9), rep(FALSE, 9))
    )

  x_imputed <- expect_error(impute(Helsel, "ROS"), NA)

  expect_equal(
    impute(Helsel, "ROS"),
    c(
      0.479, 0.857, 1.278, 1.766, 2.344, 3.039,
      1.113, 2.506, 4.805,
      3.000, 7.000, 9.000, 12.000, 15.000, 20.000, 27.000, 33.000, 50.000
    ),
    tolerance = 1e-3
  )
})

