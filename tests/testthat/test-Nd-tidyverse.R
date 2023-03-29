#' Unit tests related to interaction beyond this package (tibble, tidyr, etc.)

test_that("tidyr::pivot_longer works as expected", {
  Nd_example_named <- rlang::set_names(Nd_example, ~ letters[seq_along(.x)])

  actual <- tidyr::pivot_wider(tibble::enframe(Nd_example_named))
  expect <- tibble::as_tibble_row(Nd_example_named)

  expect_equal(actual, expect)
})
