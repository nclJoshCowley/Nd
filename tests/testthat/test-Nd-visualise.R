test_that("Consistent visuals for autoplot.Nd", {
  plot_data <- tibble::enframe(Nd_example, name = "xval", value = "yval")

  vdiffr::expect_doppelganger(
    "Nd Quantile plot",
    ggplot2::autoplot(Nd_example, type = "quantile")
  )
})


test_that("Consistent visuals for Nd layer", {
  plot_data <- tibble::enframe(Nd_example, name = "xval", value = "yval")

  vdiffr::expect_doppelganger(
    "Simple time series plot, with scale",
    ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$xval, y = .data$yval)) +
      layer_Nd_line(.data$yval)
  )

  vdiffr::expect_doppelganger(
    "Simple time series plot, without scale",
    ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$xval, y = .data$yval)) +
      layer_Nd_line(.data$yval, scale = FALSE)
  )
})
