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
