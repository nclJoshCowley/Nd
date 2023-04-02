test_that("Consistent visuals for autoplot.Nd", {
  plot_data <- tibble::enframe(Nd_example, name = "xval", value = "yval")

  vdiffr::expect_doppelganger(
    "Nd, quantile plot",
    ggplot2::autoplot(Nd_example, type = "quantile")
  )
})


test_that("Consistent visuals for Nd layer", {
  plot_data <- tibble::enframe(Nd_example, name = "xval", value = "yval")

  vdiffr::expect_doppelganger(
    "layer_Nd, with scale",
    ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$xval, y = .data$yval)) +
      layer_Nd_line(.data$yval)
  )

  vdiffr::expect_doppelganger(
    "layer_Nd, completely censored",
    plot_data[plot_data$yval$is_nd, ] |>
      ggplot2::ggplot(ggplot2::aes(x = .data$xval, y = .data$yval)) +
      layer_Nd_line(.data$yval)
  )

  vdiffr::expect_doppelganger(
    "layer_Nd, without scale",
    ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$xval, y = .data$yval)) +
      layer_Nd_line(.data$yval, scale = FALSE)
  )
})
