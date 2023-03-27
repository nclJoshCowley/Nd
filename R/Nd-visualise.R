#' Plot Non-Detect Data
#'
#' Visualisation methods for non-detect data.
#'
#' @template Nd-method
#' @param ... Extra arguments passed to other methods.
#'
#' @export
plot.Nd <- function(x, ...) {
  requireNamespace("ggplot2")
  print(ggplot2::autoplot(x, ...))
}


#' @rdname plot.Nd
#' @export
autoplot.Nd <- function(x, ...) {
  quantile_plot_Nd(x, ...)
}


#' Quantile Plot of Non-Detect Data
#'
#' Quantile plot comparing the rank to the value of data.
#'
#' @inheritParams plot.Nd
#'
#' @importFrom rlang .data
#' @keywords internal
quantile_plot_Nd <- function(x) {
  requireNamespace("ggplot2")

  quantile_data <-
    data.frame(
      i = seq_along(x),
      value = x$value,
      is_nd = factor(x$is_nd, c(FALSE, TRUE), c("Observed", "Censored")),
      rank = match(seq_along(x), order(x))
    )

  cur_scale_colour <- getOption(
    "ggplot2.discrete.colour",
    default = ggplot2::scale_colour_manual(
      name = NULL, drop = FALSE,
      values = c("Censored" = "#B03333", "Observed" = "black")
    )
  )

  cur_scale_shape <- getOption(
    "ggplot2.discrete.colour",
    default = ggplot2::scale_shape_manual(
      name = NULL, drop = FALSE,
      values = c("Censored" = 1, "Observed" = 16)
    )
  )

  quantile_data |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$value, y = .data$rank,
      shape = .data$is_nd, colour = .data$is_nd
    )) +
    ggplot2::geom_point() +
    cur_scale_shape +
    cur_scale_colour +
    ggplot2::labs(y = "Rank", x = NULL)
}
