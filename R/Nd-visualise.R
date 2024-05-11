#' Plot Non-Detect Data
#'
#' Visualisation methods for non-detect data.
#'
#' @template Nd-method
#' @param ... Extra arguments passed to other methods.
#' @param type choice. For future extensions where plot type is to be chosen.
#'
#' @export
plot.Nd <- function(x, ..., type = "quantile") {
  requireNamespace("ggplot2")
  print(ggplot2::autoplot(x, ..., type = type))
}


#' @rdname plot.Nd
#' @export
autoplot.Nd <- function(x, ..., type = "quantile") {
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
      rank = match(seq_along(x), order(x$value))
    )

  scale_colour_type <-
    getOption(
      "ggplot2.discrete.colour",
      default = c("Censored" = "#B03333", "Observed" = "black")
    )

  scale_shape_values <- c("Censored" = 1, "Observed" = 16)

  quantile_data |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$value, y = .data$rank,
      shape = .data$is_nd, colour = .data$is_nd
    )) +
    ggplot2::geom_point() +
    ggplot2::labs(y = "Rank", x = NULL, colour = NULL, shape = NULL) +
    ggplot2::scale_colour_discrete(drop = FALSE, type = scale_colour_type) +
    ggplot2::scale_shape_manual(drop = FALSE, values = scale_shape_values)
}


#' Add `ggplot2` Layer for Non-Detect Data
#'
#' In absence of a rigorous `ggplot2` extension, this function accepts a
#'   mapping to `y` and returns `geom_point`, `geom_line` and optional scale.
#'
#' @param y expression. Variable to use on y-axis.
#' @param scale logical. When `TRUE`, a custom `scale_shape_manual` is added.
#' @param ... Extra arguments passed to **both** geoms.
#'
#' @note The main use for `y` is to be placed in `aes(...)` with added
#'   information such as `value` or `is_nd`. As such no data is handled here.
#'
#' @export
layer_Nd_line <- function(y, ..., scale = TRUE) {

  y_uneval <- rlang::enexpr(y)

  mapping <-
    ggplot2::aes(
      y = `$`(!!y_uneval, value),
      shape = factor(`$`(!!y_uneval, is_nd), levels = c(FALSE, TRUE))
    )

  scale_shape_Nd <-
    ggplot2::scale_shape_manual(
      name = "Non-detect?", breaks = c(FALSE, TRUE), labels = c("No", "Yes"),
      values = c(16, 6), drop = FALSE
    )

  Filter(Negate(is.null), list(
    ggplot2::geom_point(mapping = mapping, ...),
    ggplot2::geom_line(mapping = mapping["y"], ...),
    if (isTRUE(scale)) scale_shape_Nd else NULL
  ))
}
