#' Artificial Censoring
#'
#' Converts bare numeric into a left-censored quantity with less information.
#'
#' Only one of the arguments `quantiles` and `detlims` should be supplied.
#'
#' @param x numeric. Original data to be censored.
#' @param quantiles numeric. Censor by quantile(s) between 0 and 1.
#' @param detlims numeric. Censor at chosen value(s).
#'
#' @export
artificial_censor <- function(x, quantiles, detlims) {
  if (isFALSE(xor(missing(quantiles), missing(detlims)))) {
    stop("Expected only one of 'quantiles' or 'detlims'", call. = FALSE)
  }

  if (missing(detlims)) {
    stopifnot("'quantiles' outside [0, 1]" = (0 <= quantiles & quantiles <= 1))
    detlims <- stats::quantile(x, quantiles)
  }

  cut_points <- cut(x, unique(c(-Inf, detlims)))

  x_is_nd <- !is.na(cut_points)
  x_value <- ifelse(x_is_nd, detlims[cut_points], x)

  return(new_Nd(value = x_value, is_nd = x_is_nd))
}
