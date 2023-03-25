#' Simulate Censored Regression Data
#'
#' Simulates linear regression and [artificially censors][artificial_censor()]
#'   the dependent variable.
#'
#' @note The number of independent variables, `p - 1`, is determined by the
#'   number of supplied regression coefficients, `p`.
#'
#' @param n integer. Number of observations.
#' @param regr vector. Regression coefficients, including intercept.
#' @param sd numeric. The standard deviation of the 'noise' added to `y`.
#' @inheritParams artificial_censor
#'
#' @export
simulate_censreg_data <- function(n, regr, sd, quantiles) {
  p <- length(regr) - 1

  x <- do.call(cbind, Map(stats::rnorm, n = rep(n, p)))
  colnames(x) <- sprintf("x%02i", seq_len(ncol(x)))

  y <- drop(cbind(1, x) %*% regr) + stats::rnorm(n, sd = sd)

  tibble::tibble(y = artificial_censor(y, quantiles), as.data.frame(x))
}
