#' Impute Generic
#'
#' Transform data from some class to a simplified numeric to be used in place.
#'
#' @param x object used to choose a method.
#' @param ... extra arguments used for extensibility.
#'
#' @return Regardless of input class, should always return same-length numeric.
#'
#' @export
impute <- function(x, ...) {
  UseMethod("impute")
}


#' Impute Non-Detect Data
#'
#' @template Nd-method
#' @param method coercible to function. See **Method** section.
#' @param delog logical. When `TRUE`, values are exponentiated before
#'   imputation and log transformed afterwards.
#' @param ... extra arguments passed to `method`.
#'
#' @param value,is_nd numeric, logical. Used in pre-defined methods.
#'
#' @section Method:
#' An imputation method can be pre-defined and identified via a character,
#' such as
#'   - `"DL2"`, half detection limit,
#'   - `"ROS"`, regression on order statistics via `NADA` package.
#'
#' Other types are passed to [rlang::as_function()] and is expected to return
#' a numeric vector given arguments `value` and `is_nd`.
#'
#' @export
impute.Nd <- function(x, method = "DL2", delog = FALSE, ...) {
  if (is.character(method)) {
    method <-
      switch(
        match.arg(method, choices = c("DL2", "ROS")),
        "DL2" = ~ impute_by_factor(value = .x, is_nd = .y),
        "ROS" = ~ impute_by_ros(value = .x, is_nd = .y)
      )
  }

  method <- rlang::as_function(method)

  if (delog) return(log(method(exp(x$value), x$is_nd, ...)))

  if (any(x$value <= 0, na.rm = TRUE)) {
    stop("Found non-positive values, imputation not valid")
  }

  return(method(x$value, x$is_nd, ...))
}


#' @describeIn impute.Nd
#'   Replace each non-detect with the product of some factor (`impute_fct`)
#'   and the detection limit.
#'
#' @param impute_fct numeric. Multiplicative factor, defaults to 0.5.
#'
#' @export
impute_by_factor <- function(value, is_nd, impute_fct = 0.5) {
  stopifnot(
    "Length of 'impute_fct' should be 1" = length(impute_fct) == 1,
    "Expected numeric for 'impute_fct'" = is.numeric(impute_fct)
  )

  ifelse(is_nd, value * impute_fct, value)
}


#' @describeIn impute.Nd
#'   Apply Regression on Order Statistics (ROS) method proposed by Lee and
#'   Helsel in the `NADA` package.
#'
#' @export
impute_by_ros <- function(value, is_nd) {
  requireNamespace("NADA")

  ros_obj <- NADA::ros(value, is_nd)
  ros_obj$modeled[order(order(value))]
}
