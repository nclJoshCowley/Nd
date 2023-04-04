#' Check `Nd` Class
#'
#' @template Nd-method
#'
#' @export
is.Nd <- function(x) {
  inherits(x, "Nd")
}


#' Implementation of [pillar::type_sum]
#'
#' Changes type summary (shown in tibbles) from `Surv` to `Nd`.
#'
#' @template Nd-method
#'
#' @export
type_sum.Nd <- function(x) {
  "Nd"
}


#' Compute Degree of Censoring
#'
#' @template Nd-method
#' @param na.rm logical. NA observations ignored when `TRUE`.
#'
#' @export
cens_ratio <- function(x, na.rm = FALSE) {
  mean(x$is_nd, na.rm = na.rm)
}
