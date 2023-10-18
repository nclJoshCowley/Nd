#' Non-Detect Data
#'
#' S3 class that represents left-censored data commonly referred to as
#'   **non-detect** data.
#'
#' @details The object is made up of a numerical component, `value`, that
#'   represents an observed concentration or level of detection depending on
#'   whether the logical component, `is_nd`, is `TRUE` or `FALSE` respectively.
#'
#' @section Survival:
#'   This class is an extension to the [survival::Surv] class; a special case
#'   where left-censoring is implicitly assumed.
#'
#' @param x object. Input to be converted to `Nd`.
#' @param is_nd logical. Required when `x` is numeric.
#' @inheritParams rlang::args_dots_empty
#'
#' @return An object of class `c("Nd", "Surv")`.
#'
#' @examples
#'   Nd(c(1.1, 0.5, 0.25), is_nd = c(FALSE, TRUE, TRUE))
#'   Nd(c("1.1", "<0.5", "ND<0.25", "1.312454"))
#'
#' @import survival
#' @export
Nd <- function(x, ...) UseMethod("Nd")


#' @rdname Nd
#' @export
Nd.default <- function(x, ...) {
  # Length-0 vector
  if (missing(x)) {
    return(new_Nd(value = double(), is_nd = logical()))
  }

  stop("Can't coerce <", class(x)[[1]], "> into Nd object")
}


#' @rdname Nd
#' @export
Nd.Nd <- function(x, ...) {
  return(validate_Nd(x, quiet = FALSE))
}


#' @rdname Nd
#' @export
Nd.Surv <- function(x, ...) {
  stopifnot("Surv object must be left-censored" = "left" %in% attr(x, "type"))

  class(x) <- c("Nd", class(x))

  return(validate_Nd(x, quiet = FALSE))
}


#' @rdname Nd
#' @export
Nd.numeric <- function(x, is_nd, ...) {
  if (missing(is_nd)) is_nd <- ifelse(is.na(x), NA, rep(FALSE, length(x)))

  if (length(x) != length(is_nd)) {
    stop("Input and 'is_nd' not equal lengths.", call. = FALSE)
  }

  return(validate_Nd(new_Nd(value = x, is_nd = is_nd), quiet = FALSE))
}


#' @rdname Nd
#' @param na character. Vector of values to be interpreted as NA.
#' @export
Nd.character <- function(x, ..., na = c("", "NA")) {
  # Parse using pattern
  regex_nd <- getOption("regex_nd", default = "^(ND)*[<]+[= ]*")
  value <- readr::parse_double(gsub(regex_nd, "", x), na = na)
  is_nd <- ifelse(is.na(value), NA, grepl(regex_nd, x))

  return(validate_Nd(new_Nd(value = value, is_nd = is_nd), quiet = FALSE))
}


#' @keywords internal
#' @noRd
new_Nd <- function(value, is_nd) {
  structure(
    cbind(time = value, status = !is_nd),
    class = c("Nd", "Surv"),
    type = "left"
  )
}


#' @keywords internal
#' @param quiet logical. When `TRUE`, return value is [invisible].
#' @noRd
validate_Nd <- function(x, quiet = TRUE) {
  stopifnot(
    "Invalid Nd, length mismatch" = all.equal(length(x$value), length(x$is_nd)),
    "Invalid Nd, NAs mismatch" = all.equal(is.na(x$value), is.na(x$is_nd)),
    "Invalid Nd, non-finite values" = !(any(is.infinite(x$value)))
  )

  if (isFALSE(quiet)) return(x) else invisible(x)
}
