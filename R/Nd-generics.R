#' @noRd
#' @export
format.Nd <- function(x, ...) {
  format(as.character.Nd(x), ...)
}


#' @noRd
#' @export
as.character.Nd <- function(x, ...) {
  value_character <- format(x$value, trim = TRUE, drop0trailing = TRUE)

  out <- paste0(ifelse(x$is_nd, "ND<", ""), value_character)

  out[is.na(x)] <- NA_character_

  return(out)
}


#' @noRd
#' @export
print.Nd <- function(x, quote = FALSE, ...) {
  invisible(print(as.character.Nd(x), quote = quote, ...))
}


#' @noRd
#' @export
`$.Nd` <- function(x, name) {
  switch(
    match.arg(name, c("value", "is_nd")),
    "value" = unname(unclass(x[, 1])),
    "is_nd" = !(as.logical(unname(unclass(x[, 2]))))
  )
}


#' @noRd
#' @export
`$<-.Nd` <- function(x, name, value) {
  name <- match.arg(name, c("value", "is_nd"))

  if (name == "value") {
    return(validate_Nd(new_Nd(value = value, is_nd = x$is_nd)))
  }

  return(validate_Nd(new_Nd(value = x$value, is_nd = value)))
}


#' @noRd
#' @export
log.Nd <- function(x, base = exp(1)) {
  if (any(x$value[!is.na(x)] < 0)) {
    stop("All values must be strictly positive", call. = FALSE)
  }

  x$value <- log(x$value, base = base)
  return(x)
}


#' @noRd
#' @export
exp.Nd <- function(x) {
  x$value <- exp(x$value)
  return(x)
}


#' @noRd
#' @export
mean.Nd <- function(x, ...) {
  stop("Mean of censored data is ambigous.")
}


#' @noRd
#' @export
is.na.Nd <- function(x) {
  as.vector(rowSums(is.na(unclass(x))) > 0)
}


#' @noRd
#' @export
is.finite.Nd <- function(x) {
  is.finite(x$value)
}
