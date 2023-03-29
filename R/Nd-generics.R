#' @noRd
#' @export
format.Nd <- function(x, ...) {
  format(as.character.Nd(x), ...)
}


#' @noRd
#' @export
as.character.Nd <- function(x, ...) {
  if (length(x) == 0) return("Nd(0)")

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
  x_matrix <- unname(unclass(x))
  switch(
    match.arg(name, c("value", "is_nd")),
    "value" = x_matrix[, 1],
    "is_nd" = !(as.logical(x_matrix[, 2]))
  )
}


#' @noRd
#' @export
`$<-.Nd` <- function(x, name, value) {
  name <- match.arg(name, c("value", "is_nd"))

  if (name == "value") {
    return(validate_Nd(new_Nd(value = value, is_nd = x$is_nd), quiet = FALSE))
  }

  return(validate_Nd(new_Nd(value = x$value, is_nd = value), quiet = FALSE))
}


#' @noRd
#' @export
`[.Nd` <- function(x, i, ...) {
  extra_args <- rlang::list2(...)
  if ("drop" %in% names(extra_args)) {
    warning("Ignoring 'drop' argument", call. = FALSE)
    extra_args$drop <- NULL
  }

  if (length(extra_args) > 0) {
    stop("Only one index allowed in `[.Nd`", call. = FALSE)
  }

  xattr <- attributes(x)[setdiff(names(attributes(x)), c("dim", "dimnames"))]
  x <- unclass(x)[i, , drop = FALSE]
  attributes(x) <- c(attributes(x), xattr)
  return(x)
}


#' @noRd
#' @export
`[<-.Nd` <- function(x, i, ..., value) {
  if (!rlang::is_empty(rlang::list2(...))) {
    stop("Only one index allowed in `[<-.Nd`", call. = FALSE)
  }

  if (!is.Nd(value)) {
    cls <- paste0(class(value), collapse = "/")
    stop(sprintf("Replacement must be <Nd> not <%s>", cls), call. = FALSE)
  }

  x_matrix <- unclass(x)

  x_matrix[i, 1] <- value$value
  x_matrix[i, 2] <- value$is_nd

  return(validate_Nd(new_Nd(x_matrix[, 1], x_matrix[, 2]), quiet = FALSE))
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
is.finite.Nd <- function(x) {
  is.finite(x$value)
}
