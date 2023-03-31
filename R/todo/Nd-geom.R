#' Add Non-Detect Data to ggplot
#'
#' Unconventional ggplot extension, adds [`geom_point`][ggplot2::geom_point()]
#'   and [`geom_line`][ggplot2::geom_path] representing [`Nd`] data.
#'
#' @param mapping aes. Should map `y` to the analyte data to be plotted.
#' @param ... Extra arguments passed to relevant Geom..
#'
#' @name geom_Nd
NULL


#' @rdname geom_Nd
#' @export
geom_Nd <- function(mapping, point_args = list(), line_args = list()) {
  # TODO: Rework using vignette("extending-ggplot2", "ggplot2")
  mapping <- convert_Nd_in_mapping(mapping, "y", c("y", "shapae"))

  list(
    # do.call(ggplot2::geom_point, c(list(mapping), as.list(point_args))),
    do.call(ggplot2::geom_line, c(list(mapping), as.list(line_args)))
  )
}


#' @rdname geom_Nd
#' @export
geom_line_Nd <- function(mapping, ...) {
  # TODO: Rework using vignette("extending-ggplot2", "ggplot2")
  mapping <- convert_Nd_in_mapping(mapping, "y", c("y", "shape"))
  ggplot2::geom_line(mapping, ...)
}


#' @rdname geom_Nd
#' @export
scale_shape_Nd <- function(...) {
  default_args <- list(
    name = "Non-detect?",
    breaks = c(FALSE, TRUE),
    labels = c("No", "Yes"),
    values = c(16, 1),
    drop = FALSE
  )

  args <- utils::modifyList(default_args, rlang::list2(...))

  do.call(ggplot2::scale_shape_manual, args)
}


#' @keywords internal
#' @noRd
convert_Nd_in_mapping <- function(mapping, aes_from, aes_to) {
  if (isFALSE(aes_from %in% names(mapping))) {
    stop(sprintf("Expected '%s' aesthetic", aes_from))
  }

  stopifnot("Require two aesthetics for Nd data" = length(aes_to) == 2)

  existing_aes <- intersect(setdiff(aes_to, aes_from), names(mapping))
  if (length(existing_aes) > 0) {
    warning(sprintf("Replacing '%s' aesthetic", existing_aes), call. = FALSE)
  }

  base_expr <- rlang::quo_get_expr(mapping[[aes_from]])
  base_env <- rlang::quo_get_env(mapping[[aes_from]])

  mapping[[aes_to[1]]] <-
    rlang::as_quosure(rlang::expr(`$`(!!base_expr, "value")), base_env)

  mapping[[aes_to[2]]] <-
    rlang::as_quosure(rlang::expr(`$`(!!base_expr, "is_nd")), base_env)

  return(mapping)
}
