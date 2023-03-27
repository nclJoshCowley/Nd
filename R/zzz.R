.onLoad <- function(...) {
  s3_register("tibble::type_sum", "Nd")
  s3_register("ggplot2::autoplot", "Nd")
}
