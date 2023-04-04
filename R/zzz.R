# No need to test the externally supplied method in register-s3.R
# nocov start

.onLoad <- function(...) {
  s3_register("tibble::type_sum", "Nd")
  s3_register("ggplot2::autoplot", "Nd")
}

# nocov end
