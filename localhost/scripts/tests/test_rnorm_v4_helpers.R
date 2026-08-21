# Unit tests for the Monte Carlo helper functions without executing rnorm_v4's
# destructive scenario initialization.

test_args <- commandArgs(trailingOnly = TRUE)
script_name <- if (length(test_args)) test_args[[1L]] else "rnorm_v4.R"
script <- file.path(
  normalizePath(getwd(), winslash = "/", mustWork = TRUE),
  "localhost", "scripts", script_name
)
expressions <- parse(file = script)
wanted <- c(
  "draw_truncated_or_fixed",
  "clamp_pixel_request",
  "draw_harvest_pixels"
)
for (expression in expressions) {
  if (
    is.call(expression) && identical(expression[[1L]], as.name("<-")) &&
      is.name(expression[[2L]]) && as.character(expression[[2L]]) %in% wanted
  ) {
    eval(expression, envir = .GlobalEnv)
  }
}
stopifnot(all(vapply(
  wanted,
  exists,
  logical(1),
  envir = .GlobalEnv,
  inherits = FALSE
)))

fixed <- draw_truncated_or_fixed(5L, 12, 0, lower = 0, upper = 20)
stopifnot(identical(fixed, rep(12, 5L)))
stopifnot(clamp_pixel_request(1000, 25, "test") == 25)
stopifnot(clamp_pixel_request(-4, 25, "test") == 0)

fixed_pixels <- draw_harvest_pixels(10, 25, 0, 6L, "test")
stopifnot(identical(fixed_pixels, rep(10, 6L)))
set.seed(42)
random_pixels <- draw_harvest_pixels(20, 25, 0.5, 30L, "test")
stopifnot(
  random_pixels[[1L]] == 20,
  all(random_pixels >= 0),
  all(random_pixels <= 25)
)
cat("RNORM_HELPERS_OK:", script_name, "\n")
