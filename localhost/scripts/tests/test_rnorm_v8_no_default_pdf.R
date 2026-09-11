# Regression guard: CTrees runs must not send forest histograms to R's
# implicit Rplots.pdf device.

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
stopifnot(length(script_argument) == 1L)
test_script <- sub("^--file=", "", script_argument)
repository_root <- normalizePath(
  file.path(dirname(test_script), "..", "..", ".."),
  winslash = "/",
  mustWork = TRUE
)
script_path <- file.path(
  repository_root,
  "localhost",
  "scripts",
  "rnorm_v8.R"
)
expressions <- parse(file = script_path)
script_text <- paste(readLines(script_path, warn = FALSE), collapse = "\n")

guarded_histogram_patterns <- c(
  "if \\(CTrees == 0\\) \\{[[:space:]]*hist\\(\\(r1\\*100\\)",
  "if \\(CTrees == 0\\) \\{[[:space:]]*hist\\(k1,",
  "if \\(CTrees == 0\\) \\{[[:space:]]*hist\\(st1,"
)
stopifnot(all(vapply(
  guarded_histogram_patterns,
  grepl,
  logical(1),
  x = script_text,
  perl = TRUE
)))

cat("RNORM_V8_NO_DEFAULT_PDF_OK\n")
