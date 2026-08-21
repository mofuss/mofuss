# Fixture test for finalogs_v2.R. All writes are confined to a temporary folder.

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
test_args <- commandArgs(trailingOnly = TRUE)
script_name <- if (length(test_args)) test_args[[1L]] else "finalogs_v2.R"
script <- file.path(repo_root, "localhost", "scripts", script_name)
stopifnot(file.exists(script))

fixture <- tempfile("finalogs_v2_")
dir.create(file.path(fixture, "LULCC"), recursive = TRUE)
on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)
writeLines(
  c("> proc.time()", "   user  system elapsed", "   1.00    2.00    3.00"),
  file.path(fixture, "maps_animations_v8.Rout")
)
writeLines("root log", file.path(fixture, "log.txt"))
writeLines("lulcc debug", file.path(fixture, "LULCC", "debug.txt"))

old <- setwd(fixture)
on.exit(setwd(old), add = TRUE)
output <- system2(
  file.path(R.home("bin"), "Rscript.exe"),
  c(script, "BaUvsICS=BaU"),
  stdout = TRUE,
  stderr = TRUE
)
status <- attr(output, "status")
if (is.null(status)) status <- 0L
if (status != 0L) stop(paste(output, collapse = "\n"))

stopifnot(
  file.exists("Logs/log_collection_manifest.csv"),
  file.exists("Logs/finalogs_summary.csv"),
  file.exists("Logs/maps_animations_v8_PT.csv")
)
summary <- read.csv("Logs/finalogs_summary.csv")
stopifnot(
  summary$files_discovered == 3L,
  summary$files_copied == 3L,
  summary$timing_tables_written == 1L
)
cat("FINALOGS_FIXTURE_OK:", script_name, "\n")
