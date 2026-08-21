# Verify that the V7 log collector runs without the retired spatial packages
# and recognizes the version-locked V7 Rout filenames.

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
script <- file.path(repo_root, "localhost", "scripts", "finalogs_v7.R")
stopifnot(file.exists(script))

fixture <- tempfile("finalogs_v7_bundle_")
dir.create(file.path(fixture, "LULCC"), recursive = TRUE)
dir.create(file.path(fixture, "Logs"), recursive = TRUE)
dir.create(file.path(fixture, "Out"), recursive = TRUE)
on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

writeLines(
  c("> proc.time()", "   user  system elapsed", "   1.00    2.00    3.00"),
  file.path(fixture, "maps_animations_v7.Rout")
)

old <- setwd(fixture)
on.exit(setwd(old), add = TRUE)
output <- system2(
  file.path(R.home("bin"), "Rscript.exe"),
  c(script, "BaUvsICS='BaU'"),
  stdout = TRUE,
  stderr = TRUE
)
status <- attr(output, "status")
if (is.null(status)) status <- 0L
if (status != 0L) stop(paste(output, collapse = "\n"))

stopifnot(
  file.exists("Logs/maps_animations_v7.Rout"),
  file.exists("Logs/maps_animations_v7_PT.csv")
)
cat("FINALOGS_V7_BUNDLE_FIXTURE_OK\n")
