# Verify both version-locked dormant map-bypass scripts in temporary fixtures.

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
scripts_dir <- file.path(repo_root, "localhost", "scripts")

for (script_name in c(
  "bypass_maps_animations_v7.R",
  "bypass_maps_animations_v8.R"
)) {
  script <- file.path(scripts_dir, script_name)
  stopifnot(file.exists(script))
  fixture <- tempfile(paste0(tools::file_path_sans_ext(script_name), "_"))
  dir.create(file.path(fixture, "Out"), recursive = TRUE)
  old <- setwd(fixture)
  output <- system2(
    file.path(R.home("bin"), "Rscript.exe"),
    script,
    stdout = TRUE,
    stderr = TRUE
  )
  setwd(old)
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L) stop(paste(output, collapse = "\n"))
  stopifnot(file.exists(file.path(
    fixture, "Out", "Maps and animations turned off by user.csv"
  )))
  unlink(fixture, recursive = TRUE, force = TRUE)
}

cat("BYPASS_MAPS_VERSIONED_FIXTURE_OK\n")
