# Integration test for bypassMC_v2.R.
# Uses the completed Kenya BAU as a read-only fixture source and performs all
# destructive behavior in a temporary sandbox.

stopifnot(.Platform$OS.type == "windows")

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
test_args <- commandArgs(trailingOnly = TRUE)
script_name <- if (length(test_args)) test_args[[1L]] else "bypassMC_v2.R"
script <- file.path(repo_root, "localhost", "scripts", script_name)
real_bau <- "D:/ken_1km_bau1_2030_v3_ng"
real_ccts <- "D:/ken_1km_ics3_2030_v3_ng"
stopifnot(file.exists(script), dir.exists(real_bau), dir.exists(real_ccts))

fixture <- tempfile("bypassMC_v2_test_")
bau <- file.path(fixture, "bau")
ccts <- file.path(fixture, "ccts")
dir.create(bau, recursive = TRUE)
dir.create(ccts, recursive = TRUE)
on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

copy_one <- function(source, destination) {
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(source, destination, overwrite = TRUE, copy.date = TRUE)) {
    stop("Failed to copy fixture: ", source)
  }
}

parameter_rel <- "LULCC/DownloadedDatasets/SourceDataGlobal/parameters.csv"
copy_one(file.path(real_bau, parameter_rel), file.path(bau, parameter_rel))
copy_one(file.path(real_ccts, parameter_rel), file.path(ccts, parameter_rel))

static_rel <- c(
  "LULCC/TempTables/growth_parameters1.csv",
  "LULCC/TempRaster/LULCt1_c.tif",
  "LULCC/TempRaster/agb3_c.tif",
  "LULCC/TempRaster/Mask_c.tif"
)
for (relative in static_rel) {
  copy_one(file.path(real_bau, relative), file.path(bau, relative))
  copy_one(file.path(real_bau, relative), file.path(ccts, relative))
}

mc_files <- c(
  "i_st_all.csv", "k_all.csv", "rmax_all.csv",
  "Harvest_pixels_V.csv", "Harvest_pixels_W.csv",
  "Prune_factor_V.csv", "Prune_factor_W.csv",
  "LULC_Categories1.csv"
)
for (name in mc_files) {
  copy_one(file.path(real_bau, "Temp", name), file.path(bau, "Temp", name))
}

for (id in 1:30) {
  run_dir <- file.path(bau, paste0("debugging_", id))
  dir.create(run_dir)
  file.create(file.path(run_dir, "Growth_less_harv31.tif"))
}

# Stale CCTS content must be replaced only after the complete preflight/stage.
dir.create(file.path(ccts, "Temp"), recursive = TRUE)
writeLines("stale", file.path(ccts, "Temp", "sentinel.txt"))
dir.create(file.path(ccts, "Out"), recursive = TRUE)
writeLines("stale", file.path(ccts, "Out", "sentinel.txt"))
dir.create(file.path(ccts, "HTML_animation"), recursive = TRUE)
writeLines("stale", file.path(ccts, "HTML_animation", "sentinel.txt"))
dir.create(file.path(ccts, "debugging_1"), recursive = TRUE)
writeLines("stale", file.path(ccts, "debugging_1", "sentinel.txt"))

args <- c(
  script,
  "RerunMC=0", "MC=30", "IT=2000", "STdyn=30",
  "LUCmap_v=1", "AGBmap_v=3",
  paste0("CurrentDir=", normalizePath(ccts, winslash = "/", mustWork = TRUE))
)
output <- system2(file.path(R.home("bin"), "Rscript.exe"), args, stdout = TRUE, stderr = TRUE)
status <- attr(output, "status")
if (is.null(status)) status <- 0L
if (status != 0L) stop(paste(output, collapse = "\n"))

source_hash <- unname(tools::md5sum(file.path(bau, "Temp", mc_files)))
copied_hash <- unname(tools::md5sum(file.path(ccts, "Temp", mc_files)))
stopifnot(identical(source_hash, copied_hash))
stopifnot(!file.exists(file.path(ccts, "Temp", "sentinel.txt")))
stopifnot(length(list.files(file.path(ccts, "Out"), all.files = TRUE, no.. = TRUE)) == 0L)
stopifnot(length(list.files(file.path(ccts, "HTML_animation"), all.files = TRUE, no.. = TRUE)) == 0L)
stopifnot(!dir.exists(file.path(ccts, ".bypassMC.lock")))
stopifnot(file.exists(file.path(ccts, "Temp", "mc_bypass_manifest.csv")))
stopifnot(file.exists(file.path(ccts, "Temp", "mc_bypass_file_manifest.csv")))

runs <- list.dirs(ccts, recursive = FALSE, full.names = FALSE)
run_ids <- sort(as.integer(sub("^debugging_", "", grep("^debugging_[0-9]+$", runs, value = TRUE))))
stopifnot(identical(run_ids, 1:30))

manifest <- read.csv(file.path(ccts, "Temp", "mc_bypass_manifest.csv"), stringsAsFactors = FALSE)
stopifnot(
  nrow(manifest) == 1L,
  manifest$status == "complete",
  manifest$mode == "reuse_BAU_MC_tables",
  manifest$monte_carlo_runs == 30L,
  manifest$uncapped_regrowth == 0L,
  identical(manifest$patcher_rng_paired, FALSE)
)

cat("BYPASS_MC_INTEGRATION_TEST_OK:", script_name, "\n")
