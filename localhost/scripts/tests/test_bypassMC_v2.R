# Synthetic integration test for the versioned bypassMC scripts.
# All destructive behavior is isolated in a temporary directory.

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
test_args <- commandArgs(trailingOnly = TRUE)
script_name <- if (length(test_args)) test_args[[1L]] else "bypassMC_v8.R"
script <- file.path(repo_root, "localhost", "scripts", script_name)
rnorm_script <- file.path(repo_root, "localhost", "scripts", "rnorm_v8.R")
stopifnot(file.exists(script))
stopifnot(file.exists(rnorm_script))

fixture <- tempfile("bypassMC_test_")
bau <- file.path(fixture, "rwa_bau1_capped")
ccts <- file.path(fixture, "rwa_ics3_capped")
dir.create(bau, recursive = TRUE)
dir.create(ccts, recursive = TRUE)
on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

write_parameters <- function(root, scenario_ver) {
  path <- file.path(
    root, "LULCC", "DownloadedDatasets", "SourceDataGlobal", "parameters.csv"
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(
    data.frame(
      Var = c(
        "byregion", "region2BprocessedCtry_iso", "scenario_ver",
        "start_year", "end_year", "monte_carlo_runs",
        "uncapped_regrowth", "GEE_scale"
      ),
      ParCHR = c("Country", "RWA", scenario_ver, 2000, 2030, 30, 0, 1000),
      stringsAsFactors = FALSE
    ),
    path,
    row.names = FALSE
  )
}

write_parameters(bau, "BaU1_v2")
write_parameters(ccts, "ICS3_v2")

static_rel <- c(
  "LULCC/TempTables/growth_parameters1.csv",
  "LULCC/TempRaster/LULCt1_c.tif",
  "LULCC/TempRaster/agb3_c.tif",
  "LULCC/TempRaster/Mask_c.tif"
)
for (relative in static_rel) {
  bau_path <- file.path(bau, relative)
  ccts_path <- file.path(ccts, relative)
  dir.create(dirname(bau_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(ccts_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(paste("synthetic-static-input", relative), bau_path)
  stopifnot(file.copy(bau_path, ccts_path, overwrite = TRUE))
}

dir.create(file.path(bau, "Temp"), recursive = TRUE)
run_ids <- 1:30
wide <- data.frame(run = run_ids, class_1 = run_ids / 10, class_2 = run_ids / 20)
lookup <- data.frame(run = run_ids, value = run_ids)
wide_names <- c("i_st_all.csv", "k_all.csv", "rmax_all.csv")
lookup_names <- c(
  "Harvest_pixels_V.csv", "Harvest_pixels_W.csv",
  "Prune_factor_V.csv", "Prune_factor_W.csv"
)
for (name in wide_names) {
  write.csv(wide, file.path(bau, "Temp", name), row.names = FALSE)
}
for (name in lookup_names) {
  write.csv(lookup, file.path(bau, "Temp", name), row.names = FALSE)
}
write.csv(
  data.frame(key = 1:2, label = c("forest", "woodland")),
  file.path(bau, "Temp", "LULC_Categories1.csv"),
  row.names = FALSE
)

batch_files <- c(wide_names, lookup_names, "LULC_Categories1.csv")

# Evaluate the actual publisher function without sourcing the destructive
# top-level rnorm script.
rnorm_expressions <- parse(file = rnorm_script)
definition <- function(name) {
  hits <- Filter(
    function(expr) {
      is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
        identical(expr[[2L]], as.name(name))
    },
    as.list(rnorm_expressions)
  )
  stopifnot(length(hits) == 1L)
  eval(hits[[1L]], envir = .GlobalEnv)
}
definition("mc_batch_ready_filename")
definition("write_mc_batch_ready")

write_batch_ready <- function() {
  old_wd <- setwd(bau)
  on.exit(setwd(old_wd), add = TRUE)
  manifest <- write_mc_batch_ready(
    temp_dir = "Temp", mc_runs = 30L,
    start_year = 2000L, end_year = 2030L,
    scenario_ver = "BaU1_v2", byregion = "Country", geography = "RWA",
    uncapped_regrowth = 0L, luc_version = 1L, agb_version = 3L
  )
  as.character(manifest$batch_id[[1L]])
}

# Only half of BAU dynamics are complete. MC tables are nevertheless ready.
for (id in 1:15) {
  run_dir <- file.path(bau, paste0("debugging_", id))
  dir.create(run_dir)
  file.create(file.path(run_dir, "Growth_less_harv31.tif"))
}

# Stale CCTS content must be replaced only after complete preflight/staging.
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
  "LUCmap_v=1", "AGBmap_v=3", "PatcherBypassed=1",
  paste0("CurrentDir=", normalizePath(ccts, winslash = "/", mustWork = TRUE))
)
rscript_name <- if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
run_bypass <- function() {
  output <- system2(
    file.path(R.home("bin"), rscript_name), args,
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  list(output = output, status = status)
}

# Existing tables without a current-batch readiness manifest must never be used.
missing_ready <- run_bypass()
stopifnot(
  missing_ready$status != 0L,
  any(grepl("no atomic current-batch readiness manifest", missing_ready$output)),
  file.exists(file.path(ccts, "Temp", "sentinel.txt")),
  file.exists(file.path(ccts, "Out", "sentinel.txt")),
  file.exists(file.path(ccts, "debugging_1", "sentinel.txt"))
)

batch_id <- write_batch_ready()

# A table changed after publication must be rejected without touching CCTS.
stale_k <- read.csv(file.path(bau, "Temp", "k_all.csv"), check.names = FALSE)
stale_k[[2L]][[2L]] <- stale_k[[2L]][[2L]] + 1
write.csv(stale_k, file.path(bau, "Temp", "k_all.csv"), row.names = FALSE)
changed_after_ready <- run_bypass()
stopifnot(
  changed_after_ready$status != 0L,
  any(grepl("changed after the current-batch manifest", changed_after_ready$output)),
  file.exists(file.path(ccts, "Temp", "sentinel.txt")),
  file.exists(file.path(ccts, "Out", "sentinel.txt")),
  file.exists(file.path(ccts, "debugging_1", "sentinel.txt"))
)

write.csv(wide, file.path(bau, "Temp", "k_all.csv"), row.names = FALSE)
unlink(file.path(bau, "Temp", "mc_batch_ready.csv"))
batch_id <- write_batch_ready()
success <- run_bypass()
if (success$status != 0L) stop(paste(success$output, collapse = "\n"))

mc_files <- batch_files
source_hash <- unname(tools::md5sum(file.path(bau, "Temp", mc_files)))
copied_hash <- unname(tools::md5sum(file.path(ccts, "Temp", mc_files)))
stopifnot(identical(source_hash, copied_hash))
stopifnot(!file.exists(file.path(ccts, "Temp", "sentinel.txt")))
stopifnot(length(list.files(file.path(ccts, "Out"), all.files = TRUE, no.. = TRUE)) == 0L)
stopifnot(length(list.files(file.path(ccts, "HTML_animation"), all.files = TRUE, no.. = TRUE)) == 0L)
stopifnot(!dir.exists(file.path(ccts, ".bypassMC.lock")))
stopifnot(file.exists(file.path(ccts, "Temp", "mc_bypass_manifest.csv")))
stopifnot(file.exists(file.path(ccts, "Temp", "mc_bypass_file_manifest.csv")))
stopifnot(file.exists(file.path(ccts, "Temp", "mc_batch_ready.csv")))

runs <- list.dirs(ccts, recursive = FALSE, full.names = FALSE)
created_ids <- sort(as.integer(sub(
  "^debugging_", "", grep("^debugging_[0-9]+$", runs, value = TRUE)
)))
stopifnot(identical(created_ids, 1:30))

manifest <- read.csv(
  file.path(ccts, "Temp", "mc_bypass_manifest.csv"),
  stringsAsFactors = FALSE
)
stopifnot(
  nrow(manifest) == 1L,
  manifest$status == "complete",
  manifest$mode == "reuse_BAU_MC_tables",
  identical(manifest$bau_mc_batch_id, batch_id),
  manifest$monte_carlo_runs == 30L,
  manifest$uncapped_regrowth == 0L,
  identical(manifest$bau_dynamics_complete, FALSE),
  manifest$bau_completed_run_count == 15L,
  identical(manifest$patcher_bypassed, TRUE),
  identical(manifest$patcher_rng_paired, FALSE)
)

cat("BYPASS_MC_INTEGRATION_TEST_OK:", script_name, "\n")
