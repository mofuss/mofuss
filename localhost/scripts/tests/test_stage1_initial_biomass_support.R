# Native terra integration test for immutable Stage 1 biomass reporting support.
# Run from the repository root with --scratch=E:/MoFuSS_Active/<new-task-folder>.
# Fixtures and generated rasters remain in that explicitly named scratch folder.

args <- commandArgs(trailingOnly = TRUE)
scratch_arg <- args[startsWith(args, "--scratch=")]
stopifnot(length(scratch_arg) == 1L)
scratch <- gsub("\\\\", "/", sub("^--scratch=", "", scratch_arg))
stopifnot(grepl("^[A-Za-z]:/MoFuSS_Active/[^/]+", scratch, ignore.case = TRUE))
if (dir.exists(scratch) && length(list.files(scratch, all.files = TRUE, no.. = TRUE))) {
  stop("Refusing to overwrite nonempty test scratch.")
}
dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
suppressPackageStartupMessages(library(terra))
terraOptions(tempdir = scratch)
stage <- new.env(parent = globalenv())
stage$MOFUSS_CONFIG_ONLY <- TRUE
sys.source(file.path(
  getwd(), "localhost", "scripts", "postprocessing_emissions",
  "1post_raster_fr_generator_diskmemory_v9.R"
), envir = stage)

scenario <- file.path(scratch, "synthetic_bau")
tables <- file.path(scenario, "LULCC", "TempTables")
rasters <- file.path(scenario, "LULCC", "TempRaster")
parameters <- file.path(scenario, "LULCC", "DownloadedDatasets", "SourceDataTest")
for (path in c(tables, rasters, parameters)) dir.create(path, recursive = TRUE)
dir.create(file.path(scenario, "Temp"))
write.csv(data.frame(status = "ready", lulc_version = 1L),
          file.path(scenario, "Temp", "mc_batch_ready.csv"), row.names = FALSE)
write.csv(data.frame(Key = 1L, Country = "Test"), file.path(tables, "Country.csv"), row.names = FALSE)
pars <- c(
  start_year = "2000", end_year = "2011", monte_carlo_runs = "2",
  scenario_ver = "BaU1", byregion = "Country", region2BprocessedCont = "Africa",
  region2BprocessedReg = "Test", region2BprocessedCtry = "Test",
  region2BprocessedCtry_iso = "TST", subcountry = "None", GEE_scale = "1000",
  epsg_pcs = "3395"
)
write.csv(data.frame(Var = names(pars), ParCHR = unname(pars)),
          file.path(parameters, "parameters.csv"), row.names = FALSE)
template <- rast(nrows = 2, ncols = 3, xmin = 0, xmax = 3000,
                 ymin = 0, ymax = 2000, crs = "EPSG:3395")
write_values <- function(path, x) {
  r <- template
  values(r) <- x
  writeRaster(r, path, overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)
}
reference_path <- file.path(rasters, "agb3_c.tif")
write_values(reference_path, c(NA, 0, 10, 20, Inf, 30))
baseline <- list(c(100, 0, 10, 20, 900, NA), c(200, 0, 12, 26, 900, 30))
ending <- list(c(50, 2, 4, 22, 800, 5), c(100, 4, 8, 10, 800, NA))
harvest <- list(c(100, 1, 2, 0, 900, NA), c(200, 3, 4, 0, 900, 10))
for (run in 1:2) {
  path <- file.path(scenario, paste0("debugging_", run))
  dir.create(path)
  for (code in 1:12) {
    write_values(file.path(path, sprintf("Growth%02d.tif", code)), baseline[[run]])
    # Explicit windows start after the previous year's harvest. Make that
    # endpoint differ from Growth so this also guards the baseline timing.
    write_values(file.path(path, sprintf("Growth_less_harv%02d.tif", code)),
                 if (code == 10L) baseline[[run]] else ending[[run]])
    write_values(file.path(path, sprintf("Harvest_tot%02d.tif", code)), harvest[[run]])
  }
}
plan <- stage$build_plan(scenario, stage$parse_periods("2010:2011"), "Out/webmofuss_results_test")
stopifnot(
  all(plan$records$biomass_support_policy == "finite_initial_agb_reference_v1"),
  all(plan$records$biomass_support_reference == normalizePath(reference_path, winslash = "/")),
  all(plan$records$biomass_support_reference_md5 == unname(tools::md5sum(reference_path))),
  sum(plan$records$role == "biomass_support_reference", na.rm = TRUE) == 1L
)
stage$execute_plan(plan)
read_output <- function(name) as.numeric(values(rast(file.path(plan$output_dir, name))))
near <- function(actual, expected) {
  stopifnot(identical(is.na(actual), is.na(expected)))
  stopifnot(all(abs(actual[!is.na(expected)] - expected[!is.na(expected)]) < 1e-6))
}
# NRB is bounded by the matching period harvest; no-harvest cells cannot be NRB.
near(read_output("nrb_10_11_mean.tif"), c(NA, 0, 4, 0, NA, NA))
near(read_output("nrb_10_11_sd.tif"), c(NA, 0, 0, 0, NA, NA))
near(read_output("nrb_10_11_se.tif"), c(NA, 0, 0, 0, NA, NA))
near(read_output("harv_10_11_mean.tif"), c(NA, 4, 6, 0, NA, 20))
near(read_output("harv_10_11_se.tif"), c(NA, 2, 2, 0, NA, NA))
near(read_output("agb_2011_mean.tif"), c(NA, 3, 6, 16, NA, 5))
near(read_output("agb_2011_se.tif"), c(NA, 1, 2, 6, NA, NA))
for (path in plan$records$path[plan$records$record_type == "output"]) {
  stopifnot(all(is.na(as.numeric(values(rast(path)))[c(1, 5)])))
}
# Reject misaligned reference geometry rather than silently resampling it.
support <- stage$initial_biomass_support(reference_path)
wrong <- template
ext(wrong) <- ext(1000, 4000, 0, 2000)
err <- tryCatch(stage$mask_biomass_support(wrong, support), error = identity)
stopifnot(inherits(err, "error"), grepl("geometry differ", conditionMessage(err)))
# The execution must not use a reference changed since plan validation.
write_values(reference_path, c(0, 0, 10, 20, Inf, 30))
err <- tryCatch(stage$execute_plan(plan), error = identity)
stopifnot(inherits(err, "error"), grepl("changed after validation", conditionMessage(err)))
cat("PASS: fixed finite reference support, zeros, multi-run NRB/harvest/AGB identities, provenance, geometry and changed-reference guards.\n")
