# End-to-end Stage 1 regression for LUC-neutral NRB and baseline timing.
# Run from the repository root; all generated files stay in designated scratch.
suppressPackageStartupMessages(library(terra))
scratch <- Sys.getenv("MOFUSS_TEST_SCRATCH", "E:/MoFuSS_Active/MDG_NRB_attribution_fix_2026-10-07/emissions")
stopifnot(grepl("^[A-Za-z]:[/\\\\]MoFuSS_Active[/\\\\].+", scratch))
dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
fixture <- tempfile("stage1_luc_", tmpdir = scratch)
dir.create(fixture)
terraOptions(tempdir = fixture)
stage <- new.env(parent = globalenv())
stage$MOFUSS_CONFIG_ONLY <- TRUE
sys.source(file.path(getwd(), "localhost/scripts/postprocessing_emissions",
                     "1post_raster_fr_generator_diskmemory_v9.R"), envir = stage)
scenario <- file.path(fixture, "dynamic")
tables <- file.path(scenario, "LULCC/TempTables")
rasters <- file.path(scenario, "LULCC/TempRaster")
parameters <- file.path(scenario, "LULCC/DownloadedDatasets/SourceDataTest")
debug <- file.path(scenario, "debugging_1")
for (path in c(tables, rasters, parameters, debug, file.path(scenario, "Temp"))) {
  dir.create(path, recursive = TRUE)
}
write.csv(data.frame(Key = 1L, Country = "Test"), file.path(tables, "Country.csv"), row.names = FALSE)
pars <- c(start_year = "2000", end_year = "2011", monte_carlo_runs = "1",
          scenario_ver = "BaU1", byregion = "Country", region2BprocessedCont = "Africa",
          region2BprocessedReg = "Test", region2BprocessedCtry = "Test",
          region2BprocessedCtry_iso = "TST", subcountry = "None", GEE_scale = "1000", epsg_pcs = "3395")
write.csv(data.frame(Var = names(pars), ParCHR = unname(pars)),
          file.path(parameters, "parameters.csv"), row.names = FALSE)
write.csv(data.frame(status = "ready", lulc_version = 3L),
          file.path(scenario, "Temp/mc_batch_ready.csv"), row.names = FALSE)
# A normal copied bundle contains two legacy models and a corrected v14.
# The saved wizard defaults still say static; the completed runtime batch says
# annual. Runtime evidence must select the channel and the unique contract.
wizard <- '<functor><property key="wizard.constant.input" value="Int_constant_4"/><inputport name="constant">1</inputport></functor>'
writeLines(paste0('<model><property key="mofuss.nrb.attribution.contract" value="woodfuel_attributed_signed_balance_v1"/>', wizard, '</model>'),
           file.path(scenario, "10_dyn_fixture.egoml"))
for (name in c("10_dyn_legacy.egoml", "10_dyn_legacy_linux.egoml")) {
  writeLines(paste0('<model>', wizard, '</model>'), file.path(scenario, name))
}
template <- rast(nrows = 2, ncols = 3, xmin = 0, xmax = 3000, ymin = 0, ymax = 2000, crs = "EPSG:3395")
write_values <- function(path, x) writeRaster(setValues(template, x), path, overwrite = TRUE, datatype = "FLT8S")
write_values(file.path(rasters, "agb3_c.tif"), c(100, 100, 100, 100, NA, 0))
for (step in 1:12) {
  # 1: pure clearing, zero harvest. 2: clearing plus 15 harvest.
  # 3: growth offsets harvest. 4: prior growth credit must cancel at the period
  # boundary; including START-year growth differs from a preharvest baseline.
  # Cell 6 is a zero-reference cell with a temporary missing growth domain and
  # zero harvest at START. Its valid signed balance survives that gap.
  growth <- if (step == 12L) c(20, 20, 115, 90, 0, 10) else c(100, 100, 110, 110, 100, if (step == 11L) NA else 0)
  post <- if (step == 12L) c(20, 15, 110, 85, 0, 5) else if (step == 11L) c(100, 90, 100, 100, 100, NA) else c(100, 100, 100, 100, 100, 0)
  ledger <- if (step == 12L) c(0, 15, -20, -35, 0, 5) else if (step == 11L) c(0, 10, -10, -40, 0, 0) else c(0, 0, -10, -40, 0, 0)
  harvest <- if (step == 12L) c(0, 5, 5, 5, 0, 5) else c(0, 10, 10, 10, 0, 0)
  for (entry in list(c("Growth", "growth"), c("Growth_less_harv", "post"),
                     c("Woodfuel_balance", "ledger"), c("Harvest_tot", "harvest"))) {
    write_values(file.path(debug, sprintf("%s%02d.tif", entry[[1L]], step)), get(entry[[2L]]))
  }
}
near <- function(actual, expected) stopifnot(isTRUE(all.equal(actual, expected, tolerance = 1e-6)))
explicit <- stage$build_plan(scenario, stage$parse_periods("2010:2011"), "Out/webmofuss_results_explicit")
stopifnot(all(explicit$records$nrb_attribution_method == "woodfuel_attributed_signed_balance_v1"),
          explicit$periods$baseline_source == "Growth_less_harv",
          sum(explicit$records$role == "nrb_signed_balance", na.rm = TRUE) == 2L,
          explicit$runs[[1L]]$nrb_context$luc_mode == 3L,
          basename(explicit$runs[[1L]]$nrb_context$model_path) == "10_dyn_fixture.egoml")
stage$execute_plan(explicit)
near(as.numeric(values(rast(file.path(explicit$output_dir, "nrb_10_11_mean.tif")))), c(0, 15, 0, 5, NA, 5))
near(as.numeric(values(rast(file.path(explicit$output_dir, "harv_10_11_mean.tif")))), c(0, 15, 15, 15, NA, 5))
period <- stage$parse_periods("2010:2011")
period$period_role <- "v3_stdyn_window"
preharvest <- stage$build_plan(scenario, period, "Out/webmofuss_results_preharvest")
stage$execute_plan(preharvest)
near(as.numeric(values(rast(file.path(preharvest$output_dir, "nrb_10_11_mean.tif")))), c(0, 15, 0, 15, NA, 5))
api <- stage$stage1_nrb_api()
expect_error <- function(expr, pattern) {
  err <- tryCatch(force(expr), error = identity)
  stopifnot(inherits(err, "error"), grepl(pattern, conditionMessage(err)))
}
expect_error(api$mofuss_nrb_context(scenario, expected_steps = 12.5), "step count")
expect_error(api$mofuss_nrb_context(scenario, luc_mode = 3.5), "Invalid LUC")
expect_error(api$mofuss_nrb_context(scenario, luc_mode = 1), "Conflicting LUC")
# Missing state with positive harvest must remain unknown, not get the zero-H
# exception. Restore the file afterwards for the missing-ledger check below.
write_values(file.path(debug, "Harvest_tot11.tif"), c(0, 10, 10, 10, 0, 2))
gap <- api$mofuss_period_nrb(preharvest$runs[[1L]]$nrb_context, 11L, 12L)
stopifnot(is.na(raster::values(gap$nrb)[[6L]]))
write_values(file.path(debug, "Harvest_tot11.tif"), c(0, 10, 10, 10, 0, 0))
# A completed annual model predating the ledger must fail before publishing any
# derived NRB, even when the raw stock endpoint files remain available.
file.rename(file.path(debug, "Woodfuel_balance12.tif"), file.path(debug, "saved_balance12.tif"))
error <- tryCatch(stage$build_plan(scenario, period, "Out/webmofuss_results_missing"), error = identity)
stopifnot(inherits(error, "error"), grepl("Woodfuel_balance", conditionMessage(error)),
          !dir.exists(file.path(scenario, "Out/webmofuss_results_missing")))
cat("PASS: dynamic-LUC Stage 1, no-harvest clearing, signed growth, both baselines, provenance and missing-ledger rejection.\n")
cat("FIXTURE=", fixture, "\n", sep = "")
