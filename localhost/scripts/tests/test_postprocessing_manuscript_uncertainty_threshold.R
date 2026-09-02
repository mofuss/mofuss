# Synthetic Stage 4 test for the manuscript uncertainty-output threshold.
# All generated inputs, outputs, and destructive cleanup stay inside one
# disposable fixture.

suppressPackageStartupMessages(library(terra))

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
stage4_script <- file.path(
  repo_root, "localhost", "scripts", "postprocessing_emissions",
  "4post_manuscript_outputs_v1.R"
)
stopifnot(file.exists(stage4_script))

scratch_parent <- Sys.getenv("MOFUSS_TEST_SCRATCH", unset = tempdir())
if (!dir.exists(scratch_parent)) dir.create(scratch_parent, recursive = TRUE)
fixture <- tempfile("mofuss_manuscript_threshold_", tmpdir = scratch_parent)
source_dir <- file.path(fixture, "ken_2026_2050_mc2")
agb_dir <- file.path(source_dir, "agb_decomposition")
output_dir <- file.path(source_dir, "manuscript_outputs")
dir.create(agb_dir, recursive = TRUE)
keep_fixture <- identical(
  tolower(Sys.getenv("MOFUSS_TEST_KEEP_FIXTURE", unset = "false")), "true"
)
if (!keep_fixture) {
  on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)
}

per_run <- data.frame(
  country_iso = "KEN",
  country_name = "Kenya",
  regrowth_mode = rep(c("capped", "uncapped"), each = 2L),
  display_label = rep(c("Capped regrowth", "Uncapped regrowth"), each = 2L),
  run_id = rep(1:2, times = 2L),
  simulation_start_year = 2000L,
  period_start_year = 2026L,
  period_end_year = 2050L,
  bau_end_agb_mg = c(100, 101, 200, 201),
  ics_end_agb_mg = c(95, 94, 190, 189),
  baseline_delta_agb_mg = 0,
  end_delta_agb_mg = c(5, 7, 9, 11),
  period_delta_agb_mg = c(5, 7, 9, 11),
  period_avoided_loss_mg = c(4, 5, 6, 7),
  period_regrowth_mg = c(1, 2, 3, 4),
  period_avoided_loss_tco2e = c(8, 11, 16, 19),
  period_regrowth_tco2e = c(2, 3, 4, 5),
  agb_avoided_stage2_tco2e = c(10, 14, 20, 24),
  enduse_avoided_tco2e = 5,
  total_avoided_tco2e = c(15, 19, 25, 29),
  n_decomposition_period_common = 64L,
  all_invariants_ok = TRUE,
  stringsAsFactors = FALSE
)
utils::write.csv(
  per_run, file.path(agb_dir, "agb_decomposition_per_run_fixture.csv"),
  row.names = FALSE
)

mc1_labels <- c(
  "BAU AGB 2050 (Mg)", "CCTS AGB 2050 (Mg)",
  "Period avoided loss (Mg)", "Period regrowth (Mg)",
  "Period avoided loss (tCO2e)", "Period regrowth (tCO2e)",
  "AGB avoided - stage 2 (tCO2e)", "End-use avoided (tCO2e)",
  "Total avoided (tCO2e)", "Common decomposition cells"
)
mc1_fields <- c(
  "bau_end_agb_mg", "ics_end_agb_mg", "period_avoided_loss_mg",
  "period_regrowth_mg", "period_avoided_loss_tco2e",
  "period_regrowth_tco2e", "agb_avoided_stage2_tco2e",
  "enduse_avoided_tco2e", "total_avoided_tco2e",
  "n_decomposition_period_common"
)
mc1_table <- data.frame(Metric = mc1_labels, check.names = FALSE)
for (configuration in c("capped", "uncapped")) {
  row <- per_run[per_run$regrowth_mode == configuration & per_run$run_id == 1L, ]
  label <- unique(row$display_label)
  mc1_table[[label]] <- vapply(mc1_fields, function(field) {
    as.numeric(row[[field]][[1L]])
  }, numeric(1))
}
utils::write.csv(
  mc1_table, file.path(agb_dir, "comparison_table_mc1_fixture.csv"),
  row.names = FALSE
)

raster_template <- terra::rast(
  nrows = 8L, ncols = 8L, xmin = 33, xmax = 42, ymin = -5, ymax = 5,
  crs = "EPSG:4326"
)
harvest_raster_template <- terra::project(raster_template, "EPSG:3857")
stopifnot(!terra::same.crs(harvest_raster_template, raster_template))
write_total_raster <- function(
  path, total, pattern = NULL, template = raster_template
) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  raster <- template
  if (is.null(pattern)) pattern <- rep(1, terra::ncell(raster))
  values <- as.numeric(pattern)
  stopifnot(length(values) == terra::ncell(raster))
  values <- values / sum(values) * total
  terra::values(raster) <- values
  terra::writeRaster(raster, path, overwrite = TRUE, datatype = "FLT4S")
}

pair_dirs <- setNames(
  file.path(source_dir, "pairs", paste0("ken_fixture_", c("capped", "uncapped"))),
  c("capped", "uncapped")
)
scalar_fields <- c(
  harvest = "agb_avoided_stage2_tco2e",
  enduse = "enduse_avoided_tco2e",
  total = "total_avoided_tco2e"
)
mc1_filenames <- c(
  harvest = "delta_co2_harvest.tif",
  enduse = "delta_co2_enduse.tif",
  total = "delta_co2.tif"
)
for (configuration in names(pair_dirs)) {
  rows <- per_run[per_run$regrowth_mode == configuration, ]
  for (component in names(scalar_fields)) {
    value <- rows[[scalar_fields[[component]]]][rows$run_id == 1L]
    write_total_raster(
      file.path(pair_dirs[[configuration]], "emissions", "summary_mc1", mc1_filenames[[component]]),
      value
    )
  }
}

rscript <- file.path(
  R.home("bin"), paste0("Rscript", if (.Platform$OS.type == "windows") ".exe" else "")
)
stopifnot(file.exists(rscript))
run_stage4 <- function(threshold) {
  arguments <- c(
    stage4_script,
    paste0("--source-dir=", source_dir),
    paste0("--output-dir=", output_dir),
    paste0("--min-uncertainty-runs=", threshold),
    "--overwrite"
  )
  quote_type <- if (.Platform$OS.type == "windows") "cmd" else "sh"
  result <- system2(
    rscript, vapply(arguments, shQuote, character(1), type = quote_type),
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(result, "status", exact = TRUE)
  if (!is.null(status) && status != 0L) {
    stop(paste(result, collapse = "\n"), call. = FALSE)
  }
  result
}

normalise_inventory <- function(path) {
  sort(gsub("\\\\", "/", list.files(path, recursive = TRUE)))
}
expected_mc1_rasters <- unlist(lapply(c("capped", "uncapped"), function(configuration) {
  file.path(
    "rasters", "mc_1",
    sprintf(
      "ken_2026-2050_%s_%s_mc1_tco2e.tif", configuration,
      c("harvest", "enduse", "total")
    )
  )
}), use.names = FALSE)

# Below the threshold, Stage 4 must succeed without any ensemble inputs and
# must leave a strictly MC1-only package.
low_result <- run_stage4(3L)
expected_low <- c(
  "figures/mc_1/figure_ken_2026-2050_emissions_maps.png",
  "tables/table_ken_2026-2050_mc_1.csv",
  "tables/table_ken_2026-2050_mc_1.png",
  expected_mc1_rasters
)
stopifnot(
  any(grepl("UNCERTAINTY_ADEQUATE=FALSE", low_result, fixed = TRUE)),
  any(grepl("FILE_COUNT=9", low_result, fixed = TRUE)),
  identical(normalise_inventory(output_dir), sort(expected_low)),
  !dir.exists(file.path(output_dir, "figures", "mc_all")),
  !dir.exists(file.path(output_dir, "rasters", "mc_all"))
)

# Add the Stage 3 uncertainty summary and ensemble rasters, then verify that
# equality with the threshold activates the complete MC-all package.
uncertainty_fields <- c(
  "period_delta_agb_mg", "period_avoided_loss_mg", "period_regrowth_mg",
  "period_avoided_loss_tco2e", "period_regrowth_tco2e",
  "agb_avoided_stage2_tco2e", "enduse_avoided_tco2e",
  "total_avoided_tco2e"
)
uncertainty <- do.call(rbind, lapply(c("capped", "uncapped"), function(configuration) {
  rows <- per_run[per_run$regrowth_mode == configuration, ]
  do.call(rbind, lapply(uncertainty_fields, function(field) {
    values <- as.numeric(rows[[field]])
    data.frame(
      regrowth_mode = configuration, metric = field, runs = length(values),
      mean = mean(values), sd = stats::sd(values), stringsAsFactors = FALSE
    )
  }))
}))
utils::write.csv(
  uncertainty, file.path(agb_dir, "uncertainty_summary_fixture.csv"),
  row.names = FALSE
)

sd_pattern <- seq_len(terra::ncell(raster_template))
for (configuration in names(pair_dirs)) {
  rows <- per_run[per_run$regrowth_mode == configuration, ]
  emissions_dir <- file.path(pair_dirs[[configuration]], "emissions")
  for (component in names(scalar_fields)) {
    component_template <- if (component == "harvest") {
      harvest_raster_template
    } else {
      raster_template
    }
    write_total_raster(
      file.path(
        emissions_dir,
        if (component == "enduse") "enduse" else component,
        if (component == "enduse") "delta_co2_enduse.tif" else "delta_co2_mean.tif"
      ),
      mean(as.numeric(rows[[scalar_fields[[component]]]])),
      template = component_template
    )
  }
  harvest_sd_pattern <- seq_len(terra::ncell(harvest_raster_template))
  write_total_raster(
    file.path(emissions_dir, "harvest", "delta_co2_sd.tif"),
    stats::sd(rows$agb_avoided_stage2_tco2e), harvest_sd_pattern,
    harvest_raster_template
  )
  write_total_raster(
    file.path(emissions_dir, "total", "delta_co2_sd.tif"),
    stats::sd(rows$total_avoided_tco2e), rev(sd_pattern)
  )
}

equal_result <- run_stage4(2L)
expected_mc_all_rasters <- unlist(lapply(c("capped", "uncapped"), function(configuration) {
  unlist(lapply(c("harvest", "enduse", "total"), function(component) {
    file.path(
      "rasters", "mc_all",
      sprintf(
        "ken_2026-2050_%s_%s_%s_tco2e.tif", configuration, component,
        c("mean", "sd")
      )
    )
  }), use.names = FALSE)
}), use.names = FALSE)
expected_equal <- c(
  expected_low,
  "figures/mc_all/figure_ken_2026-2050_emissions_maps_wuncer.png",
  "tables/table_ken_2026-2050_mc_all.csv",
  "tables/table_ken_2026-2050_mc_all.png",
  expected_mc_all_rasters
)
stopifnot(
  any(grepl("UNCERTAINTY_ADEQUATE=TRUE", equal_result, fixed = TRUE)),
  any(grepl("FILE_COUNT=24", equal_result, fixed = TRUE)),
  identical(normalise_inventory(output_dir), sort(expected_equal)),
  file.info(file.path(
    output_dir, "figures", "mc_all",
    "figure_ken_2026-2050_emissions_maps_wuncer.png"
  ))$size > 0
)
for (configuration in c("capped", "uncapped")) {
  enduse_sd <- terra::rast(file.path(
    output_dir, "rasters", "mc_all",
    sprintf("ken_2026-2050_%s_enduse_sd_tco2e.tif", configuration)
  ))
  stopifnot(terra::global(enduse_sd, "max", na.rm = TRUE)[[1L]] == 0)
}
rm(enduse_sd)
invisible(gc())

cat("POSTPROCESSING_MANUSCRIPT_UNCERTAINTY_THRESHOLD_TEST_OK\n")
if (keep_fixture) cat("FIXTURE_PRESERVED=", fixture, "\n", sep = "")
if (!keep_fixture) {
  terra::tmpFiles(remove = TRUE)
  cleanup_status <- unlink(fixture, recursive = TRUE, force = TRUE)
  stopifnot(cleanup_status == 0L, !file.exists(fixture))
}
