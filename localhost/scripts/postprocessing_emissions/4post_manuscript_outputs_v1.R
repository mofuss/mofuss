#!/usr/bin/env Rscript

# SPDX-License-Identifier: Apache-2.0
#
# Copyright 2025-2027 Universidad Nacional Autónoma de México
# and Stockholm Environment Institute
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# https://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# MoFuSS ----
# Script: 4post_manuscript_outputs_v1.R
# Version: 1
# Date: August 2026
# Execution: Source from RStudio; Rscript compatibility is secondary.
# Dinamica EGO does not invoke this script directly.
#
# Purpose: Build the minimal manuscript package from existing Stage 2 and
# Stage 3 outputs without rerunning MoFuSS or any emissions stage.
# Inputs: A completed Stage 2/3 analysis root containing summary CSVs and
# component rasters.
# Outputs: MC01 and MC-all manuscript tables, 18 component rasters, and one
# 300-dpi MC01 emissions-map figure.
# Side effects: A clean rebuild fully deletes the exact validated
# manuscript_outputs directory before recreating it.

# 2dolist ----

# Load libraries ----

suppressPackageStartupMessages({
  library(terra)
})

# Internal parameters ----

options(scipen = 999)
terra::terraOptions(progress = 0)

SCRIPT_VERSION <- 1L
DEFAULT_MIN_UNCERTAINTY_RUNS <- 30L
CONFIGURATION_ORDER <- c("capped", "uncapped")
COMPONENT_ORDER <- c("harvest", "enduse", "total")
COMPONENT_LABELS <- c(harvest = "Harvest / AGB", enduse = "End-use", total = "Total")

# RSTUDIO SOURCE SETTINGS. Edit these values, then press Source.
V1_RSTUDIO_SOURCE_DIR <- "D:/mofuss_postprocessing/ken_2026_2030_mc2"
V1_RSTUDIO_OUTPUT_DIR <- file.path(V1_RSTUDIO_SOURCE_DIR, "manuscript_outputs")
V1_RSTUDIO_MIN_UNCERTAINTY_RUNS <- DEFAULT_MIN_UNCERTAINTY_RUNS
V1_RSTUDIO_CLEAN_REBUILD <- TRUE

stopf <- function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(name, default = NULL) {
  prefix <- paste0("--", name, "=")
  hit <- args[startsWith(args, prefix)]
  if (!length(hit)) return(default)
  if (length(hit) > 1L) stopf("Argument --%s was supplied more than once.", name)
  substring(hit[[1L]], nchar(prefix) + 1L)
}
arg_flag <- function(name) paste0("--", name) %in% args

if (interactive()) {
  source_dir_arg <- V1_RSTUDIO_SOURCE_DIR
  output_dir_arg <- V1_RSTUDIO_OUTPUT_DIR
  overwrite <- isTRUE(V1_RSTUDIO_CLEAN_REBUILD)
  min_uncertainty_runs <- suppressWarnings(as.integer(
    V1_RSTUDIO_MIN_UNCERTAINTY_RUNS
  ))
} else {
  source_dir_arg <- arg_value("source-dir")
  output_dir_arg <- arg_value("output-dir")
  overwrite <- arg_flag("overwrite")
  min_uncertainty_runs <- suppressWarnings(as.integer(arg_value(
    "min-uncertainty-runs", as.character(DEFAULT_MIN_UNCERTAINTY_RUNS)
  )))
}

if (is.null(source_dir_arg) || !nzchar(source_dir_arg)) {
  stopf("Required argument missing: --source-dir=<existing Stage 2/3 folder>")
}
if (is.null(output_dir_arg) || !nzchar(output_dir_arg)) {
  stopf("Required argument missing: --output-dir=<manuscript output folder>")
}
if (!is.finite(min_uncertainty_runs) || min_uncertainty_runs < 2L) {
  stopf("--min-uncertainty-runs must be an integer >= 2.")
}

source_dir <- normalizePath(source_dir_arg, winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(output_dir_arg, winslash = "/", mustWork = FALSE)

v1_path_key <- function(path, must_work = FALSE) {
  tolower(gsub("/+$", "", normalizePath(
    path, winslash = "/", mustWork = must_work
  )))
}

v1_root_like <- function(path) {
  key <- gsub("\\\\", "/", path)
  identical(key, "/") ||
    grepl("^[a-z]:/?$", key, ignore.case = TRUE) ||
    grepl("^//[^/]+/[^/]+/?$", key)
}

prepare_output_dir <- function(path, source_root, allow_overwrite) {
  target <- normalizePath(path, winslash = "/", mustWork = FALSE)
  expected <- normalizePath(
    file.path(source_root, "manuscript_outputs"),
    winslash = "/", mustWork = FALSE
  )
  if (!identical(tolower(basename(target)), "manuscript_outputs") ||
      !identical(v1_path_key(target), v1_path_key(expected))) {
    stopf(
      "Refusing clean rebuild: output must be exactly <source-dir>/manuscript_outputs: %s",
      target
    )
  }
  parent <- normalizePath(dirname(target), winslash = "/", mustWork = FALSE)
  if (v1_root_like(target) || v1_root_like(parent)) {
    stopf("Refusing clean rebuild at a filesystem root or its direct child: %s", target)
  }
  if (file.exists(target) && !dir.exists(target)) {
    stopf("Output path exists and is not a directory: %s", target)
  }
  if (dir.exists(target)) {
    existing <- list.files(target, all.files = TRUE, no.. = TRUE)
    if (length(existing) && !allow_overwrite) {
      stopf("Output directory is not empty; use --overwrite to replace the package: %s", target)
    }
    if (allow_overwrite) {
      resolved <- normalizePath(target, winslash = "/", mustWork = TRUE)
      if (!identical(v1_path_key(resolved, TRUE), v1_path_key(target, TRUE))) {
        stopf("Refusing clean rebuild through a redirected output path: %s", target)
      }
      message("Removing existing manuscript output folder: ", target)
      status <- unlink(target, recursive = TRUE, force = TRUE)
      if (status != 0L || file.exists(target)) {
        stopf("Could not fully remove existing manuscript output folder: %s", target)
      }
    }
  }
  dirs <- c("figures/mc_1", "rasters/mc_1", "rasters/mc_all", "tables")
  for (d in dirs) dir.create(file.path(target, d), recursive = TRUE, showWarnings = FALSE)
}

read_csv_required <- function(path, label) {
  if (!file.exists(path)) stopf("Missing %s: %s", label, path)
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

require_columns <- function(x, fields, label) {
  missing <- setdiff(fields, names(x))
  if (length(missing)) stopf("%s is missing columns: %s", label, paste(missing, collapse = ", "))
}

select_one <- function(pattern, label) {
  hits <- Sys.glob(pattern)
  if (length(hits) != 1L) stopf("Expected exactly one %s; found %d.", label, length(hits))
  normalizePath(hits[[1L]], winslash = "/", mustWork = TRUE)
}

copy_checked <- function(from, to) {
  if (!file.exists(from)) stopf("Missing source file: %s", from)
  if (!file.copy(from, to, overwrite = TRUE, copy.date = TRUE)) stopf("Could not copy %s to %s", from, to)
  invisible(to)
}

write_csv_utf8 <- function(x, path) {
  quote_csv <- function(value) {
    if (is.na(value)) return("")
    paste0('"', gsub('"', '""', as.character(value), fixed = TRUE), '"')
  }
  header <- paste(vapply(names(x), quote_csv, character(1)), collapse = ",")
  rows <- vapply(seq_len(nrow(x)), function(i) {
    cells <- vapply(seq_along(x), function(j) {
      value <- x[[j]][[i]]
      if (is.numeric(x[[j]])) {
        if (is.na(value)) "" else format(value, scientific = FALSE, trim = TRUE, digits = 15)
      } else {
        quote_csv(value)
      }
    }, character(1))
    paste(cells, collapse = ",")
  }, character(1))
  payload <- paste0(paste(c(header, rows), collapse = "\r\n"), "\r\n")
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(charToRaw(enc2utf8(payload)), connection)
  invisible(path)
}

same_number <- function(a, b, tolerance = 1e-6) {
  isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tolerance))
}

agb_dir <- file.path(source_dir, "agb_decomposition")
agb_per_run_path <- select_one(
  file.path(agb_dir, "agb_decomposition_per_run_*.csv"), "Stage 3 per-run decomposition file"
)
agb_mc1_path <- select_one(
  file.path(agb_dir, "comparison_table_mc1_*.csv"), "Stage 3 MC1 comparison table"
)
agb_uncertainty_path <- select_one(
  file.path(agb_dir, "uncertainty_summary_*.csv"), "Stage 3 uncertainty summary"
)

per_run <- read_csv_required(agb_per_run_path, "Stage 3 per-run decomposition")
require_columns(
  per_run,
  c(
    "country_iso", "country_name", "regrowth_mode", "display_label", "run_id",
    "simulation_start_year",
    "period_start_year", "period_end_year", "bau_end_agb_mg", "ics_end_agb_mg",
    "baseline_delta_agb_mg", "end_delta_agb_mg", "period_delta_agb_mg",
    "period_avoided_loss_mg", "period_regrowth_mg", "period_avoided_loss_tco2e",
    "period_regrowth_tco2e", "agb_avoided_stage2_tco2e", "enduse_avoided_tco2e",
    "total_avoided_tco2e", "n_decomposition_period_common", "all_invariants_ok"
  ),
  "Stage 3 per-run decomposition"
)
if (!all(as.logical(per_run$all_invariants_ok))) stopf("Stage 3 per-run decomposition contains a failed invariant.")

configurations <- unique(as.character(per_run$regrowth_mode))
if (!setequal(configurations, CONFIGURATION_ORDER)) {
  stopf("Expected capped and uncapped Stage 3 configurations; found: %s", paste(configurations, collapse = ", "))
}

region_iso_values <- unique(as.character(per_run$country_iso))
region_name_values <- unique(as.character(per_run$country_name))
period_starts <- unique(as.integer(per_run$period_start_year))
period_ends <- unique(as.integer(per_run$period_end_year))
if (length(region_iso_values) != 1L || length(region_name_values) != 1L ||
    length(period_starts) != 1L || length(period_ends) != 1L) {
  stopf("Stage 3 rows do not share one region and one analysis period.")
}

region_iso <- region_iso_values[[1L]]
region_name <- region_name_values[[1L]]
region_slug <- tolower(gsub("[^A-Za-z0-9]+", "_", region_iso))
period_start <- period_starts[[1L]]
period_end <- period_ends[[1L]]
period_tag <- sprintf("%d-%d", period_start, period_end)
simulation_start_years <- unique(as.integer(per_run$simulation_start_year))
if (length(simulation_start_years) != 1L) stopf("Stage 3 rows do not share one simulation start year.")
simulation_start_year <- simulation_start_years[[1L]]
spinup_years <- period_start - simulation_start_year
reporting_years <- period_end - (simulation_start_year + spinup_years) + 1L
if (spinup_years < 0L || reporting_years < 1L) stopf("Derived spin-up or reporting duration is invalid.")

run_ids_by_configuration <- lapply(CONFIGURATION_ORDER, function(configuration) {
  sort(unique(as.integer(per_run$run_id[per_run$regrowth_mode == configuration])))
})
names(run_ids_by_configuration) <- CONFIGURATION_ORDER
if (!identical(run_ids_by_configuration$capped, run_ids_by_configuration$uncapped)) {
  stopf("Capped and uncapped configurations do not contain the same run IDs.")
}
run_ids <- run_ids_by_configuration$capped
if (length(run_ids) < 2L) stopf("At least two Monte Carlo runs are required for an SD table.")
if (!1L %in% run_ids) stopf("Run 1 is missing from the Stage 3 per-run decomposition.")
n_runs <- length(run_ids)

display_labels <- vapply(CONFIGURATION_ORDER, function(configuration) {
  hits <- unique(as.character(per_run$display_label[per_run$regrowth_mode == configuration]))
  if (length(hits) != 1L) stopf("%s has inconsistent display labels.", configuration)
  hits[[1L]]
}, character(1))
names(display_labels) <- CONFIGURATION_ORDER

metric_fields <- c(
  "bau_end_agb_mg", "ics_end_agb_mg", "period_avoided_loss_mg", "period_regrowth_mg",
  "period_avoided_loss_tco2e", "period_regrowth_tco2e", "agb_avoided_stage2_tco2e",
  "enduse_avoided_tco2e", "total_avoided_tco2e", "total_avoided_tco2e",
  "n_decomposition_period_common"
)
metric_labels <- c(
  sprintf("BAU AGB %d (Mg)", period_end),
  sprintf("CCTS AGB %d (Mg)", period_end),
  "Avoided AGB loss (Mg/period)",
  "Enhanced AGB regrowth (Mg/period)",
  "Avoided AGB-loss emissions (tCO2e/period)",
  "Enhanced-regrowth emissions (tCO2e/period)",
  "AGB emissions avoided - Stage 2 (tCO2e/period)",
  "End-use emissions avoided (tCO2e/period)",
  "Total emissions avoided (tCO2e/period)",
  "Annual emissions avoided (MtCO2e yr^-1)",
  "Common decomposition cells"
)
names(metric_fields) <- metric_labels
metric_scales <- c(rep(1, 9L), 1 / (1e6 * reporting_years), 1)
metric_digits <- c(rep(0L, 9L), 3L, 0L)
source_mc1_labels <- c(
  sprintf("BAU AGB %d (Mg)", period_end),
  sprintf("CCTS AGB %d (Mg)", period_end),
  "Period avoided loss (Mg)",
  "Period regrowth (Mg)",
  "Period avoided loss (tCO2e)",
  "Period regrowth (tCO2e)",
  "AGB avoided - stage 2 (tCO2e)",
  "End-use avoided (tCO2e)",
  "Total avoided (tCO2e)",
  NA_character_,
  "Common decomposition cells"
)

mc1_table <- data.frame(Metric = metric_labels, check.names = FALSE, stringsAsFactors = FALSE)
mc_all_table <- data.frame(Metric = metric_labels, check.names = FALSE, stringsAsFactors = FALSE)
format_metric <- function(x, digits) formatC(as.numeric(x), format = "f", digits = digits)
plus_minus <- intToUtf8(177L)

for (configuration in CONFIGURATION_ORDER) {
  rows <- per_run[per_run$regrowth_mode == configuration, , drop = FALSE]
  rows <- rows[order(as.integer(rows$run_id)), , drop = FALSE]
  mc1_row <- rows[as.integer(rows$run_id) == 1L, , drop = FALSE]
  if (nrow(mc1_row) != 1L) stopf("Expected exactly one MC1 row for %s.", configuration)

  raw_mc1 <- vapply(unname(metric_fields), function(field) as.numeric(mc1_row[[field]][[1L]]), numeric(1)) * metric_scales
  means <- vapply(unname(metric_fields), function(field) mean(as.numeric(rows[[field]])), numeric(1)) * metric_scales
  sds <- vapply(unname(metric_fields), function(field) stats::sd(as.numeric(rows[[field]])), numeric(1)) * metric_scales
  mc1_table[[display_labels[[configuration]]]] <- vapply(
    seq_along(raw_mc1), function(i) round(raw_mc1[[i]], metric_digits[[i]]), numeric(1)
  )
  mc_all_table[[display_labels[[configuration]]]] <- paste0(
    vapply(seq_along(means), function(i) format_metric(means[[i]], metric_digits[[i]]), character(1)),
    " ", plus_minus, " ",
    vapply(seq_along(sds), function(i) format_metric(sds[[i]], metric_digits[[i]]), character(1))
  )
}

# Validate MC1 values against the existing Stage 3 comparison table before rounding.
source_mc1_table <- read_csv_required(agb_mc1_path, "Stage 3 MC1 comparison table")
require_columns(source_mc1_table, c("Metric", unname(display_labels)), "Stage 3 MC1 comparison table")
for (configuration in CONFIGURATION_ORDER) {
  rows <- per_run[per_run$regrowth_mode == configuration & as.integer(per_run$run_id) == 1L, , drop = FALSE]
  for (i in which(!is.na(source_mc1_labels))) {
    source_row <- which(source_mc1_table$Metric == source_mc1_labels[[i]])
    if (length(source_row) != 1L) stopf("MC1 source metric is missing: %s", source_mc1_labels[[i]])
    raw_value <- as.numeric(rows[[unname(metric_fields[[i]])]][[1L]])
    source_value <- as.numeric(source_mc1_table[[display_labels[[configuration]]]][[source_row]])
    if (!same_number(raw_value, source_value)) {
      stopf("The derived %s MC1 metric does not reconcile: %s.", configuration, metric_labels[[i]])
    }
  }
}

# Validate Stage 3 ensemble metrics against the per-run means and sample SDs.
uncertainty <- read_csv_required(agb_uncertainty_path, "Stage 3 uncertainty summary")
require_columns(uncertainty, c("regrowth_mode", "metric", "runs", "mean", "sd"), "Stage 3 uncertainty summary")
uncertainty_checks <- c(
  period_delta_agb_mg = "period_delta_agb_mg",
  period_avoided_loss_mg = "period_avoided_loss_mg",
  period_regrowth_mg = "period_regrowth_mg",
  period_avoided_loss_tco2e = "period_avoided_loss_tco2e",
  period_regrowth_tco2e = "period_regrowth_tco2e",
  agb_avoided_stage2_tco2e = "agb_avoided_stage2_tco2e",
  enduse_avoided_tco2e = "enduse_avoided_tco2e",
  total_avoided_tco2e = "total_avoided_tco2e"
)
for (configuration in CONFIGURATION_ORDER) {
  rows <- per_run[per_run$regrowth_mode == configuration, , drop = FALSE]
  for (metric in names(uncertainty_checks)) {
    hit <- uncertainty[uncertainty$regrowth_mode == configuration & uncertainty$metric == metric, , drop = FALSE]
    if (nrow(hit) != 1L || as.integer(hit$runs[[1L]]) != n_runs) {
      stopf("Uncertainty summary row is missing or has the wrong run count: %s %s.", configuration, metric)
    }
    values <- as.numeric(rows[[uncertainty_checks[[metric]]]])
    if (!same_number(mean(values), hit$mean[[1L]], tolerance = 1e-8) ||
        !same_number(stats::sd(values), hit$sd[[1L]], tolerance = 1e-8)) {
      stopf("Uncertainty summary does not reconcile: %s %s.", configuration, metric)
    }
  }
}

footnotes <- c(
  sprintf(
    paste0(
      "Note 1: The %s reporting period begins after the %d-%d spin-up/past-to-present simulation ",
      "(%d years); its start year is calculated as simulation_start_year + .V13_SPINUP_YEARS. ",
      "Period effects use the %d ",
      "end-of-previous-year state as their baseline. BAU and CCTS may enter the reporting period with ",
      "slightly different AGB, especially when Patcher is active; therefore their %d AGB stock difference ",
      "need not equal the AGB effect accumulated during %s."
    ),
    period_tag, simulation_start_year, period_start - 1L, spinup_years, period_start - 1L,
    period_end, period_tag
  ),
  sprintf(
    paste0(
      "Note 2: Annual emissions avoided = Total emissions avoided / 1,000,000 / %d reporting years, ",
      "where %d = %d - (%d + %d) + 1."
    ),
    reporting_years, reporting_years, period_end, simulation_start_year, spinup_years
  )
)
for (note in footnotes) {
  mc1_note <- data.frame(Metric = note, check.names = FALSE, stringsAsFactors = FALSE)
  mc_all_note <- data.frame(Metric = note, check.names = FALSE, stringsAsFactors = FALSE)
  for (configuration in CONFIGURATION_ORDER) {
    mc1_note[[display_labels[[configuration]]]] <- NA_real_
    mc_all_note[[display_labels[[configuration]]]] <- NA_character_
  }
  mc1_table <- rbind(mc1_table, mc1_note)
  mc_all_table <- rbind(mc_all_table, mc_all_note)
}

table_mc1_path <- file.path(output_dir, "tables", sprintf("table_%s_%s_mc_1.csv", region_slug, period_tag))
table_mc_all_path <- file.path(output_dir, "tables", sprintf("table_%s_%s_mc_all.csv", region_slug, period_tag))

pair_root <- file.path(source_dir, "pairs")
pair_dirs <- list.dirs(pair_root, full.names = TRUE, recursive = FALSE)
find_pair_dir <- function(configuration) {
  hits <- pair_dirs[grepl(paste0("_", configuration, "$"), basename(pair_dirs))]
  if (length(hits) != 1L) stopf("Expected exactly one %s pair directory; found %d.", configuration, length(hits))
  normalizePath(hits[[1L]], winslash = "/", mustWork = TRUE)
}

records <- list()
for (configuration in CONFIGURATION_ORDER) {
  emissions_dir <- file.path(find_pair_dir(configuration), "emissions")
  records[[configuration]] <- list(
    mc1 = c(
      enduse = file.path(emissions_dir, "summary_mc1", "delta_co2_enduse.tif"),
      harvest = file.path(emissions_dir, "summary_mc1", "delta_co2_harvest.tif"),
      total = file.path(emissions_dir, "summary_mc1", "delta_co2.tif")
    ),
    mc_all_mean = c(
      enduse = file.path(emissions_dir, "enduse", "delta_co2_enduse.tif"),
      harvest = file.path(emissions_dir, "harvest", "delta_co2_mean.tif"),
      total = file.path(emissions_dir, "total", "delta_co2_mean.tif")
    ),
    mc_all_sd = c(
      enduse = file.path(emissions_dir, "enduse", "delta_co2_enduse.tif"),
      harvest = file.path(emissions_dir, "harvest", "delta_co2_sd.tif"),
      total = file.path(emissions_dir, "total", "delta_co2_sd.tif")
    )
  )
  missing <- unlist(records[[configuration]], use.names = FALSE)
  missing <- missing[!file.exists(missing)]
  if (length(missing)) stopf("Missing %s raster sources: %s", configuration, paste(missing, collapse = ", "))
}

# All scalar and raster inputs have passed their preflight checks. Only now is
# the exact manuscript_outputs directory removed and rebuilt.
prepare_output_dir(output_dir, source_dir, overwrite)
write_csv_utf8(mc1_table, table_mc1_path)
write_csv_utf8(mc_all_table, table_mc_all_path)

raster_path <- function(scope, configuration, component, statistic = NULL) {
  suffix <- if (scope == "mc_1") {
    sprintf("%s_%s_%s_%s_mc1_tco2e.tif", region_slug, period_tag, configuration, component)
  } else {
    sprintf("%s_%s_%s_%s_%s_tco2e.tif", region_slug, period_tag, configuration, component, statistic)
  }
  file.path(output_dir, "rasters", scope, suffix)
}

raster_objects <- list()
for (configuration in CONFIGURATION_ORDER) {
  scalar_rows <- per_run[per_run$regrowth_mode == configuration, , drop = FALSE]
  scalar_fields <- c(enduse = "enduse_avoided_tco2e", harvest = "agb_avoided_stage2_tco2e", total = "total_avoided_tco2e")

  for (component in COMPONENT_ORDER) {
    mc1_destination <- raster_path("mc_1", configuration, component)
    copy_checked(records[[configuration]]$mc1[[component]], mc1_destination)
    mc1_raster <- terra::rast(mc1_destination)
    mc1_sum <- as.numeric(terra::global(mc1_raster, "sum", na.rm = TRUE)[[1L]])
    mc1_scalar <- as.numeric(scalar_rows[[scalar_fields[[component]]]][as.integer(scalar_rows$run_id) == 1L])
    if (length(mc1_scalar) != 1L || abs(mc1_sum - mc1_scalar) > 0.05) {
      stopf("%s %s MC1 raster does not reconcile with the national scalar.", configuration, component)
    }
    raster_objects[[paste(configuration, component, sep = "_")]] <- mc1_raster

    mean_destination <- raster_path("mc_all", configuration, component, "mean")
    copy_checked(records[[configuration]]$mc_all_mean[[component]], mean_destination)
    mean_raster <- terra::rast(mean_destination)
    mean_sum <- as.numeric(terra::global(mean_raster, "sum", na.rm = TRUE)[[1L]])
    mean_scalar <- mean(as.numeric(scalar_rows[[scalar_fields[[component]]]]))
    if (abs(mean_sum - mean_scalar) > 0.05) {
      stopf("%s %s mean raster does not reconcile with the national scalar.", configuration, component)
    }

    sd_destination <- raster_path("mc_all", configuration, component, "sd")
    if (component == "enduse") {
      if (stats::sd(as.numeric(scalar_rows[[scalar_fields[[component]]]])) > 1e-9) {
        stopf("End-use varies across runs, so a zero SD raster cannot be derived.")
      }
      enduse_source <- terra::rast(records[[configuration]]$mc_all_sd[[component]])
      enduse_sd <- terra::ifel(!is.na(enduse_source), 0, NA)
      names(enduse_sd) <- "enduse_sd_tco2e"
      terra::writeRaster(
        enduse_sd, sd_destination, overwrite = TRUE, datatype = "FLT4S",
        gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3")
      )
    } else {
      copy_checked(records[[configuration]]$mc_all_sd[[component]], sd_destination)
    }
  }
}

quantile_pair <- function(r) {
  as.numeric(terra::global(r, stats::quantile, probs = c(0.02, 0.98), na.rm = TRUE)[1L, ])
}
map_scales <- list()
for (component in COMPONENT_ORDER) {
  values <- unlist(lapply(CONFIGURATION_ORDER, function(configuration) {
    quantile_pair(raster_objects[[paste(configuration, component, sep = "_")]])
  }))
  limit <- max(abs(values), na.rm = TRUE)
  if (!is.finite(limit) || limit <= 0) limit <- 1
  map_scales[[component]] <- c(-limit, limit)
}

extent_values <- lapply(raster_objects, function(r) as.vector(terra::ext(r)))
full_extent <- terra::ext(
  min(vapply(extent_values, function(x) x[[1L]], numeric(1))),
  max(vapply(extent_values, function(x) x[[2L]], numeric(1))),
  min(vapply(extent_values, function(x) x[[3L]], numeric(1))),
  max(vapply(extent_values, function(x) x[[4L]], numeric(1)))
)
plot_rasters <- lapply(raster_objects, function(r) {
  terra::aggregate(terra::extend(r, full_extent), fact = 4L, fun = "mean", na.rm = TRUE)
})

draw_mc1_map_figure <- function() {
  op <- graphics::par(
    mfrow = c(length(CONFIGURATION_ORDER), length(COMPONENT_ORDER)),
    mar = c(1.3, 1.3, 3.1, 4.8), oma = c(3.5, 1.0, 5.1, 1.0),
    xaxs = "i", yaxs = "i"
  )
  on.exit(graphics::par(op), add = TRUE)
  colours <- grDevices::hcl.colors(255, "Blue-Red 3")
  for (configuration in CONFIGURATION_ORDER) {
    for (component in COMPONENT_ORDER) {
      terra::plot(
        plot_rasters[[paste(configuration, component, sep = "_")]],
        col = colours, range = map_scales[[component]], axes = FALSE, maxcell = 50000,
        main = sprintf("%s: %s", tools::toTitleCase(configuration), COMPONENT_LABELS[[component]]),
        cex.main = 0.9, plg = list(cex = 0.65)
      )
    }
  }
  graphics::mtext(
    sprintf("%s placeholder: BAU - CCTS emissions differences, %s", region_name, period_tag),
    side = 3, outer = TRUE, line = 2.7, font = 2, cex = 1.15
  )
  graphics::mtext(
    "First Monte Carlo realization (MC1); capped and uncapped configurations; units are tCO2e per cell.",
    side = 3, outer = TRUE, line = 1.0, cex = 0.75
  )
  graphics::mtext(
    "Pixel values are full-period totals. Positive BAU - CCTS values indicate avoided emissions; negative values indicate higher emissions under CCTS.",
    side = 1, outer = TRUE, line = 1.2, cex = 0.67
  )
}

figure_path <- file.path(
  output_dir, "figures", "mc_1", sprintf("figure_%s_%s_emissions_maps.png", region_slug, period_tag)
)
grDevices::png(figure_path, width = 2400, height = 1600, res = 300)
draw_mc1_map_figure()
grDevices::dev.off()

expected_files <- c(
  file.path("figures", "mc_1", basename(figure_path)),
  file.path("tables", basename(table_mc1_path)),
  file.path("tables", basename(table_mc_all_path))
)
for (configuration in CONFIGURATION_ORDER) {
  for (component in COMPONENT_ORDER) {
    expected_files <- c(
      expected_files,
      file.path("rasters", "mc_1", basename(raster_path("mc_1", configuration, component))),
      file.path("rasters", "mc_all", basename(raster_path("mc_all", configuration, component, "mean"))),
      file.path("rasters", "mc_all", basename(raster_path("mc_all", configuration, component, "sd")))
    )
  }
}
actual_files <- list.files(output_dir, recursive = TRUE, all.files = FALSE)
if (!setequal(gsub("\\\\", "/", actual_files), gsub("\\\\", "/", expected_files))) {
  stopf("Final package inventory differs from the expected 21 files.")
}

if (n_runs < min_uncertainty_runs) {
  warning(sprintf(
    "Only %d runs are available (<%d); MC-all mean +/- SD values and SD rasters are exploratory placeholders.",
    n_runs, min_uncertainty_runs
  ), call. = FALSE)
}

cat(sprintf("MANUSCRIPT_PACKAGE_CREATED=%s\n", output_dir))
cat(sprintf("SCRIPT_VERSION=%d\n", SCRIPT_VERSION))
cat(sprintf("REGION=%s\n", region_iso))
cat(sprintf("PERIOD=%s\n", period_tag))
cat(sprintf("MC_RUNS=%d\n", n_runs))
cat(sprintf("UNCERTAINTY_ADEQUATE=%s\n", n_runs >= min_uncertainty_runs))
cat(sprintf("FILE_COUNT=%d\n", length(actual_files)))
