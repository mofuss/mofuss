#!/usr/bin/env Rscript

# SPDX-License-Identifier: Apache-2.0
#
# Copyright 2025-2027 Universidad Nacional Autonoma de Mexico
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
# Script: 5post_manuscript_outputsGLOBAL_v1.R
# Version: 1
# Date: September 2026
#
# Purpose: Combine validated Stage 3/4 country, subregion, and raster outputs
# into a Global South manuscript package without rerunning MoFuSS.
#
# Input contract:
# - An explicit manifest with one row per included analysis root.
# - Stage 3 country-per-run decomposition data. A legacy regional per-run file
#   is accepted only for one-country analysis roots.
# - Stage 4 capped and uncapped MC-all Total mean rasters for spatial panels.
# - The canonical M67/GME regionalization CSVs for coverage validation.
#
# Modes:
# - partial: writes visibly labelled preliminary outputs and a coverage report.
# - strict: requires complete canonical coverage, final partition inputs, and
#   the configured minimum Monte Carlo run count.
#
# Uncertainty:
# - Country uncertainty is calculated from its original per-run values.
# - Multiple analysis roots are combined either by matching run_id (aligned)
#   or by reproducible independent resampling at the analysis-root level.
# - The same combined draws feed subregion and global summaries, so every
#   global draw is exactly the sum of its corresponding subregion draws.
# - Published quantiles and SDs are never added across analysis roots.
#
# Side effects: --overwrite removes only the exact validated output directory.

suppressPackageStartupMessages({
  library(terra)
})
for (package in c("readr", "rnaturalearth")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(sprintf("Required R package is not installed: %s", package), call. = FALSE)
  }
}

options(scipen = 999)
terra::terraOptions(progress = 0)

SCRIPT_VERSION <- 1L
CONFIGURATION_ORDER <- c("capped", "uncapped")
METRIC_FIELDS <- c(
  avoided_loss = "period_avoided_loss_tco2e",
  regrowth = "period_regrowth_tco2e",
  harvest = "agb_avoided_stage2_tco2e",
  enduse = "enduse_avoided_tco2e",
  total = "total_avoided_tco2e"
)
METRIC_LABELS <- c(
  avoided_loss = "Avoided AGB loss",
  regrowth = "Enhanced regrowth",
  harvest = "Harvest / AGB",
  enduse = "End-use",
  total = "Total"
)
FIGURE_DPI <- 300L
MAP_DISPLAY_CRS <- "EPSG:8857"

# RStudio/source defaults are intentionally NULL so stale machine-specific
# paths cannot be used accidentally.
V1_RSTUDIO_MANIFEST <- NULL
V1_RSTUDIO_OUTPUT_DIR <- NULL
V1_RSTUDIO_TEMP_DIR <- NULL
V1_RSTUDIO_MODE <- "partial"
V1_RSTUDIO_MC_COMBINATION <- "independent"
V1_RSTUDIO_MIN_RUNS <- 30L
V1_RSTUDIO_GLOBAL_RESAMPLES <- 10000L
V1_RSTUDIO_RANDOM_SEED <- 20260910L
V1_RSTUDIO_CLEAN_REBUILD <- TRUE

stopf <- function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)

args <- commandArgs(trailingOnly = TRUE)
source_mode <- interactive() || sys.nframe() > 0L
arg_value <- function(name, default = NULL) {
  prefix <- paste0("--", name, "=")
  hit <- args[startsWith(args, prefix)]
  if (!length(hit)) return(default)
  if (length(hit) > 1L) stopf("Argument --%s was supplied more than once.", name)
  substring(hit[[1L]], nchar(prefix) + 1L)
}
arg_flag <- function(name) paste0("--", name) %in% args

script_command <- commandArgs(trailingOnly = FALSE)
script_argument <- script_command[startsWith(script_command, "--file=")]
script_path <- if (length(script_argument)) {
  normalizePath(
    sub("^--file=", "", script_argument[[1L]]),
    winslash = "/", mustWork = TRUE
  )
} else {
  normalizePath(
    file.path(getwd(), "5post_manuscript_outputsGLOBAL_v1.R"),
    winslash = "/", mustWork = FALSE
  )
}
script_dir <- dirname(script_path)
repository_root <- normalizePath(
  file.path(script_dir, "..", "..", ".."),
  winslash = "/", mustWork = TRUE
)

if (source_mode) {
  manifest_arg <- V1_RSTUDIO_MANIFEST
  output_dir_arg <- V1_RSTUDIO_OUTPUT_DIR
  temp_dir_arg <- V1_RSTUDIO_TEMP_DIR
  run_mode <- V1_RSTUDIO_MODE
  mc_combination <- V1_RSTUDIO_MC_COMBINATION
  min_runs <- as.integer(V1_RSTUDIO_MIN_RUNS)
  global_resamples <- as.integer(V1_RSTUDIO_GLOBAL_RESAMPLES)
  random_seed <- as.integer(V1_RSTUDIO_RANDOM_SEED)
  overwrite <- isTRUE(V1_RSTUDIO_CLEAN_REBUILD)
  regionalization_dir_arg <- file.path(repository_root, "admin_regions")
} else {
  manifest_arg <- arg_value("manifest")
  output_dir_arg <- arg_value("output-dir")
  temp_dir_arg <- arg_value("temp-dir")
  run_mode <- tolower(arg_value("mode", "strict"))
  mc_combination <- tolower(arg_value("mc-combination", "independent"))
  min_runs <- suppressWarnings(as.integer(arg_value("min-runs", "30")))
  global_resamples <- suppressWarnings(as.integer(arg_value("global-resamples", "10000")))
  random_seed <- suppressWarnings(as.integer(arg_value("random-seed", "20260910")))
  overwrite <- arg_flag("overwrite")
  regionalization_dir_arg <- arg_value(
    "regionalization-dir", file.path(repository_root, "admin_regions")
  )
}

if (is.null(manifest_arg) || !nzchar(manifest_arg)) {
  stopf("Required argument missing: --manifest=<CSV>")
}
if (is.null(output_dir_arg) || !nzchar(output_dir_arg)) {
  stopf("Required argument missing: --output-dir=<manuscript_outputs>")
}
if (is.null(temp_dir_arg) || !nzchar(temp_dir_arg)) {
  stopf("Required argument missing: --temp-dir=<E:/MoFuSS_Active/task-folder>")
}
if (!run_mode %in% c("partial", "strict")) {
  stopf("--mode must be partial or strict.")
}
if (!mc_combination %in% c("independent", "aligned")) {
  stopf("--mc-combination must be independent or aligned.")
}
if (!is.finite(min_runs) || min_runs < 2L) {
  stopf("--min-runs must be an integer >= 2.")
}
if (!is.finite(global_resamples) || global_resamples < 1000L) {
  stopf("--global-resamples must be an integer >= 1000.")
}
if (!is.finite(random_seed)) stopf("--random-seed must be an integer.")

manifest_path <- normalizePath(manifest_arg, winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(output_dir_arg, winslash = "/", mustWork = FALSE)
temp_dir <- normalizePath(temp_dir_arg, winslash = "/", mustWork = FALSE)
regionalization_dir <- normalizePath(
  regionalization_dir_arg, winslash = "/", mustWork = TRUE
)

path_key <- function(path, must_work = FALSE) {
  tolower(gsub("/+$", "", normalizePath(
    path, winslash = "/", mustWork = must_work
  )))
}
root_like <- function(path) {
  key <- gsub("\\\\", "/", path)
  identical(key, "/") ||
    grepl("^[a-z]:/?$", key, ignore.case = TRUE) ||
    grepl("^//[^/]+/[^/]+/?$", key)
}
is_descendant <- function(path, parent) {
  path_norm <- paste0(path_key(path), "/")
  parent_norm <- paste0(path_key(parent), "/")
  startsWith(path_norm, parent_norm) && !identical(path_norm, parent_norm)
}

expected_temp_root <- normalizePath(
  "E:/MoFuSS_Active", winslash = "/", mustWork = FALSE
)
if (!is_descendant(temp_dir, expected_temp_root)) {
  stopf("Temporary work must be below E:/MoFuSS_Active: %s", temp_dir)
}
if (root_like(output_dir) || root_like(dirname(output_dir)) ||
    !identical(tolower(basename(output_dir)), "manuscript_outputs")) {
  stopf("Output must be a non-root directory named manuscript_outputs: %s", output_dir)
}
if (is_descendant(output_dir, repository_root)) {
  stopf("Global manuscript outputs must not be stored in the source repository: %s", output_dir)
}

dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(temp_dir)) stopf("Could not create temporary directory: %s", temp_dir)
terra::terraOptions(tempdir = temp_dir)

read_csv_required <- function(path, label) {
  if (!file.exists(path)) stopf("Missing %s: %s", label, path)
  as.data.frame(suppressMessages(readr::read_csv(
    path, show_col_types = FALSE, name_repair = "minimal", progress = FALSE
  )))
}
require_columns <- function(x, fields, label) {
  missing <- setdiff(fields, names(x))
  if (length(missing)) {
    stopf("%s is missing columns: %s", label, paste(missing, collapse = ", "))
  }
}
select_one <- function(paths, label) {
  hits <- unique(normalizePath(paths[file.exists(paths)], winslash = "/", mustWork = TRUE))
  if (length(hits) != 1L) stopf("Expected exactly one %s; found %d.", label, length(hits))
  hits[[1L]]
}
same_number <- function(a, b, tolerance = 1e-7) {
  isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tolerance))
}

manifest <- read_csv_required(manifest_path, "global analysis manifest")
require_columns(
  manifest,
  c(
    "include", "analysis_id", "subregion_number", "subregion_name",
    "macroregion", "partition_status", "expected_country_count", "analysis_root"
  ),
  "Global analysis manifest"
)
manifest$include <- as.logical(manifest$include)
if (anyNA(manifest$include)) stopf("Manifest include values must be TRUE or FALSE.")
manifest <- manifest[manifest$include, , drop = FALSE]
if (!nrow(manifest)) stopf("Manifest contains no included analysis roots.")
manifest$subregion_number <- ifelse(
  is.na(manifest$subregion_number), "", as.character(manifest$subregion_number)
)
if (any(!nzchar(trimws(manifest$analysis_id))) || anyDuplicated(manifest$analysis_id)) {
  stopf("Included manifest analysis_id values must be nonempty and unique.")
}
manifest$analysis_root <- vapply(manifest$analysis_root, function(path) {
  normalizePath(path, winslash = "/", mustWork = TRUE)
}, character(1))
if (anyDuplicated(vapply(manifest$analysis_root, path_key, character(1), TRUE))) {
  stopf("The same analysis_root appears more than once in the manifest.")
}
manifest$expected_country_count <- suppressWarnings(as.integer(manifest$expected_country_count))
if (any(!is.finite(manifest$expected_country_count)) ||
    any(manifest$expected_country_count < 1L)) {
  stopf("Manifest expected_country_count values must be positive integers.")
}
if (run_mode == "strict" && any(manifest$partition_status != "final")) {
  stopf("Strict mode accepts only manifest rows with partition_status=final.")
}

regionalization_paths <- Sys.glob(file.path(
  regionalization_dir, "subregions*_M67_GME_V2.csv"
))
if (!length(regionalization_paths)) {
  stopf("No canonical M67/GME regionalization CSVs found in %s", regionalization_dir)
}
regionalization_optional_columns <- c(
  "RunCode", "CandidateID", "CandidateRegionID", "ImporterV", "Status"
)
regionalization <- do.call(rbind, lapply(regionalization_paths, function(path) {
  x <- read_csv_required(path, basename(path))
  require_columns(x, c("Subregion", "GID_0", "NAME_0"), basename(path))
  for (column in regionalization_optional_columns) {
    if (!column %in% names(x)) x[[column]] <- NA_character_
  }
  x$regionalization_file <- basename(path)
  x[c(
    "Subregion", "GID_0", "NAME_0", regionalization_optional_columns,
    "regionalization_file"
  )]
}))
if (anyDuplicated(regionalization$GID_0)) {
  duplicates <- unique(regionalization$GID_0[duplicated(regionalization$GID_0)])
  stopf("Canonical regionalization repeats country ISO codes: %s", paste(duplicates, collapse = ", "))
}

required_per_run_columns <- c(
  "country_iso", "country_name", "regrowth_mode", "run_id",
  "period_start_year", "period_end_year", unname(METRIC_FIELDS),
  "all_invariants_ok"
)

analysis_data <- vector("list", nrow(manifest))
input_inventory <- vector("list", nrow(manifest))
for (i in seq_len(nrow(manifest))) {
  row <- manifest[i, , drop = FALSE]
  agb_dir <- file.path(row$analysis_root[[1L]], "agb_decomposition")
  country_candidates <- Sys.glob(file.path(
    agb_dir, "agb_decomposition_by_country_per_run_*.csv"
  ))
  source_kind <- if (length(country_candidates)) "country_per_run" else "single_country_fallback"
  if (source_kind == "country_per_run") {
    per_run_path <- select_one(country_candidates, sprintf(
      "%s country per-run decomposition file", row$analysis_id[[1L]]
    ))
  } else {
    if (row$expected_country_count[[1L]] != 1L) {
      stopf(
        "%s lacks country-per-run data and is not a one-country manifest row.",
        row$analysis_id[[1L]]
      )
    }
    per_run_path <- select_one(
      Sys.glob(file.path(agb_dir, "agb_decomposition_per_run_*.csv")),
      sprintf("%s regional per-run decomposition file", row$analysis_id[[1L]])
    )
  }

  x <- read_csv_required(per_run_path, sprintf("%s per-run data", row$analysis_id[[1L]]))
  require_columns(x, required_per_run_columns, basename(per_run_path))
  if (!all(as.logical(x$all_invariants_ok))) {
    stopf("%s contains a failed Stage 3 invariant.", row$analysis_id[[1L]])
  }
  x$country_iso <- trimws(as.character(x$country_iso))
  x$country_name <- trimws(as.character(x$country_name))
  x$regrowth_mode <- trimws(as.character(x$regrowth_mode))
  x$run_id <- suppressWarnings(as.integer(x$run_id))
  x$period_start_year <- suppressWarnings(as.integer(x$period_start_year))
  x$period_end_year <- suppressWarnings(as.integer(x$period_end_year))
  if (any(!nzchar(x$country_iso)) || any(!nzchar(x$country_name)) ||
      any(!is.finite(x$run_id))) {
    stopf("%s contains missing country identities or run IDs.", row$analysis_id[[1L]])
  }
  if (!setequal(unique(x$regrowth_mode), CONFIGURATION_ORDER)) {
    stopf("%s does not contain exactly capped and uncapped configurations.", row$analysis_id[[1L]])
  }
  countries <- unique(x[c("country_iso", "country_name")])
  if (nrow(countries) != row$expected_country_count[[1L]]) {
    stopf(
      "%s expected %d countries but Stage 3 contains %d.",
      row$analysis_id[[1L]], row$expected_country_count[[1L]], nrow(countries)
    )
  }

  group_keys <- interaction(x$country_iso, x$regrowth_mode, drop = TRUE)
  groups <- split(x, group_keys)
  group_run_ids <- lapply(groups, function(rows) sort(unique(rows$run_id)))
  reference_run_ids <- group_run_ids[[1L]]
  if (any(vapply(group_run_ids, function(ids) {
    !identical(ids, reference_run_ids)
  }, logical(1)))) {
    stopf("%s has incomplete country/configuration run coverage.", row$analysis_id[[1L]])
  }
  if (length(reference_run_ids) < min_runs) {
    stopf(
      "%s has %d Monte Carlo runs; at least %d are required.",
      row$analysis_id[[1L]], length(reference_run_ids), min_runs
    )
  }

  numeric_fields <- unname(METRIC_FIELDS)
  for (field in numeric_fields) {
    x[[field]] <- suppressWarnings(as.numeric(x[[field]]))
    if (any(!is.finite(x[[field]]))) {
      stopf("%s contains non-finite values in %s.", row$analysis_id[[1L]], field)
    }
  }
  harvest_ok <- mapply(
    same_number,
    x$period_avoided_loss_tco2e + x$period_regrowth_tco2e,
    x$agb_avoided_stage2_tco2e,
    MoreArgs = list(tolerance = 1e-7)
  )
  total_ok <- mapply(
    same_number,
    x$agb_avoided_stage2_tco2e + x$enduse_avoided_tco2e,
    x$total_avoided_tco2e,
    MoreArgs = list(tolerance = 1e-7)
  )
  if (!all(harvest_ok) || !all(total_ok)) {
    stopf("%s contribution components do not reconcile.", row$analysis_id[[1L]])
  }

  x$analysis_id <- row$analysis_id[[1L]]
  x$subregion_number <- row$subregion_number[[1L]]
  x$subregion_name <- row$subregion_name[[1L]]
  x$macroregion <- row$macroregion[[1L]]
  x$partition_status <- row$partition_status[[1L]]
  analysis_data[[i]] <- x[c(
    required_per_run_columns,
    "analysis_id", "subregion_number", "subregion_name", "macroregion",
    "partition_status"
  )]

  raster_root <- file.path(
    row$analysis_root[[1L]], "manuscript_outputs", "rasters", "mc_all"
  )
  capped_rasters <- Sys.glob(file.path(raster_root, "*_capped_total_mean_tco2e.tif"))
  uncapped_rasters <- Sys.glob(file.path(raster_root, "*_uncapped_total_mean_tco2e.tif"))
  input_inventory[[i]] <- data.frame(
    analysis_id = row$analysis_id[[1L]],
    subregion_name = row$subregion_name[[1L]],
    partition_status = row$partition_status[[1L]],
    analysis_root = row$analysis_root[[1L]],
    per_run_source_kind = source_kind,
    per_run_path = per_run_path,
    country_count = nrow(countries),
    run_count = length(reference_run_ids),
    capped_total_mean_raster = if (length(capped_rasters) == 1L) capped_rasters[[1L]] else "",
    uncapped_total_mean_raster = if (length(uncapped_rasters) == 1L) uncapped_rasters[[1L]] else "",
    spatial_rasters_complete = length(capped_rasters) == 1L && length(uncapped_rasters) == 1L,
    stringsAsFactors = FALSE
  )
}

per_run <- do.call(rbind, analysis_data)
input_inventory <- do.call(rbind, input_inventory)
rownames(per_run) <- NULL
rownames(input_inventory) <- NULL

period_starts <- unique(per_run$period_start_year)
period_ends <- unique(per_run$period_end_year)
if (length(period_starts) != 1L || length(period_ends) != 1L ||
    period_ends[[1L]] < period_starts[[1L]]) {
  stopf("Included analyses do not share one valid reporting period.")
}
period_start <- period_starts[[1L]]
period_end <- period_ends[[1L]]
reporting_years <- period_end - period_start + 1L
period_tag <- sprintf("%d-%d", period_start, period_end)

source_countries <- unique(per_run[c(
  "country_iso", "country_name", "analysis_id", "subregion_name",
  "macroregion", "partition_status"
)])
if (anyDuplicated(source_countries$country_iso)) {
  duplicates <- unique(source_countries$country_iso[duplicated(source_countries$country_iso)])
  stopf("Included analysis roots overlap country ISO codes: %s", paste(duplicates, collapse = ", "))
}
unknown_countries <- setdiff(source_countries$country_iso, regionalization$GID_0)
if (length(unknown_countries)) {
  stopf("Included countries are absent from the canonical regionalization: %s", paste(unknown_countries, collapse = ", "))
}

coverage <- merge(
  regionalization[c(
    "GID_0", "NAME_0", "Subregion", "regionalization_file",
    intersect(c("RunCode", "CandidateID", "CandidateRegionID", "ImporterV", "Status"), names(regionalization))
  )],
  source_countries,
  by.x = "GID_0", by.y = "country_iso", all.x = TRUE, sort = FALSE
)
coverage$coverage_status <- ifelse(
  is.na(coverage$analysis_id),
  "missing",
  ifelse(coverage$Subregion == coverage$subregion_name, "included", "subregion_mismatch")
)
coverage <- coverage[order(coverage$regionalization_file, coverage$Subregion, coverage$GID_0), ]
rownames(coverage) <- NULL
coverage_complete <- all(coverage$coverage_status == "included")
if (run_mode == "strict" && !coverage_complete) {
  counts <- table(coverage$coverage_status)
  stopf(
    "Strict coverage failed: %s",
    paste(names(counts), as.integer(counts), sep = "=", collapse = ", ")
  )
}

# Annual per-run country values ----

country_draws <- per_run[c(
  "analysis_id", "subregion_number", "subregion_name", "macroregion",
  "partition_status", "country_iso", "country_name", "regrowth_mode",
  "run_id"
)]
for (metric in names(METRIC_FIELDS)) {
  country_draws[[metric]] <-
    as.numeric(per_run[[METRIC_FIELDS[[metric]]]]) / (1e6 * reporting_years)
}
names(country_draws)[names(country_draws) == "regrowth_mode"] <- "configuration"

summarise_draws <- function(x, identity_columns) {
  key_parts <- lapply(x[identity_columns], function(values) {
    match(values, unique(values))
  })
  keys <- interaction(key_parts, drop = TRUE, lex.order = TRUE)
  groups <- split(x, keys)
  rows <- lapply(groups, function(group) {
    identity <- group[1L, identity_columns, drop = FALSE]
    metric_rows <- lapply(names(METRIC_FIELDS), function(metric) {
      values <- as.numeric(group[[metric]])
      cbind(
        identity,
        data.frame(
          metric = metric,
          metric_label = METRIC_LABELS[[metric]],
          unit = "MtCO2e yr^-1",
          draws = length(values),
          mean = mean(values),
          sd = stats::sd(values),
          empirical_p025 = as.numeric(stats::quantile(values, 0.025, names = FALSE)),
          empirical_p975 = as.numeric(stats::quantile(values, 0.975, names = FALSE)),
          stringsAsFactors = FALSE
        )
      )
    })
    do.call(rbind, metric_rows)
  })
  answer <- do.call(rbind, rows)
  rownames(answer) <- NULL
  answer
}

country_summary <- summarise_draws(
  country_draws,
  c(
    "analysis_id", "subregion_number", "subregion_name", "macroregion",
    "country_iso", "country_name", "configuration"
  )
)
country_summary <- country_summary[
  order(
    country_summary$subregion_name, country_summary$country_iso,
    match(country_summary$configuration, CONFIGURATION_ORDER),
    match(country_summary$metric, names(METRIC_FIELDS))
  ),
]

analysis_draws <- stats::aggregate(
  country_draws[names(METRIC_FIELDS)],
  by = country_draws[c(
    "analysis_id", "subregion_number", "subregion_name", "macroregion",
    "partition_status", "configuration", "run_id"
  )],
  FUN = sum
)

build_coherent_analysis_draws <- function(x, method, resamples, seed) {
  identity_columns <- c(
    "analysis_id", "subregion_number", "subregion_name", "macroregion",
    "partition_status"
  )
  analysis_groups <- split(x, x$analysis_id)
  run_sets <- lapply(analysis_groups, function(rows) {
    by_configuration <- split(rows$run_id, rows$configuration)
    if (!setequal(names(by_configuration), CONFIGURATION_ORDER)) {
      stopf(
        "Analysis %s must contain both capped and uncapped configurations.",
        rows$analysis_id[[1L]]
      )
    }
    by_configuration <- lapply(by_configuration, function(ids) sort(unique(ids)))
    if (!identical(by_configuration[["capped"]], by_configuration[["uncapped"]])) {
      stopf(
        "Analysis %s must use identical run IDs for capped and uncapped configurations.",
        rows$analysis_id[[1L]]
      )
    }
    by_configuration[["capped"]]
  })

  if (method == "aligned") {
    if (length(run_sets) > 1L && any(vapply(run_sets[-1L], function(ids) {
      !identical(ids, run_sets[[1L]])
    }, logical(1)))) {
      stopf("Aligned combination requires identical run IDs in every analysis root.")
    }
    answer <- x
    answer$combination_method <- "aligned_run_ids"
  } else {
    resampled <- lapply(seq_along(analysis_groups), function(analysis_index) {
      rows <- analysis_groups[[analysis_index]]
      source_ids <- run_sets[[analysis_index]]
      set.seed(as.integer(seed + analysis_index - 1L))
      sampled_ids <- sample(source_ids, size = resamples, replace = TRUE)
      configuration_rows <- lapply(CONFIGURATION_ORDER, function(configuration) {
        source <- rows[rows$configuration == configuration, , drop = FALSE]
        source <- source[match(sampled_ids, source$run_id), , drop = FALSE]
        if (anyNA(source$run_id)) {
          stopf(
            "Failed to match resampled run IDs for analysis %s (%s).",
            rows$analysis_id[[1L]], configuration
          )
        }
        source$run_id <- seq_len(resamples)
        source$combination_method <- "independent_analysis_resampling"
        source
      })
      do.call(rbind, configuration_rows)
    })
    answer <- do.call(rbind, resampled)
  }

  answer <- answer[c(
    identity_columns, "configuration", "run_id", "combination_method",
    names(METRIC_FIELDS)
  )]
  answer <- answer[order(
    answer$analysis_id,
    match(answer$configuration, CONFIGURATION_ORDER),
    answer$run_id
  ), ]
  rownames(answer) <- NULL
  answer
}

coherent_analysis_draws <- build_coherent_analysis_draws(
  analysis_draws, mc_combination, global_resamples, random_seed
)

subregion_draws <- stats::aggregate(
  coherent_analysis_draws[names(METRIC_FIELDS)],
  by = coherent_analysis_draws[c(
    "subregion_number", "subregion_name", "macroregion", "configuration",
    "run_id", "combination_method"
  )],
  FUN = sum
)
subregion_draws <- subregion_draws[c(
  "subregion_number", "subregion_name", "macroregion", "configuration",
  "run_id", "combination_method", names(METRIC_FIELDS)
)]
subregion_draws <- subregion_draws[order(
  subregion_draws$subregion_name,
  match(subregion_draws$configuration, CONFIGURATION_ORDER),
  subregion_draws$run_id
), ]
rownames(subregion_draws) <- NULL
subregion_summary <- summarise_draws(
  subregion_draws,
  c("subregion_number", "subregion_name", "macroregion", "configuration")
)
subregion_summary <- subregion_summary[
  order(
    subregion_summary$subregion_name,
    match(subregion_summary$configuration, CONFIGURATION_ORDER),
    match(subregion_summary$metric, names(METRIC_FIELDS))
  ),
]

global_input <- coherent_analysis_draws
global_input$global_scope <- "Global South"
global_draws <- stats::aggregate(
  global_input[names(METRIC_FIELDS)],
  by = global_input[c(
    "global_scope", "configuration", "run_id", "combination_method"
  )],
  FUN = sum
)
global_draws <- global_draws[c(
  "global_scope", "configuration", "run_id", "combination_method",
  names(METRIC_FIELDS)
)]
global_draws <- global_draws[order(
  match(global_draws$configuration, CONFIGURATION_ORDER),
  global_draws$run_id
), ]
rownames(global_draws) <- NULL
global_summary <- summarise_draws(
  global_draws,
  c("global_scope", "configuration")
)
global_summary <- global_summary[
  order(
    match(global_summary$configuration, CONFIGURATION_ORDER),
    match(global_summary$metric, names(METRIC_FIELDS))
  ),
]

global_harvest_check <- merge(
  global_summary[global_summary$metric == "avoided_loss", c("configuration", "mean")],
  global_summary[global_summary$metric == "regrowth", c("configuration", "mean")],
  by = "configuration", suffixes = c("_loss", "_regrowth")
)
global_harvest_check <- merge(
  global_harvest_check,
  global_summary[global_summary$metric == "harvest", c("configuration", "mean")],
  by = "configuration"
)
if (!all(mapply(
  same_number,
  global_harvest_check$mean_loss + global_harvest_check$mean_regrowth,
  global_harvest_check$mean,
  MoreArgs = list(tolerance = 1e-8)
))) {
  stopf("Global mean Harvest / AGB does not reconcile to its components.")
}

subregion_coverage <- stats::aggregate(
  coverage$coverage_status == "included",
  by = coverage[c("Subregion")],
  FUN = sum
)
names(subregion_coverage) <- c("subregion_name", "included_countries")
subregion_expected <- stats::aggregate(
  rep(1L, nrow(coverage)),
  by = coverage[c("Subregion")],
  FUN = sum
)
names(subregion_expected) <- c("subregion_name", "expected_countries")
subregion_coverage <- merge(
  subregion_coverage, subregion_expected, by = "subregion_name", all = TRUE
)

validation_summary <- data.frame(
  check = c(
    "run_mode", "mc_combination", "period", "included_analysis_roots",
    "included_countries", "expected_countries", "coverage_complete",
    "spatial_analysis_roots_complete", "minimum_runs", "global_resamples",
    "random_seed"
  ),
  value = c(
    run_mode, mc_combination, period_tag, nrow(manifest),
    nrow(source_countries), nrow(regionalization), coverage_complete,
    sum(input_inventory$spatial_rasters_complete), min_runs,
    if (mc_combination == "independent") global_resamples else NA_integer_,
    random_seed
  ),
  stringsAsFactors = FALSE
)

# Output preparation ----

prepare_output_dir <- function(path, allow_overwrite) {
  target <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!identical(tolower(basename(target)), "manuscript_outputs") ||
      root_like(target) || root_like(dirname(target))) {
    stopf("Refusing global output rebuild at unsafe path: %s", target)
  }
  if (file.exists(target) && !dir.exists(target)) {
    stopf("Output path exists and is not a directory: %s", target)
  }
  if (dir.exists(target)) {
    existing <- list.files(target, all.files = TRUE, no.. = TRUE)
    if (length(existing) && !allow_overwrite) {
      stopf("Output is not empty; use --overwrite to replace it: %s", target)
    }
    if (allow_overwrite) {
      resolved <- normalizePath(target, winslash = "/", mustWork = TRUE)
      if (!identical(path_key(resolved, TRUE), path_key(target, TRUE))) {
        stopf("Refusing rebuild through a redirected output path: %s", target)
      }
      message("Removing existing global manuscript output folder: ", target)
      status <- unlink(target, recursive = TRUE, force = TRUE)
      if (status != 0L || file.exists(target)) {
        stopf("Could not remove existing global manuscript output folder: %s", target)
      }
    }
  }
  for (subdir in c("figures/main", "figures/supplement", "tables", "validation")) {
    dir.create(file.path(target, subdir), recursive = TRUE, showWarnings = FALSE)
  }
}

prepare_output_dir(output_dir, overwrite)
output_prefix <- if (run_mode == "partial") "partial_" else ""

table_paths <- c(
  country_per_run = file.path(output_dir, "tables", paste0(output_prefix, "global_south_country_per_run.csv")),
  country_summary = file.path(output_dir, "tables", paste0(output_prefix, "global_south_country_mc_all.csv")),
  subregion_per_draw = file.path(output_dir, "tables", paste0(output_prefix, "global_south_subregion_per_draw.csv")),
  subregion_summary = file.path(output_dir, "tables", paste0(output_prefix, "global_south_subregion_mc_all.csv")),
  global_per_draw = file.path(output_dir, "tables", paste0(output_prefix, "global_south_per_draw.csv")),
  global_summary = file.path(output_dir, "tables", paste0(output_prefix, "global_south_mc_all.csv"))
)
validation_paths <- c(
  coverage = file.path(output_dir, "validation", paste0(output_prefix, "coverage_report.csv")),
  inventory = file.path(output_dir, "validation", paste0(output_prefix, "input_inventory.csv")),
  summary = file.path(output_dir, "validation", paste0(output_prefix, "validation_summary.csv")),
  manifest = file.path(output_dir, "validation", paste0(output_prefix, "manifest_snapshot.csv"))
)

readr::write_csv(country_draws, table_paths[["country_per_run"]], na = "")
readr::write_csv(country_summary, table_paths[["country_summary"]], na = "")
readr::write_csv(subregion_draws, table_paths[["subregion_per_draw"]], na = "")
readr::write_csv(subregion_summary, table_paths[["subregion_summary"]], na = "")
readr::write_csv(global_draws, table_paths[["global_per_draw"]], na = "")
readr::write_csv(global_summary, table_paths[["global_summary"]], na = "")
readr::write_csv(coverage, validation_paths[["coverage"]], na = "")
readr::write_csv(input_inventory, validation_paths[["inventory"]], na = "")
readr::write_csv(validation_summary, validation_paths[["summary"]], na = "")
readr::write_csv(manifest, validation_paths[["manifest"]], na = "")

# Ranked subregion figure ----

write_ranked_subregion_figure <- function(
  summary, coverage_counts, path, mode, covered_countries, expected_countries,
  period_tag
) {
  total <- summary[summary$metric == "total", , drop = FALSE]
  total <- merge(total, coverage_counts, by = "subregion_name", all.x = TRUE)
  total$display_label <- if (mode == "partial") {
    sprintf(
      "%s [%d/%d countries]",
      total$subregion_name, total$included_countries, total$expected_countries
    )
  } else {
    total$subregion_name
  }
  ranking <- total[total$configuration == "uncapped", , drop = FALSE]
  ranking <- ranking[order(ranking$mean, decreasing = TRUE), , drop = FALSE]
  subregion_order <- ranking$display_label
  n_subregions <- length(subregion_order)
  if (!n_subregions) stopf("No subregion totals are available for plotting.")

  values <- c(0, total$empirical_p025, total$empirical_p975, total$mean)
  raw_limits <- range(values, finite = TRUE)
  raw_span <- diff(raw_limits)
  if (!is.finite(raw_span) || raw_span <= 0) raw_span <- 1
  plot_limits <- c(
    raw_limits[[1L]] - 0.05 * raw_span,
    raw_limits[[2L]] + 0.18 * raw_span
  )
  tick_values <- pretty(raw_limits, n = 6L)
  tick_values <- tick_values[
    tick_values >= plot_limits[[1L]] & tick_values <= plot_limits[[2L]]
  ]

  height_in <- max(5.8, 3.6 + n_subregions * 0.43)
  png_args <- list(
    filename = path, width = 12.5, height = height_in,
    units = "in", res = FIGURE_DPI, pointsize = 10, bg = "white"
  )
  if (isTRUE(capabilities("cairo"))) png_args$type <- "cairo"
  do.call(grDevices::png, png_args)
  on.exit(grDevices::dev.off(), add = TRUE)

  colours <- c(
    capped = "#0072B2", uncapped = "#009E73", grid = "#E1E6EB",
    border = "#9AA5B1", text = "#1F2933", muted = "#52616B"
  )
  label_width <- max(nchar(subregion_order, type = "width"))
  left_margin <- max(12, min(24, 5.5 + 0.34 * label_width))
  graphics::par(
    mar = c(5.0, left_margin, 5.4, 1.5), family = "sans",
    xaxs = "i", yaxs = "i"
  )
  graphics::plot.new()
  graphics::plot.window(
    xlim = plot_limits, ylim = c(0.45, n_subregions + 0.55),
    xaxs = "i", yaxs = "i"
  )
  y_positions <- rev(seq_len(n_subregions))
  graphics::abline(v = tick_values, col = colours[["grid"]], lwd = 0.8)
  graphics::abline(h = y_positions, col = colours[["grid"]], lwd = 0.55)
  graphics::abline(v = 0, col = colours[["border"]], lwd = 1.0)

  offsets <- c(capped = 0.14, uncapped = -0.14)
  for (configuration in CONFIGURATION_ORDER) {
    panel <- total[total$configuration == configuration, , drop = FALSE]
    panel <- panel[match(subregion_order, panel$display_label), , drop = FALSE]
    if (anyNA(panel$display_label)) {
      stopf("Subregion figure configurations do not contain the same groups.")
    }
    y <- y_positions + offsets[[configuration]]
    graphics::segments(
      panel$empirical_p025, y, panel$empirical_p975, y,
      col = colours[[configuration]], lwd = 1.8
    )
    graphics::segments(
      panel$empirical_p025, y - 0.06, panel$empirical_p025, y + 0.06,
      col = colours[[configuration]], lwd = 1.2
    )
    graphics::segments(
      panel$empirical_p975, y - 0.06, panel$empirical_p975, y + 0.06,
      col = colours[[configuration]], lwd = 1.2
    )
    graphics::points(
      panel$mean, y, pch = if (configuration == "capped") 21 else 22,
      bg = colours[[configuration]], col = "white", cex = 1.15, lwd = 0.8
    )
    label_x <- panel$empirical_p975 + 0.012 * raw_span
    graphics::text(
      label_x, y, formatC(panel$mean, format = "f", digits = 1L),
      adj = c(0, 0.5), cex = 0.72, font = 2, col = colours[["text"]]
    )
  }

  graphics::axis(
    1, at = tick_values,
    labels = format(tick_values, trim = TRUE, scientific = FALSE),
    cex.axis = 0.78, col = colours[["border"]], col.axis = colours[["text"]],
    tck = -0.02
  )
  graphics::axis(
    2, at = y_positions, labels = subregion_order,
    las = 1, tick = FALSE, cex.axis = 0.73,
    col.axis = colours[["text"]], line = -0.35
  )
  graphics::box(col = colours[["border"]], lwd = 0.8)
  graphics::mtext(
    sprintf("Annual avoided emissions by wood-energy subregion, %s", period_tag),
    side = 3, line = 3.25, font = 2, cex = 1.18, col = colours[["text"]]
  )
  coverage_label <- if (mode == "partial") {
    sprintf(
      "PRELIMINARY PARTIAL COVERAGE: %d of %d countries",
      covered_countries, expected_countries
    )
  } else {
    sprintf("Complete Global South coverage: %d countries", expected_countries)
  }
  graphics::mtext(
    coverage_label, side = 3, line = 1.75,
    cex = 0.82, col = colours[["muted"]]
  )
  graphics::legend(
    "topright", inset = c(0, -0.11), xpd = NA, horiz = TRUE, bty = "n",
    legend = c("Capped", "Uncapped"),
    pch = c(21, 22), pt.bg = colours[CONFIGURATION_ORDER],
    col = "white", pt.cex = 1.2, cex = 0.80
  )
  graphics::mtext(
    expression(paste("Mean annual avoided emissions (MtCO"[2], "e ", yr^{-1}, ")")),
    side = 1, line = 3.25, cex = 0.88, col = colours[["text"]]
  )
  graphics::mtext(
    "Points are means; whiskers are empirical 95% intervals from the configured Monte Carlo combination.",
    side = 1, line = 4.25, cex = 0.65, col = colours[["muted"]]
  )
  invisible(path)
}

ranked_figure_path <- file.path(
  output_dir, "figures", "main",
  paste0(output_prefix, "figure_global_south_subregion_totals_mc_all.png")
)
write_ranked_subregion_figure(
  subregion_summary, subregion_coverage, ranked_figure_path, run_mode,
  nrow(source_countries), nrow(regionalization), period_tag
)

# Global contribution figure ----

write_global_contribution_figure <- function(
  summary, path, mode, covered_countries, expected_countries, period_tag,
  mc_combination
) {
  mean_value <- function(configuration, metric) {
    hit <- summary[
      summary$configuration == configuration & summary$metric == metric,
      , drop = FALSE
    ]
    if (nrow(hit) != 1L) stopf("Global summary is missing %s %s.", configuration, metric)
    hit
  }
  panel_data <- do.call(rbind, lapply(CONFIGURATION_ORDER, function(configuration) {
    loss <- mean_value(configuration, "avoided_loss")
    regrowth <- mean_value(configuration, "regrowth")
    harvest <- mean_value(configuration, "harvest")
    enduse <- mean_value(configuration, "enduse")
    total <- mean_value(configuration, "total")
    data.frame(
      configuration = configuration,
      avoided_loss = loss$mean,
      regrowth = regrowth$mean,
      harvest = harvest$mean,
      enduse = enduse$mean,
      total = total$mean,
      total_p025 = total$empirical_p025,
      total_p975 = total$empirical_p975,
      stringsAsFactors = FALSE
    )
  }))
  endpoints <- c(
    0, panel_data$avoided_loss, panel_data$harvest, panel_data$total,
    panel_data$total_p025, panel_data$total_p975
  )
  raw_limits <- range(endpoints, finite = TRUE)
  raw_span <- diff(raw_limits)
  if (!is.finite(raw_span) || raw_span <= 0) raw_span <- 1
  plot_limits <- c(
    raw_limits[[1L]] - 0.05 * raw_span,
    raw_limits[[2L]] + 0.16 * raw_span
  )
  tick_values <- pretty(raw_limits, n = 5L)
  tick_values <- tick_values[
    tick_values >= plot_limits[[1L]] & tick_values <= plot_limits[[2L]]
  ]

  png_args <- list(
    filename = path, width = 11.5, height = 5.5,
    units = "in", res = FIGURE_DPI, pointsize = 10, bg = "white"
  )
  if (isTRUE(capabilities("cairo"))) png_args$type <- "cairo"
  do.call(grDevices::png, png_args)
  on.exit(grDevices::dev.off(), add = TRUE)

  colours <- c(
    avoided_loss = "#0072B2", regrowth = "#009E73", enduse = "#D55E00",
    total = "#172B4D", uncertainty = "#596775", grid = "#E1E6EB",
    border = "#9AA5B1", text = "#1F2933", muted = "#52616B"
  )
  graphics::layout(
    matrix(c(1L, 1L, 2L, 3L), nrow = 2L, byrow = TRUE),
    heights = c(1.2, 3.3)
  )
  graphics::par(oma = c(3.1, 0.4, 0.2, 0.4), family = "sans")
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  graphics::text(
    0.5, 0.88,
    sprintf("Included Global South contributions, %s", period_tag),
    font = 2, cex = 1.25, col = colours[["total"]]
  )
  subtitle <- if (mode == "partial") {
    sprintf(
      "PRELIMINARY PARTIAL COVERAGE: %d of %d countries; %s analysis combination",
      covered_countries, expected_countries, mc_combination
    )
  } else {
    sprintf("Complete %d-country coverage; %s analysis combination", expected_countries, mc_combination)
  }
  graphics::text(0.5, 0.62, subtitle, cex = 0.82, col = colours[["muted"]])
  graphics::legend(
    "bottom", horiz = TRUE, bty = "n", xpd = NA, cex = 0.76,
    legend = c(
      "Avoided AGB loss", "Enhanced regrowth", "End-use adjustment",
      "Total mean", "Empirical 95% interval"
    ),
    fill = c(colours[["avoided_loss"]], colours[["regrowth"]], NA, NA, NA),
    border = NA,
    lty = c(NA, NA, 1, NA, 1),
    lwd = c(NA, NA, 3, NA, 1.4),
    pch = c(NA, NA, NA, 21, NA),
    col = c(NA, NA, colours[["enduse"]], colours[["total"]], colours[["uncertainty"]]),
    pt.bg = c(NA, NA, NA, colours[["total"]], NA)
  )

  for (configuration in CONFIGURATION_ORDER) {
    panel <- panel_data[panel_data$configuration == configuration, , drop = FALSE]
    graphics::par(mar = c(3.0, 2.0, 2.8, 1.0), xaxs = "i", yaxs = "i")
    graphics::plot.new()
    graphics::plot.window(xlim = plot_limits, ylim = c(0.55, 1.45), xaxs = "i", yaxs = "i")
    graphics::abline(v = tick_values, col = colours[["grid"]], lwd = 0.8)
    graphics::abline(v = 0, col = colours[["border"]], lwd = 1.0)
    contribution_y <- 0.86
    total_y <- 1.18
    graphics::segments(
      panel$total_p025, total_y, panel$total_p975, total_y,
      col = colours[["uncertainty"]], lwd = 1.5
    )
    graphics::segments(
      c(panel$total_p025, panel$total_p975), total_y - 0.06,
      c(panel$total_p025, panel$total_p975), total_y + 0.06,
      col = colours[["uncertainty"]], lwd = 1.2
    )
    graphics::rect(
      min(0, panel$avoided_loss), contribution_y - 0.10,
      max(0, panel$avoided_loss), contribution_y + 0.10,
      col = colours[["avoided_loss"]], border = "white", lwd = 0.6
    )
    regrowth_end <- panel$avoided_loss + panel$regrowth
    graphics::rect(
      min(panel$avoided_loss, regrowth_end), contribution_y - 0.10,
      max(panel$avoided_loss, regrowth_end), contribution_y + 0.10,
      col = colours[["regrowth"]], border = "white", lwd = 0.6
    )
    if (!same_number(panel$harvest, panel$total, tolerance = 1e-12)) {
      graphics::arrows(
        panel$harvest, contribution_y, panel$total, contribution_y,
        length = 0.075, angle = 24, code = 2L,
        col = colours[["enduse"]], lwd = 3.0
      )
    }
    graphics::points(
      panel$total, total_y, pch = 21, cex = 1.05,
      col = "white", bg = colours[["total"]], lwd = 0.8
    )
    graphics::text(
      max(panel$total, panel$total_p975) + 0.012 * raw_span,
      total_y, formatC(panel$total, format = "f", digits = 1L),
      adj = c(0, 0.5), cex = 0.78, font = 2, col = colours[["text"]]
    )
    graphics::axis(
      1, at = tick_values,
      labels = format(tick_values, trim = TRUE, scientific = FALSE),
      cex.axis = 0.74, col = colours[["border"]], col.axis = colours[["text"]],
      tck = -0.025
    )
    graphics::box(col = colours[["border"]], lwd = 0.8)
    graphics::title(
      main = tools::toTitleCase(configuration), line = 1.0,
      font.main = 2, cex.main = 1.0, col.main = colours[["total"]]
    )
  }
  graphics::mtext(
    expression(paste("Annual avoided emissions (MtCO"[2], "e ", yr^{-1}, ")")),
    side = 1, outer = TRUE, line = 0.65, cex = 0.86, col = colours[["text"]]
  )
  graphics::mtext(
    "End-use arrows point from Harvest / AGB to Total; leftward arrows reduce avoided emissions.",
    side = 1, outer = TRUE, line = 2.0, cex = 0.65, col = colours[["muted"]]
  )
  invisible(path)
}

contribution_figure_path <- file.path(
  output_dir, "figures", "supplement",
  paste0(output_prefix, "figure_global_south_contributions_mc_all.png")
)
write_global_contribution_figure(
  global_summary, contribution_figure_path, run_mode,
  nrow(source_countries), nrow(regionalization), period_tag, mc_combination
)

# Global spatial figure ----

if (run_mode == "strict" && !all(input_inventory$spatial_rasters_complete)) {
  missing <- input_inventory$analysis_id[!input_inventory$spatial_rasters_complete]
  stopf(
    "Strict mode requires capped and uncapped MC-all Total mean rasters for: %s",
    paste(missing, collapse = ", ")
  )
}
spatial_inventory <- input_inventory[input_inventory$spatial_rasters_complete, , drop = FALSE]
if (!nrow(spatial_inventory)) {
  stopf("No included analysis root has complete MC-all Total mean rasters.")
}

make_display_intensity_raster <- function(path, reporting_years, max_cells = 60000L) {
  source <- terra::rast(path)
  if (terra::nlyr(source) != 1L) stopf("Expected one-layer Total mean raster: %s", path)
  factor <- max(1L, as.integer(ceiling(sqrt(terra::ncell(source) / max_cells))))
  cell_area <- terra::cellSize(source, unit = "km", mask = TRUE)
  annual_total <- source / reporting_years
  if (factor > 1L) {
    annual_total <- terra::aggregate(
      annual_total, fact = factor, fun = "sum", na.rm = TRUE
    )
    cell_area <- terra::aggregate(
      cell_area, fact = factor, fun = "sum", na.rm = TRUE
    )
  }
  intensity <- terra::ifel(cell_area > 0, annual_total / cell_area, NA)
  names(intensity) <- "annual_total_tco2e_km2"
  if (!terra::same.crs(intensity, MAP_DISPLAY_CRS)) {
    # Reproject only the intensive display surface. The underlying totals and
    # all tabular accounting remain in their native grids.
    intensity <- terra::project(intensity, MAP_DISPLAY_CRS, method = "bilinear")
  }
  intensity
}

display_rasters <- list()
for (i in seq_len(nrow(spatial_inventory))) {
  row <- spatial_inventory[i, , drop = FALSE]
  for (configuration in CONFIGURATION_ORDER) {
    path_column <- paste0(configuration, "_total_mean_raster")
    key <- paste(row$analysis_id[[1L]], configuration, sep = "::")
    display_rasters[[key]] <- make_display_intensity_raster(
      row[[path_column]][[1L]], reporting_years
    )
  }
}

spatial_quantiles <- unlist(lapply(display_rasters, function(raster) {
  as.numeric(terra::global(
    raster, stats::quantile, probs = c(0.02, 0.98), na.rm = TRUE
  )[1L, ])
}))
spatial_limit <- max(abs(spatial_quantiles), na.rm = TRUE)
if (!is.finite(spatial_limit) || spatial_limit <= 0) spatial_limit <- 1
spatial_scale <- c(-spatial_limit, spatial_limit)

write_global_map_figure <- function(
  rasters, summary, path, mode, mapped_countries, covered_countries,
  expected_countries, period_tag, scale
) {
  png_args <- list(
    filename = path, width = 13.5, height = 7.2,
    units = "in", res = FIGURE_DPI, pointsize = 10, bg = "white"
  )
  if (isTRUE(capabilities("cairo"))) png_args$type <- "cairo"
  do.call(grDevices::png, png_args)
  on.exit(grDevices::dev.off(), add = TRUE)

  colours <- grDevices::hcl.colors(255, "Blue-Red 3")
  text_colour <- "#1F2933"
  muted_colour <- "#52616B"
  border_colour <- "#8A98A6"
  world_geographic <- terra::vect(rnaturalearth::ne_countries(
    scale = "small", returnclass = "sf"
  ))
  world_attributes <- as.data.frame(world_geographic)
  if ("adm0_a3" %in% names(world_attributes)) {
    keep <- is.na(world_attributes$adm0_a3) | world_attributes$adm0_a3 != "ATA"
    world_geographic <- world_geographic[keep, ]
  }
  world <- terra::project(world_geographic, MAP_DISPLAY_CRS)
  world_extent <- terra::ext(world)
  map_xlim <- c(world_extent$xmin, world_extent$xmax)
  map_ylim <- c(world_extent$ymin, world_extent$ymax)
  graphics::layout(
    matrix(c(1L, 1L, 2L, 3L, 4L, 4L), nrow = 3L, byrow = TRUE),
    heights = c(0.72, 4.3, 0.75)
  )
  graphics::par(family = "sans")

  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  graphics::text(
    0.5, 0.78,
    sprintf("Spatial intensity of annual avoided emissions, %s", period_tag),
    font = 2, cex = 1.28, col = text_colour
  )
  coverage_label <- if (mode == "partial") {
    sprintf(
      "PRELIMINARY PARTIAL COVERAGE: results for %d of %d countries; mapped inputs cover %d countries",
      covered_countries, expected_countries, mapped_countries
    )
  } else {
    sprintf("Complete Global South coverage: %d countries", expected_countries)
  }
  graphics::text(0.5, 0.37, coverage_label, cex = 0.80, col = muted_colour)

  for (configuration in CONFIGURATION_ORDER) {
    graphics::par(mar = c(1.0, 1.0, 2.7, 1.0), xaxs = "i", yaxs = "i")
    terra::plot(
      world, xlim = map_xlim, ylim = map_ylim,
      col = "#F3F5F7", border = "#CDD4DB", lwd = 0.35,
      axes = FALSE, legend = FALSE
    )
    configuration_rasters <- rasters[endsWith(names(rasters), paste0("::", configuration))]
    for (raster in configuration_rasters) {
      terra::plot(
        raster, add = TRUE, col = colours, range = scale,
        legend = FALSE, axes = FALSE, maxcell = 100000
      )
    }
    terra::lines(world, col = "#5F6C78", lwd = 0.42)
    graphics::box(col = border_colour, lwd = 0.8)
    total_hit <- summary[
      summary$configuration == configuration & summary$metric == "total",
      , drop = FALSE
    ]
    graphics::title(
      main = sprintf(
        "%s   |   Included total: %.1f MtCO2e yr^-1",
        tools::toTitleCase(configuration), total_hit$mean[[1L]]
      ),
      line = 1.0, font.main = 2, cex.main = 0.94, col.main = text_colour
    )
  }

  graphics::par(mar = c(2.2, 7.0, 0.3, 7.0))
  graphics::plot.new()
  graphics::plot.window(xlim = scale, ylim = c(0, 1), xaxs = "i", yaxs = "i")
  breaks <- seq(scale[[1L]], scale[[2L]], length.out = length(colours) + 1L)
  graphics::rect(
    breaks[-length(breaks)], 0.48, breaks[-1L], 0.72,
    col = colours, border = NA
  )
  legend_ticks <- pretty(scale, n = 7L)
  legend_ticks <- legend_ticks[legend_ticks >= scale[[1L]] & legend_ticks <= scale[[2L]]]
  graphics::axis(
    1, at = legend_ticks,
    labels = formatC(legend_ticks, format = "fg", digits = 3L),
    pos = 0.45, cex.axis = 0.72, tck = -0.12,
    col = border_colour, col.axis = text_colour
  )
  graphics::text(
    mean(scale), 0.12,
    expression(paste("Annual total avoided-emissions intensity (tCO"[2], "e ", km^{-2}, " ", yr^{-1}, ")")),
    cex = 0.76, col = text_colour
  )
  graphics::text(
    mean(scale), 0.91,
    paste0(
      "Map projection: WGS 84 / Equal Earth Greenwich (EPSG:8857; equal-area). ",
      "Shared scale clipped to the combined 2nd-98th percentile range; blue is negative and red is positive."
    ),
    cex = 0.64, col = muted_colour
  )
  invisible(path)
}

mapped_country_count <- sum(spatial_inventory$country_count)
map_figure_path <- file.path(
  output_dir, "figures", "main",
  paste0(output_prefix, "figure_global_south_total_intensity_maps_mc_all.png")
)
write_global_map_figure(
  display_rasters, global_summary, map_figure_path, run_mode,
  mapped_country_count, nrow(source_countries), nrow(regionalization),
  period_tag, spatial_scale
)

# Final inventory ----

expected_files <- c(
  file.path("figures", "main", basename(ranked_figure_path)),
  file.path("figures", "main", basename(map_figure_path)),
  file.path("figures", "supplement", basename(contribution_figure_path)),
  file.path("tables", basename(table_paths)),
  file.path("validation", basename(validation_paths))
)
actual_files <- list.files(output_dir, recursive = TRUE, all.files = FALSE)
if (!setequal(gsub("\\\\", "/", actual_files), gsub("\\\\", "/", expected_files))) {
  stopf(
    "Global manuscript package inventory differs from the expected %d files.",
    length(expected_files)
  )
}

cat(sprintf("GLOBAL_MANUSCRIPT_PACKAGE_CREATED=%s\n", output_dir))
cat(sprintf("SCRIPT_VERSION=%d\n", SCRIPT_VERSION))
cat(sprintf("MODE=%s\n", run_mode))
cat(sprintf("MC_COMBINATION=%s\n", mc_combination))
cat(sprintf("PERIOD=%s\n", period_tag))
cat(sprintf("ANALYSIS_ROOTS=%d\n", nrow(manifest)))
cat(sprintf("INCLUDED_COUNTRIES=%d\n", nrow(source_countries)))
cat(sprintf("EXPECTED_COUNTRIES=%d\n", nrow(regionalization)))
cat(sprintf("COVERAGE_COMPLETE=%s\n", coverage_complete))
cat(sprintf("MAPPED_COUNTRIES=%d\n", mapped_country_count))
cat(sprintf("FILE_COUNT=%d\n", length(actual_files)))
