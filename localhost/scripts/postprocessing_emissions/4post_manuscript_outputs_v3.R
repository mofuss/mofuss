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
# Script: 4post_manuscript_outputs_v3.R
# Version: 3
# Date: September 2026
# Execution: Use regular RStudio Source, RStudio Source as Background Job, or
# run directly with Rscript from PowerShell/a terminal. Dinamica EGO does not
# invoke this script directly.
#
# Purpose: Build the minimal manuscript package from existing Stage 2 and
# Stage 3 outputs without rerunning MoFuSS or any emissions stage.
# Inputs: A completed Stage 2/3 analysis root containing summary CSVs and
# component rasters.
# Outputs: Regional and country-disaggregated MC01 manuscript tables, rasters,
# 300-dpi map figures with country boundaries and labels, and a two-panel
# country contribution figure. When the Monte Carlo sample meets the configured
# uncertainty threshold, also writes regional and country MC-all tables,
# mean/SD rasters, and mean/uncertainty figures.
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

SCRIPT_VERSION <- 3L
DEFAULT_MIN_UNCERTAINTY_RUNS <- 30L
CONFIGURATION_ORDER <- c("capped", "uncapped")
COMPONENT_ORDER <- c("harvest", "enduse", "total")
COMPONENT_LABELS <- c(harvest = "Harvest / AGB", enduse = "End-use", total = "Total")
TABLE_PNG_DPI <- 300L
TABLE_PNG_WIDTH_IN <- 7.5
COUNTRY_FIGURE_DPI <- 300L
COUNTRY_FIGURE_WIDTH_IN <- 12.5
MAP_DISPLAY_CRS <- "EPSG:8857"

# Paths are inferred centrally by 0post_emissions_pipeline_v2.R and passed as
# command-line options. NULL prevents a stale computer-specific path from being
# used when this stage is sourced directly.
V1_RSTUDIO_SOURCE_DIR <- NULL
V1_RSTUDIO_OUTPUT_DIR <- NULL
V1_RSTUDIO_MIN_UNCERTAINTY_RUNS <- DEFAULT_MIN_UNCERTAINTY_RUNS
V1_RSTUDIO_CLEAN_REBUILD <- TRUE

stopf <- function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)

args <- commandArgs(trailingOnly = TRUE)
v1_source_mode <- interactive() || sys.nframe() > 0L
arg_value <- function(name, default = NULL) {
  prefix <- paste0("--", name, "=")
  hit <- args[startsWith(args, prefix)]
  if (!length(hit)) return(default)
  if (length(hit) > 1L) stopf("Argument --%s was supplied more than once.", name)
  substring(hit[[1L]], nchar(prefix) + 1L)
}
arg_flag <- function(name) paste0("--", name) %in% args

if (v1_source_mode) {
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

prepare_output_dir <- function(
  path, source_root, allow_overwrite, include_uncertainty
) {
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
  dirs <- c("figures/mc_1", "rasters/mc_1", "tables", "spatial")
  if (include_uncertainty) {
    dirs <- c(dirs, "figures/mc_all", "rasters/mc_all")
  }
  for (d in dirs) dir.create(file.path(target, d), recursive = TRUE, showWarnings = FALSE)
}

read_csv_required <- function(path, label) {
  if (!file.exists(path)) stopf("Missing %s: %s", label, path)
  as.data.frame(suppressMessages(readr::read_csv(
    path, show_col_types = FALSE, name_repair = "minimal", progress = FALSE
  )))
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

write_table_png <- function(x, path, title, subtitle) {
  value_columns <- setdiff(names(x), "Metric")
  if (length(value_columns) != 2L) {
    stopf("PNG table renderer expects Metric plus exactly two value columns.")
  }

  is_blank <- function(value) {
    is.na(value) || !nzchar(trimws(as.character(value)))
  }
  note_rows <- vapply(seq_len(nrow(x)), function(i) {
    all(vapply(value_columns, function(column) {
      is_blank(x[[column]][[i]])
    }, logical(1)))
  }, logical(1))
  body <- x[!note_rows, , drop = FALSE]
  notes <- as.character(x$Metric[note_rows])

  format_value <- function(value, metric) {
    if (is_blank(value)) return("")
    digits <- if (grepl("MtCO2e", metric, fixed = TRUE)) 3L else 0L
    format_number <- function(z) {
      number <- suppressWarnings(as.numeric(trimws(z)))
      if (!is.finite(number)) return(trimws(z))
      formatC(number, format = "f", digits = digits, big.mark = ",")
    }
    text <- as.character(value)
    plus_minus <- intToUtf8(177L)
    if (grepl(plus_minus, text, fixed = TRUE)) {
      pieces <- strsplit(text, plus_minus, fixed = TRUE)[[1L]]
      return(paste(vapply(pieces, format_number, character(1)), collapse = paste0(" ", plus_minus, " ")))
    }
    format_number(text)
  }

  display <- body
  for (column in value_columns) {
    display[[column]] <- vapply(seq_len(nrow(body)), function(i) {
      format_value(body[[column]][[i]], body$Metric[[i]])
    }, character(1))
  }

  wrapped_notes <- lapply(notes, function(note) strwrap(note, width = 130L))
  title_height <- 0.54
  subtitle_height <- 0.28
  header_height <- 0.42
  body_height <- 0.36
  note_line_height <- 0.18
  notes_height <- if (length(wrapped_notes)) {
    sum(vapply(wrapped_notes, length, integer(1))) * note_line_height +
      length(wrapped_notes) * 0.15 + 0.12
  } else 0
  height_in <- 0.22 + title_height + subtitle_height + 0.12 +
    header_height + nrow(display) * body_height + notes_height + 0.22

  png_args <- list(
    filename = path, width = TABLE_PNG_WIDTH_IN, height = height_in,
    units = "in", res = TABLE_PNG_DPI, pointsize = 11, bg = "white"
  )
  if (isTRUE(capabilities("cairo"))) png_args$type <- "cairo"
  do.call(grDevices::png, png_args)
  on.exit(grDevices::dev.off(), add = TRUE)

  graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", family = "sans")
  graphics::plot.new()
  graphics::plot.window(
    xlim = c(0, TABLE_PNG_WIDTH_IN), ylim = c(0, height_in),
    xaxs = "i", yaxs = "i"
  )

  left <- 0.22
  right <- TABLE_PNG_WIDTH_IN - 0.22
  column_edges <- c(left, left + 3.55, left + 5.30, right)
  y <- height_in - 0.20
  graphics::text(left, y, title, adj = c(0, 1), font = 2, cex = 1.18, col = "#172B4D")
  y <- y - title_height
  graphics::text(left, y, subtitle, adj = c(0, 1), cex = 0.83, col = "#44546A")
  y <- y - subtitle_height - 0.06

  header_bottom <- y - header_height
  graphics::rect(left, header_bottom, right, y, col = "#234E70", border = NA)
  graphics::text(column_edges[[1L]] + 0.10, (y + header_bottom) / 2, "Metric",
                 adj = c(0, 0.5), font = 2, cex = 0.86, col = "white")
  for (j in seq_along(value_columns)) {
    x_mid <- mean(column_edges[c(j + 1L, j + 2L)])
    graphics::text(x_mid, (y + header_bottom) / 2, value_columns[[j]],
                   font = 2, cex = 0.82, col = "white")
  }
  y <- header_bottom

  for (i in seq_len(nrow(display))) {
    row_bottom <- y - body_height
    important <- grepl("^(Total emissions|Annual emissions)", display$Metric[[i]])
    fill <- if (important) "#DCEAF4" else if (i %% 2L == 0L) "#F4F6F8" else "white"
    graphics::rect(left, row_bottom, right, y, col = fill, border = NA)
    graphics::segments(left, row_bottom, right, row_bottom, col = "#D5DCE3", lwd = 0.55)
    graphics::text(
      column_edges[[1L]] + 0.10, (y + row_bottom) / 2, display$Metric[[i]],
      adj = c(0, 0.5), font = if (important) 2 else 1, cex = 0.80,
      col = "#1F2933"
    )
    for (j in seq_along(value_columns)) {
      graphics::text(
        column_edges[[j + 2L]] - 0.10, (y + row_bottom) / 2,
        display[[value_columns[[j]]]][[i]], adj = c(1, 0.5),
        font = if (important) 2 else 1, cex = 0.80, col = "#1F2933"
      )
    }
    y <- row_bottom
  }
  graphics::rect(left, y, right, y + nrow(display) * body_height,
                 border = "#9AA5B1", lwd = 0.8)
  graphics::segments(column_edges[[2L]], y, column_edges[[2L]],
                     y + nrow(display) * body_height + header_height,
                     col = "#C7CED6", lwd = 0.55)
  graphics::segments(column_edges[[3L]], y, column_edges[[3L]],
                     y + nrow(display) * body_height + header_height,
                     col = "#C7CED6", lwd = 0.55)

  if (length(wrapped_notes)) {
    y <- y - 0.20
    for (lines in wrapped_notes) {
      graphics::text(
        left, y, paste(lines, collapse = "\n"), adj = c(0, 1),
        cex = 0.66, col = "#52616B"
      )
      y <- y - length(lines) * note_line_height - 0.15
    }
  }
  invisible(path)
}

same_number <- function(a, b, tolerance = 1e-6) {
  isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tolerance))
}

decode_unicode_tokens <- function(x) {
  decode_one <- function(value) {
    if (is.na(value)) return(NA_character_)
    value <- as.character(value)
    repeat {
      hit <- regexpr("<U\\+[0-9A-Fa-f]{4,6}>", value, perl = TRUE)
      if (hit[[1L]] < 0L) break
      token <- regmatches(value, hit)
      codepoint <- strtoi(substr(token, 4L, nchar(token) - 1L), base = 16L)
      regmatches(value, hit) <- intToUtf8(codepoint)
    }
    enc2utf8(value)
  }
  vapply(x, decode_one, character(1), USE.NAMES = FALSE)
}

agb_dir <- file.path(source_dir, "agb_decomposition")
agb_per_run_path <- select_one(
  file.path(agb_dir, "agb_decomposition_per_run_*.csv"), "Stage 3 per-run decomposition file"
)
agb_mc1_path <- select_one(
  file.path(agb_dir, "comparison_table_mc1_*.csv"), "Stage 3 MC1 comparison table"
)
country_per_run_path <- select_one(
  file.path(agb_dir, "agb_decomposition_by_country_per_run_*.csv"),
  "Stage 3 country per-run decomposition file"
)
country_scope_path <- file.path(agb_dir, "country_scope.csv")
country_boundaries_path <- file.path(agb_dir, "country_boundaries.gpkg")

per_run <- read_csv_required(agb_per_run_path, "Stage 3 per-run decomposition")
require_columns(
  per_run,
  c(
    "country_iso", "country_name", "analysis_area_kind", "analysis_area_id",
    "analysis_area_name", "regrowth_mode", "display_label", "run_id",
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

region_iso_values <- unique(as.character(per_run$analysis_area_id))
region_name_values <- unique(decode_unicode_tokens(per_run$analysis_area_name))
region_kind_values <- unique(as.character(per_run$analysis_area_kind))
period_starts <- unique(as.integer(per_run$period_start_year))
period_ends <- unique(as.integer(per_run$period_end_year))
if (length(region_iso_values) != 1L || length(region_name_values) != 1L ||
    length(region_kind_values) != 1L ||
    length(period_starts) != 1L || length(period_ends) != 1L) {
  stopf("Stage 3 rows do not share one region and one analysis period.")
}

region_iso <- region_iso_values[[1L]]
region_name <- region_name_values[[1L]]
region_kind <- region_kind_values[[1L]]
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
if (!length(run_ids)) stopf("At least one Monte Carlo run is required.")
if (!1L %in% run_ids) stopf("Run 1 is missing from the Stage 3 per-run decomposition.")
n_runs <- length(run_ids)
uncertainty_adequate <- n_runs >= min_uncertainty_runs

display_labels <- vapply(CONFIGURATION_ORDER, function(configuration) {
  hits <- unique(as.character(per_run$display_label[per_run$regrowth_mode == configuration]))
  if (length(hits) != 1L) stopf("%s has inconsistent display labels.", configuration)
  hits[[1L]]
}, character(1))
names(display_labels) <- CONFIGURATION_ORDER

country_scope <- read_csv_required(country_scope_path, "Stage 3 country scope")
require_columns(
  country_scope,
  c(
    "analysis_area_kind", "analysis_area_id", "analysis_area_name",
    "country_id", "country_iso", "country_name", "spatial_accounting"
  ),
  "Stage 3 country scope"
)
country_scope$country_id <- suppressWarnings(as.integer(country_scope$country_id))
country_scope$country_iso <- toupper(trimws(as.character(country_scope$country_iso)))
country_scope$country_name <- trimws(decode_unicode_tokens(country_scope$country_name))
country_scope$analysis_area_name <- trimws(
  decode_unicode_tokens(country_scope$analysis_area_name)
)
country_scope <- country_scope[order(country_scope$country_id), , drop = FALSE]
if (!nrow(country_scope) || anyNA(country_scope$country_id) ||
    any(country_scope$country_id <= 0L) || anyDuplicated(country_scope$country_id) ||
    anyDuplicated(country_scope$country_iso)) {
  stopf("Stage 3 country scope contains invalid or duplicate country identities.")
}
scope_area_key <- unique(paste(
  trimws(as.character(country_scope$analysis_area_kind)),
  toupper(trimws(as.character(country_scope$analysis_area_id))),
  trimws(as.character(country_scope$analysis_area_name)), sep = "|"
))
expected_area_key <- paste(region_kind, toupper(region_iso), region_name, sep = "|")
if (length(scope_area_key) != 1L || !identical(scope_area_key, expected_area_key)) {
  stopf("Stage 3 country scope does not match the regional decomposition identity.")
}
if (any(trimws(as.character(country_scope$spatial_accounting)) != "spatial_incidence")) {
  stopf("Only spatial-incidence country accounting is supported.")
}

country_per_run <- read_csv_required(
  country_per_run_path, "Stage 3 country per-run decomposition"
)
country_required <- c(
  "analysis_area_kind", "analysis_area_id", "analysis_area_name",
  "country_id", "country_iso", "country_name", "spatial_accounting",
  "regrowth_mode", "display_label", "run_id", "period_start_year",
  "period_end_year", "bau_end_agb_mg", "ics_end_agb_mg",
  "period_avoided_loss_mg", "period_regrowth_mg",
  "period_avoided_loss_tco2e", "period_regrowth_tco2e",
  "agb_avoided_stage2_tco2e", "enduse_avoided_tco2e",
  "total_avoided_tco2e", "n_decomposition_period_common",
  "all_invariants_ok"
)
require_columns(country_per_run, country_required, "Stage 3 country per-run decomposition")
country_per_run$country_id <- suppressWarnings(as.integer(country_per_run$country_id))
country_per_run$country_iso <- toupper(trimws(as.character(country_per_run$country_iso)))
country_per_run$country_name <- trimws(decode_unicode_tokens(country_per_run$country_name))
country_per_run$analysis_area_name <- trimws(
  decode_unicode_tokens(country_per_run$analysis_area_name)
)
country_per_run$run_id <- suppressWarnings(as.integer(country_per_run$run_id))
country_per_run$period_start_year <- suppressWarnings(as.integer(country_per_run$period_start_year))
country_per_run$period_end_year <- suppressWarnings(as.integer(country_per_run$period_end_year))
if (anyNA(country_per_run$country_id) || anyNA(country_per_run$run_id) ||
    any(!as.logical(country_per_run$all_invariants_ok))) {
  stopf("Stage 3 country per-run decomposition contains invalid identities or a failed invariant.")
}
country_keys <- paste(
  country_per_run$country_id, country_per_run$regrowth_mode,
  country_per_run$run_id, sep = "|"
)
if (anyDuplicated(country_keys)) {
  stopf("Stage 3 country per-run decomposition contains duplicate country/configuration/run rows.")
}
expected_country_keys <- unlist(lapply(CONFIGURATION_ORDER, function(configuration) {
  unlist(lapply(run_ids, function(run_id) {
    paste(country_scope$country_id, configuration, run_id, sep = "|")
  }), use.names = FALSE)
}), use.names = FALSE)
if (!setequal(country_keys, expected_country_keys)) {
  stopf("Stage 3 country rows do not form a complete country/configuration/run grid.")
}
country_scope_lookup <- country_scope[, c("country_id", "country_iso", "country_name")]
country_identity <- unique(country_per_run[, c("country_id", "country_iso", "country_name")])
country_identity <- country_identity[order(country_identity$country_id), , drop = FALSE]
rownames(country_scope_lookup) <- NULL
rownames(country_identity) <- NULL
if (!identical(country_identity, country_scope_lookup)) {
  stopf("Stage 3 country per-run identities do not match country_scope.csv.")
}
country_area_keys <- unique(paste(
  trimws(as.character(country_per_run$analysis_area_kind)),
  toupper(trimws(as.character(country_per_run$analysis_area_id))),
  trimws(as.character(country_per_run$analysis_area_name)), sep = "|"
))
if (length(country_area_keys) != 1L || !identical(country_area_keys, expected_area_key) ||
    any(country_per_run$period_start_year != period_start) ||
    any(country_per_run$period_end_year != period_end) ||
    any(trimws(as.character(country_per_run$spatial_accounting)) != "spatial_incidence")) {
  stopf("Stage 3 country rows disagree with the analysis area, period, or accounting basis.")
}

country_additive_fields <- c(
  "bau_end_agb_mg", "ics_end_agb_mg", "period_avoided_loss_mg",
  "period_regrowth_mg", "period_avoided_loss_tco2e",
  "period_regrowth_tco2e", "agb_avoided_stage2_tco2e",
  "enduse_avoided_tco2e", "total_avoided_tco2e",
  "n_decomposition_period_common"
)
for (i in seq_len(nrow(per_run))) {
  regional_row <- per_run[i, , drop = FALSE]
  rows <- country_per_run[
    country_per_run$regrowth_mode == regional_row$regrowth_mode[[1L]] &
      country_per_run$run_id == as.integer(regional_row$run_id[[1L]]),
    , drop = FALSE
  ]
  for (field in country_additive_fields) {
    expected <- as.numeric(regional_row[[field]][[1L]])
    observed <- sum(as.numeric(rows[[field]]))
    tolerance <- if (startsWith(field, "n_")) 0 else max(0.05, abs(expected) * 1e-9)
    if (!is.finite(observed) || abs(observed - expected) > tolerance) {
      stopf(
        "Country values do not reconcile to the regional %s row for %s run %d.",
        field, regional_row$regrowth_mode[[1L]], regional_row$run_id[[1L]]
      )
    }
  }
}

if (!file.exists(country_boundaries_path) || dir.exists(country_boundaries_path)) {
  stopf("Missing Stage 3 country boundaries: %s", country_boundaries_path)
}
country_boundaries <- terra::vect(country_boundaries_path)
if (!all(c("ID", "GID_0", "NAME_0") %in% names(country_boundaries))) {
  stopf("Stage 3 country boundaries lack ID/GID_0/NAME_0 attributes.")
}
boundary_lookup <- data.frame(
  country_id = suppressWarnings(as.integer(country_boundaries$ID)),
  country_iso = toupper(trimws(as.character(country_boundaries$GID_0))),
  country_name = trimws(decode_unicode_tokens(country_boundaries$NAME_0)),
  stringsAsFactors = FALSE
)
boundary_lookup <- boundary_lookup[order(boundary_lookup$country_id), , drop = FALSE]
rownames(boundary_lookup) <- NULL
if (!identical(boundary_lookup, country_scope_lookup)) {
  stopf("Stage 3 country boundary attributes do not match country_scope.csv.")
}
country_boundaries$NAME_0 <- country_scope$country_name[
  match(suppressWarnings(as.integer(country_boundaries$ID)), country_scope$country_id)
]

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
mc_all_table <- if (uncertainty_adequate) {
  data.frame(Metric = metric_labels, check.names = FALSE, stringsAsFactors = FALSE)
} else {
  NULL
}
format_metric <- function(x, digits) formatC(as.numeric(x), format = "f", digits = digits)
plus_minus <- intToUtf8(177L)

for (configuration in CONFIGURATION_ORDER) {
  rows <- per_run[per_run$regrowth_mode == configuration, , drop = FALSE]
  rows <- rows[order(as.integer(rows$run_id)), , drop = FALSE]
  mc1_row <- rows[as.integer(rows$run_id) == 1L, , drop = FALSE]
  if (nrow(mc1_row) != 1L) stopf("Expected exactly one MC1 row for %s.", configuration)

  raw_mc1 <- vapply(unname(metric_fields), function(field) as.numeric(mc1_row[[field]][[1L]]), numeric(1)) * metric_scales
  mc1_table[[display_labels[[configuration]]]] <- vapply(
    seq_along(raw_mc1), function(i) round(raw_mc1[[i]], metric_digits[[i]]), numeric(1)
  )
  if (uncertainty_adequate) {
    means <- vapply(
      unname(metric_fields), function(field) mean(as.numeric(rows[[field]])), numeric(1)
    ) * metric_scales
    sds <- vapply(
      unname(metric_fields), function(field) stats::sd(as.numeric(rows[[field]])), numeric(1)
    ) * metric_scales
    mc_all_table[[display_labels[[configuration]]]] <- paste0(
      vapply(seq_along(means), function(i) format_metric(means[[i]], metric_digits[[i]]), character(1)),
      " ", plus_minus, " ",
      vapply(seq_along(sds), function(i) format_metric(sds[[i]], metric_digits[[i]]), character(1))
    )
  }
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

# Validate Stage 3 ensemble metrics only when they will be published.
if (uncertainty_adequate) {
  agb_uncertainty_path <- select_one(
    file.path(agb_dir, "uncertainty_summary_*.csv"), "Stage 3 uncertainty summary"
  )
  uncertainty <- read_csv_required(agb_uncertainty_path, "Stage 3 uncertainty summary")
  require_columns(
    uncertainty, c("regrowth_mode", "metric", "runs", "mean", "sd"),
    "Stage 3 uncertainty summary"
  )
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
      hit <- uncertainty[
        uncertainty$regrowth_mode == configuration & uncertainty$metric == metric,
        , drop = FALSE
      ]
      if (nrow(hit) != 1L || as.integer(hit$runs[[1L]]) != n_runs) {
        stopf(
          "Uncertainty summary row is missing or has the wrong run count: %s %s.",
          configuration, metric
        )
      }
      values <- as.numeric(rows[[uncertainty_checks[[metric]]]])
      if (!same_number(mean(values), hit$mean[[1L]], tolerance = 1e-8) ||
          !same_number(stats::sd(values), hit$sd[[1L]], tolerance = 1e-8)) {
        stopf("Uncertainty summary does not reconcile: %s %s.", configuration, metric)
      }
    }
  }
}

footnotes <- c(
  sprintf(
    paste0(
      "Note 1: The %s reporting period begins after the %d-%d spin-up/past-to-present simulation ",
      "(%d years); its start year is calculated as simulation_start_year plus the configured spin-up interval. ",
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
  for (configuration in CONFIGURATION_ORDER) {
    mc1_note[[display_labels[[configuration]]]] <- NA_real_
  }
  mc1_table <- rbind(mc1_table, mc1_note)
  if (uncertainty_adequate) {
    mc_all_note <- data.frame(Metric = note, check.names = FALSE, stringsAsFactors = FALSE)
    for (configuration in CONFIGURATION_ORDER) {
      mc_all_note[[display_labels[[configuration]]]] <- NA_character_
    }
    mc_all_table <- rbind(mc_all_table, mc_all_note)
  }
}

country_metric_fields <- c(
  bau_end_agb_mg = "bau_end_agb_mg",
  ics_end_agb_mg = "ics_end_agb_mg",
  avoided_agb_loss_mg = "period_avoided_loss_mg",
  enhanced_agb_regrowth_mg = "period_regrowth_mg",
  avoided_agb_loss_tco2e = "period_avoided_loss_tco2e",
  enhanced_agb_regrowth_tco2e = "period_regrowth_tco2e",
  harvest_agb_avoided_tco2e = "agb_avoided_stage2_tco2e",
  enduse_avoided_tco2e = "enduse_avoided_tco2e",
  total_avoided_tco2e = "total_avoided_tco2e",
  annual_avoided_mtco2e_per_year = "total_avoided_tco2e",
  common_decomposition_cells = "n_decomposition_period_common"
)
country_metric_labels <- c(
  bau_end_agb_mg = sprintf("BAU AGB %d", period_end),
  ics_end_agb_mg = sprintf("CCTS AGB %d", period_end),
  avoided_agb_loss_mg = "Avoided AGB loss",
  enhanced_agb_regrowth_mg = "Enhanced AGB regrowth",
  avoided_agb_loss_tco2e = "Avoided AGB-loss emissions",
  enhanced_agb_regrowth_tco2e = "Enhanced-regrowth emissions",
  harvest_agb_avoided_tco2e = "Harvest / AGB emissions avoided",
  enduse_avoided_tco2e = "End-use emissions avoided",
  total_avoided_tco2e = "Total emissions avoided",
  annual_avoided_mtco2e_per_year = "Annual emissions avoided",
  common_decomposition_cells = "Common decomposition cells"
)
country_metric_units <- c(
  bau_end_agb_mg = "Mg",
  ics_end_agb_mg = "Mg",
  avoided_agb_loss_mg = "Mg per period",
  enhanced_agb_regrowth_mg = "Mg per period",
  avoided_agb_loss_tco2e = "tCO2e per period",
  enhanced_agb_regrowth_tco2e = "tCO2e per period",
  harvest_agb_avoided_tco2e = "tCO2e per period",
  enduse_avoided_tco2e = "tCO2e per period",
  total_avoided_tco2e = "tCO2e per period",
  annual_avoided_mtco2e_per_year = "MtCO2e yr^-1",
  common_decomposition_cells = "cells"
)
country_metric_scales <- c(
  rep(1, 9L), 1 / (1e6 * reporting_years), 1
)
names(country_metric_scales) <- names(country_metric_fields)

country_identity_columns <- c(
  "analysis_area_kind", "analysis_area_id", "analysis_area_name",
  "country_id", "country_iso", "country_name", "spatial_accounting",
  "regrowth_mode", "display_label"
)
country_mc1_source <- country_per_run[country_per_run$run_id == 1L, , drop = FALSE]
country_mc1_source <- country_mc1_source[
  order(match(country_mc1_source$regrowth_mode, CONFIGURATION_ORDER), country_mc1_source$country_id),
  , drop = FALSE
]
country_mc1_tidy <- do.call(rbind, lapply(seq_len(nrow(country_mc1_source)), function(i) {
  row <- country_mc1_source[i, , drop = FALSE]
  values <- vapply(names(country_metric_fields), function(metric) {
    as.numeric(row[[country_metric_fields[[metric]]]][[1L]]) * country_metric_scales[[metric]]
  }, numeric(1))
  identity <- row[rep(1L, length(values)), country_identity_columns, drop = FALSE]
  rownames(identity) <- NULL
  cbind(
    identity,
    data.frame(
      run_id = 1L,
      period_start_year = period_start,
      period_end_year = period_end,
      metric = names(country_metric_fields),
      metric_label = unname(country_metric_labels[names(country_metric_fields)]),
      unit = unname(country_metric_units[names(country_metric_fields)]),
      estimate = unname(values),
      stringsAsFactors = FALSE
    )
  )
}))
rownames(country_mc1_tidy) <- NULL

country_mc_all_tidy <- NULL
if (uncertainty_adequate) {
  country_groups <- split(
    country_per_run,
    paste(country_per_run$regrowth_mode, country_per_run$country_id, sep = "|")
  )
  country_mc_all_tidy <- do.call(rbind, lapply(country_groups, function(rows) {
    rows <- rows[order(rows$run_id), , drop = FALSE]
    if (nrow(rows) != n_runs || !identical(rows$run_id, run_ids)) {
      stopf("Country uncertainty group has incomplete or unordered run IDs.")
    }
    metric_rows <- lapply(names(country_metric_fields), function(metric) {
      values <- as.numeric(rows[[country_metric_fields[[metric]]]]) *
        country_metric_scales[[metric]]
      identity <- rows[1L, country_identity_columns, drop = FALSE]
      cbind(
        identity,
        data.frame(
          runs = n_runs,
          period_start_year = period_start,
          period_end_year = period_end,
          metric = metric,
          metric_label = unname(country_metric_labels[[metric]]),
          unit = unname(country_metric_units[[metric]]),
          mean = mean(values),
          sd = stats::sd(values),
          empirical_p025 = as.numeric(stats::quantile(values, 0.025, names = FALSE)),
          empirical_p975 = as.numeric(stats::quantile(values, 0.975, names = FALSE)),
          stringsAsFactors = FALSE
        )
      )
    })
    do.call(rbind, metric_rows)
  }))
  country_mc_all_tidy <- country_mc_all_tidy[
    order(
      match(country_mc_all_tidy$regrowth_mode, CONFIGURATION_ORDER),
      country_mc_all_tidy$country_id,
      match(country_mc_all_tidy$metric, names(country_metric_fields))
    ),
    , drop = FALSE
  ]
  rownames(country_mc_all_tidy) <- NULL
}

country_annual_scale <- 1 / (1e6 * reporting_years)
country_figure_data <- do.call(rbind, lapply(CONFIGURATION_ORDER, function(configuration) {
  configuration_rows <- country_per_run[
    country_per_run$regrowth_mode == configuration,
    , drop = FALSE
  ]
  country_groups <- split(configuration_rows, configuration_rows$country_id)
  do.call(rbind, lapply(country_groups, function(rows) {
    rows <- rows[order(rows$run_id), , drop = FALSE]
    selected <- if (uncertainty_adequate) {
      rows
    } else {
      rows[rows$run_id == 1L, , drop = FALSE]
    }
    if (!nrow(selected)) {
      stopf("Country figure is missing MC1 data for %s.", configuration)
    }
    total_values <- as.numeric(selected$total_avoided_tco2e) * country_annual_scale
    data.frame(
      country_id = as.integer(rows$country_id[[1L]]),
      country_label = sprintf("%s (%s)", rows$country_name[[1L]], rows$country_iso[[1L]]),
      configuration = configuration,
      avoided_loss = mean(as.numeric(selected$period_avoided_loss_tco2e)) * country_annual_scale,
      regrowth = mean(as.numeric(selected$period_regrowth_tco2e)) * country_annual_scale,
      harvest = mean(as.numeric(selected$agb_avoided_stage2_tco2e)) * country_annual_scale,
      enduse = mean(as.numeric(selected$enduse_avoided_tco2e)) * country_annual_scale,
      total = mean(total_values),
      total_p025 = if (uncertainty_adequate) {
        as.numeric(stats::quantile(total_values, 0.025, names = FALSE))
      } else {
        total_values[[1L]]
      },
      total_p975 = if (uncertainty_adequate) {
        as.numeric(stats::quantile(total_values, 0.975, names = FALSE))
      } else {
        total_values[[1L]]
      },
      stringsAsFactors = FALSE
    )
  }))
}))
country_figure_data <- country_figure_data[
  order(
    match(country_figure_data$configuration, CONFIGURATION_ORDER),
    country_figure_data$country_id
  ),
  , drop = FALSE
]
rownames(country_figure_data) <- NULL

country_figure_values <- unlist(
  country_figure_data[c(
    "avoided_loss", "regrowth", "harvest", "enduse", "total",
    "total_p025", "total_p975"
  )],
  use.names = FALSE
)
if (any(!is.finite(country_figure_values))) {
  stopf("Country contribution figure data contain non-finite values.")
}
country_harvest_ok <- mapply(
  same_number,
  country_figure_data$avoided_loss + country_figure_data$regrowth,
  country_figure_data$harvest,
  MoreArgs = list(tolerance = 1e-8)
)
country_total_ok <- mapply(
  same_number,
  country_figure_data$harvest + country_figure_data$enduse,
  country_figure_data$total,
  MoreArgs = list(tolerance = 1e-8)
)
if (!all(country_harvest_ok) || !all(country_total_ok)) {
  stopf("Country contribution figure components do not reconcile to their totals.")
}
if (any(country_figure_data$total_p025 > country_figure_data$total) ||
    any(country_figure_data$total_p975 < country_figure_data$total)) {
  stopf("Country contribution figure uncertainty intervals do not contain their means.")
}

write_country_contribution_figure <- function(
  x, path, region_name, period_tag, n_runs, show_uncertainty
) {
  if (!nrow(x) || !setequal(unique(x$configuration), CONFIGURATION_ORDER)) {
    stopf("Country contribution figure requires both configured panels.")
  }
  panel_counts <- table(x$configuration)
  if (length(unique(as.integer(panel_counts))) != 1L) {
    stopf("Country contribution figure panels have different country counts.")
  }

  ranking <- x[x$configuration == "uncapped", , drop = FALSE]
  ranking <- ranking[order(ranking$total, decreasing = TRUE), , drop = FALSE]
  country_order <- ranking$country_label
  if (anyDuplicated(country_order)) {
    stopf("Country contribution figure requires unique country labels.")
  }
  n_countries <- length(country_order)

  endpoints <- c(
    0, x$avoided_loss, x$avoided_loss + x$regrowth,
    x$harvest, x$total, x$total_p025, x$total_p975
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

  figure_height_in <- max(5.8, 3.9 + n_countries * 0.42)
  png_args <- list(
    filename = path,
    width = COUNTRY_FIGURE_WIDTH_IN,
    height = figure_height_in,
    units = "in",
    res = COUNTRY_FIGURE_DPI,
    pointsize = 10,
    bg = "white"
  )
  if (isTRUE(capabilities("cairo"))) png_args$type <- "cairo"
  do.call(grDevices::png, png_args)
  on.exit(grDevices::dev.off(), add = TRUE)

  colours <- c(
    avoided_loss = "#0072B2",
    regrowth = "#009E73",
    enduse = "#D55E00",
    total = "#172B4D",
    uncertainty = "#596775",
    grid = "#E1E6EB",
    border = "#9AA5B1",
    text = "#1F2933",
    muted = "#52616B"
  )

  graphics::layout(
    matrix(c(1L, 1L, 2L, 3L), nrow = 2L, byrow = TRUE),
    heights = c(1.25, 4)
  )
  graphics::par(oma = c(3.5, 0.4, 0.2, 0.4), family = "sans")

  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
  graphics::text(
    0.5, 0.88,
    sprintf("Annual avoided emissions by country and configuration, %s", period_tag),
    font = 2, cex = 1.30, col = colours[["total"]]
  )
  subtitle <- if (show_uncertainty) {
    sprintf(
      "%s; mean across %d Monte Carlo realizations (BAU - CCTS)",
      region_name, n_runs
    )
  } else {
    sprintf("%s; first Monte Carlo realization (BAU - CCTS)", region_name)
  }
  graphics::text(0.5, 0.64, subtitle, cex = 0.87, col = colours[["muted"]])

  legend_labels <- c(
    "Avoided AGB loss", "Enhanced regrowth", "End-use adjustment", "Total mean"
  )
  legend_fill <- c(
    colours[["avoided_loss"]], colours[["regrowth"]], NA, NA
  )
  legend_border <- rep(NA, length(legend_labels))
  legend_lty <- c(NA, NA, 1, NA)
  legend_lwd <- c(NA, NA, 3, NA)
  legend_pch <- c(NA, NA, NA, 21)
  legend_col <- c(NA, NA, colours[["enduse"]], colours[["total"]])
  legend_pt_bg <- c(NA, NA, NA, colours[["total"]])
  if (show_uncertainty) {
    legend_labels <- c(legend_labels, "Empirical 95% interval")
    legend_fill <- c(legend_fill, NA)
    legend_border <- c(legend_border, NA)
    legend_lty <- c(legend_lty, 1)
    legend_lwd <- c(legend_lwd, 1.4)
    legend_pch <- c(legend_pch, NA)
    legend_col <- c(legend_col, colours[["uncertainty"]])
    legend_pt_bg <- c(legend_pt_bg, NA)
  }
  graphics::legend(
    "bottom", legend = legend_labels, horiz = TRUE, bty = "n",
    fill = legend_fill, border = legend_border,
    lty = legend_lty, lwd = legend_lwd,
    pch = legend_pch, col = legend_col, pt.bg = legend_pt_bg,
    pt.cex = 1.0, cex = 0.78, xpd = NA
  )

  label_characters <- max(nchar(country_order, type = "width"))
  left_margin_lines <- max(8.5, min(13.5, 4.8 + 0.36 * label_characters))
  y_positions <- rev(seq_len(n_countries))
  bar_half_height <- min(0.17, 0.32 / sqrt(max(1, n_countries / 8)))
  label_cex <- max(0.58, min(0.82, 1.05 / sqrt(max(1, n_countries / 7))))

  for (configuration in CONFIGURATION_ORDER) {
    panel <- x[x$configuration == configuration, , drop = FALSE]
    panel <- panel[match(country_order, panel$country_label), , drop = FALSE]
    if (anyNA(panel$country_label)) {
      stopf("Country contribution figure panels do not share the same countries.")
    }

    graphics::par(
      mar = c(3.2, left_margin_lines, 2.8, 1.0),
      xaxs = "i", yaxs = "i"
    )
    graphics::plot.new()
    graphics::plot.window(
      xlim = plot_limits, ylim = c(0.45, n_countries + 0.55),
      xaxs = "i", yaxs = "i"
    )
    graphics::abline(v = tick_values, col = colours[["grid"]], lwd = 0.8)
    graphics::abline(h = y_positions, col = colours[["grid"]], lwd = 0.55)
    graphics::abline(v = 0, col = colours[["border"]], lwd = 1.0)

    for (i in seq_len(nrow(panel))) {
      y <- y_positions[[i]]
      contribution_y <- y - 0.17
      total_y <- y + 0.18
      loss_start <- 0
      loss_end <- panel$avoided_loss[[i]]
      regrowth_start <- loss_end
      regrowth_end <- loss_end + panel$regrowth[[i]]
      harvest_end <- panel$harvest[[i]]
      total_end <- panel$total[[i]]

      if (show_uncertainty) {
        graphics::segments(
          panel$total_p025[[i]], total_y, panel$total_p975[[i]], total_y,
          col = colours[["uncertainty"]], lwd = 1.35
        )
        graphics::segments(
          rep(c(panel$total_p025[[i]], panel$total_p975[[i]]), each = 1L),
          total_y - 0.07,
          rep(c(panel$total_p025[[i]], panel$total_p975[[i]]), each = 1L),
          total_y + 0.07,
          col = colours[["uncertainty"]], lwd = 1.15
        )
      }

      if (!same_number(loss_start, loss_end, tolerance = 1e-12)) {
        graphics::rect(
          min(loss_start, loss_end), contribution_y - bar_half_height,
          max(loss_start, loss_end), contribution_y + bar_half_height,
          col = colours[["avoided_loss"]], border = "white", lwd = 0.55
        )
      }
      if (!same_number(regrowth_start, regrowth_end, tolerance = 1e-12)) {
        graphics::rect(
          min(regrowth_start, regrowth_end), contribution_y - bar_half_height,
          max(regrowth_start, regrowth_end), contribution_y + bar_half_height,
          col = colours[["regrowth"]], border = "white", lwd = 0.55
        )
      }
      if (!same_number(harvest_end, total_end, tolerance = 1e-12)) {
        graphics::arrows(
          harvest_end, contribution_y, total_end, contribution_y,
          length = 0.075, angle = 24, code = 2L,
          col = colours[["enduse"]], lwd = 3.0
        )
      }
      graphics::points(
        total_end, total_y, pch = 21, cex = 1.0,
        col = "white", bg = colours[["total"]], lwd = 0.8
      )

      if (total_end >= 0) {
        label_x <- max(total_end, panel$total_p975[[i]]) + 0.012 * raw_span
        label_adj <- c(0, 0.5)
      } else {
        label_x <- min(total_end, panel$total_p025[[i]]) - 0.012 * raw_span
        label_adj <- c(1, 0.5)
      }
      graphics::text(
        label_x, total_y, formatC(total_end, format = "f", digits = 1L),
        adj = label_adj, cex = label_cex, font = 2, col = colours[["text"]]
      )
    }

    graphics::axis(
      1, at = tick_values,
      labels = format(tick_values, trim = TRUE, scientific = FALSE),
      cex.axis = 0.74, col = colours[["border"]], col.axis = colours[["text"]],
      tck = -0.025
    )
    graphics::axis(
      2, at = y_positions, labels = country_order,
      las = 1, tick = FALSE, cex.axis = label_cex,
      col.axis = colours[["text"]], line = -0.35
    )
    graphics::box(col = colours[["border"]], lwd = 0.8)
    graphics::title(
      main = tools::toTitleCase(configuration), line = 1.05,
      font.main = 2, cex.main = 1.02, col.main = colours[["total"]]
    )
  }

  graphics::mtext(
    expression(paste("Annual avoided emissions (MtCO"[2], "e ", yr^{-1}, ")")),
    side = 1, outer = TRUE, line = 0.7, cex = 0.88, col = colours[["text"]]
  )
  graphics::mtext(
    paste0(
      "Country values use spatial incidence. End-use arrows point from Harvest / AGB to Total; ",
      "leftward arrows reduce avoided emissions."
    ),
    side = 1, outer = TRUE, line = 2.25, cex = 0.66, col = colours[["muted"]]
  )
  invisible(path)
}

make_country_compact_table <- function(configuration) {
  rows <- country_mc1_source[
    country_mc1_source$regrowth_mode == configuration,
    , drop = FALSE
  ]
  rows <- rows[order(rows$country_id), , drop = FALSE]
  data.frame(
    Country = sprintf("%s (%s)", rows$country_name, rows$country_iso),
    `Avoided loss` = as.numeric(rows$period_avoided_loss_tco2e) / 1e6,
    Regrowth = as.numeric(rows$period_regrowth_tco2e) / 1e6,
    `Harvest / AGB` = as.numeric(rows$agb_avoided_stage2_tco2e) / 1e6,
    `End-use` = as.numeric(rows$enduse_avoided_tco2e) / 1e6,
    Total = as.numeric(rows$total_avoided_tco2e) / 1e6,
    Annual = as.numeric(rows$total_avoided_tco2e) / (1e6 * reporting_years),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}
country_compact_tables <- lapply(CONFIGURATION_ORDER, make_country_compact_table)
names(country_compact_tables) <- CONFIGURATION_ORDER

table_mc1_path <- file.path(output_dir, "tables", sprintf("table_%s_%s_mc_1.csv", region_slug, period_tag))
table_mc_all_path <- file.path(output_dir, "tables", sprintf("table_%s_%s_mc_all.csv", region_slug, period_tag))
table_mc1_png_path <- sub("\\.csv$", ".png", table_mc1_path)
table_mc_all_png_path <- sub("\\.csv$", ".png", table_mc_all_path)
country_tidy_mc1_path <- file.path(
  output_dir, "tables", sprintf("table_%s_%s_by_country_mc_1.csv", region_slug, period_tag)
)
country_tidy_mc_all_path <- file.path(
  output_dir, "tables", sprintf("table_%s_%s_by_country_mc_all.csv", region_slug, period_tag)
)
country_compact_csv_paths <- setNames(vapply(CONFIGURATION_ORDER, function(configuration) {
  file.path(
    output_dir, "tables",
    sprintf("table_%s_%s_by_country_%s_mc_1.csv", region_slug, period_tag, configuration)
  )
}, character(1)), CONFIGURATION_ORDER)
country_figure_scope <- if (uncertainty_adequate) "mc_all" else "mc_1"
country_figure_statistic <- if (uncertainty_adequate) "mc_all" else "mc1"
country_figure_path <- file.path(
  output_dir, "figures", country_figure_scope,
  sprintf(
    "figure_%s_%s_by_country_contributions_%s.png",
    region_slug, period_tag, country_figure_statistic
  )
)
country_scope_output_path <- file.path(output_dir, "spatial", "country_scope.csv")
country_boundaries_output_path <- file.path(output_dir, "spatial", "country_boundaries.gpkg")

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
  records[[configuration]] <- list(mc1 = c(
    enduse = file.path(emissions_dir, "summary_mc1", "delta_co2_enduse.tif"),
    harvest = file.path(emissions_dir, "summary_mc1", "delta_co2_harvest.tif"),
    total = file.path(emissions_dir, "summary_mc1", "delta_co2.tif")
  ))
  if (uncertainty_adequate) {
    records[[configuration]]$mc_all_mean <- c(
      enduse = file.path(emissions_dir, "enduse", "delta_co2_enduse.tif"),
      harvest = file.path(emissions_dir, "harvest", "delta_co2_mean.tif"),
      total = file.path(emissions_dir, "total", "delta_co2_mean.tif")
    )
    records[[configuration]]$mc_all_sd <- c(
      enduse = file.path(emissions_dir, "enduse", "delta_co2_enduse.tif"),
      harvest = file.path(emissions_dir, "harvest", "delta_co2_sd.tif"),
      total = file.path(emissions_dir, "total", "delta_co2_sd.tif")
    )
  }
  missing <- unlist(records[[configuration]], use.names = FALSE)
  missing <- missing[!file.exists(missing)]
  if (length(missing)) stopf("Missing %s raster sources: %s", configuration, paste(missing, collapse = ", "))
}

# All scalar and raster inputs have passed their preflight checks. Only now is
# the exact manuscript_outputs directory removed and rebuilt.
prepare_output_dir(output_dir, source_dir, overwrite, uncertainty_adequate)
write_csv_utf8(mc1_table, table_mc1_path)
write_table_png(
  mc1_table, table_mc1_png_path,
  sprintf("%s MoFuSS results, %s", region_name, period_tag),
  "First Monte Carlo realization (MC1)"
)
if (uncertainty_adequate) {
  write_csv_utf8(mc_all_table, table_mc_all_path)
  write_table_png(
    mc_all_table, table_mc_all_png_path,
    sprintf("%s MoFuSS results, %s", region_name, period_tag),
    sprintf("Mean %s sample SD across %d Monte Carlo realizations", plus_minus, n_runs)
  )
}
write_csv_utf8(country_mc1_tidy, country_tidy_mc1_path)
if (uncertainty_adequate) {
  write_csv_utf8(country_mc_all_tidy, country_tidy_mc_all_path)
}
for (configuration in CONFIGURATION_ORDER) {
  write_csv_utf8(
    country_compact_tables[[configuration]],
    country_compact_csv_paths[[configuration]]
  )
}
write_country_contribution_figure(
  country_figure_data,
  country_figure_path,
  region_name,
  period_tag,
  n_runs,
  uncertainty_adequate
)
copy_checked(country_scope_path, country_scope_output_path)
terra::writeVector(
  country_boundaries,
  country_boundaries_output_path,
  filetype = "GPKG",
  overwrite = TRUE
)

raster_path <- function(scope, configuration, component, statistic = NULL) {
  suffix <- if (scope == "mc_1") {
    sprintf("%s_%s_%s_%s_mc1_tco2e.tif", region_slug, period_tag, configuration, component)
  } else {
    sprintf("%s_%s_%s_%s_%s_tco2e.tif", region_slug, period_tag, configuration, component, statistic)
  }
  file.path(output_dir, "rasters", scope, suffix)
}

mc1_raster_objects <- list()
mc_all_mean_raster_objects <- list()
mc_all_sd_raster_objects <- list()
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
      stopf("%s %s MC1 raster does not reconcile with the analysis-area scalar.", configuration, component)
    }
    key <- paste(configuration, component, sep = "_")
    mc1_raster_objects[[key]] <- mc1_raster

    if (uncertainty_adequate) {
      mean_destination <- raster_path("mc_all", configuration, component, "mean")
      copy_checked(records[[configuration]]$mc_all_mean[[component]], mean_destination)
      mean_raster <- terra::rast(mean_destination)
      mean_sum <- as.numeric(terra::global(mean_raster, "sum", na.rm = TRUE)[[1L]])
      mean_scalar <- mean(as.numeric(scalar_rows[[scalar_fields[[component]]]]))
      if (abs(mean_sum - mean_scalar) > 0.05) {
        stopf("%s %s mean raster does not reconcile with the analysis-area scalar.", configuration, component)
      }
      mc_all_mean_raster_objects[[key]] <- mean_raster

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
      mc_all_sd_raster_objects[[key]] <- terra::rast(sd_destination)
    }
  }
}

quantile_pair <- function(r) {
  as.numeric(terra::global(r, stats::quantile, probs = c(0.02, 0.98), na.rm = TRUE)[1L, ])
}

# Every manuscript map is display-only in Equal Earth. Native raster products
# remain untouched. Convert extensive native-cell totals to ground-area
# intensity before reprojection so the colour scale remains physically valid.
as_map_intensity <- function(r, aggregate_factor = 4L) {
  ground_area_km2 <- terra::cellSize(
    r, unit = "km", mask = FALSE, transform = TRUE
  )
  intensity <- terra::ifel(ground_area_km2 > 0, r / ground_area_km2, NA)
  if (aggregate_factor > 1L) {
    intensity <- terra::aggregate(
      intensity, fact = aggregate_factor, fun = "mean", na.rm = TRUE
    )
  }
  intensity
}

mc1_intensity_rasters <- lapply(mc1_raster_objects, as_map_intensity)
mc1_plot_reference <- terra::project(
  mc1_intensity_rasters[["capped_total"]], MAP_DISPLAY_CRS,
  method = "bilinear"
)
plot_rasters <- lapply(mc1_intensity_rasters, function(r) {
  terra::project(r, mc1_plot_reference, method = "bilinear")
})

map_scales <- list()
for (component in COMPONENT_ORDER) {
  values <- unlist(lapply(CONFIGURATION_ORDER, function(configuration) {
    quantile_pair(plot_rasters[[paste(configuration, component, sep = "_")]])
  }))
  limit <- max(abs(values), na.rm = TRUE)
  if (!is.finite(limit) || limit <= 0) limit <- 1
  map_scales[[component]] <- c(-limit, limit)
}

draw_country_overlay <- function(raster) {
  boundaries <- terra::project(country_boundaries, terra::crs(raster))
  terra::lines(boundaries, col = "#25313C", lwd = 0.65)
  label_points <- suppressWarnings(terra::centroids(boundaries, inside = TRUE))
  coordinates <- terra::crds(label_points)
  labels <- trimws(as.character(label_points$NAME_0))
  if (!nrow(coordinates) || length(labels) != nrow(coordinates)) {
    stopf("Could not derive country-label points for the manuscript map.")
  }
  plot_limits <- graphics::par("usr")
  dx <- diff(plot_limits[1:2]) * 0.0016
  dy <- diff(plot_limits[3:4]) * 0.0016
  label_cex <- max(0.34, min(0.54, 0.78 / sqrt(nrow(coordinates) / 4)))
  halo_offsets <- rbind(
    c(-dx, 0), c(dx, 0), c(0, -dy), c(0, dy),
    c(-dx, -dy), c(-dx, dy), c(dx, -dy), c(dx, dy)
  )
  for (i in seq_len(nrow(halo_offsets))) {
    graphics::text(
      coordinates[, 1L] + halo_offsets[i, 1L],
      coordinates[, 2L] + halo_offsets[i, 2L],
      labels = labels, cex = label_cex, font = 2, col = "white"
    )
  }
  graphics::text(
    coordinates[, 1L], coordinates[, 2L], labels = labels,
    cex = label_cex, font = 2, col = "#172B4D"
  )
  invisible(NULL)
}

draw_mc1_map_figure <- function() {
  op <- graphics::par(
    mfrow = c(length(CONFIGURATION_ORDER), length(COMPONENT_ORDER)),
    mar = c(1.3, 1.3, 3.1, 4.8), oma = c(4.5, 1.0, 5.1, 1.0),
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
      draw_country_overlay(plot_rasters[[paste(configuration, component, sep = "_")]])
    }
  }
  graphics::mtext(
    sprintf("%s: BAU - CCTS emissions differences, %s", region_name, period_tag),
    side = 3, outer = TRUE, line = 2.7, font = 2, cex = 1.15
  )
  graphics::mtext(
    paste0(
      "First Monte Carlo realization (MC1); capped and uncapped configurations; ",
      "full-period intensity in tCO2e km^-2."
    ),
    side = 3, outer = TRUE, line = 1.0, cex = 0.75
  )
  graphics::mtext(
    "Map projection: WGS 84 / Equal Earth Greenwich (EPSG:8857; equal-area).",
    side = 1, outer = TRUE, line = 2.4, cex = 0.67
  )
  graphics::mtext(
    "Positive BAU - CCTS values indicate avoided emissions; negative values indicate higher emissions under CCTS.",
    side = 1, outer = TRUE, line = 0.9, cex = 0.67
  )
}

figure_path <- file.path(
  output_dir, "figures", "mc_1", sprintf("figure_%s_%s_emissions_maps.png", region_slug, period_tag)
)
grDevices::png(figure_path, width = 2400, height = 1600, res = 300)
draw_mc1_map_figure()
grDevices::dev.off()

mc_all_figure_path <- file.path(
  output_dir, "figures", "mc_all",
  sprintf("figure_%s_%s_emissions_maps_wuncer.png", region_slug, period_tag)
)
if (uncertainty_adequate) {
  sd_components <- c("harvest", "total")
  for (component in sd_components) {
    rasters <- lapply(CONFIGURATION_ORDER, function(configuration) {
      mc_all_sd_raster_objects[[paste(configuration, component, sep = "_")]]
    })
    minima <- vapply(rasters, function(r) {
      as.numeric(terra::global(r, "min", na.rm = TRUE)[[1L]])
    }, numeric(1))
    if (any(minima < -1e-9, na.rm = TRUE)) {
      stopf("%s SD raster contains negative values.", COMPONENT_LABELS[[component]])
    }
  }

  # Stage 2 harvest rasters and the End-use/Total rasters can use different
  # native CRSs. Convert each native-cell total or SD to ground-area intensity,
  # then align display-only copies to one Equal Earth panel grid. Published
  # raster products remain unchanged in their native geometry and units.
  mc_all_map_reference <- as_map_intensity(
    mc_all_mean_raster_objects[["capped_total"]]
  )
  mc_all_plot_reference <- terra::project(
    mc_all_map_reference, MAP_DISPLAY_CRS, method = "bilinear"
  )
  align_mc_all_plot_raster <- function(r) {
    terra::project(
      as_map_intensity(r), mc_all_plot_reference, method = "bilinear"
    )
  }
  mc_all_mean_plot_rasters <- lapply(
    mc_all_mean_raster_objects, align_mc_all_plot_raster
  )
  mc_all_sd_plot_rasters <- lapply(
    mc_all_sd_raster_objects, align_mc_all_plot_raster
  )
  mean_map_scales <- list()
  for (component in COMPONENT_ORDER) {
    values <- unlist(lapply(CONFIGURATION_ORDER, function(configuration) {
      quantile_pair(mc_all_mean_plot_rasters[[
        paste(configuration, component, sep = "_")
      ]])
    }))
    limit <- max(abs(values), na.rm = TRUE)
    if (!is.finite(limit) || limit <= 0) limit <- 1
    mean_map_scales[[component]] <- c(-limit, limit)
  }
  plot_geometry_ok <- vapply(
    c(mc_all_mean_plot_rasters, mc_all_sd_plot_rasters),
    function(r) terra::compareGeom(
      r, mc_all_plot_reference, lyrs = FALSE, crs = TRUE, ext = TRUE,
      rowcol = TRUE, res = TRUE, stopOnError = FALSE
    ),
    logical(1)
  )
  if (!all(plot_geometry_ok)) {
    stopf("Could not align all MC-all rasters to one plotting geometry.")
  }
  mc_all_panel_layout <- data.frame(
    statistic = c("mean", "sd", "mean", "mean", "sd"),
    component = c("harvest", "harvest", "enduse", "total", "total"),
    stringsAsFactors = FALSE
  )

  draw_mc_all_map_figure <- function() {
    op <- graphics::par(
      mfrow = c(length(CONFIGURATION_ORDER), 5L),
      mar = c(1.3, 1.3, 3.1, 4.8), oma = c(3.5, 1.0, 5.1, 1.0),
      xaxs = "i", yaxs = "i"
    )
    on.exit(graphics::par(op), add = TRUE)
    colours <- grDevices::hcl.colors(255, "Blue-Red 3")
    for (configuration in CONFIGURATION_ORDER) {
      for (panel_index in seq_len(nrow(mc_all_panel_layout))) {
        statistic <- mc_all_panel_layout$statistic[[panel_index]]
        component <- mc_all_panel_layout$component[[panel_index]]
        statistic_label <- if (statistic == "sd") "SD" else "Mean"
        key <- paste(configuration, component, sep = "_")
        raster <- if (statistic == "mean") {
          mc_all_mean_plot_rasters[[key]]
        } else {
          mc_all_sd_plot_rasters[[key]]
        }
        terra::plot(
          raster,
          col = colours, range = mean_map_scales[[component]],
          axes = FALSE, maxcell = 50000,
          main = sprintf(
            "%s: %s %s", tools::toTitleCase(configuration),
            statistic_label,
            COMPONENT_LABELS[[component]]
          ),
          cex.main = 0.82, plg = list(cex = 0.58)
        )
        draw_country_overlay(raster)
      }
    }
    graphics::mtext(
      sprintf(
        "%s: BAU - CCTS emissions differences, %s",
        region_name, period_tag
      ),
      side = 3, outer = TRUE, line = 2.7, font = 2, cex = 1.15
    )
    graphics::mtext(
      sprintf(
        paste0(
          "Monte Carlo mean and sample SD across %d realizations; capped and ",
          "uncapped configurations; full-period intensity in tCO2e km^-2."
        ),
        n_runs
      ),
      side = 3, outer = TRUE, line = 1.0, cex = 0.75
    )
    graphics::mtext(
      paste0(
        "Map projection: WGS 84 / Equal Earth Greenwich (EPSG:8857; equal-area). Mean maps show full-period intensity ",
        "(positive values indicate avoided emissions). ",
        "Each adjacent Mean/SD pair uses the same palette and scale. End-use SD is zero ",
        "across runs and is omitted."
      ),
      side = 1, outer = TRUE, line = 1.2, cex = 0.67
    )
  }

  grDevices::png(mc_all_figure_path, width = 4000, height = 1600, res = 300)
  draw_mc_all_map_figure()
  grDevices::dev.off()
}

expected_files <- c(
  file.path("figures", "mc_1", basename(figure_path)),
  file.path("tables", basename(table_mc1_path)),
  file.path("tables", basename(table_mc1_png_path)),
  file.path("tables", basename(country_tidy_mc1_path)),
  file.path("tables", basename(country_compact_csv_paths)),
  file.path("figures", country_figure_scope, basename(country_figure_path)),
  file.path("spatial", basename(country_scope_output_path)),
  file.path("spatial", basename(country_boundaries_output_path))
)
if (uncertainty_adequate) {
  expected_files <- c(
    expected_files,
    file.path("figures", "mc_all", basename(mc_all_figure_path)),
    file.path("tables", basename(table_mc_all_path)),
    file.path("tables", basename(table_mc_all_png_path)),
    file.path("tables", basename(country_tidy_mc_all_path))
  )
}
for (configuration in CONFIGURATION_ORDER) {
  for (component in COMPONENT_ORDER) {
    expected_files <- c(
      expected_files,
      file.path("rasters", "mc_1", basename(raster_path("mc_1", configuration, component)))
    )
    if (uncertainty_adequate) {
      expected_files <- c(
        expected_files,
        file.path("rasters", "mc_all", basename(raster_path("mc_all", configuration, component, "mean"))),
        file.path("rasters", "mc_all", basename(raster_path("mc_all", configuration, component, "sd")))
      )
    }
  }
}
actual_files <- list.files(output_dir, recursive = TRUE, all.files = FALSE)
if (!setequal(gsub("\\\\", "/", actual_files), gsub("\\\\", "/", expected_files))) {
  stopf("Final package inventory differs from the expected %d files.", length(expected_files))
}

if (!uncertainty_adequate) {
  warning(sprintf(
    paste0(
      "Only %d %s available (<%d); the manuscript package contains MC1 ",
      "outputs only. MC-all tables, figures, and rasters were omitted."
    ),
    n_runs, if (n_runs == 1L) "run is" else "runs are", min_uncertainty_runs
  ), call. = FALSE)
}

cat(sprintf("MANUSCRIPT_PACKAGE_CREATED=%s\n", output_dir))
cat(sprintf("SCRIPT_VERSION=%d\n", SCRIPT_VERSION))
cat(sprintf("REGION=%s\n", region_iso))
cat(sprintf("ANALYSIS_AREA_KIND=%s\n", region_kind))
cat(sprintf("COUNTRY_UNIT_COUNT=%d\n", nrow(country_scope)))
cat(sprintf("PERIOD=%s\n", period_tag))
cat(sprintf("MC_RUNS=%d\n", n_runs))
cat(sprintf("UNCERTAINTY_ADEQUATE=%s\n", uncertainty_adequate))
cat(sprintf("FILE_COUNT=%d\n", length(actual_files)))
