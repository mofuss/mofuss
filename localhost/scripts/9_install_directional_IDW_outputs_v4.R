# SPDX-License-Identifier: Apache-2.0
#
# Copyright 2025-2027 Universidad Nacional Autónoma de México
# and Stockholm Environment Institute
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# MoFuSS ----
# Script: 9_install_directional_IDW_outputs_v4.R
# Version: 4
# Date: Sep 2026
# Execution: Source from RStudio only after every CostDistance_IDW HC job has
# completed and its output directory has been returned to HC_jobs.
#
# Purpose: Validate, assemble and install the decennial directional IDW outputs
# consumed by the Dinamica EGO model. W origin-country components and V
# directional components remain separate for annual, post-eligibility
# normalization in MoFuSS v11. Summed W and V rasters are installed only for
# Patcher ranking and diagnostics. Component NA cells are treated as zero only
# while those ranking surfaces are assembled.
#
# Expected HC output layout:
#   In/DemandScenarios/HC_jobs/idw_<JobID>/IDW_C++_fw_<w|v><NN>.tif
#
# Installed outputs:
#   In/IDW_C++_fw_w<NN>.tif
#   In/IDW_C++_fw_v<NN>.tif
#   In/W_origin_components/IDW_C++_fw_w<CCC>_<NN>.tif
#   In/V_origin_components/IDW_C++_fw_v<CCC>_<NN>.tif
#   In/DemandScenarios/W_origin_demand<SS>.csv
#   In/DemandScenarios/V_origin_demand<SS>.csv
#   In/DemandScenarios/W_origin_component_index.csv
#   In/DemandScenarios/V_origin_component_index.csv
#   In/DemandScenarios/HC_jobs/HC_IDW_install_manifest.csv
#   In/DemandScenarios/HC_jobs/README_IDW_INSTALL.txt
#
# This script is fail-closed. It refuses incomplete jobs, geometry mismatches,
# invalid values, source-domain leakage and pre-existing installed outputs.
# It never runs CostDistance_IDW and never overwrites an installed IDW.

suppressPackageStartupMessages(library(terra))

.idw6f_stop <- function(...) {
  stop(..., call. = FALSE)
}

.idw6f_normalize <- function(path, must_work = TRUE) {
  normalizePath(path, winslash = "/", mustWork = must_work)
}

.idw6f_sha256 <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    .idw6f_stop("Package `digest` is required to record SHA-256 checksums.")
  }
  unname(digest::digest(path, algo = "sha256", file = TRUE))
}

.idw6f_resolve_run_root <- function() {
  inherited_countrydir <- get0("countrydir", inherits = TRUE, ifnotfound = NULL)
  candidates <- unique(c(
    if (!is.null(inherited_countrydir)) as.character(inherited_countrydir) else NULL,
    getwd()
  ))
  candidates <- candidates[nzchar(candidates)]
  required_relative <- file.path(
    "In", "DemandScenarios", "HC_jobs", "HC_job_manifest_idw_ready.csv"
  )
  matches <- candidates[file.exists(file.path(candidates, required_relative))]
  if (length(matches) == 0L) {
    .idw6f_stop(
      "Could not locate a MoFuSS run containing ", required_relative,
      ". Define `countrydir` or set the working directory to the run root."
    )
  }
  .idw6f_normalize(matches[[1L]])
}

.idw6f_assert_single_raster <- function(raster, label) {
  if (terra::nlyr(raster) != 1L) {
    .idw6f_stop(label, " must contain exactly one raster layer.")
  }
  invisible(TRUE)
}

.idw6f_assert_same_geometry <- function(x, y, x_label, y_label) {
  same_geometry <- isTRUE(terra::compareGeom(
    x, y,
    lyrs = FALSE,
    crs = TRUE,
    ext = TRUE,
    rowcol = TRUE,
    res = TRUE,
    stopOnError = FALSE
  ))
  if (!same_geometry) {
    .idw6f_stop(x_label, " does not match the geometry of ", y_label, ".")
  }
  invisible(TRUE)
}

.idw6f_safe_job_id <- function(job_id) {
  job_id <- trimws(as.character(job_id))
  if (length(job_id) != 1L || is.na(job_id) ||
      !grepl("^[A-Za-z0-9_]+$", job_id)) {
    .idw6f_stop("Unsafe or invalid HC JobID: ", paste(job_id, collapse = ", "))
  }
  job_id
}

.idw6f_scalar_integer <- function(value, label) {
  parsed <- suppressWarnings(as.integer(value))
  if (length(parsed) != 1L || is.na(parsed) || parsed < 1L ||
      !isTRUE(all.equal(as.numeric(value), as.numeric(parsed)))) {
    .idw6f_stop(label, " must be one positive integer.")
  }
  parsed
}

.idw6f_raster_stats <- function(raster) {
  values <- terra::values(raster, mat = FALSE)
  finite <- is.finite(values)
  finite_values <- values[finite]
  if (length(finite_values) == 0L) {
    return(list(
      non_na = 0,
      positive = 0,
      minimum = NA_real_,
      maximum = NA_real_,
      sum = NA_real_
    ))
  }
  list(
    non_na = sum(!is.na(values)),
    positive = sum(finite_values > 0),
    minimum = min(finite_values),
    maximum = max(finite_values),
    sum = sum(finite_values)
  )
}

.idw6f_validate_stats_for_demand <- function(stats, demand_tons, label) {
  if (length(demand_tons) != 1L || is.na(demand_tons) ||
      !is.finite(demand_tons) || demand_tons < 0) {
    .idw6f_stop(label, " has an invalid corresponding demand total.")
  }
  if (stats$non_na == 0L || !is.finite(stats$minimum) ||
      !is.finite(stats$maximum) || !is.finite(stats$sum) ||
      stats$minimum < 0) {
    .idw6f_stop(label, " has invalid raster statistics.")
  }

  zero_demand <- demand_tons == 0
  if (zero_demand) {
    if (stats$positive != 0L || stats$minimum != 0 ||
        stats$maximum != 0 || stats$sum != 0) {
      .idw6f_stop(
        label,
        " must be an all-zero raster because its corresponding demand is zero."
      )
    }
  } else if (stats$positive == 0L || stats$maximum <= 0) {
    .idw6f_stop(label, " contains no positive IDW values for positive demand.")
  }
  invisible(zero_demand)
}

.idw6f_validate_component <- function(
    raster_path,
    source_mask,
    template,
    label,
    demand_tons) {
  if (!file.exists(raster_path)) {
    .idw6f_stop(label, " is missing: ", raster_path)
  }
  raster <- terra::rast(raster_path)
  .idw6f_assert_single_raster(raster, label)
  .idw6f_assert_same_geometry(raster, template, label, "channel template")
  .idw6f_assert_same_geometry(source_mask, template, paste0(label, " source mask"), "channel template")

  raster_values <- terra::values(raster, mat = FALSE)
  mask_values <- terra::values(source_mask, mat = FALSE)
  allowed <- !is.na(mask_values) & mask_values == 1
  if (!any(allowed)) {
    .idw6f_stop(label, " source mask contains no permitted cells.")
  }
  if (any(!is.finite(raster_values[!is.na(raster_values)]))) {
    .idw6f_stop(label, " contains non-finite values.")
  }
  if (any(raster_values < 0, na.rm = TRUE)) {
    .idw6f_stop(label, " contains negative values.")
  }
  if (any(is.na(raster_values[allowed]))) {
    .idw6f_stop(label, " contains NA values inside its permitted source domain.")
  }
  if (any(raster_values[!allowed] > 0, na.rm = TRUE)) {
    .idw6f_stop(label, " contains positive values outside its permitted source domain.")
  }
  stats <- .idw6f_raster_stats(raster)
  .idw6f_validate_stats_for_demand(stats, demand_tons, label)
  stats
}

.idw6f_validate_manifest <- function(manifest) {
  required_columns <- c(
    "JobID", "Channel", "Status", "PeriodStart", "PeriodEnd",
    "YearStart", "YearEnd", "SourceDomainMask", "CombineOperation",
    "OutputRole", "DemandISO3", "AllowedSourceISO3", "DirectionRule",
    "DemandTable"
  )
  missing_columns <- setdiff(required_columns, names(manifest))
  if (length(missing_columns) > 0L) {
    .idw6f_stop(
      "IDW-ready manifest is missing required column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }
  if (nrow(manifest) == 0L) {
    .idw6f_stop("IDW-ready manifest contains no jobs.")
  }
  manifest$JobID <- vapply(manifest$JobID, .idw6f_safe_job_id, character(1))
  manifest$Channel <- toupper(trimws(as.character(manifest$Channel)))
  manifest$Status <- toupper(trimws(as.character(manifest$Status)))
  if (anyDuplicated(manifest$JobID)) {
    .idw6f_stop("IDW-ready manifest contains duplicate JobID values.")
  }
  if (any(!manifest$Channel %in% c("W", "V"))) {
    .idw6f_stop("Every IDW-ready job must have channel W or V.")
  }
  if (any(manifest$Status != "IDW_READY")) {
    .idw6f_stop("Every manifest job must have Status=IDW_READY before installation.")
  }
  if (sum(manifest$Channel == "W") < 1L) {
    .idw6f_stop("Directional installation requires at least one W job.")
  }
  if (sum(manifest$Channel == "V") < 1L) {
    .idw6f_stop("Directional installation requires at least one V job.")
  }
  w_operations <- trimws(as.character(
    manifest$CombineOperation[manifest$Channel == "W"]
  ))
  if (any(w_operations != "runtime_normalize_by_origin_then_sum")) {
    .idw6f_stop(
      "W jobs must declare CombineOperation=",
      "runtime_normalize_by_origin_then_sum."
    )
  }
  w_iso3 <- trimws(as.character(
    manifest$DemandISO3[manifest$Channel == "W"]
  ))
  if (any(!grepl("^[A-Z]{3}$", w_iso3)) || anyDuplicated(w_iso3)) {
    .idw6f_stop(
      "Every W job must contain one unique three-letter DemandISO3 value."
    )
  }
  v_operations <- trimws(as.character(
    manifest$CombineOperation[manifest$Channel == "V"]
  ))
  allowed_v_operations <- if (sum(manifest$Channel == "V") == 1L) {
    c(
      "runtime_normalize_by_origin_then_sum",
      "pixelwise_sum_by_year",
      "use_directly"
    )
  } else {
    c("runtime_normalize_by_origin_then_sum", "pixelwise_sum_by_year")
  }
  if (any(!v_operations %in% allowed_v_operations)) {
    .idw6f_stop(
      "V jobs must declare component-preserving runtime normalization. ",
      "Legacy v2 pixelwise_sum_by_year manifests are accepted only so their ",
      "existing HC outputs can be upgraded without rerunning IDW."
    )
  }
  if (any(v_operations != "runtime_normalize_by_origin_then_sum")) {
    message(
      "Accepting legacy V combination metadata; v3 will preserve and ",
      "runtime-normalize every V component separately."
    )
  }
  v_iso3_groups <- strsplit(
    trimws(as.character(manifest$DemandISO3[manifest$Channel == "V"])),
    ";",
    fixed = TRUE
  )
  v_iso3 <- trimws(unlist(v_iso3_groups, use.names = FALSE))
  if (length(v_iso3) == 0L || any(!grepl("^[A-Z]{3}$", v_iso3)) ||
      anyDuplicated(v_iso3)) {
    .idw6f_stop(
      "Every V demand country must occur in exactly one directional component."
    )
  }

  integer_columns <- c("PeriodStart", "PeriodEnd", "YearStart", "YearEnd")
  for (column in integer_columns) {
    manifest[[column]] <- vapply(
      manifest[[column]], .idw6f_scalar_integer, integer(1), label = column
    )
  }
  for (column in integer_columns) {
    if (length(unique(manifest[[column]])) != 1L) {
      .idw6f_stop("All jobs must share one ", column, " value.")
    }
  }
  period_start <- manifest$PeriodStart[[1L]]
  period_end <- manifest$PeriodEnd[[1L]]
  year_start <- manifest$YearStart[[1L]]
  year_end <- manifest$YearEnd[[1L]]
  if (period_end < period_start || year_end < year_start ||
      (period_end - period_start) != (year_end - year_start)) {
    .idw6f_stop("Manifest period and year ranges are inconsistent.")
  }
  if ((period_end - period_start) %% 10L != 0L) {
    .idw6f_stop("The model requires a complete 10-year IDW sequence.")
  }
  manifest
}

.idw6f_component_path <- function(hc_root, output_prefix, job_id, channel, period) {
  file.path(
    hc_root,
    paste0(output_prefix, job_id),
    sprintf("IDW_C++_fw_%s%02d.tif", tolower(channel), period)
  )
}

.idw6f_combine_components <- function(
    component_paths,
    mask_paths,
    template,
    label) {
  component_stack <- terra::rast(component_paths)
  .idw6f_assert_same_geometry(
    component_stack[[1L]], template, paste0(label, " component stack"),
    paste0(label, " template")
  )
  combined <- terra::app(component_stack, sum, na.rm = TRUE)

  mask_stack <- terra::rast(mask_paths)
  permitted <- terra::app(
    mask_stack,
    function(values) as.integer(any(values == 1, na.rm = TRUE))
  )
  terra::ifel(permitted == 1, combined, NA)
}

.idw6f_read_annual_demand <- function(path, channel, label) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    .idw6f_stop("Package `data.table` is required to read demand components.")
  }
  if (!file.exists(path)) {
    .idw6f_stop(label, " does not exist: ", path)
  }
  demand <- data.table::fread(path, showProgress = FALSE)
  expected_pattern <- paste0("^([0-9]{4})_fw_", tolower(channel), "$")
  demand_columns <- names(demand)[grepl(expected_pattern, names(demand))]
  if (!identical(names(demand)[[1L]], "ID") || length(demand_columns) == 0L ||
      !all(vapply(demand[, ..demand_columns], is.numeric, logical(1)))) {
    .idw6f_stop(label, " has an invalid annual demand schema: ", path)
  }
  years <- as.integer(sub(expected_pattern, "\\1", demand_columns))
  if (!identical(years, seq.int(min(years), max(years)))) {
    .idw6f_stop(label, " demand years must be ordered and contiguous.")
  }
  totals <- colSums(as.matrix(demand[, ..demand_columns]), na.rm = TRUE)
  if (any(!is.finite(totals)) || any(totals < 0)) {
    .idw6f_stop(label, " contains invalid annual demand totals.")
  }
  names(totals) <- years
  totals
}

.idw6f_read_lookup_total <- function(path, label) {
  lookup <- read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    strip.white = TRUE
  )
  if (!identical(names(lookup), c("Key", "Value")) ||
      !is.numeric(lookup$Value) || any(!is.finite(lookup$Value)) ||
      any(lookup$Value < 0)) {
    .idw6f_stop(label, " has an invalid Key/Value schema: ", path)
  }
  sum(lookup$Value)
}

.idw6f_demand_equal <- function(x, y) {
  isTRUE(all.equal(
    as.numeric(x),
    as.numeric(y),
    tolerance = 1e-8,
    check.attributes = FALSE
  ))
}

.idw6f_write_raster <- function(raster, filename) {
  terra::writeRaster(
    raster,
    filename,
    overwrite = FALSE,
    datatype = "FLT4S",
    NAflag = -9999,
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3", "TILED=YES")
  )
}

install_directional_idw_outputs <- function(
    run_root = .idw6f_resolve_run_root(),
    output_prefix = "idw_",
    dry_run = FALSE) {
  if (length(dry_run) != 1L || is.na(dry_run) || !is.logical(dry_run)) {
    .idw6f_stop("dry_run must be TRUE or FALSE.")
  }
  if (length(output_prefix) != 1L || is.na(output_prefix) ||
      !grepl("^[A-Za-z0-9_]+$", output_prefix)) {
    .idw6f_stop("output_prefix must contain only letters, numbers and underscores.")
  }
  run_root <- .idw6f_normalize(run_root)
  in_root <- file.path(run_root, "In")
  hc_root <- file.path(in_root, "DemandScenarios", "HC_jobs")
  manifest_path <- file.path(hc_root, "HC_job_manifest_idw_ready.csv")
  if (!file.exists(manifest_path)) {
    .idw6f_stop("IDW-ready manifest does not exist: ", manifest_path)
  }
  manifest <- read.csv(
    manifest_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
  manifest <- .idw6f_validate_manifest(manifest)

  period_start <- manifest$PeriodStart[[1L]]
  period_end <- manifest$PeriodEnd[[1L]]
  year_start <- manifest$YearStart[[1L]]
  periods <- seq.int(period_start, period_end, by = 10L)
  years <- year_start + periods - period_start
  if (any(periods > 99L)) {
    .idw6f_stop("Two-digit IDW suffixes cannot represent periods above 99.")
  }

  templates <- list(
    W = terra::rast(file.path(in_root, "fricc_w.tif")),
    V = terra::rast(file.path(in_root, "fricc_v.tif"))
  )
  lapply(names(templates), function(channel) {
    .idw6f_assert_single_raster(templates[[channel]], paste0(channel, " template"))
  })
  .idw6f_assert_same_geometry(templates$W, templates$V, "W template", "V template")

  masks <- vector("list", nrow(manifest))
  names(masks) <- manifest$JobID
  mask_paths <- character(nrow(manifest))
  names(mask_paths) <- manifest$JobID
  for (row_index in seq_len(nrow(manifest))) {
    mask_path <- as.character(manifest$SourceDomainMask[[row_index]])
    if (is.na(mask_path) || !file.exists(mask_path)) {
      .idw6f_stop(
        "Source-domain mask is missing for ", manifest$JobID[[row_index]],
        ": ", mask_path
      )
    }
    mask_paths[[manifest$JobID[[row_index]]]] <- .idw6f_normalize(mask_path)
    masks[[manifest$JobID[[row_index]]]] <- terra::rast(mask_path)
    .idw6f_assert_single_raster(
      masks[[manifest$JobID[[row_index]]]],
      paste0("Source-domain mask for ", manifest$JobID[[row_index]])
    )
  }

  annual_years <- seq.int(year_start, manifest$YearEnd[[1L]])
  annual_periods <- annual_years - year_start + period_start
  component_demand_by_job <- setNames(
    lapply(seq_len(nrow(manifest)), function(row_index) {
      job_id <- manifest$JobID[[row_index]]
      channel <- manifest$Channel[[row_index]]
      totals <- .idw6f_read_annual_demand(
        as.character(manifest$DemandTable[[row_index]]),
        channel,
        paste0(channel, " origin component ", job_id)
      )
      if (!identical(as.integer(names(totals)), annual_years)) {
        .idw6f_stop(
          channel, " origin component ", job_id,
          " does not cover the manifest's complete annual period."
        )
      }
      totals
    }),
    manifest$JobID
  )

  component_rows <- list()
  component_paths <- list()
  row_counter <- 0L
  message("MoFuSS run: ", run_root)
  message(
    "Validating ", nrow(manifest), " HC job(s) for periods ",
    paste(sprintf("%02d", periods), collapse = ", "), "."
  )
  for (row_index in seq_len(nrow(manifest))) {
    job_id <- manifest$JobID[[row_index]]
    channel <- manifest$Channel[[row_index]]
    job_paths <- vapply(
      periods,
      function(period) .idw6f_component_path(
        hc_root, output_prefix, job_id, channel, period
      ),
      character(1)
    )
    component_paths[[job_id]] <- job_paths
    for (period_index in seq_along(periods)) {
      label <- paste0("HC output ", job_id, " period ", sprintf("%02d", periods[[period_index]]))
      demand_tons <- unname(component_demand_by_job[[job_id]][[
        as.character(years[[period_index]])
      ]])
      stats <- .idw6f_validate_component(
        job_paths[[period_index]],
        masks[[job_id]],
        templates[[channel]],
        label,
        demand_tons
      )
      row_counter <- row_counter + 1L
      component_rows[[row_counter]] <- data.frame(
        JobID = job_id,
        Channel = channel,
        Period = periods[[period_index]],
        Year = years[[period_index]],
        DemandTons = demand_tons,
        ZeroDemand = demand_tons == 0,
        SourcePath = .idw6f_normalize(job_paths[[period_index]]),
        SourceSHA256 = .idw6f_sha256(job_paths[[period_index]]),
        NonNACells = stats$non_na,
        PositiveCells = stats$positive,
        Minimum = stats$minimum,
        Maximum = stats$maximum,
        Sum = stats$sum,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
    message("  validated ", job_id)
  }
  components <- do.call(rbind, component_rows)

  w_rows <- which(manifest$Channel == "W")
  w_rows <- w_rows[order(manifest$DemandISO3[w_rows], manifest$JobID[w_rows])]
  w_jobs <- manifest$JobID[w_rows]
  w_iso3 <- trimws(as.character(manifest$DemandISO3[w_rows]))
  v_rows <- which(manifest$Channel == "V")
  v_priority <- ifelse(
    manifest$DirectionRule[v_rows] == "importer_demand_regional_sources",
    0L,
    1L
  )
  v_rows <- v_rows[order(
    v_priority,
    manifest$DemandISO3[v_rows],
    manifest$JobID[v_rows]
  )]
  v_jobs <- manifest$JobID[v_rows]
  v_iso3 <- trimws(as.character(manifest$DemandISO3[v_rows]))

  read_component_demand <- function(jobs) {
    demand_by_job <- unname(component_demand_by_job[jobs])
    demand_matrix <- do.call(rbind, demand_by_job)
    rownames(demand_matrix) <- jobs
    colnames(demand_matrix) <- annual_years
    demand_matrix
  }
  w_demand_matrix <- read_component_demand(w_jobs)
  v_demand_matrix <- read_component_demand(v_jobs)

  # Each component table must reconstruct the exact regional trajectory used by
  # the model. This protects both channels from dropped or duplicated origins.
  validate_regional_demand <- function(channel, demand_matrix) {
    for (year_index in seq_along(annual_years)) {
      regional_lookup_path <- file.path(
        in_root,
        "DemandScenarios",
        sprintf(
          "fwuse_%s_ext_fwdef%02d.csv",
          channel,
          annual_periods[[year_index]]
        )
      )
      if (!file.exists(regional_lookup_path)) {
        .idw6f_stop(
          "Regional ", channel, " demand lookup is missing: ",
          regional_lookup_path
        )
      }
      regional_total <- .idw6f_read_lookup_total(
        regional_lookup_path,
        paste0("Regional ", channel, " demand for ", annual_years[[year_index]])
      )
      component_total <- sum(demand_matrix[, year_index])
      if (!.idw6f_demand_equal(regional_total, component_total)) {
        .idw6f_stop(
          channel, " origin components do not reconstruct regional demand for ",
          annual_years[[year_index]], ": components=", component_total,
          "; regional=", regional_total, "."
        )
      }
    }
  }
  validate_regional_demand("W", w_demand_matrix)
  validate_regional_demand("V", v_demand_matrix)

  component_index_table <- function(rows, jobs, demand_iso3, demand_matrix) {
    data.frame(
      ComponentIndex = seq_along(jobs),
      DemandISO3 = demand_iso3,
      JobID = jobs,
      DirectionRule = as.character(manifest$DirectionRule[rows]),
      AllowedSourceISO3 = as.character(manifest$AllowedSourceISO3[rows]),
      FirstYearDemandTons = as.numeric(demand_matrix[, 1L]),
      LastYearDemandTons = as.numeric(demand_matrix[, ncol(demand_matrix)]),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  w_component_index <- component_index_table(
    w_rows, w_jobs, w_iso3, w_demand_matrix
  )
  v_component_index <- component_index_table(
    v_rows, v_jobs, v_iso3, v_demand_matrix
  )

  w_component_root <- file.path(in_root, "W_origin_components")
  v_component_root <- file.path(in_root, "V_origin_components")
  top_target_paths <- unlist(lapply(
    c("w", "v"),
    function(channel) file.path(
      in_root,
      sprintf("IDW_C++_fw_%s%02d.tif", channel, periods)
    )
  ), use.names = FALSE)
  w_component_target_paths <- unlist(lapply(
    seq_along(w_jobs),
    function(component_index) file.path(
      w_component_root,
      sprintf("IDW_C++_fw_w%03d_%02d.tif", component_index, periods)
    )
  ), use.names = FALSE)
  v_component_target_paths <- unlist(lapply(
    seq_along(v_jobs),
    function(component_index) file.path(
      v_component_root,
      sprintf("IDW_C++_fw_v%03d_%02d.tif", component_index, periods)
    )
  ), use.names = FALSE)
  w_demand_target_paths <- file.path(
    in_root,
    "DemandScenarios",
    sprintf("W_origin_demand%02d.csv", annual_periods)
  )
  v_demand_target_paths <- file.path(
    in_root,
    "DemandScenarios",
    sprintf("V_origin_demand%02d.csv", annual_periods)
  )
  w_component_index_path <- file.path(
    in_root, "DemandScenarios", "W_origin_component_index.csv"
  )
  v_component_index_path <- file.path(
    in_root, "DemandScenarios", "V_origin_component_index.csv"
  )
  w_demand_audit_path <- file.path(
    hc_root, "W_origin_demand_manifest.csv"
  )
  v_demand_audit_path <- file.path(
    hc_root, "V_origin_demand_manifest.csv"
  )
  audit_path <- file.path(hc_root, "HC_IDW_install_manifest.csv")
  readme_path <- file.path(hc_root, "README_IDW_INSTALL.txt")
  all_install_paths <- c(
    top_target_paths,
    w_component_target_paths,
    v_component_target_paths,
    w_demand_target_paths,
    v_demand_target_paths,
    w_component_index_path,
    v_component_index_path,
    w_demand_audit_path,
    v_demand_audit_path,
    audit_path,
    readme_path
  )
  existing <- all_install_paths[file.exists(all_install_paths)]
  if (!dry_run && length(existing) > 0L) {
    .idw6f_stop(
      "Refusing to overwrite existing installed IDW product(s):\n",
      paste(.idw6f_normalize(existing), collapse = "\n")
    )
  } else if (dry_run && length(existing) > 0L) {
    message(
      "Dry run is validating inputs beside ", length(existing),
      " existing installed product(s); no file will be overwritten."
    )
  }

  prospective_rows <- list()
  staged_paths <- character()
  staging_root <- NULL
  if (!dry_run) {
    staging_root <- tempfile("mofuss_6f_install_")
    if (!dir.create(staging_root, recursive = TRUE)) {
      .idw6f_stop("Could not create staging directory: ", staging_root)
    }
    on.exit(unlink(staging_root, recursive = TRUE, force = TRUE), add = TRUE)
  }

  for (period_index in seq_along(periods)) {
    period <- periods[[period_index]]
    year <- years[[period_index]]
    suffix <- sprintf("%02d", period)

    w_sources <- vapply(
      w_jobs,
      function(job_id) component_paths[[job_id]][[period_index]],
      character(1)
    )
    w_masks <- unname(mask_paths[w_jobs])

    for (component_index in seq_along(w_jobs)) {
      w_source <- w_sources[[component_index]]
      w_component_target <- file.path(
        w_component_root,
        sprintf("IDW_C++_fw_w%03d_%s.tif", component_index, suffix)
      )
      w_component_raster <- terra::rast(w_source)
      w_component_stats <- .idw6f_raster_stats(w_component_raster)
      w_component_stage <- if (dry_run) {
        w_source
      } else {
        file.path(
          staging_root,
          sprintf("IDW_C++_fw_w%03d_%s.tif", component_index, suffix)
        )
      }
      if (!dry_run && !isTRUE(file.copy(
        w_source,
        w_component_stage,
        overwrite = FALSE
      ))) {
        .idw6f_stop("Could not stage W component output: ", w_source)
      }
      if (!dry_run) staged_paths <- c(staged_paths, w_component_stage)
      prospective_rows[[length(prospective_rows) + 1L]] <- data.frame(
        Channel = "W_COMPONENT",
        Period = period,
        Year = year,
        DemandTons = unname(w_demand_matrix[
          component_index, as.character(year)
        ]),
        ZeroDemand = unname(w_demand_matrix[
          component_index, as.character(year)
        ]) == 0,
        ComponentIndex = component_index,
        DemandISO3 = w_iso3[[component_index]],
        InstallOperation = "preserve_for_runtime_origin_normalization",
        ComponentJobs = w_jobs[[component_index]],
        ComponentPaths = .idw6f_normalize(w_source),
        ComponentSHA256 = .idw6f_sha256(w_source),
        TargetPath = .idw6f_normalize(w_component_target, must_work = FALSE),
        OutputSHA256 = .idw6f_sha256(w_component_stage),
        NonNACells = w_component_stats$non_na,
        PositiveCells = w_component_stats$positive,
        Minimum = w_component_stats$minimum,
        Maximum = w_component_stats$maximum,
        Sum = w_component_stats$sum,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }

    w_combined <- .idw6f_combine_components(
      w_sources,
      w_masks,
      templates$W,
      "W"
    )
    w_target <- file.path(in_root, paste0("IDW_C++_fw_w", suffix, ".tif"))
    w_stats <- .idw6f_raster_stats(w_combined)
    w_period_demand <- sum(w_demand_matrix[, as.character(year)])
    .idw6f_validate_stats_for_demand(
      w_stats,
      w_period_demand,
      paste0("Combined W ranking output for period ", suffix)
    )
    w_stage <- if (dry_run) NULL else file.path(staging_root, basename(w_target))
    if (!dry_run) {
      .idw6f_write_raster(w_combined, w_stage)
      staged_paths <- c(staged_paths, w_stage)
    }
    prospective_rows[[length(prospective_rows) + 1L]] <- data.frame(
      Channel = "W",
      Period = period,
      Year = year,
      DemandTons = w_period_demand,
      ZeroDemand = w_period_demand == 0,
      ComponentIndex = NA_integer_,
      DemandISO3 = paste(w_iso3, collapse = ";"),
      InstallOperation = "pixelwise_sum_W_components_for_ranking_only",
      ComponentJobs = paste(w_jobs, collapse = ";"),
      ComponentPaths = paste(
        vapply(w_sources, .idw6f_normalize, character(1)),
        collapse = ";"
      ),
      ComponentSHA256 = paste(
        vapply(w_sources, .idw6f_sha256, character(1)),
        collapse = ";"
      ),
      TargetPath = .idw6f_normalize(w_target, must_work = FALSE),
      OutputSHA256 = if (dry_run) NA_character_ else .idw6f_sha256(w_stage),
      NonNACells = w_stats$non_na,
      PositiveCells = w_stats$positive,
      Minimum = w_stats$minimum,
      Maximum = w_stats$maximum,
      Sum = w_stats$sum,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    v_sources <- vapply(
      v_jobs,
      function(job_id) component_paths[[job_id]][[period_index]],
      character(1)
    )
    v_masks <- unname(mask_paths[v_jobs])

    for (component_index in seq_along(v_jobs)) {
      v_source <- v_sources[[component_index]]
      v_component_target <- file.path(
        v_component_root,
        sprintf("IDW_C++_fw_v%03d_%s.tif", component_index, suffix)
      )
      v_component_raster <- terra::rast(v_source)
      v_component_stats <- .idw6f_raster_stats(v_component_raster)
      v_component_stage <- if (dry_run) {
        v_source
      } else {
        file.path(
          staging_root,
          sprintf("IDW_C++_fw_v%03d_%s.tif", component_index, suffix)
        )
      }
      if (!dry_run && !isTRUE(file.copy(
        v_source,
        v_component_stage,
        overwrite = FALSE
      ))) {
        .idw6f_stop("Could not stage V component output: ", v_source)
      }
      if (!dry_run) staged_paths <- c(staged_paths, v_component_stage)
      prospective_rows[[length(prospective_rows) + 1L]] <- data.frame(
        Channel = "V_COMPONENT",
        Period = period,
        Year = year,
        DemandTons = unname(v_demand_matrix[
          component_index, as.character(year)
        ]),
        ZeroDemand = unname(v_demand_matrix[
          component_index, as.character(year)
        ]) == 0,
        ComponentIndex = component_index,
        DemandISO3 = v_iso3[[component_index]],
        InstallOperation = "preserve_for_runtime_origin_normalization",
        ComponentJobs = v_jobs[[component_index]],
        ComponentPaths = .idw6f_normalize(v_source),
        ComponentSHA256 = .idw6f_sha256(v_source),
        TargetPath = .idw6f_normalize(v_component_target, must_work = FALSE),
        OutputSHA256 = .idw6f_sha256(v_component_stage),
        NonNACells = v_component_stats$non_na,
        PositiveCells = v_component_stats$positive,
        Minimum = v_component_stats$minimum,
        Maximum = v_component_stats$maximum,
        Sum = v_component_stats$sum,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }

    v_combined <- .idw6f_combine_components(
      v_sources,
      v_masks,
      templates$V,
      "V"
    )
    v_stats <- .idw6f_raster_stats(v_combined)
    v_period_demand <- sum(v_demand_matrix[, as.character(year)])
    .idw6f_validate_stats_for_demand(
      v_stats,
      v_period_demand,
      paste0("Combined V ranking output for period ", suffix)
    )
    v_target <- file.path(in_root, paste0("IDW_C++_fw_v", suffix, ".tif"))
    v_stage <- if (dry_run) NULL else file.path(staging_root, basename(v_target))
    if (!dry_run) {
      .idw6f_write_raster(v_combined, v_stage)
      staged_paths <- c(staged_paths, v_stage)
    }
    prospective_rows[[length(prospective_rows) + 1L]] <- data.frame(
      Channel = "V",
      Period = period,
      Year = year,
      DemandTons = v_period_demand,
      ZeroDemand = v_period_demand == 0,
      ComponentIndex = NA_integer_,
      DemandISO3 = paste(
        as.character(manifest$DemandISO3[manifest$Channel == "V"]),
        collapse = ";"
      ),
      InstallOperation = "pixelwise_sum_V_components_for_ranking_only",
      ComponentJobs = paste(v_jobs, collapse = ";"),
      ComponentPaths = paste(vapply(v_sources, .idw6f_normalize, character(1)), collapse = ";"),
      ComponentSHA256 = paste(vapply(v_sources, .idw6f_sha256, character(1)), collapse = ";"),
      TargetPath = .idw6f_normalize(v_target, must_work = FALSE),
      OutputSHA256 = if (dry_run) NA_character_ else .idw6f_sha256(v_stage),
      NonNACells = v_stats$non_na,
      PositiveCells = v_stats$positive,
      Minimum = v_stats$minimum,
      Maximum = v_stats$maximum,
      Sum = v_stats$sum,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    message("  assembled period ", suffix, " (", year, ")")
  }
  outputs <- do.call(rbind, prospective_rows)
  outputs$CreatedUTC <- format(Sys.time(), tz = "UTC", usetz = TRUE)

  if (dry_run) {
    message("Dry run passed. No files were installed.")
    return(invisible(list(
      components = components,
      outputs = outputs,
      w_component_index = w_component_index,
      v_component_index = v_component_index,
      w_demand_matrix = w_demand_matrix,
      v_demand_matrix = v_demand_matrix
    )))
  }

  stage_demand_lookups <- function(channel, jobs, demand_matrix, target_paths) {
    demand_audit_rows <- vector("list", length(annual_years))
    demand_stages <- character(length(annual_years))
    for (year_index in seq_along(annual_years)) {
      demand_stage <- file.path(
        staging_root,
        sprintf(
          "%s_origin_demand%02d.csv",
          channel,
          annual_periods[[year_index]]
        )
      )
      demand_lookup <- data.frame(
        Key = seq_along(jobs),
        Value = as.numeric(demand_matrix[, year_index]),
        check.names = FALSE
      )
      write.csv(
        demand_lookup,
        demand_stage,
        row.names = FALSE,
        quote = TRUE,
        na = ""
      )
      demand_stages[[year_index]] <- demand_stage
      demand_audit_rows[[year_index]] <- data.frame(
        Period = annual_periods[[year_index]],
        Year = annual_years[[year_index]],
        ComponentCount = length(jobs),
        TotalDemandTons = sum(demand_lookup$Value),
        TargetPath = .idw6f_normalize(
          target_paths[[year_index]],
          must_work = FALSE
        ),
        SHA256 = .idw6f_sha256(demand_stage),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
    list(
      stages = demand_stages,
      audit = do.call(rbind, demand_audit_rows)
    )
  }
  w_demand_products <- stage_demand_lookups(
    "W", w_jobs, w_demand_matrix, w_demand_target_paths
  )
  v_demand_products <- stage_demand_lookups(
    "V", v_jobs, v_demand_matrix, v_demand_target_paths
  )
  staged_paths <- c(
    staged_paths,
    w_demand_products$stages,
    v_demand_products$stages
  )

  staged_w_component_index <- file.path(
    staging_root, basename(w_component_index_path)
  )
  staged_v_component_index <- file.path(
    staging_root, basename(v_component_index_path)
  )
  staged_w_demand_audit <- file.path(
    staging_root, basename(w_demand_audit_path)
  )
  staged_v_demand_audit <- file.path(
    staging_root, basename(v_demand_audit_path)
  )
  staged_audit <- file.path(staging_root, basename(audit_path))
  staged_readme <- file.path(staging_root, basename(readme_path))
  write.csv(
    w_component_index,
    staged_w_component_index,
    row.names = FALSE,
    quote = TRUE,
    na = ""
  )
  write.csv(
    v_component_index,
    staged_v_component_index,
    row.names = FALSE,
    quote = TRUE,
    na = ""
  )
  write.csv(
    w_demand_products$audit,
    staged_w_demand_audit,
    row.names = FALSE,
    quote = TRUE,
    na = ""
  )
  write.csv(
    v_demand_products$audit,
    staged_v_demand_audit,
    row.names = FALSE,
    quote = TRUE,
    na = ""
  )
  write.csv(
    outputs,
    staged_audit,
    row.names = FALSE,
    quote = TRUE,
    na = ""
  )
  readme_lines <- c(
    paste0("MoFuSS directional IDW installation for ", basename(run_root)),
    "",
    paste0("Installed periods: ", paste(sprintf("%02d", periods), collapse = ", "), "."),
    paste0(
      "W origin-country components preserved separately: ",
      paste(paste0(w_iso3, "=", w_jobs), collapse = ", "), "."
    ),
    paste0(
      "V directional components preserved separately: ",
      paste(paste0(v_iso3, "=", v_jobs), collapse = ", "), "."
    ),
    "The top-level W and V rasters are summed ranking surfaces only; MoFuSS v11 must not normalize either one as a regional demand pool.",
    "Annual W_origin_demandNN.csv lookups reconstruct the regional W demand exactly and retain one key per origin country.",
    "Annual V_origin_demandNN.csv lookups reconstruct regional V demand exactly and retain one key per directional component.",
    "Component NA cells were treated as zero only during addition.",
    "An all-zero component or combined raster was accepted only when its corresponding annual demand was exactly zero.",
    "The union of permitted source domains was restored on each top-level ranking raster.",
    "Every source and installed raster passed geometry, finite-value, nonnegative-value and source-domain checks.",
    "SHA-256 checksums and source paths are recorded in HC_IDW_install_manifest.csv and both origin-demand manifests.",
    "CostDistance_IDW runtime parameters (-t and -e) are not embedded in GeoTIFFs and must be preserved with the HPC logs.",
    "Use 10_dyn_Sc17_webmofuss_ctrees_g_v11.egoml with these installed W and V components."
  )
  writeLines(readme_lines, staged_readme, useBytes = TRUE)
  staged_paths <- c(
    staged_paths,
    staged_w_component_index,
    staged_v_component_index,
    staged_w_demand_audit,
    staged_v_demand_audit,
    staged_audit,
    staged_readme
  )
  destinations <- c(
    outputs$TargetPath,
    vapply(w_demand_target_paths, .idw6f_normalize, character(1), must_work = FALSE),
    vapply(v_demand_target_paths, .idw6f_normalize, character(1), must_work = FALSE),
    .idw6f_normalize(w_component_index_path, must_work = FALSE),
    .idw6f_normalize(v_component_index_path, must_work = FALSE),
    .idw6f_normalize(w_demand_audit_path, must_work = FALSE),
    .idw6f_normalize(v_demand_audit_path, must_work = FALSE),
    .idw6f_normalize(audit_path, must_work = FALSE),
    .idw6f_normalize(readme_path, must_work = FALSE)
  )
  if (length(staged_paths) != length(destinations)) {
    .idw6f_stop("Internal error: staged and destination file counts differ.")
  }

  installed <- character()
  staged_hashes <- vapply(staged_paths, .idw6f_sha256, character(1))
  tryCatch(
    {
      for (index in seq_along(staged_paths)) {
        dir.create(
          dirname(destinations[[index]]),
          recursive = TRUE,
          showWarnings = FALSE
        )
        if (!isTRUE(file.copy(staged_paths[[index]], destinations[[index]], overwrite = FALSE))) {
          .idw6f_stop("Could not install: ", destinations[[index]])
        }
        installed <- c(installed, destinations[[index]])
        target_hash <- .idw6f_sha256(destinations[[index]])
        if (!identical(target_hash, staged_hashes[[index]])) {
          .idw6f_stop(
            "Installed checksum mismatch: ", destinations[[index]]
          )
        }
      }
    },
    error = function(error) {
      unlink(installed, force = TRUE)
      stop(error)
    }
  )

  message("Directional IDWs installed successfully in ", .idw6f_normalize(in_root), ".")
  message("Audit manifest: ", .idw6f_normalize(audit_path))
  invisible(list(
    components = components,
    outputs = outputs,
    w_component_index = w_component_index,
    v_component_index = v_component_index,
    w_demand_matrix = w_demand_matrix,
    v_demand_matrix = v_demand_matrix,
    w_demand_audit = w_demand_products$audit,
    v_demand_audit = v_demand_products$audit
  ))
}

if (!identical(Sys.getenv("MOFUSS_6F_NO_AUTORUN"), "1")) {
  install_directional_idw_outputs()
}
