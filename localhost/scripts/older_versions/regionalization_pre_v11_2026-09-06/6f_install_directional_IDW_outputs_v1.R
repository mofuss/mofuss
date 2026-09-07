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
# Script: 6f_install_directional_IDW_outputs_v1.R
# Version: 1
# Date: Sep 2026
# Execution: Source from RStudio only after every CostDistance_IDW HC job has
# completed and its output directory has been returned to HC_jobs.
#
# Purpose: Validate, assemble and install the decennial directional IDW outputs
# consumed by the Dinamica EGO model. The single W output is copied directly.
# Every V component is summed pixel by pixel exactly once for each period.
# Component NA cells are treated as zero during addition, after which the union
# of the permitted V source domains is restored as the final analysis mask.
#
# Expected HC output layout:
#   In/DemandScenarios/HC_jobs/idw_<JobID>/IDW_C++_fw_<w|v><NN>.tif
#
# Installed outputs:
#   In/IDW_C++_fw_w<NN>.tif
#   In/IDW_C++_fw_v<NN>.tif
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

.idw6f_validate_component <- function(
    raster_path,
    source_mask,
    template,
    label) {
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
  if (stats$positive == 0L || !is.finite(stats$maximum) || stats$maximum <= 0) {
    .idw6f_stop(label, " contains no positive IDW values.")
  }
  stats
}

.idw6f_validate_manifest <- function(manifest) {
  required_columns <- c(
    "JobID", "Channel", "Status", "PeriodStart", "PeriodEnd",
    "YearStart", "YearEnd", "SourceDomainMask", "CombineOperation",
    "OutputRole"
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
  if (sum(manifest$Channel == "W") != 1L) {
    .idw6f_stop("Directional installation requires exactly one W job.")
  }
  if (sum(manifest$Channel == "V") < 1L) {
    .idw6f_stop("Directional installation requires at least one V job.")
  }
  w_operation <- trimws(as.character(
    manifest$CombineOperation[manifest$Channel == "W"]
  ))
  if (!identical(w_operation, "use_directly")) {
    .idw6f_stop("The W job must declare CombineOperation=use_directly.")
  }
  v_operations <- trimws(as.character(
    manifest$CombineOperation[manifest$Channel == "V"]
  ))
  allowed_v_operations <- if (sum(manifest$Channel == "V") == 1L) {
    c("use_directly", "pixelwise_sum_by_year")
  } else {
    "pixelwise_sum_by_year"
  }
  if (any(!v_operations %in% allowed_v_operations)) {
    .idw6f_stop(
      "V jobs must declare CombineOperation=pixelwise_sum_by_year",
      if (length(allowed_v_operations) > 1L) " (or use_directly for one V job)." else "."
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

.idw6f_combine_v <- function(component_paths, mask_paths, template) {
  component_stack <- terra::rast(component_paths)
  .idw6f_assert_same_geometry(
    component_stack[[1L]], template, "V component stack", "V template"
  )
  combined <- terra::app(component_stack, sum, na.rm = TRUE)

  mask_stack <- terra::rast(mask_paths)
  permitted <- terra::app(
    mask_stack,
    function(values) as.integer(any(values == 1, na.rm = TRUE))
  )
  terra::ifel(permitted == 1, combined, NA)
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
      stats <- .idw6f_validate_component(
        job_paths[[period_index]],
        masks[[job_id]],
        templates[[channel]],
        label
      )
      row_counter <- row_counter + 1L
      component_rows[[row_counter]] <- data.frame(
        JobID = job_id,
        Channel = channel,
        Period = periods[[period_index]],
        Year = years[[period_index]],
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

  target_paths <- unlist(lapply(
    c("w", "v"),
    function(channel) file.path(
      in_root,
      sprintf("IDW_C++_fw_%s%02d.tif", channel, periods)
    )
  ), use.names = FALSE)
  audit_path <- file.path(hc_root, "HC_IDW_install_manifest.csv")
  readme_path <- file.path(hc_root, "README_IDW_INSTALL.txt")
  all_install_paths <- c(target_paths, audit_path, readme_path)
  existing <- all_install_paths[file.exists(all_install_paths)]
  if (length(existing) > 0L) {
    .idw6f_stop(
      "Refusing to overwrite existing installed IDW product(s):\n",
      paste(.idw6f_normalize(existing), collapse = "\n")
    )
  }

  w_job <- manifest$JobID[manifest$Channel == "W"][[1L]]
  v_jobs <- manifest$JobID[manifest$Channel == "V"]
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

    w_source <- component_paths[[w_job]][[period_index]]
    w_target <- file.path(in_root, paste0("IDW_C++_fw_w", suffix, ".tif"))
    w_raster <- terra::rast(w_source)
    w_stats <- .idw6f_raster_stats(w_raster)
    w_stage <- if (dry_run) w_source else file.path(staging_root, basename(w_target))
    if (!dry_run && !isTRUE(file.copy(w_source, w_stage, overwrite = FALSE))) {
      .idw6f_stop("Could not stage W output: ", w_source)
    }
    if (!dry_run) staged_paths <- c(staged_paths, w_stage)
    prospective_rows[[length(prospective_rows) + 1L]] <- data.frame(
      Channel = "W",
      Period = period,
      Year = year,
      InstallOperation = "copy_single_W_job",
      ComponentJobs = w_job,
      ComponentPaths = .idw6f_normalize(w_source),
      ComponentSHA256 = .idw6f_sha256(w_source),
      TargetPath = .idw6f_normalize(w_target, must_work = FALSE),
      OutputSHA256 = .idw6f_sha256(w_stage),
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
    v_combined <- .idw6f_combine_v(v_sources, v_masks, templates$V)
    v_stats <- .idw6f_raster_stats(v_combined)
    if (v_stats$positive == 0L || v_stats$minimum < 0 ||
        !is.finite(v_stats$maximum)) {
      .idw6f_stop("Combined V output is invalid for period ", suffix, ".")
    }
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
      InstallOperation = if (length(v_jobs) == 1L) {
        "validated_single_V_job"
      } else {
        "pixelwise_sum_V_jobs_NA_as_zero"
      },
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
    return(invisible(list(components = components, outputs = outputs)))
  }

  staged_audit <- file.path(staging_root, basename(audit_path))
  staged_readme <- file.path(staging_root, basename(readme_path))
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
    paste0("W job copied directly: ", w_job, "."),
    paste0("V jobs combined pixel by pixel: ", paste(v_jobs, collapse = ", "), "."),
    "Component NA cells were treated as zero only during addition.",
    "The union of all permitted V source domains was restored as the final V mask.",
    "Every source and installed raster passed geometry, finite-value, nonnegative-value and source-domain checks.",
    "SHA-256 checksums and source paths are recorded in HC_IDW_install_manifest.csv.",
    "CostDistance_IDW runtime parameters (-t and -e) are not embedded in GeoTIFFs and must be preserved with the HPC logs.",
    "No legacy top-level IDW compatibility raster was included."
  )
  writeLines(readme_lines, staged_readme, useBytes = TRUE)
  staged_paths <- c(staged_paths, staged_audit, staged_readme)
  destinations <- c(
    outputs$TargetPath,
    .idw6f_normalize(audit_path, must_work = FALSE),
    .idw6f_normalize(readme_path, must_work = FALSE)
  )
  if (length(staged_paths) != length(destinations)) {
    .idw6f_stop("Internal error: staged and destination file counts differ.")
  }

  installed <- character()
  tryCatch(
    {
      for (index in seq_along(staged_paths)) {
        if (!isTRUE(file.copy(staged_paths[[index]], destinations[[index]], overwrite = FALSE))) {
          .idw6f_stop("Could not install: ", destinations[[index]])
        }
        installed <- c(installed, destinations[[index]])
      }
      for (row_index in seq_len(nrow(outputs))) {
        target_hash <- .idw6f_sha256(outputs$TargetPath[[row_index]])
        if (!identical(target_hash, outputs$OutputSHA256[[row_index]])) {
          .idw6f_stop(
            "Installed checksum mismatch: ", outputs$TargetPath[[row_index]]
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
  invisible(list(components = components, outputs = outputs))
}

if (!identical(Sys.getenv("MOFUSS_6F_NO_AUTORUN"), "1")) {
  install_directional_idw_outputs()
}
