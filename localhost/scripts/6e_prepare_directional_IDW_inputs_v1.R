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
# Script: 6e_prepare_directional_IDW_inputs_v1.R
# Version: 1
# Date: Sep 2026
# Execution: Source from RStudio after scripts 5, 6a and 6d have completed.
#
# Purpose: Convert the harmonized directional HC-job bundles into the three-file
# input interface used by the CostDistance_IDW executable. Each source-domain
# mask is embedded in a job-specific friction raster; harmonization-fringe
# demand is snapped to the nearest source cell and merged there when the cell
# already contains a locality.
#
# Inputs:
#   In/fricc_w.tif and In/fricc_v.tif
#   In/DemandScenarios/HC_jobs/HC_job_manifest_harmonized.csv
#   Harmonized locality rasters, source masks and demand tables in each job.
#
# Outputs:
#   fricc_<channel>_idw.tif, locs_c_<channel>_idw.tif and *_idw.csv in each job
#   In/DemandScenarios/HC_jobs/HC_job_manifest_idw_ready.csv
#   In/DemandScenarios/HC_jobs/README_IDW_UPLOAD.txt
#
# This script does not run CostDistance_IDW and does not alter the harmonizer
# products, the base friction rasters, or the demand tables.

suppressPackageStartupMessages(library(terra))

.idw_stop <- function(...) {
  stop(..., call. = FALSE)
}

.idw_normalize <- function(path, must_work = TRUE) {
  normalizePath(path, winslash = "/", mustWork = must_work)
}

.idw_resolve_run_root <- function() {
  inherited_countrydir <- get0("countrydir", inherits = TRUE, ifnotfound = NULL)
  candidates <- unique(c(
    if (!is.null(inherited_countrydir)) as.character(inherited_countrydir) else NULL,
    getwd()
  ))
  candidates <- candidates[nzchar(candidates)]
  required_relative <- file.path(
    "In", "DemandScenarios", "HC_jobs", "HC_job_manifest_harmonized.csv"
  )
  matches <- candidates[file.exists(file.path(candidates, required_relative))]
  if (length(matches) == 0L) {
    .idw_stop(
      "Could not locate a MoFuSS run containing ", required_relative,
      ". Define `countrydir` or set the working directory to the run root."
    )
  }
  .idw_normalize(matches[[1L]])
}

.idw_safe_relative_path <- function(path, label) {
  path <- trimws(as.character(path))
  if (length(path) != 1L || is.na(path) || !nzchar(path) ||
      grepl("^[A-Za-z]:", path) || startsWith(path, "/") ||
      startsWith(path, "\\") || grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)", path)) {
    .idw_stop(label, " is not a safe HC-job relative path: ", path)
  }
  path
}

.idw_manifest_path <- function(hc_root, relative_path, label) {
  relative_path <- .idw_safe_relative_path(relative_path, label)
  output <- file.path(hc_root, relative_path)
  if (!file.exists(output)) {
    .idw_stop(label, " does not exist: ", output)
  }
  output
}

.idw_assert_single_raster <- function(raster, label) {
  if (terra::nlyr(raster) != 1L) {
    .idw_stop(label, " must contain exactly one raster layer.")
  }
  invisible(TRUE)
}

.idw_assert_same_geometry <- function(x, y, x_label, y_label) {
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
    .idw_stop(x_label, " does not match the geometry of ", y_label, ".")
  }
  invisible(TRUE)
}

.idw_count <- function(binary_raster) {
  count <- terra::global(binary_raster, "sum", na.rm = TRUE)[[1L]]
  if (length(count) != 1L || is.na(count)) 0 else as.numeric(count)
}

.idw_range <- function(raster) {
  result <- terra::global(raster, c("min", "max"), na.rm = TRUE)
  c(min = as.numeric(result[[1L]]), max = as.numeric(result[[2L]]))
}

.idw_extract_locations <- function(location_raster, label) {
  location_data <- terra::as.data.frame(
    location_raster,
    cells = TRUE,
    na.rm = TRUE
  )
  if (nrow(location_data) == 0L || ncol(location_data) != 2L) {
    .idw_stop(label, " contains no location IDs or an unexpected layer count.")
  }
  ids <- suppressWarnings(as.numeric(location_data[[2L]]))
  if (any(!is.finite(ids)) || any(ids <= 0) ||
      any(ids != floor(ids)) || anyDuplicated(ids)) {
    .idw_stop(label, " contains invalid or duplicate positive integer IDs.")
  }
  list(cells = as.integer(location_data$cell), ids = as.integer(ids))
}

.idw_read_demand_metadata <- function(demand_path, channel, label) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    .idw_stop("Package `data.table` is required to validate the large demand tables.")
  }
  header <- names(data.table::fread(
    demand_path,
    nrows = 0L,
    showProgress = FALSE
  ))
  if (length(header) < 2L || !identical(header[[1L]], "ID")) {
    .idw_stop(label, " must begin with ID and contain at least one demand year.")
  }
  demand_pattern <- paste0("^([0-9]{4})_fw_", channel, "$")
  if (!all(grepl(demand_pattern, header[-1L]))) {
    .idw_stop(label, " has demand columns inconsistent with channel ", channel, ".")
  }
  years <- as.integer(sub(demand_pattern, "\\1", header[-1L]))
  if (anyDuplicated(years) || !identical(years, seq.int(min(years), max(years)))) {
    .idw_stop(label, " demand years must be unique, ordered and contiguous.")
  }
  demand_ids <- data.table::fread(
    demand_path,
    select = 1L,
    showProgress = FALSE
  )[[1L]]
  demand_ids <- suppressWarnings(as.numeric(demand_ids))
  if (any(!is.finite(demand_ids)) || any(demand_ids <= 0) ||
      any(demand_ids != floor(demand_ids)) || anyDuplicated(demand_ids)) {
    .idw_stop(label, " contains invalid or duplicate positive integer IDs.")
  }
  list(
    ids = as.integer(demand_ids),
    years = years,
    rows = length(demand_ids)
  )
}

.idw_assert_matching_ids <- function(location_ids, demand_ids, label) {
  if (length(location_ids) != length(demand_ids) ||
      !identical(sort.int(location_ids), sort.int(demand_ids))) {
    .idw_stop(label, " locality IDs do not match demand-table IDs exactly.")
  }
  invisible(TRUE)
}

.idw_neighbor_rings <- function(max_radius_cells, resolution) {
  lapply(seq_len(max_radius_cells), function(radius) {
    offsets <- expand.grid(
      row_offset = seq.int(-radius, radius),
      col_offset = seq.int(-radius, radius)
    )
    offsets <- offsets[
      pmax(abs(offsets$row_offset), abs(offsets$col_offset)) == radius,
      ,
      drop = FALSE
    ]
    offsets$squared_shift <-
      (offsets$col_offset * resolution[[1L]])^2 +
      (offsets$row_offset * resolution[[2L]])^2
    offsets
  })
}

.idw_fill_friction_gaps <- function(
    base_values,
    allowed_values,
    fallback_value,
    barrier_value) {
  if (length(base_values) != length(allowed_values)) {
    .idw_stop("Internal error: friction and mask values have different lengths.")
  }
  if (length(fallback_value) != 1L || !is.finite(fallback_value) ||
      fallback_value <= 0 || fallback_value >= barrier_value) {
    .idw_stop("The friction fallback must be one positive non-barrier value.")
  }

  base_valid <- is.finite(base_values) & base_values > 0
  gap_cells <- which(allowed_values & !base_valid)
  prepared_values <- rep(NA_real_, length(base_values))
  prepared_values[allowed_values & base_valid] <-
    base_values[allowed_values & base_valid]
  prepared_values[gap_cells] <- fallback_value

  list(
    values = prepared_values,
    gap_count = length(gap_cells),
    gap_cells = gap_cells,
    fallback_value = fallback_value
  )
}

.idw_friction_fallback <- function(run_root, channel, barrier_value) {
  table_name <- if (identical(channel, "w")) {
    "Friction_walkingoverroads_r.csv"
  } else {
    "Friction_walkingcrosscountry_r.csv"
  }
  table_path <- file.path(run_root, "LULCC", "TempTables", table_name)
  if (!file.exists(table_path)) {
    .idw_stop("Friction fallback table does not exist: ", table_path)
  }
  friction_table <- read.csv(
    table_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    strip.white = TRUE
  )
  names(friction_table) <- trimws(tolower(names(friction_table)))
  if (!all(c("from", "value") %in% names(friction_table))) {
    .idw_stop("Friction fallback table has an invalid schema: ", table_path)
  }
  from_values <- suppressWarnings(as.numeric(friction_table$from))
  friction_values <- suppressWarnings(as.numeric(friction_table$value))
  candidates <- which(
    is.finite(from_values) & from_values == min(from_values, na.rm = TRUE) &
      is.finite(friction_values) & friction_values > 0 &
      friction_values < barrier_value
  )
  if (length(candidates) != 1L) {
    .idw_stop("Could not identify one zero-slope fallback in: ", table_path)
  }
  friction_values[[candidates]]
}

.idw_nearest_allowed_cells <- function(
    source_cells,
    allowed,
    template,
    max_radius_cells = 64L) {
  if (length(source_cells) == 0L) {
    return(list(cells = integer(), radii = integer(), shifts = numeric()))
  }
  if (length(allowed) != terra::ncell(template)) {
    .idw_stop("Internal error: allowed-cell vector does not match raster geometry.")
  }
  max_radius_cells <- as.integer(max_radius_cells)
  if (is.na(max_radius_cells) || max_radius_cells < 1L) {
    .idw_stop("`max_radius_cells` must be a positive integer.")
  }

  n_rows <- terra::nrow(template)
  n_cols <- terra::ncol(template)
  resolution <- terra::res(template)
  rings <- .idw_neighbor_rings(max_radius_cells, resolution)
  source_rc <- terra::rowColFromCell(template, source_cells)
  target_cells <- rep(NA_integer_, length(source_cells))
  target_radii <- rep(NA_integer_, length(source_cells))
  target_shifts <- rep(NA_real_, length(source_cells))

  for (source_index in seq_along(source_cells)) {
    source_row <- source_rc[source_index, 1L]
    source_col <- source_rc[source_index, 2L]
    assigned <- FALSE
    for (radius in seq_len(max_radius_cells)) {
      offsets <- rings[[radius]]
      candidate_rows <- source_row + offsets$row_offset
      candidate_cols <- source_col + offsets$col_offset
      in_grid <- candidate_rows >= 1L & candidate_rows <= n_rows &
        candidate_cols >= 1L & candidate_cols <= n_cols
      if (!any(in_grid)) {
        next
      }
      offsets <- offsets[in_grid, , drop = FALSE]
      candidate_rows <- candidate_rows[in_grid]
      candidate_cols <- candidate_cols[in_grid]
      candidate_cells <- as.integer((candidate_rows - 1L) * n_cols + candidate_cols)
      usable <- allowed[candidate_cells]
      if (!any(usable)) {
        next
      }
      offsets <- offsets[usable, , drop = FALSE]
      candidate_cells <- candidate_cells[usable]
      selected <- order(offsets$squared_shift, candidate_cells)[[1L]]
      target_cells[[source_index]] <- candidate_cells[[selected]]
      target_radii[[source_index]] <- radius
      target_shifts[[source_index]] <- sqrt(offsets$squared_shift[[selected]])
      assigned <- TRUE
      break
    }
    if (!assigned) {
      .idw_stop(
        "Could not map source cell ", source_cells[[source_index]],
        " into the allowed source domain within ", max_radius_cells, " cells."
      )
    }
  }
  list(cells = target_cells, radii = target_radii, shifts = target_shifts)
}

.idw_sha256 <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    return(NA_character_)
  }
  unname(digest::digest(path, algo = "sha256", file = TRUE))
}

.idw_collapse_location_mapping <- function(
    locations,
    allowed_values,
    template,
    max_radius_cells) {
  inside_domain <- allowed_values[locations$cells]
  inside_domain[is.na(inside_domain)] <- FALSE
  outside_indices <- which(!inside_domain)
  target_cells <- locations$cells
  shift_map_units <- rep(0, length(locations$ids))
  shift_cells <- rep(0L, length(locations$ids))

  if (length(outside_indices) > 0L) {
    moved <- .idw_nearest_allowed_cells(
      source_cells = locations$cells[outside_indices],
      allowed = allowed_values,
      template = template,
      max_radius_cells = max_radius_cells
    )
    target_cells[outside_indices] <- moved$cells
    shift_map_units[outside_indices] <- moved$shifts
    shift_cells[outside_indices] <- moved$radii
  }
  if (any(!allowed_values[target_cells])) {
    .idw_stop("Internal error: a mapped locality remains outside its source domain.")
  }

  # Prefer the ID already located at a target cell. If several fringe origins
  # share an otherwise empty target, retain the smallest ID. Other IDs are
  # merged into that survivor in the IDW-specific demand table.
  already_at_target <- locations$cells == target_cells
  mapping_order <- order(target_cells, !already_at_target, locations$ids)
  ordered_cells <- target_cells[mapping_order]
  run_lengths <- rle(ordered_cells)$lengths
  run_starts <- cumsum(c(1L, head(run_lengths, -1L)))
  ordered_survivors <- rep(
    locations$ids[mapping_order][run_starts],
    run_lengths
  )
  survivor_ids <- integer(length(locations$ids))
  survivor_ids[mapping_order] <- ordered_survivors
  survivor_indices <- which(locations$ids == survivor_ids)

  if (length(survivor_indices) != length(unique(target_cells)) ||
      anyDuplicated(target_cells[survivor_indices]) ||
      anyDuplicated(locations$ids[survivor_indices])) {
    .idw_stop("Internal error while collapsing locality IDs onto source cells.")
  }

  changed_indices <- which(
    locations$cells != target_cells | locations$ids != survivor_ids
  )
  if (length(changed_indices) > 0L) {
    original_xy <- terra::xyFromCell(template, locations$cells[changed_indices])
    target_xy <- terra::xyFromCell(template, target_cells[changed_indices])
  } else {
    original_xy <- matrix(numeric(), nrow = 0L, ncol = 2L)
    target_xy <- matrix(numeric(), nrow = 0L, ncol = 2L)
  }
  adjustments <- data.frame(
    OriginalID = locations$ids[changed_indices],
    SurvivorID = survivor_ids[changed_indices],
    OriginalCell = locations$cells[changed_indices],
    TargetCell = target_cells[changed_indices],
    OriginalX = original_xy[, 1L],
    OriginalY = original_xy[, 2L],
    TargetX = target_xy[, 1L],
    TargetY = target_xy[, 2L],
    ShiftMapUnits = shift_map_units[changed_indices],
    ShiftCells = shift_cells[changed_indices],
    Action = ifelse(
      locations$ids[changed_indices] == survivor_ids[changed_indices],
      "moved",
      "merged_into_survivor"
    ),
    stringsAsFactors = FALSE
  )

  list(
    target_cells = target_cells,
    survivor_ids = survivor_ids,
    survivor_indices = survivor_indices,
    outside_indices = outside_indices,
    adjustments = adjustments,
    shift_map_units = shift_map_units,
    shift_cells = shift_cells
  )
}

.idw_write_collapsed_demand <- function(
    input_path,
    output_path,
    location_ids,
    survivor_ids,
    channel) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    .idw_stop("Package `data.table` is required to write IDW demand tables.")
  }
  demand <- data.table::fread(input_path, showProgress = FALSE)
  demand_columns <- setdiff(names(demand), "ID")
  expected_pattern <- paste0("^[0-9]{4}_fw_", channel, "$")
  if (!identical(names(demand)[[1L]], "ID") ||
      length(demand_columns) == 0L ||
      !all(grepl(expected_pattern, demand_columns))) {
    .idw_stop("Demand table has an invalid schema: ", input_path)
  }
  if (!all(vapply(demand[, ..demand_columns], is.numeric, logical(1)))) {
    .idw_stop("Demand table contains nonnumeric demand columns: ", input_path)
  }
  demand_ids <- suppressWarnings(as.integer(demand$ID))
  if (any(is.na(demand_ids)) || anyDuplicated(demand_ids)) {
    .idw_stop("Demand table contains invalid or duplicate IDs: ", input_path)
  }
  row_for_location <- match(location_ids, demand_ids)
  if (anyNA(row_for_location) || length(row_for_location) != nrow(demand)) {
    .idw_stop("Demand rows and locality IDs do not match before collapsing: ", input_path)
  }

  input_totals <- vapply(
    demand[, ..demand_columns],
    sum,
    numeric(1),
    na.rm = FALSE
  )
  if (any(!is.finite(input_totals))) {
    .idw_stop("Demand table contains missing or non-finite values: ", input_path)
  }
  donor_indices <- which(location_ids != survivor_ids)
  donor_rows <- row_for_location[donor_indices]
  if (length(donor_rows) > 0L) {
    donor_demand <- data.table::copy(demand[donor_rows, c("ID", demand_columns), with = FALSE])
    donor_demand[, SurvivorID := survivor_ids[donor_indices]]
    donor_sums <- donor_demand[, lapply(.SD, sum), by = SurvivorID, .SDcols = demand_columns]
    target_rows <- match(donor_sums$SurvivorID, demand_ids)
    if (anyNA(target_rows)) {
      .idw_stop("A survivor ID is absent from the source demand table.")
    }
    for (column_name in demand_columns) {
      data.table::set(
        demand,
        i = target_rows,
        j = column_name,
        value = demand[[column_name]][target_rows] + donor_sums[[column_name]]
      )
    }
    keep <- rep(TRUE, nrow(demand))
    keep[donor_rows] <- FALSE
    demand <- demand[keep]
  }
  data.table::setorder(demand, ID)
  output_totals <- vapply(
    demand[, ..demand_columns],
    sum,
    numeric(1),
    na.rm = FALSE
  )
  annual_delta <- output_totals - input_totals
  tolerance <- pmax(1e-6, abs(input_totals) * 1e-12)
  if (any(!is.finite(output_totals)) || any(abs(annual_delta) > tolerance)) {
    .idw_stop("Annual demand was not conserved while collapsing: ", input_path)
  }
  data.table::fwrite(
    demand,
    output_path,
    quote = FALSE,
    na = "",
    showProgress = FALSE
  )
  if (!file.exists(output_path) || file.info(output_path)$size <= 0) {
    .idw_stop("Could not write the IDW-specific demand table: ", output_path)
  }
  installed <- .idw_read_demand_metadata(
    output_path,
    channel,
    paste0("Installed IDW demand table ", basename(output_path))
  )
  if (!identical(sort.int(installed$ids), sort.int(as.integer(demand$ID)))) {
    .idw_stop("Installed IDW demand IDs do not match the prepared table: ", output_path)
  }
  list(
    ids = installed$ids,
    rows = installed$rows,
    merged_ids = length(donor_indices),
    max_annual_delta = max(abs(annual_delta)),
    annual_totals = output_totals
  )
}

.idw_prepare_job <- function(
    manifest_row,
    run_root,
    hc_root,
    barrier_value = 999999,
    max_snap_radius_cells = 64L) {
  job_id <- trimws(as.character(manifest_row$JobID[[1L]]))
  channel <- tolower(trimws(as.character(manifest_row$Channel[[1L]])))
  if (!grepl("^[A-Za-z0-9_]+$", job_id)) {
    .idw_stop("Unsafe or empty JobID in the harmonized HC manifest: ", job_id)
  }
  if (!(channel %in% c("w", "v"))) {
    .idw_stop("HC job ", job_id, " has unsupported channel: ", channel)
  }

  message("Preparing directional IDW inputs for ", job_id, "...")
  base_friction_path <- file.path(run_root, "In", paste0("fricc_", channel, ".tif"))
  if (!file.exists(base_friction_path)) {
    .idw_stop("Base friction raster does not exist for ", job_id, ": ", base_friction_path)
  }
  location_path <- .idw_manifest_path(
    hc_root,
    manifest_row$HarmonizedLocationsRaster[[1L]],
    paste0("HC job ", job_id, " location raster")
  )
  source_mask_path <- .idw_manifest_path(
    hc_root,
    manifest_row$HarmonizedSourceDomainMask[[1L]],
    paste0("HC job ", job_id, " source-domain mask")
  )
  demand_path <- .idw_manifest_path(
    hc_root,
    manifest_row$HarmonizedDemandTable[[1L]],
    paste0("HC job ", job_id, " demand table")
  )

  base_friction <- terra::rast(base_friction_path)
  location_raster <- terra::rast(location_path)
  source_mask <- terra::rast(source_mask_path)
  .idw_assert_single_raster(base_friction, paste0("HC job ", job_id, " base friction"))
  .idw_assert_single_raster(location_raster, paste0("HC job ", job_id, " locations"))
  .idw_assert_single_raster(source_mask, paste0("HC job ", job_id, " source mask"))
  .idw_assert_same_geometry(location_raster, base_friction, "Location raster", "base friction")
  .idw_assert_same_geometry(source_mask, base_friction, "Source mask", "base friction")

  mask_values <- terra::values(source_mask, mat = FALSE)
  unexpected_mask_values <- unique(mask_values[
    !is.na(mask_values) & !(mask_values %in% c(0, 1))
  ])
  if (length(unexpected_mask_values) > 0L) {
    .idw_stop("HC job ", job_id, " source mask contains values other than 0, 1 or NA.")
  }
  allowed_values <- !is.na(mask_values) & mask_values == 1
  if (!any(allowed_values)) {
    .idw_stop("HC job ", job_id, " source mask contains no allowed cells.")
  }

  if (terra::is.lonlat(base_friction)) {
    .idw_stop("HC job ", job_id, " friction must use a projected CRS.")
  }
  base_values <- terra::values(base_friction, mat = FALSE)
  fallback_value <- .idw_friction_fallback(run_root, channel, barrier_value)
  repaired <- .idw_fill_friction_gaps(
    base_values = base_values,
    allowed_values = allowed_values,
    fallback_value = fallback_value,
    barrier_value = barrier_value
  )
  gap_count <- repaired$gap_count
  allowed <- terra::setValues(terra::rast(base_friction), as.integer(allowed_values))
  prepared_friction <- terra::setValues(terra::rast(base_friction), repaired$values)

  remaining_invalid <- .idw_count(
    allowed == 1 &
      (is.na(prepared_friction) | prepared_friction <= 0 | prepared_friction >= Inf)
  )
  leaked_cells <- .idw_count(allowed == 0 & !is.na(prepared_friction))
  if (remaining_invalid > 0 || leaked_cells > 0) {
    .idw_stop(
      "HC job ", job_id, " prepared friction failed domain validation: ",
      remaining_invalid, " invalid allowed cells; ", leaked_cells,
      " populated disallowed cells."
    )
  }

  locations <- .idw_extract_locations(
    location_raster,
    paste0("HC job ", job_id, " harmonized location raster")
  )
  demand <- .idw_read_demand_metadata(
    demand_path,
    channel,
    paste0("HC job ", job_id, " demand table")
  )
  .idw_assert_matching_ids(
    locations$ids,
    demand$ids,
    paste0("HC job ", job_id)
  )

  location_mapping <- .idw_collapse_location_mapping(
    locations = locations,
    allowed_values = allowed_values,
    template = base_friction,
    max_radius_cells = max_snap_radius_cells
  )
  survivor_indices <- location_mapping$survivor_indices
  final_cells <- location_mapping$target_cells[survivor_indices]
  final_ids <- locations$ids[survivor_indices]
  final_location_values <- rep(NA_integer_, terra::ncell(base_friction))
  final_location_values[final_cells] <- final_ids
  final_location <- terra::setValues(terra::rast(base_friction), final_location_values)
  names(final_location) <- paste0("location_id_", channel)

  job_directory <- dirname(location_path)
  friction_output <- file.path(job_directory, paste0("fricc_", channel, "_idw.tif"))
  location_output <- file.path(job_directory, paste0("locs_c_", channel, "_idw.tif"))
  demand_output <- file.path(
    job_directory,
    sub("\\.csv$", "_idw.csv", basename(demand_path), ignore.case = TRUE)
  )
  adjustments_output <- file.path(job_directory, "IDW_location_adjustments.csv")
  collapsed_demand <- .idw_write_collapsed_demand(
    input_path = demand_path,
    output_path = demand_output,
    location_ids = locations$ids,
    survivor_ids = location_mapping$survivor_ids,
    channel = channel
  )
  if (!identical(sort.int(final_ids), sort.int(collapsed_demand$ids))) {
    .idw_stop("HC job ", job_id, " collapsed demand IDs do not match final locality IDs.")
  }
  write.csv(
    location_mapping$adjustments,
    adjustments_output,
    row.names = FALSE,
    quote = TRUE,
    na = ""
  )
  terra::writeRaster(
    prepared_friction,
    friction_output,
    overwrite = TRUE,
    datatype = "FLT4S",
    NAflag = -9999,
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3", "TILED=YES")
  )
  terra::writeRaster(
    final_location,
    location_output,
    overwrite = TRUE,
    datatype = "INT4S",
    NAflag = -2147483648,
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES")
  )

  installed_friction <- terra::rast(friction_output)
  installed_location <- terra::rast(location_output)
  .idw_assert_same_geometry(installed_friction, base_friction, "Prepared friction", "base friction")
  .idw_assert_same_geometry(installed_location, base_friction, "Prepared locations", "base friction")
  installed_ids <- .idw_extract_locations(
    installed_location,
    paste0("HC job ", job_id, " installed IDW locality raster")
  )$ids
  .idw_assert_matching_ids(
    installed_ids,
    collapsed_demand$ids,
    paste0("HC job ", job_id, " installed files")
  )

  installed_valid <- terra::ifel(
    allowed == 1 &
      !is.na(installed_friction) &
      installed_friction > 0 &
      installed_friction < Inf,
    1,
    0
  )
  if (.idw_count(installed_valid) != sum(allowed_values)) {
    .idw_stop("HC job ", job_id, " installed friction does not cover every allowed source cell.")
  }
  installed_range <- .idw_range(installed_friction)
  barrier_cells <- .idw_count(
    allowed == 1 & installed_friction >= barrier_value
  )

  message(
    "  ", job_id, ": assigned fallback friction to ", gap_count,
    " cells; mapped ", length(location_mapping$outside_indices),
    " fringe IDs and merged ", collapsed_demand$merged_ids, " collisions."
  )

  data.frame(
    JobID = job_id,
    Channel = toupper(channel),
    Status = "IDW_READY",
    PeriodStart = 1L,
    PeriodEnd = length(demand$years),
    YearStart = min(demand$years),
    YearEnd = max(demand$years),
    FrictionRaster = .idw_normalize(friction_output),
    LocationsRaster = .idw_normalize(location_output),
    DemandTable = .idw_normalize(demand_output),
    OriginalDemandTable = .idw_normalize(demand_path),
    SourceDomainMask = .idw_normalize(source_mask_path),
    OriginalDemandRows = demand$rows,
    DemandRows = collapsed_demand$rows,
    LocationIDs = length(installed_ids),
    AllowedSourceCells = sum(allowed_values),
    FrictionGapCellsFilled = gap_count,
    FrictionGapFallbackValue = repaired$fallback_value,
    MappedFringeLocationIDs = length(location_mapping$outside_indices),
    MergedDemandIDs = collapsed_demand$merged_ids,
    MaxLocationShiftMapUnits = if (length(location_mapping$outside_indices)) {
      max(location_mapping$shift_map_units[location_mapping$outside_indices])
    } else 0,
    MaxLocationShiftCells = if (length(location_mapping$outside_indices)) {
      max(location_mapping$shift_cells[location_mapping$outside_indices])
    } else 0L,
    MaxAnnualDemandDelta = collapsed_demand$max_annual_delta,
    MinFriction = installed_range[["min"]],
    MaxFriction = installed_range[["max"]],
    BarrierCells = barrier_cells,
    CombineOperation = as.character(manifest_row$CombineOperation[[1L]]),
    OutputRole = as.character(manifest_row$OutputRole[[1L]]),
    FrictionSHA256 = .idw_sha256(friction_output),
    LocationsSHA256 = .idw_sha256(location_output),
    DemandSHA256 = .idw_sha256(demand_output),
    LocationAdjustments = .idw_normalize(adjustments_output),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

prepare_directional_idw_inputs <- function(
    run_root = .idw_resolve_run_root(),
    barrier_value = 999999,
    max_snap_radius_cells = 64L) {
  run_root <- .idw_normalize(run_root)
  hc_root <- file.path(run_root, "In", "DemandScenarios", "HC_jobs")
  manifest_path <- file.path(hc_root, "HC_job_manifest_harmonized.csv")
  if (!file.exists(manifest_path)) {
    .idw_stop("Harmonized HC-job manifest does not exist: ", manifest_path)
  }
  manifest <- read.csv(
    manifest_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
  required_columns <- c(
    "JobID", "Channel", "RunOnHCCluster", "HarmonizedDemandTable",
    "HarmonizedLocationsRaster", "HarmonizedSourceDomainMask",
    "CombineOperation", "OutputRole"
  )
  missing_columns <- setdiff(required_columns, names(manifest))
  if (length(missing_columns) > 0L) {
    .idw_stop(
      "Harmonized HC manifest is missing required column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }
  run_flag <- toupper(trimws(as.character(manifest$RunOnHCCluster))) == "TRUE"
  if (!any(run_flag)) {
    .idw_stop("The harmonized HC manifest contains no jobs marked RunOnHCCluster=TRUE.")
  }
  jobs <- manifest[run_flag, , drop = FALSE]
  if (anyDuplicated(jobs$JobID)) {
    .idw_stop("The harmonized HC manifest contains duplicate JobID values.")
  }

  ready_manifest_path <- file.path(hc_root, "HC_job_manifest_idw_ready.csv")
  upload_readme_path <- file.path(hc_root, "README_IDW_UPLOAD.txt")
  # Never leave a stale ready signal in place while a new preparation is
  # running. Job products are accepted only after every row validates.
  unlink(c(ready_manifest_path, upload_readme_path), force = TRUE)
  message("MoFuSS run: ", run_root)
  message("Preparing ", nrow(jobs), " directional IDW job(s).")

  results <- lapply(
    seq_len(nrow(jobs)),
    function(row_index) {
      .idw_prepare_job(
        jobs[row_index, , drop = FALSE],
        run_root = run_root,
        hc_root = hc_root,
        barrier_value = barrier_value,
        max_snap_radius_cells = max_snap_radius_cells
      )
    }
  )
  ready_manifest <- do.call(rbind, results)
  write.csv(
    ready_manifest,
    ready_manifest_path,
    row.names = FALSE,
    quote = TRUE,
    na = ""
  )

  v_rows <- ready_manifest$Channel == "V"
  readme_lines <- c(
    paste0("MoFuSS directional CostDistance_IDW upload guide for ", basename(run_root)),
    "",
    "Use only rows marked IDW_READY in HC_job_manifest_idw_ready.csv.",
    "For each row upload the exact FrictionRaster, LocationsRaster and DemandTable.",
    "The *_idw.csv tables conserve annual demand after merging fringe origins that share a target cell.",
    "IDW_location_adjustments.csv records every moved or merged origin.",
    paste0(
      "Periods are 1-based: period 1 = ", min(ready_manifest$YearStart),
      "; period ", max(ready_manifest$PeriodEnd), " = ",
      max(ready_manifest$YearEnd), "."
    ),
    "W output is used directly.",
    if (sum(v_rows) > 1L) {
      "For each period, sum all V job outputs pixel by pixel exactly once."
    } else {
      "Use the single V output directly."
    },
    "Do not add the legacy top-level W or V compatibility bundle.",
    "The *_idw.tif files embed the allowed source domain; do not substitute the base friction files.",
    "This preparation script does not run CostDistance_IDW.",
    "",
    paste0(
      ready_manifest$JobID, ": ", ready_manifest$FrictionRaster, " | ",
      ready_manifest$LocationsRaster, " | ", ready_manifest$DemandTable
    )
  )
  writeLines(readme_lines, upload_readme_path, useBytes = TRUE)

  message("Directional IDW inputs are ready: ", .idw_normalize(ready_manifest_path))
  invisible(ready_manifest)
}

if (!identical(Sys.getenv("MOFUSS_6E_NO_AUTORUN"), "1")) {
  prepare_directional_idw_inputs()
}
