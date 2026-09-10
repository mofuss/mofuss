# SPDX-License-Identifier: Apache-2.0
#
# Copyright 2025-2027 Universidad Nacional Autonoma de Mexico
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

# MoFuSS mechanics/calibration/AGB validation pipeline ----
#
# Edit only the USER INPUTS block, then use RStudio Source / Source as
# Background Job, or run this file with Rscript. Enabled batches run
# sequentially; each batch completes its selected stages in fresh R processes
# before the next batch starts. growth_loss_gains.R is deliberately excluded.
#
# Read-only configuration check:
#   Rscript 0calib_valid_agb_pipeline_v1.R --check

# USER INPUTS: edit this block only -----------------------------------------

# Each enabled batch is one independent BAU/ICS x capped/uncapped analysis.
# Declare its common parent once in `root`; keep placeholders disabled until
# that root exists on the current computer.
PIPELINE_BATCHES <- list(
  GOG = list(
    enabled = TRUE,
    root = "E:/",
    folders = c(
      "GOG_1000m_bau1_2050_mc30_capped",
      "GOG_1000m_bau1_2050_mc30_uncapped",
      "GOG_1000m_ics3_2050_mc30_capped",
      "GOG_1000m_ics3_2050_mc30_uncapped"
    )
  ),
  madagascar = list(
    enabled = FALSE,
    root = "C:/Users/aghil/Documents/MoFuSS_localhost",
    folders = c(
      "mdg_1000m_bau1_2050_mc2_capped",
      "mdg_1000m_bau1_2050_mc2_uncapped",
      "mdg_1000m_ics3_2050_mc2_capped",
      "mdg_1000m_ics3_2050_mc2_uncapped"
    )
  ),
  kenya = list(
    enabled = FALSE,
    root = "E:/",  # Set the folder containing these four runs before enabling.
    folders = c(
      "ken_1000m_bau1_2050_mc30_capped",
      "ken_1000m_bau1_2050_mc30_uncapped",
      "ken_1000m_ics3_2050_mc30_capped",
      "ken_1000m_ics3_2050_mc30_uncapped"
    )
  ),
  rwanda = list(
    enabled = FALSE,
    root = "E:/",  # Set the folder containing these four runs before enabling.
    folders = c(
      "rwa_1000m_bau1_2050_mc30_capped",
      "rwa_1000m_bau1_2050_mc30_uncapped",
      "rwa_1000m_ics3_2050_mc30_capped",
      "rwa_1000m_ics3_2050_mc30_uncapped"
    )
  )
)

# Run all stages in order. Use 2:3, for example, to resume at Stage 2.
PIPELINE_STAGES <- 2:3

# Used in the versioned analysis-root name and aligned with emissions
# postprocessing. It does not define the empirical validation window.
PIPELINE_SPINUP_YEARS <- 26L

# External validation data.
PIPELINE_ADMIN_VECTOR <- paste0(
  "D:/",
  "admin_regions/regions_adm0/mofuss_regions0.gpkg"
)
PIPELINE_AGB_OBS_TYPE <- "projected"  # projected (MgDM/ha) or latlong (MgCO2/ha)
PIPELINE_AGB_OBS_DIR <- paste0(
  "G:/Mi unidad/webpages/2026_MoFuSSGlobal_Datasets/",
  "ctrees_dic2025_agb_cr/1km_agco2_2000_2025/agb_projected_ha"
)

# NULL writes beside the working-folder parent in mofuss_postprocessing.
# Set an explicit directory only for a controlled alternate output root.
PIPELINE_POSTPROCESSING_ROOT <- NULL

# Disposable computation staging. Final products are promoted to the dedicated
# mofuss_postprocessing analysis root only after a stage succeeds.
PIPELINE_TEMP_ROOT <- "E:/MoFuSS_Active/gog_agb_validation_redesign_2026-09-10"

# Stage 1: pixel-wise mechanics verification.
PIPELINE_GROWTH_MODEL <- "auto"
PIPELINE_DEPLETED_RESET_MG_CELL <- 2
PIPELINE_FLOAT_TOLERANCE_MG_CELL <- 0.01
PIPELINE_PLOT_SEED <- 42L
PIPELINE_PLOT_CELLS_PER_GROUP <- 3L
PIPELINE_RNORM_SCRIPT <- "rnorm_v8.R"
PIPELINE_MAPS_SCRIPT <- "maps_animations_v8.R"

# Stages 2--3: fixed-support BaU AGB consistency validation. Stage 2 prepares
# all-MC regional/country/50-km aggregates; Stage 3 summarizes and plots them.
PIPELINE_AGB_OBS_START_YEAR <- 2000L
PIPELINE_AGB_OBS_END_YEAR <- 2025L
PIPELINE_CTREES_RECENT_COMPARABLE <- FALSE
PIPELINE_PRIMARY_START_YEAR <- 2010L
PIPELINE_PRIMARY_END_YEAR <- if (PIPELINE_CTREES_RECENT_COMPARABLE) 2025L else 2020L
PIPELINE_BLOCK_SIZE_KM <- 50
PIPELINE_MIN_BLOCK_CELLS <- 100L
PIPELINE_BOOTSTRAP_REPS <- 1000L
PIPELINE_BOOTSTRAP_SEED <- 42L
PIPELINE_EXCLUDE_HYDROLAKES <- TRUE
# NULL auto-finds hydrolakes_pcs.tif inside each scenario pair.
PIPELINE_HYDROLAKES_RASTER <- NULL
PIPELINE_CARBON_FRACTION <- 0.47

# TRUE removes the exact validation output tree and task-specific temporary
# products before rebuilding. --check and --dry-run never delete anything.
PIPELINE_CLEAN_REBUILD <- TRUE

# TRUE runs no-write preflights and skips Stage 1, which has no dry-run mode.
# Stage 3 is preflighted only when a prepared Stage-2 product already exists.
PIPELINE_DRY_RUN <- FALSE

# END USER INPUTS -----------------------------------------------------------

pipeline_stop <- function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)

pipeline_script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg)) {
    path <- sub("^--file=", "", file_arg[[1L]])
    if (file.exists(path)) return(normalizePath(path, winslash = "/", mustWork = TRUE))
  }
  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    path <- get0("ofile", envir = frames[[i]], inherits = FALSE, ifnotfound = NA_character_)
    if (length(path) == 1L && !is.na(path) && file.exists(path)) {
      return(normalizePath(path, winslash = "/", mustWork = TRUE))
    }
  }
  candidate <- file.path(getwd(), "calib_valid_agb", "0calib_valid_agb_pipeline_v1.R")
  if (file.exists(candidate)) return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
  pipeline_stop("Could not locate 0calib_valid_agb_pipeline_v1.R.")
}

pipeline_bool <- function(x, label) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    pipeline_stop("%s must be exactly TRUE or FALSE.", label)
  }
  x
}

pipeline_resolve_batches <- function() {
  batches <- PIPELINE_BATCHES
  if (!is.list(batches) || !length(batches) || is.null(names(batches)) ||
      anyNA(names(batches)) || any(!nzchar(trimws(names(batches)))) ||
      anyDuplicated(tolower(trimws(names(batches))))) {
    pipeline_stop("PIPELINE_BATCHES must be a non-empty, uniquely named list.")
  }

  resolved <- list()
  disabled <- character()
  for (batch_name in names(batches)) {
    entry <- batches[[batch_name]]
    label <- sprintf("PIPELINE_BATCHES[['%s']]", batch_name)
    if (!is.list(entry) || !all(c("enabled", "root", "folders") %in% names(entry))) {
      pipeline_stop("%s must contain enabled, root, and folders.", label)
    }
    enabled <- pipeline_bool(entry$enabled, paste0(label, "$enabled"))
    folders <- trimws(as.character(entry$folders))
    if (length(folders) != 4L || anyNA(folders) || any(!nzchar(folders)) ||
        any(grepl("[/\\\\]", folders)) || any(folders %in% c(".", "..")) ||
        anyDuplicated(tolower(folders))) {
      pipeline_stop("%s$folders must contain four unique child-folder names.", label)
    }
    if (!enabled) {
      disabled <- c(disabled, batch_name)
      next
    }

    root <- trimws(as.character(entry$root))
    if (length(root) != 1L || is.na(root) || !nzchar(root)) {
      pipeline_stop("%s is enabled, so %s$root must be declared.", batch_name, label)
    }
    if (!dir.exists(root)) pipeline_stop("Batch '%s' root does not exist: %s", batch_name, root)
    root <- normalizePath(root, winslash = "/", mustWork = TRUE)
    working_dirs <- file.path(root, folders)
    missing <- working_dirs[!dir.exists(working_dirs)]
    if (length(missing)) {
      pipeline_stop("Batch '%s' is missing working folder(s): %s", batch_name, paste(missing, collapse = ", "))
    }
    working_dirs <- vapply(
      working_dirs, normalizePath, character(1), winslash = "/", mustWork = TRUE
    )
    resolved[[batch_name]] <- list(
      name = batch_name, root = root, working_dirs = unname(working_dirs)
    )
  }
  if (!length(resolved)) pipeline_stop("PIPELINE_BATCHES has no enabled batches.")

  all_dirs <- tolower(unlist(lapply(resolved, `[[`, "working_dirs"), use.names = FALSE))
  if (anyDuplicated(all_dirs)) {
    pipeline_stop("Enabled batches may not reuse the same working folder.")
  }
  list(enabled = resolved, disabled = disabled)
}

pipeline_integer <- function(x, label, minimum = 0L) {
  numeric_value <- suppressWarnings(as.numeric(x))
  integer_value <- suppressWarnings(as.integer(x))
  if (length(integer_value) != 1L || is.na(integer_value) ||
      !is.finite(numeric_value) || numeric_value != integer_value || integer_value < minimum) {
    pipeline_stop("%s must be one integer >= %d.", label, minimum)
  }
  integer_value
}

pipeline_safe_id <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (length(x) != 1L || is.na(x) || !nzchar(x)) {
    pipeline_stop("Could not construct an analysis identifier from scenario metadata.")
  }
  x
}

pipeline_guarded_remove_tree <- function(target, expected_parent, expected_leaf) {
  target <- normalizePath(path.expand(target), winslash = "/", mustWork = FALSE)
  expected_parent <- normalizePath(path.expand(expected_parent), winslash = "/", mustWork = FALSE)
  if (!identical(basename(target), expected_leaf) ||
      !identical(tolower(dirname(target)), tolower(expected_parent)) ||
      identical(tolower(target), tolower(expected_parent)) ||
      identical(dirname(expected_parent), expected_parent)) {
    pipeline_stop("Refusing unexpected cleanup target: %s", target)
  }
  if (file.exists(target) && !dir.exists(target)) {
    pipeline_stop("Cleanup target exists but is not a directory: %s", target)
  }
  if (!dir.exists(target)) return(invisible(FALSE))
  resolved_target <- normalizePath(target, winslash = "/", mustWork = TRUE)
  resolved_parent <- normalizePath(expected_parent, winslash = "/", mustWork = TRUE)
  if (!identical(tolower(dirname(resolved_target)), tolower(resolved_parent)) ||
      !identical(basename(resolved_target), expected_leaf)) {
    pipeline_stop("Resolved cleanup target escaped its guarded parent: %s", resolved_target)
  }
  status <- unlink(resolved_target, recursive = TRUE, force = TRUE)
  if (!identical(status, 0L) || file.exists(target)) {
    pipeline_stop("Failed to remove previous validation output: %s", target)
  }
  cat(sprintf("  removed previous output: %s\n", target))
  invisible(TRUE)
}

pipeline_clean_validation_outputs <- function(config) {
  if (!config$clean_rebuild || config$dry_run) return(invisible(FALSE))
  if (2L %in% config$stages) {
    cat("\nGUARDED CLEAN REBUILD\n")
    for (batch in config$batches) {
      target <- file.path(batch$analysis_root, "validation")
      pipeline_guarded_remove_tree(target, batch$analysis_root, "validation")
    }
    temp_parent <- dirname(config$temp_output_root)
    pipeline_guarded_remove_tree(
      config$temp_output_root, temp_parent, basename(config$temp_output_root)
    )
    if (!dir.create(config$temp_output_root, recursive = TRUE, showWarnings = FALSE) &&
        !dir.exists(config$temp_output_root)) {
      pipeline_stop("Could not recreate clean temporary output root: %s", config$temp_output_root)
    }
    cat("  previous validation results fully removed; rebuilding from raw inputs.\n")
  }
  invisible(TRUE)
}

pipeline_parameters_file <- function(working_dir) {
  root <- file.path(working_dir, "LULCC", "DownloadedDatasets")
  files <- list.files(
    root, pattern = "^parameters.*[.]csv$", recursive = TRUE,
    full.names = TRUE, ignore.case = TRUE
  )
  if (length(files) != 1L) {
    pipeline_stop("Expected exactly one parameters*.csv under %s; found %d.", root, length(files))
  }
  files[[1L]]
}

pipeline_metadata <- function(working_dir) {
  path <- pipeline_parameters_file(working_dir)
  table <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (ncol(table) < 2L) pipeline_stop("Invalid parameters table: %s", path)
  keys <- trimws(as.character(table[[1L]]))
  values <- trimws(as.character(table[[2L]]))
  value <- function(key) {
    hit <- values[keys == key]
    hit <- hit[!is.na(hit) & nzchar(hit)]
    if (length(hit) != 1L) pipeline_stop("Expected one '%s' value in %s.", key, path)
    hit[[1L]]
  }
  int_value <- function(key) pipeline_integer(value(key), paste0(key, " in ", path))
  scenario <- value("scenario_ver")
  uncapped <- int_value("uncapped_regrowth")
  role <- if (grepl("^bau", scenario, ignore.case = TRUE)) {
    "bau"
  } else if (grepl("^(ics|ccts)", scenario, ignore.case = TRUE)) {
    "ccts"
  } else {
    pipeline_stop("Cannot classify scenario_ver '%s' in %s.", scenario, path)
  }
  byregion <- trimws(value("byregion"))
  aoi_poly <- int_value("aoi_poly")
  if (!aoi_poly %in% c(0L, 1L)) {
    pipeline_stop("aoi_poly must be 0 or 1 in %s.", path)
  }
  iso3 <- toupper(value("region2BprocessedCtry_iso"))
  country <- value("region2BprocessedCtry")
  region <- value("region2BprocessedReg")
  aoi_poly_file <- value("aoi_poly_file")
  scope <- if (aoi_poly == 1L) {
    aoi_name <- tools::file_path_sans_ext(basename(aoi_poly_file))
    if (!nzchar(aoi_name)) pipeline_stop("Own-polygon run has an empty aoi_poly_file in %s.", path)
    list(kind = "OwnPolygon", id = paste0("AOI_", aoi_name), name = paste0("Own polygon: ", aoi_name))
  } else if (identical(tolower(byregion), "country")) {
    list(kind = "Country", id = iso3, name = country)
  } else if (identical(tolower(byregion), "regional")) {
    if (!nzchar(region)) pipeline_stop("Regional run has an empty region2BprocessedReg in %s.", path)
    list(kind = "Regional", id = region, name = region)
  } else {
    pipeline_stop("Unsupported byregion value '%s' in %s; expected Country or Regional.", byregion, path)
  }
  boundary_path <- file.path(working_dir, "LULCC", "TempVector", "userarea1.gpkg")
  if (!file.exists(boundary_path) || dir.exists(boundary_path)) {
    pipeline_stop("Model-native analysis boundary is missing: %s", boundary_path)
  }
  data.frame(
    working_dir = working_dir,
    iso3 = iso3,
    country = country,
    byregion = byregion,
    region = region,
    aoi_poly = aoi_poly,
    aoi_poly_file = aoi_poly_file,
    analysis_area_kind = scope$kind,
    analysis_area_id = scope$id,
    analysis_area_name = scope$name,
    analysis_boundary = normalizePath(boundary_path, winslash = "/", mustWork = TRUE),
    scenario = scenario,
    role = role,
    mode = if (uncapped == 1L) "uncapped" else "capped",
    model_start = int_value("start_year"),
    model_end = int_value("end_year"),
    mc_runs = int_value("monte_carlo_runs"),
    stringsAsFactors = FALSE
  )
}

pipeline_validate <- function(script_dir) {
  batch_selection <- pipeline_resolve_batches()

  stages <- suppressWarnings(as.integer(PIPELINE_STAGES))
  if (!length(stages) || anyNA(stages) || any(!stages %in% 1:3) ||
      anyDuplicated(stages) || !identical(stages, sort(stages))) {
    pipeline_stop("PIPELINE_STAGES must be an increasing subset of 1:3.")
  }
  stage_scripts <- file.path(script_dir, c(
    "1_mechanics_verifications_v3.R",
    "2_prepare_agb_validation_v2.R",
    "3_mofuss_agb_validation_v4.R"
  ))
  missing <- stage_scripts[!file.exists(stage_scripts)]
  if (length(missing)) pipeline_stop("Missing stage script(s): %s", paste(missing, collapse = ", "))

  spinup_years <- pipeline_integer(PIPELINE_SPINUP_YEARS, "PIPELINE_SPINUP_YEARS")
  admin_vector <- path.expand(as.character(PIPELINE_ADMIN_VECTOR))
  if (length(admin_vector) != 1L || !file.exists(admin_vector) || dir.exists(admin_vector)) {
    pipeline_stop("PIPELINE_ADMIN_VECTOR must be one existing file.")
  }
  admin_vector <- normalizePath(admin_vector, winslash = "/", mustWork = TRUE)
  obs_dirs <- c(agb = PIPELINE_AGB_OBS_DIR)
  obs_dirs <- vapply(obs_dirs, function(path) {
    if (length(path) != 1L || !dir.exists(path)) pipeline_stop("Observation folder does not exist: %s", path)
    normalizePath(path, winslash = "/", mustWork = TRUE)
  }, character(1))

  batch_configs <- lapply(batch_selection$enabled, function(batch) {
    metadata <- do.call(rbind, lapply(batch$working_dirs, pipeline_metadata))
    common_fields <- c(
      "analysis_area_kind", "analysis_area_id", "analysis_area_name",
      "model_start", "model_end", "mc_runs"
    )
    for (field in common_fields) {
      if (length(unique(tolower(as.character(metadata[[field]])))) != 1L) {
        pipeline_stop("Batch '%s' working folders disagree on '%s'.", batch$name, field)
      }
    }
    combos <- paste(metadata$role, metadata$mode, sep = "/")
    expected <- c("bau/capped", "bau/uncapped", "ccts/capped", "ccts/uncapped")
    if (!setequal(combos, expected) || anyDuplicated(combos)) {
      pipeline_stop(
        "Batch '%s' must contain BAU and ICS/CCTS intervention folders, each capped and uncapped; found: %s",
        batch$name, paste(combos, collapse = ", ")
      )
    }
    batch$metadata <- metadata
    batch
  })

  agb_type <- tolower(trimws(as.character(PIPELINE_AGB_OBS_TYPE)))
  if (length(agb_type) != 1L || !agb_type %in% c("projected", "latlong")) {
    pipeline_stop("PIPELINE_AGB_OBS_TYPE must be projected or latlong.")
  }
  growth_model <- tolower(as.character(PIPELINE_GROWTH_MODEL))
  if (length(growth_model) != 1L ||
      !growth_model %in% c("auto", "logistic", "chapman-richards")) {
    pipeline_stop("PIPELINE_GROWTH_MODEL is invalid.")
  }

  agb_base <- pipeline_integer(PIPELINE_AGB_OBS_START_YEAR, "PIPELINE_AGB_OBS_START_YEAR")
  agb_end <- pipeline_integer(PIPELINE_AGB_OBS_END_YEAR, "PIPELINE_AGB_OBS_END_YEAR")
  primary_start <- pipeline_integer(PIPELINE_PRIMARY_START_YEAR, "PIPELINE_PRIMARY_START_YEAR")
  primary_end <- pipeline_integer(PIPELINE_PRIMARY_END_YEAR, "PIPELINE_PRIMARY_END_YEAR")
  if (agb_end <= agb_base) pipeline_stop("AGB observation end year must be later than its start year.")
  if (primary_start < agb_base || primary_end > agb_end || primary_end <= primary_start) {
    pipeline_stop("The primary AGB validation period must lie inside the observation period.")
  }
  recent_comparable <- pipeline_bool(
    PIPELINE_CTREES_RECENT_COMPARABLE, "PIPELINE_CTREES_RECENT_COMPARABLE"
  )
  if (!recent_comparable && primary_end > 2020L) {
    pipeline_stop("Keep the primary end year at or before 2020 while recent CTrees comparability is unconfirmed.")
  }
  for (batch in batch_configs) {
    metadata <- batch$metadata
    if (agb_base < metadata$model_start[[1L]] || agb_end > metadata$model_end[[1L]]) {
      pipeline_stop(
        "Configured validation years fall outside the MoFuSS simulation horizon for batch '%s'.",
        batch$name
      )
    }
  }

  positive_values <- list(
    PIPELINE_DEPLETED_RESET_MG_CELL = PIPELINE_DEPLETED_RESET_MG_CELL,
    PIPELINE_FLOAT_TOLERANCE_MG_CELL = PIPELINE_FLOAT_TOLERANCE_MG_CELL,
    PIPELINE_BLOCK_SIZE_KM = PIPELINE_BLOCK_SIZE_KM,
    PIPELINE_MIN_BLOCK_CELLS = PIPELINE_MIN_BLOCK_CELLS,
    PIPELINE_BOOTSTRAP_REPS = PIPELINE_BOOTSTRAP_REPS,
    PIPELINE_CARBON_FRACTION = PIPELINE_CARBON_FRACTION
  )
  for (name in names(positive_values)) {
    value <- suppressWarnings(as.numeric(positive_values[[name]]))
    lower <- if (name == "PIPELINE_CARBON_FRACTION") 0 else -1e-15
    if (length(value) != 1L || !is.finite(value) || value <= lower) {
      pipeline_stop("%s has an invalid numeric value.", name)
    }
  }

  hydrolakes <- PIPELINE_HYDROLAKES_RASTER
  if (!is.null(hydrolakes) && length(hydrolakes) == 1L && nzchar(hydrolakes)) {
    if (!file.exists(hydrolakes) || dir.exists(hydrolakes)) {
      pipeline_stop("PIPELINE_HYDROLAKES_RASTER does not exist: %s", hydrolakes)
    }
    hydrolakes <- normalizePath(hydrolakes, winslash = "/", mustWork = TRUE)
  } else {
    hydrolakes <- NULL
  }
  exclude_lakes <- pipeline_bool(
    PIPELINE_EXCLUDE_HYDROLAKES, "PIPELINE_EXCLUDE_HYDROLAKES"
  )

  explicit_postprocessing_root <- PIPELINE_POSTPROCESSING_ROOT
  if (!is.null(explicit_postprocessing_root)) {
    explicit_postprocessing_root <- trimws(as.character(explicit_postprocessing_root))
    if (length(explicit_postprocessing_root) != 1L || is.na(explicit_postprocessing_root) ||
        !nzchar(explicit_postprocessing_root)) {
      pipeline_stop("PIPELINE_POSTPROCESSING_ROOT must be NULL or one non-empty directory path.")
    }
    if (!dir.exists(explicit_postprocessing_root)) {
      pipeline_stop("PIPELINE_POSTPROCESSING_ROOT does not exist: %s", explicit_postprocessing_root)
    }
    explicit_postprocessing_root <- normalizePath(
      explicit_postprocessing_root, winslash = "/", mustWork = TRUE
    )
  }
  temp_output_root <- trimws(as.character(PIPELINE_TEMP_ROOT))
  if (length(temp_output_root) != 1L || is.na(temp_output_root) ||
      !nzchar(temp_output_root) || !dir.exists(temp_output_root)) {
    pipeline_stop("PIPELINE_TEMP_ROOT must be one existing task-specific temporary directory.")
  }
  temp_output_root <- normalizePath(temp_output_root, winslash = "/", mustWork = TRUE)
  if (!grepl("validation|calib|agb", basename(temp_output_root), ignore.case = TRUE) ||
      identical(dirname(temp_output_root), temp_output_root) ||
      identical(dirname(dirname(temp_output_root)), dirname(temp_output_root))) {
    pipeline_stop(
      "PIPELINE_TEMP_ROOT must be a specifically named validation task folder below a non-root parent: %s",
      temp_output_root
    )
  }
  for (batch_name in names(batch_configs)) {
    batch <- batch_configs[[batch_name]]
    metadata <- batch$metadata
    analysis_id <- paste(
      pipeline_safe_id(metadata$analysis_area_id[[1L]]),
      metadata$model_start[[1L]] + spinup_years,
      metadata$model_end[[1L]],
      paste0("mc", metadata$mc_runs[[1L]]),
      sep = "_"
    )
    batch$analysis_root <- normalizePath(
      file.path(
        if (is.null(explicit_postprocessing_root)) file.path(batch$root, "mofuss_postprocessing") else explicit_postprocessing_root,
        analysis_id
      ),
      winslash = "/", mustWork = FALSE
    )
    batch_hydrolakes <- hydrolakes
    if (exclude_lakes && is.null(batch_hydrolakes)) {
      rel <- file.path(
        "LULCC", "DownloadedDatasets", "SourceDataGlobal", "InRaster",
        "hydrolakes_pcs.tif"
      )
      candidates <- file.path(batch$working_dirs, rel)
      candidates <- candidates[file.exists(candidates)]
      if (!length(candidates)) {
        pipeline_stop(
          "Could not auto-find hydrolakes_pcs.tif for batch '%s'. Set PIPELINE_HYDROLAKES_RASTER explicitly.",
          batch$name
        )
      }
      batch_hydrolakes <- normalizePath(candidates[[1L]], winslash = "/", mustWork = TRUE)
    }
    batch$hydrolakes <- batch_hydrolakes
    batch_configs[[batch_name]] <- batch
  }
  analysis_roots <- tolower(vapply(
    batch_configs, `[[`, character(1), "analysis_root"
  ))
  if (anyDuplicated(analysis_roots)) {
    pipeline_stop("Enabled batches must resolve to different analysis roots.")
  }

  list(
    batches = batch_configs,
    disabled_batches = batch_selection$disabled,
    stages = stages,
    stage_scripts = stage_scripts,
    spinup_years = spinup_years,
    admin_vector = admin_vector,
    agb_obs_dir = obs_dirs[["agb"]],
    agb_type = agb_type,
    growth_model = growth_model,
    agb_base = agb_base,
    agb_end = agb_end,
    primary_start = primary_start,
    primary_end = primary_end,
    recent_comparable = recent_comparable,
    clean_rebuild = pipeline_bool(PIPELINE_CLEAN_REBUILD, "PIPELINE_CLEAN_REBUILD"),
    dry_run = pipeline_bool(PIPELINE_DRY_RUN, "PIPELINE_DRY_RUN"),
    postprocessing_root = explicit_postprocessing_root,
    temp_output_root = temp_output_root,
    exclude_lakes = exclude_lakes
  )
}

pipeline_rscript <- function() {
  suffix <- if (.Platform$OS.type == "windows") ".exe" else ""
  candidates <- c(
    file.path(R.home("bin"), paste0("Rscript", suffix)),
    file.path(R.home("bin"), "x64", paste0("Rscript", suffix)),
    Sys.which("Rscript")
  )
  candidates <- unique(candidates[nzchar(candidates) & file.exists(candidates)])
  if (!length(candidates)) pipeline_stop("Could not locate Rscript for child stages.")
  normalizePath(candidates[[1L]], winslash = "/", mustWork = TRUE)
}

pipeline_quote_args <- function(args) {
  quote_type <- if (.Platform$OS.type == "windows") "cmd" else "sh"
  vapply(args, shQuote, character(1), type = quote_type)
}

pipeline_run_stage <- function(number, script, args, rscript, temp_dir) {
  cat(sprintf("\n========== MoFuSS calibration/validation Stage %d/3 ==========\n", number))
  cat(sprintf("Script: %s\n", script))
  variables <- c("TMPDIR", "TMP", "TEMP")
  old_values <- Sys.getenv(variables, unset = NA_character_)
  on.exit({
    for (i in seq_along(variables)) {
      if (is.na(old_values[[i]])) {
        Sys.unsetenv(variables[[i]])
      } else {
        do.call(Sys.setenv, setNames(list(old_values[[i]]), variables[[i]]))
      }
    }
  }, add = TRUE)
  do.call(Sys.setenv, as.list(setNames(rep(temp_dir, length(variables)), variables)))
  status <- system2(
    rscript, args = pipeline_quote_args(c(script, args)),
    stdout = "", stderr = "", wait = TRUE
  )
  if (!identical(as.integer(status), 0L)) {
    pipeline_stop("Stage %d failed with exit status %s; later stages were not run.", number, status)
  }
  cat(sprintf("Stage %d completed successfully.\n", number))
  invisible(TRUE)
}

pipeline_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  unknown <- setdiff(args, c("--check", "--dry-run"))
  if (length(unknown)) pipeline_stop("Unknown pipeline argument(s): %s", paste(unknown, collapse = ", "))
  check_only <- "--check" %in% args
  script_path <- pipeline_script_path()
  config <- pipeline_validate(dirname(script_path))
  if ("--dry-run" %in% args) config$dry_run <- TRUE

  cat("MoFuSS calibration/validation pipeline plan\n")
  cat(sprintf("  stages: %s\n", paste(config$stages, collapse = " -> ")))
  cat(sprintf("  enabled batches: %d (%s)\n", length(config$batches), paste(names(config$batches), collapse = ", ")))
  cat(sprintf(
    "  disabled placeholders: %s\n",
    if (length(config$disabled_batches)) paste(config$disabled_batches, collapse = ", ") else "none"
  ))
  cat(sprintf("  spin-up years: %d\n", config$spinup_years))
  cat(sprintf("  admin vector: %s\n", config$admin_vector))
  cat(sprintf("  AGB observations: %s (%s)\n", config$agb_obs_dir, config$agb_type))
  cat(sprintf("  temporary output: %s\n", config$temp_output_root))
  cat(sprintf("  observation period: %d--%d\n", config$agb_base, config$agb_end))
  cat(sprintf("  primary period: %d--%d\n", config$primary_start, config$primary_end))
  cat(sprintf("  recent CTrees comparability confirmed: %s\n", config$recent_comparable))
  cat("  growth_loss_gains.R: excluded\n")
  cat(sprintf("  clean rebuild: %s\n", config$clean_rebuild))
  cat(sprintf("  dry run: %s\n", config$dry_run))
  for (i in seq_along(config$batches)) {
    batch <- config$batches[[i]]
    cat(sprintf("\n  Batch %d/%d: %s\n", i, length(config$batches), batch$name))
    cat(sprintf("    root: %s\n", batch$root))
    cat(sprintf("    working folders: %s\n", paste(basename(batch$working_dirs), collapse = ", ")))
    cat(sprintf(
      "    analysis area: %s (%s)\n",
      batch$metadata$analysis_area_id[[1L]], batch$metadata$analysis_area_kind[[1L]]
    ))
    cat(sprintf("    analysis root: %s\n", batch$analysis_root))
    cat(sprintf("    HydroLakes: %s\n", if (config$exclude_lakes) batch$hydrolakes else "excluded mask disabled"))
  }
  if (check_only) {
    cat("\nCHECK COMPLETE: all enabled batches and inferred paths are valid; no outputs were written.\n")
    return(invisible(config))
  }

  pipeline_clean_validation_outputs(config)

  temp_root <- file.path(tempdir(), "mofuss_calib_valid_agb")
  if (!dir.exists(temp_root) && !dir.create(temp_root, recursive = TRUE)) {
    pipeline_stop("Could not create disposable R scratch folder: %s", temp_root)
  }
  rscript <- pipeline_rscript()
  dry_arg <- if (config$dry_run) "--dry-run" else character()
  overwrite_arg <- if (config$clean_rebuild) "--overwrite" else character()

  for (batch_index in seq_along(config$batches)) {
    batch <- config$batches[[batch_index]]
    batch_id <- gsub("[^A-Za-z0-9._-]+", "_", batch$name)
    batch_temp_dir <- file.path(
      temp_root, sprintf("%02d_%s", batch_index, batch_id)
    )
    if (!dir.exists(batch_temp_dir) && !dir.create(batch_temp_dir, recursive = TRUE)) {
      pipeline_stop("Could not create batch scratch folder: %s", batch_temp_dir)
    }
    batch_staging_root <- file.path(
      config$temp_output_root, sprintf("%02d_%s", batch_index, batch_id)
    )
    if (!config$dry_run && !dir.exists(batch_staging_root) &&
        !dir.create(batch_staging_root, recursive = TRUE, showWarnings = FALSE)) {
      pipeline_stop("Could not create batch validation staging folder: %s", batch_staging_root)
    }
    working_args_stage1 <- paste0("--working-dir=", batch$working_dirs)
    working_args_stage2 <- paste0("--workdir=", batch$working_dirs)
    prepared_dir <- file.path(
      batch_staging_root, "2_agb_consistency_preparation_v2"
    )
    final_validation_dir <- file.path(batch$analysis_root, "validation")
    hydrolakes_arg <- if (config$exclude_lakes) paste0("--hydro=", batch$hydrolakes) else character()
    stage_args <- list(
      c(
        working_args_stage1,
        paste0("--growth-model=", config$growth_model),
        paste0("--depleted-reset-mg-cell=", PIPELINE_DEPLETED_RESET_MG_CELL),
        paste0("--float-tolerance-mg-cell=", PIPELINE_FLOAT_TOLERANCE_MG_CELL),
        paste0("--plot-seed=", pipeline_integer(PIPELINE_PLOT_SEED, "PIPELINE_PLOT_SEED")),
        paste0("--plot-cells-per-group=", pipeline_integer(
          PIPELINE_PLOT_CELLS_PER_GROUP, "PIPELINE_PLOT_CELLS_PER_GROUP", 1L
        )),
        paste0("--rnorm-script=", PIPELINE_RNORM_SCRIPT),
        paste0("--maps-script=", PIPELINE_MAPS_SCRIPT)
      ),
      c(
        working_args_stage2,
        paste0("--postprocessing-dir=", batch$analysis_root),
        paste0("--staging-root=", batch_staging_root),
        paste0("--obs-type=", config$agb_type),
        paste0("--obs-dir=", config$agb_obs_dir),
        paste0("--admin=", config$admin_vector),
        paste0("--obs-start=", config$agb_base),
        paste0("--obs-end=", config$agb_end),
        paste0("--model-start=", batch$metadata$model_start[[1L]]),
        paste0("--primary-start=", config$primary_start),
        paste0("--primary-end=", config$primary_end),
        paste0("--ctrees-recent-comparable=", tolower(config$recent_comparable)),
        paste0("--block-size-km=", PIPELINE_BLOCK_SIZE_KM),
        paste0("--min-block-cells=", pipeline_integer(PIPELINE_MIN_BLOCK_CELLS, "PIPELINE_MIN_BLOCK_CELLS", 1L)),
        paste0("--exclude-hydrolakes=", tolower(config$exclude_lakes)),
        hydrolakes_arg,
        paste0("--carbon-fraction=", PIPELINE_CARBON_FRACTION),
        overwrite_arg,
        dry_arg
      ),
      c(
        paste0("--prepared-dir=", prepared_dir),
        paste0("--staging-root=", batch_staging_root),
        paste0("--final-validation-dir=", final_validation_dir),
        paste0("--bootstrap-reps=", pipeline_integer(PIPELINE_BOOTSTRAP_REPS, "PIPELINE_BOOTSTRAP_REPS", 100L)),
        paste0("--bootstrap-seed=", pipeline_integer(PIPELINE_BOOTSTRAP_SEED, "PIPELINE_BOOTSTRAP_SEED")),
        overwrite_arg,
        dry_arg
      )
    )

    cat(sprintf(
      "\n############ Calibration/validation batch %d/%d: %s ############\n",
      batch_index, length(config$batches), batch$name
    ))
    for (stage in config$stages) {
      if (stage == 1L && config$dry_run) {
        cat("\nStage 1 skipped: PIPELINE_DRY_RUN=TRUE and Stage 1 has no no-write mode.\n")
        next
      }
      if (stage == 3L && config$dry_run && !file.exists(file.path(prepared_dir, "agb_validation_prepared_v2.rds"))) {
        cat("\nStage 3 dry-run skipped: Stage 2 intentionally wrote no prepared product. Its script and configuration were validated.\n")
        next
      }
      pipeline_run_stage(
        stage, config$stage_scripts[[stage]], stage_args[[stage]], rscript,
        batch_temp_dir
      )
    }
    cat(sprintf("\nBATCH COMPLETE: %s\n", batch$name))
    cat(sprintf("ANALYSIS_ROOT=%s\n", batch$analysis_root))
  }

  cat(sprintf("\nPIPELINE COMPLETE: %d batch(es)\n", length(config$batches)))
  invisible(config)
}

tryCatch(
  pipeline_main(),
  error = function(error) {
    message("PIPELINE ERROR: ", conditionMessage(error))
    if (!interactive()) quit(save = "no", status = 1L, runLast = FALSE)
    invisible(NULL)
  }
)
