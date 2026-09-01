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
    enabled = TRUE,
    root = "E:/",  # Set the folder containing these four runs before enabling.
    folders = c(
      "ken_1000m_bau1_2050_mc30_capped",
      "ken_1000m_bau1_2050_mc30_uncapped",
      "ken_1000m_ics3_2050_mc30_capped",
      "ken_1000m_ics3_2050_mc30_uncapped"
    )
  ),
  rwanda = list(
    enabled = TRUE,
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
PIPELINE_STAGES <- 1:3

# Shared by Stages 2 and 3 and aligned with emissions postprocessing.
PIPELINE_SPINUP_YEARS <- 26L

# External validation data. These are the only required non-MoFuSS paths.
PIPELINE_ADMIN_VECTOR <- paste0(
  "C:/Users/aghil/Documents/MoFuSS_localhost/",
  "admin_regions/regions_adm0/mofuss_regions0.gpkg"
)
PIPELINE_FNRB_OBS_DIR <- paste0(
  "G:/Mi unidad/webpages/2026_MoFuSSGlobal_Datasets/",
  "fnrb_obs_data/1km_agco2_2000_2025"
)
PIPELINE_AGB_OBS_TYPE <- "projected"  # projected (MgDM/ha) or latlong (MgCO2/ha)
PIPELINE_AGB_OBS_DIR <- paste0(
  "G:/Mi unidad/webpages/2026_MoFuSSGlobal_Datasets/",
  "ctrees_dic2025_agb_cr/1km_agco2_2000_2025/agb_projected_ha"
)

# Stage 1: pixel-wise mechanics verification.
PIPELINE_GROWTH_MODEL <- "auto"
PIPELINE_DEPLETED_RESET_MG_CELL <- 2
PIPELINE_FLOAT_TOLERANCE_MG_CELL <- 0.01
PIPELINE_PLOT_SEED <- 42L
PIPELINE_PLOT_CELLS_PER_GROUP <- 3L
PIPELINE_RNORM_SCRIPT <- "rnorm_v8.R"
PIPELINE_MAPS_SCRIPT <- "maps_animations_v8.R"

# Stage 2: MC1 simulated-vs-observed NRB validation.
PIPELINE_NRB_START_YEAR <- 2010L
PIPELINE_NRB_END_YEAR <- 2020L
PIPELINE_NRB_RESOLUTION <- "1km"
PIPELINE_NRB_AGG_FACTOR <- 1L
PIPELINE_NRB_AOI_MODE <- "country"  # country or full for non-interactive pipeline runs
PIPELINE_NRB_SQUARE_DRAW_AOI <- TRUE
PIPELINE_NRB_THRESHOLD_MG_PIXEL <- 100
PIPELINE_CTREES_UNITS <- "CO2"      # CO2 or C for *_AGC rasters

# Stage 3: full AGB trajectory and endpoint validation.
PIPELINE_AGB_BASE_YEAR <- 2000L
PIPELINE_AGB_END_YEAR <- 2025L
PIPELINE_AGB_SIM_END_YEAR <- 2050L
PIPELINE_CLIP_OBS_TO_COUNTRY <- TRUE
PIPELINE_EXCLUDE_HYDROLAKES <- TRUE
# NULL auto-finds hydrolakes_pcs.tif inside each scenario pair.
PIPELINE_HYDROLAKES_RASTER <- NULL
PIPELINE_CARBON_FRACTION <- 0.47

# TRUE runs the Stage 2/3 no-write preflights and skips Stage 1, which has no
# dry-run mode. FALSE performs the complete guarded clean rebuild.
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
  data.frame(
    working_dir = working_dir,
    iso3 = toupper(value("region2BprocessedCtry_iso")),
    country = value("region2BprocessedCtry"),
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
    "1_mechanics_verifications_v2.R",
    "2_sim-nrb_vs_obs-nrb_v1.R",
    "3_mofuss_agb_validation_v3.R"
  ))
  missing <- stage_scripts[!file.exists(stage_scripts)]
  if (length(missing)) pipeline_stop("Missing stage script(s): %s", paste(missing, collapse = ", "))

  spinup_years <- pipeline_integer(PIPELINE_SPINUP_YEARS, "PIPELINE_SPINUP_YEARS")
  admin_vector <- path.expand(as.character(PIPELINE_ADMIN_VECTOR))
  if (length(admin_vector) != 1L || !file.exists(admin_vector) || dir.exists(admin_vector)) {
    pipeline_stop("PIPELINE_ADMIN_VECTOR must be one existing file.")
  }
  admin_vector <- normalizePath(admin_vector, winslash = "/", mustWork = TRUE)
  obs_dirs <- c(fnrb = PIPELINE_FNRB_OBS_DIR, agb = PIPELINE_AGB_OBS_DIR)
  obs_dirs <- vapply(obs_dirs, function(path) {
    if (length(path) != 1L || !dir.exists(path)) pipeline_stop("Observation folder does not exist: %s", path)
    normalizePath(path, winslash = "/", mustWork = TRUE)
  }, character(1))

  batch_configs <- lapply(batch_selection$enabled, function(batch) {
    metadata <- do.call(rbind, lapply(batch$working_dirs, pipeline_metadata))
    common_fields <- c("iso3", "country", "model_start", "model_end", "mc_runs")
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
  nrb_resolution <- as.character(PIPELINE_NRB_RESOLUTION)
  if (length(nrb_resolution) != 1L || !nrb_resolution %in% c("1km", "100m")) {
    pipeline_stop("PIPELINE_NRB_RESOLUTION must be 1km or 100m.")
  }
  nrb_aoi <- tolower(as.character(PIPELINE_NRB_AOI_MODE))
  if (length(nrb_aoi) != 1L || !nrb_aoi %in% c("country", "full")) {
    pipeline_stop("Pipeline AOI mode must be country or full; use Stage 2 directly for interactive draw mode.")
  }
  ctrees_units <- toupper(as.character(PIPELINE_CTREES_UNITS))
  if (length(ctrees_units) != 1L || !ctrees_units %in% c("CO2", "C")) {
    pipeline_stop("PIPELINE_CTREES_UNITS must be CO2 or C.")
  }
  growth_model <- tolower(as.character(PIPELINE_GROWTH_MODEL))
  if (length(growth_model) != 1L ||
      !growth_model %in% c("auto", "logistic", "chapman-richards")) {
    pipeline_stop("PIPELINE_GROWTH_MODEL is invalid.")
  }

  nrb_start <- pipeline_integer(PIPELINE_NRB_START_YEAR, "PIPELINE_NRB_START_YEAR")
  nrb_end <- pipeline_integer(PIPELINE_NRB_END_YEAR, "PIPELINE_NRB_END_YEAR")
  agb_base <- pipeline_integer(PIPELINE_AGB_BASE_YEAR, "PIPELINE_AGB_BASE_YEAR")
  agb_end <- pipeline_integer(PIPELINE_AGB_END_YEAR, "PIPELINE_AGB_END_YEAR")
  agb_sim_end <- pipeline_integer(PIPELINE_AGB_SIM_END_YEAR, "PIPELINE_AGB_SIM_END_YEAR")
  if (nrb_end <= nrb_start) pipeline_stop("NRB end year must be later than its start year.")
  if (agb_end <= agb_base || agb_sim_end < agb_end) pipeline_stop("AGB validation years are inconsistent.")
  for (batch in batch_configs) {
    metadata <- batch$metadata
    if (nrb_start < metadata$model_start[[1L]] || nrb_end > metadata$model_end[[1L]] ||
        agb_base < metadata$model_start[[1L]] || agb_sim_end > metadata$model_end[[1L]]) {
      pipeline_stop(
        "Configured validation years fall outside the MoFuSS simulation horizon for batch '%s'.",
        batch$name
      )
    }
  }

  positive_values <- list(
    PIPELINE_DEPLETED_RESET_MG_CELL = PIPELINE_DEPLETED_RESET_MG_CELL,
    PIPELINE_FLOAT_TOLERANCE_MG_CELL = PIPELINE_FLOAT_TOLERANCE_MG_CELL,
    PIPELINE_NRB_THRESHOLD_MG_PIXEL = PIPELINE_NRB_THRESHOLD_MG_PIXEL,
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

  for (batch_name in names(batch_configs)) {
    batch <- batch_configs[[batch_name]]
    metadata <- batch$metadata
    analysis_id <- paste(
      tolower(metadata$iso3[[1L]]),
      metadata$model_start[[1L]] + spinup_years,
      metadata$model_end[[1L]],
      paste0("mc", metadata$mc_runs[[1L]]),
      sep = "_"
    )
    batch$analysis_root <- normalizePath(
      file.path(batch$root, "mofuss_postprocessing", analysis_id),
      winslash = "/", mustWork = FALSE
    )
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
    fnrb_obs_dir = obs_dirs[["fnrb"]],
    agb_obs_dir = obs_dirs[["agb"]],
    agb_type = agb_type,
    nrb_resolution = nrb_resolution,
    nrb_aoi = nrb_aoi,
    ctrees_units = ctrees_units,
    growth_model = growth_model,
    nrb_start = nrb_start,
    nrb_end = nrb_end,
    agb_base = agb_base,
    agb_end = agb_end,
    agb_sim_end = agb_sim_end,
    hydrolakes = hydrolakes,
    dry_run = pipeline_bool(PIPELINE_DRY_RUN, "PIPELINE_DRY_RUN"),
    square_draw = pipeline_bool(PIPELINE_NRB_SQUARE_DRAW_AOI, "PIPELINE_NRB_SQUARE_DRAW_AOI"),
    clip_obs = pipeline_bool(PIPELINE_CLIP_OBS_TO_COUNTRY, "PIPELINE_CLIP_OBS_TO_COUNTRY"),
    exclude_lakes = pipeline_bool(PIPELINE_EXCLUDE_HYDROLAKES, "PIPELINE_EXCLUDE_HYDROLAKES")
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
  cat(sprintf("  fNRB observations: %s\n", config$fnrb_obs_dir))
  cat(sprintf("  AGB observations: %s (%s)\n", config$agb_obs_dir, config$agb_type))
  cat("  growth_loss_gains.R: excluded\n")
  cat(sprintf("  dry run: %s\n", config$dry_run))
  for (i in seq_along(config$batches)) {
    batch <- config$batches[[i]]
    cat(sprintf("\n  Batch %d/%d: %s\n", i, length(config$batches), batch$name))
    cat(sprintf("    root: %s\n", batch$root))
    cat(sprintf("    working folders: %s\n", paste(basename(batch$working_dirs), collapse = ", ")))
    cat(sprintf("    analysis root: %s\n", batch$analysis_root))
  }
  if (check_only) {
    cat("\nCHECK COMPLETE: all enabled batches and inferred paths are valid; no outputs were written.\n")
    return(invisible(config))
  }

  temp_root <- file.path(tempdir(), "mofuss_calib_valid_agb")
  if (!dir.exists(temp_root) && !dir.create(temp_root, recursive = TRUE)) {
    pipeline_stop("Could not create disposable R scratch folder: %s", temp_root)
  }
  rscript <- pipeline_rscript()
  dry_arg <- if (config$dry_run) "--dry-run" else character()
  hydrolakes_arg <- if (!is.null(config$hydrolakes)) {
    paste0("--hydrolakes-raster=", config$hydrolakes)
  } else {
    character()
  }

  for (batch_index in seq_along(config$batches)) {
    batch <- config$batches[[batch_index]]
    batch_id <- gsub("[^A-Za-z0-9._-]+", "_", batch$name)
    batch_temp_dir <- file.path(
      temp_root, sprintf("%02d_%s", batch_index, batch_id)
    )
    if (!dir.exists(batch_temp_dir) && !dir.create(batch_temp_dir, recursive = TRUE)) {
      pipeline_stop("Could not create batch scratch folder: %s", batch_temp_dir)
    }
    working_args <- paste0("--working-dir=", batch$working_dirs)
    stage_args <- list(
      c(
        working_args,
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
        working_args,
        paste0("--spinup-years=", config$spinup_years),
        paste0("--ctrees-dir=", config$fnrb_obs_dir),
        paste0("--admin-vector=", config$admin_vector),
        paste0("--start-year=", config$nrb_start),
        paste0("--end-year=", config$nrb_end),
        paste0("--resolution=", config$nrb_resolution),
        paste0("--agg-factor=", pipeline_integer(PIPELINE_NRB_AGG_FACTOR, "PIPELINE_NRB_AGG_FACTOR", 1L)),
        paste0("--aoi-mode=", config$nrb_aoi),
        paste0("--square-draw-aoi=", tolower(config$square_draw)),
        paste0("--nrb-threshold=", PIPELINE_NRB_THRESHOLD_MG_PIXEL),
        paste0("--ctrees-units=", config$ctrees_units),
        dry_arg
      ),
      c(
        working_args,
        paste0("--spinup-years=", config$spinup_years),
        paste0("--obs-type=", config$agb_type),
        paste0("--obs-dir=", config$agb_obs_dir),
        paste0("--admin-vector=", config$admin_vector),
        paste0("--base-year=", config$agb_base),
        paste0("--end-year=", config$agb_end),
        paste0("--sim-end-year=", config$agb_sim_end),
        paste0("--clip-obs-to-country=", tolower(config$clip_obs)),
        paste0("--exclude-hydrolakes=", tolower(config$exclude_lakes)),
        hydrolakes_arg,
        paste0("--carbon-fraction=", PIPELINE_CARBON_FRACTION),
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
