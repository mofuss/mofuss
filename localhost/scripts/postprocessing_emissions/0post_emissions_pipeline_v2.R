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

# MoFuSS emissions post-processing pipeline ----
#
# This is the normal entry point for Stages 1-5. Edit only the USER INPUTS
# block, then use RStudio Source / Source as Background Job, or run:
#
#   Rscript 0post_emissions_pipeline_v2.R
#
# Each stage runs in a fresh R process. Enabled batches run sequentially, and
# each batch completes its selected stages before the next batch starts. This
# releases raster memory between stages and keeps country outputs isolated.
# Use `Rscript 0post_emissions_pipeline_v2.R --check` for a read-only plan.

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
  mdg = list(
    enabled = TRUE,
    root = "E:/",
    folders = c(
      "mdg_1000m_bau1_2050_mc30_capped",
      "mdg_1000m_bau1_2050_mc30_uncapped",
      "mdg_1000m_ics3_2050_mc30_capped",
      "mdg_1000m_ics3_2050_mc30_uncapped"
    )
  ),
  GLEA = list(
    enabled = FALSE,
    root = "E:/",
    folders = c(
      "GLEA_1000m_bau1_2030_mc2_capped",
      "GLEA_1000m_bau1_2030_mc2_uncapped",
      "GLEA_1000m_ics3_2030_mc2_capped",
      "GLEA_1000m_ics3_2030_mc2_uncapped"
    )
  )
)

# Run all stages in order. Use, for example, 3:4 to resume at Stage 3.
PIPELINE_STAGES <- 2:5

# Stage 1: character() retains the v9 default multi-period/snapshot schedule.
# Otherwise supply one or more explicit periods, for example c("2026:2050").
PIPELINE_STAGE1_PERIODS <- character()
PIPELINE_STAGE1_OUTPUT_SUBDIR <- file.path("Out", "webmofuss_results_v9")

# Stages 2 and 3. Spin-up is the number of years added to the simulation start
# year to obtain the first reporting year. "auto" uses that reporting start
# through the simulation end year.
PIPELINE_SPINUP_YEARS <- 26L
PIPELINE_ANALYSIS_PERIOD <- "auto"
PIPELINE_RUN_IDS <- "all"
PIPELINE_PAIRING_POLICY <- "strict"
PIPELINE_MAKE_DECOMPOSITION_PLOT <- TRUE

# MC2 is suitable for mechanics testing, not manuscript uncertainty. Keeping
# this at 30 makes Stage 4 publish only MC1 manuscript outputs for MC2 batches;
# MC-all tables, figures, and rasters begin at this run-count threshold.
PIPELINE_MIN_UNCERTAINTY_RUNS <- 30L

# TRUE rebuilds each stage's exact, guarded output folder.
PIPELINE_CLEAN_REBUILD <- TRUE

# TRUE performs the available Stage 1-3 dry runs and skips Stages 4-5, which
# have no no-write mode. FALSE performs the complete pipeline.
PIPELINE_DRY_RUN <- FALSE

# Optional disposable raster scratch override. NULL is portable and uses a
# session-specific folder under R's tempdir(). Supply a path only when a
# particular computer needs a dedicated scratch disk.
PIPELINE_TEMP_DIR <- NULL

# Stage 5 combines all included analysis roots once, after every selected
# regional batch has finished. The manifest may include additional completed
# roots that are not enabled above. Relative manifest paths resolve beside this
# pipeline script; output and scratch paths should normally be absolute.
PIPELINE_GLOBAL_MANIFEST <- "global_south_postprocessing_manifest_partial_v1.csv"
PIPELINE_GLOBAL_OUTPUT_DIR <-
  "E:/mofuss_postprocessing/global_south_2026_2050_mc30/manuscript_outputs"
PIPELINE_GLOBAL_TEMP_DIR <-
  "E:/MoFuSS_Active/global_manuscript_outputs_v1_dev"
PIPELINE_GLOBAL_MODE <- "partial"
PIPELINE_GLOBAL_MC_COMBINATION <- "independent"
PIPELINE_GLOBAL_RESAMPLES <- 10000L
PIPELINE_GLOBAL_RANDOM_SEED <- 20260910L

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

  candidate <- file.path(
    getwd(), "localhost", "scripts", "postprocessing_emissions",
    "0post_emissions_pipeline_v2.R"
  )
  if (file.exists(candidate)) {
    return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
  }
  pipeline_stop("Could not locate 0post_emissions_pipeline_v2.R.")
}

pipeline_bool <- function(x, label) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    pipeline_stop("%s must be exactly TRUE or FALSE.", label)
  }
  x
}

pipeline_resolve_path <- function(path, base_dir, label, must_work = FALSE) {
  value <- trimws(as.character(path))
  if (length(value) != 1L || is.na(value) || !nzchar(value)) {
    pipeline_stop("%s must be one non-blank path.", label)
  }
  is_absolute <- grepl("^[A-Za-z]:[/\\\\]", value) ||
    grepl("^[/\\\\]{2}", value) || startsWith(value, "/")
  if (!is_absolute) value <- file.path(base_dir, value)
  normalizePath(path.expand(value), winslash = "/", mustWork = must_work)
}

pipeline_path_key <- function(path, must_work = FALSE) {
  tolower(gsub("/+$", "", normalizePath(
    path, winslash = "/", mustWork = must_work
  )))
}

pipeline_is_descendant <- function(path, parent) {
  path_key <- paste0(pipeline_path_key(path), "/")
  parent_key <- paste0(pipeline_path_key(parent), "/")
  startsWith(path_key, parent_key) && !identical(path_key, parent_key)
}

pipeline_resolve_batches <- function(require_enabled = TRUE) {
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
    scenario_dirs <- file.path(root, folders)
    missing <- scenario_dirs[!dir.exists(scenario_dirs)]
    if (length(missing)) {
      pipeline_stop("Batch '%s' is missing scenario folder(s): %s", batch_name, paste(missing, collapse = ", "))
    }
    scenario_dirs <- vapply(
      scenario_dirs, normalizePath, character(1), winslash = "/", mustWork = TRUE
    )
    resolved[[batch_name]] <- list(
      name = batch_name, root = root, scenario_dirs = unname(scenario_dirs)
    )
  }
  if (require_enabled && !length(resolved)) {
    pipeline_stop("PIPELINE_BATCHES has no enabled batches for Stages 1-4.")
  }

  all_dirs <- tolower(unlist(lapply(resolved, `[[`, "scenario_dirs"), use.names = FALSE))
  if (anyDuplicated(all_dirs)) {
    pipeline_stop("Enabled batches may not reuse the same scenario folder.")
  }
  list(enabled = resolved, disabled = disabled)
}

pipeline_validate_inputs <- function(script_dir) {
  stages <- suppressWarnings(as.integer(PIPELINE_STAGES))
  if (!length(stages) || anyNA(stages) || any(!stages %in% 1:5) ||
      anyDuplicated(stages) || !identical(stages, sort(stages))) {
    pipeline_stop("PIPELINE_STAGES must be an increasing subset of 1:5.")
  }
  batch_selection <- pipeline_resolve_batches(
    require_enabled = any(stages %in% 1:4)
  )

  stage_scripts <- file.path(script_dir, c(
    "1post_raster_fr_generator_diskmemory_v9.R",
    "2post_emissions_bau-vs-ics_v14.R",
    "3post_agb_decomposition_v6.R",
    "4post_manuscript_outputs_v3.R",
    "5post_manuscript_outputsGLOBAL_v1.R"
  ))
  missing_scripts <- stage_scripts[!file.exists(stage_scripts)]
  if (length(missing_scripts)) {
    pipeline_stop("Missing stage script(s): %s", paste(missing_scripts, collapse = ", "))
  }

  if (length(PIPELINE_STAGE1_PERIODS)) {
    specs <- trimws(as.character(PIPELINE_STAGE1_PERIODS))
    if (any(!grepl("^[0-9]{4}:[0-9]{4}$", specs))) {
      pipeline_stop("Every PIPELINE_STAGE1_PERIODS value must have form YYYY:YYYY.")
    }
  }
  if (length(PIPELINE_ANALYSIS_PERIOD) != 1L || is.na(PIPELINE_ANALYSIS_PERIOD) ||
      !grepl("^(auto|[0-9]{4}:[0-9]{4})$", PIPELINE_ANALYSIS_PERIOD, ignore.case = TRUE)) {
    pipeline_stop("PIPELINE_ANALYSIS_PERIOD must be 'auto' or YYYY:YYYY.")
  }
  if (length(PIPELINE_RUN_IDS) != 1L || is.na(PIPELINE_RUN_IDS) || !nzchar(PIPELINE_RUN_IDS)) {
    pipeline_stop("PIPELINE_RUN_IDS must be one non-blank value such as 'all' or '1,2'.")
  }
  pairing_policy <- tolower(trimws(as.character(PIPELINE_PAIRING_POLICY)))
  if (length(pairing_policy) != 1L || is.na(pairing_policy) ||
      !pairing_policy %in% c("strict", "diagnostic")) {
    pipeline_stop("PIPELINE_PAIRING_POLICY must be 'strict' or 'diagnostic'.")
  }
  min_runs <- suppressWarnings(as.integer(PIPELINE_MIN_UNCERTAINTY_RUNS))
  if (length(min_runs) != 1L || is.na(min_runs) || min_runs < 2L) {
    pipeline_stop("PIPELINE_MIN_UNCERTAINTY_RUNS must be an integer >= 2.")
  }
  spinup_numeric <- suppressWarnings(as.numeric(PIPELINE_SPINUP_YEARS))
  spinup_years <- suppressWarnings(as.integer(PIPELINE_SPINUP_YEARS))
  if (length(spinup_years) != 1L || is.na(spinup_years) ||
      !is.finite(spinup_numeric) || spinup_numeric != spinup_years || spinup_years < 0L) {
    pipeline_stop("PIPELINE_SPINUP_YEARS must be one non-negative integer.")
  }

  temp_dir_input <- PIPELINE_TEMP_DIR
  if (is.null(temp_dir_input) ||
      (length(temp_dir_input) == 1L && !is.na(temp_dir_input) &&
       !nzchar(trimws(as.character(temp_dir_input))))) {
    temp_dir <- file.path(tempdir(), "mofuss_postprocessing_emissions")
  } else {
    if (length(temp_dir_input) != 1L || is.na(temp_dir_input) ||
        !nzchar(trimws(as.character(temp_dir_input)))) {
      pipeline_stop("PIPELINE_TEMP_DIR must be NULL or one non-blank folder path.")
    }
    temp_dir <- path.expand(trimws(as.character(temp_dir_input)))
  }
  temp_dir <- normalizePath(temp_dir, winslash = "/", mustWork = FALSE)
  if (file.exists(temp_dir) && !dir.exists(temp_dir)) {
    pipeline_stop("PIPELINE_TEMP_DIR exists and is not a folder: %s", temp_dir)
  }

  global_config <- NULL
  if (5L %in% stages) {
    global_manifest <- pipeline_resolve_path(
      PIPELINE_GLOBAL_MANIFEST, script_dir,
      "PIPELINE_GLOBAL_MANIFEST", must_work = TRUE
    )
    global_output_dir <- pipeline_resolve_path(
      PIPELINE_GLOBAL_OUTPUT_DIR, script_dir,
      "PIPELINE_GLOBAL_OUTPUT_DIR", must_work = FALSE
    )
    global_temp_dir <- pipeline_resolve_path(
      PIPELINE_GLOBAL_TEMP_DIR, script_dir,
      "PIPELINE_GLOBAL_TEMP_DIR", must_work = FALSE
    )
    global_mode <- tolower(trimws(as.character(PIPELINE_GLOBAL_MODE)))
    if (length(global_mode) != 1L || is.na(global_mode) ||
        !global_mode %in% c("partial", "strict")) {
      pipeline_stop("PIPELINE_GLOBAL_MODE must be 'partial' or 'strict'.")
    }
    global_mc_combination <- tolower(trimws(as.character(
      PIPELINE_GLOBAL_MC_COMBINATION
    )))
    if (length(global_mc_combination) != 1L ||
        is.na(global_mc_combination) ||
        !global_mc_combination %in% c("independent", "aligned")) {
      pipeline_stop(
        "PIPELINE_GLOBAL_MC_COMBINATION must be 'independent' or 'aligned'."
      )
    }
    global_resamples_numeric <- suppressWarnings(as.numeric(
      PIPELINE_GLOBAL_RESAMPLES
    ))
    global_resamples <- suppressWarnings(as.integer(
      PIPELINE_GLOBAL_RESAMPLES
    ))
    if (length(global_resamples) != 1L || is.na(global_resamples) ||
        !is.finite(global_resamples_numeric) ||
        global_resamples_numeric != global_resamples ||
        global_resamples < 1000L) {
      pipeline_stop("PIPELINE_GLOBAL_RESAMPLES must be an integer >= 1000.")
    }
    global_seed_numeric <- suppressWarnings(as.numeric(
      PIPELINE_GLOBAL_RANDOM_SEED
    ))
    global_seed <- suppressWarnings(as.integer(
      PIPELINE_GLOBAL_RANDOM_SEED
    ))
    if (length(global_seed) != 1L || is.na(global_seed) ||
        !is.finite(global_seed_numeric) || global_seed_numeric != global_seed) {
      pipeline_stop("PIPELINE_GLOBAL_RANDOM_SEED must be one integer.")
    }
    if (!identical(tolower(basename(global_output_dir)),
                   "manuscript_outputs")) {
      pipeline_stop(
        "PIPELINE_GLOBAL_OUTPUT_DIR must end in manuscript_outputs: %s",
        global_output_dir
      )
    }
    if (file.exists(global_output_dir) && !dir.exists(global_output_dir)) {
      pipeline_stop(
        "PIPELINE_GLOBAL_OUTPUT_DIR exists and is not a folder: %s",
        global_output_dir
      )
    }
    expected_global_temp_root <- normalizePath(
      "E:/MoFuSS_Active", winslash = "/", mustWork = FALSE
    )
    if (!pipeline_is_descendant(global_temp_dir, expected_global_temp_root)) {
      pipeline_stop(
        "PIPELINE_GLOBAL_TEMP_DIR must be below E:/MoFuSS_Active: %s",
        global_temp_dir
      )
    }
    if (file.exists(global_temp_dir) && !dir.exists(global_temp_dir)) {
      pipeline_stop(
        "PIPELINE_GLOBAL_TEMP_DIR exists and is not a folder: %s",
        global_temp_dir
      )
    }
    global_config <- list(
      manifest = global_manifest,
      output_dir = global_output_dir,
      temp_dir = global_temp_dir,
      mode = global_mode,
      mc_combination = global_mc_combination,
      resamples = global_resamples,
      random_seed = global_seed
    )
  }

  list(
    batches = batch_selection$enabled,
    disabled_batches = batch_selection$disabled,
    stages = stages,
    stage_scripts = stage_scripts,
    pairing_policy = pairing_policy,
    spinup_years = spinup_years,
    min_runs = min_runs,
    temp_dir = temp_dir,
    clean_rebuild = pipeline_bool(PIPELINE_CLEAN_REBUILD, "PIPELINE_CLEAN_REBUILD"),
    dry_run = pipeline_bool(PIPELINE_DRY_RUN, "PIPELINE_DRY_RUN"),
    make_plot = pipeline_bool(
      PIPELINE_MAKE_DECOMPOSITION_PLOT, "PIPELINE_MAKE_DECOMPOSITION_PLOT"
    ),
    global = global_config
  )
}

pipeline_manifest_analysis_roots <- function(
  path, allowed_missing_roots = character()
) {
  manifest <- tryCatch(
    utils::read.csv(
      path, stringsAsFactors = FALSE, check.names = FALSE,
      na.strings = c("", "NA")
    ),
    error = function(error) pipeline_stop(
      "Could not read PIPELINE_GLOBAL_MANIFEST: %s", conditionMessage(error)
    )
  )
  required <- c("include", "analysis_root")
  missing <- setdiff(required, names(manifest))
  if (length(missing)) {
    pipeline_stop(
      "PIPELINE_GLOBAL_MANIFEST is missing columns: %s",
      paste(missing, collapse = ", ")
    )
  }
  include <- as.logical(manifest$include)
  if (anyNA(include)) {
    pipeline_stop("PIPELINE_GLOBAL_MANIFEST include values must be TRUE or FALSE.")
  }
  roots <- trimws(as.character(manifest$analysis_root[include]))
  if (!length(roots) || anyNA(roots) || any(!nzchar(roots))) {
    pipeline_stop(
      "PIPELINE_GLOBAL_MANIFEST must contain at least one included analysis_root."
    )
  }
  root_keys <- vapply(
    roots, pipeline_path_key, character(1), must_work = FALSE
  )
  missing_roots <- !dir.exists(roots)
  unexpected_missing <- missing_roots &
    !root_keys %in% allowed_missing_roots
  if (any(unexpected_missing)) {
    pipeline_stop(
      paste0(
        "PIPELINE_GLOBAL_MANIFEST analysis root(s) are missing and will not ",
        "be created by the selected stages: %s"
      ),
      paste(roots[unexpected_missing], collapse = ", ")
    )
  }
  root_keys
}

pipeline_infer_analysis_root <- function(stage2_script, scenario_dirs, spinup_years) {
  stage2_env <- new.env(parent = globalenv())
  stage2_env$MOFUSS_CONFIG_ONLY <- TRUE
  sys.source(stage2_script, envir = stage2_env)
  stage2_env$.V13_SPINUP_YEARS <- spinup_years
  pairs <- stage2_env$.v13_internal_pairs(scenario_dirs)
  roots <- unique(vapply(
    pairs$emissions_dir,
    function(path) dirname(dirname(dirname(path))),
    character(1)
  ))
  if (length(roots) != 1L) {
    pipeline_stop("Scenario folders imply %d analysis roots; expected exactly one.", length(roots))
  }
  normalizePath(roots[[1L]], winslash = "/", mustWork = FALSE)
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
  cat(sprintf("\n========== MoFuSS emissions Stage %d/5 ==========\n", number))
  cat(sprintf("Script: %s\n", script))

  # system2(env=...) is not portable on Windows. Temporarily export the three
  # variables R checks at startup, let the child inherit them, then restore the
  # calling R/RStudio session even when the child fails.
  temp_variables <- c("TMPDIR", "TMP", "TEMP")
  old_temp_values <- Sys.getenv(temp_variables, unset = NA_character_)
  on.exit({
    for (i in seq_along(temp_variables)) {
      if (is.na(old_temp_values[[i]])) {
        Sys.unsetenv(temp_variables[[i]])
      } else {
        do.call(Sys.setenv, setNames(list(old_temp_values[[i]]), temp_variables[[i]]))
      }
    }
  }, add = TRUE)
  do.call(
    Sys.setenv,
    as.list(setNames(rep(temp_dir, length(temp_variables)), temp_variables))
  )

  status <- system2(
    command = rscript,
    args = pipeline_quote_args(c(script, args)),
    stdout = "",
    stderr = "",
    wait = TRUE
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
  config <- pipeline_validate_inputs(dirname(script_path))
  if ("--dry-run" %in% args) config$dry_run <- TRUE
  for (batch_name in names(config$batches)) {
    analysis_root <- pipeline_infer_analysis_root(
      config$stage_scripts[[2L]],
      config$batches[[batch_name]]$scenario_dirs,
      config$spinup_years
    )
    config$batches[[batch_name]]$analysis_root <- analysis_root
    config$batches[[batch_name]]$manuscript_output <- file.path(
      analysis_root, "manuscript_outputs"
    )
  }
  analysis_roots <- tolower(vapply(
    config$batches, `[[`, character(1), "analysis_root"
  ))
  if (anyDuplicated(analysis_roots)) {
    pipeline_stop("Enabled batches must resolve to different analysis roots.")
  }
  if (!is.null(config$global)) {
    allowed_missing_manifest_roots <- if (2L %in% config$stages) {
      analysis_roots
    } else {
      character()
    }
    manifest_roots <- pipeline_manifest_analysis_roots(
      config$global$manifest,
      allowed_missing_roots = allowed_missing_manifest_roots
    )
    missing_batch_roots <- setdiff(analysis_roots, manifest_roots)
    if (length(missing_batch_roots)) {
      pipeline_stop(
        paste0(
          "Stage 5 manifest does not include enabled batch analysis root(s): %s. ",
          "Add them to the manifest or disable Stage 5."
        ),
        paste(missing_batch_roots, collapse = ", ")
      )
    }
    regional_outputs <- tolower(vapply(
      config$batches, `[[`, character(1), "manuscript_output"
    ))
    if (pipeline_path_key(config$global$output_dir) %in% regional_outputs) {
      pipeline_stop(
        "Stage 5 global output must differ from every regional manuscript output."
      )
    }
  }

  cat("MoFuSS emissions pipeline plan\n")
  cat(sprintf("  stages: %s\n", paste(config$stages, collapse = " -> ")))
  enabled_batch_label <- if (length(config$batches)) {
    paste(names(config$batches), collapse = ", ")
  } else {
    "none"
  }
  cat(sprintf(
    "  enabled batches: %d (%s)\n",
    length(config$batches), enabled_batch_label
  ))
  cat(sprintf(
    "  disabled placeholders: %s\n",
    if (length(config$disabled_batches)) paste(config$disabled_batches, collapse = ", ") else "none"
  ))
  cat(sprintf("  spin-up years: %d\n", config$spinup_years))
  cat(sprintf("  temporary scratch: %s\n", config$temp_dir))
  cat(sprintf("  clean rebuild: %s | dry run: %s\n", config$clean_rebuild, config$dry_run))
  for (i in seq_along(config$batches)) {
    batch <- config$batches[[i]]
    cat(sprintf("\n  Batch %d/%d: %s\n", i, length(config$batches), batch$name))
    cat(sprintf("    root: %s\n", batch$root))
    cat(sprintf("    scenarios: %s\n", paste(basename(batch$scenario_dirs), collapse = ", ")))
    analysis_root_note <- if (!dir.exists(batch$analysis_root) &&
                              2L %in% config$stages) {
      " (will be created by Stage 2)"
    } else {
      ""
    }
    cat(sprintf(
      "    analysis root: %s%s\n",
      batch$analysis_root, analysis_root_note
    ))
    cat(sprintf("    manuscript output: %s\n", batch$manuscript_output))
  }
  if (!is.null(config$global)) {
    cat("\n  Stage 5 global aggregation:\n")
    cat(sprintf("    manifest: %s\n", config$global$manifest))
    cat(sprintf("    output: %s\n", config$global$output_dir))
    cat(sprintf("    scratch: %s\n", config$global$temp_dir))
    cat(sprintf(
      "    mode: %s | MC combination: %s | resamples: %d | seed: %d\n",
      config$global$mode, config$global$mc_combination,
      config$global$resamples, config$global$random_seed
    ))
  }

  if (check_only) {
    cat("\nCHECK COMPLETE: all enabled batches and inferred paths are valid; no outputs were written.\n")
    return(invisible(config))
  }

  if (!dir.exists(config$temp_dir) && !dir.create(config$temp_dir, recursive = TRUE)) {
    pipeline_stop("Could not create PIPELINE_TEMP_DIR: %s", config$temp_dir)
  }
  if (file.access(config$temp_dir, 2L) != 0L) {
    pipeline_stop("PIPELINE_TEMP_DIR is not writable: %s", config$temp_dir)
  }

  rscript <- pipeline_rscript()
  stage1_period_args <- if (length(PIPELINE_STAGE1_PERIODS)) {
    paste0("--period=", PIPELINE_STAGE1_PERIODS)
  } else {
    character()
  }
  overwrite_arg <- if (config$clean_rebuild) "--overwrite" else character()
  dry_run_arg <- if (config$dry_run) "--dry-run" else character()
  batch_stages <- intersect(config$stages, 1:4)

  for (batch_index in seq_along(config$batches)) {
    batch <- config$batches[[batch_index]]
    batch_id <- gsub("[^A-Za-z0-9._-]+", "_", batch$name)
    batch_temp_dir <- file.path(
      config$temp_dir, sprintf("%02d_%s", batch_index, batch_id)
    )
    if (!dir.exists(batch_temp_dir) && !dir.create(batch_temp_dir, recursive = TRUE)) {
      pipeline_stop("Could not create batch scratch folder: %s", batch_temp_dir)
    }
    if (file.access(batch_temp_dir, 2L) != 0L) {
      pipeline_stop("Batch scratch folder is not writable: %s", batch_temp_dir)
    }

    scenario_args <- paste0("--scenario-dir=", batch$scenario_dirs)
    stage_args <- list(
      c(
        scenario_args,
        stage1_period_args,
        paste0("--output-subdir=", PIPELINE_STAGE1_OUTPUT_SUBDIR),
        overwrite_arg,
        dry_run_arg
      ),
      c(
        scenario_args,
        paste0("--spinup-years=", config$spinup_years),
        paste0("--period=", PIPELINE_ANALYSIS_PERIOD),
        paste0("--run-ids=", PIPELINE_RUN_IDS),
        paste0("--pairing-policy=", config$pairing_policy),
        paste0("--temp-dir=", batch_temp_dir),
        overwrite_arg,
        dry_run_arg
      ),
      c(
        scenario_args,
        paste0("--spinup-years=", config$spinup_years),
        paste0("--period=", PIPELINE_ANALYSIS_PERIOD),
        paste0("--run-ids=", PIPELINE_RUN_IDS),
        paste0("--pairing-policy=", config$pairing_policy),
        if (!config$make_plot) "--no-plot" else character(),
        overwrite_arg,
        dry_run_arg
      ),
      c(
        paste0("--source-dir=", batch$analysis_root),
        paste0("--output-dir=", batch$manuscript_output),
        paste0("--min-uncertainty-runs=", config$min_runs),
        overwrite_arg
      )
    )

    cat(sprintf(
      "\n################ Emissions batch %d/%d: %s ################\n",
      batch_index, length(config$batches), batch$name
    ))
    for (stage in batch_stages) {
      if (stage == 4L && config$dry_run) {
        cat("\nStage 4 skipped: PIPELINE_DRY_RUN=TRUE and Stage 4 has no no-write mode.\n")
        next
      }
      pipeline_run_stage(
        stage, config$stage_scripts[[stage]], stage_args[[stage]], rscript,
        batch_temp_dir
      )
    }
    cat(sprintf("\nBATCH COMPLETE: %s\n", batch$name))
    cat(sprintf("ANALYSIS_ROOT=%s\n", batch$analysis_root))
    if (4L %in% config$stages && !config$dry_run) {
      cat(sprintf("MANUSCRIPT_OUTPUT=%s\n", batch$manuscript_output))
    }
  }

  if (5L %in% config$stages) {
    if (config$dry_run) {
      cat("\nStage 5 skipped: PIPELINE_DRY_RUN=TRUE and Stage 5 has no no-write mode.\n")
    } else {
      if (!dir.exists(config$global$temp_dir) &&
          !dir.create(config$global$temp_dir, recursive = TRUE)) {
        pipeline_stop(
          "Could not create Stage 5 scratch folder: %s",
          config$global$temp_dir
        )
      }
      if (file.access(config$global$temp_dir, 2L) != 0L) {
        pipeline_stop(
          "Stage 5 scratch folder is not writable: %s",
          config$global$temp_dir
        )
      }
      global_args <- c(
        paste0("--manifest=", config$global$manifest),
        paste0("--output-dir=", config$global$output_dir),
        paste0("--temp-dir=", config$global$temp_dir),
        paste0("--mode=", config$global$mode),
        paste0("--mc-combination=", config$global$mc_combination),
        paste0("--min-runs=", config$min_runs),
        paste0("--global-resamples=", config$global$resamples),
        paste0("--random-seed=", config$global$random_seed),
        overwrite_arg
      )
      pipeline_run_stage(
        5L, config$stage_scripts[[5L]], global_args, rscript,
        config$global$temp_dir
      )
      cat("\nGLOBAL STAGE COMPLETE\n")
      cat(sprintf("GLOBAL_MANUSCRIPT_OUTPUT=%s\n", config$global$output_dir))
    }
  }

  cat(sprintf(
    "\nPIPELINE COMPLETE: %d regional batch(es); Stage 5 %s\n",
    length(config$batches),
    if (5L %in% config$stages && !config$dry_run) "completed" else "not run"
  ))
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
