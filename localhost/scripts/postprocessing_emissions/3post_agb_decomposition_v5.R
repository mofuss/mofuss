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
# Script: 3post_agb_decomposition_v5.R
# Version: 5
# Date: August 2026
# Execution: Source from RStudio; Rscript compatibility is secondary.
# Dinamica EGO does not invoke this script directly.
#
# Purpose: Decompose the period change in the BAU-vs-CCTS AGB difference into
# avoided-loss and enhanced-regrowth components across Monte Carlo runs.
# Inputs: SCENARIO_DIRS, parameters.csv, Stage 2 emissions outputs, scenario AGB
# rasters, and pairing provenance.
# Outputs: AGB-decomposition rasters, tables, uncertainty summaries, and plots
# in the guarded agb_decomposition output directory.
# Side effects: A clean rebuild fully deletes the exact validated decomposition
# directory before writing its replacement products.

# Accounting and pairing notes ----
# The default period starts after the configured modeled spin-up years. It
# evaluates the decomposition state at end-(START-1) and at END, then subtracts
# the former from the latter.
#
# Default: infer BAU/CCTS pairs, the post-spin-up period, all configured runs,
# and outputs. One execution writes the direct nominal MC01 decomposition and
# the MC01:n decomposition uncertainty products.
# A normal RStudio Source run validates every input, fully removes only the exact
# guarded agb_decomposition output folder, and rebuilds it. Rscript users opt in
# to the same clean rebuild with --overwrite. Add --no-plot to omit the PNG.
# Pairing is established from the BAU tables reused by CCTS. Patcher may be
# bypassed, RNG-paired, or intentionally independent between scenarios. The
# last design is valid but semi-paired and includes spatial-allocation noise.

# 2dolist ----

# Internal parameters ----

stopf <- function(fmt, ...) {
  stop(sprintf(fmt, ...), call. = FALSE)
}

V5_SPINUP_YEARS <- 26L
V5_MIN_UNCERTAINTY_RUNS <- 30L

pairing_design_status <- function(
  paired_mc_inputs_validated, patcher_bypassed, patcher_rng_paired
) {
  paired_mc_inputs_validated <- isTRUE(paired_mc_inputs_validated)
  patcher_bypassed <- isTRUE(patcher_bypassed)
  patcher_rng_paired <- isTRUE(patcher_rng_paired)
  full <- paired_mc_inputs_validated &&
    (patcher_bypassed || patcher_rng_paired)
  design <- if (!paired_mc_inputs_validated) {
    "unverified_mc_input_pairing"
  } else if (patcher_bypassed) {
    "paired_mc_inputs_patcher_bypassed"
  } else if (patcher_rng_paired) {
    "paired_mc_inputs_patcher_rng_paired"
  } else {
    "paired_mc_inputs_independent_patcher_rng"
  }
  uncertainty_status <- if (!paired_mc_inputs_validated) {
    "DIAGNOSTIC_ONLY_unverified_bypass_inputs"
  } else if (patcher_bypassed) {
    "paired_mc_inputs_validated_patcher_skipped"
  } else if (patcher_rng_paired) {
    "paired_mc_inputs_and_patcher_rng_validated"
  } else {
    "paired_mc_inputs_validated_independent_patcher_rng"
  }
  list(
    comparison_validated = paired_mc_inputs_validated,
    full_stochastic_pairing_validated = full,
    pairing_design = design,
    independent_patcher_rng_included =
      paired_mc_inputs_validated && !patcher_bypassed && !patcher_rng_paired,
    uncertainty_status = uncertainty_status
  )
}

# EDIT ONLY THIS BLOCK when changing country/region scenario folders.
# Folder order does not define pairing; parameters.csv does.
SCENARIO_DIRS <- c(
  "E:/rwa_1000m_bau1_2050_mc30_capped",
  "E:/rwa_1000m_bau1_2050_mc30_uncapped",
  "E:/rwa_1000m_ics3_2050_mc30_capped",
  "E:/rwa_1000m_ics3_2050_mc30_uncapped"
)

# RSTUDIO SOURCE SETTINGS. Edit these values, then press Source.
# NULL output means <analysis root>/agb_decomposition, inferred from the pairs.
V5_RSTUDIO_OUTPUT_DIR <- NULL
V5_RSTUDIO_PERIOD <- "auto"
V5_RSTUDIO_RUN_IDS <- "all"
V5_RSTUDIO_PAIRING_POLICY <- "strict"
V5_RSTUDIO_DRY_RUN <- FALSE
V5_RSTUDIO_CLEAN_REBUILD <- TRUE
V5_RSTUDIO_MAKE_PLOT <- TRUE

# Load libraries ----
# Required packages are checked and loaded by the Stage 3 runner.

usage <- function() {
  cat(paste0(
    "Usage:\n",
    "  Rscript 3post_agb_decomposition_v5.R ",
    "[--output-dir=DIR] [--period=auto|START:END] [--run-ids=all|LIST] [--dry-run] ",
    "[--pairing-policy=strict|diagnostic] [--overwrite] [--no-plot]\n\n",
    "Default input: SCENARIO_DIRS near the top of this script.\n",
    "RStudio: edit the RSTUDIO SOURCE SETTINGS block and press Source.\n",
    "Pairings, post-spin-up period, stage-2 inputs and output directory are inferred.\n",
    "Default --run-ids=all writes both nominal MC1 and MC1:n uncertainty analyses.\n",
    "Legacy --run-id=N remains supported for one-run diagnostics.\n",
    "Legacy --manifest=CONFIGS.csv remains supported with --output-dir.\n",
    "Strict bypass-table validation is the default. Active independent Patcher RNG is accepted ",
    "as a valid semi-paired design when the BAU/CCTS input tables are verified.\n",
    "Dry-run reads and validates every input and performs all calculations, ",
    "but writes nothing. --overwrite fully removes only the validated exact ",
    "agb_decomposition folder immediately before rebuilding it.\n"
  ))
}

parse_cli <- function(args) {
  out <- list(
    manifest = NULL,
    output_dir = NULL,
    period = "auto",
    run_ids = "all",
    run_id = NULL,
    pairing_policy = "strict",
    dry_run = FALSE,
    overwrite = FALSE,
    make_plot = TRUE,
    help = FALSE
  )
  value_names <- c(
    "--manifest" = "manifest",
    "--output-dir" = "output_dir",
    "--period" = "period",
    "--run-ids" = "run_ids",
    "--run-id" = "run_id",
    "--pairing-policy" = "pairing_policy"
  )
  i <- 1L
  while (i <= length(args)) {
    a <- args[[i]]
    if (a %in% c("--help", "-h")) {
      out$help <- TRUE
    } else if (a == "--dry-run") {
      out$dry_run <- TRUE
    } else if (a == "--overwrite") {
      out$overwrite <- TRUE
    } else if (a == "--no-plot") {
      out$make_plot <- FALSE
    } else if (a %in% names(value_names)) {
      if (i == length(args)) stopf("Missing value after %s.", a)
      i <- i + 1L
      out[[unname(value_names[[a]])]] <- args[[i]]
    } else if (grepl("^--manifest=", a)) {
      out$manifest <- sub("^--manifest=", "", a)
    } else if (grepl("^--output-dir=", a)) {
      out$output_dir <- sub("^--output-dir=", "", a)
    } else if (grepl("^--period=", a)) {
      out$period <- sub("^--period=", "", a)
    } else if (grepl("^--run-ids=", a)) {
      out$run_ids <- sub("^--run-ids=", "", a)
    } else if (grepl("^--run-id=", a)) {
      out$run_id <- sub("^--run-id=", "", a)
    } else if (grepl("^--pairing-policy=", a)) {
      out$pairing_policy <- sub("^--pairing-policy=", "", a)
    } else {
      stopf("Unknown argument: %s", a)
    }
    i <- i + 1L
  }
  out
}

is_absolute_path <- function(x) {
  grepl("^[A-Za-z]:[/\\\\]", x) || grepl("^[/\\\\]{2}", x) || grepl("^/", x)
}

resolve_path <- function(x, base_dir, must_exist = TRUE, kind = c("any", "file", "dir")) {
  kind <- match.arg(kind)
  if (length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stopf("A required path is blank.")
  }
  x <- path.expand(trimws(x))
  p <- if (is_absolute_path(x)) x else file.path(base_dir, x)
  p <- normalizePath(p, winslash = "/", mustWork = FALSE)
  if (must_exist && !file.exists(p)) stopf("Path does not exist: %s", p)
  if (must_exist && kind == "file" && dir.exists(p)) stopf("Expected a file, found a directory: %s", p)
  if (must_exist && kind == "dir" && !dir.exists(p)) stopf("Expected a directory: %s", p)
  p
}

v5_path_key <- function(path, must_work = FALSE) {
  tolower(gsub("/+$", "", normalizePath(
    path, winslash = "/", mustWork = must_work
  )))
}

v5_is_within <- function(path, parent) {
  child_key <- v5_path_key(path, FALSE)
  parent_key <- v5_path_key(parent, FALSE)
  identical(child_key, parent_key) || startsWith(child_key, paste0(parent_key, "/"))
}

v5_root_like <- function(path) {
  key <- gsub("\\\\", "/", path)
  identical(key, "/") ||
    grepl("^[a-z]:/?$", key, ignore.case = TRUE) ||
    grepl("^//[^/]+/[^/]+/?$", key)
}

validate_v5_clean_target <- function(output_dir, input_dirs, inferred_output = NULL) {
  target <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  if (!identical(tolower(basename(target)), "agb_decomposition")) {
    stopf(
      "Refusing clean rebuild: Stage 3 output leaf must be exactly 'agb_decomposition': %s",
      target
    )
  }
  parent <- normalizePath(dirname(target), winslash = "/", mustWork = FALSE)
  if (v5_root_like(target) || v5_root_like(parent)) {
    stopf("Refusing clean rebuild at a filesystem root or its direct child: %s", target)
  }
  if (!is.null(inferred_output) &&
      !identical(v5_path_key(target), v5_path_key(inferred_output))) {
    stopf(
      "Refusing clean rebuild: inferred Stage 3 output is %s, not %s.",
      normalizePath(inferred_output, winslash = "/", mustWork = FALSE), target
    )
  }
  for (input_dir in unique(input_dirs)) {
    if (v5_is_within(target, input_dir) || v5_is_within(input_dir, target)) {
      stopf(
        "Refusing clean rebuild because output and scenario input overlap: %s ; %s",
        target, input_dir
      )
    }
  }
  if (dir.exists(target)) {
    resolved <- normalizePath(target, winslash = "/", mustWork = TRUE)
    if (!identical(v5_path_key(resolved, TRUE), v5_path_key(target, TRUE))) {
      stopf("Refusing clean rebuild through a redirected output path: %s", target)
    }
  }
  target
}

clean_v5_output <- function(target) {
  if (!dir.exists(target)) return(invisible(FALSE))
  message("Removing existing Stage 3 output folder: ", target)
  status <- unlink(target, recursive = TRUE, force = TRUE)
  if (status != 0L || file.exists(target)) {
    stopf("Could not fully remove existing Stage 3 output folder: %s", target)
  }
  invisible(TRUE)
}

file_md5 <- function(path) {
  if (!file.exists(path) || dir.exists(path)) stopf("Cannot hash missing/non-file path: %s", path)
  unname(as.character(tools::md5sum(path)))
}

strict_numeric <- function(x, field, context = "input") {
  raw <- trimws(as.character(x))
  clean <- gsub(",", "", raw, fixed = TRUE)
  value <- suppressWarnings(as.numeric(clean))
  bad <- is.na(raw) | !nzchar(raw) | !is.finite(value)
  if (any(bad)) {
    stopf("%s has blank, non-numeric, or non-finite values in '%s'.", context, field)
  }
  value
}

strict_integer <- function(x, field, context = "input") {
  value <- strict_numeric(x, field, context)
  if (any(abs(value - round(value)) > 1e-9)) {
    stopf("%s has non-integer values in '%s'.", context, field)
  }
  as.integer(round(value))
}

safe_label <- function(x) {
  y <- gsub("[^A-Za-z0-9._-]+", "_", trimws(x))
  y <- gsub("^_+|_+$", "", y)
  if (!nzchar(y)) stopf("Manifest label '%s' cannot form a safe output name.", x)
  y
}

parse_period <- function(x) {
  if (identical(tolower(trimws(x)), "auto")) return(NULL)
  if (length(x) != 1L || !grepl("^[0-9]{4}:[0-9]{4}$", x)) {
    stopf("--period must have the form START:END, for example 2000:2030.")
  }
  z <- as.integer(strsplit(x, ":", fixed = TRUE)[[1]])
  if (z[[2]] < z[[1]]) stopf("Period end must not precede period start: %s", x)
  list(start = z[[1]], end = z[[2]], label = sprintf("%d-%d", z[[1]], z[[2]]))
}

parse_run_ids <- function(x, configured_runs) {
  if (length(configured_runs) != 1L || is.na(configured_runs) || configured_runs < 1L) {
    stopf("Configured Monte Carlo run count must be one positive integer.")
  }
  x <- tolower(trimws(as.character(x)))
  if (length(x) != 1L || is.na(x) || !nzchar(x)) {
    stopf("--run-ids must be 'all' or a comma-separated list/range.")
  }
  if (identical(x, "all")) return(seq_len(configured_runs))
  tokens <- strsplit(x, ",", fixed = TRUE)[[1L]]
  ids <- integer()
  for (token in tokens) {
    token <- trimws(token)
    if (grepl("^[0-9]+$", token)) {
      ids <- c(ids, as.integer(token))
    } else if (grepl("^[0-9]+:[0-9]+$", token)) {
      bounds <- as.integer(strsplit(token, ":", fixed = TRUE)[[1L]])
      if (bounds[[1L]] > bounds[[2L]]) stopf("Descending run range is invalid: %s", token)
      ids <- c(ids, seq.int(bounds[[1L]], bounds[[2L]]))
    } else {
      stopf("Invalid --run-ids token: %s", token)
    }
  }
  ids <- sort(unique(ids))
  if (!length(ids) || any(ids < 1L) || any(ids > configured_runs)) {
    stopf("--run-ids must be within 1..%d.", configured_runs)
  }
  ids
}

read_manifest <- function(path) {
  dat <- readr::read_csv(path, show_col_types = FALSE, name_repair = "minimal")
  if (anyDuplicated(names(dat))) stopf("Manifest has duplicate column names: %s", path)
  required <- c("label", "bau_dir", "ics_dir", "emissions_dir")
  missing <- setdiff(required, names(dat))
  if (length(missing)) stopf("Manifest is missing required column(s): %s", paste(missing, collapse = ", "))
  if (!nrow(dat)) stopf("Manifest has no configuration rows: %s", path)
  manifest_dir <- dirname(path)
  labels <- trimws(as.character(dat$label))
  if (any(is.na(labels) | !nzchar(labels))) stopf("Every manifest row must have a non-blank label.")
  safe <- vapply(labels, safe_label, character(1))
  if (anyDuplicated(tolower(labels))) stopf("Manifest labels must be unique (case-insensitive).")
  if (anyDuplicated(tolower(safe))) stopf("Manifest labels collide after output-name sanitization.")
  rows <- vector("list", nrow(dat))
  for (i in seq_len(nrow(dat))) {
    rows[[i]] <- list(
      manifest_row = i,
      label = labels[[i]],
      safe_label = safe[[i]],
      bau_dir = resolve_path(as.character(dat$bau_dir[[i]]), manifest_dir, TRUE, "dir"),
      ics_dir = resolve_path(as.character(dat$ics_dir[[i]]), manifest_dir, TRUE, "dir"),
      emissions_dir = resolve_path(as.character(dat$emissions_dir[[i]]), manifest_dir, TRUE, "dir")
    )
    if (tolower(rows[[i]]$bau_dir) == tolower(rows[[i]]$ics_dir)) {
      stopf("Manifest row %d uses the same BAU and ICS directory.", i)
    }
  }
  rows
}

configs_from_pairs <- function(pairs) {
  lapply(seq_len(nrow(pairs)), function(i) {
    list(
      manifest_row = i,
      label = as.character(pairs$label[[i]]),
      safe_label = safe_label(as.character(pairs$label[[i]])),
      bau_dir = as.character(pairs$bau_dir[[i]]),
      ics_dir = as.character(pairs$ics_dir[[i]]),
      emissions_dir = as.character(pairs$emissions_dir[[i]])
    )
  })
}

read_parameters <- function(scenario_dir, role = NULL) {
  path <- file.path(
    scenario_dir, "LULCC", "DownloadedDatasets", "SourceDataGlobal", "parameters.csv"
  )
  if (!file.exists(path)) stopf("Missing %s scenario parameters: %s", role, path)
  first <- readLines(path, n = 1L, warn = FALSE)
  delim <- if (grepl(";", first, fixed = TRUE)) ";" else ","
  tab <- readr::read_delim(
    path, delim = delim, show_col_types = FALSE, trim_ws = TRUE,
    name_repair = "minimal"
  )
  if (!all(c("Var", "ParCHR") %in% names(tab))) {
    stopf("Scenario parameters require columns Var and ParCHR: %s", path)
  }
  get_one <- function(key) {
    hit <- which(trimws(as.character(tab$Var)) == key)
    if (length(hit) != 1L) stopf("Expected exactly one '%s' row in %s", key, path)
    value <- trimws(as.character(tab$ParCHR[[hit]]))
    if (is.na(value) || !nzchar(value)) stopf("Parameter '%s' is blank in %s", key, path)
    value
  }
  out <- list(
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    md5 = file_md5(path),
    scenario_ver = get_one("scenario_ver"),
    simulation_start_year = strict_integer(get_one("start_year"), "start_year", path),
    simulation_end_year = strict_integer(get_one("end_year"), "end_year", path),
    monte_carlo_runs = strict_integer(get_one("monte_carlo_runs"), "monte_carlo_runs", path),
    uncapped_regrowth = strict_integer(get_one("uncapped_regrowth"), "uncapped_regrowth", path),
    byregion = get_one("byregion"),
    continent = get_one("region2BprocessedCont"),
    region = get_one("region2BprocessedReg"),
    country_iso = toupper(get_one("region2BprocessedCtry_iso")),
    country_name = get_one("region2BprocessedCtry"),
    subcountry = get_one("subcountry"),
    epsg_pcs = get_one("epsg_pcs"),
    gee_scale = strict_numeric(get_one("GEE_scale"), "GEE_scale", path),
    efchratio = strict_numeric(get_one("efchratio"), "efchratio", path)
  )
  if (!out$uncapped_regrowth %in% c(0L, 1L)) {
    stopf("uncapped_regrowth must be 0 or 1 in %s", path)
  }
  if (out$simulation_end_year < out$simulation_start_year) {
    stopf("Scenario end_year precedes start_year in %s", path)
  }
  if (out$monte_carlo_runs < 1L) stopf("monte_carlo_runs must be positive in %s", path)
  if (!is.null(role)) {
    role_ok <- if (role == "BAU") {
      grepl("^bau", out$scenario_ver, ignore.case = TRUE)
    } else {
      !grepl("^bau", out$scenario_ver, ignore.case = TRUE)
    }
    if (!role_ok) {
      stopf("%s scenario_ver '%s' has the wrong scenario role (%s).",
            role, out$scenario_ver, path)
    }
  }
  out
}

v5_safe_id <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) stopf("Cannot construct an output identifier from scenario metadata.")
  x
}

v5_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  command_files <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
  frame_files <- unlist(lapply(sys.frames(), function(frame) {
    tryCatch(
      {
        value <- get("ofile", envir = frame, inherits = FALSE)
        if (is.character(value) && length(value) == 1L) value else character()
      },
      error = function(error) character()
    )
  }), use.names = FALSE)
  rstudio_file <- tryCatch(
    {
      if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        rstudioapi::getSourceEditorContext()$path
      } else {
        character()
      }
    },
    error = function(error) character()
  )
  candidates <- unique(c(
    command_files,
    frame_files,
    rstudio_file,
    file.path(getwd(), "3post_agb_decomposition_v5.R"),
    file.path(
      getwd(), "localhost", "scripts", "postprocessing_emissions",
      "3post_agb_decomposition_v5.R"
    )
  ))
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  candidates <- candidates[basename(candidates) == "3post_agb_decomposition_v5.R"]
  if (!length(candidates)) return(NA_character_)
  normalizePath(candidates[[1]], winslash = "/", mustWork = TRUE)
}

v5_internal_pairs <- function(scenario_dirs = SCENARIO_DIRS) {
  if (!length(scenario_dirs)) stopf("SCENARIO_DIRS is empty.")
  paths <- vapply(
    scenario_dirs, resolve_path, character(1),
    base_dir = getwd(), must_exist = TRUE, kind = "dir"
  )
  if (anyDuplicated(tolower(paths))) stopf("SCENARIO_DIRS contains duplicate folders.")
  parents <- unique(tolower(dirname(paths)))
  if (length(parents) != 1L) {
    stopf("All SCENARIO_DIRS must share one immediate parent for automatic outputs.")
  }
  parent <- dirname(paths)[match(parents[[1]], tolower(dirname(paths)))]
  parameters <- lapply(paths, read_parameters)
  metadata <- data.frame(
    scenario_dir = paths,
    scenario = vapply(parameters, `[[`, character(1), "scenario_ver"),
    role = ifelse(
      grepl("^bau", vapply(parameters, `[[`, character(1), "scenario_ver"), ignore.case = TRUE),
      "BAU", "CCTS"
    ),
    iso3 = vapply(parameters, `[[`, character(1), "country_iso"),
    country = vapply(parameters, `[[`, character(1), "country_name"),
    byregion = vapply(parameters, `[[`, character(1), "byregion"),
    continent = vapply(parameters, `[[`, character(1), "continent"),
    region = vapply(parameters, `[[`, character(1), "region"),
    subcountry = vapply(parameters, `[[`, character(1), "subcountry"),
    start_year = vapply(parameters, `[[`, integer(1), "simulation_start_year"),
    end_year = vapply(parameters, `[[`, integer(1), "simulation_end_year"),
    mc_runs = vapply(parameters, `[[`, integer(1), "monte_carlo_runs"),
    uncapped = vapply(parameters, `[[`, integer(1), "uncapped_regrowth"),
    gee_scale = vapply(parameters, `[[`, numeric(1), "gee_scale"),
    epsg_pcs = vapply(parameters, function(x) as.integer(x$epsg_pcs), integer(1)),
    efchratio = vapply(parameters, `[[`, numeric(1), "efchratio"),
    stringsAsFactors = FALSE
  )
  key_fields <- c(
    "iso3", "country", "byregion", "continent", "region", "subcountry",
    "start_year", "end_year", "mc_runs", "uncapped", "gee_scale",
    "epsg_pcs", "efchratio"
  )
  keys <- apply(metadata[, key_fields, drop = FALSE], 1L, function(row) {
    paste(tolower(trimws(as.character(row))), collapse = "|")
  })
  bau <- which(metadata$role == "BAU")
  alternatives <- which(metadata$role == "CCTS")
  if (!length(bau)) stopf("SCENARIO_DIRS contains no scenario_ver beginning with BAU.")
  if (!length(alternatives)) stopf("SCENARIO_DIRS contains no CCTS/alternative scenario.")
  rows <- lapply(alternatives, function(i) {
    match_bau <- bau[keys[bau] == keys[[i]]]
    if (length(match_bau) != 1L) {
      stopf("Alternative '%s' matched %d BAU folders; expected exactly one: %s",
            metadata$scenario[[i]], length(match_bau), metadata$scenario_dir[[i]])
    }
    b <- metadata[match_bau, , drop = FALSE]
    a <- metadata[i, , drop = FALSE]
    mode <- if (a$uncapped == 1L) "uncapped" else "capped"
    analysis_start_year <- a$start_year + V5_SPINUP_YEARS
    scope_type <- tolower(trimws(a$byregion))
    scope_name <- if (identical(scope_type, "country")) {
      a$iso3
    } else if (identical(scope_type, "regional")) {
      a$region
    } else {
      stopf(
        "Unsupported byregion value '%s' for %s. Expected 'Country' or 'Regional'.",
        a$byregion, a$scenario
      )
    }
    scope_id <- v5_safe_id(scope_name)
    label <- paste(
      scope_id,
      paste0(format(a$gee_scale, scientific = FALSE, trim = TRUE), "m"),
      paste0(v5_safe_id(b$scenario), "_vs_", v5_safe_id(a$scenario)),
      paste0(analysis_start_year, "_", a$end_year),
      paste0("mc", a$mc_runs),
      mode,
      sep = "_"
    )
    analysis_id <- paste(
      scope_id, analysis_start_year, a$end_year, paste0("mc", a$mc_runs),
      sep = "_"
    )
    emissions_dir <- normalizePath(
      file.path(parent, "mofuss_postprocessing", analysis_id, "pairs", label, "emissions"),
      winslash = "/", mustWork = FALSE
    )
    data.frame(
      label = label,
      bau_dir = b$scenario_dir,
      ics_dir = a$scenario_dir,
      emissions_dir = emissions_dir,
      model_start_year = a$start_year,
      model_end_year = a$end_year,
      analysis_start_year = analysis_start_year,
      mc_runs = a$mc_runs,
      analysis_root = normalizePath(
        file.path(parent, "mofuss_postprocessing", analysis_id),
        winslash = "/", mustWork = FALSE
      ),
      stringsAsFactors = FALSE
    )
  })
  pairs <- do.call(rbind, rows)
  rownames(pairs) <- NULL
  if (anyDuplicated(tolower(pairs$label))) stopf("Automatically inferred pair labels are not unique.")
  if (length(unique(tolower(pairs$analysis_root))) != 1L) {
    stopf("Stage 3 requires one country/region and one common model horizon per run.")
  }
  pairs
}

folder_regrowth_mode <- function(path) {
  b <- basename(path)
  if (grepl("_ng$", b, ignore.case = TRUE)) return("capped")
  if (grepl("_g$", b, ignore.case = TRUE)) return("uncapped")
  NA_character_
}

validate_parameter_pair <- function(cfg, bau, ics, run_id, period) {
  same_fields <- c(
    "simulation_start_year", "simulation_end_year", "monte_carlo_runs",
    "uncapped_regrowth", "byregion", "continent", "region", "country_iso",
    "country_name", "subcountry", "epsg_pcs", "gee_scale"
  )
  for (field in same_fields) {
    if (!isTRUE(all.equal(bau[[field]], ics[[field]], tolerance = 0))) {
      stopf("Config '%s' has mismatched BAU/ICS parameter '%s'.", cfg$label, field)
    }
  }
  if (run_id > bau$monte_carlo_runs) {
    stopf("Config '%s' requests run %d but monte_carlo_runs=%d.",
          cfg$label, run_id, bau$monte_carlo_runs)
  }
  if (period$start < bau$simulation_start_year) {
    stopf("Config '%s' period starts in %d, before simulation start %d.",
          cfg$label, period$start, bau$simulation_start_year)
  }
  analysis_start_year <- bau$simulation_start_year + V5_SPINUP_YEARS
  if (period$start < analysis_start_year) {
    stopf(
      "Config '%s' period starts in %d, before post-spin-up analysis start %d.",
      cfg$label, period$start, analysis_start_year
    )
  }
  if (period$end > bau$simulation_end_year) {
    stopf("Config '%s' period ends in %d, after scenario end_year=%d.",
          cfg$label, period$end, bau$simulation_end_year)
  }
  mode <- if (bau$uncapped_regrowth == 1L) "uncapped" else "capped"
  for (p in c(cfg$bau_dir, cfg$ics_dir)) {
    suffix_mode <- folder_regrowth_mode(p)
    if (!is.na(suffix_mode) && suffix_mode != mode) {
      stopf("Config '%s' folder suffix disagrees with uncapped_regrowth=%d: %s",
            cfg$label, bau$uncapped_regrowth, p)
    }
  }
  label_lower <- tolower(cfg$label)
  label_mode <- if (grepl("uncapped", label_lower)) {
    "uncapped"
  } else if (grepl("capped", label_lower)) {
    "capped"
  } else {
    NA_character_
  }
  if (!is.na(label_mode) && label_mode != mode) {
    stopf("Config label '%s' disagrees with uncapped_regrowth=%d.",
          cfg$label, bau$uncapped_regrowth)
  }
  full_horizon <- period$start == bau$simulation_start_year
  baseline_year <- if (full_horizon) bau$simulation_start_year else period$start - 1L
  baseline_timing <- if (full_horizon) {
    "start_of_model_start_year_before_first_step"
  } else {
    "end_of_previous_year"
  }
  list(
    full_horizon = full_horizon,
    baseline_year = baseline_year,
    baseline_code = if (full_horizon) 0L else baseline_year - bau$simulation_start_year + 1L,
    baseline_source = if (full_horizon) "initial_agb_reference" else "Growth_less_harv",
    baseline_timing = baseline_timing,
    end_code = period$end - bau$simulation_start_year + 1L,
    regrowth_mode = mode
  )
}

read_mc_row <- function(path, run_id) {
  tab <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(tab)) stopf("Empty Monte Carlo parameter table: %s", path)
  if ("Key" %in% names(tab)) {
    key <- strict_integer(tab$Key, "Key", path)
    hit <- which(key == run_id)
    if (length(hit) != 1L) stopf("Expected one Key=%d row in %s", run_id, path)
  } else {
    if (run_id > nrow(tab)) stopf("Run %d is absent from %s", run_id, path)
    hit <- run_id
  }
  row <- tab[hit, , drop = FALSE]
  drop <- is.na(names(row)) | !nzchar(names(row)) | grepl("^X($|[.][0-9]+$)", names(row))
  row[, !drop, drop = FALSE]
}

validate_mc_pairing <- function(cfg, run_id) {
  # These exported tables define every paired numerical input. When Patcher is
  # active its internal spatial RNG may remain independent, but its requested
  # cell counts and prune factors must still match BAU by run ID.
  rel <- file.path("Temp", c(
    "k_all.csv", "rmax_all.csv", "i_st_all.csv",
    "Harvest_pixels_V.csv", "Harvest_pixels_W.csv",
    "Prune_factor_V.csv", "Prune_factor_W.csv"
  ))
  for (r in rel) {
    b <- file.path(cfg$bau_dir, r)
    i <- file.path(cfg$ics_dir, r)
    if (!file.exists(b) || !file.exists(i)) {
      stopf("Cannot verify paired MC run %d for config '%s'; missing %s in BAU or ICS.",
            run_id, cfg$label, r)
    }
    br <- read_mc_row(b, run_id)
    ir <- read_mc_row(i, run_id)
    if (!identical(names(br), names(ir)) ||
        !isTRUE(all.equal(br, ir, tolerance = 0, check.attributes = FALSE))) {
      stopf(paste0(
        "Config '%s' run %d is not a paired BAU/ICS MC realization in %s. ",
        "Reuse the same MC input draws; do not pair independent run IDs."
      ), cfg$label, run_id, r)
    }
  }
  TRUE
}

read_pairing_provenance <- function(cfg, bau, ics, pairing_policy) {
  path <- file.path(cfg$ics_dir, "Temp", "mc_bypass_manifest.csv")
  if (!file.exists(path) || dir.exists(path)) {
    status <- list(
      manifest_path = normalizePath(path, winslash = "/", mustWork = FALSE),
      manifest_md5 = NA_character_, bypass_status = "missing",
      bypass_mode = NA_character_, mc_tables_declared_reused = FALSE,
      patcher_bypassed = FALSE,
      patcher_rng_paired = FALSE, comparison_validated = FALSE,
      full_stochastic_pairing_validated = FALSE,
      pairing_design = "unverified_mc_input_pairing",
      independent_patcher_rng_included = FALSE,
      issue = "CCTS mc_bypass_manifest.csv is missing"
    )
  } else {
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    tab <- readr::read_csv(
      path, show_col_types = FALSE, name_repair = "minimal",
      col_types = readr::cols(.default = readr::col_character())
    )
    required <- c(
      "status", "mode", "current_scenario_dir", "current_scenario_ver",
      "bau_source_dir", "bau_scenario_ver", "geography", "start_year",
      "end_year", "monte_carlo_runs", "uncapped_regrowth",
      "patcher_bypassed", "patcher_rng_paired"
    )
    missing <- setdiff(required, names(tab))
    if (nrow(tab) != 1L || length(missing)) {
      stopf("Pairing manifest must contain one row and fields %s: %s",
            paste(required, collapse = ", "), path)
    }
    value <- function(field) trimws(as.character(tab[[field]][[1]]))
    current_dir <- resolve_path(value("current_scenario_dir"), dirname(path), TRUE, "dir")
    source_dir <- resolve_path(value("bau_source_dir"), dirname(path), TRUE, "dir")
    checks <- c(
      current_scenario_dir = identical(tolower(current_dir), tolower(cfg$ics_dir)),
      bau_source_dir = identical(tolower(source_dir), tolower(cfg$bau_dir)),
      current_scenario_ver = identical(value("current_scenario_ver"), ics$scenario_ver),
      bau_scenario_ver = identical(value("bau_scenario_ver"), bau$scenario_ver),
      geography = identical(toupper(value("geography")), toupper(ics$country_iso)),
      start_year = identical(strict_integer(value("start_year"), "start_year", path), ics$simulation_start_year),
      end_year = identical(strict_integer(value("end_year"), "end_year", path), ics$simulation_end_year),
      monte_carlo_runs = identical(strict_integer(value("monte_carlo_runs"), "monte_carlo_runs", path), ics$monte_carlo_runs),
      uncapped_regrowth = identical(strict_integer(value("uncapped_regrowth"), "uncapped_regrowth", path), ics$uncapped_regrowth)
    )
    failed <- names(checks)[!checks]
    if (length(failed)) {
      stopf("Pairing manifest disagrees with config '%s' for: %s.",
            cfg$label, paste(failed, collapse = ", "))
    }
    bypass_status <- value("status")
    bypass_mode <- value("mode")
    reused <- identical(bypass_status, "complete") &&
      identical(bypass_mode, "reuse_BAU_MC_tables")
    patcher_bypassed_value <- tolower(value("patcher_bypassed"))
    if (!patcher_bypassed_value %in% c("true", "t", "1", "false", "f", "0")) {
      stopf("patcher_bypassed is not boolean in %s", path)
    }
    patcher_bypassed <- patcher_bypassed_value %in% c("true", "t", "1")
    patcher_value <- tolower(value("patcher_rng_paired"))
    if (!patcher_value %in% c("true", "t", "1", "false", "f", "0")) {
      stopf("patcher_rng_paired is not boolean in %s", path)
    }
    patcher_paired <- patcher_value %in% c("true", "t", "1")
    design <- pairing_design_status(reused, patcher_bypassed, patcher_paired)
    issue <- if (!reused) {
      paste0("MC bypass status/mode is ", bypass_status, "/", bypass_mode)
    } else {
      ""
    }
    status <- list(
      manifest_path = path,
      manifest_md5 = file_md5(path),
      bypass_status = bypass_status,
      bypass_mode = bypass_mode,
      mc_tables_declared_reused = reused,
      patcher_bypassed = patcher_bypassed,
      patcher_rng_paired = patcher_paired,
      comparison_validated = design$comparison_validated,
      full_stochastic_pairing_validated =
        design$full_stochastic_pairing_validated,
      pairing_design = design$pairing_design,
      independent_patcher_rng_included =
        design$independent_patcher_rng_included,
      issue = issue
    )
  }
  if (!status$comparison_validated) {
    msg <- paste0(
      "Config '", cfg$label, "' lacks validated paired BAU/CCTS input tables: ",
      status$issue, ". Re-run or repair the BAU-table bypass before comparison."
    )
    if (identical(pairing_policy, "strict")) stopf("%s", msg)
    warning(paste0(msg, " Continuing only because pairing-policy=diagnostic."),
            call. = FALSE)
  }
  status$pairing_policy <- pairing_policy
  status$uncertainty_status <- pairing_design_status(
    status$comparison_validated,
    status$patcher_bypassed,
    status$patcher_rng_paired
  )$uncertainty_status
  status
}

read_harvest_total <- function(
  emissions_dir, run_id, period, baseline_code, baseline_source,
  baseline_timing, end_code
) {
  path <- file.path(emissions_dir, "harvest", "per_run_sumco2.csv")
  if (!file.exists(path)) stopf("Missing harvest run table: %s", path)
  tab <- readr::read_csv(path, show_col_types = FALSE, name_repair = "minimal")
  required <- c(
    "run_id", "period_start_year", "period_end_year",
    "baseline_year_code", "end_year_code"
  )
  missing <- setdiff(required, names(tab))
  if (length(missing)) {
    stopf("Harvest table %s lacks explicit period column(s): %s",
          path, paste(missing, collapse = ", "))
  }
  run <- strict_integer(tab$run_id, "run_id", path)
  ps <- strict_integer(tab$period_start_year, "period_start_year", path)
  pe <- strict_integer(tab$period_end_year, "period_end_year", path)
  bc <- strict_integer(tab$baseline_year_code, "baseline_year_code", path)
  bs <- if ("baseline_source" %in% names(tab)) {
    trimws(as.character(tab$baseline_source))
  } else {
    ifelse(bc == 0L, "initial_agb_reference", "Growth_less_harv")
  }
  bt <- if ("baseline_timing" %in% names(tab)) {
    trimws(as.character(tab$baseline_timing))
  } else {
    ifelse(
      bc == 0L,
      "start_of_model_start_year_before_first_step",
      "end_of_previous_year"
    )
  }
  ec <- strict_integer(tab$end_year_code, "end_year_code", path)
  value_field <- if ("agb_avoided_tCO2e" %in% names(tab)) {
    "agb_avoided_tCO2e"
  } else if ("sumco2_Mg" %in% names(tab)) {
    "sumco2_Mg"
  } else {
    stopf(
      "Harvest table %s lacks agb_avoided_tCO2e (or legacy sumco2_Mg).",
      path
    )
  }
  value <- strict_numeric(tab[[value_field]], value_field, path)
  hit <- which(
    run == run_id & ps == period$start & pe == period$end &
      bc == baseline_code & bs == baseline_source & bt == baseline_timing & ec == end_code
  )
  if (length(hit) != 1L) {
    stopf(paste0(
      "Expected exactly one harvest row for run=%d, period=%s, baseline_code=%d, ",
      "baseline_source=%s, baseline_timing=%s, end_code=%d in %s; found %d."
    ), run_id, period$label, baseline_code, baseline_source, baseline_timing,
    end_code, path, length(hit))
  }
  if ("period" %in% names(tab)) {
    saved_period <- trimws(as.character(tab$period[[hit]]))
    if (!identical(saved_period, period$label)) {
      stopf("Harvest row period '%s' does not equal '%s' in %s.",
            saved_period, period$label, path)
    }
  }
  list(
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    md5 = file_md5(path),
    tco2e = value[[hit]]
  )
}

read_enduse_total <- function(emissions_dir, period) {
  path <- file.path(
    emissions_dir, "enduse", sprintf("summary_co2_%s.csv", period$label)
  )
  if (!file.exists(path)) stopf("Missing exact end-use period summary: %s", path)
  tab <- readr::read_csv(path, show_col_types = FALSE, name_repair = "minimal")
  required <- c("fuel", "scenario", "period", "total_tCO2e")
  missing <- setdiff(required, names(tab))
  if (length(missing)) stopf("End-use table %s is missing: %s", path, paste(missing, collapse = ", "))
  fuel <- trimws(ifelse(is.na(tab$fuel), "", as.character(tab$fuel)))
  scenario <- toupper(trimws(ifelse(is.na(tab$scenario), "", as.character(tab$scenario))))
  saved_period <- trimws(ifelse(is.na(tab$period), "", as.character(tab$period)))
  keep <- nzchar(fuel)
  if (!any(keep)) stopf("End-use table has no per-fuel rows: %s", path)
  if (any(saved_period[keep] != period$label)) {
    stopf("End-use table contains a per-fuel row outside period %s: %s", period$label, path)
  }
  if (any(!scenario[keep] %in% c("BAU", "ICS", "DELTA"))) {
    stopf("End-use table contains scenarios other than BAU, ICS, Delta: %s", path)
  }
  value <- strict_numeric(tab$total_tCO2e[keep], "total_tCO2e", path)
  work <- data.frame(
    fuel = fuel[keep], fuel_key = tolower(fuel[keep]), scenario = scenario[keep],
    total_tCO2e = value, stringsAsFactors = FALSE
  )
  fuels <- unique(work$fuel_key)
  checked <- vector("list", length(fuels))
  for (j in seq_along(fuels)) {
    key <- fuels[[j]]
    x <- work[work$fuel_key == key, , drop = FALSE]
    counts <- table(factor(x$scenario, levels = c("BAU", "ICS", "DELTA")))
    if (any(counts != 1L)) {
      stopf("Fuel '%s' must have exactly one BAU, ICS, and Delta row in %s.", x$fuel[[1]], path)
    }
    b <- x$total_tCO2e[x$scenario == "BAU"]
    i <- x$total_tCO2e[x$scenario == "ICS"]
    d <- x$total_tCO2e[x$scenario == "DELTA"]
    residual <- (b - i) - d
    tolerance <- max(0.01, max(abs(c(b, i, d))) * 1e-9)
    ok <- abs(residual) <= tolerance
    if (!ok) {
      stopf("End-use Delta != BAU-ICS for fuel '%s' (residual=%g, tolerance=%g) in %s.",
            x$fuel[[1]], residual, tolerance, path)
    }
    checked[[j]] <- data.frame(
      fuel = x$fuel[[1]], bau_tco2e = b, ics_tco2e = i, delta_tco2e = d,
      residual_tco2e = residual, tolerance_tco2e = tolerance,
      recon_ok = ok, stringsAsFactors = FALSE
    )
  }
  checked <- do.call(rbind, checked)
  total <- sum(checked$delta_tco2e)
  total_residual <- (sum(checked$bau_tco2e) - sum(checked$ics_tco2e)) - total
  total_tolerance <- max(0.01, max(abs(c(
    sum(checked$bau_tco2e), sum(checked$ics_tco2e), total
  ))) * 1e-9)
  if (abs(total_residual) > total_tolerance) {
    stopf("End-use total reconciliation failed in %s.", path)
  }
  list(
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    md5 = file_md5(path),
    tco2e = total,
    per_fuel = checked,
    ignored_blank_fuel_rows = sum(!keep),
    max_abs_residual_tco2e = max(abs(checked$residual_tco2e))
  )
}

read_stage2_run_manifest <- function(
  cfg, period, run_id, timing, initial_agb_md5, pairing
) {
  path <- file.path(cfg$emissions_dir, "run_manifest.csv")
  if (!file.exists(path)) {
    stopf(
      "Stage-2 output is incomplete: run_manifest.csv is absent (written last): %s",
      path
    )
  }
  # Read provenance fields as text. Otherwise readr may interpret the quoted
  # comma-separated selected_run_ids value ("1,2,...,30") as one number with
  # grouping marks and irreversibly lose the run IDs.
  tab <- readr::read_csv(
    path,
    show_col_types = FALSE,
    name_repair = "minimal",
    col_types = readr::cols(.default = readr::col_character())
  )
  if (nrow(tab) != 1L) stopf("Stage-2 run manifest must contain exactly one row: %s", path)
  required <- c(
    "label", "bau_dir", "ics_dir", "emissions_dir", "period_start_year",
    "period_end_year", "baseline_year_code", "end_year_code",
    "selected_run_ids", "enduse_basis", "stage2_script",
    "stage2_script_md5", "status"
  )
  missing <- setdiff(required, names(tab))
  if (length(missing)) {
    stopf("Stage-2 run manifest %s is missing: %s", path, paste(missing, collapse = ", "))
  }
  stage2_status <- trimws(as.character(tab$status[[1]]))
  allowed_status <- if (identical(pairing$pairing_policy, "strict")) {
    "complete"
  } else {
    c("complete", "diagnostic_complete_unverified_bypass_inputs")
  }
  if (!stage2_status %in% allowed_status) {
    stopf("Stage-2 run manifest status '%s' is not allowed: %s", stage2_status, path)
  }
  if (!identical(trimws(as.character(tab$label[[1]])), cfg$label)) {
    stopf("Stage-2 run manifest label does not match '%s': %s", cfg$label, path)
  }

  expected_paths <- c(
    bau_dir = cfg$bau_dir, ics_dir = cfg$ics_dir, emissions_dir = cfg$emissions_dir
  )
  for (field in names(expected_paths)) {
    saved <- resolve_path(as.character(tab[[field]][[1]]), dirname(path), TRUE, "dir")
    if (tolower(saved) != tolower(expected_paths[[field]])) {
      stopf("Stage-2 run manifest '%s' does not match the configured path: %s", field, path)
    }
  }

  ps <- strict_integer(tab$period_start_year[[1]], "period_start_year", path)
  pe <- strict_integer(tab$period_end_year[[1]], "period_end_year", path)
  bc <- strict_integer(tab$baseline_year_code[[1]], "baseline_year_code", path)
  ec <- strict_integer(tab$end_year_code[[1]], "end_year_code", path)
  if (ps != period$start || pe != period$end ||
      bc != timing$baseline_code || ec != timing$end_code) {
    stopf("Stage-2 run manifest period/code metadata does not match this run: %s", path)
  }
  saved_ids <- trimws(unlist(strsplit(
    as.character(tab$selected_run_ids[[1]]), ",", fixed = TRUE
  )))
  saved_ids <- suppressWarnings(as.integer(saved_ids[nzchar(saved_ids)]))
  if (!length(saved_ids) || anyNA(saved_ids) || !run_id %in% saved_ids) {
    stopf("Stage-2 run manifest does not include run ID %d: %s", run_id, path)
  }
  if (tolower(trimws(as.character(tab$enduse_basis[[1]]))) != "demand") {
    stopf("Stage-2 end-use basis is not demand in: %s", path)
  }

  saved_stage2_script <- trimws(as.character(tab$stage2_script[[1]]))
  saved_stage2_md5 <- tolower(trimws(as.character(tab$stage2_script_md5[[1]])))
  if (!grepl("^[a-f0-9]{32}$", saved_stage2_md5)) {
    stopf("Stage-2 script MD5 is absent or malformed in: %s", path)
  }
  current_v5 <- v5_script_path()
  stage2_candidates <- unique(c(
    saved_stage2_script,
    if (!is.na(current_v5)) {
      file.path(dirname(current_v5), basename(saved_stage2_script))
    } else {
      character()
    }
  ))
  stage2_candidates <- stage2_candidates[
    nzchar(stage2_candidates) & file.exists(stage2_candidates)
  ]
  if (!length(stage2_candidates) || !any(
    vapply(stage2_candidates, file_md5, character(1)) == saved_stage2_md5
  )) {
    stopf(
      "No available Stage-2 script matches recorded MD5 %s: %s",
      saved_stage2_md5, path
    )
  }

  optional_equal <- function(field, expected) {
    if (field %in% names(tab)) {
      saved <- trimws(as.character(tab[[field]][[1]]))
      if (!identical(saved, as.character(expected))) {
        stopf("Stage-2 run manifest '%s' does not match '%s': %s", field, expected, path)
      }
    }
  }
  optional_equal("baseline_source", timing$baseline_source)
  optional_equal("baseline_timing", timing$baseline_timing)

  stage2_full_pairing <- FALSE
  if ("full_stochastic_pairing_validated" %in% names(tab)) {
    saved <- tolower(trimws(as.character(
      tab$full_stochastic_pairing_validated[[1]]
    )))
    if (!saved %in% c("true", "t", "1", "false", "f", "0")) {
      stopf("Stage-2 full_stochastic_pairing_validated is not boolean: %s", path)
    }
    stage2_full_pairing <- saved %in% c("true", "t", "1")
  } else if ("paired_mc_inputs_validated" %in% names(tab)) {
    saved <- tolower(trimws(as.character(tab$paired_mc_inputs_validated[[1]])))
    if (!saved %in% c("true", "t", "1", "false", "f", "0")) {
      stopf("Stage-2 paired_mc_inputs_validated is not boolean: %s", path)
    }
    stage2_full_pairing <- (saved %in% c("true", "t", "1")) &&
      pairing$full_stochastic_pairing_validated
  }
  stage2_comparison_validated <- stage2_full_pairing
  comparison_field <- intersect(
    c("comparison_validated", "paired_mc_inputs_validated"), names(tab)
  )
  if (length(comparison_field)) {
    saved <- tolower(trimws(as.character(tab[[comparison_field[[1L]]]][[1L]])))
    if (!saved %in% c("true", "t", "1", "false", "f", "0")) {
      stopf("Stage-2 comparison validation is not boolean: %s", path)
    }
    stage2_comparison_validated <- saved %in% c("true", "t", "1")
  }
  if ("patcher_rng_paired" %in% names(tab)) {
    saved <- tolower(trimws(as.character(tab$patcher_rng_paired[[1]])))
    if (!saved %in% c("true", "t", "1", "false", "f", "0")) {
      stopf("Stage-2 patcher_rng_paired is not boolean: %s", path)
    }
    if ((saved %in% c("true", "t", "1")) != pairing$patcher_rng_paired) {
      stopf("Stage-2 Patcher pairing provenance disagrees with the CCTS bypass manifest: %s", path)
    }
  }
  stage2_pairing_design <- if ("pairing_design" %in% names(tab)) {
    trimws(as.character(tab$pairing_design[[1L]]))
  } else {
    pairing_design_status(
      stage2_comparison_validated,
      pairing$patcher_bypassed,
      pairing$patcher_rng_paired
    )$pairing_design
  }
  if (stage2_comparison_validated &&
      !identical(stage2_pairing_design, pairing$pairing_design)) {
    stopf("Stage-2 pairing design disagrees with the CCTS bypass manifest: %s", path)
  }
  if (stage2_full_pairing != pairing$full_stochastic_pairing_validated) {
    stopf("Stage-2 full-pairing provenance disagrees with the CCTS bypass manifest: %s", path)
  }
  if (identical(pairing$pairing_policy, "strict") &&
      !stage2_comparison_validated) {
    stopf(
      paste0(
        "Stage-2 output does not certify paired MC input tables: %s. ",
        "Re-run stage 2 after validating the BAU tables reused by CCTS."
      ),
      path
    )
  }

  if (timing$full_horizon) {
    full_required <- c(
      "full_horizon", "baseline_source", "baseline_timing", "initial_agb_md5"
    )
    missing_full <- setdiff(full_required, names(tab))
    if (length(missing_full)) {
      stopf(
        "Full-horizon stage-2 run manifest %s is missing: %s",
        path, paste(missing_full, collapse = ", ")
      )
    }
    full_value <- tolower(trimws(as.character(tab$full_horizon[[1]])))
    if (!full_value %in% c("true", "t", "1")) {
      stopf("Stage-2 run manifest does not declare full_horizon=TRUE: %s", path)
    }
    saved_md5 <- tolower(trimws(as.character(tab$initial_agb_md5[[1]])))
    if (!identical(saved_md5, tolower(initial_agb_md5))) {
      stopf("Stage-2 initial AGB MD5 does not match the current reference: %s", path)
    }
  }

  list(
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    md5 = file_md5(path),
    status = stage2_status,
    script_path = normalizePath(
      stage2_candidates[[which(vapply(
        stage2_candidates, file_md5, character(1)
      ) == saved_stage2_md5)[[1L]]]],
      winslash = "/", mustWork = TRUE
    ),
    script_md5 = saved_stage2_md5,
    comparison_validated = stage2_comparison_validated,
    full_stochastic_pairing_validated = stage2_full_pairing,
    pairing_design = stage2_pairing_design,
    uncertainty_status = if ("uncertainty_status" %in% names(tab)) {
      trimws(as.character(tab$uncertainty_status[[1]]))
    } else {
      "legacy_manifest_without_full_pairing_status"
    }
  )
}

preflight_config <- function(cfg, run_id, period, output_dir, pairing_policy) {
  bau_params <- read_parameters(cfg$bau_dir, "BAU")
  ics_params <- read_parameters(cfg$ics_dir, "ICS")
  timing <- validate_parameter_pair(cfg, bau_params, ics_params, run_id, period)
  mc_table_pairing_validated <- validate_mc_pairing(cfg, run_id)
  pairing <- read_pairing_provenance(
    cfg, bau_params, ics_params, pairing_policy
  )

  run_rel <- sprintf("debugging_%d", run_id)
  agb_name <- function(code) sprintf("Growth_less_harv%02d.tif", code)
  ref_bau <- file.path(cfg$bau_dir, "LULCC", "TempRaster", "agb3_c.tif")
  ref_ics <- file.path(cfg$ics_dir, "LULCC", "TempRaster", "agb3_c.tif")
  paths <- list(
    bau_baseline = if (timing$full_horizon) {
      ref_bau
    } else {
      file.path(cfg$bau_dir, run_rel, agb_name(timing$baseline_code))
    },
    ics_baseline = if (timing$full_horizon) {
      ref_ics
    } else {
      file.path(cfg$ics_dir, run_rel, agb_name(timing$baseline_code))
    },
    bau_end = file.path(cfg$bau_dir, run_rel, agb_name(timing$end_code)),
    ics_end = file.path(cfg$ics_dir, run_rel, agb_name(timing$end_code)),
    ref_bau = ref_bau,
    ref_ics = ref_ics
  )
  for (nm in names(paths)) {
    if (!file.exists(paths[[nm]]) || dir.exists(paths[[nm]])) {
      stopf("Config '%s' is missing %s raster: %s", cfg$label, nm, paths[[nm]])
    }
    paths[[nm]] <- normalizePath(paths[[nm]], winslash = "/", mustWork = TRUE)
  }
  ref_bau_md5 <- file_md5(paths$ref_bau)
  ref_ics_md5 <- file_md5(paths$ref_ics)
  if (!identical(ref_bau_md5, ref_ics_md5)) {
    stopf("Config '%s' has different BAU and ICS AGB-2000 reference rasters.", cfg$label)
  }
  stage2_manifest <- read_stage2_run_manifest(
    cfg, period, run_id, timing, ref_bau_md5, pairing
  )
  harvest <- read_harvest_total(
    cfg$emissions_dir, run_id, period, timing$baseline_code,
    timing$baseline_source, timing$baseline_timing, timing$end_code
  )
  enduse <- read_enduse_total(cfg$emissions_dir, period)
  prefix <- sprintf("%s_run%03d_%s", cfg$safe_label, run_id, period$label)
  out_files <- list(
    delta_mg = file.path(output_dir, paste0(prefix, "_period_delta_agb_mg.tif")),
    avoided_mg = file.path(output_dir, paste0(prefix, "_period_avoided_loss_mg.tif")),
    regrowth_mg = file.path(output_dir, paste0(prefix, "_period_regrowth_mg.tif")),
    avoided_tco2e = file.path(output_dir, paste0(prefix, "_period_avoided_loss_tco2e.tif")),
    regrowth_tco2e = file.path(output_dir, paste0(prefix, "_period_regrowth_tco2e.tif"))
  )
  c(cfg, list(
    bau_params = bau_params,
    ics_params = ics_params,
    full_horizon = timing$full_horizon,
    baseline_year = timing$baseline_year,
    baseline_code = timing$baseline_code,
    baseline_source = timing$baseline_source,
    baseline_timing = timing$baseline_timing,
    end_code = timing$end_code,
    regrowth_mode = timing$regrowth_mode,
    raster_paths = paths,
    raster_md5 = vapply(paths, file_md5, character(1)),
    reference_md5 = ref_bau_md5,
    pairing = pairing,
    mc_table_pairing_validated = mc_table_pairing_validated,
    stage2_manifest = stage2_manifest,
    harvest = harvest,
    enduse = enduse,
    output_prefix = prefix,
    out_files = out_files
  ))
}

global_sum0 <- function(r) {
  x <- as.numeric(terra::global(r, "sum", na.rm = TRUE)[1, 1])
  if (!is.finite(x)) 0 else x
}

count_true <- function(r) {
  global_sum0(terra::ifel(r, 1, NA))
}

max_abs0 <- function(r) {
  x <- as.numeric(terra::global(abs(r), "max", na.rm = TRUE)[1, 1])
  if (!is.finite(x)) 0 else x
}

component_stats <- function(r, eps) {
  list(
    net_mg = global_sum0(r),
    positive_mg = global_sum0(terra::ifel(r > eps, r, NA)),
    negative_mg = global_sum0(terra::ifel(r < -eps, r, NA)),
    n_positive = count_true(r > eps),
    n_negative = count_true(r < -eps),
    n_near_zero = count_true(abs(r) <= eps)
  )
}

decompose_state <- function(bau, ics, reference, eps) {
  delta <- ics - bau
  gate <- bau < (reference - eps)
  ics_capped <- terra::ifel(ics > reference, reference, ics)
  avoided <- terra::ifel(gate, ics_capped - bau, 0)
  regrowth <- delta - avoided
  list(
    delta = delta,
    avoided = avoided,
    regrowth = regrowth,
    gate = gate,
    exceedance = ics > (reference + eps)
  )
}

process_config <- function(meta, run_id, period, co2_factor, eps) {
  cat(sprintf("\n[%s] Reading run %d rasters...\n", meta$label, run_id))
  r <- lapply(meta$raster_paths, terra::rast)
  if (any(vapply(r, terra::nlyr, numeric(1)) != 1)) {
    stopf("Config '%s' requires single-layer AGB rasters.", meta$label)
  }
  anchor <- r$bau_baseline
  for (nm in setdiff(names(r), "bau_baseline")) {
    if (!terra::compareGeom(anchor, r[[nm]], stopOnError = FALSE)) {
      stopf("Config '%s' geometry mismatch: bau_baseline vs %s.", meta$label, nm)
    }
  }

  pair_valid <- is.finite(r$bau_baseline) & is.finite(r$ics_baseline) &
    is.finite(r$bau_end) & is.finite(r$ics_end)
  period_valid <- pair_valid & is.finite(r$ref_bau)
  n_pair_common <- count_true(pair_valid)
  n_period_common <- count_true(period_valid)
  if (n_period_common <= 0) stopf("Config '%s' has no common valid period cells.", meta$label)

  mask_with <- function(x, valid) terra::ifel(valid, x, NA)
  b0_pair <- mask_with(r$bau_baseline, pair_valid)
  i0_pair <- mask_with(r$ics_baseline, pair_valid)
  b1_pair <- mask_with(r$bau_end, pair_valid)
  i1_pair <- mask_with(r$ics_end, pair_valid)
  pair_period_delta <- (i1_pair - b1_pair) - (i0_pair - b0_pair)
  pair_period_delta_sum <- global_sum0(pair_period_delta)

  b0 <- mask_with(r$bau_baseline, period_valid)
  i0 <- mask_with(r$ics_baseline, period_valid)
  b1 <- mask_with(r$bau_end, period_valid)
  i1 <- mask_with(r$ics_end, period_valid)
  ref <- mask_with(r$ref_bau, period_valid)
  state0 <- decompose_state(b0, i0, ref, eps)
  state1 <- decompose_state(b1, i1, ref, eps)
  period_delta <- state1$delta - state0$delta
  period_avoided <- state1$avoided - state0$avoided
  period_regrowth <- state1$regrowth - state0$regrowth

  baseline_delta_sum <- global_sum0(state0$delta)
  end_delta_sum <- global_sum0(state1$delta)
  period_delta_sum <- global_sum0(period_delta)
  avoided_stats <- component_stats(period_avoided, eps)
  regrowth_stats <- component_stats(period_regrowth, eps)
  delta_stats <- component_stats(period_delta, eps)

  split_residual_mg <- (avoided_stats$net_mg + regrowth_stats$net_mg) - period_delta_sum
  split_tolerance_mg <- max(1e-3, abs(period_delta_sum) * 1e-10)
  raster_identity_max_mg <- max_abs0((period_avoided + period_regrowth) - period_delta)
  raster_tolerance_mg <- 1e-5
  reference_excluded_delta_mg <- pair_period_delta_sum - period_delta_sum
  reference_excluded_tolerance_mg <- max(1e-3, abs(pair_period_delta_sum) * 1e-10)
  period_state_residual_mg <- (end_delta_sum - baseline_delta_sum) - period_delta_sum
  period_state_tolerance_mg <- max(1e-3, abs(period_delta_sum) * 1e-10)

  calculated_harvest_tco2e <- period_delta_sum * co2_factor
  harvest_residual_tco2e <- calculated_harvest_tco2e - meta$harvest$tco2e
  harvest_tolerance_tco2e <- max(2.0, abs(meta$harvest$tco2e) * 1e-6)

  split_ok <- abs(split_residual_mg) <= split_tolerance_mg
  raster_identity_ok <- raster_identity_max_mg <= raster_tolerance_mg
  reference_coverage_ok <- abs(reference_excluded_delta_mg) <= reference_excluded_tolerance_mg
  period_state_ok <- abs(period_state_residual_mg) <= period_state_tolerance_mg
  harvest_recon_ok <- abs(harvest_residual_tco2e) <= harvest_tolerance_tco2e
  if (!split_ok) stopf("Config '%s' period component sum failed reconciliation.", meta$label)
  if (!raster_identity_ok) stopf("Config '%s' pixel identity failed reconciliation.", meta$label)
  if (!reference_coverage_ok) {
    stopf(paste0(
      "Config '%s' has %g Mg of period delta outside the AGB-2000 reference footprint; ",
      "the harvest total cannot be decomposed completely."
    ), meta$label, reference_excluded_delta_mg)
  }
  if (!period_state_ok) stopf("Config '%s' end-minus-baseline state reconciliation failed.", meta$label)
  if (!harvest_recon_ok) {
    stopf(paste0(
      "Config '%s' period AGB CO2 does not match stage 2 ",
      "(calculated=%g, saved=%g, residual=%g, tolerance=%g tCO2e)."
    ), meta$label, calculated_harvest_tco2e, meta$harvest$tco2e,
    harvest_residual_tco2e, harvest_tolerance_tco2e)
  }

  n_ref_valid <- count_true(is.finite(r$ref_bau))
  row <- data.frame(
    label = meta$label,
    display_label = paste(
      meta$regrowth_mode,
      v5_safe_id(meta$ics_params$scenario_ver),
      sep = "_"
    ),
    safe_label = meta$safe_label,
    country_iso = meta$bau_params$country_iso,
    country_name = meta$bau_params$country_name,
    regrowth_mode = meta$regrowth_mode,
    pairing_policy = meta$pairing$pairing_policy,
    mc_table_rows_paired = meta$mc_table_pairing_validated,
    patcher_bypassed = meta$pairing$patcher_bypassed,
    patcher_rng_paired = meta$pairing$patcher_rng_paired,
    comparison_validated = meta$pairing$comparison_validated,
    full_stochastic_pairing_validated =
      meta$pairing$full_stochastic_pairing_validated,
    pairing_design = meta$pairing$pairing_design,
    independent_patcher_rng_included =
      meta$pairing$independent_patcher_rng_included,
    uncertainty_status = meta$pairing$uncertainty_status,
    run_id = run_id,
    period_start_year = period$start,
    period_end_year = period$end,
    full_horizon = meta$full_horizon,
    baseline_year = meta$baseline_year,
    simulation_start_year = meta$bau_params$simulation_start_year,
    baseline_year_code = meta$baseline_code,
    baseline_source = meta$baseline_source,
    baseline_timing = meta$baseline_timing,
    end_year_code = meta$end_code,
    bau_baseline_agb_mg = global_sum0(b0),
    ics_baseline_agb_mg = global_sum0(i0),
    bau_end_agb_mg = global_sum0(b1),
    ics_end_agb_mg = global_sum0(i1),
    baseline_delta_agb_mg = baseline_delta_sum,
    end_delta_agb_mg = end_delta_sum,
    period_delta_agb_mg = period_delta_sum,
    period_delta_positive_mg = delta_stats$positive_mg,
    period_delta_negative_mg = delta_stats$negative_mg,
    period_avoided_loss_mg = avoided_stats$net_mg,
    period_avoided_loss_positive_mg = avoided_stats$positive_mg,
    period_avoided_loss_negative_mg = avoided_stats$negative_mg,
    period_regrowth_mg = regrowth_stats$net_mg,
    period_regrowth_positive_mg = regrowth_stats$positive_mg,
    period_regrowth_negative_mg = regrowth_stats$negative_mg,
    period_delta_tco2e = period_delta_sum * co2_factor,
    period_avoided_loss_tco2e = avoided_stats$net_mg * co2_factor,
    period_regrowth_tco2e = regrowth_stats$net_mg * co2_factor,
    agb_avoided_stage2_tco2e = meta$harvest$tco2e,
    enduse_avoided_tco2e = meta$enduse$tco2e,
    total_avoided_tco2e = meta$harvest$tco2e + meta$enduse$tco2e,
    n_reference_valid = n_ref_valid,
    n_bau_baseline_valid = count_true(is.finite(r$bau_baseline)),
    n_ics_baseline_valid = count_true(is.finite(r$ics_baseline)),
    n_bau_end_valid = count_true(is.finite(r$bau_end)),
    n_ics_end_valid = count_true(is.finite(r$ics_end)),
    n_pair_period_common = n_pair_common,
    n_decomposition_period_common = n_period_common,
    common_fraction_reference = n_period_common / n_ref_valid,
    n_gated_baseline = count_true(state0$gate),
    n_gated_end = count_true(state1$gate),
    n_ics_exceeds_reference_baseline = count_true(state0$exceedance),
    n_ics_exceeds_reference_end = count_true(state1$exceedance),
    n_period_delta_positive = delta_stats$n_positive,
    n_period_delta_negative = delta_stats$n_negative,
    n_period_delta_near_zero = delta_stats$n_near_zero,
    n_period_avoided_positive = avoided_stats$n_positive,
    n_period_avoided_negative = avoided_stats$n_negative,
    n_period_avoided_near_zero = avoided_stats$n_near_zero,
    n_period_regrowth_positive = regrowth_stats$n_positive,
    n_period_regrowth_negative = regrowth_stats$n_negative,
    n_period_regrowth_near_zero = regrowth_stats$n_near_zero,
    split_residual_mg = split_residual_mg,
    split_tolerance_mg = split_tolerance_mg,
    raster_identity_max_mg = raster_identity_max_mg,
    raster_tolerance_mg = raster_tolerance_mg,
    reference_excluded_delta_mg = reference_excluded_delta_mg,
    reference_excluded_tolerance_mg = reference_excluded_tolerance_mg,
    period_state_residual_mg = period_state_residual_mg,
    harvest_residual_tco2e = harvest_residual_tco2e,
    harvest_tolerance_tco2e = harvest_tolerance_tco2e,
    enduse_max_abs_residual_tco2e = meta$enduse$max_abs_residual_tco2e,
    split_ok = split_ok,
    raster_identity_ok = raster_identity_ok,
    reference_coverage_ok = reference_coverage_ok,
    period_state_ok = period_state_ok,
    harvest_recon_ok = harvest_recon_ok,
    enduse_recon_ok = TRUE,
    all_invariants_ok = TRUE,
    stringsAsFactors = FALSE
  )
  cat(sprintf(
    "[%s] period delta=%0.6f Mg; avoided=%0.6f Mg; regrowth=%0.6f Mg; AGB=%0.6f tCO2e\n",
    meta$label, period_delta_sum, avoided_stats$net_mg, regrowth_stats$net_mg,
    calculated_harvest_tco2e
  ))
  list(
    row = row,
    rasters = list(
      delta_mg = period_delta,
      avoided_mg = period_avoided,
      regrowth_mg = period_regrowth,
      avoided_tco2e = period_avoided * co2_factor,
      regrowth_tco2e = period_regrowth * co2_factor
    ),
    valid_mask = period_valid,
    enduse_by_fuel = transform(meta$enduse$per_fuel, label = meta$label),
    meta = meta
  )
}

make_comparison_table <- function(summary) {
  end_year <- unique(summary$period_end_year)
  baseline_year <- unique(summary$baseline_year)
  period_start <- unique(summary$period_start_year)
  if (length(end_year) != 1L || length(baseline_year) != 1L || length(period_start) != 1L) {
    stopf("Comparison table requires one common period across configurations.")
  }
  metrics <- c(
    stats::setNames("bau_end_agb_mg", sprintf("BAU AGB %d (Mg)", end_year)),
    stats::setNames("ics_end_agb_mg", sprintf("CCTS AGB %d (Mg)", end_year)),
    stats::setNames(
      "baseline_delta_agb_mg",
      sprintf("Baseline CCTS-BAU AGB gap %d (Mg)", baseline_year)
    ),
    stats::setNames("end_delta_agb_mg", sprintf("End CCTS-BAU AGB gap %d (Mg)", end_year)),
    stats::setNames(
      "period_delta_agb_mg",
      sprintf("Period delta AGB %d-%d (Mg)", period_start, end_year)
    ),
    "Period avoided loss (Mg)" = "period_avoided_loss_mg",
    "Period regrowth (Mg)" = "period_regrowth_mg",
    "Period avoided loss (tCO2e)" = "period_avoided_loss_tco2e",
    "Period regrowth (tCO2e)" = "period_regrowth_tco2e",
    "AGB avoided - stage 2 (tCO2e)" = "agb_avoided_stage2_tco2e",
    "End-use avoided (tCO2e)" = "enduse_avoided_tco2e",
    "Total avoided (tCO2e)" = "total_avoided_tco2e",
    "Common decomposition cells" = "n_decomposition_period_common"
  )
  out <- data.frame(Metric = names(metrics), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(summary))) {
    out[[summary$display_label[[i]]]] <- vapply(unname(metrics), function(field) {
      as.numeric(summary[[field]][[i]])
    }, numeric(1))
  }
  out
}

print_comparison_table <- function(comparison) {
  shown <- comparison
  for (column in setdiff(names(shown), "Metric")) {
    shown[[column]] <- round(shown[[column]])
  }
  print(shown, row.names = FALSE)
  invisible(shown)
}

build_provenance <- function(processed, summary, manifest_path, output_dir,
                             period, run_id, co2_factor, eps, command_line,
                             footprint_note) {
  script_path <- v5_script_path()
  script_md5 <- if (!is.na(script_path) && file.exists(script_path)) file_md5(script_path) else NA_character_
  created <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  pkg_versions <- paste0(
    "R=", getRversion(), ";terra=", as.character(utils::packageVersion("terra")),
    ";readr=", as.character(utils::packageVersion("readr"))
  )
  rows <- vector("list", length(processed))
  for (i in seq_along(processed)) {
    p <- processed[[i]]
    m <- p$meta
    s <- summary[i, , drop = FALSE]
    rows[[i]] <- data.frame(
      validation_status = if (m$pairing$full_stochastic_pairing_validated) {
        "PASS_FULLY_PAIRED"
      } else if (m$pairing$comparison_validated) {
        "PASS_PAIRED_MC_INPUTS_INDEPENDENT_PATCHER_RNG"
      } else {
        "DIAGNOSTIC_IDENTITIES_ONLY_UNVERIFIED_BYPASS_INPUTS"
      },
      created_utc = created,
      script_version = "5",
      script_path = script_path,
      script_md5 = script_md5,
      runtime_versions = pkg_versions,
      command_line = command_line,
      manifest_path = manifest_path,
      manifest_md5 = if (file.exists(manifest_path)) file_md5(manifest_path) else NA_character_,
      manifest_row = m$manifest_row,
      output_dir = output_dir,
      label = m$label,
      safe_label = m$safe_label,
      country_iso = m$bau_params$country_iso,
      regrowth_mode = m$regrowth_mode,
      pairing_policy = m$pairing$pairing_policy,
      mc_table_rows_paired = m$mc_table_pairing_validated,
      mc_tables_declared_reused = m$pairing$mc_tables_declared_reused,
      patcher_bypassed = m$pairing$patcher_bypassed,
      patcher_rng_paired = m$pairing$patcher_rng_paired,
      comparison_validated = m$pairing$comparison_validated,
      full_stochastic_pairing_validated =
        m$pairing$full_stochastic_pairing_validated,
      pairing_design = m$pairing$pairing_design,
      independent_patcher_rng_included =
        m$pairing$independent_patcher_rng_included,
      uncertainty_status = m$pairing$uncertainty_status,
      pairing_issue = m$pairing$issue,
      mc_bypass_manifest = m$pairing$manifest_path,
      mc_bypass_manifest_md5 = m$pairing$manifest_md5,
      run_id = run_id,
      period = period$label,
      full_horizon = m$full_horizon,
      baseline_year = m$baseline_year,
      baseline_year_code = m$baseline_code,
      baseline_source = m$baseline_source,
      baseline_timing = m$baseline_timing,
      end_year_code = m$end_code,
      co2_factor = co2_factor,
      classification_epsilon_mg = eps,
      bau_dir = m$bau_dir,
      ics_dir = m$ics_dir,
      emissions_dir = m$emissions_dir,
      bau_scenario_version = m$bau_params$scenario_ver,
      ics_scenario_version = m$ics_params$scenario_ver,
      bau_parameters_path = m$bau_params$path,
      bau_parameters_md5 = m$bau_params$md5,
      ics_parameters_path = m$ics_params$path,
      ics_parameters_md5 = m$ics_params$md5,
      stage2_run_manifest_path = m$stage2_manifest$path,
      stage2_run_manifest_md5 = m$stage2_manifest$md5,
      stage2_script_path = m$stage2_manifest$script_path,
      stage2_script_md5 = m$stage2_manifest$script_md5,
      stage2_status = m$stage2_manifest$status,
      stage2_comparison_validated =
        m$stage2_manifest$comparison_validated,
      stage2_full_stochastic_pairing_validated =
        m$stage2_manifest$full_stochastic_pairing_validated,
      stage2_pairing_design = m$stage2_manifest$pairing_design,
      stage2_uncertainty_status = m$stage2_manifest$uncertainty_status,
      agb_reference_md5 = m$reference_md5,
      bau_baseline_raster = m$raster_paths$bau_baseline,
      bau_baseline_md5 = m$raster_md5[["bau_baseline"]],
      ics_baseline_raster = m$raster_paths$ics_baseline,
      ics_baseline_md5 = m$raster_md5[["ics_baseline"]],
      bau_end_raster = m$raster_paths$bau_end,
      bau_end_md5 = m$raster_md5[["bau_end"]],
      ics_end_raster = m$raster_paths$ics_end,
      ics_end_md5 = m$raster_md5[["ics_end"]],
      reference_raster_bau = m$raster_paths$ref_bau,
      reference_raster_ics = m$raster_paths$ref_ics,
      harvest_csv = m$harvest$path,
      harvest_csv_md5 = m$harvest$md5,
      enduse_csv = m$enduse$path,
      enduse_csv_md5 = m$enduse$md5,
      period_delta_agb_mg = s$period_delta_agb_mg,
      period_avoided_loss_mg = s$period_avoided_loss_mg,
      period_regrowth_mg = s$period_regrowth_mg,
      agb_avoided_stage2_tco2e = s$agb_avoided_stage2_tco2e,
      enduse_avoided_tco2e = s$enduse_avoided_tco2e,
      total_avoided_tco2e = s$total_avoided_tco2e,
      n_decomposition_period_common = s$n_decomposition_period_common,
      common_fraction_reference = s$common_fraction_reference,
      footprint_comparability = footprint_note,
      split_residual_mg = s$split_residual_mg,
      raster_identity_max_mg = s$raster_identity_max_mg,
      reference_excluded_delta_mg = s$reference_excluded_delta_mg,
      harvest_residual_tco2e = s$harvest_residual_tco2e,
      all_invariants_ok = s$all_invariants_ok,
      output_delta_mg = m$out_files$delta_mg,
      output_avoided_mg = m$out_files$avoided_mg,
      output_regrowth_mg = m$out_files$regrowth_mg,
      output_avoided_tco2e = m$out_files$avoided_tco2e,
      output_regrowth_tco2e = m$out_files$regrowth_tco2e,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

write_plot <- function(summary, path) {
  mat <- rbind(
    `Avoided loss` = summary$period_avoided_loss_tco2e,
    Regrowth = summary$period_regrowth_tco2e
  ) / 1e6
  colnames(mat) <- summary$label
  beside <- any(mat < 0)
  positive_extent <- if (beside) {
    max(c(0, mat))
  } else {
    max(c(0, colSums(pmax(mat, 0))))
  }
  negative_extent <- if (beside) {
    min(c(0, mat))
  } else {
    min(c(0, colSums(pmin(mat, 0))))
  }
  value_span <- max(positive_extent - negative_extent, 1e-9)
  plot_limits <- c(
    if (negative_extent < 0) negative_extent - 0.08 * value_span else 0,
    positive_extent + 0.22 * value_span
  )
  grDevices::png(path, width = 1400, height = 900, res = 140)
  op <- graphics::par(mar = c(8, 5.5, 4, 10), xpd = NA)
  on.exit({
    graphics::par(op)
    grDevices::dev.off()
  }, add = TRUE)
  cols <- c(`Avoided loss` = "#E1A100", Regrowth = "#1B9E77")
  pairing_title <- if (all(summary$full_stochastic_pairing_validated)) {
    "fully paired"
  } else if (all(summary$comparison_validated)) {
    "paired MC inputs; independent Patcher spatial RNG"
  } else {
    "DIAGNOSTIC ONLY: bypass inputs unverified"
  }
  bp <- graphics::barplot(
    mat, beside = beside, col = cols, border = NA,
    ylab = expression("Period avoided emissions (Mt CO"[2] * "e)"),
    main = paste0(
      "Period AGB decomposition: signed avoided loss and regrowth\n",
      pairing_title
    ),
    las = 2, cex.names = 0.9, cex.axis = 0.9, ylim = plot_limits
  )
  if (!beside) {
    totals <- colSums(mat)
    graphics::text(
      bp, totals, labels = formatC(totals, format = "f", digits = 2),
      pos = 3, cex = 0.85, font = 2
    )
  }
  graphics::legend(
    "topright", inset = c(0.01, 0.01), fill = cols, legend = names(cols),
    bty = "n", border = NA, title = "Signed component", xpd = FALSE
  )
  invisible(path)
}

make_uncertainty_summary <- function(per_run_summary) {
  metric_fields <- c(
    period_delta_agb_mg = "Mg",
    period_avoided_loss_mg = "Mg",
    period_regrowth_mg = "Mg",
    period_delta_tco2e = "tCO2e",
    period_avoided_loss_tco2e = "tCO2e",
    period_regrowth_tco2e = "tCO2e",
    agb_avoided_stage2_tco2e = "tCO2e",
    enduse_avoided_tco2e = "tCO2e",
    total_avoided_tco2e = "tCO2e"
  )
  groups <- split(per_run_summary, per_run_summary$label)
  rows <- list()
  for (group in groups) {
    for (field in names(metric_fields)) {
      values <- strict_numeric(group[[field]], field, "per-run decomposition")
      q <- if (length(values) >= 2L) {
        as.numeric(stats::quantile(values, c(0.025, 0.5, 0.975), names = FALSE))
      } else {
        c(NA_real_, values[[1L]], NA_real_)
      }
      rows[[length(rows) + 1L]] <- data.frame(
        label = group$label[[1L]],
        display_label = group$display_label[[1L]],
        regrowth_mode = group$regrowth_mode[[1L]],
        analysis = if (all(group$independent_patcher_rng_included)) {
          "MC1_to_n_paired_mc_inputs_independent_patcher_uncertainty"
        } else {
          "MC1_to_n_fully_paired_uncertainty"
        },
        metric = field,
        unit = unname(metric_fields[[field]]),
        runs = length(values),
        run_ids = paste(group$run_id, collapse = ","),
        includes_mc1 = 1L %in% group$run_id,
        uncertainty_estimable = length(values) >= 2L,
        requested_minimum_uncertainty_runs = V5_MIN_UNCERTAINTY_RUNS,
        uncertainty_sample_adequate = length(values) >= V5_MIN_UNCERTAINTY_RUNS,
        mean = mean(values),
        sd = if (length(values) >= 2L) stats::sd(values) else NA_real_,
        se = if (length(values) >= 2L) stats::sd(values) / sqrt(length(values)) else NA_real_,
        empirical_p025 = q[[1L]],
        median = q[[2L]],
        empirical_p975 = q[[3L]],
        min = min(values),
        max = max(values),
        negative_runs = sum(values < 0),
        zero_runs = sum(values == 0),
        positive_runs = sum(values > 0),
        probability_positive = mean(values > 0),
        interval_type = if (length(values) >= 2L) {
          if (all(group$independent_patcher_rng_included)) {
            "empirical_central_95_percent_across_paired_mc_inputs_independent_patcher_runs"
          } else {
            "empirical_central_95_percent_across_fully_paired_runs"
          }
        } else {
          "not_estimable_fewer_than_two_runs"
        },
        comparison_validated = all(group$comparison_validated),
        full_stochastic_pairing_validated = all(
          group$full_stochastic_pairing_validated
        ),
        pairing_design = paste(unique(group$pairing_design), collapse = ","),
        independent_patcher_rng_included = all(
          group$independent_patcher_rng_included
        ),
        cross_configuration_pooling = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

write_uncertainty_plot <- function(summary, path) {
  keep <- summary$metric %in% c(
    "period_avoided_loss_tco2e", "period_regrowth_tco2e"
  )
  x <- summary[keep, , drop = FALSE]
  if (!nrow(x)) return(invisible(NULL))
  configs <- unique(x$display_label)
  metrics <- c("period_avoided_loss_tco2e", "period_regrowth_tco2e")
  value <- lower <- upper <- matrix(
    NA_real_, nrow = length(metrics), ncol = length(configs),
    dimnames = list(c("Avoided loss", "Regrowth"), configs)
  )
  for (j in seq_along(configs)) for (i in seq_along(metrics)) {
    hit <- which(x$display_label == configs[[j]] & x$metric == metrics[[i]])
    if (length(hit) != 1L) stopf("Uncertainty plot input is not unique.")
    value[i, j] <- x$mean[[hit]] / 1e6
    lower[i, j] <- x$empirical_p025[[hit]] / 1e6
    upper[i, j] <- x$empirical_p975[[hit]] / 1e6
  }
  png(path, width = 1900, height = 1150, res = 180)
  on.exit(dev.off(), add = TRUE)
  extent <- range(c(lower, upper, value, 0), na.rm = TRUE)
  pad <- max(diff(extent) * 0.12, 1e-9)
  centers <- barplot(
    value, beside = TRUE, col = c("#E1A100", "#1B9E77"),
    ylim = c(extent[[1L]] - pad, extent[[2L]] + pad),
    ylab = expression("Mean avoided emissions (Mt CO"[2] * "e)"),
    main = "Paired MC1:n AGB decomposition\nempirical 2.5th-97.5th percentiles",
    las = 1
  )
  for (j in seq_len(ncol(value))) for (i in seq_len(nrow(value))) {
    if (is.finite(lower[i, j]) && is.finite(upper[i, j])) {
      # Draw interval stems and caps directly. graphics::arrows() warns when an
      # interval is non-zero numerically but shorter than a device pixel.
      segments(centers[i, j], lower[i, j], centers[i, j], upper[i, j], lwd = 1.5)
      segments(
        centers[i, j] - 0.05, lower[i, j],
        centers[i, j] + 0.05, lower[i, j], lwd = 1.5
      )
      segments(
        centers[i, j] - 0.05, upper[i, j],
        centers[i, j] + 0.05, upper[i, j], lwd = 1.5
      )
    }
  }
  abline(h = 0, col = "#555555", lty = 2)
  legend(
    "topright", fill = c("#E1A100", "#1B9E77"),
    legend = rownames(value), bty = "n"
  )
  invisible(path)
}

write_mc_raster_summaries <- function(
  metas_by_run, run_ids, run_tag, output_dir, overwrite, scalar_summary
) {
  if (length(run_ids) < 2L) return(invisible(character()))
  raster_dir <- file.path(output_dir, "uncertainty_rasters")
  dir.create(raster_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(raster_dir)) stopf("Could not create %s", raster_dir)
  metrics <- c("delta_mg", "avoided_mg", "regrowth_mg", "avoided_tco2e", "regrowth_tco2e")
  scalar_fields <- c(
    delta_mg = "period_delta_agb_mg",
    avoided_mg = "period_avoided_loss_mg",
    regrowth_mg = "period_regrowth_mg",
    avoided_tco2e = "period_avoided_loss_tco2e",
    regrowth_tco2e = "period_regrowth_tco2e"
  )
  written <- character()
  config_count <- length(metas_by_run[[1L]])
  for (config_index in seq_len(config_count)) {
    label <- metas_by_run[[1L]][[config_index]]$safe_label
    for (metric in metrics) {
      paths <- vapply(
        metas_by_run,
        function(run_metas) run_metas[[config_index]]$out_files[[metric]],
        character(1)
      )
      if (any(!file.exists(paths))) stopf("Missing per-run raster before MC summary: %s", paths[!file.exists(paths)][[1L]])
      stack <- terra::rast(paths)
      count <- terra::app(!is.na(stack), sum)
      avg <- terra::ifel(count > 0, terra::app(stack, mean, na.rm = TRUE), NA)
      sdev <- terra::ifel(count > 1, terra::app(stack, stats::sd, na.rm = TRUE), NA)
      stats_rasters <- list(mean = avg, sd = sdev, se = sdev / sqrt(count))
      for (statistic in names(stats_rasters)) {
        path <- file.path(
          raster_dir,
          sprintf("%s_%s_%s_%s.tif", label, metric, run_tag, statistic)
        )
        terra::writeRaster(
          stats_rasters[[statistic]], path, overwrite = overwrite,
          wopt = list(gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=IF_SAFER"))
        )
        written <- c(written, path)
      }
      expected <- scalar_summary[
        scalar_summary$label == metas_by_run[[1L]][[config_index]]$label &
          scalar_summary$metric == scalar_fields[[metric]],
        "mean"
      ]
      observed <- global_sum0(avg)
      tolerance <- max(2.0, abs(expected) * 1e-6)
      if (length(expected) != 1L || abs(observed - expected) > tolerance) {
        stopf("MC mean raster/table reconciliation failed for %s/%s.", label, metric)
      }
    }
  }
  invisible(written)
}

main <- function(args = commandArgs(trailingOnly = TRUE), source_mode = interactive()) {
  opts <- if (isTRUE(source_mode)) {
    list(
      manifest = NULL,
      output_dir = V5_RSTUDIO_OUTPUT_DIR,
      period = V5_RSTUDIO_PERIOD,
      run_ids = V5_RSTUDIO_RUN_IDS,
      run_id = NULL,
      pairing_policy = V5_RSTUDIO_PAIRING_POLICY,
      dry_run = isTRUE(V5_RSTUDIO_DRY_RUN),
      overwrite = isTRUE(V5_RSTUDIO_CLEAN_REBUILD),
      make_plot = isTRUE(V5_RSTUDIO_MAKE_PLOT),
      help = FALSE
    )
  } else {
    parse_cli(args)
  }
  if (opts$help) {
    usage()
    return(invisible(TRUE))
  }
  using_internal <- is.null(opts$manifest) || !nzchar(opts$manifest)
  pairing_policy <- tolower(trimws(as.character(opts$pairing_policy)))
  if (length(pairing_policy) != 1L || is.na(pairing_policy) ||
      !pairing_policy %in% c("strict", "diagnostic")) {
    stopf("--pairing-policy must be 'strict' or 'diagnostic'.")
  }
  period <- parse_period(opts$period)

  required_packages <- c("terra", "readr")
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_packages)) {
    stopf("Missing required R package(s): %s. Install them before running.",
          paste(missing_packages, collapse = ", "))
  }
  suppressPackageStartupMessages({
    library(terra)
    library(readr)
  })

  if (using_internal) {
    pairs <- v5_internal_pairs(SCENARIO_DIRS)
    configs <- configs_from_pairs(pairs)
    internal_path <- v5_script_path()
    manifest_path <- if (is.na(internal_path)) {
      "embedded_SCENARIO_DIRS_in_stage3_v5"
    } else {
      internal_path
    }
    inferred_output <- file.path(unique(pairs$analysis_root), "agb_decomposition")
    configured_counts <- unique(as.integer(pairs$mc_runs))
  } else {
    manifest_path <- resolve_path(opts$manifest, getwd(), TRUE, "file")
    configs <- read_manifest(manifest_path)
    pairs <- NULL
    inferred_output <- NULL
    configured_counts <- unique(vapply(
      configs,
      function(cfg) read_parameters(cfg$bau_dir, "BAU")$monte_carlo_runs,
      integer(1)
    ))
  }
  if (length(configured_counts) != 1L) {
    stopf("Stage 3 requires one common configured MC count; found: %s",
          paste(configured_counts, collapse = ","))
  }
  configured_runs <- configured_counts[[1L]]
  if (!is.null(opts$run_id)) {
    if (!identical(tolower(trimws(opts$run_ids)), "all")) {
      stopf("Use either --run-id or --run-ids, not both.")
    }
    run_ids <- parse_run_ids(opts$run_id, configured_runs)
  } else {
    run_ids <- parse_run_ids(opts$run_ids, configured_runs)
  }

  if (is.null(period)) {
    horizons <- if (using_internal) {
      unique(pairs[, c("analysis_start_year", "model_end_year"), drop = FALSE])
    } else {
      values <- do.call(rbind, lapply(configs, function(cfg) {
        parameters <- read_parameters(cfg$bau_dir, "BAU")
        c(
          parameters$simulation_start_year + V5_SPINUP_YEARS,
          parameters$simulation_end_year
        )
      }))
      unique(data.frame(
        analysis_start_year = values[, 1],
        model_end_year = values[, 2]
      ))
    }
    if (nrow(horizons) != 1L) {
      stopf(
        "Automatic post-spin-up mode requires one common analysis horizon; found: %s. Edit SCENARIO_DIRS or use --period.",
        paste(apply(horizons, 1L, paste, collapse = ":"), collapse = ", ")
      )
    }
    period <- list(
      start = as.integer(horizons$analysis_start_year[[1]]),
      end = as.integer(horizons$model_end_year[[1]]),
      label = sprintf(
        "%d-%d", horizons$analysis_start_year[[1]], horizons$model_end_year[[1]]
      )
    )
  }

  if (is.null(opts$output_dir) || !nzchar(opts$output_dir)) {
    if (!using_internal) {
      usage()
      stopf("--output-dir is required with legacy --manifest.")
    }
    output_dir <- normalizePath(inferred_output, winslash = "/", mustWork = FALSE)
  } else {
    output_dir <- resolve_path(opts$output_dir, getwd(), FALSE, "any")
  }
  if (file.exists(output_dir) && !dir.exists(output_dir)) {
    stopf("Output path exists and is not a directory: %s", output_dir)
  }
  co2_factor <- 0.47 * (44 / 12)
  eps <- 1e-6
  all_configured_runs <- identical(run_ids, seq_len(configured_runs))
  run_tag <- if (all_configured_runs) {
    sprintf("mc1-%d", configured_runs)
  } else {
    paste0("selected_", paste(run_ids, collapse = "-"))
  }
  tag <- paste(run_tag, period$label, sep = "_")

  cat(sprintf(
    paste0(
      "MoFuSS AGB decomposition v5 | configs=%d | runs=%s | period=%s | ",
      "pairing_policy=%s | dry_run=%s\n"
    ),
    length(configs), paste(run_ids, collapse = ","), period$label,
    pairing_policy, opts$dry_run
  ))
  cat(if (using_internal) "Internal config:" else "Manifest:", manifest_path,
      "\nOutput:", output_dir, "\n")

  metas_by_run <- lapply(run_ids, function(run_id) {
    lapply(
      configs, preflight_config,
      run_id = run_id, period = period, output_dir = output_dir,
      pairing_policy = pairing_policy
    )
  })
  names(metas_by_run) <- as.character(run_ids)

  aggregate_files <- list(
    per_run = file.path(output_dir, paste0("agb_decomposition_per_run_", tag, ".csv")),
    deterministic = file.path(output_dir, paste0("deterministic_mc1_summary_", period$label, ".csv")),
    uncertainty = file.path(output_dir, paste0("uncertainty_summary_", tag, ".csv")),
    comparison = file.path(output_dir, paste0("comparison_table_mc1_", period$label, ".csv")),
    provenance = file.path(output_dir, paste0("provenance_", tag, ".csv")),
    enduse_validation = file.path(output_dir, paste0("enduse_validation_", tag, ".csv")),
    plot_mc1 = file.path(output_dir, paste0("agb_decomposition_plot_mc1_", period$label, ".png")),
    plot_uncertainty = file.path(output_dir, paste0("agb_decomposition_plot_", tag, ".png")),
    run_manifest = file.path(output_dir, paste0("run_manifest_", tag, ".csv"))
  )
  planned <- unlist(lapply(
    metas_by_run,
    function(run_metas) unlist(lapply(run_metas, `[[`, "out_files"), use.names = FALSE)
  ), use.names = FALSE)
  planned_aggregates <- unlist(aggregate_files[c(
    "per_run", "uncertainty", "provenance", "enduse_validation", "run_manifest"
  )], use.names = FALSE)
  if (1L %in% run_ids) {
    planned_aggregates <- c(
      planned_aggregates, aggregate_files$deterministic, aggregate_files$comparison
    )
    if (opts$make_plot) planned_aggregates <- c(planned_aggregates, aggregate_files$plot_mc1)
  }
  if (length(run_ids) >= 2L && opts$make_plot) {
    planned_aggregates <- c(planned_aggregates, aggregate_files$plot_uncertainty)
  }
  if (length(run_ids) >= 2L) {
    raster_dir <- file.path(output_dir, "uncertainty_rasters")
    metrics <- c("delta_mg", "avoided_mg", "regrowth_mg", "avoided_tco2e", "regrowth_tco2e")
    for (meta in metas_by_run[[1L]]) for (metric in metrics) for (statistic in c("mean", "sd", "se")) {
      planned_aggregates <- c(
        planned_aggregates,
        file.path(
          raster_dir,
          sprintf("%s_%s_%s_%s.tif", meta$safe_label, metric, run_tag, statistic)
        )
      )
    }
  }
  planned <- c(planned, planned_aggregates)
  collisions <- planned[file.exists(planned)]
  if (length(collisions) && !opts$overwrite) {
    stopf("Refusing to overwrite %d existing output(s); use --overwrite only after review. First: %s",
          length(collisions), collisions[[1L]])
  }

  clean_target <- NULL
  if (opts$overwrite) {
    input_dirs <- unlist(lapply(
      configs, function(cfg) c(cfg$bau_dir, cfg$ics_dir, cfg$emissions_dir)
    ), use.names = FALSE)
    clean_target <- validate_v5_clean_target(
      output_dir, input_dirs,
      if (using_internal) inferred_output else NULL
    )
  }

  if (!opts$dry_run) {
    if (!is.null(clean_target)) clean_v5_output(clean_target)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(output_dir)) stopf("Could not create output directory: %s", output_dir)
    write_test <- file.path(output_dir, sprintf(".write_test_%d.tmp", Sys.getpid()))
    ok <- tryCatch({
      writeLines("ok", write_test)
      unlink(write_test)
      TRUE
    }, error = function(e) FALSE)
    if (!ok) stopf("Output directory is not writable: %s", output_dir)
  }

  wopt <- list(gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=IF_SAFER"))
  summary_rows <- provenance_rows <- enduse_rows <- list()
  mc1_comparison <- NULL
  mc1_summary <- NULL
  all_comparisons_valid <- TRUE
  all_full_pairing <- TRUE
  for (run_index in seq_along(run_ids)) {
    run_id <- run_ids[[run_index]]
    processed <- lapply(
      metas_by_run[[run_index]], process_config,
      run_id = run_id, period = period, co2_factor = co2_factor, eps = eps
    )
    run_summary <- do.call(rbind, lapply(processed, `[[`, "row"))
    if (!all(run_summary$all_invariants_ok)) stopf("An invariant failed for run %d.", run_id)
    all_comparisons_valid <- all_comparisons_valid && all(
      run_summary$comparison_validated
    )
    all_full_pairing <- all_full_pairing && all(
      run_summary$full_stochastic_pairing_validated
    )
    common_counts <- run_summary$n_decomposition_period_common
    footprint_note <- if (length(unique(common_counts)) > 1L) {
      paste0(
        "INDEPENDENT FOOTPRINTS: common-cell counts differ across configurations; ",
        "totals are not directly comparable as per-area effects."
      )
    } else {
      paste0(
        "Common-cell counts are equal, but capped and uncapped remain independent ",
        "simulation units and are not pooled."
      )
    }
    run_summary$footprint_comparability <- footprint_note
    summary_rows[[length(summary_rows) + 1L]] <- run_summary
    run_enduse <- do.call(rbind, lapply(processed, `[[`, "enduse_by_fuel"))
    run_enduse$run_id <- run_id
    enduse_rows[[length(enduse_rows) + 1L]] <- run_enduse
    provenance_rows[[length(provenance_rows) + 1L]] <- build_provenance(
      processed, run_summary, manifest_path, output_dir, period, run_id,
      co2_factor, eps, paste(commandArgs(trailingOnly = TRUE), collapse = " "),
      footprint_note
    )
    if (run_id == 1L) {
      mc1_summary <- run_summary
      mc1_comparison <- make_comparison_table(run_summary)
    }
    if (!opts$dry_run) {
      for (p in processed) for (nm in names(p$rasters)) {
        terra::writeRaster(
          p$rasters[[nm]], p$meta$out_files[[nm]], overwrite = opts$overwrite,
          wopt = wopt
        )
      }
    }
    rm(processed)
    gc(FALSE)
  }

  per_run_summary <- do.call(rbind, summary_rows)
  provenance <- do.call(rbind, provenance_rows)
  enduse_validation <- do.call(rbind, enduse_rows)
  uncertainty <- make_uncertainty_summary(per_run_summary)
  deterministic <- if (1L %in% run_ids) {
    out <- make_uncertainty_summary(per_run_summary[per_run_summary$run_id == 1L, , drop = FALSE])
    out$analysis <- if (all(
      per_run_summary$patcher_bypassed[per_run_summary$run_id == 1L]
    )) {
      "MC1_deterministic_nominal_parameters"
    } else {
      "MC1_nominal_parameters_with_patcher_spatial_rng"
    }
    out
  } else {
    NULL
  }

  if (opts$dry_run) {
    cat("\nDry-run validated every selected run, calculation identity, and output collision; wrote nothing.\n")
    if (!is.null(mc1_comparison)) print_comparison_table(mc1_comparison)
    print(uncertainty[
      uncertainty$metric %in% c(
        "period_avoided_loss_tco2e", "period_regrowth_tco2e",
        "agb_avoided_stage2_tco2e", "enduse_avoided_tco2e",
        "total_avoided_tco2e"
      ),
      c("display_label", "metric", "runs", "mean", "sd", "empirical_p025", "empirical_p975")
    ], row.names = FALSE)
    if (all_full_pairing) {
      cat("[DRY-RUN PASS] Full pairing and all run/configuration invariants passed.\n")
    } else if (all_comparisons_valid) {
      cat(paste0(
        "[DRY-RUN PASS] Paired MC inputs and all invariants passed; ",
        "independent Patcher RNG is included as spatial-allocation uncertainty.\n"
      ))
    } else {
      cat("[DRY-RUN DIAGNOSTIC ONLY] Calculation identities passed but pairing is uncertified.\n")
    }
    return(invisible(list(
      per_run = per_run_summary, deterministic = deterministic,
      uncertainty = uncertainty, provenance = provenance
    )))
  }

  readr::write_csv(per_run_summary, aggregate_files$per_run)
  if (!is.null(deterministic)) {
    readr::write_csv(deterministic, aggregate_files$deterministic)
    readr::write_csv(mc1_comparison, aggregate_files$comparison)
  }
  readr::write_csv(uncertainty, aggregate_files$uncertainty)
  readr::write_csv(provenance, aggregate_files$provenance)
  readr::write_csv(enduse_validation, aggregate_files$enduse_validation)
  write_mc_raster_summaries(
    metas_by_run, run_ids, run_tag, output_dir, opts$overwrite, uncertainty
  )
  if (opts$make_plot && !is.null(mc1_summary)) {
    write_plot(mc1_summary, aggregate_files$plot_mc1)
  }
  if (opts$make_plot && length(run_ids) >= 2L) {
    write_uncertainty_plot(uncertainty, aggregate_files$plot_uncertainty)
  }
  script_path <- v5_script_path()
  run_manifest <- data.frame(
    script_version = 5L,
    script_path = script_path,
    script_md5 = if (!is.na(script_path) && file.exists(script_path)) file_md5(script_path) else NA_character_,
    analysis_products = if (any(!per_run_summary$patcher_bypassed)) {
      if (any(per_run_summary$independent_patcher_rng_included)) {
        "nominal_MC1_and_paired_MC_inputs_independent_Patcher_uncertainty"
      } else {
        "nominal_MC1_and_fully_paired_Patcher_RNG_uncertainty"
      }
    } else if (1L %in% run_ids && length(run_ids) >= 2L) {
      "MC1_deterministic_and_fully_paired_MC1_to_n_uncertainty"
    } else if (1L %in% run_ids) {
      "MC1_deterministic_only_patcher_bypassed"
    } else {
      "selected_run_diagnostic_without_MC1"
    },
    configured_mc_runs = configured_runs,
    selected_run_ids = paste(run_ids, collapse = ","),
    selected_run_count = length(run_ids),
    all_configured_runs_included = all_configured_runs,
    nominal_run_id = if (1L %in% run_ids) 1L else NA_integer_,
    deterministic_run_id = if (1L %in% run_ids &&
      all(per_run_summary$patcher_bypassed)) 1L else NA_integer_,
    requested_minimum_uncertainty_runs = V5_MIN_UNCERTAINTY_RUNS,
    uncertainty_sample_adequate = length(run_ids) >= V5_MIN_UNCERTAINTY_RUNS,
    uncertainty_interval = if (length(run_ids) >= 2L) {
      if (any(per_run_summary$independent_patcher_rng_included)) {
        "empirical_central_95_percent_across_paired_mc_inputs_independent_patcher_runs"
      } else {
        "empirical_central_95_percent_across_fully_paired_runs"
      }
    } else {
      "not_estimable_fewer_than_two_runs"
    },
    period_start_year = period$start,
    period_end_year = period$end,
    pairing_policy = pairing_policy,
    comparison_validated = all_comparisons_valid,
    full_stochastic_pairing_validated = all_full_pairing,
    pairing_design = paste(unique(per_run_summary$pairing_design), collapse = ","),
    independent_patcher_rng_included = any(
      per_run_summary$independent_patcher_rng_included
    ),
    cross_configuration_pooling = FALSE,
    capped_uncapped_relationship = "independent_simulation_units",
    output_dir = output_dir,
    per_run_summary = aggregate_files$per_run,
    deterministic_summary = if (!is.null(deterministic)) aggregate_files$deterministic else NA_character_,
    uncertainty_summary = aggregate_files$uncertainty,
    stage2_script_md5 = paste(unique(provenance$stage2_script_md5), collapse = ","),
    completed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    status = if (all_comparisons_valid) {
      "complete"
    } else {
      "diagnostic_complete_unverified_pairing"
    },
    stringsAsFactors = FALSE
  )
  # Written last: its presence certifies that every planned batch product above
  # completed successfully.
  readr::write_csv(run_manifest, aggregate_files$run_manifest)

  if (all_full_pairing) {
    cat("\n[SUCCESS] Full pairing and all selected runs/configurations passed; outputs written to:",
        output_dir, "\n")
  } else if (all_comparisons_valid) {
    cat(paste0(
      "\n[SUCCESS] Paired MC inputs and all selected runs/configurations passed; ",
      "independent Patcher RNG is included in uncertainty; outputs written to: "
    ), output_dir, "\n")
  } else {
    cat("\n[DIAGNOSTIC OUTPUT ONLY] Calculations passed but pairing is uncertified; outputs written to:",
        output_dir, "\n")
  }
  if (!is.null(mc1_comparison)) print_comparison_table(mc1_comparison)
  invisible(list(
    per_run = per_run_summary, deterministic = deterministic,
    uncertainty = uncertainty, provenance = provenance,
    run_manifest = run_manifest
  ))
}

v5_config_only <- isTRUE(get0(
  "MOFUSS_CONFIG_ONLY", envir = .GlobalEnv, inherits = FALSE, ifnotfound = FALSE
))
if (!v5_config_only && (sys.nframe() == 0L || interactive())) {
  main(source_mode = interactive())
}
