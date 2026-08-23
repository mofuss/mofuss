# Copyright 2025 Stockholm Environment Institute
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0
#
# MoFuSS post-processing: period AGB decomposition
# Version 5
# Date: August 2026
#
# This program is deliberately non-interactive. It decomposes the change in the
# BAU-vs-ICS AGB difference over an accounting period into signed avoided-loss
# and regrowth components. The default period starts after the first ten modeled
# spin-up years. It evaluates the decomposition state at end-(START-1) and at
# END, then subtracts the former from the latter.
#
# Default: infer BAU/CCTS pairs, 2010-post-spin-up period, MC01 and outputs.
# Add --overwrite only after a dry run succeeds. Add --no-plot to omit the PNG.
# Patcher is bypassed in this workflow, so its RNG stream is unused; pairing is
# established from the BAU tables reused by CCTS.

stopf <- function(fmt, ...) {
  stop(sprintf(fmt, ...), call. = FALSE)
}

V5_SPINUP_YEARS <- 10L

# EDIT ONLY THIS BLOCK when changing country/region scenario folders.
# Folder order does not define pairing; parameters.csv does.
SCENARIO_DIRS <- c(
  "D:/ken_1km_bau1_2030_v3_ng",
  "D:/ken_1km_bau1_2030_v3_g",
  "D:/ken_1km_ics3_2030_v3_ng",
  "D:/ken_1km_ics3_2030_v3_g"
)

usage <- function() {
  cat(paste0(
    "Usage:\n",
    "  Rscript 3post_agb_decomposition_v5.R ",
    "[--output-dir=DIR] [--period=auto|START:END] [--run-id=1] [--dry-run] ",
    "[--pairing-policy=strict|diagnostic] [--overwrite] [--no-plot]\n\n",
    "Default input: SCENARIO_DIRS near the top of this script.\n",
    "Pairings, post-spin-up period, stage-2 inputs and output directory are inferred.\n",
    "Legacy --manifest=CONFIGS.csv remains supported with --output-dir.\n",
    "Strict bypass-table validation is the default. Diagnostic mode keeps signed values visible ",
    "but does not certify them as a paired BAU/CCTS effect.\n",
    "Dry-run reads and validates every input and performs all calculations, ",
    "but writes nothing.\n"
  ))
}

parse_cli <- function(args) {
  out <- list(
    manifest = NULL,
    output_dir = NULL,
    period = "auto",
    run_id = "1",
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
      mode,
      sep = "_"
    )
    analysis_id <- paste(scope_id, analysis_start_year, a$end_year, sep = "_")
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
  # These exported biological tables define the paired biological draw.
  # Patcher is bypassed in these simulations, so no Patcher RNG is consumed.
  rel <- file.path("Temp", c("k_all.csv", "rmax_all.csv", "i_st_all.csv"))
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
        "Reuse the same biological MC draws/seeds; do not pair independent run IDs."
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
      patcher_rng_paired = FALSE, full_stochastic_pairing_validated = FALSE,
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
      "patcher_rng_paired"
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
    patcher_value <- tolower(value("patcher_rng_paired"))
    if (!patcher_value %in% c("true", "t", "1", "false", "f", "0")) {
      stopf("patcher_rng_paired is not boolean in %s", path)
    }
    patcher_paired <- patcher_value %in% c("true", "t", "1")
    full <- reused
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
      patcher_rng_paired = patcher_paired,
      full_stochastic_pairing_validated = full,
      issue = issue
    )
  }
  if (!status$full_stochastic_pairing_validated) {
    msg <- paste0(
      "Config '", cfg$label, "' is not a fully paired BAU/CCTS experiment: ",
      status$issue, ". Re-run or repair the BAU-table bypass before comparison."
    )
    if (identical(pairing_policy, "strict")) stopf("%s", msg)
    warning(paste0(msg, " Continuing only because pairing-policy=diagnostic."),
            call. = FALSE)
  }
  status$pairing_policy <- pairing_policy
  status$uncertainty_status <- if (status$full_stochastic_pairing_validated) {
    "paired_bypass_inputs_validated_patcher_skipped"
  } else {
    "DIAGNOSTIC_ONLY_unverified_bypass_inputs"
  }
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
    "baseline_year_code", "end_year_code", "sumco2_Mg"
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
  value <- strict_numeric(tab$sumco2_Mg, "sumco2_Mg", path)
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
    "selected_run_ids", "enduse_basis", "status"
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
  if ("patcher_rng_paired" %in% names(tab)) {
    saved <- tolower(trimws(as.character(tab$patcher_rng_paired[[1]])))
    if (!saved %in% c("true", "t", "1", "false", "f", "0")) {
      stopf("Stage-2 patcher_rng_paired is not boolean: %s", path)
    }
    if ((saved %in% c("true", "t", "1")) != pairing$patcher_rng_paired) {
      stopf("Stage-2 Patcher pairing provenance disagrees with the CCTS bypass manifest: %s", path)
    }
  }
  if (identical(pairing$pairing_policy, "strict") && !stage2_full_pairing) {
    stopf(
      paste0(
        "Stage-2 output does not certify paired bypass inputs: %s. ",
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
    full_stochastic_pairing_validated = stage2_full_pairing,
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
    patcher_rng_paired = meta$pairing$patcher_rng_paired,
    full_stochastic_pairing_validated =
      meta$pairing$full_stochastic_pairing_validated,
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
      patcher_rng_paired = m$pairing$patcher_rng_paired,
      full_stochastic_pairing_validated =
        m$pairing$full_stochastic_pairing_validated,
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
      stage2_status = m$stage2_manifest$status,
      stage2_full_stochastic_pairing_validated =
        m$stage2_manifest$full_stochastic_pairing_validated,
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

main <- function() {
  opts <- parse_cli(commandArgs(trailingOnly = TRUE))
  if (opts$help) {
    usage()
    return(invisible(TRUE))
  }
  using_internal <- is.null(opts$manifest) || !nzchar(opts$manifest)
  run_id <- strict_integer(opts$run_id, "--run-id", "command line")
  if (length(run_id) != 1L || run_id < 1L) stopf("--run-id must be a positive integer.")
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
  } else {
    manifest_path <- resolve_path(opts$manifest, getwd(), TRUE, "file")
    configs <- read_manifest(manifest_path)
    pairs <- NULL
    inferred_output <- NULL
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

  cat(sprintf(
    paste0(
      "MoFuSS AGB decomposition v5 | configs=%d | run=%d | period=%s | ",
      "pairing_policy=%s | dry_run=%s\n"
    ),
    length(configs), run_id, period$label, pairing_policy, opts$dry_run
  ))
  cat(if (using_internal) "Internal config:" else "Manifest:", manifest_path,
      "\nOutput:", output_dir, "\n")

  metas <- lapply(configs, preflight_config,
                  run_id = run_id, period = period, output_dir = output_dir,
                  pairing_policy = pairing_policy)
  tag <- sprintf("run%03d_%s", run_id, period$label)
  aggregate_files <- list(
    summary = file.path(output_dir, paste0("agb_decomposition_summary_", tag, ".csv")),
    comparison = file.path(output_dir, paste0("comparison_table_", tag, ".csv")),
    provenance = file.path(output_dir, paste0("provenance_", tag, ".csv")),
    enduse_validation = file.path(output_dir, paste0("enduse_validation_", tag, ".csv")),
    plot = file.path(output_dir, paste0("agb_decomposition_plot_", tag, ".png"))
  )
  planned <- unlist(lapply(metas, function(x) x$out_files), use.names = FALSE)
  planned <- c(planned, unlist(aggregate_files[c(
    "summary", "comparison", "provenance", "enduse_validation"
  )], use.names = FALSE))
  if (opts$make_plot) planned <- c(planned, aggregate_files$plot)
  collisions <- planned[file.exists(planned)]
  if (length(collisions) && !opts$overwrite) {
    stopf("Refusing to overwrite %d existing output(s); use --overwrite only after review. First: %s",
          length(collisions), collisions[[1]])
  }

  processed <- lapply(
    metas, process_config, run_id = run_id, period = period,
    co2_factor = co2_factor, eps = eps
  )
  summary <- do.call(rbind, lapply(processed, `[[`, "row"))
  if (!all(summary$all_invariants_ok)) stopf("An invariant failed; no outputs were written.")
  full_pairing <- all(summary$full_stochastic_pairing_validated)

  common_counts <- summary$n_decomposition_period_common
  footprint_note <- if (length(unique(common_counts)) > 1L) {
    paste0(
      "INDEPENDENT FOOTPRINTS: common-cell counts differ across configurations; ",
      "totals are not directly comparable as per-area effects."
    )
  } else {
    paste0(
      "Common-cell counts are equal, but configurations still use independent masks; ",
      "confirm spatial mask equivalence before per-area comparison."
    )
  }
  if (length(unique(common_counts)) > 1L) warning(footprint_note, call. = FALSE)
  summary$footprint_comparability <- footprint_note
  comparison <- make_comparison_table(summary)
  enduse_validation <- do.call(rbind, lapply(processed, `[[`, "enduse_by_fuel"))
  provenance <- build_provenance(
    processed, summary, manifest_path, output_dir, period, run_id,
    co2_factor, eps, paste(commandArgs(trailingOnly = TRUE), collapse = " "),
    footprint_note
  )

  if (opts$dry_run) {
    cat("\nDry-run validated all inputs, calculations, and output collisions; wrote nothing.\n")
    print_comparison_table(comparison)
    if (full_pairing) {
      cat("[DRY-RUN PASS] Full pairing and all calculation/provenance invariants passed.\n")
    } else {
      cat(paste0(
        "[DRY-RUN DIAGNOSTIC ONLY] Calculation identities passed, but the ",
        "BAU/CCTS effect is not identified because bypass inputs were unverified.\n"
      ))
    }
    return(invisible(list(summary = summary, provenance = provenance)))
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) stopf("Could not create output directory: %s", output_dir)
  write_test <- file.path(output_dir, sprintf(".write_test_%d.tmp", Sys.getpid()))
  ok <- tryCatch({
    writeLines("ok", write_test)
    unlink(write_test)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) stopf("Output directory is not writable: %s", output_dir)

  wopt <- list(gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=IF_SAFER"))
  for (p in processed) {
    for (nm in names(p$rasters)) {
      terra::writeRaster(
        p$rasters[[nm]], p$meta$out_files[[nm]], overwrite = opts$overwrite,
        wopt = wopt
      )
    }
  }
  readr::write_csv(summary, aggregate_files$summary)
  readr::write_csv(comparison, aggregate_files$comparison)
  readr::write_csv(provenance, aggregate_files$provenance)
  readr::write_csv(enduse_validation, aggregate_files$enduse_validation)
  if (opts$make_plot) write_plot(summary, aggregate_files$plot)

  if (full_pairing) {
    cat("\n[SUCCESS] Full pairing and all configurations passed; outputs written to:",
        output_dir, "\n")
  } else {
    cat(paste0(
      "\n[DIAGNOSTIC OUTPUT ONLY] Calculation identities passed, but these are ",
      "not certified paired BAU/CCTS effects; outputs written to: ", output_dir, "\n"
    ))
  }
  print_comparison_table(comparison)
  invisible(list(summary = summary, provenance = provenance))
}

if (sys.nframe() == 0L || interactive()) {
  main()
}
