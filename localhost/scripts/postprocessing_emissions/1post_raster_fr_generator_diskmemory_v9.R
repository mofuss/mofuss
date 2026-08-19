# Copyright 2026 Stockholm Environment Institute
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0
#
# MoFuSS postprocessing: MC raster summaries (version 9)

SCRIPT_VERSION <- "9"
DEFAULT_OUTPUT_SUBDIR <- file.path("Out", "webmofuss_results_v9")

# EDIT ONLY THIS BLOCK when changing the country/region analysis.
# Each entry is one completed MoFuSS scenario folder. Metadata, full horizon,
# available runs and output locations are read/inferred by the script.
SCENARIO_DIRS <- c(
  "D:/ken_1km_bau1_2030_v3_ng",
  "D:/ken_1km_bau1_2030_v3_g",
  "D:/ken_1km_ics3_2030_v3_ng",
  "D:/ken_1km_ics3_2030_v3_g"
)

usage <- function() {
  paste(
    "Usage:",
    "  Rscript 1post_raster_fr_generator_diskmemory_v9.R",
    "    [--scenario-dir=PATH ...] (default: internal SCENARIO_DIRS)",
    "    [--period=START:END ...] (default: v3 STdyn windows after spin-up)",
    "    [--output-subdir=Out/webmofuss_results_v9] [--dry-run] [--overwrite]",
    "",
    "Edit SCENARIO_DIRS near the top for a new country/region.",
    "",
    "Period semantics:",
    "  Default periods reproduce v3's STdyn-dependent output schedule exactly.",
    "  Supported STdyn values: 20, 30, 35, 40, 50.",
    "  The first 10 modeled years are spin-up; no output period may start earlier.",
    "  v3 window: baseline=Growth in START. Harvest years=START..END inclusive.",
    "  Explicit --period windows retain v7 semantics: baseline=end of START-1.",
    "  NRB = max(baseline AGB - Growth_less_harv[end], 0).",
    "  AGB snapshots use correctly dated post-harvest model rasters.",
    sep = "\n"
  )
}

stopf <- function(fmt, ...) {
  stop(sprintf(fmt, ...), call. = FALSE)
}

normalize_existing_path <- function(path, label) {
  if (!nzchar(path) || !dir.exists(path)) {
    stopf("%s does not exist or is not a directory: %s", label, path)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

validate_output_subdir <- function(path) {
  if (!nzchar(path)) stop("--output-subdir cannot be empty.", call. = FALSE)
  slash_path <- gsub("\\\\", "/", path)
  if (grepl("^[A-Za-z]:", slash_path) || startsWith(slash_path, "/")) {
    stop("--output-subdir must be relative to each scenario directory.", call. = FALSE)
  }
  parts <- strsplit(slash_path, "/", fixed = TRUE)[[1]]
  if (any(parts %in% c("", ".", ".."))) {
    stop("--output-subdir cannot contain empty, '.' or '..' path components.", call. = FALSE)
  }
  do.call(file.path, as.list(parts))
}

parse_cli <- function(args) {
  config <- list(
    scenario_dirs = character(),
    periods = character(),
    output_subdir = DEFAULT_OUTPUT_SUBDIR,
    dry_run = FALSE,
    overwrite = FALSE,
    help = FALSE
  )
  output_seen <- FALSE

  for (arg in args) {
    if (arg %in% c("--help", "-h")) {
      config$help <- TRUE
    } else if (startsWith(arg, "--scenario-dir=")) {
      config$scenario_dirs <- c(config$scenario_dirs, sub("^--scenario-dir=", "", arg))
    } else if (startsWith(arg, "--period=")) {
      config$periods <- c(config$periods, sub("^--period=", "", arg))
    } else if (startsWith(arg, "--output-subdir=")) {
      if (output_seen) stop("--output-subdir may be supplied only once.", call. = FALSE)
      config$output_subdir <- sub("^--output-subdir=", "", arg)
      output_seen <- TRUE
    } else if (identical(arg, "--dry-run")) {
      config$dry_run <- TRUE
    } else if (identical(arg, "--overwrite")) {
      config$overwrite <- TRUE
    } else {
      stopf("Unknown argument: %s\n\n%s", arg, usage())
    }
  }

  if (!config$help) {
    config$output_subdir <- validate_output_subdir(config$output_subdir)
  }
  config
}

parse_periods <- function(specs) {
  parse_one <- function(spec) {
    match <- regexec("^([0-9]{4}):([0-9]{4})$", trimws(spec), perl = TRUE)
    fields <- regmatches(trimws(spec), match)[[1]]
    if (length(fields) != 3L) {
      stopf("Invalid period '%s'; expected START:END, for example 2000:2030.", spec)
    }
    c(start = as.integer(fields[2]), end = as.integer(fields[3]))
  }
  parsed <- do.call(rbind, lapply(specs, parse_one))
  parsed <- unique(as.data.frame(parsed, stringsAsFactors = FALSE))
  rownames(parsed) <- NULL
  if (any(parsed$start > parsed$end)) stop("Every period must have START <= END.", call. = FALSE)
  parsed
}

# Reproduce the period and AGB-snapshot schedule in the original v3 script,
# expressed as offsets from the model start so it is not hard-coded to 2000.
# The first ten modeled years are spin-up and deliberately produce no outputs.
v3_stdyn_schedule <- function(model_start_year, model_end_year) {
  stdyn <- as.integer(model_end_year - model_start_year)
  schedules <- list(
    "20" = list(
      period_offsets = matrix(c(10L, 20L), ncol = 2L, byrow = TRUE),
      snapshot_offsets = c(10L, 20L)
    ),
    "30" = list(
      period_offsets = matrix(c(10L, 20L, 20L, 30L), ncol = 2L, byrow = TRUE),
      snapshot_offsets = c(10L, 20L, 30L)
    ),
    "35" = list(
      period_offsets = matrix(c(10L, 20L, 20L, 35L), ncol = 2L, byrow = TRUE),
      snapshot_offsets = c(10L, 20L, 35L)
    ),
    "40" = list(
      period_offsets = matrix(
        c(10L, 20L, 20L, 30L, 20L, 35L, 20L, 40L, 30L, 40L),
        ncol = 2L,
        byrow = TRUE
      ),
      snapshot_offsets = c(10L, 20L, 30L, 35L, 40L)
    ),
    "50" = list(
      period_offsets = matrix(
        c(10L, 20L, 20L, 30L, 20L, 35L, 20L, 50L, 30L, 40L, 40L, 50L),
        ncol = 2L,
        byrow = TRUE
      ),
      snapshot_offsets = c(10L, 20L, 30L, 35L, 40L, 50L)
    )
  )
  key <- as.character(stdyn)
  if (!key %in% names(schedules)) {
    stopf(
      paste0(
        "Unsupported STdyn=%d (end_year - start_year) for the default v3 schedule. ",
        "Supported values are 20, 30, 35, 40 and 50. ",
        "Specify explicit --period=START:END values rather than guessing a schedule."
      ),
      stdyn
    )
  }
  selected <- schedules[[key]]
  diagnostic_periods <- data.frame(
    start = model_start_year + selected$period_offsets[, 1L],
    end = model_start_year + selected$period_offsets[, 2L],
    period_role = "v3_stdyn_window",
    stringsAsFactors = FALSE
  )
  snapshot_years <- model_start_year + selected$snapshot_offsets
  list(
    stdyn = stdyn,
    periods = diagnostic_periods,
    snapshot_years = as.integer(snapshot_years)
  )
}

read_delimited_table <- function(path) {
  first_line <- readLines(path, n = 1L, warn = FALSE)
  if (length(first_line) != 1L) stopf("Cannot read a header from: %s", path)
  comma_fields <- length(strsplit(first_line, ",", fixed = TRUE)[[1]])
  semicolon_fields <- length(strsplit(first_line, ";", fixed = TRUE)[[1]])
  separator <- if (semicolon_fields > comma_fields) ";" else ","
  utils::read.table(
    path,
    header = TRUE,
    sep = separator,
    quote = "\"",
    comment.char = "",
    fill = TRUE,
    check.names = TRUE,
    stringsAsFactors = FALSE
  )
}

read_model_metadata <- function(scenario_dir) {
  country_path <- file.path(scenario_dir, "LULCC", "TempTables", "Country.csv")
  if (!file.exists(country_path)) stopf("Missing Country.csv: %s", country_path)
  country_table <- read_delimited_table(country_path)
  key_column <- intersect(c("Key.", "Key"), names(country_table))
  if (length(key_column) != 1L || !"Country" %in% names(country_table)) {
    stopf("Country.csv must contain one Key/Key. column and a Country column: %s", country_path)
  }
  country_rows <- which(trimws(as.character(country_table[[key_column]])) == "1")
  if (length(country_rows) != 1L) {
    stopf("Country.csv must have exactly one row with Key == 1: %s", country_path)
  }
  country_name <- trimws(as.character(country_table$Country[country_rows]))
  if (!nzchar(country_name) || grepl("[/\\\\]", country_name)) {
    stopf("Invalid Country value in: %s", country_path)
  }

  parameter_dir <- file.path(
    scenario_dir, "LULCC", "DownloadedDatasets", paste0("SourceData", country_name)
  )
  if (!dir.exists(parameter_dir)) stopf("Missing parameter directory: %s", parameter_dir)
  parameter_files <- list.files(
    parameter_dir,
    pattern = "^parameters.*\\.csv$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  )
  if (length(parameter_files) != 1L) {
    stopf(
      "Expected exactly one parameters*.csv in %s; found %d.",
      parameter_dir, length(parameter_files)
    )
  }
  parameter_path <- normalizePath(parameter_files, winslash = "/", mustWork = TRUE)
  parameter_table <- read_delimited_table(parameter_path)
  if (!all(c("Var", "ParCHR") %in% names(parameter_table))) {
    stopf("Parameter file must contain Var and ParCHR columns: %s", parameter_path)
  }

  integer_parameter <- function(key) {
    rows <- which(trimws(as.character(parameter_table$Var)) == key)
    if (length(rows) != 1L) stopf("Parameter '%s' must occur exactly once in %s", key, parameter_path)
    raw <- trimws(as.character(parameter_table$ParCHR[rows]))
    value <- suppressWarnings(as.integer(raw))
    if (length(value) != 1L || is.na(value) || !grepl("^[0-9]+$", raw)) {
      stopf("Parameter '%s' must be a positive integer in %s; got '%s'.", key, parameter_path, raw)
    }
    value
  }
  character_parameter <- function(key) {
    rows <- which(trimws(as.character(parameter_table$Var)) == key)
    if (length(rows) != 1L) stopf("Parameter '%s' must occur exactly once in %s", key, parameter_path)
    value <- trimws(as.character(parameter_table$ParCHR[rows]))
    if (length(value) != 1L || is.na(value) || !nzchar(value)) {
      stopf("Parameter '%s' must be nonblank in %s.", key, parameter_path)
    }
    value
  }

  start_year <- integer_parameter("start_year")
  end_year <- integer_parameter("end_year")
  mc_runs <- integer_parameter("monte_carlo_runs")
  if (end_year < start_year) stopf("end_year precedes start_year in %s", parameter_path)
  if (mc_runs < 1L) stopf("monte_carlo_runs must be positive in %s", parameter_path)
  initial_agb <- file.path(scenario_dir, "LULCC", "TempRaster", "agb3_c.tif")
  if (!file.exists(initial_agb) || dir.exists(initial_agb)) {
    stopf("Missing initial AGB reference raster: %s", initial_agb)
  }
  initial_agb <- normalizePath(initial_agb, winslash = "/", mustWork = TRUE)

  list(
    source_name = country_name,
    scenario_ver = character_parameter("scenario_ver"),
    byregion = character_parameter("byregion"),
    continent = character_parameter("region2BprocessedCont"),
    region = character_parameter("region2BprocessedReg"),
    country = character_parameter("region2BprocessedCtry"),
    country_iso = toupper(character_parameter("region2BprocessedCtry_iso")),
    subcountry = character_parameter("subcountry"),
    gee_scale = integer_parameter("GEE_scale"),
    epsg_pcs = integer_parameter("epsg_pcs"),
    parameter_file = parameter_path,
    start_year = start_year,
    end_year = end_year,
    mc_runs = mc_runs,
    initial_agb = initial_agb,
    initial_agb_md5 = unname(as.character(tools::md5sum(initial_agb))),
    expected_codes = seq_len(end_year - start_year + 1L)
  )
}

discover_family <- function(run_dir, pattern, expected_codes, label) {
  candidates <- list.files(run_dir, full.names = TRUE, recursive = FALSE)
  matches <- regexec(pattern, basename(candidates), perl = TRUE)
  fields <- regmatches(basename(candidates), matches)
  keep <- lengths(fields) == 2L
  files <- candidates[keep]
  codes <- as.integer(vapply(fields[keep], `[[`, character(1), 2L))
  if (anyDuplicated(codes)) stopf("Duplicate numeric %s codes in %s", label, run_dir)
  order_index <- order(codes)
  codes <- codes[order_index]
  files <- files[order_index]
  if (!identical(codes, as.integer(expected_codes))) {
    missing <- setdiff(expected_codes, codes)
    extra <- setdiff(codes, expected_codes)
    stopf(
      "%s code sequence is invalid in %s. Missing: [%s]. Extra: [%s].",
      label,
      run_dir,
      paste(missing, collapse = ","),
      paste(extra, collapse = ",")
    )
  }
  stats <- file.info(files)
  if (any(is.na(stats$size) | stats$size <= 0)) stopf("Empty or unreadable %s raster in %s", label, run_dir)
  setNames(normalizePath(files, winslash = "/", mustWork = TRUE), as.character(codes))
}

discover_runs <- function(scenario_dir, metadata) {
  directories <- list.dirs(scenario_dir, full.names = TRUE, recursive = FALSE)
  matches <- regexec("^debugging_([0-9]+)$", basename(directories), perl = TRUE)
  fields <- regmatches(basename(directories), matches)
  keep <- lengths(fields) == 2L
  run_dirs <- directories[keep]
  run_ids <- as.integer(vapply(fields[keep], `[[`, character(1), 2L))
  if (anyDuplicated(run_ids)) stopf("Duplicate debugging run IDs in %s", scenario_dir)
  order_index <- order(run_ids)
  run_ids <- run_ids[order_index]
  run_dirs <- run_dirs[order_index]
  expected_ids <- seq_len(metadata$mc_runs)

  unexpected_ids <- setdiff(run_ids, expected_ids)
  if (length(unexpected_ids)) {
    stopf(
      "Run IDs in %s exceed configured range 1:%d. Extra: [%s].",
      scenario_dir,
      metadata$mc_runs,
      paste(unexpected_ids, collapse = ",")
    )
  }

  missing_ids <- setdiff(expected_ids, run_ids)
  if (length(missing_ids)) {
    stopf(
      "Configured run directories are missing in %s: [%s].",
      scenario_dir,
      paste(missing_ids, collapse = ",")
    )
  }
  expected_index <- match(expected_ids, run_ids)
  run_ids <- run_ids[expected_index]
  run_dirs <- run_dirs[expected_index]

  lapply(seq_along(run_ids), function(index) {
    run_dir <- normalizePath(run_dirs[index], winslash = "/", mustWork = TRUE)
    list(
      run_id = run_ids[index],
      run_dir = run_dir,
      files = list(
        growth = discover_family(
          run_dir, "^Growth([0-9]+)\\.tif$", metadata$expected_codes, "Growth"
        ),
        post_harvest = discover_family(
          run_dir,
          "^Growth_less_harv([0-9]+)\\.tif$",
          metadata$expected_codes,
          "Growth_less_harv"
        ),
        harvest_total = discover_family(
          run_dir,
          "^Harvest_tot([0-9]+)\\.tif$",
          metadata$expected_codes,
          "Harvest_tot"
        )
      )
    )
  })
}

year_to_code <- function(year, model_start_year) {
  as.integer(year - model_start_year + 1L)
}

period_suffix <- function(start_year, end_year) {
  sprintf("%02d_%02d", start_year %% 100L, end_year %% 100L)
}

manifest_row <- function(
  scenario_name,
  scenario_dir,
  parameter_file,
  scenario_ver,
  byregion,
  continent,
  region,
  country,
  country_iso,
  subcountry,
  gee_scale,
  epsg_pcs,
  model_start_year,
  model_end_year,
  stdyn,
  configured_mc_runs,
  processed_mc_runs,
  processed_run_ids,
  initial_agb,
  initial_agb_md5,
  terra_version,
  record_type,
  run_id = NA_integer_,
  period_start = NA_integer_,
  period_end = NA_integer_,
  period_role = NA_character_,
  baseline_source = NA_character_,
  baseline_timing = NA_character_,
  role = NA_character_,
  metric = NA_character_,
  statistic = NA_character_,
  calendar_year = NA_integer_,
  raster_code = NA_integer_,
  source_family = NA_character_,
  path = NA_character_,
  definition = NA_character_
) {
  data.frame(
    script_version = SCRIPT_VERSION,
    generated_utc = NA_character_,
    terra_version = terra_version,
    scenario_name = scenario_name,
    scenario_dir = scenario_dir,
    parameter_file = parameter_file,
    scenario_ver = scenario_ver,
    byregion = byregion,
    continent = continent,
    region = region,
    country = country,
    country_iso = country_iso,
    subcountry = subcountry,
    gee_scale = gee_scale,
    epsg_pcs = epsg_pcs,
    model_start_year = model_start_year,
    model_end_year = model_end_year,
    stdyn = stdyn,
    configured_mc_runs = configured_mc_runs,
    processed_mc_runs = processed_mc_runs,
    processed_run_ids = processed_run_ids,
    initial_agb = initial_agb,
    initial_agb_md5 = initial_agb_md5,
    record_type = record_type,
    run_id = run_id,
    period_start = period_start,
    period_end = period_end,
    period_role = period_role,
    baseline_source = baseline_source,
    baseline_timing = baseline_timing,
    role = role,
    metric = metric,
    statistic = statistic,
    calendar_year = calendar_year,
    raster_code = raster_code,
    source_family = source_family,
    path = path,
    definition = definition,
    stringsAsFactors = FALSE
  )
}

validate_geometry <- function(paths, scenario_name) {
  paths <- unique(paths)
  if (!length(paths)) stopf("No planned raster inputs for %s", scenario_name)
  reference_path <- paths[1]
  reference <- terra::rast(reference_path)
  for (path in paths[-1]) {
    candidate <- terra::rast(path)
    compatible <- suppressWarnings(terra::compareGeom(
      reference,
      candidate,
      stopOnError = FALSE,
      crs = TRUE,
      ext = TRUE,
      rowcol = TRUE,
      res = TRUE
    ))
    if (!isTRUE(compatible)) {
      stopf("Raster geometry mismatch in %s:\n  reference: %s\n  candidate: %s", scenario_name, reference_path, path)
    }
  }
  invisible(TRUE)
}

build_plan <- function(scenario_dir, periods = NULL, output_subdir) {
  scenario_dir <- normalize_existing_path(scenario_dir, "Scenario directory")
  scenario_name <- basename(scenario_dir)
  metadata <- read_model_metadata(scenario_dir)
  stdyn <- as.integer(metadata$end_year - metadata$start_year)

  if (is.null(periods)) {
    schedule <- v3_stdyn_schedule(metadata$start_year, metadata$end_year)
    periods <- schedule$periods
    scheduled_snapshot_years <- schedule$snapshot_years
  } else {
    if (!"period_role" %in% names(periods)) periods$period_role <- "explicit_window"
    scheduled_snapshot_years <- NULL
  }

  analysis_start_year <- metadata$start_year + 10L
  if (any(periods$start < analysis_start_year)) {
    invalid <- periods[periods$start < analysis_start_year, , drop = FALSE]
    stopf(
      paste0(
        "Period START precedes the post-spin-up analysis start year=%d ",
        "(model start_year + 10). Invalid: %s"
      ),
      analysis_start_year,
      paste(sprintf("%d:%d", invalid$start, invalid$end), collapse = ", ")
    )
  }
  if (any(periods$end > metadata$end_year)) {
    invalid <- periods[periods$end > metadata$end_year, , drop = FALSE]
    stopf(
      "Period END exceeds model end_year=%d. Invalid: %s",
      metadata$end_year,
      paste(sprintf("%d:%d", invalid$start, invalid$end), collapse = ", ")
    )
  }

  periods$uses_initial_baseline <- FALSE
  periods$v3_stdyn_window <- periods$period_role == "v3_stdyn_window"
  periods$baseline_year <- ifelse(
    periods$uses_initial_baseline,
    metadata$start_year,
    ifelse(periods$v3_stdyn_window, periods$start, periods$start - 1L)
  )
  periods$baseline_code <- ifelse(
    periods$uses_initial_baseline,
    0L,
    year_to_code(periods$baseline_year, metadata$start_year)
  )
  periods$baseline_source <- ifelse(
    periods$uses_initial_baseline, "initial_agb_reference", "Growth"
  )
  periods$baseline_timing <- ifelse(
    periods$uses_initial_baseline,
    "start_of_model_start_year_before_first_step",
    ifelse(
      periods$v3_stdyn_window,
      "within_start_year_after_growth_before_harvest",
      "end_of_previous_year"
    )
  )
  periods$start_code <- year_to_code(periods$start, metadata$start_year)
  periods$end_code <- year_to_code(periods$end, metadata$start_year)
  periods$suffix <- mapply(period_suffix, periods$start, periods$end, USE.NAMES = FALSE)
  if (anyDuplicated(periods$suffix)) {
    stopf("Duplicate output period suffixes were inferred for %s.", scenario_name)
  }

  runs <- discover_runs(scenario_dir, metadata)
  processed_ids <- vapply(runs, `[[`, integer(1), "run_id")
  output_dir <- normalizePath(
    file.path(scenario_dir, output_subdir), winslash = "/", mustWork = FALSE
  )
  terra_version <- as.character(utils::packageVersion("terra"))
  rows <- list()
  add_row <- function(...) {
    rows[[length(rows) + 1L]] <<- manifest_row(
      scenario_name = scenario_name,
      scenario_dir = scenario_dir,
      parameter_file = metadata$parameter_file,
      scenario_ver = metadata$scenario_ver,
      byregion = metadata$byregion,
      continent = metadata$continent,
      region = metadata$region,
      country = metadata$country,
      country_iso = metadata$country_iso,
      subcountry = metadata$subcountry,
      gee_scale = metadata$gee_scale,
      epsg_pcs = metadata$epsg_pcs,
      model_start_year = metadata$start_year,
      model_end_year = metadata$end_year,
      stdyn = stdyn,
      configured_mc_runs = metadata$mc_runs,
      processed_mc_runs = length(processed_ids),
      processed_run_ids = paste(processed_ids, collapse = ","),
      initial_agb = metadata$initial_agb,
      initial_agb_md5 = metadata$initial_agb_md5,
      terra_version = terra_version,
      ...
    )
  }

  for (period_index in seq_len(nrow(periods))) {
    period <- periods[period_index, , drop = FALSE]
    nrb_definition <- if (period$uses_initial_baseline) {
      sprintf(
        "max(initial AGB at start-of-%d - Growth_less_harv[%d], 0); baseline code 0",
        period$baseline_year, period$end
      )
    } else if (period$v3_stdyn_window) {
      sprintf(
        paste0(
          "max(Growth[%d] - Growth_less_harv[%d], 0); ",
          "v3 STdyn window with corrected calendar-year raster codes"
        ),
        period$baseline_year, period$end
      )
    } else {
      sprintf(
        "max(Growth[%d] - Growth_less_harv[%d], 0); baseline=START-1",
        period$baseline_year, period$end
      )
    }
    harvest_definition <- sprintf(
      "sum(Harvest_tot[%d:%d]); inclusive calendar years",
      period$start, period$end
    )

    for (run in runs) {
      add_row(
        record_type = "input",
        run_id = run$run_id,
        period_start = period$start,
        period_end = period$end,
        period_role = period$period_role,
        baseline_source = period$baseline_source,
        baseline_timing = period$baseline_timing,
        role = "nrb_baseline",
        metric = "nrb",
        calendar_year = period$baseline_year,
        raster_code = period$baseline_code,
        source_family = if (period$uses_initial_baseline) "agb3_c" else "Growth",
        path = if (period$uses_initial_baseline) {
          metadata$initial_agb
        } else {
          unname(run$files$growth[as.character(period$baseline_code)])
        },
        definition = nrb_definition
      )
      add_row(
        record_type = "input",
        run_id = run$run_id,
        period_start = period$start,
        period_end = period$end,
        period_role = period$period_role,
        baseline_source = period$baseline_source,
        baseline_timing = period$baseline_timing,
        role = "nrb_end",
        metric = "nrb",
        calendar_year = period$end,
        raster_code = period$end_code,
        source_family = "Growth_less_harv",
        path = unname(run$files$post_harvest[as.character(period$end_code)]),
        definition = nrb_definition
      )
      for (year in seq.int(period$start, period$end)) {
        code <- year_to_code(year, metadata$start_year)
        add_row(
          record_type = "input",
          run_id = run$run_id,
          period_start = period$start,
          period_end = period$end,
          period_role = period$period_role,
          baseline_source = period$baseline_source,
          baseline_timing = period$baseline_timing,
          role = "harvest_year",
          metric = "harvest",
          calendar_year = year,
          raster_code = code,
          source_family = "Harvest_tot",
          path = unname(run$files$harvest_total[as.character(code)]),
          definition = harvest_definition
        )
      }
    }

    for (metric in c("nrb", "harv")) {
      definition <- if (metric == "nrb") nrb_definition else harvest_definition
      for (statistic in c("mean", "sd", "se")) {
        add_row(
          record_type = "output",
          period_start = period$start,
          period_end = period$end,
          period_role = period$period_role,
          baseline_source = period$baseline_source,
          baseline_timing = period$baseline_timing,
          role = "mc_summary",
          metric = metric,
          statistic = statistic,
          path = file.path(output_dir, sprintf("%s_%s_%s.tif", metric, period$suffix, statistic)),
          definition = paste0(definition, "; cellwise non-NA count used for MC statistics")
        )
      }
    }
  }

  if (is.null(scheduled_snapshot_years)) {
    scheduled_snapshot_years <- sort(unique(c(
      periods$baseline_year[!periods$uses_initial_baseline],
      periods$end
    )))
  }
  modeled_snapshots <- data.frame(
    output_tag = as.character(scheduled_snapshot_years),
    calendar_year = as.integer(scheduled_snapshot_years),
    raster_code = year_to_code(scheduled_snapshot_years, metadata$start_year),
    source_family = "Growth_less_harv",
    stringsAsFactors = FALSE
  )
  initial_snapshot <- if (any(periods$uses_initial_baseline)) {
    data.frame(
      output_tag = paste0("initial_", metadata$start_year),
      calendar_year = metadata$start_year,
      raster_code = 0L,
      source_family = "agb3_c",
      stringsAsFactors = FALSE
    )
  } else {
    modeled_snapshots[FALSE, , drop = FALSE]
  }
  agb_snapshots <- unique(rbind(initial_snapshot, modeled_snapshots))
  agb_snapshots <- agb_snapshots[order(
    agb_snapshots$calendar_year, agb_snapshots$raster_code
  ), , drop = FALSE]
  rownames(agb_snapshots) <- NULL
  for (snapshot_index in seq_len(nrow(agb_snapshots))) {
    snapshot <- agb_snapshots[snapshot_index, , drop = FALSE]
    for (run in runs) {
      is_initial <- snapshot$source_family == "agb3_c"
      add_row(
        record_type = "input",
        run_id = run$run_id,
        period_role = "agb_snapshot",
        baseline_source = if (is_initial) "initial_agb_reference" else NA_character_,
        baseline_timing = if (is_initial) {
          "start_of_model_start_year_before_first_step"
        } else {
          "end_of_calendar_year_after_harvest"
        },
        role = "agb_snapshot",
        metric = "agb",
        calendar_year = snapshot$calendar_year,
        raster_code = snapshot$raster_code,
        source_family = snapshot$source_family,
        path = if (is_initial) {
          metadata$initial_agb
        } else {
          unname(run$files$post_harvest[as.character(snapshot$raster_code)])
        },
        definition = if (is_initial) {
          "common initial standing AGB before the first model step"
        } else {
          "post-harvest standing AGB: Growth_less_harv[calendar year]"
        }
      )
    }
    for (statistic in c("mean", "sd", "se")) {
      add_row(
        record_type = "output",
        period_role = "agb_snapshot",
        role = "mc_summary",
        metric = "agb",
        statistic = statistic,
        calendar_year = snapshot$calendar_year,
        raster_code = snapshot$raster_code,
        source_family = snapshot$source_family,
        path = file.path(
          output_dir, sprintf("agb_%s_%s.tif", snapshot$output_tag, statistic)
        ),
        definition = if (snapshot$source_family == "agb3_c") {
          "common initial standing AGB before first model step; repeated across MC layers"
        } else {
          "post-harvest standing AGB; cellwise non-NA count used for MC statistics"
        }
      )
    }
  }

  manifest_path <- file.path(output_dir, "stage1_v9_provenance_manifest.csv")
  add_row(
    record_type = "manifest",
    role = "provenance",
    metric = "manifest",
    path = manifest_path,
    definition = "Exact inputs, outputs, parameters, formulas, versions and run IDs for this execution"
  )
  records <- do.call(rbind, rows)
  validate_geometry(records$path[records$record_type == "input"], scenario_name)

  list(
    scenario_name = scenario_name,
    scenario_dir = scenario_dir,
    metadata = metadata,
    periods = periods,
    agb_snapshots = agb_snapshots,
    runs = runs,
    output_dir = output_dir,
    manifest_path = manifest_path,
    records = records
  )
}

print_plan <- function(plan) {
  cat("\nDRY RUN - validated plan\n")
  cat("Scenario:       ", plan$scenario_name, "\n", sep = "")
  cat(
    "Metadata:       ", plan$metadata$country, " (", plan$metadata$country_iso,
    "); ", plan$metadata$scenario_ver, "; ", plan$metadata$byregion, "\n", sep = ""
  )
  cat("Scenario path:  ", plan$scenario_dir, "\n", sep = "")
  cat("Parameter file: ", plan$metadata$parameter_file, "\n", sep = "")
  cat(
    "Model:           ", plan$metadata$start_year, "-", plan$metadata$end_year,
    "; STdyn=", plan$metadata$end_year - plan$metadata$start_year,
    "; configured MC runs=", plan$metadata$mc_runs, "\n", sep = ""
  )
  cat(
    "Periods:         ",
    paste(
      sprintf("%d:%d [%s]", plan$periods$start, plan$periods$end, plan$periods$period_role),
      collapse = ", "
    ),
    "\n",
    sep = ""
  )
  processed_ids <- vapply(plan$runs, `[[`, integer(1), "run_id")
  cat(
    "MC runs:         ", paste(processed_ids, collapse = ","),
    " (n=", length(processed_ids), ")\n", sep = ""
  )
  cat("Output path:    ", plan$output_dir, "\n", sep = "")
  cat("Planned inputs (exact paths; read-only):\n")
  input_columns <- c(
    "run_id", "period_start", "period_end", "period_role", "baseline_source",
    "baseline_timing", "role", "calendar_year", "raster_code",
    "source_family", "path"
  )
  utils::write.table(
    plan$records[plan$records$record_type == "input", input_columns, drop = FALSE],
    file = "",
    sep = "\t",
    row.names = FALSE,
    quote = FALSE,
    na = ""
  )
  cat("Planned outputs (not written in dry-run):\n")
  output_columns <- c(
    "record_type", "period_start", "period_end", "period_role", "metric", "statistic",
    "calendar_year", "path"
  )
  utils::write.table(
    plan$records[plan$records$record_type != "input", output_columns, drop = FALSE],
    file = "",
    sep = "\t",
    row.names = FALSE,
    quote = FALSE,
    na = ""
  )
  invisible(plan)
}

paths_for_code <- function(plan, family, code) {
  vapply(
    plan$runs,
    function(run) unname(run$files[[family]][as.character(code)]),
    character(1)
  )
}

summarize_mc <- function(rasters) {
  if (!inherits(rasters, "SpatRaster") || terra::nlyr(rasters) < 1L) {
    stop("summarize_mc requires a nonempty SpatRaster.", call. = FALSE)
  }
  count <- terra::app(!is.na(rasters), sum)
  # Use the stable two-pass implementations in mean()/sd(). The algebraic
  # sum-of-squares formula loses precision for large, nearly equal AGB values.
  mean_raster <- terra::app(rasters, mean, na.rm = TRUE)
  mean_raster <- terra::ifel(count > 0, mean_raster, NA)
  sd_raster <- terra::app(rasters, sd, na.rm = TRUE)
  sd_raster <- terra::ifel(count > 1, sd_raster, NA)
  se_raster <- sd_raster / sqrt(count)
  list(mean = mean_raster, sd = sd_raster, se = se_raster, n = count)
}

write_stat_triplet <- function(stats, prefix, overwrite) {
  for (statistic in c("mean", "sd", "se")) {
    output_path <- paste0(prefix, "_", statistic, ".tif")
    terra::writeRaster(
      stats[[statistic]],
      output_path,
      overwrite = overwrite,
      wopt = list(
        datatype = "FLT4S",
        gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3", "BIGTIFF=IF_SAFER")
      )
    )
  }
  invisible(TRUE)
}

execute_plan <- function(plan, overwrite = FALSE) {
  output_paths <- unique(plan$records$path[plan$records$record_type != "input"])
  existing <- output_paths[file.exists(output_paths)]
  if (length(existing) && !overwrite) {
    stopf(
      "Refusing to overwrite %d existing planned output(s) for %s. First existing path: %s",
      length(existing), plan$scenario_name, existing[1]
    )
  }
  if (file.exists(plan$output_dir) && !dir.exists(plan$output_dir)) {
    stopf("Output path exists but is not a directory: %s", plan$output_dir)
  }
  if (!dir.exists(plan$output_dir) && !dir.create(plan$output_dir, recursive = TRUE)) {
    stopf("Could not create output directory: %s", plan$output_dir)
  }

  processed_ids <- vapply(plan$runs, `[[`, integer(1), "run_id")
  processed_count <- length(processed_ids)
  layer_names <- sprintf("run_%03d", processed_ids)
  message(
    "Processing ", plan$scenario_name, " (MC runs: ",
    paste(processed_ids, collapse = ","), "; n=", processed_count, ")"
  )
  for (period_index in seq_len(nrow(plan$periods))) {
    period <- plan$periods[period_index, , drop = FALSE]
    message("  Period ", period$start, ":", period$end)

    baseline_growth <- if (period$uses_initial_baseline) {
      terra::rast(rep(plan$metadata$initial_agb, processed_count))
    } else {
      terra::rast(paths_for_code(plan, "growth", period$baseline_code))
    }
    end_post_harvest <- terra::rast(paths_for_code(plan, "post_harvest", period$end_code))
    names(baseline_growth) <- names(end_post_harvest) <- layer_names
    nrb <- baseline_growth - end_post_harvest
    nrb <- terra::ifel(nrb < 0, 0, nrb)
    nrb_stats <- summarize_mc(nrb)
    write_stat_triplet(
      nrb_stats,
      file.path(plan$output_dir, paste0("nrb_", period$suffix)),
      overwrite
    )

    harvest_codes <- seq.int(period$start_code, period$end_code)
    per_run_harvest <- lapply(plan$runs, function(run) {
      annual_paths <- unname(run$files$harvest_total[as.character(harvest_codes)])
      terra::app(terra::rast(annual_paths), sum, na.rm = FALSE)
    })
    harvest <- do.call(c, per_run_harvest)
    names(harvest) <- layer_names
    harvest_stats <- summarize_mc(harvest)
    write_stat_triplet(
      harvest_stats,
      file.path(plan$output_dir, paste0("harv_", period$suffix)),
      overwrite
    )
  }

  for (snapshot_index in seq_len(nrow(plan$agb_snapshots))) {
    snapshot <- plan$agb_snapshots[snapshot_index, , drop = FALSE]
    message("  AGB snapshot: ", snapshot$output_tag)
    agb <- if (snapshot$source_family == "agb3_c") {
      terra::rast(rep(plan$metadata$initial_agb, processed_count))
    } else {
      terra::rast(paths_for_code(plan, "post_harvest", snapshot$raster_code))
    }
    names(agb) <- layer_names
    agb_stats <- summarize_mc(agb)
    write_stat_triplet(
      agb_stats,
      file.path(plan$output_dir, paste0("agb_", snapshot$output_tag)),
      overwrite
    )
  }

  manifest <- plan$records
  manifest$generated_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  utils::write.csv(manifest, plan$manifest_path, row.names = FALSE, na = "")
  message("Completed: ", plan$output_dir)
  invisible(plan)
}

run_stage1 <- function(
  scenario_dirs,
  period_specs = character(),
  output_subdir = DEFAULT_OUTPUT_SUBDIR,
  dry_run = FALSE,
  overwrite = FALSE
) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Required package 'terra' is not installed. No packages were installed.", call. = FALSE)
  }
  output_subdir <- validate_output_subdir(output_subdir)
  periods <- if (length(period_specs)) parse_periods(period_specs) else NULL
  normalized_scenarios <- vapply(
    scenario_dirs,
    normalize_existing_path,
    character(1),
    label = "Scenario directory"
  )
  if (anyDuplicated(tolower(normalized_scenarios))) {
    stop("Duplicate --scenario-dir values are not allowed.", call. = FALSE)
  }

  # Validate every scenario, input sequence, raster geometry and output collision
  # before creating or writing anything.
  plans <- lapply(
    normalized_scenarios,
    build_plan,
    periods = periods,
    output_subdir = output_subdir
  )
  for (plan in plans) {
    planned_outputs <- unique(plan$records$path[plan$records$record_type != "input"])
    existing <- planned_outputs[file.exists(planned_outputs)]
    if (length(existing) && !overwrite && !dry_run) {
      stopf(
        "Refusing to overwrite %d existing planned output(s) for %s. First existing path: %s",
        length(existing), plan$scenario_name, existing[1]
      )
    }
  }

  if (dry_run) {
    lapply(plans, print_plan)
    cat("\nDRY RUN COMPLETE: validation passed; no files or directories were written.\n")
    return(invisible(plans))
  }

  lapply(plans, execute_plan, overwrite = overwrite)
  invisible(plans)
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  config <- parse_cli(args)
  if (config$help) {
    cat(usage(), "\n")
    return(invisible(NULL))
  }
  scenario_dirs <- if (length(config$scenario_dirs)) config$scenario_dirs else SCENARIO_DIRS
  if (!length(scenario_dirs)) stop("SCENARIO_DIRS is empty.", call. = FALSE)
  run_stage1(
    scenario_dirs = scenario_dirs,
    period_specs = config$periods,
    output_subdir = config$output_subdir,
    dry_run = config$dry_run,
    overwrite = config$overwrite
  )
}

config_only <- isTRUE(get0(
  "MOFUSS_CONFIG_ONLY", envir = environment(), inherits = FALSE, ifnotfound = FALSE
))
if (!config_only && (sys.nframe() == 0L || interactive())) {
  if (interactive()) {
    # RStudio Source: run with the internal configuration and no CLI arguments.
    main(args = character())
  } else {
    tryCatch(
      main(),
      error = function(error) {
        message("ERROR: ", conditionMessage(error))
        quit(save = "no", status = 1L, runLast = FALSE)
      }
    )
  }
}
