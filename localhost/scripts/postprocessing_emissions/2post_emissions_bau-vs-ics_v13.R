# Copyright 2025 Stockholm Environment Institute
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0
#
# MoFuSS post-processing: avoided emissions, version 13
# Date: August 2026
#
# This script compares BAU/CCTS scenario pairs inferred from the SCENARIO_DIRS
# vector below. parameters.csv supplies the spatial scope, scenario metadata,
# model horizon, MC count, pairing keys, and output IDs.
# It is deliberately non-interactive and never installs packages,
# changes the working directory, deletes an output tree, or modifies a scenario
# folder.
#
# Run/source this file directly after editing only SCENARIO_DIRS below.
# By default, every configured MoFuSS run (including nominal/debug MC01) is
# processed. BAU/CCTS runs are paired by the same run ID within each
# configuration; capped and uncapped configurations are never pooled.
#
# The default accounting period is inferred from parameters.csv as the model
# start plus ten spin-up years through the model end. For a 2000-2030 model this
# is 2010-2030, using end-2009 (code 10) as the state baseline and end-2030
# (code 31) as the endpoint. The primary AGB result is therefore the change in
# the BAU-vs-CCTS AGB gap over the post-spin-up accounting period.
# Code 10 is an opening stock only: demand and Harvest_tot flows begin at code
# 11 (calendar 2010), so no 2000-2009 flow is counted.
#
# For a later subperiod, the baseline remains the end of START-1; for example,
# 2026:2030 uses (ICS_2030 - BAU_2030) - (ICS_2025 - BAU_2025).
#
# Positive values are avoided emissions. End-use avoided emissions use
# BAU - ICS, so the sign convention is consistent when both components are
# added.

.V9_REQUIRED_PACKAGES <- c("terra", "fs", "stringr", "dplyr", "readr", "tibble")
.V9_CO2_FACTOR <- 0.47 * (44 / 12)
.V9_MC_FILES <- c("k_all.csv", "rmax_all.csv", "i_st_all.csv")
.V13_SPINUP_YEARS <- 10L

# EDIT ONLY THIS BLOCK when changing country/region scenario folders.
# Folder order does not define pairing. BAU/CCTS roles and capped/uncapped
# configurations are read from each folder's parameters.csv.
SCENARIO_DIRS <- c(
  "D:/ken_1km_bau1_2030_v3_ng",
  "D:/ken_1km_bau1_2030_v3_g",
  "D:/ken_1km_ics3_2030_v3_ng",
  "D:/ken_1km_ics3_2030_v3_g"
)

.v9_stop <- function(...) {
  stop(paste0(...), call. = FALSE)
}

.v9_require_packages <- function() {
  missing <- .V9_REQUIRED_PACKAGES[
    !vapply(.V9_REQUIRED_PACKAGES, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    .v9_stop(
      "Missing required R packages: ", paste(missing, collapse = ", "),
      ". Install them before running v13; this script never installs packages."
    )
  }
  invisible(TRUE)
}

.v9_parse_bool <- function(x, option_name) {
  if (is.logical(x) && length(x) == 1L && !is.na(x)) return(x)
  y <- tolower(trimws(as.character(x)))
  if (y %in% c("true", "t", "1", "yes", "y")) return(TRUE)
  if (y %in% c("false", "f", "0", "no", "n")) return(FALSE)
  .v9_stop("Invalid boolean for --", option_name, ": ", x)
}

.v9_parse_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  allowed <- c(
    "manifest", "config-label", "period", "run-ids", "temp-dir",
    "dry-run", "overwrite", "enduse-basis", "help"
  )
  out <- list(
    manifest = NULL,
    config_label = NULL,
    period = "auto",
    run_ids = "all",
    temp_dir = tempdir(),
    dry_run = FALSE,
    overwrite = FALSE,
    enduse_basis = "demand",
    help = FALSE
  )

  for (arg in args) {
    if (!startsWith(arg, "--")) .v9_stop("Unknown positional argument: ", arg)
    item <- substring(arg, 3L)
    pieces <- strsplit(item, "=", fixed = TRUE)[[1]]
    key <- pieces[1]
    if (!key %in% allowed) .v9_stop("Unknown option --", key)
    value <- if (length(pieces) > 1L) paste(pieces[-1], collapse = "=") else NULL

    if (key %in% c("dry-run", "overwrite", "help")) {
      value <- if (is.null(value)) TRUE else .v9_parse_bool(value, key)
    } else if (is.null(value) || !nzchar(value)) {
      .v9_stop("Option --", key, " requires a value.")
    }

    key_r <- gsub("-", "_", key, fixed = TRUE)
    out[[key_r]] <- value
  }
  out
}

.v9_usage <- function() {
  cat(
    paste0(
      "MoFuSS avoided-emissions post-processing v13\n\n",
      "Default input:\n",
      "  SCENARIO_DIRS near the top of this script\n\n",
      "Options:\n",
      "  --manifest=CSV             Legacy resolved-pair manifest\n",
      "  --config-label=LABEL       Process one manifest label (default: all)\n",
      "  --period=auto|YYYY:YYYY    Default: post-spin-up horizon (start+10):end\n",
      "  --run-ids=all|1,2,5:10    Selected MC runs (default: all, including MC01)\n",
      "  --temp-dir=DIR             Existing writable terra temp directory\n",
      "  --enduse-basis=demand      Only implemented basis\n",
      "  --dry-run[=true|false]     Validate without creating outputs\n",
      "  --overwrite[=true|false]   Replace known output files; never deletes directories\n",
      "  --help\n"
    )
  )
}

.v9_parse_period <- function(x) {
  if (identical(tolower(trimws(x)), "auto")) return(NULL)
  m <- stringr::str_match(trimws(x), "^([0-9]{4}):([0-9]{4})$")
  if (any(is.na(m))) .v9_stop("--period must have the form YYYY:YYYY; got: ", x)
  years <- as.integer(m[1, 2:3])
  if (years[1] > years[2]) .v9_stop("Period start is after period end: ", x)
  years
}

.v9_parse_run_ids <- function(x) {
  x <- trimws(tolower(x))
  if (identical(x, "all")) return(NULL)
  tokens <- strsplit(x, ",", fixed = TRUE)[[1]]
  ids <- integer()
  for (token in tokens) {
    token <- trimws(token)
    if (grepl("^[0-9]+$", token)) {
      ids <- c(ids, as.integer(token))
    } else if (grepl("^[0-9]+:[0-9]+$", token)) {
      ends <- as.integer(strsplit(token, ":", fixed = TRUE)[[1]])
      if (ends[1] > ends[2]) .v9_stop("Descending run range is not allowed: ", token)
      ids <- c(ids, seq.int(ends[1], ends[2]))
    } else {
      .v9_stop("Invalid --run-ids token: ", token)
    }
  }
  ids <- sort(unique(ids))
  if (!length(ids) || any(ids < 1L)) .v9_stop("--run-ids must contain positive integers.")
  ids
}

.v9_norm_existing <- function(path, what) {
  if (is.na(path) || !nzchar(trimws(path))) .v9_stop("Missing ", what, " path.")
  if (!dir.exists(path) && !file.exists(path)) .v9_stop(what, " does not exist: ", path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.v9_norm_output <- function(path) {
  if (is.na(path) || !nzchar(trimws(path))) .v9_stop("Missing emissions_dir path.")
  path <- path.expand(trimws(path))
  if (!grepl("^(?:[A-Za-z]:[/\\\\]|/)", path)) path <- file.path(getwd(), path)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

.v9_is_absolute_path <- function(path) {
  grepl("^(?:[A-Za-z]:[/\\\\]|/)", path)
}

.v9_resolve_manifest_path <- function(path, manifest_dir) {
  path <- trimws(as.character(path))
  if (length(path) != 1L || is.na(path) || !nzchar(path)) return(path)
  if (.v9_is_absolute_path(path)) path else file.path(manifest_dir, path)
}

.v9_path_key <- function(path) {
  path <- gsub("\\\\", "/", path)
  path <- sub("/+$", "", path)
  if (.Platform$OS.type == "windows") tolower(path) else path
}

.v9_is_within <- function(child, parent) {
  child <- paste0(.v9_path_key(child), "/")
  parent <- paste0(.v9_path_key(parent), "/")
  startsWith(child, parent)
}

.v9_read_delimited <- function(path) {
  first <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
  if (!length(first)) .v9_stop("Empty table: ", path)
  commas <- lengths(regmatches(first, gregexpr(",", first, fixed = TRUE)))
  semis <- lengths(regmatches(first, gregexpr(";", first, fixed = TRUE)))
  delim <- if (semis > commas) ";" else ","
  suppressMessages(
    readr::read_delim(
      path,
      delim = delim,
      trim_ws = TRUE,
      show_col_types = FALSE,
      progress = FALSE,
      name_repair = "minimal"
    )
  )
}

.v9_param_value <- function(tbl, key, path) {
  if (!all(c("Var", "ParCHR") %in% names(tbl))) {
    .v9_stop("Parameter table must contain Var and ParCHR columns: ", path)
  }
  hit <- which(trimws(tbl[["Var"]]) == key)
  if (length(hit) != 1L) {
    .v9_stop("Expected exactly one parameter '", key, "' in ", path, "; found ", length(hit), ".")
  }
  trimws(as.character(tbl[["ParCHR"]][hit]))
}

.v9_read_scenario_parameters <- function(root) {
  country_file <- file.path(root, "LULCC", "TempTables", "Country.csv")
  if (!file.exists(country_file)) .v9_stop("Missing Country.csv: ", country_file)
  country_tbl <- .v9_read_delimited(country_file)
  if (!"Country" %in% names(country_tbl)) .v9_stop("Country.csv lacks a Country column: ", country_file)
  key_col <- intersect(c("Key.", "Key"), names(country_tbl))
  rows <- if (length(key_col)) which(as.character(country_tbl[[key_col[1]]]) == "1") else 1L
  if (length(rows) != 1L) .v9_stop("Could not identify exactly one active Country.csv row: ", country_file)
  source_name <- trimws(as.character(country_tbl[["Country"]][rows]))
  parameter_dir <- file.path(root, "LULCC", "DownloadedDatasets", paste0("SourceData", source_name))
  if (!dir.exists(parameter_dir)) .v9_stop("Missing parameter directory: ", parameter_dir)
  candidates <- list.files(
    parameter_dir,
    pattern = "^parameters.*\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(candidates) != 1L) {
    .v9_stop("Expected one parameters*.csv in ", parameter_dir, "; found ", length(candidates), ".")
  }
  parameter_file <- normalizePath(candidates, winslash = "/", mustWork = TRUE)
  tbl <- .v9_read_delimited(parameter_file)
  get_chr <- function(key) .v9_param_value(tbl, key, parameter_file)
  get_int <- function(key) {
    value <- suppressWarnings(as.integer(get_chr(key)))
    if (length(value) != 1L || is.na(value)) .v9_stop("Parameter '", key, "' is not an integer in ", parameter_file)
    value
  }
  get_num <- function(key) {
    value <- suppressWarnings(as.numeric(get_chr(key)))
    if (length(value) != 1L || !is.finite(value)) .v9_stop("Parameter '", key, "' is not numeric in ", parameter_file)
    value
  }
  list(
    parameter_file = parameter_file,
    source_name = source_name,
    iso3 = toupper(get_chr("region2BprocessedCtry_iso")),
    country = get_chr("region2BprocessedCtry"),
    byregion = get_chr("byregion"),
    continent = get_chr("region2BprocessedCont"),
    region = get_chr("region2BprocessedReg"),
    subcountry = get_chr("subcountry"),
    scenario = get_chr("scenario_ver"),
    model_start_year = get_int("start_year"),
    model_end_year = get_int("end_year"),
    mc_runs = get_int("monte_carlo_runs"),
    uncapped_regrowth = get_int("uncapped_regrowth"),
    gee_scale = get_num("GEE_scale"),
    epsg_pcs = get_int("epsg_pcs"),
    efchratio = get_num("efchratio")
  )
}

.v11_safe_id <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) .v9_stop("Cannot construct an output identifier from scenario metadata.")
  x
}

.v13_script_path <- function() {
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
    file.path(getwd(), "2post_emissions_bau-vs-ics_v13.R"),
    file.path(
      getwd(), "localhost", "scripts", "postprocessing_emissions",
      "2post_emissions_bau-vs-ics_v13.R"
    )
  ))
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  candidates <- candidates[
    basename(candidates) == "2post_emissions_bau-vs-ics_v13.R"
  ]
  if (!length(candidates)) return(NA_character_)
  normalizePath(candidates[[1]], winslash = "/", mustWork = TRUE)
}

.v13_internal_pairs <- function(scenario_dirs = SCENARIO_DIRS) {
  if (!length(scenario_dirs)) .v9_stop("SCENARIO_DIRS is empty.")
  paths <- vapply(
    scenario_dirs, .v9_norm_existing, character(1), what = "scenario directory"
  )
  if (anyDuplicated(tolower(paths))) .v9_stop("SCENARIO_DIRS contains duplicate folders.")
  parents <- unique(tolower(dirname(paths)))
  if (length(parents) != 1L) {
    .v9_stop("All SCENARIO_DIRS must share one immediate parent for automatic outputs.")
  }
  parent <- dirname(paths)[match(parents[[1]], tolower(dirname(paths)))]
  parameters <- lapply(paths, .v9_read_scenario_parameters)
  metadata <- data.frame(
    scenario_dir = paths,
    scenario = vapply(parameters, `[[`, character(1), "scenario"),
    role = ifelse(
      grepl("^bau", vapply(parameters, `[[`, character(1), "scenario"), ignore.case = TRUE),
      "BAU", "CCTS"
    ),
    iso3 = vapply(parameters, `[[`, character(1), "iso3"),
    country = vapply(parameters, `[[`, character(1), "country"),
    byregion = vapply(parameters, `[[`, character(1), "byregion"),
    continent = vapply(parameters, `[[`, character(1), "continent"),
    region = vapply(parameters, `[[`, character(1), "region"),
    subcountry = vapply(parameters, `[[`, character(1), "subcountry"),
    start_year = vapply(parameters, `[[`, integer(1), "model_start_year"),
    end_year = vapply(parameters, `[[`, integer(1), "model_end_year"),
    mc_runs = vapply(parameters, `[[`, integer(1), "mc_runs"),
    uncapped = vapply(parameters, `[[`, integer(1), "uncapped_regrowth"),
    gee_scale = vapply(parameters, `[[`, numeric(1), "gee_scale"),
    epsg_pcs = vapply(parameters, `[[`, integer(1), "epsg_pcs"),
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
  if (!length(bau)) .v9_stop("SCENARIO_DIRS contains no scenario_ver beginning with BAU.")
  if (!length(alternatives)) .v9_stop("SCENARIO_DIRS contains no CCTS/alternative scenario.")

  pairs <- lapply(alternatives, function(i) {
    match_bau <- bau[keys[bau] == keys[[i]]]
    if (length(match_bau) != 1L) {
      .v9_stop(
        "Alternative ", metadata$scenario[[i]], " matched ", length(match_bau),
        " BAU folders; expected exactly one: ", metadata$scenario_dir[[i]]
      )
    }
    b <- metadata[match_bau, , drop = FALSE]
    a <- metadata[i, , drop = FALSE]
    mode <- if (a$uncapped == 1L) "uncapped" else "capped"
    analysis_start_year <- a$start_year + .V13_SPINUP_YEARS
    scope_type <- tolower(trimws(a$byregion))
    scope_name <- if (identical(scope_type, "country")) {
      a$iso3
    } else if (identical(scope_type, "regional")) {
      a$region
    } else {
      .v9_stop(
        "Unsupported byregion value '", a$byregion, "' for ", a$scenario,
        ". Expected exactly 'Country' or 'Regional'."
      )
    }
    scope_id <- .v11_safe_id(scope_name)
    label <- paste(
      scope_id,
      paste0(format(a$gee_scale, scientific = FALSE, trim = TRUE), "m"),
      paste0(.v11_safe_id(b$scenario), "_vs_", .v11_safe_id(a$scenario)),
      paste0(analysis_start_year, "_", a$end_year),
      mode,
      sep = "_"
    )
    analysis_id <- paste(
      scope_id, analysis_start_year, a$end_year, sep = "_"
    )
    output <- normalizePath(
      file.path(parent, "mofuss_postprocessing", analysis_id, "pairs", label, "emissions"),
      winslash = "/", mustWork = FALSE
    )
    data.frame(
      label = label,
      bau_dir = b$scenario_dir,
      ics_dir = a$scenario_dir,
      emissions_dir = output,
      stringsAsFactors = FALSE
    )
  })
  pairs <- do.call(rbind, pairs)
  rownames(pairs) <- NULL
  if (anyDuplicated(tolower(pairs$label))) .v9_stop("Automatically inferred pair labels are not unique.")
  unused_bau <- metadata$scenario_dir[
    metadata$role == "BAU" & !tolower(metadata$scenario_dir) %in% tolower(pairs$bau_dir)
  ]
  if (length(unused_bau)) {
    .v9_stop("BAU folders have no matching CCTS scenario: ", paste(unused_bau, collapse = ", "))
  }
  pairs
}

.v9_discover_runs <- function(root) {
  dirs <- list.dirs(root, recursive = FALSE, full.names = TRUE)
  names0 <- basename(dirs)
  m <- stringr::str_match(names0, "^debugging_([0-9]+)$")
  keep <- !is.na(m[, 2])
  out <- tibble::tibble(
    run_id = as.integer(m[keep, 2]),
    run_dir = normalizePath(dirs[keep], winslash = "/", mustWork = TRUE)
  )
  if (anyDuplicated(out$run_id)) .v9_stop("Duplicate debugging_N run IDs under: ", root)
  dplyr::arrange(out, .data$run_id)
}

.v9_code_file <- function(run_dir, stem, code) {
  file.path(run_dir, sprintf("%s%02d.tif", stem, code))
}

.v9_read_mc_table <- function(path) {
  if (!file.exists(path)) .v9_stop("Missing Monte Carlo input table: ", path)
  x <- suppressMessages(
    readr::read_csv(
      path,
      show_col_types = FALSE,
      progress = FALSE,
      name_repair = "minimal"
    )
  )
  if (!"Key" %in% names(x)) .v9_stop("Monte Carlo table lacks Key column: ", path)
  key <- suppressWarnings(as.integer(x[["Key"]]))
  if (anyNA(key) || anyDuplicated(key)) .v9_stop("Invalid or duplicate Key values in: ", path)
  x[["Key"]] <- key
  x
}

.v9_compare_mc_rows <- function(bau_root, ics_root, run_ids, tolerance = 1e-12) {
  rows <- list()
  check_ids <- sort(unique(c(1L, run_ids)))
  for (file_name in .V9_MC_FILES) {
    bau_file <- file.path(bau_root, "Temp", file_name)
    ics_file <- file.path(ics_root, "Temp", file_name)
    bau <- .v9_read_mc_table(bau_file)
    ics <- .v9_read_mc_table(ics_file)
    bau_file <- normalizePath(bau_file, winslash = "/", mustWork = TRUE)
    ics_file <- normalizePath(ics_file, winslash = "/", mustWork = TRUE)
    bau_md5 <- unname(tools::md5sum(bau_file))
    ics_md5 <- unname(tools::md5sum(ics_file))
    same_columns <- identical(names(bau), names(ics))
    if (!same_columns) {
      .v9_stop("BAU/ICS Monte Carlo table columns differ for ", file_name, ".")
    }
    value_cols <- setdiff(names(bau), "Key")
    for (run_id in check_ids) {
      ib <- which(bau[["Key"]] == run_id)
      ii <- which(ics[["Key"]] == run_id)
      if (length(ib) != 1L || length(ii) != 1L) {
        rows[[length(rows) + 1L]] <- tibble::tibble(
          file = file_name,
          run_id = run_id,
          selected = run_id %in% run_ids,
          matched = FALSE,
          max_abs_diff = NA_real_,
          mismatch_columns = "missing_or_duplicate_Key",
          bau_file = bau_file,
          ics_file = ics_file,
          bau_md5 = bau_md5,
          ics_md5 = ics_md5
        )
        next
      }
      mismatches <- character()
      max_abs <- 0
      for (column in value_cols) {
        a <- bau[[column]][ib]
        b <- ics[[column]][ii]
        if (is.numeric(a) && is.numeric(b)) {
          difference <- abs(a - b)
          ok <- (is.na(a) && is.na(b)) || (!is.na(difference) && difference <= tolerance)
          if (!is.na(difference)) max_abs <- max(max_abs, difference)
        } else {
          ok <- identical(as.character(a), as.character(b))
        }
        if (!ok) mismatches <- c(mismatches, column)
      }
      rows[[length(rows) + 1L]] <- tibble::tibble(
        file = file_name,
        run_id = run_id,
        selected = run_id %in% run_ids,
        matched = !length(mismatches),
        max_abs_diff = max_abs,
        mismatch_columns = paste(utils::head(mismatches, 25L), collapse = ";"),
        bau_file = bau_file,
        ics_file = ics_file,
        bau_md5 = bau_md5,
        ics_md5 = ics_md5
      )
    }
  }
  dplyr::bind_rows(rows)
}

.v9_demand_inventory <- function(demand_dir) {
  if (!dir.exists(demand_dir)) .v9_stop("Missing demand_out directory: ", demand_dir)
  files <- list.files(demand_dir, pattern = "\\.tif$", full.names = TRUE, ignore.case = TRUE)
  m <- stringr::str_match(basename(files), "^WorldPop_(.+)_([0-9]{4})_demand\\.tif$")
  keep <- !is.na(m[, 2])
  tibble::tibble(
    tag = tolower(m[keep, 2]),
    year = as.integer(m[keep, 3]),
    file = normalizePath(files[keep], winslash = "/", mustWork = TRUE)
  )
}

.v9_fuel_config <- function() {
  tibble::tribble(
    ~tag,            ~ef_name,        ~co2_in_harvest, ~charcoal_ratio, ~demand_unit,
    "fuelwood",      "Biomass",       TRUE,            FALSE,           "tonnes_dry_wood",
    "imp_fuelwood",  "Imp_biomass",   TRUE,            FALSE,           "tonnes_dry_wood",
    "charcoal",      "Charcoal",      TRUE,            TRUE,            "tonnes_wood_equivalent",
    "imp_charcoal",  "Imp_charcoal",  TRUE,            TRUE,            "tonnes_wood_equivalent",
    "gas",           "Gas",            FALSE,           FALSE,           "tonnes_fuel",
    "kerosene",      "Kerosene",       FALSE,           FALSE,           "tonnes_fuel",
    "electric",      "Electricity",    FALSE,           FALSE,           "MWh",
    "pellets",       "Pellets",        FALSE,           FALSE,           "tonnes_fuel",
    "ethanol",       "Ethanol",        FALSE,           FALSE,           "tonnes_fuel",
    "biogas",        "Biogas",         FALSE,           FALSE,           "tonnes_fuel",
    "coal",          "Coal",           FALSE,           FALSE,           "tonnes_fuel"
  )
}

.v9_read_efdb <- function(root, iso3) {
  path <- file.path(
    root, "LULCC", "DownloadedDatasets", "SourceDataGlobal",
    "demand", "demand_in", "efdb_all.csv"
  )
  if (!file.exists(path)) .v9_stop("Missing emission-factor table: ", path)
  x <- suppressMessages(readr::read_csv(path, show_col_types = FALSE, progress = FALSE))
  required <- c("GID_0", "fueltype", "CO2", "CH4", "N2O")
  if (!all(required %in% names(x))) .v9_stop("EF table lacks required columns: ", path)
  x <- x[toupper(trimws(x[["GID_0"]])) == iso3, required]
  x[["fuel_norm"]] <- tolower(trimws(x[["fueltype"]]))
  x <- dplyr::arrange(x, .data$fuel_norm)
  list(path = normalizePath(path, winslash = "/", mustWork = TRUE), rows = x)
}

.v9_check_output_path <- function(output_dir, bau_dir, ics_dir, overwrite) {
  out_key <- .v9_path_key(output_dir)
  parent_key <- .v9_path_key(dirname(output_dir))
  if (identical(out_key, parent_key)) .v9_stop("Refusing drive/root emissions_dir: ", output_dir)
  for (scenario_dir in c(bau_dir, ics_dir)) {
    if (.v9_is_within(output_dir, scenario_dir) || .v9_is_within(scenario_dir, output_dir)) {
      .v9_stop("emissions_dir must be separate from scenario trees: ", output_dir)
    }
  }
  if (dir.exists(output_dir)) {
    existing <- list.files(output_dir, all.files = TRUE, no.. = TRUE)
    if (length(existing) && !overwrite) {
      .v9_stop("Output directory is not empty and --overwrite is false: ", output_dir)
    }
  } else if (file.exists(output_dir)) {
    .v9_stop("emissions_dir is an existing file: ", output_dir)
  }
  invisible(TRUE)
}

.v9_preflight_config <- function(config, period, run_ids_requested, temp_dir, overwrite) {
  label <- trimws(as.character(config[["label"]]))
  if (!nzchar(label)) .v9_stop("Manifest contains an empty label.")
  bau_dir <- .v9_norm_existing(config[["bau_dir"]], paste0("BAU directory for ", label))
  ics_dir <- .v9_norm_existing(config[["ics_dir"]], paste0("ICS directory for ", label))
  emissions_dir <- .v9_norm_output(config[["emissions_dir"]])
  if (identical(.v9_path_key(bau_dir), .v9_path_key(ics_dir))) {
    .v9_stop("BAU and ICS directories are identical for label ", label, ".")
  }
  .v9_check_output_path(emissions_dir, bau_dir, ics_dir, overwrite)

  bau_par <- .v9_read_scenario_parameters(bau_dir)
  ics_par <- .v9_read_scenario_parameters(ics_dir)
  comparable <- c(
    "iso3", "country", "byregion", "continent", "region", "subcountry",
    "model_start_year", "model_end_year", "mc_runs", "uncapped_regrowth",
    "gee_scale", "epsg_pcs", "efchratio"
  )
  for (field in comparable) {
    if (!isTRUE(all.equal(bau_par[[field]], ics_par[[field]], tolerance = 0))) {
      .v9_stop("BAU/ICS parameter mismatch for '", field, "' in label ", label, ".")
    }
  }
  analysis_start_year <- bau_par$model_start_year + .V13_SPINUP_YEARS
  if (analysis_start_year > bau_par$model_end_year) {
    .v9_stop(
      "The inferred post-spin-up start (", analysis_start_year,
      ") exceeds model end_year for label ", label, "."
    )
  }
  if (is.null(period)) {
    period <- c(analysis_start_year, bau_par$model_end_year)
  }
  if (!grepl("^bau", bau_par$scenario, ignore.case = TRUE)) {
    .v9_stop("BAU scenario_ver does not begin with BAU for label ", label, ": ", bau_par$scenario)
  }
  if (grepl("^bau", ics_par$scenario, ignore.case = TRUE)) {
    .v9_stop("CCTS/alternative scenario_ver begins with BAU for label ", label, ": ", ics_par$scenario)
  }
  if (period[1] < bau_par$model_start_year || period[2] > bau_par$model_end_year) {
    .v9_stop(
      "Period ", paste(period, collapse = ":"), " is outside the model horizon ",
      bau_par$model_start_year, ":", bau_par$model_end_year, " for label ", label, "."
    )
  }
  if (period[1] < analysis_start_year) {
    .v9_stop(
      "Period starts before the post-spin-up analysis year ", analysis_start_year,
      " (model start_year + ", .V13_SPINUP_YEARS, ") for label ", label, "."
    )
  }

  full_horizon <- period[1] == bau_par$model_start_year
  start_code <- period[1] - bau_par$model_start_year + 1L
  baseline_code <- if (full_horizon) 0L else start_code - 1L
  baseline_year <- if (full_horizon) bau_par$model_start_year else period[1] - 1L
  baseline_source <- if (full_horizon) "initial_agb_reference" else "Growth_less_harv"
  baseline_timing <- if (full_horizon) {
    "start_of_model_start_year_before_first_step"
  } else {
    "end_of_previous_year"
  }
  end_code <- period[2] - bau_par$model_start_year + 1L
  initial_agb <- c(
    BAU = file.path(bau_dir, "LULCC", "TempRaster", "agb3_c.tif"),
    ICS = file.path(ics_dir, "LULCC", "TempRaster", "agb3_c.tif")
  )
  if (full_horizon) {
    missing_initial <- initial_agb[!file.exists(initial_agb)]
    if (length(missing_initial)) {
      .v9_stop(
        "Full-horizon accounting requires the initial AGB reference raster(s): ",
        paste(missing_initial, collapse = ", ")
      )
    }
    initial_agb <- vapply(
      initial_agb, normalizePath, character(1), winslash = "/", mustWork = TRUE
    )
    initial_md5 <- unname(tools::md5sum(initial_agb))
    if (!identical(initial_md5[1], initial_md5[2])) {
      .v9_stop(
        "Full-horizon BAU/ICS initial AGB references differ for label ", label,
        "; a common zero-gap baseline cannot be assumed."
      )
    }
  } else {
    initial_md5 <- rep(NA_character_, 2L)
  }
  expected_runs <- seq_len(bau_par$mc_runs)
  run_ids <- if (is.null(run_ids_requested)) expected_runs else run_ids_requested
  if (any(!run_ids %in% expected_runs)) {
    .v9_stop("Requested run IDs outside 1:", bau_par$mc_runs, " for label ", label, ".")
  }

  bau_runs <- .v9_discover_runs(bau_dir)
  ics_runs <- .v9_discover_runs(ics_dir)
  unexpected_bau <- setdiff(bau_runs$run_id, expected_runs)
  unexpected_ics <- setdiff(ics_runs$run_id, expected_runs)
  if (length(unexpected_bau) || length(unexpected_ics)) {
    .v9_stop(
      "Run directories exceed monte_carlo_runs for label ", label,
      "; BAU unexpected: ", paste(unexpected_bau, collapse = ","),
      "; ICS unexpected: ", paste(unexpected_ics, collapse = ","), "."
    )
  }
  bau_map <- stats::setNames(bau_runs$run_dir, as.character(bau_runs$run_id))
  ics_map <- stats::setNames(ics_runs$run_dir, as.character(ics_runs$run_id))
  period_codes <- seq.int(start_code, end_code)

  completeness <- dplyr::bind_rows(lapply(expected_runs, function(run_id) {
    key <- as.character(run_id)
    bau_run <- unname(bau_map[key])
    ics_run <- unname(ics_map[key])
    bau_exists <- length(bau_run) == 1L && !is.na(bau_run)
    ics_exists <- length(ics_run) == 1L && !is.na(ics_run)
    path_or_na <- function(run, exists, stem, code) {
      if (!exists) return(NA_character_)
      .v9_code_file(run, stem, code)
    }
    bau_baseline <- if (full_horizon && bau_exists) {
      unname(initial_agb[["BAU"]])
    } else {
      path_or_na(bau_run, bau_exists, "Growth_less_harv", baseline_code)
    }
    ics_baseline <- if (full_horizon && ics_exists) {
      unname(initial_agb[["ICS"]])
    } else {
      path_or_na(ics_run, ics_exists, "Growth_less_harv", baseline_code)
    }
    bau_end <- path_or_na(bau_run, bau_exists, "Growth_less_harv", end_code)
    ics_end <- path_or_na(ics_run, ics_exists, "Growth_less_harv", end_code)
    bau_harvest <- if (bau_exists) vapply(period_codes, function(code) {
      file.exists(.v9_code_file(bau_run, "Harvest_tot", code))
    }, logical(1)) else rep(FALSE, length(period_codes))
    ics_harvest <- if (ics_exists) vapply(period_codes, function(code) {
      file.exists(.v9_code_file(ics_run, "Harvest_tot", code))
    }, logical(1)) else rep(FALSE, length(period_codes))
    tibble::tibble(
      label = label,
      run_id = run_id,
      selected = run_id %in% run_ids,
      bau_run_exists = bau_exists,
      ics_run_exists = ics_exists,
      bau_baseline_exists = !is.na(bau_baseline) && file.exists(bau_baseline),
      ics_baseline_exists = !is.na(ics_baseline) && file.exists(ics_baseline),
      bau_end_exists = !is.na(bau_end) && file.exists(bau_end),
      ics_end_exists = !is.na(ics_end) && file.exists(ics_end),
      bau_harvest_years_found = sum(bau_harvest),
      ics_harvest_years_found = sum(ics_harvest),
      harvest_years_expected = length(period_codes),
      complete = bau_exists && ics_exists &&
        !is.na(bau_baseline) && file.exists(bau_baseline) &&
        !is.na(ics_baseline) && file.exists(ics_baseline) &&
        !is.na(bau_end) && file.exists(bau_end) &&
        !is.na(ics_end) && file.exists(ics_end) &&
        all(bau_harvest) && all(ics_harvest),
      bau_run_dir = if (bau_exists) bau_run else NA_character_,
      ics_run_dir = if (ics_exists) ics_run else NA_character_,
      bau_baseline_file = bau_baseline,
      ics_baseline_file = ics_baseline,
      bau_end_file = bau_end,
      ics_end_file = ics_end
    )
  }))
  failed_selected <- completeness$selected & !completeness$complete
  if (any(failed_selected)) {
    .v9_stop("Incomplete selected runs for label ", label, ": ", paste(completeness$run_id[failed_selected], collapse = ", "))
  }

  pairing <- .v9_compare_mc_rows(bau_dir, ics_dir, run_ids)
  failed_pairing <- pairing$selected & !pairing$matched
  pairing_validated <- !any(failed_pairing)
  if (any(failed_pairing)) {
    bad <- unique(pairing$run_id[failed_pairing])
    warning(
      paste0(
        "BAU/CCTS MC input rows are not yet paired for label ", label,
        "; run-ID comparisons will be processed provisionally. Failing run IDs: ",
        paste(bad, collapse = ", "),
        ". Re-run CCTS with the corrected bypassMC workflow before treating the ",
        "batch SD/SE as paired uncertainty. See mc_pairing_check.csv."
      ),
      call. = FALSE
    )
  }

  selected_rows <- completeness[match(run_ids, completeness$run_id), ]
  for (j in seq_len(nrow(selected_rows))) {
    rasters <- lapply(
      c(
        selected_rows$bau_baseline_file[j], selected_rows$ics_baseline_file[j],
        selected_rows$bau_end_file[j], selected_rows$ics_end_file[j]
      ),
      terra::rast
    )
    if (!do.call(terra::compareGeom, c(rasters, list(stopOnError = FALSE)))) {
      .v9_stop("Geometry mismatch among the four AGB endpoints for label ", label, ", run ", selected_rows$run_id[j], ".")
    }
  }

  bau_demand_dir <- file.path(
    bau_dir, "LULCC", "DownloadedDatasets", "SourceDataGlobal",
    "demand", "demand_out"
  )
  ics_demand_dir <- file.path(
    ics_dir, "LULCC", "DownloadedDatasets", "SourceDataGlobal",
    "demand", "demand_out"
  )
  bau_demand <- .v9_demand_inventory(bau_demand_dir)
  ics_demand <- .v9_demand_inventory(ics_demand_dir)
  bau_demand <- bau_demand[bau_demand$year >= period[1] & bau_demand$year <= period[2], ]
  ics_demand <- ics_demand[ics_demand$year >= period[1] & ics_demand$year <= period[2], ]
  if (!nrow(bau_demand) || !nrow(ics_demand)) .v9_stop("No period demand rasters for label ", label, ".")
  fuel_config <- .v9_fuel_config()
  tags_bau <- sort(unique(bau_demand$tag))
  tags_ics <- sort(unique(ics_demand$tag))
  if (!identical(tags_bau, tags_ics)) .v9_stop("BAU/ICS period demand tag sets differ for label ", label, ".")
  unknown <- setdiff(tags_bau, fuel_config$tag)
  if (length(unknown)) .v9_stop("Unexplained demand fuel tags for label ", label, ": ", paste(unknown, collapse = ", "))
  expected_years <- seq.int(period[1], period[2])
  for (tag in tags_bau) {
    for (scenario in c("BAU", "ICS")) {
      index <- if (scenario == "BAU") bau_demand else ics_demand
      rows <- index[index$tag == tag, ]
      if (anyDuplicated(rows$year) || !identical(sort(rows$year), expected_years)) {
        .v9_stop(scenario, " demand files are incomplete or duplicated for tag ", tag, " in label ", label, ".")
      }
    }
  }

  all_demand_files <- c(bau_demand$file, ics_demand$file)
  demand_template <- terra::rast(all_demand_files[1])
  for (file in all_demand_files[-1]) {
    if (!terra::compareGeom(demand_template, terra::rast(file), stopOnError = FALSE)) {
      .v9_stop("Demand raster geometry mismatch for label ", label, ": ", file)
    }
  }

  bau_ef <- .v9_read_efdb(bau_dir, bau_par$iso3)
  ics_ef <- .v9_read_efdb(ics_dir, ics_par$iso3)
  needed_ef <- fuel_config$ef_name[match(tags_bau, fuel_config$tag)]
  needed_norm <- tolower(needed_ef)
  subset_ef <- function(ef) {
    x <- ef$rows[ef$rows$fuel_norm %in% needed_norm, c("fuel_norm", "CO2", "CH4", "N2O")]
    dplyr::arrange(x, .data$fuel_norm)
  }
  bau_needed <- subset_ef(bau_ef)
  ics_needed <- subset_ef(ics_ef)
  for (ef_name in needed_norm) {
    if (sum(bau_needed$fuel_norm == ef_name) != 1L || sum(ics_needed$fuel_norm == ef_name) != 1L) {
      .v9_stop("Expected one BAU and ICS EF row for '", ef_name, "' in label ", label, ".")
    }
  }
  if (!isTRUE(all.equal(bau_needed, ics_needed, tolerance = 0, check.attributes = FALSE))) {
    .v9_stop("BAU/ICS emission-factor rows differ for label ", label, ".")
  }
  ef_values <- unlist(bau_needed[c("CO2", "CH4", "N2O")], use.names = FALSE)
  if (!all(is.finite(as.numeric(ef_values)))) .v9_stop("Non-finite emission factors for label ", label, ".")

  list(
    label = label,
    bau_dir = bau_dir,
    ics_dir = ics_dir,
    emissions_dir = emissions_dir,
    bau_parameters = bau_par,
    ics_parameters = ics_par,
    spinup_years = .V13_SPINUP_YEARS,
    analysis_start_year = analysis_start_year,
    period = period,
    period_years = expected_years,
    period_codes = period_codes,
    full_horizon = full_horizon,
    start_code = start_code,
    baseline_year = baseline_year,
    baseline_code = baseline_code,
    baseline_source = baseline_source,
    baseline_timing = baseline_timing,
    initial_agb_bau = unname(initial_agb[["BAU"]]),
    initial_agb_ics = unname(initial_agb[["ICS"]]),
    initial_agb_md5 = unname(initial_md5[1]),
    end_code = end_code,
    run_ids = run_ids,
    completeness = completeness,
    selected_runs = selected_rows,
    pairing = pairing,
    pairing_validated = pairing_validated,
    bau_demand = bau_demand,
    ics_demand = ics_demand,
    demand_template = demand_template,
    fuel_config = fuel_config,
    demand_tags = tags_bau,
    ef_rows = bau_needed,
    efdb_path = bau_ef$path,
    efdb_ics_path = ics_ef$path,
    temp_dir = temp_dir
  )
}

.v9_write_csv <- function(x, path, overwrite) {
  if (file.exists(path) && !overwrite) .v9_stop("Refusing to overwrite: ", path)
  readr::write_csv(x, path, na = "")
  invisible(path)
}

.v9_write_raster <- function(x, path, overwrite) {
  if (file.exists(path) && !overwrite) .v9_stop("Refusing to overwrite: ", path)
  terra::writeRaster(
    x,
    path,
    filetype = "GTiff",
    datatype = "FLT8S",
    gdal = "COMPRESS=DEFLATE",
    overwrite = overwrite
  )
}

.v9_global_sum <- function(x) {
  as.numeric(terra::global(x, "sum", na.rm = TRUE)[1, 1])
}

.v9_sum_rasters_na <- function(rasters) {
  if (!length(rasters)) .v9_stop("Cannot sum an empty raster list.")
  if (length(rasters) == 1L) return(rasters[[1]])
  stack <- terra::rast(rasters)
  value_sum <- terra::app(stack, fun = sum, na.rm = TRUE)
  valid_count <- terra::app(!is.na(stack), fun = sum, na.rm = TRUE)
  terra::ifel(valid_count == 0, NA, value_sum)
}

.v9_sum_files_na <- function(files) {
  rasters <- lapply(files, terra::rast)
  template <- rasters[[1]]
  for (raster in rasters[-1]) {
    if (!terra::compareGeom(template, raster, stopOnError = FALSE)) {
      .v9_stop("Geometry mismatch while summing: ", paste(files, collapse = ", "))
    }
  }
  .v9_sum_rasters_na(rasters)
}

.v9_tolerance <- function(reference, relative = 1e-8, absolute = 1e-3) {
  max(absolute, abs(reference) * relative)
}

.v9_build_enduse <- function(preflight, dirs, overwrite) {
  period_label <- paste(preflight$period, collapse = "-")
  post_rows <- list()
  demand_rows <- list()
  delta_paths <- character()
  demand_totals <- list()

  ordered_tags <- preflight$fuel_config$tag[
    preflight$fuel_config$tag %in% preflight$demand_tags
  ]
  for (tag in ordered_tags) {
    config <- preflight$fuel_config[preflight$fuel_config$tag == tag, ]
    bau_rows <- preflight$bau_demand[preflight$bau_demand$tag == tag, ]
    ics_rows <- preflight$ics_demand[preflight$ics_demand$tag == tag, ]
    bau_rows <- dplyr::arrange(bau_rows, .data$year)
    ics_rows <- dplyr::arrange(ics_rows, .data$year)
    bau_sum <- .v9_sum_files_na(bau_rows$file)
    ics_sum <- .v9_sum_files_na(ics_rows$file)
    if (!terra::compareGeom(bau_sum, ics_sum, stopOnError = FALSE)) {
      .v9_stop("BAU/ICS demand geometry mismatch for tag ", tag, ".")
    }
    # Use a common BAU/ICS support for every per-fuel scalar and raster. The
    # annual summation itself retains a cell if at least one year is observed,
    # but a BAU-only or ICS-only cell is excluded from both sides here.
    common_support <- !is.na(bau_sum) & !is.na(ics_sum)
    bau_sum <- terra::ifel(common_support, bau_sum, NA)
    ics_sum <- terra::ifel(common_support, ics_sum, NA)

    ef_norm <- tolower(config$ef_name)
    ef <- preflight$ef_rows[preflight$ef_rows$fuel_norm == ef_norm, ]
    ef_co2 <- as.numeric(ef$CO2)
    ef_nonco2 <- as.numeric(ef$CH4) + as.numeric(ef$N2O)
    ef_value <- if (isTRUE(config$co2_in_harvest)) ef_nonco2 else ef_co2 + ef_nonco2
    divisor <- if (isTRUE(config$charcoal_ratio)) preflight$bau_parameters$efchratio else 1
    bau_emissions <- (bau_sum / divisor) * ef_value
    ics_emissions <- (ics_sum / divisor) * ef_value
    delta <- bau_emissions - ics_emissions

    bau_demand_total <- .v9_global_sum(bau_sum)
    ics_demand_total <- .v9_global_sum(ics_sum)
    bau_emissions_total <- .v9_global_sum(bau_emissions)
    ics_emissions_total <- .v9_global_sum(ics_emissions)
    delta_total <- .v9_global_sum(delta)
    if (abs((bau_emissions_total - ics_emissions_total) - delta_total) > .v9_tolerance(delta_total)) {
      .v9_stop("End-use raster/table reconciliation failed for tag ", tag, ".")
    }

    .v9_write_raster(bau_sum, file.path(dirs$enduse, paste0("bau_sum_", tag, ".tif")), overwrite)
    .v9_write_raster(ics_sum, file.path(dirs$enduse, paste0("ics_sum_", tag, ".tif")), overwrite)
    .v9_write_raster(bau_emissions, file.path(dirs$enduse, paste0("bau_co2_", tag, "enduse.tif")), overwrite)
    .v9_write_raster(ics_emissions, file.path(dirs$enduse, paste0("ics_co2_", tag, "enduse.tif")), overwrite)
    delta_path <- file.path(dirs$enduse, paste0("delta_co2_", tag, "enduse.tif"))
    .v9_write_raster(delta, delta_path, overwrite)
    delta_paths[[tag]] <- normalizePath(delta_path, winslash = "/", mustWork = TRUE)

    demand_rows[[tag]] <- tibble::tibble(
      fuel = tag,
      scenario = c("BAU", "ICS"),
      period = period_label,
      total = c(bau_demand_total, ics_demand_total),
      unit = config$demand_unit
    )
    post_rows[[tag]] <- tibble::tibble(
      fuel = tag,
      scenario = c("BAU", "ICS", "Delta"),
      period = period_label,
      total_tCO2e = c(bau_emissions_total, ics_emissions_total, delta_total),
      unit = "tonnes_CO2e",
      demand_unit = config$demand_unit,
      emission_factor_tCO2e_per_unit = ef_value,
      CO2_in_harvest_module = config$co2_in_harvest,
      charcoal_wood_to_fuel_ratio = divisor,
      enduse_basis = "demand"
    )
    demand_totals[[tag]] <- c(BAU = bau_demand_total, ICS = ics_demand_total)
  }

  demand_table <- dplyr::bind_rows(demand_rows)
  post_table <- dplyr::bind_rows(post_rows)
  enduse_total <- .v9_sum_rasters_na(lapply(unname(delta_paths), terra::rast))
  enduse_path <- file.path(dirs$enduse, "delta_co2_enduse.tif")
  enduse_total <- .v9_write_raster(enduse_total, enduse_path, overwrite)
  enduse_scalar_table <- sum(post_table$total_tCO2e[post_table$scenario == "Delta"])
  enduse_scalar_raster <- .v9_global_sum(enduse_total)
  if (abs(enduse_scalar_table - enduse_scalar_raster) > .v9_tolerance(enduse_scalar_table)) {
    .v9_stop("Total end-use raster does not reconcile to per-fuel Delta rows.")
  }

  .v9_write_csv(
    demand_table,
    file.path(dirs$enduse, paste0("summary_demand_", period_label, ".csv")),
    overwrite
  )
  .v9_write_csv(
    post_table,
    file.path(dirs$enduse, paste0("summary_co2_", period_label, ".csv")),
    overwrite
  )
  .v9_write_csv(
    tibble::tibble(fuel = names(delta_paths), file = unname(delta_paths)),
    file.path(dirs$enduse, "enduse_delta_raster_manifest.csv"),
    overwrite
  )
  list(
    raster = enduse_total,
    raster_path = enduse_path,
    total_tCO2e = enduse_scalar_table,
    post_table = post_table,
    demand_table = demand_table,
    demand_totals = demand_totals
  )
}

.v9_accumulate <- function(x, state, temp_dir, prefix) {
  prefix <- gsub("[^A-Za-z0-9_.-]", "_", prefix)
  new_sum_path <- tempfile(
    pattern = paste0(prefix, "_sum_"), tmpdir = temp_dir, fileext = ".tif"
  )
  new_sumsq_path <- tempfile(
    pattern = paste0(prefix, "_sumsq_"), tmpdir = temp_dir, fileext = ".tif"
  )
  if (is.null(state$sum)) {
    state$sum <- terra::writeRaster(
      x, new_sum_path, filetype = "GTiff", datatype = "FLT8S",
      gdal = "COMPRESS=DEFLATE", overwrite = FALSE
    )
    state$sumsq <- terra::writeRaster(
      x * x, new_sumsq_path, filetype = "GTiff", datatype = "FLT8S",
      gdal = "COMPRESS=DEFLATE", overwrite = FALSE
    )
    state$n <- 1L
  } else {
    if (!terra::compareGeom(state$sum, x, stopOnError = FALSE)) {
      .v9_stop("Monte Carlo raster geometry changed while accumulating ", prefix, ".")
    }
    na_mismatch <- (!is.na(state$sum) & is.na(x)) | (is.na(state$sum) & !is.na(x))
    if (.v9_global_sum(na_mismatch) != 0) {
      .v9_stop("Monte Carlo raster NA support changed while accumulating ", prefix, ".")
    }
    # Never overwrite an accumulator that is also a source in the expression.
    state$sum <- terra::writeRaster(
      state$sum + x, new_sum_path, filetype = "GTiff", datatype = "FLT8S",
      gdal = "COMPRESS=DEFLATE", overwrite = FALSE
    )
    state$sumsq <- terra::writeRaster(
      state$sumsq + x * x, new_sumsq_path, filetype = "GTiff", datatype = "FLT8S",
      gdal = "COMPRESS=DEFLATE", overwrite = FALSE
    )
    state$n <- state$n + 1L
  }
  state
}

.v9_write_mc_rasters <- function(state, directory, prefix, overwrite, scalar_values) {
  .v9_write_raster(state$sum, file.path(directory, paste0("S_", prefix, ".tif")), overwrite)
  .v9_write_raster(state$sumsq, file.path(directory, paste0("S2_", prefix, ".tif")), overwrite)
  mean_raster <- state$sum / state$n
  mean_raster <- .v9_write_raster(
    mean_raster, file.path(directory, paste0(prefix, "_mean.tif")), overwrite
  )
  expected_mean <- mean(scalar_values)
  raster_mean <- .v9_global_sum(mean_raster)
  if (abs(raster_mean - expected_mean) > .v9_tolerance(expected_mean)) {
    .v9_stop("Monte Carlo mean raster/table reconciliation failed for ", prefix, ".")
  }
  if (state$n >= 2L) {
    variance <- (state$sumsq - (state$sum * state$sum) / state$n) / (state$n - 1L)
    variance <- terra::ifel(variance < 0, 0, variance)
    sd_raster <- sqrt(variance)
    se_raster <- sd_raster / sqrt(state$n)
    .v9_write_raster(sd_raster, file.path(directory, paste0(prefix, "_sd.tif")), overwrite)
    .v9_write_raster(se_raster, file.path(directory, paste0(prefix, "_se.tif")), overwrite)
  }
  invisible(TRUE)
}

.v9_harvest_diagnostic <- function(preflight, enduse, overwrite, harvest_dir) {
  woody <- intersect(c("fuelwood", "imp_fuelwood", "charcoal", "imp_charcoal"), names(enduse$demand_totals))
  woody_bau <- sum(vapply(woody, function(tag) enduse$demand_totals[[tag]][["BAU"]], numeric(1)))
  woody_ics <- sum(vapply(woody, function(tag) enduse$demand_totals[[tag]][["ICS"]], numeric(1)))
  rows <- list()
  for (j in seq_len(nrow(preflight$selected_runs))) {
    run_id <- preflight$selected_runs$run_id[j]
    for (scenario in c("BAU", "ICS")) {
      run_dir <- if (scenario == "BAU") preflight$selected_runs$bau_run_dir[j] else preflight$selected_runs$ics_run_dir[j]
      files <- vapply(preflight$period_codes, function(code) {
        .v9_code_file(run_dir, "Harvest_tot", code)
      }, character(1))
      rasters <- lapply(files, terra::rast)
      for (raster in rasters[-1]) {
        if (!terra::compareGeom(rasters[[1]], raster, stopOnError = FALSE)) {
          .v9_stop("Harvest_tot geometry mismatch for ", preflight$label, ", run ", run_id, ", ", scenario, ".")
        }
      }
      actual <- sum(vapply(rasters, .v9_global_sum, numeric(1)))
      demand <- if (scenario == "BAU") woody_bau else woody_ics
      rows[[length(rows) + 1L]] <- tibble::tibble(
        label = preflight$label,
        run_id = run_id,
        scenario = scenario,
        period_start_year = preflight$period[1],
        period_end_year = preflight$period[2],
        actual_harvest_Mg = actual,
        woody_demand_Mg_wood_equivalent = demand,
        harvest_to_demand_ratio = if (demand == 0) NA_real_ else actual / demand,
        unmet_Mg_wood_equivalent = demand - actual,
        enduse_basis = "demand",
        adjustment_applied = FALSE,
        note = "NOT APPLIED: diagnostic only; end-use emissions use mapped demand"
      )
    }
  }
  diagnostic <- dplyr::bind_rows(rows)
  .v9_write_csv(
    diagnostic,
    file.path(harvest_dir, "harvest_vs_demand_diagnostic_NOT_APPLIED.csv"),
    overwrite
  )
  diagnostic
}

.v9_process_config <- function(preflight, overwrite) {
  output <- preflight$emissions_dir
  dirs <- list(
    root = output,
    harvest = file.path(output, "harvest"),
    enduse = file.path(output, "enduse"),
    total = file.path(output, "total")
  )
  include_mc1 <- 1L %in% preflight$run_ids
  if (include_mc1) dirs$summary_mc1 <- file.path(output, "summary_mc1")
  for (directory in dirs) dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  failed_dirs <- unlist(dirs)[!dir.exists(unlist(dirs))]
  if (length(failed_dirs)) {
    .v9_stop("Could not create output directories: ", paste(failed_dirs, collapse = ", "))
  }
  terra::terraOptions(progress = 1, tempdir = preflight$temp_dir)

  .v9_write_csv(preflight$completeness, file.path(output, "input_completeness.csv"), overwrite)
  .v9_write_csv(preflight$pairing, file.path(output, "mc_pairing_check.csv"), overwrite)
  plan <- preflight$selected_runs[, c(
    "run_id", "bau_run_dir", "ics_run_dir", "bau_baseline_file",
    "ics_baseline_file", "bau_end_file", "ics_end_file"
  )]
  plan$period_start_year <- preflight$period[1]
  plan$period_end_year <- preflight$period[2]
  plan$baseline_year <- preflight$baseline_year
  plan$baseline_year_code <- preflight$baseline_code
  plan$baseline_source <- preflight$baseline_source
  plan$baseline_timing <- preflight$baseline_timing
  plan$end_year_code <- preflight$end_code
  .v9_write_csv(plan, file.path(dirs$harvest, "plan_runs_and_files.csv"), overwrite)

  enduse <- .v9_build_enduse(preflight, dirs, overwrite)
  if (include_mc1) {
    .v9_write_raster(enduse$raster, file.path(dirs$summary_mc1, "delta_co2_enduse.tif"), overwrite)
  }

  harvest_state <- list(sum = NULL, sumsq = NULL, n = 0L)
  total_state <- list(sum = NULL, sumsq = NULL, n = 0L)
  harvest_rows <- list()
  total_rows <- list()

  for (j in seq_len(nrow(preflight$selected_runs))) {
    row <- preflight$selected_runs[j, ]
    run_id <- row$run_id
    bau_baseline <- terra::rast(row$bau_baseline_file)
    ics_baseline <- terra::rast(row$ics_baseline_file)
    bau_end <- terra::rast(row$bau_end_file)
    ics_end <- terra::rast(row$ics_end_file)
    if (!terra::compareGeom(bau_baseline, ics_baseline, bau_end, ics_end, stopOnError = FALSE)) {
      .v9_stop("AGB endpoint geometry changed after preflight for run ", run_id, ".")
    }

    # Exact v8 harvest-side cross-check: terminal CCTS-minus-BAU stock only.
    # This is retained for MC01 debugging, but it is not the post-spin-up
    # accounting result when a non-zero BAU/CCTS gap already exists at baseline.
    end_support <- !is.na(bau_end) & !is.na(ics_end)
    legacy_v8_delta_agb <- terra::ifel(end_support, ics_end - bau_end, NA)
    legacy_v8_delta_co2 <- legacy_v8_delta_agb * .V9_CO2_FACTOR
    legacy_v8_harvest_tCO2e <- .v9_global_sum(legacy_v8_delta_co2)

    common_support <- !is.na(bau_baseline) & !is.na(ics_baseline) &
      !is.na(bau_end) & !is.na(ics_end)
    baseline_gap_agb <- terra::ifel(common_support, ics_baseline - bau_baseline, NA)
    end_gap_agb <- terra::ifel(common_support, ics_end - bau_end, NA)
    delta_agb_period <- end_gap_agb - baseline_gap_agb
    delta_co2_period <- delta_agb_period * .V9_CO2_FACTOR
    baseline_gap_agb_total <- .v9_global_sum(baseline_gap_agb)
    end_gap_agb_total <- .v9_global_sum(end_gap_agb)
    sumco2 <- .v9_global_sum(delta_co2_period)
    algebraic <- (end_gap_agb_total - baseline_gap_agb_total) * .V9_CO2_FACTOR
    if (abs(sumco2 - algebraic) > .v9_tolerance(algebraic)) {
      .v9_stop("Harvest AGB gap-change reconciliation failed for run ", run_id, ".")
    }

    harvest_path <- file.path(
      dirs$harvest,
      sprintf("delta_co2_run%03d_%d-%d.tif", run_id, preflight$period[1], preflight$period[2])
    )
    delta_co2_written <- .v9_write_raster(delta_co2_period, harvest_path, overwrite)
    if (run_id == 1L) {
      .v9_write_raster(bau_baseline, file.path(dirs$harvest, "bau_agb_baseline_mc1.tif"), overwrite)
      .v9_write_raster(ics_baseline, file.path(dirs$harvest, "ics_agb_baseline_mc1.tif"), overwrite)
      .v9_write_raster(bau_end, file.path(dirs$harvest, "bau_agb_end_mc1.tif"), overwrite)
      .v9_write_raster(ics_end, file.path(dirs$harvest, "ics_agb_end_mc1.tif"), overwrite)
      .v9_write_raster(delta_agb_period, file.path(dirs$harvest, "delta_agb_period_mc1.tif"), overwrite)
      .v9_write_raster(delta_co2_period, file.path(dirs$harvest, "delta_co2_mc1.tif"), overwrite)
      .v9_write_raster(
        legacy_v8_delta_co2,
        file.path(dirs$summary_mc1, "delta_co2_harvest_mc01_v8_terminal_crosscheck.tif"),
        overwrite
      )
    }

    harvest_state <- .v9_accumulate(
      delta_co2_written,
      harvest_state,
      preflight$temp_dir,
      paste0(preflight$label, "_harvest")
    )

    harvest_projected <- terra::project(delta_co2_written, enduse$raster, method = "sum")
    projected_total <- .v9_global_sum(harvest_projected)
    projection_error <- projected_total - sumco2
    if (abs(projection_error) > .v9_tolerance(sumco2)) {
      .v9_stop("Harvest projection failed mass reconciliation for run ", run_id, ".")
    }
    projected_path <- file.path(dirs$total, sprintf("delta_co2_harvest_projected_run%03d.tif", run_id))
    harvest_projected <- .v9_write_raster(harvest_projected, projected_path, overwrite)
    total_raster <- .v9_sum_rasters_na(list(enduse$raster, harvest_projected))
    total_path <- file.path(dirs$total, sprintf("delta_co2_run%03d.tif", run_id))
    total_raster <- .v9_write_raster(total_raster, total_path, overwrite)
    total_scalar <- .v9_global_sum(total_raster)
    expected_total <- sumco2 + enduse$total_tCO2e
    if (abs(total_scalar - expected_total) > .v9_tolerance(expected_total)) {
      .v9_stop("Total raster/table reconciliation failed for run ", run_id, ".")
    }
    if (run_id == 1L) {
      .v9_write_raster(harvest_projected, file.path(dirs$summary_mc1, "delta_co2_harvest.tif"), overwrite)
      .v9_write_raster(total_raster, file.path(dirs$summary_mc1, "delta_co2.tif"), overwrite)
    }
    total_state <- .v9_accumulate(
      total_raster,
      total_state,
      preflight$temp_dir,
      paste0(preflight$label, "_total")
    )

    harvest_rows[[length(harvest_rows) + 1L]] <- tibble::tibble(
      run_id = run_id,
      mc01_nominal_debug_case = run_id == 1L,
      included_in_mc_batch = TRUE,
      period_start_year = preflight$period[1],
      period_end_year = preflight$period[2],
      baseline_year = preflight$baseline_year,
      baseline_year_code = preflight$baseline_code,
      baseline_source = preflight$baseline_source,
      baseline_timing = preflight$baseline_timing,
      end_year_code = preflight$end_code,
      sumco2_Mg = sumco2,
      legacy_v8_terminal_sumco2_Mg = legacy_v8_harvest_tCO2e,
      post_spinup_correction_vs_v8_Mg = sumco2 - legacy_v8_harvest_tCO2e,
      baseline_gap_agb_Mg = baseline_gap_agb_total,
      end_gap_agb_Mg = end_gap_agb_total,
      baseline_gap_co2_Mg = baseline_gap_agb_total * .V9_CO2_FACTOR,
      end_gap_co2_Mg = end_gap_agb_total * .V9_CO2_FACTOR,
      co2_factor = .V9_CO2_FACTOR,
      bau_baseline_file = row$bau_baseline_file,
      ics_baseline_file = row$ics_baseline_file,
      bau_end_file = row$bau_end_file,
      ics_end_file = row$ics_end_file,
      delta_co2_file = harvest_path
    )
    total_rows[[length(total_rows) + 1L]] <- tibble::tibble(
      label = preflight$label,
      run_id = run_id,
      mc01_nominal_debug_case = run_id == 1L,
      included_in_mc_batch = TRUE,
      period_start_year = preflight$period[1],
      period_end_year = preflight$period[2],
      harvest_tCO2e = sumco2,
      enduse_tCO2e = enduse$total_tCO2e,
      total_tCO2e = expected_total,
      legacy_v8_terminal_harvest_tCO2e = legacy_v8_harvest_tCO2e,
      legacy_v8_terminal_total_tCO2e = legacy_v8_harvest_tCO2e + enduse$total_tCO2e,
      post_spinup_correction_vs_v8_tCO2e = sumco2 - legacy_v8_harvest_tCO2e,
      projected_harvest_tCO2e = projected_total,
      projection_mass_error_tCO2e = projection_error,
      total_raster_tCO2e = total_scalar,
      total_raster_file = total_path
    )
  }

  harvest_table <- dplyr::bind_rows(harvest_rows)
  total_table <- dplyr::bind_rows(total_rows)
  .v9_write_csv(harvest_table, file.path(dirs$harvest, "per_run_sumco2.csv"), overwrite)
  .v9_write_csv(total_table, file.path(output, "total_by_run.csv"), overwrite)
  if (include_mc1) {
    mc01_harvest <- harvest_table[harvest_table$run_id == 1L, , drop = FALSE]
    mc01_total <- total_table[total_table$run_id == 1L, , drop = FALSE]
    if (nrow(mc01_harvest) != 1L || nrow(mc01_total) != 1L) {
      .v9_stop("Expected exactly one MC01 result for ", preflight$label, ".")
    }
    mc01_crosscheck <- tibble::tibble(
      label = preflight$label,
      run_id = 1L,
      mc01_role = "nominal_mean_debug_case_included_in_mc_batch",
      included_in_mc_batch = TRUE,
      period_start_year = preflight$period[1],
      period_end_year = preflight$period[2],
      baseline_year = preflight$baseline_year,
      baseline_year_code = preflight$baseline_code,
      end_year_code = preflight$end_code,
      primary_post_spinup_harvest_tCO2e = mc01_harvest$sumco2_Mg,
      v8_terminal_harvest_tCO2e = mc01_harvest$legacy_v8_terminal_sumco2_Mg,
      post_spinup_correction_vs_v8_tCO2e = mc01_harvest$post_spinup_correction_vs_v8_Mg,
      enduse_demand_tCO2e = enduse$total_tCO2e,
      primary_post_spinup_total_tCO2e = mc01_total$total_tCO2e,
      v8_terminal_total_tCO2e = mc01_total$legacy_v8_terminal_total_tCO2e,
      v8_crosscheck_formula = "(CCTS_end - BAU_end) * 0.47 * 44/12",
      primary_formula = "[(CCTS_end-BAU_end) - (CCTS_baseline-BAU_baseline)] * 0.47 * 44/12"
    )
    .v9_write_csv(
      mc01_crosscheck,
      file.path(dirs$summary_mc1, "mc01_v8_terminal_crosscheck.csv"),
      overwrite
    )
  }

  scalar_summary <- function(values, component) {
    all_configured_runs <- identical(
      preflight$run_ids,
      seq_len(preflight$bau_parameters$mc_runs)
    )
    tibble::tibble(
      component = component,
      runs = length(values),
      run_set = if (all_configured_runs) {
        "all_configured_MoFuSS_runs_including_nominal_MC01"
      } else {
        paste0("selected_MoFuSS_runs_", paste(preflight$run_ids, collapse = "_"))
      },
      all_configured_runs_included = all_configured_runs,
      includes_mc01 = 1L %in% preflight$run_ids,
      mc01_role = "nominal_mean_debug_case_included_in_batch",
      paired_inputs_validated = preflight$pairing_validated,
      uncertainty_status = if (preflight$pairing_validated) {
        "paired_run_ids_validated"
      } else {
        "PROVISIONAL_until_CCTS_is_rerun_with_shared_MC_inputs"
      },
      mean_tCO2e = mean(values),
      sd_tCO2e = if (length(values) >= 2L) stats::sd(values) else NA_real_,
      se_tCO2e = if (length(values) >= 2L) stats::sd(values) / sqrt(length(values)) else NA_real_,
      min_tCO2e = min(values),
      max_tCO2e = max(values)
    )
  }
  mc_summary <- dplyr::bind_rows(
    scalar_summary(harvest_table$sumco2_Mg, "harvest"),
    scalar_summary(rep(enduse$total_tCO2e, nrow(total_table)), "enduse_demand"),
    scalar_summary(total_table$total_tCO2e, "total")
  )
  .v9_write_csv(mc_summary, file.path(output, "summary_mc.csv"), overwrite)
  .v9_write_mc_rasters(
    harvest_state, dirs$harvest, "delta_co2", overwrite, harvest_table$sumco2_Mg
  )
  .v9_write_mc_rasters(
    total_state, dirs$total, "delta_co2", overwrite, total_table$total_tCO2e
  )
  diagnostic <- .v9_harvest_diagnostic(preflight, enduse, overwrite, dirs$harvest)

  manifest_row <- tibble::tibble(
    label = preflight$label,
    bau_dir = preflight$bau_dir,
    ics_dir = preflight$ics_dir,
    emissions_dir = preflight$emissions_dir,
    bau_scenario = preflight$bau_parameters$scenario,
    ics_scenario = preflight$ics_parameters$scenario,
    iso3 = preflight$bau_parameters$iso3,
    country = preflight$bau_parameters$country,
    byregion = preflight$bau_parameters$byregion,
    continent = preflight$bau_parameters$continent,
    region = preflight$bau_parameters$region,
    subcountry = preflight$bau_parameters$subcountry,
    gee_scale = preflight$bau_parameters$gee_scale,
    epsg_pcs = preflight$bau_parameters$epsg_pcs,
    uncapped_regrowth = preflight$bau_parameters$uncapped_regrowth,
    model_start_year = preflight$bau_parameters$model_start_year,
    model_end_year = preflight$bau_parameters$model_end_year,
    spinup_years_discarded = preflight$spinup_years,
    post_spinup_analysis_start_year = preflight$analysis_start_year,
    period_start_year = preflight$period[1],
    period_end_year = preflight$period[2],
    full_horizon = preflight$full_horizon,
    baseline_year = preflight$baseline_year,
    baseline_year_code = preflight$baseline_code,
    baseline_source = preflight$baseline_source,
    baseline_timing = preflight$baseline_timing,
    initial_agb_bau = preflight$initial_agb_bau,
    initial_agb_ics = preflight$initial_agb_ics,
    initial_agb_md5 = preflight$initial_agb_md5,
    end_year_code = preflight$end_code,
    selected_run_ids = paste(preflight$run_ids, collapse = ","),
    all_configured_runs_included = identical(
      preflight$run_ids,
      seq_len(preflight$bau_parameters$mc_runs)
    ),
    mc01_included_in_batch = 1L %in% preflight$run_ids,
    mc01_role = "nominal_mean_debug_case_included_in_batch",
    expected_mc_runs = preflight$bau_parameters$mc_runs,
    paired_run_id_design = TRUE,
    paired_mc_inputs_validated = preflight$pairing_validated,
    uncertainty_status = if (preflight$pairing_validated) {
      "paired_run_ids_validated"
    } else {
      "PROVISIONAL_until_CCTS_is_rerun_with_shared_MC_inputs"
    },
    cross_configuration_pooling = FALSE,
    enduse_basis = "demand",
    unmet_adjustment_applied = FALSE,
    co2_factor = .V9_CO2_FACTOR,
    charcoal_wood_to_fuel_ratio = preflight$bau_parameters$efchratio,
    efdb_path = preflight$efdb_path,
    efdb_ics_path = preflight$efdb_ics_path,
    manifest_path = preflight$manifest_path,
    manifest_md5 = preflight$manifest_md5,
    parameters_bau = preflight$bau_parameters$parameter_file,
    parameters_ics = preflight$ics_parameters$parameter_file,
    stage2_script = .v13_script_path(),
    terra_version = as.character(utils::packageVersion("terra")),
    r_version = R.version.string,
    completed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    status = "complete"
  )
  .v9_write_csv(manifest_row, file.path(output, "run_manifest.csv"), overwrite)
  message("[v13] Completed ", preflight$label, " -> ", output)
  invisible(list(
    manifest = manifest_row,
    harvest = harvest_table,
    enduse = enduse$post_table,
    total = total_table,
    diagnostic = diagnostic
  ))
}

run_emissions_manifest <- function(
  manifest = NULL,
  scenario_dirs = SCENARIO_DIRS,
  period = NULL,
  run_ids = NULL,
  config_label = NULL,
  temp_dir = tempdir(),
  dry_run = FALSE,
  overwrite = FALSE,
  enduse_basis = "demand"
) {
  .v9_require_packages()
  if (!identical(tolower(enduse_basis), "demand")) {
    .v9_stop("Only --enduse-basis=demand is implemented in v13.")
  }
  if (!is.null(period)) {
    period <- as.integer(period)
    if (length(period) != 2L || anyNA(period) || period[1] > period[2]) {
      .v9_stop("period must contain two increasing calendar years, or be NULL for auto.")
    }
  }
  dry_run <- .v9_parse_bool(dry_run, "dry-run")
  overwrite <- .v9_parse_bool(overwrite, "overwrite")
  if (!is.null(run_ids)) {
    original_run_ids <- run_ids
    run_ids <- suppressWarnings(as.integer(run_ids))
    if (
      !length(run_ids) || anyNA(run_ids) || any(run_ids < 1L) ||
      any(as.numeric(original_run_ids) != run_ids) || anyDuplicated(run_ids)
    ) {
      .v9_stop("run_ids must be unique positive integers, or NULL for all runs.")
    }
    run_ids <- sort(run_ids)
    if (!1L %in% run_ids) {
      .v9_stop("MC01 must remain in every selected MoFuSS batch in v13.")
    }
  }
  temp_dir <- .v9_norm_existing(temp_dir, "temp directory")
  if (!dir.exists(temp_dir) || file.access(temp_dir, 2L) != 0L) {
    .v9_stop("Temp directory is not writable: ", temp_dir)
  }

  required <- c("label", "bau_dir", "ics_dir", "emissions_dir")
  if (is.null(manifest)) {
    script_path <- .v13_script_path()
    manifest_path <- if (is.na(script_path)) {
      "embedded_SCENARIO_DIRS_in_stage2_v13"
    } else {
      script_path
    }
    manifest_md5 <- if (file.exists(manifest_path)) {
      unname(tools::md5sum(manifest_path))
    } else {
      NA_character_
    }
    manifest_table <- .v13_internal_pairs(scenario_dirs)
  } else {
    manifest_path <- .v9_norm_existing(manifest, "manifest")
    manifest_md5 <- unname(tools::md5sum(manifest_path))
    manifest_table <- suppressMessages(
      readr::read_csv(manifest_path, show_col_types = FALSE, progress = FALSE)
    )
  }
  if (!all(required %in% names(manifest_table))) {
    .v9_stop(
      "Resolved manifest lacks required fields: ",
      paste(setdiff(required, names(manifest_table)), collapse = ", ")
    )
  }
  manifest_table <- manifest_table[, required]
  manifest_table$label <- trimws(as.character(manifest_table$label))
  if (any(!nzchar(manifest_table$label)) || anyDuplicated(manifest_table$label)) {
    .v9_stop("Manifest labels must be non-empty and unique.")
  }
  if (!is.null(config_label)) {
    keep <- manifest_table$label == config_label
    if (sum(keep) != 1L) .v9_stop("--config-label did not match exactly one manifest row: ", config_label)
    manifest_table <- manifest_table[keep, ]
  }
  manifest_dir <- if (file.exists(manifest_path)) dirname(manifest_path) else getwd()
  for (column in c("bau_dir", "ics_dir", "emissions_dir")) {
    manifest_table[[column]] <- vapply(
      manifest_table[[column]], .v9_resolve_manifest_path,
      character(1), manifest_dir = manifest_dir
    )
  }
  output_keys <- vapply(manifest_table$emissions_dir, function(x) .v9_path_key(.v9_norm_output(x)), character(1))
  if (anyDuplicated(output_keys)) .v9_stop("Each manifest row must use a unique emissions_dir.")
  if (length(output_keys) > 1L) {
    for (i in seq_along(output_keys)) for (j in seq_along(output_keys)) {
      if (i != j && .v9_is_within(output_keys[i], output_keys[j])) {
        .v9_stop("Manifest emissions_dir values may not contain one another.")
      }
    }
  }

  preflights <- lapply(seq_len(nrow(manifest_table)), function(i) {
    .v9_preflight_config(
      manifest_table[i, ], period, run_ids, temp_dir, overwrite
    )
  })
  preflights <- lapply(preflights, function(preflight) {
    preflight$manifest_path <- manifest_path
    preflight$manifest_md5 <- manifest_md5
    preflight
  })
  names(preflights) <- vapply(preflights, `[[`, character(1), "label")

  if (dry_run) {
    for (preflight in preflights) {
      message(
        "[v13 dry-run] OK label=", preflight$label,
        " | BAU=", preflight$bau_parameters$scenario,
        " | ICS=", preflight$ics_parameters$scenario,
        " | regrowth=", if (preflight$bau_parameters$uncapped_regrowth == 1L) "uncapped" else "capped",
        " | period=", paste(preflight$period, collapse = ":"),
        " | baseline=", preflight$baseline_source,
        " (code ", preflight$baseline_code, ")->", preflight$end_code,
        " | runs=", paste(preflight$run_ids, collapse = ","),
        " | MC01 included=", 1L %in% preflight$run_ids,
        " | MC pairing validated=", preflight$pairing_validated,
        " | output=", preflight$emissions_dir
      )
    }
    return(invisible(preflights))
  }

  results <- lapply(preflights, .v9_process_config, overwrite = overwrite)
  invisible(results)
}

.v9_main <- function() {
  .v9_require_packages()
  options <- .v9_parse_cli()
  if (isTRUE(options$help)) {
    .v9_usage()
    return(invisible(NULL))
  }
  period <- .v9_parse_period(options$period)
  run_ids <- .v9_parse_run_ids(options$run_ids)
  run_emissions_manifest(
    manifest = options$manifest,
    scenario_dirs = SCENARIO_DIRS,
    period = period,
    run_ids = run_ids,
    config_label = options$config_label,
    temp_dir = options$temp_dir,
    dry_run = options$dry_run,
    overwrite = options$overwrite,
    enduse_basis = options$enduse_basis
  )
}

if (sys.nframe() == 0L || interactive()) {
  .v9_main()
}
