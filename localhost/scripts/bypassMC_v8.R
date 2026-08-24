# MoFuSS Monte Carlo bypass
# Version 2
# Date: Aug 2026
# EGOML dependency bundle: V8
#
# Purpose
# -------
# Reuse the Monte Carlo tables from a matching BAU scenario when starting a
# CCTS/ICS simulation in a separate working directory. The MC tables are
# generated before the dynamic simulations, so BAU dynamics do not need to
# finish before CCTS starts. BAU completion is recorded for provenance only.
#
# Source resolution order
# -----------------------
# 1. BAU_MC_DIR=<absolute or relative path> supplied by Dinamica/R.
# 2. A one-line bau_mc_source.txt in the current scenario root or in
#    LULCC/TempTables.
# 3. Automatic discovery of one matching BAU sibling directory.
#
# The script deliberately does not copy or control Dinamica's internal Patcher
# random-number stream. It pairs the Monte Carlo parameter tables only.

options(stringsAsFactors = FALSE)

required_mc_files <- c(
  "i_st_all.csv",
  "k_all.csv",
  "rmax_all.csv",
  "Harvest_pixels_V.csv",
  "Harvest_pixels_W.csv",
  "Prune_factor_V.csv",
  "Prune_factor_W.csv"
)
mc_batch_ready_filename <- "mc_batch_ready.csv"

stopf <- function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)

trim_quotes <- function(x) {
  x <- trimws(x)
  if (nchar(x) >= 2L) {
    first <- substr(x, 1L, 1L)
    last <- substr(x, nchar(x), nchar(x))
    if ((first == "\"" && last == "\"") || (first == "'" && last == "'")) {
      x <- substr(x, 2L, nchar(x) - 1L)
    }
  }
  x
}

parse_named_args <- function(x) {
  out <- list()
  for (token in x) {
    pos <- regexpr("=", token, fixed = TRUE)[1]
    if (pos < 2L) stopf("Invalid argument (expected name=value): %s", token)
    key <- substr(token, 1L, pos - 1L)
    value <- substr(token, pos + 1L, nchar(token))
    if (!grepl("^[A-Za-z][A-Za-z0-9_.]*$", key)) {
      stopf("Invalid argument name: %s", key)
    }
    out[[key]] <- trim_quotes(value)
  }
  out
}

arg_text <- function(args, key, default = NULL) {
  value <- args[[key]]
  if (is.null(value) || !nzchar(trimws(value))) default else trimws(value)
}

arg_int <- function(args, key, default = NA_integer_) {
  value <- arg_text(args, key, NULL)
  if (is.null(value)) return(default)
  parsed <- suppressWarnings(as.integer(value))
  if (length(parsed) != 1L || is.na(parsed)) stopf("%s must be an integer.", key)
  parsed
}

arg_bool <- function(args, key, default = FALSE) {
  value <- tolower(arg_text(args, key, if (default) "true" else "false"))
  if (value %in% c("1", "true", "yes", ".yes")) return(TRUE)
  if (value %in% c("0", "false", "no", ".no")) return(FALSE)
  stopf("%s must be true/false or 1/0.", key)
}

norm_dir <- function(path, must_work = TRUE, base = getwd()) {
  if (!grepl("^(?:[A-Za-z]:[/\\\\]|/)", path)) path <- file.path(base, path)
  normalizePath(path, winslash = "/", mustWork = must_work)
}

find_parameters_file <- function(root) {
  downloaded <- file.path(root, "LULCC", "DownloadedDatasets")
  if (!dir.exists(downloaded)) return(character())
  hits <- list.files(
    downloaded,
    pattern = "^parameters.*\\.csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  hits <- hits[file.info(hits)$isdir %in% FALSE]
  normalizePath(hits, winslash = "/", mustWork = TRUE)
}

read_scenario_metadata <- function(root, quiet = FALSE) {
  files <- find_parameters_file(root)
  if (length(files) != 1L) {
    if (quiet) return(NULL)
    stopf("Expected exactly one parameters*.csv below %s; found %d.", root, length(files))
  }
  first_line <- readLines(files, n = 1L, warn = FALSE)
  sep <- if (grepl(";", first_line, fixed = TRUE)) ";" else ","
  tab <- read.csv(files, sep = sep, check.names = FALSE, stringsAsFactors = FALSE)
  if (!all(c("Var", "ParCHR") %in% names(tab))) {
    if (quiet) return(NULL)
    stopf("Parameter table lacks Var/ParCHR columns: %s", files)
  }
  tab$Var <- trimws(as.character(tab$Var))
  tab$ParCHR <- trimws(as.character(tab$ParCHR))
  tab <- tab[nzchar(tab$Var), c("Var", "ParCHR"), drop = FALSE]
  duplicated_keys <- unique(tab$Var[duplicated(tab$Var)])
  if (length(duplicated_keys)) {
    if (quiet) return(NULL)
    stopf("Duplicate parameter key(s) in %s: %s", files, paste(duplicated_keys, collapse = ", "))
  }
  values <- setNames(tab$ParCHR, tab$Var)
  getv <- function(key, required = TRUE) {
    value <- unname(values[key])
    if (!length(value) || is.na(value) || !nzchar(value)) {
      if (required) stopf("Missing parameter '%s' in %s", key, files)
      return(NA_character_)
    }
    value
  }
  byregion <- getv("byregion")
  geo_key <- if (tolower(byregion) == "regional") {
    "region2BprocessedReg"
  } else if (tolower(byregion) == "country") {
    "region2BprocessedCtry_iso"
  } else {
    stopf("Unsupported byregion='%s' in %s", byregion, files)
  }
  scenario_ver <- getv("scenario_ver")
  role <- if (grepl("^bau", scenario_ver, ignore.case = TRUE)) {
    "BAU"
  } else if (grepl("^(ics|ccts)", scenario_ver, ignore.case = TRUE)) {
    "CCTS"
  } else {
    "UNKNOWN"
  }
  as_int <- function(key) {
    value <- suppressWarnings(as.integer(getv(key)))
    if (is.na(value)) stopf("Parameter '%s' is not an integer in %s", key, files)
    value
  }
  list(
    root = norm_dir(root),
    parameters_file = files,
    scenario_ver = scenario_ver,
    role = role,
    byregion = byregion,
    geography_key = geo_key,
    geography = getv(geo_key),
    start_year = as_int("start_year"),
    end_year = as_int("end_year"),
    monte_carlo_runs = as_int("monte_carlo_runs"),
    uncapped_regrowth = as_int("uncapped_regrowth"),
    gee_scale = getv("GEE_scale", required = FALSE)
  )
}

same_text <- function(a, b) identical(tolower(trimws(as.character(a))), tolower(trimws(as.character(b))))

assert_matching_pair <- function(bau, ccts) {
  if (bau$role != "BAU") stopf("MC source is not a BAU scenario: %s (%s)", bau$root, bau$scenario_ver)
  if (ccts$role != "CCTS") stopf("Current directory is not an ICS/CCTS scenario: %s (%s)", ccts$root, ccts$scenario_ver)
  fields <- c(
    "byregion", "geography", "start_year", "end_year",
    "monte_carlo_runs", "uncapped_regrowth", "gee_scale"
  )
  bad <- fields[!vapply(fields, function(field) same_text(bau[[field]], ccts[[field]]), logical(1))]
  if (length(bad)) {
    details <- vapply(
      bad,
      function(field) sprintf("%s: BAU='%s', CCTS='%s'", field, bau[[field]], ccts[[field]]),
      character(1)
    )
    stopf("BAU/CCTS pairing metadata mismatch:\n  %s", paste(details, collapse = "\n  "))
  }
}

source_from_link_file <- function(current_root) {
  candidates <- c(
    file.path(current_root, "bau_mc_source.txt"),
    file.path(current_root, "LULCC", "TempTables", "bau_mc_source.txt")
  )
  candidates <- candidates[file.exists(candidates)]
  if (!length(candidates)) return(NULL)
  if (length(candidates) > 1L) stopf("Multiple bau_mc_source.txt files found; retain only one.")
  lines <- trimws(readLines(candidates, warn = FALSE))
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
  if (length(lines) != 1L) stopf("%s must contain exactly one non-comment path.", candidates)
  norm_dir(lines, base = dirname(candidates))
}

discover_matching_bau <- function(current, parent = dirname(current$root)) {
  siblings <- list.dirs(parent, recursive = FALSE, full.names = TRUE)
  siblings <- siblings[normalizePath(siblings, winslash = "/", mustWork = TRUE) != current$root]
  metadata <- lapply(siblings, function(path) {
    tryCatch(read_scenario_metadata(path, quiet = TRUE), error = function(e) NULL)
  })
  candidates <- Filter(Negate(is.null), metadata)
  candidates <- Filter(function(x) {
    x$role == "BAU" &&
      same_text(x$byregion, current$byregion) &&
      same_text(x$geography, current$geography) &&
      same_text(x$start_year, current$start_year) &&
      same_text(x$end_year, current$end_year) &&
      same_text(x$monte_carlo_runs, current$monte_carlo_runs) &&
      same_text(x$uncapped_regrowth, current$uncapped_regrowth) &&
      same_text(x$gee_scale, current$gee_scale)
  }, candidates)
  if (length(candidates) != 1L) {
    found <- if (length(candidates)) paste(vapply(candidates, `[[`, character(1), "root"), collapse = ", ") else "none"
    stopf(
      paste0(
        "Automatic BAU discovery for %s found %d matching sibling(s): %s. ",
        "Supply BAU_MC_DIR or create bau_mc_source.txt."
      ),
      current$root, length(candidates), found
    )
  }
  candidates[[1L]]
}

read_mc_csv <- function(path) {
  tryCatch(
    read.csv(path, check.names = FALSE, stringsAsFactors = FALSE),
    error = function(e) stopf("Cannot read %s: %s", path, conditionMessage(e))
  )
}

assert_mc_table <- function(path, expected_mc, kind = c("wide", "lookup")) {
  kind <- match.arg(kind)
  tab <- read_mc_csv(path)
  if (nrow(tab) != expected_mc) {
    stopf("%s has %d rows; expected %d.", path, nrow(tab), expected_mc)
  }
  if (ncol(tab) < 2L) stopf("%s must have at least two columns.", path)
  key <- suppressWarnings(as.integer(tab[[1L]]))
  if (!identical(key, seq_len(expected_mc))) {
    stopf("First column of %s must contain run keys 1..%d.", path, expected_mc)
  }
  numeric_part <- lapply(tab[-1L], function(x) suppressWarnings(as.numeric(x)))
  if (any(!vapply(numeric_part, function(x) length(x) == expected_mc && all(is.finite(x)), logical(1)))) {
    stopf("%s contains non-numeric or non-finite MC values.", path)
  }
  if (kind == "lookup" && ncol(tab) != 2L) {
    stopf("Lookup table %s must have exactly two columns.", path)
  }
  invisible(tab)
}

md5 <- function(paths) unname(tools::md5sum(paths))

read_mc_batch_ready <- function(
  bau, selected_files, luc_version, agb_version
) {
  path <- file.path(bau$root, "Temp", mc_batch_ready_filename)
  if (!file.exists(path)) {
    stopf(
      paste0(
        "BAU has no atomic current-batch readiness manifest: %s. ",
        "Finish rnorm_v8.R for the current BAU batch before starting CCTS."
      ),
      path
    )
  }
  manifest_hash <- md5(path)
  tab <- read_mc_csv(path)
  required_columns <- c(
    "schema_version", "status", "batch_id", "created_utc",
    "generated_by", "script_bundle", "scenario_dir", "scenario_ver",
    "byregion", "geography", "start_year", "end_year",
    "monte_carlo_runs", "uncapped_regrowth", "lulc_version",
    "agb_version", "file", "file_size_bytes", "md5"
  )
  missing_columns <- setdiff(required_columns, names(tab))
  if (length(missing_columns)) {
    stopf(
      "BAU MC batch-ready manifest lacks column(s): %s",
      paste(missing_columns, collapse = ", ")
    )
  }
  scalar <- function(column) {
    values <- unique(trimws(as.character(tab[[column]])))
    values <- values[!is.na(values) & nzchar(values)]
    if (length(values) != 1L) {
      stopf("BAU MC batch-ready manifest has inconsistent '%s' values.", column)
    }
    values[[1L]]
  }
  scalar_int <- function(column) {
    value <- suppressWarnings(as.integer(scalar(column)))
    if (is.na(value)) stopf("BAU MC batch-ready '%s' must be an integer.", column)
    value
  }

  if (scalar_int("schema_version") != 1L ||
      !same_text(scalar("status"), "ready") ||
      !same_text(scalar("generated_by"), "rnorm_v8.R") ||
      !same_text(scalar("script_bundle"), "V8")) {
    stopf("BAU MC batch-ready manifest is not a supported ready V8 batch: %s", path)
  }
  recorded_root <- norm_dir(scalar("scenario_dir"), must_work = FALSE)
  if (!identical(recorded_root, bau$root)) {
    stopf(
      "BAU MC batch-ready manifest belongs to a different scenario directory: %s",
      scalar("scenario_dir")
    )
  }
  checks <- list(
    scenario_ver = bau$scenario_ver,
    byregion = bau$byregion,
    geography = bau$geography,
    start_year = bau$start_year,
    end_year = bau$end_year,
    monte_carlo_runs = bau$monte_carlo_runs,
    uncapped_regrowth = bau$uncapped_regrowth,
    lulc_version = luc_version,
    agb_version = agb_version
  )
  bad <- names(checks)[!vapply(
    names(checks),
    function(column) same_text(scalar(column), checks[[column]]),
    logical(1)
  )]
  if (length(bad)) {
    details <- vapply(
      bad,
      function(column) sprintf(
        "%s: manifest='%s', BAU/current='%s'",
        column, scalar(column), checks[[column]]
      ),
      character(1)
    )
    stopf("BAU MC batch-ready metadata mismatch:\n  %s", paste(details, collapse = "\n  "))
  }

  files <- as.character(tab$file)
  if (anyDuplicated(files) || !identical(sort(files), sort(selected_files))) {
    stopf(
      "BAU MC batch-ready file inventory differs from the required batch: %s",
      path
    )
  }
  tab <- tab[match(selected_files, files), , drop = FALSE]
  expected_sizes <- suppressWarnings(as.numeric(tab$file_size_bytes))
  expected_hashes <- tolower(trimws(as.character(tab$md5)))
  if (anyNA(expected_sizes) || any(expected_sizes <= 0) ||
      any(!grepl("^[0-9a-f]{32}$", expected_hashes))) {
    stopf("BAU MC batch-ready manifest contains invalid sizes or MD5 hashes: %s", path)
  }
  source_paths <- file.path(bau$root, "Temp", selected_files)
  if (!all(file.exists(source_paths))) {
    missing <- selected_files[!file.exists(source_paths)]
    stopf("Ready BAU batch is missing file(s): %s", paste(missing, collapse = ", "))
  }
  actual_sizes <- as.numeric(file.info(source_paths)$size)
  actual_hashes <- md5(source_paths)
  if (!identical(actual_sizes, expected_sizes) ||
      !identical(actual_hashes, expected_hashes)) {
    stopf(
      paste0(
        "BAU MC tables changed after the current-batch manifest was published. ",
        "Do not start CCTS; rerun rnorm_v8.R to publish one complete current batch."
      )
    )
  }
  list(
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    manifest_md5 = manifest_hash,
    batch_id = scalar("batch_id"),
    created_utc = scalar("created_utc"),
    files = selected_files,
    sizes = expected_sizes,
    hashes = expected_hashes,
    rows = tab
  )
}

assert_static_match <- function(bau_root, ccts_root, relative_path, required = TRUE) {
  bau_path <- file.path(bau_root, relative_path)
  ccts_path <- file.path(ccts_root, relative_path)
  present <- file.exists(c(bau_path, ccts_path))
  if (!all(present)) {
    if (required) stopf("Missing paired static input: %s", relative_path)
    return(invisible(FALSE))
  }
  hashes <- md5(c(bau_path, ccts_path))
  if (!identical(hashes[1L], hashes[2L])) {
    stopf("BAU/CCTS static input differs: %s", relative_path)
  }
  invisible(TRUE)
}

discover_debugging_runs <- function(root) {
  dirs <- list.dirs(root, recursive = FALSE, full.names = TRUE)
  names <- basename(dirs)
  keep <- grepl("^debugging_[0-9]+$", names, ignore.case = TRUE)
  ids <- suppressWarnings(as.integer(sub("^debugging_", "", names[keep], ignore.case = TRUE)))
  setNames(dirs[keep], ids)
}

inspect_bau_completion <- function(bau, expected_mc) {
  runs <- discover_debugging_runs(bau$root)
  expected <- seq_len(expected_mc)
  end_code <- bau$end_year - bau$start_year + 1L
  complete <- integer()
  for (id in expected) {
    run_dir <- unname(runs[as.character(id)])
    if (!length(run_dir) || is.na(run_dir) || !dir.exists(run_dir)) next
    files <- list.files(run_dir, full.names = FALSE)
    pattern <- sprintf("^Growth_less_harv0*%d(?:\\.[^.]+)?$", end_code)
    if (any(grepl(pattern, files, ignore.case = TRUE, perl = TRUE))) {
      complete <- c(complete, id)
    }
  }
  list(
    end_code = end_code,
    completed_run_ids = complete,
    incomplete_run_ids = setdiff(expected, complete),
    all_complete = identical(complete, expected)
  )
}

write_scalar_csv <- function(value, path) {
  write.csv(data.frame(x = value), path)
}

safe_remove_children <- function(root, relative_dir) {
  target <- normalizePath(file.path(root, relative_dir), winslash = "/", mustWork = FALSE)
  prefix <- paste0(root, "/")
  if (!startsWith(target, prefix) || identical(target, root)) stopf("Unsafe cleanup target: %s", target)
  if (!dir.exists(target)) return(invisible(TRUE))
  children <- list.files(target, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  if (length(children)) unlink(children, recursive = TRUE, force = TRUE)
  remaining <- list.files(target, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  if (length(remaining)) stopf("Could not clear %s", target)
  invisible(TRUE)
}

prepare_stage <- function(
  bau, ccts, selected_files, patcher_bypassed, bau_completion, batch_ready
) {
  stage <- tempfile(pattern = ".bypassMC_stage_", tmpdir = ccts$root)
  if (!dir.create(stage, recursive = FALSE)) stopf("Cannot create staging directory: %s", stage)
  ok <- FALSE
  on.exit(if (!ok && dir.exists(stage)) unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)

  source_paths <- file.path(bau$root, "Temp", selected_files)
  if (!identical(md5(source_paths), batch_ready$hashes)) {
    stopf("BAU MC batch changed before staging began; CCTS was not modified.")
  }
  copied <- file.copy(source_paths, stage, overwrite = FALSE, copy.date = TRUE)
  if (!all(copied)) stopf("Failed to stage one or more BAU MC files.")
  copied_ready <- file.copy(
    batch_ready$path,
    file.path(stage, mc_batch_ready_filename),
    overwrite = FALSE,
    copy.date = TRUE
  )
  if (!copied_ready) stopf("Failed to stage the BAU MC batch-ready manifest.")
  staged_paths <- file.path(stage, selected_files)
  source_hash <- md5(source_paths)
  staged_hash <- md5(staged_paths)
  if (!identical(staged_hash, batch_ready$hashes) ||
      !identical(source_hash, batch_ready$hashes)) {
    stopf("Staged MC file hash verification against the ready batch failed.")
  }
  if (!identical(md5(batch_ready$path), batch_ready$manifest_md5) ||
      !identical(md5(file.path(stage, mc_batch_ready_filename)), batch_ready$manifest_md5)) {
    stopf("BAU MC batch-ready manifest changed during staging.")
  }

  k <- read_mc_csv(file.path(stage, "k_all.csv"))
  k_values <- as.data.frame(lapply(k[-1L], as.numeric), check.names = FALSE)
  write_scalar_csv(max(as.matrix(k_values), na.rm = TRUE), file.path(stage, "MaxAGB.csv"))
  write_scalar_csv(max(as.numeric(k_values[1L, ]), na.rm = TRUE), file.path(stage, "MaxAGB_firstMC.csv"))
  write_scalar_csv(max(as.numeric(k_values[nrow(k_values), ]), na.rm = TRUE), file.path(stage, "MaxAGB_lastMC.csv"))

  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  file_manifest <- data.frame(
    file = selected_files,
    source_path = normalizePath(source_paths, winslash = "/", mustWork = TRUE),
    source_md5 = source_hash,
    copied_md5 = staged_hash,
    bau_mc_batch_id = batch_ready$batch_id,
    stringsAsFactors = FALSE
  )
  write.csv(file_manifest, file.path(stage, "mc_bypass_file_manifest.csv"), row.names = FALSE)
  run_manifest <- data.frame(
    status = "complete",
    created_at = now,
    mode = "reuse_BAU_MC_tables",
    current_scenario_dir = ccts$root,
    current_scenario_ver = ccts$scenario_ver,
    bau_source_dir = bau$root,
    bau_scenario_ver = bau$scenario_ver,
    bau_mc_batch_id = batch_ready$batch_id,
    bau_mc_batch_created_utc = batch_ready$created_utc,
    bau_mc_batch_manifest = batch_ready$path,
    bau_mc_batch_manifest_md5 = batch_ready$manifest_md5,
    geography = ccts$geography,
    start_year = ccts$start_year,
    end_year = ccts$end_year,
    monte_carlo_runs = ccts$monte_carlo_runs,
    uncapped_regrowth = ccts$uncapped_regrowth,
    bau_dynamics_complete = bau_completion$all_complete,
    bau_completed_run_count = length(bau_completion$completed_run_ids),
    bau_completed_run_ids = paste(bau_completion$completed_run_ids, collapse = ";"),
    patcher_bypassed = patcher_bypassed,
    patcher_rng_paired = FALSE,
    stringsAsFactors = FALSE
  )
  write.csv(run_manifest, file.path(stage, "mc_bypass_manifest.csv"), row.names = FALSE)
  ok <- TRUE
  stage
}

install_stage <- function(stage, ccts) {
  lock <- file.path(ccts$root, ".bypassMC.lock")
  if (!dir.create(lock, recursive = FALSE)) {
    stopf("Another bypassMC run may be active, or a stale lock exists: %s", lock)
  }
  on.exit(if (dir.exists(lock)) unlink(lock, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(
    c(sprintf("pid=%d", Sys.getpid()), sprintf("started=%s", Sys.time())),
    file.path(lock, "owner.txt")
  )

  old_temp <- file.path(ccts$root, "Temp")
  backup <- tempfile(pattern = ".Temp_before_bypass_", tmpdir = ccts$root)
  moved_old <- FALSE
  if (dir.exists(old_temp)) {
    moved_old <- file.rename(old_temp, backup)
    if (!moved_old) stopf("Cannot move existing Temp directory aside safely.")
  }
  installed <- file.rename(stage, old_temp)
  if (!installed) {
    if (moved_old) file.rename(backup, old_temp)
    stopf("Cannot install staged Monte Carlo Temp directory.")
  }
  if (moved_old && dir.exists(backup)) unlink(backup, recursive = TRUE, force = TRUE)

  safe_remove_children(ccts$root, "Out")
  safe_remove_children(ccts$root, "HTML_animation")
  if (dir.exists(file.path(ccts$root, "Debugging"))) {
    unlink(file.path(ccts$root, "Debugging"), recursive = TRUE, force = TRUE)
  }
  dir.create(file.path(ccts$root, "Debugging"), showWarnings = FALSE)

  old_runs <- discover_debugging_runs(ccts$root)
  if (length(old_runs)) unlink(unname(old_runs), recursive = TRUE, force = TRUE)
  for (id in seq_len(ccts$monte_carlo_runs)) {
    path <- file.path(ccts$root, paste0("debugging_", id))
    if (!dir.create(path)) stopf("Cannot create %s", path)
  }

  stale_files <- c(
    "LaTeX/InputPara.csv", "LaTeX/NRBTable.csv", "LaTeX/fNRBTable.csv",
    "LaTeX/SumTable.csv", "LaTeX/SumTableBaU.csv", "LaTeX/SumTableICS.csv",
    "LaTeX/Growth_Harvest_AniOutBaU.mp4", "LaTeX/Growth_Harvest_AniOutICS.mp4"
  )
  stale_paths <- file.path(ccts$root, stale_files)
  unlink(stale_paths[file.exists(stale_paths)], force = TRUE)
  invisible(TRUE)
}

main <- function() {
  args <- parse_named_args(commandArgs(trailingOnly = TRUE))
  current_root <- norm_dir(arg_text(args, "CurrentDir", getwd()))
  current <- read_scenario_metadata(current_root)
  if (current$role != "CCTS") {
    stopf(
      "bypassMC_v8 is only valid inside an ICS/CCTS scenario; found %s in %s.",
      current$scenario_ver, current_root
    )
  }
  if (arg_bool(args, "RerunMC", FALSE)) {
  stopf("bypassMC_v8 received RerunMC=true; BAU regeneration must call rnorm_v8.R.")
  }

  requested_mc <- arg_int(args, "MC", current$monte_carlo_runs)
  if (requested_mc != current$monte_carlo_runs) {
    stopf("EGOML MC=%d differs from parameters.csv monte_carlo_runs=%d.", requested_mc, current$monte_carlo_runs)
  }
  requested_it <- arg_int(args, "IT", current$start_year)
  if (requested_it != current$start_year) {
    stopf("EGOML IT=%d differs from parameters.csv start_year=%d.", requested_it, current$start_year)
  }
  requested_st <- arg_int(args, "STdyn", current$end_year - current$start_year)
  if (requested_st != current$end_year - current$start_year) {
    stopf("EGOML STdyn=%d is inconsistent with parameters.csv years.", requested_st)
  }
  patcher_bypassed <- arg_bool(args, "PatcherBypassed", TRUE)

  explicit_source <- arg_text(args, "BAU_MC_DIR", NULL)
  if (!is.null(explicit_source) && toupper(explicit_source) != "AUTO") {
    bau <- read_scenario_metadata(norm_dir(explicit_source, base = current_root))
    source_method <- "BAU_MC_DIR"
  } else {
    linked <- source_from_link_file(current_root)
    if (!is.null(linked)) {
      bau <- read_scenario_metadata(linked)
      source_method <- "bau_mc_source.txt"
    } else {
      bau <- discover_matching_bau(current)
      source_method <- "matching_sibling"
    }
  }
  assert_matching_pair(bau, current)

  luc_version <- arg_int(args, "LUCmap_v", 1L)
  agb_version <- arg_int(args, "AGBmap_v", 3L)
  static_inputs <- c(
    sprintf("LULCC/TempTables/growth_parameters%d.csv", luc_version),
    sprintf("LULCC/TempRaster/LULCt%d_c.tif", luc_version),
    sprintf("LULCC/TempRaster/agb%d_c.tif", agb_version),
    "LULCC/TempRaster/Mask_c.tif"
  )
  for (path in static_inputs) assert_static_match(bau$root, current$root, path)

  source_temp <- file.path(bau$root, "Temp")
  source_paths <- file.path(source_temp, required_mc_files)
  missing <- required_mc_files[!file.exists(source_paths)]
  if (length(missing)) stopf("BAU Temp is missing MC file(s): %s", paste(missing, collapse = ", "))

  wide <- c("i_st_all.csv", "k_all.csv", "rmax_all.csv")
  for (name in wide) assert_mc_table(file.path(source_temp, name), requested_mc, "wide")
  for (name in setdiff(required_mc_files, wide)) {
    assert_mc_table(file.path(source_temp, name), requested_mc, "lookup")
  }
  category_name <- sprintf("LULC_Categories%d.csv", luc_version)
  category_path <- file.path(source_temp, category_name)
  if (!file.exists(category_path)) stopf("BAU Temp is missing %s.", category_name)
  selected_files <- c(required_mc_files, category_name)
  batch_ready <- read_mc_batch_ready(
    bau, selected_files, luc_version = luc_version, agb_version = agb_version
  )
  bau_completion <- inspect_bau_completion(bau, requested_mc)

  cat(sprintf("[OK] CCTS scenario: %s\n", current$root))
  cat(sprintf("[OK] Matching BAU:  %s (%s)\n", bau$root, source_method))
  cat(sprintf("[OK] Pair identity: %s, %d-%d, MC=%d, uncapped_regrowth=%d\n",
              current$geography, current$start_year, current$end_year,
              current$monte_carlo_runs, current$uncapped_regrowth))
  cat("[OK] Seven Dinamica MC tables and the LULC category table passed validation.\n")
  cat(sprintf(
    "[OK] Atomic current BAU MC batch verified: %s (%s).\n",
    batch_ready$batch_id, batch_ready$created_utc
  ))
  cat(sprintf("[OK] Patcher bypass requested by EGOML: %s\n", patcher_bypassed))
  if (!bau_completion$all_complete) {
    warning(
      sprintf(
        paste0(
          "BAU MC tables are ready, but BAU dynamics are still incomplete: ",
          "%d/%d endpoint runs found; waiting on run(s) %s. ",
          "CCTS may start now, but paired emissions require both scenarios to finish."
        ),
        length(bau_completion$completed_run_ids), requested_mc,
        paste(bau_completion$incomplete_run_ids, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (arg_bool(args, "DryRun", FALSE)) {
    cat("[DRY-RUN] No files or folders were changed.\n")
    return(invisible(TRUE))
  }

  stage <- prepare_stage(
    bau, current, selected_files,
    patcher_bypassed, bau_completion, batch_ready
  )
  install_stage(stage, current)
  installed <- file.path(current$root, "Temp", selected_files)
  if (!identical(batch_ready$hashes, md5(installed)) ||
      !identical(
        batch_ready$manifest_md5,
        md5(file.path(current$root, "Temp", mc_batch_ready_filename))
      )) {
    stopf("Post-install MC table hash verification failed.")
  }
  cat("[OK] BAU Monte Carlo tables installed in CCTS Temp with verified hashes.\n")
  if (patcher_bypassed) {
    cat("[OK] Dinamica harvest-allocation Patchers are bypassed; their stochastic locations are not used.\n")
  } else {
    cat("[NOTE] Dinamica Patcher random choices are not paired by this table bypass.\n")
  }
  invisible(TRUE)
}

tryCatch(
  main(),
  error = function(e) {
    message("ERROR: ", conditionMessage(e))
    quit(save = "no", status = 1L, runLast = FALSE)
  }
)
