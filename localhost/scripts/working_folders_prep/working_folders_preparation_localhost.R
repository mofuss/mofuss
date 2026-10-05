# SPDX-License-Identifier: Apache-2.0
#
# Prepare four MoFuSS localhost working folders from one 1000 m seed folder:
#   BaU1 capped, BaU1 uncapped, ICS3 capped, and ICS3 uncapped.
#
# Normal use (RStudio):
#   1. Open this file in RStudio and click Source.
#   2. Select a BaU1 capped parameters.csv when prompted.
#   3. Select the parent folder containing _1000m_ and where the four
#      working folders should be created.
#   4. Review the proposed folders and confirm.
#
# Command-line use:
#   Rscript working_folders_preparation_localhost.R --parameters="path/to/parameters.csv" --output-dir="path/to/output"
#
# Optional command-line flags:
#   --template="path/to/_1000m_" Override seed discovery within output-dir.
#   --dry-run                      Validate and print the plan without copying.
#   --yes                          Skip the final confirmation prompt.
# Use E:/... paths on Windows or /mnt/... paths on Linux.

# BEGIN USER INPUTS ----------------------------------------------------------
# Supply --parameters, --template and --output-dir, or use the interactive
# folder/file selectors when sourcing this script. All data come from the seed.
# END USER INPUTS ------------------------------------------------------------

options(stringsAsFactors = FALSE)

stopf <- function(fmt, ...) {
  stop(sprintf(fmt, ...), call. = FALSE)
}

normalize_existing <- function(path, label) {
  if (!length(path) || is.na(path) || !nzchar(trimws(path))) {
    stopf("%s was not provided.", label)
  }
  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!file.exists(normalized) && !dir.exists(normalized)) {
    stopf("%s does not exist: %s", label, normalized)
  }
  normalizePath(normalized, winslash = "/", mustWork = TRUE)
}

parse_options <- function(args) {
  result <- list(
    parameters = NULL,
    template = NULL,
    output_dir = NULL,
    dry_run = FALSE,
    yes = FALSE
  )

  for (arg in args) {
    if (identical(arg, "--dry-run")) {
      result$dry_run <- TRUE
    } else if (identical(arg, "--yes")) {
      result$yes <- TRUE
    } else if (startsWith(arg, "--parameters=")) {
      result$parameters <- sub("^--parameters=", "", arg)
    } else if (startsWith(arg, "--template=")) {
      result$template <- sub("^--template=", "", arg)
    } else if (startsWith(arg, "--output-dir=")) {
      result$output_dir <- sub("^--output-dir=", "", arg)
    } else {
      stopf("Unknown command-line option: %s", arg)
    }
  }
  result
}

choose_parameters_file <- function() {
  if (!interactive()) {
    stopf(
      paste0(
        "No parameters table was supplied. Use ",
        "--parameters=\"path/to/parameters.csv\" when running non-interactively."
      )
    )
  }

  message("Select the BaU1 capped parameters.csv table.")
  tryCatch(
    file.choose(),
    error = function(error) {
      stopf("Parameters table selection failed: %s", conditionMessage(error))
    }
  )
}

choose_output_dir <- function() {
  if (!interactive()) {
    stopf(
      "No output folder was supplied. Use --output-dir=\"path/to/output\" when running non-interactively."
    )
  }

  message("Select the folder containing _1000m_ and where the four working folders will be created.")
  selected <- if (capabilities("tcltk")) {
    tryCatch(
      tcltk::tk_choose.dir(
        default = getwd(),
        caption = "Select the parent folder for four working folders"
      ),
      error = function(error) {
        message("Folder picker unavailable: ", conditionMessage(error))
        NULL
      }
    )
  } else {
    NULL
  }
  if (is.null(selected)) {
    selected <- readline("Enter the destination parent folder path: ")
  }
  if (!length(selected) || is.na(selected) || !nzchar(trimws(selected))) {
    stopf("No output folder was selected.")
  }
  selected
}

find_template <- function(output_dir, explicit_template = NULL) {
  if (!is.null(explicit_template)) {
    template <- normalize_existing(explicit_template, "Template folder")
  } else {
    children <- list.dirs(output_dir, recursive = FALSE, full.names = TRUE)
    # The seed is deliberately distinguished from working folders by leading
    # and trailing underscores, for example E:/_1000m_ or /mnt/_1000m_.
    candidates <- children[
      grepl("^_.*1000m.*_$", basename(children), ignore.case = TRUE)
    ]
    if (!length(candidates)) {
      stopf(
        "No seed folder named like _*1000m*_ was found in the selected output folder: %s",
        output_dir
      )
    }
    if (length(candidates) > 1L) {
      stopf(
        "More than one _*1000m*_ seed folder was found: %s",
        paste(basename(candidates), collapse = ", ")
      )
    }
    template <- normalize_existing(candidates[[1L]], "Template folder")
  }

  if (!dir.exists(template)) stopf("Template is not a folder: %s", template)
  if (!grepl("^_.*1000m.*_$", basename(template), ignore.case = TRUE)) {
    stopf(
      paste0(
        "Refusing template '%s'. The seed-folder name must begin and end with '_' ",
        "and contain '1000m' (for example, _1000m_)."
      ),
      basename(template)
    )
  }

  source_data <- file.path(
    template, "LULCC", "DownloadedDatasets", "SourceDataGlobal"
  )
  if (!dir.exists(source_data)) {
    stopf("Template lacks LULCC/DownloadedDatasets/SourceDataGlobal: %s", template)
  }
  unexpected_parameters <- file.path(source_data, "parameters.csv")
  if (file.exists(unexpected_parameters)) {
    stopf(
      paste0(
        "The immutable seed unexpectedly contains parameters.csv: %s\n",
        "Remove it deliberately from the seed before using this helper."
      ),
      unexpected_parameters
    )
  }
  template
}

read_parameters <- function(path) {
  path <- normalize_existing(path, "Parameters table")
  if (!identical(tolower(basename(path)), "parameters.csv")) {
    stopf("The selected file must be named parameters.csv: %s", path)
  }

  table <- tryCatch(
    utils::read.csv(
      path,
      check.names = FALSE,
      colClasses = "character",
      na.strings = NULL,
      strip.white = FALSE
    ),
    error = function(error) {
      stopf("Could not read parameters.csv: %s", conditionMessage(error))
    }
  )
  required_columns <- c("Var", "ParCHR")
  missing_columns <- setdiff(required_columns, names(table))
  if (length(missing_columns)) {
    stopf(
      "parameters.csv is missing required column(s): %s",
      paste(missing_columns, collapse = ", ")
    )
  }
  list(path = path, table = table)
}

parameter_value <- function(table, key) {
  rows <- which(trimws(table$Var) == key)
  if (length(rows) != 1L) {
    stopf("parameters.csv must contain exactly one '%s' row.", key)
  }
  value <- trimws(table$ParCHR[[rows]])
  if (is.na(value) || !nzchar(value)) {
    stopf("parameters.csv has an empty '%s' value.", key)
  }
  value
}

set_parameter <- function(table, key, value) {
  rows <- which(trimws(table$Var) == key)
  if (length(rows) != 1L) {
    stopf("parameters.csv must contain exactly one '%s' row.", key)
  }
  table$ParCHR[[rows]] <- as.character(value)
  table
}

optional_parameter <- function(table, key, default) {
  rows <- which(trimws(table$Var) == key)
  if (!length(rows)) return(default)
  if (length(rows) != 1L) {
    stopf("parameters.csv has duplicate '%s' rows.", key)
  }
  value <- trimws(table$ParCHR[[rows]])
  if (is.na(value) || !nzchar(value)) default else value
}

woodman_input_spec <- function(table) {
  if (toupper(optional_parameter(table, "LULCt3map", "NO")) != "YES") {
    return(NULL)
  }
  map_name <- parameter_value(table, "LULCt3map_name")
  if (grepl("[/\\\\]", map_name) || !grepl("[.]tif$", map_name,
                                             ignore.case = TRUE)) {
    stopf("Woodman map name must be a .tif filename: %s", map_name)
  }
  start_year <- positive_integer(parameter_value(table, "start_year"),
                                 "start_year")
  end_year <- positive_integer(parameter_value(table, "end_year"),
                               "end_year")
  if (start_year != 2000L || end_year < start_year || end_year > 2050L) {
    stopf("Woodman requires start_year=2000 and end_year no later than 2050.")
  }
  years <- start_year:end_year
  list(
    rasters = c(
      "woodman_zone_pcs.tif",
      sprintf("woodman_luc_%d_pcs.tif", years),
      paste0("pre", years, "_v1_", map_name),
      sprintf("woodman_tof_%d_pcs.tif", years)
    ),
    tables = c("growth_parameters_v3_woodman.csv",
               "woodman_key_crosswalk.csv")
  )
}

resolve_woodman_inputs <- function(spec, template) {
  if (is.null(spec)) return(NULL)
  seed_base <- file.path(template, "LULCC", "DownloadedDatasets",
                         "SourceDataGlobal")
  seed_rasters <- file.path(seed_base, "InRaster", spec$rasters)
  seed_tables <- file.path(seed_base, "InTables", spec$tables)
  required <- c(seed_rasters, seed_tables)
  missing <- required[!file.exists(required) | dir.exists(required)]
  if (length(missing)) {
    stopf(paste0(
      "The seed has Woodman source maps but is missing a calibrated input: %s. ",
      "Publish v7 out_pcs into SourceDataGlobal/InRaster and InTables first."
    ), missing[[1L]])
  }
  sizes <- file.info(required)$size
  if (anyNA(sizes) || any(sizes <= 0)) {
    stopf("The seed contains an empty Woodman input file.")
  }
  c(spec, list(origin = paste0("seed: ", seed_base), sizes = sizes))
}

verify_woodman_inputs <- function(inputs, destination) {
  if (is.null(inputs)) return(invisible(TRUE))
  base <- file.path(destination, "LULCC", "DownloadedDatasets",
                    "SourceDataGlobal")
  paths <- c(file.path(base, "InRaster", inputs$rasters),
             file.path(base, "InTables", inputs$tables))
  missing <- paths[!file.exists(paths)]
  if (length(missing)) stopf("Copied folder lacks Woodman input: %s", missing[[1L]])
  actual_sizes <- file.info(paths)$size
  if (!identical(unname(actual_sizes), unname(inputs$sizes))) {
    stopf("Copied folder has a missing or truncated Woodman input: %s",
          destination)
  }
  invisible(TRUE)
}

positive_integer <- function(value, key) {
  if (!grepl("^[0-9]+$", value)) {
    stopf("Parameter '%s' must be a whole number; found '%s'.", key, value)
  }
  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed) || parsed < 1L) {
    stopf("Parameter '%s' must be at least 1; found '%s'.", key, value)
  }
  parsed
}

derive_run_code <- function(region_value) {
  # Examples:
  #   SSA_adm0_mdg -> mdg (singleton)
  #   SSA_adm0_GOG -> GOG (multicountry region RunCode)
  marker <- regexpr("_adm[0-9]+_", region_value, ignore.case = TRUE, perl = TRUE)
  if (marker[[1L]] < 0L) {
    stopf(
      paste0(
        "Cannot derive a RunCode from region2BprocessedReg '%s'. Expected a value ",
        "such as SSA_adm0_mdg or SSA_adm0_GOG."
      ),
      region_value
    )
  }
  marker_length <- attr(marker, "match.length")[[1L]]
  code <- substr(region_value, marker[[1L]] + marker_length, nchar(region_value))
  if (!nzchar(code) || !grepl("^[A-Za-z0-9]+(?:[-_][A-Za-z0-9]+)*$", code)) {
    stopf("Derived RunCode is not safe for a folder name: '%s'.", code)
  }
  code
}

build_plan <- function(parameters, template, output_dir) {
  scenario <- parameter_value(parameters, "scenario_ver")
  uncapped <- parameter_value(parameters, "uncapped_regrowth")
  scale <- positive_integer(parameter_value(parameters, "GEE_scale"), "GEE_scale")
  end_year <- positive_integer(parameter_value(parameters, "end_year"), "end_year")
  mc_runs <- positive_integer(
    parameter_value(parameters, "monte_carlo_runs"),
    "monte_carlo_runs"
  )
  byregion <- parameter_value(parameters, "byregion")
  region <- parameter_value(parameters, "region2BprocessedReg")

  if (!grepl("^bau1(?:_|$)", scenario, ignore.case = TRUE, perl = TRUE)) {
    stopf(
      "The selected seed table must be BaU1; scenario_ver is '%s'.",
      scenario
    )
  }
  if (!identical(uncapped, "0")) {
    stopf(
      paste0(
        "The selected seed table must be capped; uncapped_regrowth must be 0, ",
        "not '%s'."
      ),
      uncapped
    )
  }
  if (!tolower(byregion) %in% c("regional", "country")) {
    stopf(
      "Unsupported byregion '%s'; expected Regional or Country.",
      byregion
    )
  }
  if (scale != 1000L) {
    stopf("This helper requires GEE_scale 1000; found %s.", scale)
  }

  resolution_match <- regmatches(
    basename(template),
    regexpr("[0-9]+m", basename(template), ignore.case = TRUE)
  )
  if (!length(resolution_match) || !nzchar(resolution_match)) {
    stopf("Could not derive the resolution from template '%s'.", basename(template))
  }
  resolution <- tolower(resolution_match)
  if (!identical(resolution, paste0(scale, "m"))) {
    stopf(
      "Template resolution '%s' conflicts with GEE_scale '%s'.",
      resolution,
      scale
    )
  }

  run_code <- derive_run_code(region)
  ics_scenario <- sub("(?i)^bau1", "ICS3", scenario, perl = TRUE)
  variants <- data.frame(
    scenario_ver = c(scenario, scenario, ics_scenario, ics_scenario),
    uncapped_regrowth = c("0", "1", "0", "1"),
    mode = c("capped", "uncapped", "capped", "uncapped"),
    stringsAsFactors = FALSE
  )
  variants$scenario_name <- tolower(sub("_.*$", "", variants$scenario_ver))
  variants$folder_name <- paste(
    run_code,
    resolution,
    variants$scenario_name,
    end_year,
    paste0("mc", mc_runs),
    variants$mode,
    sep = "_"
  )
  variants$destination <- file.path(output_dir, variants$folder_name)
  variants
}

preflight_destinations <- function(plan, template) {
  normalized_template <- normalizePath(template, winslash = "/", mustWork = TRUE)
  normalized_destinations <- normalizePath(
    plan$destination,
    winslash = "/",
    mustWork = FALSE
  )
  if (any(tolower(normalized_destinations) == tolower(normalized_template))) {
    stopf("A proposed destination resolves to the template folder; refusing to continue.")
  }
  if (anyDuplicated(tolower(normalized_destinations))) {
    stopf("The proposed destination folder names are not unique.")
  }

  existing <- plan$destination[
    file.exists(plan$destination) | dir.exists(plan$destination)
  ]
  if (length(existing)) {
    stopf(
      paste0(
        "Refusing to overwrite or merge with existing working folder(s):\n  %s\n",
        "No folders were changed. Move or rename them deliberately before retrying."
      ),
      paste(existing, collapse = "\n  ")
    )
  }
}

print_plan <- function(parameters_path, template, output_dir, plan,
                       woodman_inputs = NULL) {
  cat("\nMoFuSS working-folder plan\n")
  cat("  Parameters: ", parameters_path, "\n", sep = "")
  cat("  Seed folder: ", template, " (read-only)\n", sep = "")
  cat("  Output root: ", output_dir, "\n", sep = "")
  if (!is.null(woodman_inputs)) {
    cat("  Woodman inputs: ", woodman_inputs$origin, "\n", sep = "")
    cat("  Woodman products: ", length(woodman_inputs$rasters),
        " rasters, ", length(woodman_inputs$tables), " tables\n", sep = "")
  }
  cat("\nFolders and parameter changes:\n")
  for (row in seq_len(nrow(plan))) {
    cat(
      "  ", plan$folder_name[[row]],
      "  [scenario_ver=", plan$scenario_ver[[row]],
      ", uncapped_regrowth=", plan$uncapped_regrowth[[row]], "]\n",
      sep = ""
    )
  }
  cat("\nExisting working folders will never be overwritten or merged.\n")
}

confirm_plan <- function() {
  answer <- trimws(tolower(readline("Create these four folders? [y/N]: ")))
  answer %in% c("y", "yes")
}

copy_directory <- function(source, destination) {
  if (file.exists(destination) || dir.exists(destination)) {
    stopf("Copy destination already exists; refusing to merge: %s", destination)
  }

  cat("\nCopying:\n  from: ", source, "\n  to:   ", destination, "\n", sep = "")
  if (.Platform$OS.type == "windows") {
    status <- system2(
      "robocopy",
      args = c(
        shQuote(source),
        shQuote(destination),
        "/E",
        "/COPY:DAT",
        "/DCOPY:DAT",
        "/R:2",
        "/W:2",
        "/XJ"
      )
    )
    if (is.null(status)) status <- 0L
    # Robocopy codes 0 through 7 are successful outcomes; 8+ are failures.
    failed <- is.na(status) || status >= 8L
    command <- "robocopy"
  } else {
    # GNU cp preserves hidden files, empty directories, symlinks, and timestamps.
    status <- system2(
      "cp",
      args = c("-a", "--", shQuote(source), shQuote(destination))
    )
    if (is.null(status)) status <- 0L
    failed <- is.na(status) || status != 0L
    command <- "cp"
  }
  if (failed) {
    stopf(
      paste0(
        "%s failed with exit code %s while creating:\n  %s\n",
        "A partial new folder may remain; no pre-existing folder was touched."
      ),
      command, status, destination
    )
  }
  if (!dir.exists(destination)) {
    stopf("Copy command finished but destination was not created: %s", destination)
  }
  invisible(destination)
}

write_parameters <- function(
    table,
    destination,
    scenario_ver,
    uncapped_regrowth,
    allow_inherited = FALSE) {
  table <- set_parameter(table, "scenario_ver", scenario_ver)
  table <- set_parameter(table, "uncapped_regrowth", uncapped_regrowth)
  target_dir <- file.path(
    destination, "LULCC", "DownloadedDatasets", "SourceDataGlobal"
  )
  if (!dir.exists(target_dir)) {
    stopf("Copied folder lacks the parameters destination: %s", target_dir)
  }
  target <- file.path(target_dir, "parameters.csv")
  if (file.exists(target) && !allow_inherited) {
    stopf("Refusing to overwrite an unexpected parameters.csv: %s", target)
  }
  utils::write.table(
    table,
    file = target,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    na = "",
    # Use "\n" here. On Windows, R's text connection translates it to CRLF;
    # supplying "\r\n" explicitly would produce CR-CR-LF and appear as blanks.
    eol = "\n",
    fileEncoding = "UTF-8"
  )
  target
}

verify_output <- function(destination, expected_scenario, expected_uncapped,
                          woodman_inputs = NULL) {
  target <- file.path(
    destination,
    "LULCC",
    "DownloadedDatasets",
    "SourceDataGlobal",
    "parameters.csv"
  )
  verified <- read_parameters(target)$table
  actual_scenario <- parameter_value(verified, "scenario_ver")
  actual_uncapped <- parameter_value(verified, "uncapped_regrowth")
  if (!identical(actual_scenario, expected_scenario) ||
      !identical(actual_uncapped, expected_uncapped)) {
    stopf("Parameter verification failed in: %s", target)
  }
  verify_woodman_inputs(woodman_inputs, destination)
  invisible(TRUE)
}

main <- function() {
  opts <- parse_options(commandArgs(trailingOnly = TRUE))
  parameters_path <- if (is.null(opts$parameters)) {
    choose_parameters_file()
  } else {
    opts$parameters
  }
  input <- read_parameters(parameters_path)
  output_dir <- normalize_existing(
    if (is.null(opts$output_dir)) choose_output_dir() else opts$output_dir,
    "Output folder"
  )
  if (!dir.exists(output_dir)) stopf("Output path is not a folder: %s", output_dir)

  template <- find_template(output_dir, opts$template)
  plan <- build_plan(input$table, template, output_dir)

  # Complete all non-mutating validation before the first large copy.
  preflight_destinations(plan, template)
  woodman_inputs <- resolve_woodman_inputs(woodman_input_spec(input$table),
                                           template)
  print_plan(input$path, template, output_dir, plan, woodman_inputs)

  if (opts$dry_run) {
    cat("\nDry run complete. No folders were created or changed.\n")
    return(invisible(plan))
  }
  if (!opts$yes && !confirm_plan()) {
    cat("\nCancelled. No folders were created or changed.\n")
    return(invisible(NULL))
  }

  # Create BaU1 capped directly from the immutable seed and place the selected
  # table inside it. The other three folders are cloned from that complete first
  # folder, then receive their scenario-specific table.
  copy_directory(template, plan$destination[[1L]])
  write_parameters(
    input$table,
    plan$destination[[1L]],
    plan$scenario_ver[[1L]],
    plan$uncapped_regrowth[[1L]]
  )
  verify_output(
    plan$destination[[1L]],
    plan$scenario_ver[[1L]],
    plan$uncapped_regrowth[[1L]],
    woodman_inputs
  )

  for (row in 2L:nrow(plan)) {
    copy_directory(plan$destination[[1L]], plan$destination[[row]])
    write_parameters(
      input$table,
      plan$destination[[row]],
      plan$scenario_ver[[row]],
      plan$uncapped_regrowth[[row]],
      allow_inherited = TRUE
    )
    verify_output(
      plan$destination[[row]],
      plan$scenario_ver[[row]],
      plan$uncapped_regrowth[[row]],
      woodman_inputs
    )
  }

  cat("\nCreated and verified all four MoFuSS working folders:\n")
  cat(paste0("  ", plan$destination, collapse = "\n"), "\n")
  invisible(plan)
}

tryCatch(main(), error = function(error) {
  stop(paste0("\nERROR: ", conditionMessage(error)), call. = FALSE)
})
