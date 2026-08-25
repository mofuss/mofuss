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
# Script: 6d_parameters_dinamica_v1.R
# Version: 1
# Date: Jul 2026
# Execution: Source from RStudio; Dinamica EGO does not invoke this script directly.
#
# Purpose: Extract and validate the required runtime parameters, then install the
# verified parameters_dinamica.csv consumed by Dinamica EGO.
# Inputs: Source parameters.csv and inherited country/source-data paths.
# Outputs: LULCC/DownloadedDatasets/SourceDataGlobal/parameters_dinamica.csv.
# Side effects: Changes working directory and atomically replaces the Dinamica
# runtime parameter table after validation.

# 2dolist ----

# Internal parameters ----

# Load libraries ----
library(conflicted)

library(dplyr)
library(readr)

# Detect OS
os <- Sys.info()["sysname"]

setwd(countrydir)
getwd()

# Read parameters table ----
if (length(webmofuss) != 1L || is.na(webmofuss) ||
    !(webmofuss %in% c(0, 1))) {
  stop("`webmofuss` must be exactly 0 or 1.", call. = FALSE)
}

if (!file.exists(parameters_file_path)) {
  stop(
    sprintf("Source parameters file does not exist: %s", parameters_file_path),
    call. = FALSE
  )
}

if (as.integer(webmofuss) == 1L) {
  # Read parameters table in webmofuss
  country_parameters <- read_csv(parameters_file_path, show_col_types = FALSE)
} else {
  # Read parameters table (recognizing the delimiter)
  detect_delimiter <- function(file_path) {
    # Read the first line of the file
    first_line <- readLines(file_path, n = 1, warn = FALSE)
    # Check if the first line contains ',' or ';'
    if (grepl(";", first_line)) {
      return(";")
    } else {
      return(",")
    }
  }
  # Detect the delimiter
  delimiter <- detect_delimiter(parameters_file_path)
  # Read the CSV file with the detected delimiter
  country_parameters <- read_delim(
    parameters_file_path,
    delim = delimiter,
    show_col_types = FALSE
  )
  print(tibble::as_tibble(country_parameters), n = 100)
}

required_parameters <- c(
  "start_year",
  "end_year",
  "monte_carlo_runs",
  "uncapped_regrowth",
  "npa_ease"
)

required_columns <- c("Var", "ParCHR")
missing_columns <- setdiff(required_columns, names(country_parameters))
if (length(missing_columns) > 0L) {
  stop(
    sprintf(
      "Source parameters file is missing required column(s): %s",
      paste(missing_columns, collapse = ", ")
    ),
    call. = FALSE
  )
}

country_parameters$Var <- trimws(as.character(country_parameters$Var))

parameter_counts <- vapply(
  required_parameters,
  function(parameter_name) {
    sum(country_parameters$Var == parameter_name, na.rm = TRUE)
  },
  integer(1)
)

bad_counts <- parameter_counts[parameter_counts != 1L]
if (length(bad_counts) > 0L) {
  stop(
    paste0(
      "Each required parameter must occur exactly once in ",
      normalizePath(parameters_file_path, winslash = "/", mustWork = TRUE),
      ". Invalid row counts: ",
      paste(
        sprintf("%s=%d", names(bad_counts), unname(bad_counts)),
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

parse_integer_parameter <- function(raw_value, parameter_name, source_label) {
  value_text <- trimws(as.character(raw_value))

  if (length(value_text) != 1L || is.na(value_text) ||
      !nzchar(value_text) || !grepl("^[+-]?[0-9]+$", value_text)) {
    stop(
      sprintf(
        "Parameter `%s` in %s must be a single whole-number value; found `%s`.",
        parameter_name,
        source_label,
        paste(value_text, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  numeric_value <- suppressWarnings(as.numeric(value_text))
  integer_min <- -(.Machine$integer.max) - 1
  if (!is.finite(numeric_value) || numeric_value < integer_min ||
      numeric_value > .Machine$integer.max) {
    stop(
      sprintf(
        "Parameter `%s` in %s is outside the supported integer range: `%s`.",
        parameter_name,
        source_label,
        value_text
      ),
      call. = FALSE
    )
  }

  as.integer(numeric_value)
}

source_label <- normalizePath(
  parameters_file_path,
  winslash = "/",
  mustWork = TRUE
)

source_values <- setNames(
  vapply(
    required_parameters,
    function(parameter_name) {
      source_row <- country_parameters[country_parameters$Var == parameter_name, ]
      parse_integer_parameter(
        source_row$ParCHR[[1]],
        parameter_name,
        source_label
      )
    },
    integer(1)
  ),
  required_parameters
)

start_year <- source_values[["start_year"]]
end_year <- source_values[["end_year"]]
monte_carlo_runs <- source_values[["monte_carlo_runs"]]
uncapped_regrowth <- source_values[["uncapped_regrowth"]]
npa_ease <- source_values[["npa_ease"]]

if (end_year < start_year) {
  stop("`end_year` cannot be earlier than `start_year`.", call. = FALSE)
}
if (monte_carlo_runs < 1L) {
  stop("`monte_carlo_runs` must be at least 1.", call. = FALSE)
}
if (!(uncapped_regrowth %in% c(0L, 1L))) {
  stop("`uncapped_regrowth` must be exactly 0 or 1.", call. = FALSE)
}

# Save parameters table for Dinamica EGO ----
country_parameters_din <- data.frame(
  "Var" = required_parameters,
  "ParCHR" = unname(source_values),
  check.names = FALSE
)

assert_runtime_matches_source <- function(runtime_path, expected_table) {
  runtime_table <- tryCatch(
    read.csv(
      runtime_path,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      colClasses = "character"
    ),
    error = function(error_condition) {
      stop(
        sprintf(
          "Could not read generated Dinamica parameters table `%s`: %s",
          runtime_path,
          conditionMessage(error_condition)
        ),
        call. = FALSE
      )
    }
  )

  runtime_missing_columns <- setdiff(required_columns, names(runtime_table))
  if (length(runtime_missing_columns) > 0L) {
    stop(
      sprintf(
        "Generated Dinamica table is missing column(s): %s",
        paste(runtime_missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  runtime_table <- runtime_table[, required_columns, drop = FALSE]
  runtime_table$Var <- trimws(as.character(runtime_table$Var))

  if (nrow(runtime_table) != nrow(expected_table) ||
      !identical(runtime_table$Var, expected_table$Var)) {
    stop(
      paste0(
        "Generated Dinamica parameter rows do not match the source rows. ",
        "Expected order: ", paste(expected_table$Var, collapse = ", "),
        "; found: ", paste(runtime_table$Var, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  runtime_values <- vapply(
    seq_len(nrow(runtime_table)),
    function(row_number) {
      parse_integer_parameter(
        runtime_table$ParCHR[[row_number]],
        runtime_table$Var[[row_number]],
        normalizePath(runtime_path, winslash = "/", mustWork = TRUE)
      )
    },
    integer(1)
  )

  mismatch_rows <- which(runtime_values != expected_table$ParCHR)
  if (length(mismatch_rows) > 0L) {
    mismatch_report <- paste(
      sprintf(
        "%s: parameters.csv=%d, parameters_dinamica.csv=%d",
        expected_table$Var[mismatch_rows],
        expected_table$ParCHR[mismatch_rows],
        runtime_values[mismatch_rows]
      ),
      collapse = "; "
    )
    stop(
      paste0("Dinamica parameter verification failed: ", mismatch_report),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

write_verified_parameters <- function(expected_table, runtime_path) {
  runtime_directory <- dirname(runtime_path)
  if (!dir.exists(runtime_directory) &&
      !dir.create(runtime_directory, recursive = TRUE, showWarnings = FALSE)) {
    stop(
      sprintf("Could not create Dinamica parameters directory: %s", runtime_directory),
      call. = FALSE
    )
  }

  temporary_path <- tempfile(
    pattern = "parameters_dinamica_",
    tmpdir = runtime_directory,
    fileext = ".csv"
  )
  on.exit(unlink(temporary_path, force = TRUE), add = TRUE)

  write.csv(
    expected_table,
    file = temporary_path,
    row.names = FALSE,
    quote = FALSE
  )
  assert_runtime_matches_source(temporary_path, expected_table)

  backup_path <- NULL
  if (file.exists(runtime_path)) {
    backup_path <- tempfile(
      pattern = "parameters_dinamica_previous_",
      tmpdir = runtime_directory,
      fileext = ".csv"
    )
    if (!file.rename(runtime_path, backup_path)) {
      stop(
        sprintf("Could not prepare `%s` for safe replacement.", runtime_path),
        call. = FALSE
      )
    }
  }

  if (!file.rename(temporary_path, runtime_path)) {
    if (!is.null(backup_path) && file.exists(backup_path)) {
      file.rename(backup_path, runtime_path)
    }
    stop(
      sprintf("Could not install verified Dinamica parameters table: %s", runtime_path),
      call. = FALSE
    )
  }

  verification_error <- tryCatch(
    {
      assert_runtime_matches_source(runtime_path, expected_table)
      NULL
    },
    error = function(error_condition) error_condition
  )

  if (!is.null(verification_error)) {
    unlink(runtime_path, force = TRUE)
    restored <- !is.null(backup_path) && file.exists(backup_path) &&
      file.rename(backup_path, runtime_path)
    stop(
      paste0(
        conditionMessage(verification_error),
        if (restored) " The previous runtime table was restored." else ""
      ),
      call. = FALSE
    )
  }

  if (!is.null(backup_path) && file.exists(backup_path)) {
    unlink(backup_path, force = TRUE)
  }

  invisible(runtime_path)
}

runtime_parameters_path <- file.path(
  "LULCC",
  "TempTables",
  "parameters_dinamica.csv"
)

write_verified_parameters(country_parameters_din, runtime_parameters_path)

message(
  sprintf(
    paste0(
      "[OK] Dinamica parameters verified against parameters.csv: ",
      "start_year=%d, end_year=%d, monte_carlo_runs=%d, ",
      "uncapped_regrowth=%d, npa_ease=%d"
    ),
    start_year,
    end_year,
    monte_carlo_runs,
    uncapped_regrowth,
    npa_ease
  )
)

