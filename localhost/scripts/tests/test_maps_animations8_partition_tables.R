args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: Rscript test_maps_animations8_partition_tables.R <scenario_dir> <maps_animations8.R>")
}

scenario_dir <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
maps_script <- normalizePath(args[[2]], winslash = "/", mustWork = TRUE)
scratch <- tempfile("mofuss_fnrb_partition_")
dir.create(scratch, recursive = TRUE)
dir.create(file.path(scratch, "LULCC"), recursive = TRUE)
dir.create(file.path(scratch, "LULCC", "TempTables"), recursive = TRUE)
dir.create(file.path(scratch, "Out", "webmofuss_results"), recursive = TRUE)

junction <- function(relative_path) {
  source <- file.path(scenario_dir, relative_path)
  target <- file.path(scratch, relative_path)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(source)) stop("Missing test input directory: ", source)
  ok <- Sys.junction(source, target)
  if (!isTRUE(ok)) stop("Could not create test junction: ", target)
}

junction("debugging_1")
junction(file.path("LULCC", "TempRaster"))
junction(file.path("LULCC", "TempVector"))
junction(file.path("LULCC", "DownloadedDatasets"))

suppressPackageStartupMessages({
  library(data.table)
  library(foreach)
  library(raster)
  library(sf)
  library(tidyverse)
})

country_file <- file.path(scenario_dir, "LULCC", "TempTables", "Country.csv")
country_name <- read.csv(country_file, stringsAsFactors = FALSE) |>
  dplyr::filter(Key. == 1) |>
  dplyr::pull(Country)
parameter_dir <- file.path(
  scenario_dir, "LULCC", "DownloadedDatasets", paste0("SourceData", country_name)
)
parameter_file <- list.files(
  parameter_dir, pattern = "^parameters.*[.]csv$", full.names = TRUE
)
if (length(parameter_file) != 1L) stop("Expected exactly one parameter table.")
first_line <- readLines(parameter_file, n = 1L, warn = FALSE)
country_parameters <- readr::read_delim(
  parameter_file, delim = if (grepl(";", first_line)) ";" else ",",
  show_col_types = FALSE
)
parameter_value <- function(key) {
  value <- country_parameters$ParCHR[country_parameters$Var == key]
  if (length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
    stop("Expected one parameter value for ", key)
  }
  trimws(as.character(value))
}

MC <- 1L
STdyn <- as.integer(parameter_value("end_year")) - as.integer(parameter_value("start_year"))
aoi_poly <- as.integer(parameter_value("aoi_poly"))
mcthreshold <- 30L
uncertainty_digits <- 2L
fNRB_partition_tables <- 1L

script_lines <- readLines(maps_script, warn = FALSE)
block_start <- grep("^summarise_mc_uncertainty <- function", script_lines)
block_end <- grep("^} # if [(]fNRB_partition_tables == 1[)]", script_lines)
if (length(block_start) != 1L || length(block_end) != 1L || block_end <= block_start) {
  stop("Could not locate the production fNRB partition block.")
}

old_wd <- setwd(scratch)
on.exit(setwd(old_wd), add = TRUE)
eval(parse(text = script_lines[block_start:block_end]), envir = .GlobalEnv)

expected_tables <- c(
  "summary_adm0_fr.csv", "summary_adm1_fr.csv", "summary_adm2_fr.csv",
  "summary_ecoregions_fr.csv"
)
output_dir <- file.path(scratch, "Out", "webmofuss_results")
missing_tables <- expected_tables[!file.exists(file.path(output_dir, expected_tables))]
if (length(missing_tables)) {
  stop("Missing period-level table(s): ", paste(missing_tables, collapse = ", "))
}
if (file.exists(file.path(output_dir, "summary_adm0.csv"))) {
  stop("The legacy annual summary table was unexpectedly generated.")
}

adm0 <- read.csv(file.path(output_dir, "summary_adm0_fr.csv"), check.names = FALSE)
required_columns <- c(
  "NRB_2020_2030_mean", "Harv_2020_2030_mean", "fNRB_2020_2030_mean", "MC_n"
)
missing_columns <- setdiff(required_columns, names(adm0))
if (length(missing_columns)) {
  stop("Missing period result columns: ", paste(missing_columns, collapse = ", "))
}
if (any(adm0$MC_n != 1L)) stop("The MC_n field does not reflect the one-run fixture.")

cat("MAPS_ANIMATIONS8_PARTITION_TABLES_MC1_OK\n")
cat("Scratch output:", scratch, "\n")
