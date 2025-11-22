# MoFuSS
# Version 3
# Date: Mar 2024

# 2dolist ----

# Internal parameters ----
#	Charcoal conversion factor	6.3	tons of charcoal per ton oven-dry wood 
# (using the study's forest:kiln efficiency, converting to oven-dry assuming 20% moisture for the air-dry value given in the report)	
efchratio  <- 6

# Load libraries ----
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)
library(openxlsx)  # or writexl if you prefer

# Detect OS
os <- Sys.info()["sysname"]

setwd(countrydir)
getwd()

# Read parameters table ----
# Read parameters table (recognizing the delimiter)
detect_delimiter <- function(file_path) {
  # Read the first line of the file
  first_line <- readLines(file_path, n = 1)
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
country_parameters <- read_delim(parameters_file_path, delim = delimiter)
print(tibble::as_tibble(country_parameters), n=100)

country_parameters %>%
  dplyr::filter(Var == "region2BprocessedCtry_iso") %>%
  pull(ParCHR) -> region2BprocessedCtry_iso 

# fuel_intensity and efchratio
base_path <- paste0(countrydir, "/LULCC/DownloadedDatasets")

# Find the folder that starts with "SourceData"
country_subfolder <- list.dirs(base_path, full.names = FALSE, recursive = FALSE) %>%
  grep("^SourceData", ., value = TRUE)

full_path <- file.path(base_path, country_subfolder)

full_path

file <- paste0(full_path,"/demand_parameters.xlsx")

fuel_intensity <- read_excel(
  file,
  sheet = "intensity",
  skip = 0
) |> 
  dplyr::select(fuel, cons_pc, per_capita_consumption) |> 
  mutate(
    fuel = as.character(fuel),
    cons_pc = as.numeric(cons_pc),
    per_capita_consumption = as.character(per_capita_consumption)
  )
fuel_intensity

efchratio_tb <- read_excel(
  file,
  sheet = "efchratio",
  skip = 0
) |> 
  mutate(
    efchratio = as.numeric(efchratio)
  )
efchratio <- efchratio_tb %>% dplyr::pull(efchratio)
efchratio

fuel_intensity <- fuel_intensity %>%
  mutate(
    cons_pc = if_else(fuel == "Charcoal", cons_pc * efchratio, cons_pc)
  )

fuel_intensity

# Set directory to demand_in
setwd(paste0(demanddir,"/demand_in"))

outfile2 <- "cons_fuels_years_proj_original.csv"

if (!file.exists(outfile2)) {
  dem_file <- "cons_fuels_years_proj.csv"
  # Read parameters table (recognizing the delimiter)
  detect_delimiter <- function(file_path) {
    # Read the first line of the file
    first_line <- readLines(file_path, n = 1)
    # Check if the first line contains ',' or ';'
    if (grepl(";", first_line)) {
      return(";")
    } else {
      return(",")
    }
  }
  # Detect the delimiter
  delimiter <- detect_delimiter(dem_file)
  # Read the CSV file with the detected delimiter
  dem_raw <- read_delim(dem_file, delim = delimiter)
  print(tibble::as_tibble(dem_raw), n=5)
  
  # Write fixed full table
  write_csv(
    dem_raw,
    "cons_fuels_years_proj_original.csv"
  )
} else {
  message("File already exists — not overwriting: ", outfile2," 
          Reading from original")
  dem_file <- "cons_fuels_years_proj_original.csv"
  # Read parameters table (recognizing the delimiter)
  detect_delimiter <- function(file_path) {
    # Read the first line of the file
    first_line <- readLines(file_path, n = 1)
    # Check if the first line contains ',' or ';'
    if (grepl(";", first_line)) {
      return(";")
    } else {
      return(",")
    }
  }
  # Detect the delimiter
  delimiter <- detect_delimiter(dem_file)
  # Read the CSV file with the detected delimiter
  dem_raw <- read_delim(dem_file, delim = delimiter)
  print(tibble::as_tibble(dem_raw), n=5)
}

dem_fixed <- dem_raw %>%
  # attach population (in thousands) from pop_fixed
  left_join(
    pop_fixed %>%
      select(iso3, area, fuel, year, pop),
    by = c("iso3", "area", "fuel", "year")
  ) %>%
  # attach per-capita intensities
  left_join(fuel_intensity, by = "fuel") %>%
  # compute new fuel_tons3 where we have both pop and cons_pc
  mutate(
    fuel_tons3 = dplyr::if_else(
      !is.na(cons_pc) & !is.na(pop),
      pop * 1000 * cons_pc,   # pop is in thousands → *1000 to get persons
      fuel_tons3              # otherwise keep original
    )
  ) %>%
  # drop helper columns so structure matches dem_raw exactly
  select(names(dem_raw))

# Write fixed full table
write_csv(
  dem_fixed,
  "cons_fuels_years_proj.csv"
)
