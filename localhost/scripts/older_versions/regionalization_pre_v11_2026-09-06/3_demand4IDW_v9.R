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
# Script: 3_demand4IDW_v9.R
# Version: 9
# Date: Sep 2026
# Execution: Source from RStudio; Dinamica EGO does not invoke this script directly.
#
# Purpose: Spatialize annual fuel-user and fuel-demand tables to raster grids,
# including IDW inputs and separate woody/non-woody demand products.
# Inputs: Processed demand tables, population/urban-rural rasters, parameters.csv
# and inherited country, demand and temporary-directory paths.
# Outputs: Annual per-fuel demand rasters, woody-demand aggregates, IDW tables,
# and (for evidence-based multi-country regions) directional HC-cluster jobs.
# Side effects: Changes working directory, clears the configured Terra temporary
# directory and overwrites demand rasters and supporting files.

# 2dolist ----
# FIX THE MASK ISSUE WITH LINUX, THAT WAS PATCHED FOR THE MOMENT!
# VERY IMPORTANT TO DEFINE A SOLID WORKFLOW FOR REGIONALIZING COUNTRIES, e.g. Zambia
# start_year = 2000 ok # check why 2001 doesn't work

# Internal parameters ----
optimizeD = 0
temdirdefined = 1
cube_rasters = 0

# Load libraries ----
library(conflicted)

library(terra)
# terraOptions(steps = 55)
if (temdirdefined == 1) {
  # Leave memory headroom for later scripts sourced into this same R session.
  # A 0.9 fraction persisted globally and made the harmonizer materialize
  # multi-GB raster intermediates in memory.
  terraOptions(tempdir = rTempdir, memfrac = 0.5)
  # List all files and directories inside the folder
  contents <- list.files(rTempdir, full.names = TRUE, recursive = TRUE)
  # Delete the contents but keep the folder
  unlink(contents, recursive = TRUE, force = TRUE)
}
# terraOptions(progress=0)
library(dplyr)
library(gdata)
library(ggplot2)
#library(hacksaw)
#library(mapview)
library(purrr)
library(raster)
library(readxl)
library(rlang)
library(sf)
library(stringr)
library(svDialogs)
library(terra)
library(tibble)
library(tictoc)
library(tidyterra)
library(tidyverse)
library(tidyr)

# Detect OS
os <- Sys.info()["sysname"]

setwd(countrydir)
getwd()

# Read parameters table ----
if (webmofuss == 1) {
  # Read parameters table in webmofuss
  country_parameters <- read_csv(parameters_file_path)
} else if(webmofuss == 0) {
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
}

country_parameters %>%
  dplyr::filter(Var == "proj_gcs") %>%
  pull(ParCHR) -> proj_gcs

country_parameters %>%
  dplyr::filter(Var == "epsg_gcs") %>%
  pull(ParCHR) %>%
  as.integer(.) -> epsg_gcs

country_parameters %>%
  dplyr::filter(Var == "proj_pcs") %>%
  pull(ParCHR) -> proj_pcs

country_parameters %>%
  dplyr::filter(Var == "epsg_pcs") %>%
  pull(ParCHR) %>%
  as.integer(.) -> epsg_pcs

country_parameters %>%
  dplyr::filter(Var == "proj_authority") %>%
  pull(ParCHR) -> proj_authority

country_parameters %>%
  dplyr::filter(Var == "pop_ver") %>%
  pull(ParCHR) -> pop_ver

country_parameters %>%
  dplyr::filter(Var == "pop_map_name") %>%
  pull(ParCHR) -> pop_map_name

country_parameters %>%
  dplyr::filter(Var == "scenario_ver") %>%
  pull(ParCHR) -> scenario_ver

urb_shift_factor <- country_parameters %>%
  dplyr::filter(Var == "urb_shift_factor") %>%
  dplyr::pull(ParCHR) %>%
  as.numeric()

country_parameters %>%
  dplyr::filter(Var == "byregion") %>%
  pull(ParCHR) -> byregion
if (byregion != "Country") {
  urb_shift_factor <- 1
}

country_parameters %>%
  dplyr::filter(Var == "subcountry") %>%
  pull(ParCHR) %>%
  as.integer(.) -> subcountry

country_parameters %>%
  dplyr::filter(Var == "end_year") %>%
  pull(ParCHR) %>%
  as.integer(.) -> end_year

country_parameters %>%
  dplyr::filter(Var == "pop_map_yr") %>%
  pull(ParCHR) %>%
  as.integer(.) -> yr

country_parameters %>%
  dplyr::filter(Var == "GEE_scale") %>%
  pull(ParCHR) %>%
  as.integer(.) -> GEE_scale

country_parameters %>%
  dplyr::filter(Var == "demand_col") %>%
  pull(ParCHR) -> demand_col

country_parameters %>%
  dplyr::filter(Var == "aoi_poly") %>%
  pull(ParCHR) -> aoi_poly

country_parameters %>%
  dplyr::filter(Var == "aoi_poly_file") %>%
  pull(ParCHR) -> aoi_poly_file

country_parameters %>%
  dplyr::filter(Var == "start_year") %>%
  pull(ParCHR) %>%
  as.integer(.) -> start_year

country_parameters %>%
  dplyr::filter(Var == "end_year") %>%
  pull(ParCHR) %>%
  as.integer(.) -> end_year

start_year = start_year # check why 2001 doesn't work

setwd(demanddir)

unlink("pop_maps_byregion/", recursive=TRUE)
unlink("pop_temp/", recursive=TRUE)
unlink("pop_out/", recursive=TRUE)
unlink("demand_temp/", recursive=TRUE)
unlink("demand_out/", recursive=TRUE)
unlink("to_idw/", recursive=TRUE)

setwd(countrydir)
unlink("scenario_ver.txt")
setwd(demanddir)

if (!dir.exists("pop_maps_byregion")) {dir.create("pop_maps_byregion")}
if (!dir.exists("pop_temp")) {dir.create("pop_temp")} 
if (!dir.exists("pop_out")) {dir.create("pop_out")} 
if (!dir.exists("demand_temp")) {dir.create("demand_temp")} 
if (!dir.exists("demand_out")) {dir.create("demand_out")} 
if (!dir.exists("to_idw")) {dir.create("to_idw")} 

getwd()
poprast <- paste0("demand_in/",pop_map_name) 

read_wfdb <- function(file) {
  if (!file.exists(file)) {
    stop("Demand table not found: ", file)
  }

  # Personal ICS exports can contain scenario metadata before the actual
  # demand-table header. Locate the real header and infer its delimiter.
  header_lines <- readLines(file, n = 100, warn = FALSE)
  required_columns <- unique(c(
    "iso3",
    "area",
    "fuel",
    "year",
    "num_fuel_users_thousands",
    demand_col
  ))
  header_row <- Inf
  delimiter <- NULL

  for (candidate_delimiter in c(",", ";")) {
    candidate_rows <- which(vapply(
      header_lines,
      function(header_line) {
        header_fields <- strsplit(
          sub("^\ufeff", "", header_line),
          candidate_delimiter,
          fixed = TRUE
        )[[1]]
        header_fields <- tolower(trimws(gsub(
          "\"",
          "",
          header_fields,
          fixed = TRUE
        )))
        all(tolower(required_columns) %in% header_fields)
      },
      logical(1)
    ))

    if (length(candidate_rows) > 0 && candidate_rows[[1]] < header_row) {
      header_row <- candidate_rows[[1]]
      delimiter <- candidate_delimiter
    }
  }

  if (is.null(delimiter)) {
    stop(
      "Could not find a valid demand-table header in ", file,
      ". Required columns: ", paste(required_columns, collapse = ", ")
    )
  }

  demand_table <- readr::read_delim(
    file,
    delim = delimiter,
    skip = header_row - 1,
    show_col_types = FALSE,
    progress = FALSE,
    trim_ws = TRUE
  )
  names(demand_table) <- trimws(sub("^\ufeff", "", names(demand_table)))

  missing_columns <- setdiff(required_columns, names(demand_table))
  if (length(missing_columns) > 0) {
    stop(
      "Demand table ", file, " is missing required column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }

  demand_table
}

# Define scenarios ----
if (scenario_ver %in% c("BaU1_v2", "BaU2_v2", "BaU3_v2",
                        "ICS1_v2", "ICS2_v2", "ICS3_v2")) {
  
  wfdb <- read_wfdb(
    paste0(
      "demand_in/demand_",
      tolower(scenario_ver),
      ".csv"
    )
  )
  
} else {
  
  stop(paste0("Invalid scenario_ver: ", scenario_ver))
  
}

unique(wfdb$fuel)
unique(wfdb$area)
# Remove rows where area == "Overall" (any capitalization)
wfdb <- wfdb %>%
  dplyr::filter(!str_detect(area, regex("^overall$", ignore_case = TRUE)))
unique(wfdb$area)

unique(wfdb$fuel)
head(wfdb)
print(scenario_ver) # save as text to recover later down the river

setwd(countrydir)
write.table(scenario_ver, "LULCC/TempTables/scenario_ver.txt")
SceVer <- read.table("LULCC/TempTables/scenario_ver.txt") %>% .$x
write.table(byregion, "LULCC/TempTables/region_ext.txt")
reg_ext <- read.table("LULCC/TempTables/region_ext.txt") %>% .$x
setwd(demanddir)

# Time period
annos.list2 <- c(start_year:end_year) 
# annos <- annos.list2[!annos.list2 %in% yr]
annos <- annos.list2

# Save in LULCC/TempTables to replace parameters years
setwd(countrydir)
write.table(annos, "LULCC/TempTables/annos.txt")
annostxt <- read.table("LULCC/TempTables/annos.txt") %>% .$x 
setwd(demanddir)

# Select a region
if (aoi_poly == 1) {
  # ### Selection of largest overlap country for demand calculations
  # mofuss_regions0_gpkg <- vect(st_read("demand_in/mofuss_regions0.gpkg"))
  # # mofuss_regions0 <- as.data.frame(mofuss_regions0_gpkg)
  # # Handle the case where aoi_poly is 1, regardless of byregion
  # cat("aoi_poly is set to 1. This overrides other conditions.\n")
  # # Define file paths
  # kml_file_path <- Sys.glob(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceDataGlobal/InVector_GCS/",aoi_poly_file))
  # # Read the SpatVector files
  # kml_data <- vect(kml_file_path) # Read the .kml file
  # # plot(kml_data)
  # # Reassign the CRS of kml_data to match mofuss_regions0_gpkg
  # crs(kml_data) <- crs(mofuss_regions0_gpkg)
  # # Ensure both layers are in the same projection
  # if (!crs(mofuss_regions0_gpkg) == crs(kml_data)) {
  #   stop("Projections do not match!")
  # }
  # # Generic rename handler
  # if ("GID_0" %in% names(kml_data)) names(kml_data)[names(kml_data) == "GID_0"] <- "GID_0_kml"
  # # Intersect the two layers to calculate the overlapping areas
  # overlap <- try(terra::intersect(mofuss_regions0_gpkg, kml_data), silent = TRUE)
  # # Check if the result is valid
  # if (inherits(overlap, "try-error") || is.null(overlap) || length(overlap) == 0) {
  #   stop("No valid overlap found between the KML file and the GPKG regions.")
  # }
  # # Add an area column for the overlap polygons
  # overlap$area <- expanse(overlap, unit = "km") # Area in square kilometers
  # # Group by the `GID_0` and sum the overlapping areas for each GID_0
  # overlap_summary <- as.data.frame(overlap) %>%
  #   group_by(GID_0) %>%
  #   summarise(total_area = sum(area, na.rm = TRUE))
  # # Check if overlap_summary is empty
  # if (nrow(overlap_summary) == 0) {
  #   stop("No overlapping regions found.")
  # }
  # # Find the GID_0 with the largest total overlapping area
  # largest_overlap <- overlap_summary[which.max(overlap_summary$total_area), ]
  # 
  # # Find the NAME_0 corresponding to the largest_overlap GID_0
  # matching_row <- mofuss_regions0_gpkg[mofuss_regions0_gpkg$GID_0 == largest_overlap$GID_0, ]
  # # Extract the NAME_0 value
  # mofuss_region <- matching_row$GID_0
  # mofuss_region_kml <- matching_row$GID_0
  # 
  # # Print the result
  # cat("The GID_0 with the largest overlap is:", largest_overlap$GID_0, "\n")
  # cat("Overlapping area:", largest_overlap$total_area, "km²\n")
  # ###
  
  
  ### Selection of all overlapping countries for demand calculations
  
  mofuss_regions0_gpkg <- vect(st_read("demand_in/mofuss_regions0.gpkg"))
  # mofuss_regions0 <- as.data.frame(mofuss_regions0_gpkg)
  # Handle the case where aoi_poly is 1, regardless of byregion
  cat("aoi_poly is set to 1. This overrides other conditions.\n")
  # Define file paths
  kml_file_path <- Sys.glob(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceDataGlobal/InVector_GCS/",aoi_poly_file))
  # Read the SpatVector files
  kml_data <- vect(kml_file_path) # Read the .kml file
  # plot(kml_data)
  # Reassign the CRS of kml_data to match mofuss_regions0_gpkg
  crs(kml_data) <- crs(mofuss_regions0_gpkg)
  # Ensure both layers are in the same projection
  if (!crs(mofuss_regions0_gpkg) == crs(kml_data)) {
    stop("Projections do not match!")
  }
  # Generic rename handler
  if ("GID_0" %in% names(kml_data)) names(kml_data)[names(kml_data) == "GID_0"] <- "GID_0_kml"
  # Intersect the two layers to calculate the overlapping areas
  overlap <- try(terra::intersect(mofuss_regions0_gpkg, kml_data), silent = TRUE)
  # Check if the result is valid
  if (inherits(overlap, "try-error") || is.null(overlap) || length(overlap) == 0) {
    stop("No valid overlap found between the KML file and the GPKG regions.")
  }
  # Add an area column for the overlap polygons
  overlap$area <- expanse(overlap, unit = "km") # Area in square kilometers
  # Group by the `GID_0` and sum the overlapping areas for each GID_0
  overlap_summary <- as.data.frame(overlap) %>%
    group_by(GID_0) %>%
    summarise(total_area = sum(area, na.rm = TRUE))
  # Check if overlap_summary is empty
  if (nrow(overlap_summary) == 0) {
    stop("No overlapping regions found.")
  }
  # Select all countries with some overlap (total_area > 0)
  overlap_with_overlap <- overlap_summary %>%
    dplyr::filter(total_area > 0)
  
  # Check if any regions with overlap exist
  if (nrow(overlap_with_overlap) == 0) {
    cat("No countries with overlap found.\n")
  } else {
    # Print the results for all countries with overlap
    cat("Countries with overlap and their total overlap areas:\n")
    print(overlap_with_overlap)
  }
  
  # Optionally, if you want to extract the names of the countries with overlap, use:
  overlap_with_overlap_details <- merge(overlap_with_overlap, mofuss_regions0_gpkg, by = "GID_0")
  countries_with_overlap <- overlap_with_overlap_details %>%
    dplyr::select(GID_0, NAME_0, total_area)
  
  # Print the countries and their overlap areas
  cat("Countries with overlap areas:\n")
  print(countries_with_overlap)
  
  mofuss_region_kml <- overlap_with_overlap_details
  
  ###
  
} else if (byregion == "Continental" & aoi_poly == 0) {
  country_parameters %>%
    dplyr::filter(Var == "region2BprocessedCont") %>%
    pull(ParCHR) -> mofuss_region
  
  if (!length(mofuss_region)) {
    cat("You cancelled the choice\n")
  } else {
    cat("You selected:\n")
    print(mofuss_region)
  }
  
} else if (byregion == "Regional" & aoi_poly == 0) {
  country_parameters %>%
    dplyr::filter(Var == "region2BprocessedReg") %>%
    pull(ParCHR) -> mofuss_region
  
  if (!length(mofuss_region)) {
    cat("You cancelled the choice\n")
  } else {
    cat("You selected:\n")
    print(mofuss_region)
  }
} else if (byregion == "Country" & aoi_poly == 0) {
  country_parameters %>%
    dplyr::filter(Var == "region2BprocessedCtry_iso") %>%
    pull(ParCHR) -> mofuss_region
  
  if (!length(mofuss_region)) {
    cat("You cancelled the choice\n")
  } else {
    cat("You selected:\n")
    print(mofuss_region)
  }
  
} else {
  # Handle any other conditions if necessary
  cat("No specific conditions met.\n")
}

# Get mofuss region for parameters below
mofuss_regions0_gpkg <- vect(st_read("demand_in/mofuss_regions0.gpkg"))
mofuss_regions0 <- as.data.frame(mofuss_regions0_gpkg)

continent.list <- mofuss_regions0 %>%
  dplyr::select(mofuss_reg) %>%
  terra::unique()

regions.list <- mofuss_regions0 %>%
  dplyr::select(mofuss_reg) %>%
  terra::unique()

countries.list <- mofuss_regions0 %>%
  dplyr::select(NAME_0, GID_0) %>%
  terra::unique() %>%
  arrange(NAME_0)

if (subcountry != 1) {
  
  totpopwfdb <- wfdb %>% 
    dplyr::filter(year == yr) %>%
    dplyr::filter(area %in% c("urban", "rural")) %>%
    group_by(iso3) %>% 
    summarise(
      sum_pop = sum(num_fuel_users_thousands) * 1000,
      .groups = "drop"
    )
  
  
  # Reads furb in 2018 from WHO dataset
  wfdb_join <- wfdb %>%
    dplyr::select(iso3, country) %>%
    terra::unique()
  
  furb_wfdb <- wfdb %>%
    dplyr::filter(grepl(yr, year)) %>%
    dplyr::filter(grepl('urban', area)) %>%
    group_by(iso3) %>% 
    summarise(urb_pop=sum(num_fuel_users_thousands)*1000,
              .groups = 'drop') %>%
    left_join(totpopwfdb, ., by="iso3") %>% 
    mutate(furb = urb_pop/sum_pop) %>%
    left_join(wfdb_join, ., by = "iso3") %>%
    dplyr::select(iso3, country, furb) %>%
    rename(GID_0 = iso3,
           NAME_0 = country)
  
  # furb_wfdb %>%
  #   dplyr::filter(GID_0 == "ZMB")
  
} else if (subcountry == 1) {
  
  totpoprob <- wfdb %>%
    dplyr::filter(grepl(yr, year)) %>%
    dplyr::filter(area %in% c("Urban", "Rural")) %>%
    group_by(iso3) %>%
    summarise(sum_pop=sum(num_fuel_users_thousands)*1000, 
              .groups = 'drop')
  
  furb_rob <- wfdb %>%
    dplyr::filter(year == yr, area %in% c("Urban", "Rural")) %>%
    dplyr::group_by(iso3, country) %>%   # <-- key fix: include country (split)
    dplyr::summarise(
      urb_pop = sum(num_fuel_users_thousands[area == "Urban"], na.rm = TRUE) * 1000,
      rur_pop = sum(num_fuel_users_thousands[area == "Rural"], na.rm = TRUE) * 1000,
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      furb = urb_pop / (urb_pop + rur_pop)
    ) %>%
    dplyr::select(GID_0 = iso3, NAME_0 = country, furb)
  
  furb_rob
  
}

pop0 <- rast(poprast) #in base year

if (aoi_poly == 1) {
  # Handle the case where aoi_poly is 1, regardless of byregion
  cat("aoi_poly is set to 1. This overrides other conditions.\n")
  print("***NOW RUNNING GLOBAL DEMAND SCENARIOS - Polygon***")
  adm0_reg <- mofuss_regions0_gpkg %>% 
    dplyr::filter(GID_0 %in% mofuss_region_kml$GID_0) # Check if multiple countries or values is doable
  pop0_K <- crop(pop0, ext(adm0_reg) + .01)
  if (os == "Windows") {
    pop0_reg <- terra::mask(pop0_K, adm0_reg) # THIS BREAKS IN UBUNTU
  } else if(os == "Linux") {
    pop0_reg <- terra::mask(pop0_K, adm0_reg)
    # pop0_reg <- pop0_K
  }
  # plot(pop0_reg, main=paste0("Overlapping with your AoI")) #,mofuss_region2))
  # lines(adm0_reg)
  # Sys.sleep(10)
  
} else if (byregion == "Global" & aoi_poly == 0) {
  print("***NOW RUNNING GLOBAL DEMAND SCENARIOS - Global***")
  adm0_reg <- mofuss_regions0_gpkg
  pop0_K <- crop(pop0, ext(adm0_reg) + .01)
  if (os == "Windows") {
    pop0_reg <- terra::mask(pop0_K, adm0_reg) #THIS BREAKS IN UBUNTU
  } else if(os == "Linux") {
    pop0_reg <- terra::mask(pop0_K, adm0_reg)
    #pop0_reg <- pop0_K
  }
  # plot(pop0_reg)
  # lines(adm0_reg)
  
} else if (byregion == "Continental" & aoi_poly == 0) {
  print("***NOW RUNNING CONTINENTAL DEMAND SCENARIOS - Continental***")
  adm0_reg <- mofuss_regions0_gpkg %>%
    dplyr::filter(grepl(paste0(mofuss_region,"*"), mofuss_reg))
  # # plot(pop0)
  # # lines(adm0_reg, lwd=2)
  pop0_K <- crop(pop0, ext(adm0_reg) + .01)
  if (os == "Windows") {
    pop0_reg <- terra::mask(pop0_K, adm0_reg) #THIS BREAKS IN UBUNTU
  } else if(os == "Linux") {
    pop0_reg <- terra::mask(pop0_K, adm0_reg)
    # pop0_reg <- pop0_K
  }
  # plot(pop0_reg,main=c("Region to be processed"))
  # lines(adm0_reg)
  # Sys.sleep(10)
  
} else if (byregion == "Regional" & aoi_poly == 0) {
  print("***NOW RUNNING REGION DEMAND SCENARIOS - Regional***")
  adm0_reg <- mofuss_regions0_gpkg %>% 
    dplyr::filter(grepl(mofuss_region, mofuss_reg))
  # plot(pop0)
  # lines(adm0_reg, lwd=2)
  pop0_K <- crop(pop0, ext(adm0_reg) + .01)
  if (os == "Windows") {
    pop0_reg <- terra::mask(pop0_K, adm0_reg) # THIS BREAKS IN UBUNTU NA WINDOWS AS ELL
  } else if(os == "Linux") {
    pop0_reg <- terra::mask(pop0_K, adm0_reg)
    #pop0_reg <- pop0_K
  }
  # plot(pop0_reg,main=c("Region to be processed"))
  # lines(adm0_reg)
  # Sys.sleep(10)
  
} else if (byregion == "Country" & aoi_poly == 0 & subcountry != 1) {
  print("***NOW RUNNING COUNTRY DEMAND SCENARIOS - Country***")
  adm0_reg <- mofuss_regions0_gpkg %>% 
    dplyr::filter(GID_0 == mofuss_region) # Check if multiple countries or values is doable
  pop0_K <- crop(pop0, ext(adm0_reg) + .01)
  if (os == "Windows") {
    pop0_reg <- terra::mask(pop0_K, adm0_reg) #THIS BREAKS IN UBUNTU
  } else if(os == "Linux") {
    pop0_reg <- terra::mask(pop0_K, adm0_reg)
    # pop0_reg <- pop0_K
  }
  # plot(pop0_reg, main=paste0("You selected ",mofuss_region))
  # lines(adm0_reg)
  # Sys.sleep(10)
  
} else if (byregion == "Country" & aoi_poly == 0 & subcountry == 1) { 
  print("***NOW RUNNING SUB-COUNTRY DEMAND SCENARIOS - Country***")
  # VERY IMPORTANT TO DEFINE A SOLID WORKFLOW FOR REGIONALIZING COUNTRIES, e.g. Zambia
  
  country_parameters %>%
    dplyr::filter(Var == "region2BprocessedCtry_iso") %>%
    pull(ParCHR) -> region2BprocessedCtry_iso
  
  mofuss_regions2_gpkg <- vect(st_read("demand_in/mofuss_regions2.gpkg"))
  mofuss_regions2 <- as.data.frame(mofuss_regions2_gpkg)
  subcountries.list <- mofuss_regions2 %>%
    dplyr::select(NAME_2, GID_2) %>%
    terra::unique() %>%
    arrange(NAME_2)
  
  # Function: dissolve by NAME_2 matching furb_rob$NAME_0
  # - ignore_case: set TRUE for case-insensitive matching
  # x: SpatVector with NAME_2, GID_0
  # y: tibble/data.frame with NAME_0
  # iso_filter: e.g., region2BprocessedCtry_iso ("ZMB")
  dissolve_by_match <- function(x, y, iso_filter, ignore_case = FALSE, not_label = NULL) {
    stopifnot(inherits(x, "SpatVector"))
    if (missing(iso_filter) || is.null(iso_filter)) {
      stop("Please provide 'iso_filter' (e.g., region2BprocessedCtry_iso).")
    }
    
    # 1) Filter SpatVector to target ISO
    x <- x[x$GID_0 == iso_filter, ]
    if (nrow(x) == 0) stop("No features after filtering by GID_0 == iso_filter.")
    
    key <- function(v) {
      v <- trimws(v)
      if (ignore_case) toupper(v) else v
    }
    
    # 2) Build lookup from y
    y_lkp <- y %>%
      mutate(.key = key(NAME_0)) %>%
      distinct(.key, .keep_all = TRUE)
    
    # 3) Decide the unmatched label
    if (is.null(not_label)) {
      not_rows <- y_lkp %>% dplyr::filter(grepl("^NOT", .key))  # "Not*" considering ignore_case
      if (nrow(not_rows) == 1) {
        not_label <- not_rows$NAME_0[[1]]
      } else if (nrow(not_rows) > 1) {
        # prefer one present in x$NAME_2 if possible
        cand <- not_rows$NAME_0
        present <- cand[key(cand) %in% key(unique(x$NAME_2))]
        if (length(present) == 1) {
          not_label <- present
        } else {
          stop(
            "Multiple 'Not*' labels found in y$NAME_0 but none uniquely matches x$NAME_2.\n",
            "Candidates: ", paste(not_rows$NAME_0, collapse = ", "), "\n",
            "Specify 'not_label=' explicitly (e.g., not_label = 'NotLusaka')."
          )
        }
      } else {
        not_label <- "Other"  # fallback when no Not* in y
      }
    }
    
    # 4) Match vs. unmatched
    key_sv <- key(x$NAME_2)
    matched_vals <- base::intersect(unique(key_sv), unique(y_lkp$.key))
    
    # Unmatched get the decided not_label
    x$match_name <- ifelse(key_sv %in% matched_vals, x$NAME_2, not_label)
    
    # 5) Dissolve by match_name
    x_diss <- terra::aggregate(x["match_name"], by = "match_name")
    
    # 6) Join attributes from y (GID_0 required; furb optional)
    x_diss$.key <- key(x_diss$match_name)
    bring_cols <- base::intersect(c("GID_0", "furb"), names(y_lkp))
    if (!"GID_0" %in% bring_cols) stop("y must contain column 'GID_0'.")
    
    lkp_to_merge <- y_lkp %>% dplyr::select(.key, all_of(bring_cols))
    x_out <- terra::merge(x_diss, lkp_to_merge, by = ".key", all.x = TRUE)
    
    # 7) Fill missing GID_0 (e.g., if not_label wasn't in y)
    x_out$GID_0 <- ifelse(is.na(x_out$GID_0), iso_filter, x_out$GID_0)
    
    # 8) Final columns
    keep <- c("match_name", "GID_0")
    if ("furb" %in% names(x_out)) keep <- c(keep, "furb")
    x_out <- x_out[, keep]
    
    x_out
  }
  
  # Usage
  result_vec <- dissolve_by_match(
    x = mofuss_regions2_gpkg,
    y = furb_rob,
    iso_filter = region2BprocessedCtry_iso,
    ignore_case = TRUE,
    #not_label = "NotLusaka"
  )
  
  # # Quick visual sanity check:
  # plot(result_vec, col = rainbow(nrow(result_vec))); result_vec
  # result_vec
  
  adm0_reg <- result_vec
  pop0_K <- crop(pop0, ext(adm0_reg) + .01)
  if (os == "Windows") {
    pop0_reg <- terra::mask(pop0_K, adm0_reg) #THIS BREAKS IN UBUNTU
  } else if(os == "Linux") {
    pop0_reg <- terra::mask(pop0_K, adm0_reg)
    # pop0_reg <- pop0_K
  }
  # plot(pop0_reg, main=paste0("You selected ",region2BprocessedCtry_iso))
  # lines(adm0_reg)
  # Sys.sleep(10)
  
} else {
  # Handle any other conditions if necessary
  cat("No specific conditions met.\n")
}

# To cross-check with excel demand dataset: cons_fuels_years.xlsx
unique(adm0_reg$GID_0)
# unique(adm1_reg$GID_1)
# unique(adm2_reg$GID_2)

for (i in adm0_reg$GID_0) { # Start of outer region (i) loop ----
  # i = "ZMB_1"
  # i = "KEN"
  print(i)
  if (subcountry != 1) {
    ctry_furb <- furb_wfdb %>%
      dplyr::filter(GID_0 == i) %>%
      pull(furb)
    wfdb_ctry_pop <- totpopwfdb %>%
      dplyr::filter(iso3 == i) %>%
      pull(sum_pop)
    ctry_vector <- adm0_reg %>%
      dplyr::filter(GID_0 == i)
  } else if (subcountry == 1) {
    ctry_furb <- furb_rob %>%
      dplyr::filter(GID_0 == i) %>%
      pull(furb)
    rob_ctry_pop <- totpoprob %>%
      dplyr::filter(iso3 == i) %>%
      pull(sum_pop)
    ctry_vector <- adm0_reg %>%
      dplyr::filter(GID_0 == i)
  }
  
  pop0_K2 <- crop(pop0_reg, ext(ctry_vector) + .01)
  if (os == "Windows") {
    pop0_ctry_ras <- mask(pop0_K2, ctry_vector)
  } else if(os == "Linux") {
    pop0_ctry_ras <- pop0_K2
  }
  png(file=paste0("pop_maps_byregion/",i,".png"),
      width=600, height=350)
  # plot(pop0_ctry_ras, main=i, xlab = "Long", ylab = "Lat")
  # lines(ctry_vector, lwd=0.2)
  # Sys.sleep(5)
  # dev.off()
  
  totpop <- global(pop0_ctry_ras, "sum", na.rm=TRUE) %>%
    pull(sum)
  urbpop <- totpop * ctry_furb 
  rurpop <- totpop - urbpop
  totpop
  urbpop
  rurpop
  
  if (subcountry != 1) {
    pop0_ctry_rasadj <- pop0_ctry_ras*wfdb_ctry_pop/totpop
  } else if (subcountry == 1) {
    pop0_ctry_rasadj <- pop0_ctry_ras*rob_ctry_pop/totpop
  }
  
  totpopadj <- global(pop0_ctry_rasadj, "sum", na.rm=TRUE) %>%
    pull(sum)
  urbpopadj <- totpopadj * ctry_furb
  rurpopadj <- totpopadj - urbpopadj
  totpopadj
  urbpopadj
  rurpopadj
  terra::writeRaster(pop0_ctry_rasadj, paste0("pop_temp/",pop_ver,"_",i,"_",yr,"_popadj.tif"), filetype = "GTiff", overwrite = TRUE)
  
  for (j in annos) { ## Start of inner years (j) loop ----
    # i="PNG"
    # j=2000
    
    gc()
    print(j)
    
    if (subcountry != 1) {
      
      furb_wfdb.anno <- wfdb %>%
        dplyr::filter(
          iso3 == i,
          grepl(j, year)
        ) %>%
        group_by(iso3) %>%
        summarise(
          urb_frac = sum(num_fuel_users_thousands[area == "urban"]) /
            sum(num_fuel_users_thousands[area %in% c("urban", "rural")]),
          .groups = "drop"
        ) %>%
        pull(urb_frac)
      
      totpopwfdb_annual <- wfdb %>% 
        dplyr::filter(area %in% c("urban", "rural")) %>%
        group_by(iso3, year) %>% 
        summarise(
          sum_pop = sum(num_fuel_users_thousands) * 1000,
          .groups = 'drop'
        )
      
      wfdb_ctry_pop_annual <- totpopwfdb_annual %>%
        dplyr::filter(iso3 == i) %>%
        dplyr::filter(year == j) %>%
        pull(sum_pop)
      
      pop0_ctry_rasadj.anno<- pop0_ctry_ras*wfdb_ctry_pop_annual/totpop
      totpopadj.anno <- global(pop0_ctry_rasadj.anno, "sum", na.rm=TRUE) %>%
        pull(sum)
      urbpopadj.anno <- totpopadj.anno * furb_wfdb.anno
      rurpopadj.anno <- totpopadj.anno - urbpopadj.anno
      totpopadj.anno
      urbpopadj.anno
      rurpopadj.anno
      terra::writeRaster(pop0_ctry_rasadj.anno, paste0("pop_temp/",pop_ver,"_",i,"_",j,"_popadj.tif"), filetype = "GTiff", overwrite = TRUE)
      
      
    } else if (subcountry == 1) {
      
      furb_rob.anno <- wfdb %>%
        dplyr::filter(
          iso3 == i,
          grepl(j, year)
        ) %>%
        group_by(iso3) %>%
        summarise(
          urb_frac = sum(num_fuel_users_thousands[grepl("urban", area)]) / sum(num_fuel_users_thousands),
          .groups = "drop"
        ) %>%
        dplyr::pull(urb_frac)
      
      totpopROB_annual <- wfdb %>% 
        group_by(iso3,year) %>% 
        summarise(sum_pop=sum(num_fuel_users_thousands)*1000,
                  .groups = 'drop')
      
      rob_ctry_pop_annual <- totpopROB_annual %>%
        dplyr::filter(iso3 == i) %>%
        dplyr::filter(year == j) %>%
        pull(sum_pop)
      
      pop0_ctry_rasadj.anno<- pop0_ctry_ras*rob_ctry_pop_annual/totpop
      totpopadj.anno <- global(pop0_ctry_rasadj.anno, "sum", na.rm=TRUE) %>%
        pull(sum)
      urbpopadj.anno <- totpopadj.anno * furb_rob.anno
      rurpopadj.anno <- totpopadj.anno - urbpopadj.anno
      totpopadj.anno
      urbpopadj.anno
      rurpopadj.anno
      terra::writeRaster(pop0_ctry_rasadj.anno, paste0("pop_temp/",pop_ver,"_",i,"_",j,"_popadj.tif"), filetype = "GTiff", overwrite = TRUE)
      
    } 
    
    # Saca el umbral de corte urbano/rural para el año base de 2018 O 2020
    vec.anno <- as_tibble(pop0_ctry_rasadj.anno, na.rm = TRUE) %>% 
      arrange(desc(.)) %>%
      dplyr::select(matches("pop_2020$")) %>%  # Select columns ending with "WorldPop"
      pull(1)
    
    #### Manual tuning of urban/rural ratio
    # Some countries are ill-defined towards rural/urban population, such as the case of Nepal,
    # in which could be possible that urban population accounts for more than what 
    # the WHO dataset says.
    print(paste0("Manual tuning of urban/rural ratio: ",urb_shift_factor))
    
    ix.anno <- length(which(cumsum(vec.anno) <= urbpopadj.anno)) * urb_shift_factor
    vec.anno[ix.anno] #Valor de corte
    
    # filtra por el umbral
    # First, find the column name that ends with "WorldPop"
    column_name <- names(pop0_ctry_rasadj.anno)[grepl("pop_2020$", names(pop0_ctry_rasadj.anno))]
    column_name <- column_name[1]
    # Convert the column name to a symbol
    column_symbol <- sym(column_name)
    # Now, use `filter()` dynamically
    urbanpopulation.anno <- pop0_ctry_rasadj.anno %>%
      dplyr::filter(!!column_symbol > vec.anno[ix.anno])
    
    # terra::writeRaster(urbanpopulation, paste0("population_temp/",pop_ver,"_",i,"_",j,"_urbpop.tif"), filetype = "GTiff", overwrite = TRUE)
    m_urb <- c(-Inf, 0, NA,
               0, Inf, 2)
    rcl_urb <- matrix(m_urb, ncol=3, byrow=TRUE)
    urbanpopulationR.anno <- urbanpopulation.anno %>%
      classify(rcl_urb, include.lowest=TRUE)
    # terra::writeRaster(urbanpopulation, paste0("population_temp/",pop_ver,"_",i,"_",j,"_urbpopR.tif"), filetype = "GTiff", overwrite = TRUE)
    
    # First, find the column name that ends with "WorldPop"
    column_name <- names(pop0_ctry_rasadj.anno)[grepl("pop_2020$", names(pop0_ctry_rasadj.anno))]
    column_name <- column_name[1]
    # Convert the column name to a symbol
    column_symbol <- sym(column_name)
    # Now, use `filter()` dynamically
    ruralpopulation.anno <- pop0_ctry_rasadj.anno %>%
      dplyr::filter(!!column_symbol <= vec.anno[ix.anno])
    
    # terra::writeRaster(ruralpopulation, paste0("population_temp/",pop_ver,"_",i,"_",j,"_rurpop.tif"), filetype = "GTiff", overwrite = TRUE)
    m_rur <- c(-Inf, 0, NA,
               0, Inf, 1)
    rcl_rur <- matrix(m_rur, ncol=3, byrow=TRUE)
    ruralpopulationR.anno <- ruralpopulation.anno %>%
      classify(rcl_rur, include.lowest=TRUE)
    # terra::writeRaster(ruralpopulation, paste0("population_temp/",pop_ver,"_",i,"_",j,"_rurpopR.tif"), filetype = "GTiff", overwrite = TRUE)
    
    rururbpopulationR.anno <- merge(urbanpopulationR.anno, ruralpopulationR.anno)
    rururbpopulationR_plot.anno <- rururbpopulationR.anno %>%
      mutate(!!column_name := recode(!!column_symbol,
                                     `1` = "rural",
                                     `2` = "urban"))
    
    # plot(rururbpopulationR_plot.anno, main=paste0(i," : ",j))
    # lines(ctry_vector, lwd=2)
    terra::writeRaster(rururbpopulationR.anno, paste0("pop_temp/",pop_ver,"_",i,"_",j,"_rururbR.tif"), filetype = "GTiff", overwrite = TRUE)
    
    # Validation
    urbpopmap.anno <- global(urbanpopulation.anno, "sum", na.rm=TRUE) %>% 
      pull(sum)
    urbpopmap.anno
    urbpopadj.anno
    (urbpopmap.anno/totpopadj.anno)
    
    rurpopmap.anno <- global(ruralpopulation.anno, "sum", na.rm=TRUE) %>% 
      pull(sum)
    rurpopmap.anno
    rurpopadj.anno
    (rurpopmap.anno/totpopadj.anno)
    
    ### Spread population and demand by ENERGY CARRIER use and urban vs rural ----
    ### Spread population and demand by ENERGY CARRIER use and urban vs rural ----
    suppressPackageStartupMessages({
      library(dplyr)
      library(terra)
      library(stringr)
      library(rlang)
    })
    
    .match_fuel <- function(x, target) tolower(trimws(x)) == tolower(trimws(target))
    
    compute_fuel_maps <- function(
    fuel_name,            # <- renamed to avoid shadowing the 'fuel' column
    subcountry,
    i, j,
    wfdb,
    demand_col,
    urbanpopulation.anno,
    ruralpopulation.anno,
    urbpopmap.anno,
    rurpopmap.anno,
    pop_ver,
    out_dir_pop    = "pop_temp",
    out_dir_demand = "demand_temp",
    write_percap   = FALSE,
    wfdb_fuels_for_demand = NULL 
    ) {
      # browser()
      allowed_wfdb <- unique(wfdb$fuel)
      
      if (!(fuel_name %in% allowed_wfdb)) {
        stop(sprintf("fuel must be one of: %s",
                     paste(allowed_wfdb, collapse = ", ")))
      }
      
      # POP source
      pop_tbl <- wfdb %>%
        dplyr::filter(grepl(i, .data$iso3)) %>%
        dplyr::filter(.match_fuel(.data$fuel, fuel_name)) %>%
        dplyr::filter(grepl(j, .data$year)) %>%
        dplyr::filter(grepl("rur|urb", .data$area))
      
      pop_col_name <- "num_fuel_users_thousands"
      
      # ---- (B) DEMAND TABLE: always from wfdb (exact & case-insensitive)
      if (is.null(wfdb_fuels_for_demand)) {
        demand_tbl <- wfdb %>%
          dplyr::filter(grepl(i, .data$iso3)) %>%
          dplyr::filter(.data$year == as.numeric(j)) %>%
          dplyr::filter(grepl("rur|urb", .data$area)) %>%
          dplyr::filter(tolower(trimws(.data$fuel)) == tolower(trimws(fuel_name)))
      } else {
        demand_tbl <- wfdb %>%
          dplyr::filter(grepl(i, .data$iso3)) %>%
          dplyr::filter(.data$year == as.numeric(j)) %>%
          dplyr::filter(grepl("rur|urb", .data$area)) %>%
          dplyr::filter(tolower(trimws(.data$fuel)) %in% tolower(trimws(wfdb_fuels_for_demand)))
      }
      
      # ---- Urban users
      biourb_total_people <- pop_tbl %>%
        dplyr::filter(grepl("urb", .data$area)) %>%
        pull(all_of(pop_col_name)) %>%
        sum(na.rm = TRUE) * 1000
      
      urbbio_Sctry.anno <- if (urbpopmap.anno == 0) {
        urbanpopulation.anno * 0
      } else {
        (urbanpopulation.anno * biourb_total_people) / urbpopmap.anno
      }
      
      # ---- Urban demand
      biourb_d_tons <- demand_tbl %>%
        dplyr::filter(grepl("urb", .data$area)) %>%
        pull(all_of(demand_col)) %>%
        sum(na.rm = TRUE)
      
      urbbioDem_Sctry.anno <- if (urbpopmap.anno == 0) {
        urbanpopulation.anno * 0
      } else {
        urbanpopulation.anno * biourb_d_tons / urbpopmap.anno
      }
      
      # ---- Rural users
      biorur_total_people <- pop_tbl %>%
        dplyr::filter(grepl("rur", .data$area)) %>%
        pull(all_of(pop_col_name)) %>%
        sum(na.rm = TRUE) * 1000
      
      rurbio_Sctry.anno <- if (rurpopmap.anno == 0) {
        ruralpopulation.anno * 0
      } else {
        ruralpopulation.anno * biorur_total_people / rurpopmap.anno
      }
      
      # ---- Rural demand
      biorur_d_tons <- demand_tbl %>%
        dplyr::filter(grepl("rur", .data$area)) %>%
        pull(all_of(demand_col)) %>%
        sum(na.rm = TRUE)
      
      rurbioDem_Sctry.anno <- if (rurpopmap.anno == 0) {
        ruralpopulation.anno * 0
      } else {
        ruralpopulation.anno * biorur_d_tons / rurpopmap.anno
      }
      
      # ---- Merge & write
      users_raster  <- merge(rurbio_Sctry.anno, urbbio_Sctry.anno)
      demand_raster <- merge(rurbioDem_Sctry.anno, urbbioDem_Sctry.anno)
      
      # old (buggy): fuel_tag <- tolower(gsub("[^a-z0-9]+", "_", fuel_name))
      fuel_tag <- gsub("[^a-z0-9]+", "_", tolower(fuel_name))
      if (!dir.exists(out_dir_pop))    dir.create(out_dir_pop, recursive = TRUE, showWarnings = FALSE)
      if (!dir.exists(out_dir_demand)) dir.create(out_dir_demand, recursive = TRUE, showWarnings = FALSE)
      
      users_path  <- file.path(out_dir_pop,    sprintf("%s_%s_%s_%s_users.tif",  pop_ver, i, j, fuel_tag))
      demand_path <- file.path(out_dir_demand, sprintf("%s_%s_%s_%s_demand.tif", pop_ver, i, j, fuel_tag))
      
      terra::writeRaster(users_raster,  users_path,  filetype = "GTiff", overwrite = TRUE)
      terra::writeRaster(demand_raster, demand_path, filetype = "GTiff", overwrite = TRUE)
      
      percap_raster <- (demand_raster / users_raster) * 1000 / 365
      if (isTRUE(write_percap)) {
        percap_path <- file.path(out_dir_demand, sprintf("%s_%s_%s_%s_percap.tif", pop_ver, i, j, fuel_tag))
        terra::writeRaster(percap_raster, percap_path, filetype = "GTiff", overwrite = TRUE)
      }
      
      invisible(list(
        fuel                = fuel_name,
        subcountry          = subcountry,
        urb_users_sum       = as.numeric(global(urbbio_Sctry.anno,  "sum", na.rm = TRUE)$sum),
        rur_users_sum       = as.numeric(global(rurbio_Sctry.anno,  "sum", na.rm = TRUE)$sum),
        urb_demand_sum_tons = biourb_d_tons,
        rur_demand_sum_tons = biorur_d_tons,
        users_raster_path   = users_path,
        demand_raster_path  = demand_path,
        # NEW: return the separate demand rasters so we can aggregate later
        urb_dem_rast        = urbbioDem_Sctry.anno,
        rur_dem_rast        = rurbioDem_Sctry.anno
      ))
      
    }
    
    if (subcountry != 1) {
      
      # fuels_who <- c("Kerosene","Gas","Electricity","Biomass","Charcoal","Coal")
      fuels_wfdb <- unique(wfdb$fuel)
      
      results <- lapply(fuels_wfdb, function(fu)
        compute_fuel_maps(
          fuel_name = fu,              # ← use the new argument name explicitly
          subcountry = subcountry,     # != 1 here
          i = i, j = j,
          wfdb = wfdb,
          demand_col = demand_col,
          urbanpopulation.anno = urbanpopulation.anno,
          ruralpopulation.anno = ruralpopulation.anno,
          urbpopmap.anno = urbpopmap.anno,
          rurpopmap.anno = rurpopmap.anno,
          pop_ver = pop_ver,
          out_dir_pop = "pop_temp",
          out_dir_demand = "demand_temp",
          write_percap = FALSE
        )
      )
      
      save_wf_aggregates_wfdb <- function(
    i, j,
    wfdb, demand_col,
    subcountry = 0,  # anything != 1
    urbanpopulation.anno, ruralpopulation.anno,
    urbpopmap.anno, rurpopmap.anno,
    pop_ver,
    out_dir = "demand_temp"
      ) {
        stopifnot(subcountry != 1)
        
        get_dem_wfdb <- function(wfdb_fuel) {
          res <- compute_fuel_maps(
            fuel_name = wfdb_fuel,        # WHO label used both in whodb and wfdb
            subcountry = subcountry,     # != 1
            i = i, j = j,
            wfdb = wfdb,
            demand_col = demand_col,
            urbanpopulation.anno = urbanpopulation.anno,
            ruralpopulation.anno = ruralpopulation.anno,
            urbpopmap.anno = urbpopmap.anno,
            rurpopmap.anno = rurpopmap.anno,
            pop_ver = pop_ver,
            out_dir_pop = "pop_temp",
            out_dir_demand = "demand_temp",
            write_percap = FALSE
          )
          list(urb = res$urb_dem_rast, rur = res$rur_dem_rast)
        }
        
        fuels_avail <- unique(wfdb$fuel)
        
        safe_get_dem_wfdb <- function(fuel_name) {
          if (fuel_name %in% fuels_avail) {
            get_dem_wfdb(fuel_name)
          } else {
            NULL
          }
        }
        
        fw  <- safe_get_dem_wfdb("fuelwood")
        ifw <- safe_get_dem_wfdb("imp_fuelwood")
        ch  <- safe_get_dem_wfdb("charcoal")
        ich <- safe_get_dem_wfdb("imp_charcoal")
        
        # ---- wftons_w: rural fuelwood + rural imp_fuelwood
        wf_w_sum   <- app(c(fw$rur, ifw$rur), fun = sum, na.rm = TRUE)
        
        # ---- wftons_v: urban fw + urban ifw + urban ch + urban ich + rural ch + rural ich
        wf_v_sum   <- app( c(fw$urb, ifw$urb, ch$urb, ich$urb, ch$rur, ich$rur), fun = sum, na.rm = TRUE)
        
        if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
        terra::writeRaster(wf_w_sum, file.path(out_dir, paste0(pop_ver, "_", i, "_", j, "_wftons_w.tif")),
                           filetype = "GTiff", overwrite = TRUE)
        terra::writeRaster(wf_v_sum, file.path(out_dir, paste0(pop_ver, "_", i, "_", j, "_wftons_v.tif")),
                           filetype = "GTiff", overwrite = TRUE)
        
        invisible(TRUE)
      }
      
      
      save_wf_aggregates_wfdb(
        i = i, j = j,
        wfdb = wfdb, demand_col = demand_col,
        subcountry = subcountry,  # must be != 1
        urbanpopulation.anno = urbanpopulation.anno,
        ruralpopulation.anno = ruralpopulation.anno,
        urbpopmap.anno = urbpopmap.anno,
        rurpopmap.anno = rurpopmap.anno,
        pop_ver = pop_ver,
        out_dir = "demand_temp"
      )
      
    } else if (subcountry == 1) {
      
      # fuels_wfdb <- c("fuelwood","charcoal","imp_fuelwood","imp_charcoal",
      #                 "gas","kerosene","electric","pellets","ethanol","biogas","other")
      fuels_wfdb <- unique(wfdb$fuel)
      
      results <- lapply(fuels_wfdb, function(fu)
        compute_fuel_maps(
          fuel = fu,
          subcountry = 1,
          i = i, j = j,
          wfdb = wfdb,
          demand_col = demand_col,
          urbanpopulation.anno = urbanpopulation.anno,
          ruralpopulation.anno = ruralpopulation.anno,
          urbpopmap.anno = urbpopmap.anno,
          rurpopmap.anno = rurpopmap.anno,
          pop_ver = pop_ver
        )
      )
      
      save_wf_aggregates <- function(
    i, j,
    wfdb, demand_col,
    subcountry = 1,                 # this aggregate is for the ROB branch
    urbanpopulation.anno, ruralpopulation.anno,
    urbpopmap.anno, rurpopmap.anno,
    pop_ver,
    out_dir = "demand_temp"
      ) {
        stopifnot(subcountry == 1)
        
        # Run once per needed fuel to obtain separate urban/rural demand rasters
        get_dem <- function(fuel_name) {
          res <- compute_fuel_maps(
            fuel_name = fuel_name,
            subcountry = subcountry,
            i = i, j = j,
            wfdb = wfdb,
            demand_col = demand_col,
            urbanpopulation.anno = urbanpopulation.anno,
            ruralpopulation.anno = ruralpopulation.anno,
            urbpopmap.anno = urbpopmap.anno,
            rurpopmap.anno = rurpopmap.anno,
            pop_ver = pop_ver,
            out_dir_pop = "pop_temp",
            out_dir_demand = "demand_temp",
            write_percap = FALSE
          )
          list(urb = res$urb_dem_rast, rur = res$rur_dem_rast)
        }
        
        fuels_avail <- unique(wfdb$fuel)
        
        safe_get_dem <- function(fuel_name) {
          if (fuel_name %in% fuels_avail) {
            get_dem(fuel_name)
          } else {
            NULL
          }
        }
        
        fw  <- safe_get_dem("fuelwood")
        ifw <- safe_get_dem("imp_fuelwood")
        ch  <- safe_get_dem("charcoal")
        ich <- safe_get_dem("imp_charcoal")
        
        # ---- wftons_w: rural fuelwood + rural imp_fuelwood
        wf_w_sum   <- app(c(fw$rur, ifw$rur), fun = sum, na.rm = TRUE)
        
        # ---- wftons_v: urban fw + urban ifw + urban ch + urban ich + rural ch + rural ich
        wf_v_sum   <- app(c(fw$urb, ifw$urb, ch$urb, ich$urb, ch$rur, ich$rur), fun = sum, na.rm = TRUE)
        
        if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
        
        wf_w_path <- file.path(out_dir, paste0(pop_ver, "_", i, "_", j, "_wftons_w.tif"))
        wf_v_path <- file.path(out_dir, paste0(pop_ver, "_", i, "_", j, "_wftons_v.tif"))
        
        terra::writeRaster(wf_w_sum, wf_w_path, filetype = "GTiff", overwrite = TRUE)
        terra::writeRaster(wf_v_sum, wf_v_path, filetype = "GTiff", overwrite = TRUE)
        
        # Quick non-NA checks (optional)
        global(wf_w_sum, fun = "notNA")
        global(wf_v_sum, fun = "notNA")
        
        invisible(list(w_path = wf_w_path, v_path = wf_v_path))
      }
      
      # After sourcing your corrected compute_fuel_maps(v2) with the added returns:
      save_wf_aggregates(
        i = i, j = j,
        wfdb = wfdb, demand_col = demand_col,
        subcountry = 1,
        urbanpopulation.anno = urbanpopulation.anno,
        ruralpopulation.anno = ruralpopulation.anno,
        urbpopmap.anno = urbpopmap.anno,
        rurpopmap.anno = rurpopmap.anno,
        pop_ver = pop_ver,
        out_dir = "demand_temp"
      )
      
    }
    
  }
  
} # End of outer region (i) loop ----

Sys.sleep(3)

# Load country pop and demand rasters and merge into original region ----
# Merge a list of rasters (paths) → single raster written to out_path
.merge_and_write <- function(paths, out_path, fun = c("sum","max"), datatype = "FLT4S") {
  fun <- match.arg(fun)
  if (!length(paths)) return(invisible(NULL))
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  
  # Single file → byte copy
  if (length(paths) == 1) {
    ok <- file.copy(paths[1], out_path, overwrite = TRUE)
    if (!ok) stop(sprintf("file.copy failed: %s -> %s", paths[1], out_path))
    return(invisible(out_path))
  }
  
  # Only compute mass-in/out when using SUM
  sum_in <- NA_real_
  if (fun == "sum") {
    sum_in <- sum(vapply(paths, function(p)
      as.numeric(global(rast(p), "sum", na.rm = TRUE)$sum), numeric(1)))
  }
  
  # Mosaic with requested function ("sum" for continuous, "max" for categorical 1/2)
  rc <- sprc(paths)
  r  <- mosaic(rc, fun = fun)
  
  # Choose safe defaults per datatype
  # - Continuous: FLT4S with NAflag -9999, compression
  # - Categorical: INT1U with NAflag 255 (can't be negative)
  gdal_opts <- c("TILED=YES","COMPRESS=LZW","ZLEVEL=6","BIGTIFF=IF_NEEDED")
  if (datatype == "FLT4S") {
    writeRaster(r, out_path, overwrite = TRUE, filetype = "GTiff",
                datatype = "FLT4S", NAflag = -9999, gdal = c(gdal_opts, "PREDICTOR=3"))
  } else {
    writeRaster(r, out_path, overwrite = TRUE, filetype = "GTiff",
                datatype = datatype, NAflag = 255, gdal = gdal_opts)
  }
  
  if (fun == "sum") {
    out_sum <- as.numeric(global(rast(out_path), "sum", na.rm = TRUE)$sum)
    cat(sprintf("[merge] fun=%s | tiles=%d | in=%s | out=%s | Δ=%.4f%%\n",
                fun, length(paths),
                format(sum_in, big.mark=","), format(out_sum, big.mark=","),
                ifelse(sum_in==0, 0, 100*(out_sum - sum_in)/sum_in)))
  } else {
    cat(sprintf("[merge] fun=%s | tiles=%d | wrote=%s\n",
                fun, length(paths), out_path))
  }
  
  invisible(out_path)
}


# Build a regex that matches: <pop_ver>_<any area>_<k>_<fuel_tag>_SUFFIX.tif
# (pop_ver may contain dashes/letters/numbers/underscores)
.build_pattern <- function(pop_ver, k, fuel_tag, suffix) {
  sprintf("^%s_.+_%s_%s_%s\\.tif$",
          stringr::str_replace_all(pop_ver, "([\\W])", "\\\\\\1"),
          k, fuel_tag, suffix)
}

# For the special wftons_* (they don't have a fuel_tag at the end)
.build_pattern_wf <- function(pop_ver, k, which) {
  sprintf("^%s_.+_%s_%s\\.tif$",
          stringr::str_replace_all(pop_ver, "([\\W])", "\\\\\\1"),
          k, which)  # which ∈ {"wftons_w","wftons_v"}
}

if (subcountry != 1) {
  
  # ---- Main: merge all areas into one raster per fuel per year (subcountry != 1; WHO labels)
  merge_across_areas_wfdb <- function(
    years,                 # vector of years (k)
    pop_ver,               # string tag used in filenames
    in_pop_dir    = "pop_temp",
    in_dem_dir    = "demand_temp",
    out_pop_dir   = "pop_out",
    out_dem_dir   = "demand_out",
    # WHO fuel names as they appear in whodb/wfdb; tags are lowercased in filenames
    fuels_users   = unique(wfdb$fuel), #c("Kerosene","Gas","Electricity","Biomass","Charcoal","Coal"),
    fuels_demand  = unique(wfdb$fuel), #c("Kerosene","Gas","Electricity","Biomass","Charcoal","Coal"),
    include_wftons = TRUE,   # write merged wftons_w / wftons_v when present
    include_poprururb = TRUE # write merged pop and rururb when present
    
  ) {
    #browser()
    # filename tags are lowercased by compute_fuel_maps()
    fuel_tags_users  <- tolower(fuels_users)
    fuel_tags_demand <- tolower(fuels_demand)
    
    for (k in years) {
      #k = 1990
      # ---- USERS (by fuel)
      for (fu_tag in fuel_tags_users) {
        #fu_tag = "charcoal"
        patt_u <- .build_pattern(pop_ver, k, fu_tag, "users")
        files_u <- list.files(in_pop_dir, pattern = patt_u, full.names = TRUE)
        out_u <- file.path(out_pop_dir, sprintf("%s_%s_%s_users.tif", pop_ver, fu_tag, k))
        .merge_and_write(files_u, out_u)
      }
      
      # ---- DEMAND (by fuel)
      for (fu_tag in fuel_tags_demand) {
        #fu_tag = "charcoal"
        patt_d <- .build_pattern(pop_ver, k, fu_tag, "demand")
        files_d <- list.files(in_dem_dir, pattern = patt_d, full.names = TRUE)
        out_d <- file.path(out_dem_dir, sprintf("%s_%s_%s_demand.tif", pop_ver, fu_tag, k))
        .merge_and_write(files_d, out_d)
      }
      
      # ---- Special aggregates (optional)
      if (isTRUE(include_wftons)) {
        # wftons_w
        patt_w <- .build_pattern_wf(pop_ver, k, "wftons_w")
        files_w <- list.files(in_dem_dir, pattern = patt_w, full.names = TRUE)
        out_w <- file.path(out_dem_dir, sprintf("%s_wftons_w_%s.tif", pop_ver, k))
        .merge_and_write(files_w, out_w)
        
        # wftons_v
        patt_v <- .build_pattern_wf(pop_ver, k, "wftons_v")
        files_v <- list.files(in_dem_dir, pattern = patt_v, full.names = TRUE)
        out_v <- file.path(out_dem_dir, sprintf("%s_wftons_v_%s.tif", pop_ver, k))
        .merge_and_write(files_v, out_v)
      }
      
      if (isTRUE(include_poprururb)) {
        # population adjusted
        patt_p <- .build_pattern_wf(pop_ver, k, "popadj")
        files_p <- list.files(in_pop_dir, pattern = patt_p, full.names = TRUE)
        out_p <- file.path(out_pop_dir, sprintf("%s_popadj_%s.tif", pop_ver, k))
        .merge_and_write(files_p, out_p)
        
        # rural urban
        # rural urban (CATEGORICAL: 1 = rural, 2 = urban)
        patt_u <- .build_pattern_wf(pop_ver, k, "rururbR")
        files_u <- list.files(in_pop_dir, pattern = patt_u, full.names = TRUE)
        out_u <- file.path(out_pop_dir, sprintf("%s_rururbR_%s.tif", pop_ver, k))
        .merge_and_write(files_u, out_u, fun = "max", datatype = "INT1U")
      }
      
    }
    invisible(TRUE)
  }
  
  merge_across_areas_wfdb(
    years = annos,
    pop_ver = pop_ver,
    include_wftons = TRUE,
    include_poprururb = TRUE
  )
  
} else if (subcountry == 1) {
  
  # ---- Main: merge all areas into one raster per fuel per year (subcountry == 1)
  merge_across_areas_sub1 <- function(
    years,                 # vector of years (k)
    pop_ver,               # string tag used in filenames
    in_pop_dir    = "pop_temp",
    in_dem_dir    = "demand_temp",
    out_pop_dir   = "pop_out",
    out_dem_dir   = "demand_out",
    fuels_users   = unique(wfdb$fuel), #("fuelwood","imp_fuelwood","charcoal","imp_charcoal",
    #"gas","kerosene","electric","pellets","ethanol","biogas","other"),
    fuels_demand  = unique(wfdb$fuel), #c("fuelwood","imp_fuelwood","charcoal","imp_charcoal",
    #"gas","kerosene","electric","pellets","ethanol","biogas","other"),
    include_wftons = TRUE,   # write merged wftons_w / wftons_v when present
    include_poprururb = TRUE # write merged pop and rururb when present
  ) {
    
    # filename tags are lowercased by compute_fuel_maps()
    fuel_tags_users  <- tolower(fuels_users)
    fuel_tags_demand <- tolower(fuels_demand)
    
    for (k in years) {
      # ---- USERS (by fuel)
      for (fu in fuel_tags_users) {
        patt_u <- .build_pattern(pop_ver, k, fu, "users")
        files_u <- list.files(in_pop_dir, pattern = patt_u, full.names = TRUE)
        out_u <- file.path(out_pop_dir, sprintf("%s_%s_%s_users.tif", pop_ver, fu, k))
        .merge_and_write(files_u, out_u)
      }
      
      # ---- DEMAND (by fuel)
      for (fu in fuel_tags_demand) {
        patt_d <- .build_pattern(pop_ver, k, fu, "demand")
        files_d <- list.files(in_dem_dir, pattern = patt_d, full.names = TRUE)
        out_d <- file.path(out_dem_dir, sprintf("%s_%s_%s_demand.tif", pop_ver, fu, k))
        .merge_and_write(files_d, out_d)
      }
      
      # ---- Special aggregates (optional)
      if (isTRUE(include_wftons)) {
        # wftons_w
        patt_w <- .build_pattern_wf(pop_ver, k, "wftons_w")
        files_w <- list.files(in_dem_dir, pattern = patt_w, full.names = TRUE)
        out_w <- file.path(out_dem_dir, sprintf("%s_wftons_w_%s.tif", pop_ver, k))
        .merge_and_write(files_w, out_w)
        
        # wftons_v
        patt_v <- .build_pattern_wf(pop_ver, k, "wftons_v")
        files_v <- list.files(in_dem_dir, pattern = patt_v, full.names = TRUE)
        out_v <- file.path(out_dem_dir, sprintf("%s_wftons_v_%s.tif", pop_ver, k))
        .merge_and_write(files_v, out_v)
      }
      
      if (isTRUE(include_poprururb)) {
        # population adjusted
        patt_p <- .build_pattern_wf(pop_ver, k, "popadj")
        files_p <- list.files(in_pop_dir, pattern = patt_p, full.names = TRUE)
        out_p <- file.path(out_pop_dir, sprintf("%s_popadj_%s.tif", pop_ver, k))
        .merge_and_write(files_p, out_p)
        
        # rural urban
        # rural urban (CATEGORICAL: 1 = rural, 2 = urban)
        patt_u <- .build_pattern_wf(pop_ver, k, "rururbR")
        files_u <- list.files(in_pop_dir, pattern = patt_u, full.names = TRUE)
        out_u <- file.path(out_pop_dir, sprintf("%s_rururbR_%s.tif", pop_ver, k))
        .merge_and_write(files_u, out_u, fun = "max", datatype = "INT1U")
      }
      
    }
    invisible(TRUE)
  }
  
  # Example: merge all area-i tiles into national mosaics for these years
  merge_across_areas_sub1(
    years = annos,      # e.g., c(2010, 2015, 2018, 2022)
    pop_ver = pop_ver,  # same tag you used earlier
    # directories default to pop_temp/demand_temp → pop_out/demand_out
    include_wftons = TRUE,
    include_poprururb = TRUE
  )
  
}

# Save in a format ingestible by MoFuSS (IDW C++ script) ----
# Important to remove zeros from both; moreover for subcountry == 1 

# Location rasters are lookup tables, not continuous surfaces. Their values must
# therefore be written as signed 32-bit integers. FLT4S cannot represent every
# integer above 2^24 exactly and can silently merge otherwise distinct MoFuSS
# cell IDs in large regional rasters.
.validate_location_id_raster <- function(location_file, expected_ids, label) {
  expected_ids <- as.numeric(expected_ids)
  invalid_expected_ids <- !is.finite(expected_ids) |
    expected_ids != round(expected_ids) |
    expected_ids < -2147483648 |
    expected_ids > 2147483647

  if (any(invalid_expected_ids)) {
    stop(label, " contains IDs that cannot be represented exactly as INT4S.")
  }
  if (anyDuplicated(expected_ids)) {
    stop(label, " contains duplicate IDs before rasterization.")
  }

  location_check <- terra::rast(location_file)
  if (!all(terra::datatype(location_check) == "INT4S")) {
    stop(label, " was not written as an INT4S raster: ", location_file)
  }

  raster_ids <- as.numeric(terra::values(
    location_check,
    mat = FALSE,
    na.rm = TRUE
  ))
  missing_ids <- setdiff(expected_ids, raster_ids)
  unexpected_ids <- setdiff(raster_ids, expected_ids)

  if (length(raster_ids) != length(expected_ids) ||
      anyDuplicated(raster_ids) ||
      length(missing_ids) > 0L ||
      length(unexpected_ids) > 0L) {
    stop(
      label, " lost or changed IDs during rasterization. Expected ",
      length(expected_ids), " unique IDs; found ",
      length(unique(raster_ids)), " unique raster IDs. Missing: ",
      length(missing_ids), "; unexpected: ", length(unexpected_ids), "."
    )
  }

  cat(sprintf(
    "\033[32m[OK] %s: %d unique IDs preserved exactly as INT4S.\033[0m\n",
    label, length(expected_ids)
  ))
  invisible(location_file)
}

## Walking ----
if (optimizeD == 1) {
  keep(annos, optimizeD, , country, countrydir, #endpath,
       githubdir, country, countrydir, demanddir, admindir, emissionsdir, rTempdir, 
       proj_gcs, epsg_gcs, proj_pcs, epsg_pcs, proj_authority, GEE_scale,
       byregion, scenario_ver, pop_ver, mofuss_region, adm0_reg, aoi_poly,
       rTempdir, .validate_location_id_raster,
       sure=TRUE) # shows you which variables will not be removed
  ls()
  gc()
  Sys.sleep(5)
}

wf_w_list <- list.files(path = "demand_out/",
                        pattern = "_wftons_w.*\\.tif$", full.names = TRUE)

wf_w_stNoAdj <- rast(wf_w_list) %>%
  terra::project(paste0(proj_authority,":",epsg_pcs), method= "bilinear", res = GEE_scale) #, threads=TRUE)

# Correction due to projection for year 1 (w), its the same for any year.
w_preProj <- raster(wf_w_list[[1]]) %>% cellStats(stat='sum', na.rm=TRUE)
w_pstProj <- raster(wf_w_stNoAdj[[1]]) %>% cellStats(stat='sum', na.rm=TRUE)
proj_factor_2010w <- w_preProj/w_pstProj
wf_w_st <- wf_w_stNoAdj*proj_factor_2010w

terra::writeRaster(wf_w_st[[1]], paste0("demand_out/wf_w_st_2010_db.tif"),
                   filetype = "GTiff", overwrite = TRUE)
# terra::writeRaster(wf_w_st[[41]], paste0("demand_out/wf_w_st_2050_db.tif"),
#                    filetype = "GTiff", overwrite = TRUE)# Keep turned off

if (optimizeD == 1) {
  gc()
  Sys.sleep(5)
}

# --- 1) Build value dfs (x, y, <layer_value>) ---
if (nlyr(wf_w_st) != length(annos)) {
  stop("Walking raster count does not match the simulation year count")
}

names(wf_w_st) <- paste0("y", annos)
layer_namesw <- names(wf_w_st)

val_dfsw <- map2(seq_len(nlyr(wf_w_st)), layer_namesw, function(k, nm) {
  dfw <- as.data.frame(wf_w_st[[k]], xy = TRUE, na.rm = TRUE)
  val_colw <- setdiff(names(dfw), c("x", "y"))[1]
  transmute(dfw, x, y, !!nm := .data[[val_colw]])
})

# --- 2) Build a single (x, y) -> ID lookup from ANY layer (first non-NA) ---
id_dfw <- map(seq_len(nlyr(wf_w_st)), function(k) {
  dfw <- as.data.frame(wf_w_st[[k]], xy = TRUE, na.rm = TRUE)
  tibble(x = dfw$x, y = dfw$y, ID = as.integer(row.names(dfw)))
}) %>% bind_rows() %>%
  group_by(x, y) %>%
  summarise(ID = dplyr::first(ID), .groups = "drop")   # take the first if duplicated

# --- 3) Align all values by (x, y) ---
wf_w_dbx <- reduce(val_dfsw, full_join, by = c("x", "y"))

# centroids = present in first layer; move it to the end later
first_val_namew <- names(val_dfsw[[1]])[3]
wf_w_dbx <- wf_w_dbx %>%
  mutate(centroids = !is.na(.data[[first_val_namew]]))

# add ID by (x, y)
wf_w_dbx <- wf_w_dbx %>%
  left_join(id_dfw, by = c("x", "y"))

# --- 4) Fill NAs only in value columns (not x,y,centroids,ID) ---
val_colsw <- setdiff(names(wf_w_dbx), c("x", "y", "centroids", "ID"))
wf_w_dbx[val_colsw] <- lapply(wf_w_dbx[val_colsw], function(v) replace(v, is.na(v), 0))

# --- 5) Final column order: ID first, centroids last ---
wf_w_db <- wf_w_dbx %>%
  relocate(ID) %>%
  relocate(centroids, .after = last_col())

# Preview
head(wf_w_db)

id_conflictsw <- map(seq_len(nlyr(wf_w_st)), function(k) {
  dfw <- as.data.frame(wf_w_st[[k]], xy = TRUE, na.rm = TRUE)
  tibble(x = dfw$x, y = dfw$y, ID = as.integer(row.names(dfw)))
}) %>% bind_rows() %>%
  distinct(x, y, ID) %>%
  count(x, y) %>%
  dplyr::filter(n > 1)

if (nrow(id_conflictsw) > 0) {
  warning("Found (x,y) points with multiple IDs across layers; using the first one.")
}

# wf_w_db <- wf_w_dbx %>%
#   relocate(centroids, .after = last_col())

# Output result
head(wf_w_db) # Check the structure
# all.equal(wf_w_db, wf_w_db2)
colnames(wf_w_db) <- c("ID","x","y",paste0(annos,"_fw_w"),"centroids")
# wf_w_db4idw_prezero <- tibble::rownames_to_column(wf_w_db, "ID")
wf_w_db4idw_prezero <- wf_w_db
head(wf_w_db4idw_prezero)

### Take out zero here! Walking ----
# Calculate the row sums for the specified columns
target_colsw <- grep("^[0-9]{4}_fw_w$", names(wf_w_db4idw_prezero))
rowSumsSubsetW <- rowSums(wf_w_db4idw_prezero[, target_colsw])

# Check if all rows were filtered out
if (all(rowSumsSubsetW < 0.1)) {
  # Replace all values in the target columns with 0.2
  wf_w_db4idw <- wf_w_db4idw_prezero
  wf_w_db4idw[, target_colsw] <- 0.2
} else {
  # Keep only rows where the sum is >= 0.1
  wf_w_db4idw <- wf_w_db4idw_prezero[rowSumsSubsetW >= 0.1, ]
}

# Creates a raster based in locs IDs - check the snaps
# ext_wf_w <- ext(wf_w_st[[1]])
# extalign <- terra::align(ext_wf_w, wf_w_st[[1]],snap="near")
newlocs_w <- wf_w_db4idw %>%
  dplyr::select(x,y,ID) %>%
  mutate_at(c('ID'), as.integer)
locs_raster_w <- rast(newlocs_w, type="xyz", crs=crs(wf_w_st[[1]]), digits=0)
terra::writeRaster(locs_raster_w,"to_idw/locs_raster_w.tif",
                   filetype = "GTiff", datatype = "INT4S",
                   gdal = c("COMPRESS=DEFLATE"), overwrite = TRUE)
.validate_location_id_raster(
  "to_idw/locs_raster_w.tif",
  wf_w_db4idw$ID,
  "Walking location raster"
)

wf_w_db4idw %>%
  dplyr::select(!c(x,y,centroids)) %>%
  mutate_if(is.character, as.numeric) %>%
  # mutate(across(where(is.numeric), round, 6)) %>%
  mutate_if(is.numeric, round, 6) %>%
  write.csv(paste0("to_idw/",substr(scenario_ver, 1, 3),"_fwch_w.csv"), row.names=FALSE, quote=FALSE)

## Vehicle ----
if (optimizeD == 1) {
  keep(annos, optimizeD, , country, countrydir, #endpath,
       githubdir, country, countrydir, demanddir, admindir, emissionsdir, rTempdir, 
       proj_gcs, epsg_gcs, proj_pcs, epsg_pcs, proj_authority, GEE_scale,
       byregion, scenario_ver, pop_ver, mofuss_region, adm0_reg, aoi_poly,
       wf_w_db4idw, target_colsw, rTempdir, .validate_location_id_raster,
       sure=TRUE)
  ls()
  gc()
  Sys.sleep(5)
}

wf_v_list <- list.files(path = "demand_out/",
                        pattern = "_wftons_v.*\\.tif$", full.names = TRUE)
wf_v_stNoAdj <- rast(wf_v_list) %>%
  terra::project(paste0(proj_authority,":",epsg_pcs), method= "bilinear", res = GEE_scale) #, threads=TRUE)

# Correction due to projection for year 1 (v), its the same for any year.
v_preProj <- raster(wf_v_list[[1]]) %>% cellStats(stat='sum', na.rm=TRUE)
v_pstProj <- raster(wf_v_stNoAdj[[1]]) %>% cellStats(stat='sum', na.rm=TRUE)
proj_factor_2010v <- v_preProj/v_pstProj
wf_v_st <- wf_v_stNoAdj*proj_factor_2010v

terra::writeRaster(wf_v_st[[1]], paste0("demand_out/wf_v_st_2010_db.tif"),
                   filetype = "GTiff", overwrite = TRUE)
# terra::writeRaster(wf_v_st[[41]], paste0("demand_out/wf_v_st_2050_db.tif"),
#                    filetype = "GTiff", overwrite = TRUE) # Keep turned off

if (optimizeD == 1) {
  gc()
  Sys.sleep(5)
}

# --- 1) Build value dfs (x, y, <layer_value>) ---
if (nlyr(wf_v_st) != length(annos)) {
  stop("Vehicle raster count does not match the simulation year count")
}

names(wf_v_st) <- paste0("y", annos)
layer_namesv <- names(wf_v_st)

val_dfsv <- map2(seq_len(nlyr(wf_v_st)), layer_namesv, function(k, nm) {
  dfv <- as.data.frame(wf_v_st[[k]], xy = TRUE, na.rm = TRUE)
  val_colv <- setdiff(names(dfv), c("x", "y"))[1]
  transmute(dfv, x, y, !!nm := .data[[val_colv]])
})

# --- 2) Build a single (x, y) -> ID lookup from ANY layer (first non-NA) ---
id_dfv <- map(seq_len(nlyr(wf_v_st)), function(k) {
  dfv <- as.data.frame(wf_v_st[[k]], xy = TRUE, na.rm = TRUE)
  tibble(x = dfv$x, y = dfv$y, ID = as.integer(row.names(dfv)))
}) %>% bind_rows() %>%
  group_by(x, y) %>%
  summarise(ID = dplyr::first(ID), .groups = "drop")   # take the first if duplicated

# --- 3) Align all values by (x, y) ---
wf_v_dbx <- reduce(val_dfsv, full_join, by = c("x", "y"))

# centroids = present in first layer; move it to the end later
first_val_namev <- names(val_dfsv[[1]])[3]
wf_v_dbx <- wf_v_dbx %>%
  mutate(centroids = !is.na(.data[[first_val_namev]]))

# add ID by (x, y)
wf_v_dbx <- wf_v_dbx %>%
  left_join(id_dfv, by = c("x", "y"))

# --- 4) Fill NAs only in value columns (not x,y,centroids,ID) ---
val_colsv <- setdiff(names(wf_v_dbx), c("x", "y", "centroids", "ID"))
wf_v_dbx[val_colsv] <- lapply(wf_v_dbx[val_colsv], function(v) replace(v, is.na(v), 0))

# --- 5) Final column order: ID first, centroids last ---
wf_v_db <- wf_v_dbx %>%
  relocate(ID) %>%
  relocate(centroids, .after = last_col())

# Preview
head(wf_v_db)

id_conflictsv <- map(seq_len(nlyr(wf_v_st)), function(k) {
  dfv <- as.data.frame(wf_v_st[[k]], xy = TRUE, na.rm = TRUE)
  tibble(x = dfv$x, y = dfv$y, ID = as.integer(row.names(dfv)))
}) %>% bind_rows() %>%
  distinct(x, y, ID) %>%
  count(x, y) %>%
  dplyr::filter(n > 1)

if (nrow(id_conflictsv) > 0) {
  warning("Found (x,y) points with multiple IDs across layers; using the first one.")
}

# Output result
head(wf_v_db) # Check the structure
# all.equal(wf_w_db, wf_w_db2)
colnames(wf_v_db) <- c("ID","x","y",paste0(annos,"_fw_v"),"centroids")
# wf_v_db4idw_prezero <- tibble::rownames_to_column(wf_v_db, "ID")
wf_v_db4idw_prezero <- wf_v_db
head(wf_v_db4idw_prezero)

### Take out zero here! Vehicle ----
# Calculate the row sums for the specified columns
target_colsv <- grep("^[0-9]{4}_fw_v$", names(wf_v_db4idw_prezero))
rowSumsSubsetV <- rowSums(wf_v_db4idw_prezero[, target_colsv])

# Check if all rows were filtered out
if (all(rowSumsSubsetV < 0.1)) {
  # Replace all values in the target columns with 0.2
  wf_v_db4idw <- wf_v_db4idw_prezero
  wf_v_db4idw[, target_colsv] <- 0.2
} else {
  # Keep only rows where the sum is >= 0.1
  wf_v_db4idw <- wf_v_db4idw_prezero[rowSumsSubsetV >= 0.1, ]
}

# Creates a raster based in locs IDs - check the snaps
# ext_wf_w <- ext(wf_w_st[[1]])
# extalign <- terra::align(ext_wf_w, wf_w_st[[1]],snap="near")
newlocs_v <- wf_v_db4idw %>%
  dplyr::select(x,y,ID) %>%
  mutate_at(c('ID'), as.integer)
locs_raster_v <- rast(newlocs_v, type="xyz", crs=crs(wf_v_st[[1]]), digits=0)
terra::writeRaster(locs_raster_v,"to_idw/locs_raster_v.tif",
                   filetype = "GTiff", datatype = "INT4S",
                   gdal = c("COMPRESS=DEFLATE"), overwrite = TRUE)
.validate_location_id_raster(
  "to_idw/locs_raster_v.tif",
  wf_v_db4idw$ID,
  "Vehicle location raster"
)

wf_v_db4idw %>%
  dplyr::select(!c(x,y,centroids)) %>%
  mutate_if(is.character, as.numeric) %>%
  # mutate(across(where(is.numeric), round, 6)) %>%
  mutate_if(is.numeric, round, 6) %>%
  write.csv(paste0("to_idw/",substr(scenario_ver, 1, 3),"_fwch_v.csv"), row.names=FALSE, quote=FALSE)


# Directional regional HC-cluster bundles ----
#
# Directionality applies only to V (charcoal + urban fuelwood). Importer-country
# demand is pooled and may reach every source pixel in the selected region.
# Each non-importer country's demand is kept in its own job and may reach only
# source pixels inside that country. Walking demand (W) remains one regional job.
# The IDW operation is additive, so the V component outputs must later be summed
# pixel by pixel and year by year. This script only prepares inputs; it does not
# execute IDW.

.single_metadata_value <- function(x, label) {
  values <- unique(trimws(as.character(x)))
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) != 1L) {
    stop(label, " must have exactly one non-missing value in the selected region.")
  }
  values[[1]]
}

.manifest_path <- function(...) {
  gsub("\\\\", "/", file.path(...))
}

.copy_file_or_stop <- function(from, to) {
  if (!file.exists(from)) {
    stop("Copy source does not exist: ", from, ".")
  }
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)

  from_normalized <- tolower(normalizePath(from, winslash = "/", mustWork = TRUE))
  to_normalized <- tolower(normalizePath(to, winslash = "/", mustWork = FALSE))
  if (identical(from_normalized, to_normalized)) {
    return(invisible(to))
  }

  files_are_identical <- function(first, second) {
    if (!file.exists(first) || !file.exists(second)) {
      return(FALSE)
    }
    first_size <- file.info(first)$size
    second_size <- file.info(second)$size
    if (is.na(first_size) || is.na(second_size) || first_size != second_size) {
      return(FALSE)
    }
    hashes <- unname(tools::md5sum(c(first, second)))
    !anyNA(hashes) && identical(hashes[[1L]], hashes[[2L]])
  }

  # Reruns commonly encounter an HC input already being read by another
  # application.  If it is byte-for-byte current, do not attempt an unnecessary
  # Windows overwrite (which can fail solely because that file is open).
  if (files_are_identical(from, to)) {
    message("Reusing identical existing HC input: ", .manifest_path(to))
    return(invisible(to))
  }

  copied <- suppressWarnings(file.copy(from = from, to = to, overwrite = TRUE))
  if (!isTRUE(copied)) {
    stop(
      "Could not replace ", to, " with ", from, ". ",
      "If the destination is open in Excel, R, or another program, close it and rerun."
    )
  }
  if (!files_are_identical(from, to)) {
    stop("Copy verification failed for ", from, " and ", to, ".")
  }
  invisible(to)
}

.write_source_domain_mask <- function(polygons_pcs, template, output_file) {
  if (nrow(polygons_pcs) == 0L) {
    stop("Cannot build an HC source-domain mask from zero polygons.")
  }
  polygons_for_mask <- polygons_pcs
  polygons_for_mask$allowed_source <- 1L
  source_mask <- terra::rasterize(
    polygons_for_mask,
    terra::rast(template),
    field = "allowed_source",
    background = NA,
    touches = FALSE
  )
  names(source_mask) <- "allowed_source"
  terra::writeRaster(
    source_mask,
    output_file,
    filetype = "GTiff",
    datatype = "INT1U",
    gdal = c("COMPRESS=DEFLATE"),
    overwrite = TRUE
  )
  invisible(output_file)
}

.write_directional_idw_bundle <- function(demand_db, channel, template,
                                          job_dir, scenario_prefix,
                                          source_polygons_pcs) {
  channel <- tolower(channel)
  if (!channel %in% c("w", "v")) {
    stop("HC job channel must be 'w' or 'v'.")
  }
  demand_columns <- grep(
    paste0("^[0-9]{4}_fw_", channel, "$"),
    names(demand_db),
    value = TRUE
  )
  required_columns <- c("ID", "x", "y", demand_columns, "centroids")
  missing_columns <- setdiff(required_columns, names(demand_db))
  if (length(demand_columns) == 0L || length(missing_columns) > 0L) {
    stop(
      "HC ", toupper(channel), " job is missing required demand columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  if (nrow(demand_db) == 0L) {
    stop("Cannot write an HC ", toupper(channel), " job with zero demand rows.")
  }
  if (anyNA(demand_db[, c("ID", "x", "y")])) {
    stop("HC ", toupper(channel), " job contains missing ID or coordinate values.")
  }
  if (anyDuplicated(demand_db[, c("x", "y")])) {
    stop("HC ", toupper(channel), " job contains duplicate demand coordinates.")
  }

  dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
  location_file <- file.path(job_dir, paste0("locs_raster_", channel, ".tif"))
  demand_file <- file.path(
    job_dir,
    paste0(scenario_prefix, "_fwch_", channel, ".csv")
  )
  source_mask_file <- file.path(job_dir, "source_domain_mask_raw.tif")

  location_points <- terra::vect(
    as.data.frame(demand_db[, c("x", "y", "ID")]),
    geom = c("x", "y"),
    crs = terra::crs(template)
  )
  location_raster <- terra::rasterize(
    location_points,
    terra::rast(template),
    field = "ID",
    background = NA
  )
  names(location_raster) <- paste0("location_id_", channel)
  terra::writeRaster(
    location_raster,
    location_file,
    filetype = "GTiff",
    datatype = "INT4S",
    gdal = c("COMPRESS=DEFLATE"),
    overwrite = TRUE
  )
  .validate_location_id_raster(
    location_file,
    demand_db$ID,
    paste0("HC ", toupper(channel), " location raster in ", basename(job_dir))
  )

  demand_db[, c("ID", demand_columns), drop = FALSE] %>%
    mutate_if(is.character, as.numeric) %>%
    mutate_if(is.numeric, round, 6) %>%
    write.csv(demand_file, row.names = FALSE, quote = FALSE)

  .write_source_domain_mask(
    source_polygons_pcs,
    template,
    source_mask_file
  )

  list(
    demand_file = demand_file,
    location_file = location_file,
    source_mask_file = source_mask_file,
    demand_columns = demand_columns
  )
}

directional_hc_jobs_created <- FALSE
hc_jobs_dir <- file.path("to_idw", "HC_jobs")

if (identical(byregion, "Regional") && as.integer(aoi_poly) == 0L) {
  required_direction_fields <- c(
    "GID_0", "NAME_0", "mofuss_reg", "Subregion", "RunCode", "CandidateID",
    "CandidateRegionID", "ImporterV", "EvidenceConfidence", "Status"
  )
  missing_direction_fields <- setdiff(required_direction_fields, names(adm0_reg))
  if (length(missing_direction_fields) > 0L) {
    stop(
      "Directional regional demand preparation requires these fields in ",
      "mofuss_regions0.gpkg: ",
      paste(missing_direction_fields, collapse = ", ")
    )
  }

  country_direction_rules <- as.data.frame(adm0_reg) %>%
    dplyr::select(dplyr::all_of(required_direction_fields)) %>%
    dplyr::distinct()

  if (nrow(country_direction_rules) < 2L) {
    stop("Directional regional demand preparation requires a multi-country region.")
  }
  if (anyDuplicated(country_direction_rules$GID_0)) {
    stop("The selected region contains duplicate country direction records.")
  }

  character_fields <- setdiff(required_direction_fields, "ImporterV")
  for (field in character_fields) {
    country_direction_rules[[field]] <- trimws(
      as.character(country_direction_rules[[field]])
    )
    if (anyNA(country_direction_rules[[field]]) ||
        any(!nzchar(country_direction_rules[[field]]))) {
      stop("Directional field ", field, " contains missing or blank values.")
    }
  }

  importer_values <- trimws(as.character(country_direction_rules$ImporterV))
  if (anyNA(importer_values) || any(!importer_values %in% c("0", "1"))) {
    stop("ImporterV must contain only 0 or 1 in the selected region.")
  }
  country_direction_rules$ImporterV <- as.integer(importer_values)

  region_run_code <- .single_metadata_value(
    country_direction_rules$RunCode,
    "RunCode"
  )
  model_region_code <- .single_metadata_value(
    country_direction_rules$mofuss_reg,
    "mofuss_reg"
  )
  candidate_id <- .single_metadata_value(
    country_direction_rules$CandidateID,
    "CandidateID"
  )
  candidate_region_id <- .single_metadata_value(
    country_direction_rules$CandidateRegionID,
    "CandidateRegionID"
  )
  region_name <- .single_metadata_value(
    country_direction_rules$Subregion,
    "Subregion"
  )
  selected_run_code <- trimws(as.character(mofuss_region))
  if (length(selected_run_code) != 1L ||
      !identical(model_region_code, selected_run_code)) {
    stop(
      "Selected region parameter ('", paste(selected_run_code, collapse = ", "),
      "') does not exactly match mofuss_reg ('", model_region_code, "')."
    )
  }

  if (dir.exists(hc_jobs_dir)) {
    unlink(hc_jobs_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(hc_jobs_dir, recursive = TRUE, showWarnings = FALSE)

  all_iso3 <- sort(country_direction_rules$GID_0)
  importer_iso3 <- sort(
    country_direction_rules$GID_0[country_direction_rules$ImporterV == 1L]
  )
  domestic_iso3 <- sort(
    country_direction_rules$GID_0[country_direction_rules$ImporterV == 0L]
  )
  all_iso3_text <- paste(all_iso3, collapse = ";")
  scenario_prefix <- substr(scenario_ver, 1, 3)

  country_direction_rules <- country_direction_rules %>%
    dplyr::arrange(GID_0) %>%
    dplyr::mutate(
      DemandRule = ifelse(
        ImporterV == 1L,
        "regional_pool",
        "domestic_only"
      ),
      AllowedSourceISO3 = ifelse(
        ImporterV == 1L,
        all_iso3_text,
        GID_0
      )
    )
  write.csv(
    country_direction_rules,
    file.path(hc_jobs_dir, "country_direction_rules.csv"),
    row.names = FALSE,
    quote = TRUE,
    na = ""
  )

  adm0_reg_pcs <- terra::project(adm0_reg, terra::crs(wf_v_st[[1]]))
  country_index_lookup <- country_direction_rules %>%
    dplyr::select(GID_0) %>%
    dplyr::mutate(country_index = dplyr::row_number())
  adm0_reg_pcs$country_index <- country_index_lookup$country_index[
    match(adm0_reg_pcs$GID_0, country_index_lookup$GID_0)
  ]
  if (anyNA(adm0_reg_pcs$country_index)) {
    stop("Could not attach country indices to every selected regional polygon.")
  }

  country_index_raster_centres <- terra::rasterize(
    adm0_reg_pcs,
    terra::rast(wf_v_st[[1]]),
    field = "country_index",
    background = NA,
    touches = FALSE
  )
  # Bilinear reprojection can leave a narrow fringe of positive demand whose
  # cell centre falls just outside the source polygon. Preserve centre-based
  # assignments and use touched polygons only to fill that otherwise-unassigned
  # outer fringe; this does not overwrite internal-border assignments.
  country_index_raster_touches <- terra::rasterize(
    adm0_reg_pcs,
    terra::rast(wf_v_st[[1]]),
    field = "country_index",
    background = NA,
    touches = TRUE
  )
  country_index_raster <- terra::cover(
    country_index_raster_centres,
    country_index_raster_touches
  )
  names(country_index_raster) <- "country_index"
  demand_cells <- terra::cellFromXY(
    country_index_raster,
    as.matrix(wf_v_db4idw[, c("x", "y")])
  )
  if (anyNA(demand_cells)) {
    stop("Some positive V-demand coordinates fall outside the regional raster.")
  }
  extracted_country_index <- terra::extract(
    country_index_raster,
    demand_cells
  )
  if (is.data.frame(extracted_country_index) || is.matrix(extracted_country_index)) {
    if ("country_index" %in% colnames(extracted_country_index)) {
      extracted_country_index <- extracted_country_index[, "country_index"]
    } else {
      extracted_country_index <- extracted_country_index[, ncol(extracted_country_index)]
    }
  }
  extracted_country_index <- as.integer(extracted_country_index)

  # A bilinearly projected demand surface can retain positive values in a very
  # narrow outer fringe even when the projected cell centre and the entire cell
  # fall just outside the GADM polygon. Assign only those sub-pixel cases to the
  # nearest selected country. Anything farther than one output-cell diagonal is
  # treated as a genuine selection/alignment error and still fails fast.
  unassigned_demand_rows <- which(is.na(extracted_country_index))
  if (length(unassigned_demand_rows) > 0L) {
    if (terra::is.lonlat(wf_v_st[[1]])) {
      stop("Directional V-demand assignment requires a projected raster CRS.")
    }
    unassigned_points <- sf::st_as_sf(
      data.frame(
        demand_row = unassigned_demand_rows,
        x = wf_v_db4idw$x[unassigned_demand_rows],
        y = wf_v_db4idw$y[unassigned_demand_rows]
      ),
      coords = c("x", "y"),
      crs = sf::st_crs(terra::crs(wf_v_st[[1]]))
    )
    country_polygons_sf <- sf::st_as_sf(adm0_reg_pcs)
    nearest_country_rows <- sf::st_nearest_feature(
      unassigned_points,
      country_polygons_sf
    )
    nearest_country_distance <- as.numeric(sf::st_distance(
      unassigned_points,
      country_polygons_sf[nearest_country_rows, ],
      by_element = TRUE
    ))
    one_cell_diagonal <- sqrt(sum(terra::res(wf_v_st[[1]])^2))
    outside_tolerance <- !is.finite(nearest_country_distance) |
      nearest_country_distance > one_cell_diagonal * 1.01
    if (any(outside_tolerance)) {
      stop(
        sum(outside_tolerance),
        " positive V-demand origin(s) remain outside every selected country ",
        "by more than one raster-cell diagonal (", round(one_cell_diagonal, 2),
        " map units). This indicates a region or raster alignment error."
      )
    }
    polygon_country_index <- as.integer(
      as.data.frame(adm0_reg_pcs)$country_index
    )
    extracted_country_index[unassigned_demand_rows] <-
      polygon_country_index[nearest_country_rows]
    message(
      "Assigned ", length(unassigned_demand_rows),
      " V-demand origin(s) in the one-cell reprojection fringe to the nearest ",
      "country (maximum distance ",
      round(max(nearest_country_distance), 2), " map units)."
    )
  }

  if (length(extracted_country_index) != nrow(wf_v_db4idw) ||
      anyNA(extracted_country_index) ||
      any(!extracted_country_index %in% country_index_lookup$country_index)) {
    stop(
      "Every positive V-demand origin must be assigned to exactly one ",
      "country in the selected region."
    )
  }
  wf_v_db4idw$GID_0 <- country_index_lookup$GID_0[
    match(extracted_country_index, country_index_lookup$country_index)
  ]

  manifest_rows <- list()
  partitioned_v_dbs <- list()
  add_manifest_row <- function(job_id, channel, demand_iso3,
                               importer_v, allowed_source_iso3,
                               rule, bundle = NULL, status = "ready") {
    demand_rows <- if (is.null(bundle)) 0L else bundle$demand_rows
    first_year_demand <- if (is.null(bundle)) 0 else bundle$first_year_demand
    all_years_demand <- if (is.null(bundle)) 0 else bundle$all_years_demand
    manifest_rows[[length(manifest_rows) + 1L]] <<- data.frame(
      CandidateID = candidate_id,
      CandidateRegionID = candidate_region_id,
      MoFuSSRegion = model_region_code,
      RegionRunCode = region_run_code,
      RegionName = region_name,
      JobID = job_id,
      Channel = toupper(channel),
      DemandISO3 = paste(demand_iso3, collapse = ";"),
      ImporterV = importer_v,
      AllowedSourceISO3 = paste(allowed_source_iso3, collapse = ";"),
      DirectionRule = rule,
      DemandRows = demand_rows,
      FirstYearDemandTons = first_year_demand,
      AllYearsDemandTons = all_years_demand,
      DemandTable = if (is.null(bundle)) "" else bundle$demand_file,
      LocationsRaster = if (is.null(bundle)) "" else bundle$location_file,
      SourceDomainMaskRaw = if (is.null(bundle)) "" else bundle$source_mask_file,
      RunOnHCCluster = identical(status, "ready"),
      OutputRole = if (tolower(channel) == "w") {
        "regional_W_pressure"
      } else {
        "directional_V_pressure_component"
      },
      CombineOperation = if (tolower(channel) == "w") {
        "use_directly"
      } else {
        "pixelwise_sum_by_year"
      },
      Status = status,
      stringsAsFactors = FALSE
    )
  }

  # W is deliberately unchanged: one demand table and the full regional domain.
  w_job_id <- paste0("W_", region_run_code)
  w_job_dir <- file.path(hc_jobs_dir, w_job_id)
  dir.create(w_job_dir, recursive = TRUE, showWarnings = FALSE)
  w_demand_source <- file.path(
    "to_idw",
    paste0(scenario_prefix, "_fwch_w.csv")
  )
  w_location_source <- file.path("to_idw", "locs_raster_w.tif")
  w_demand_target <- file.path(w_job_dir, basename(w_demand_source))
  w_location_target <- file.path(w_job_dir, basename(w_location_source))
  w_source_mask_target <- file.path(w_job_dir, "source_domain_mask_raw.tif")
  .copy_file_or_stop(w_demand_source, w_demand_target)
  .copy_file_or_stop(w_location_source, w_location_target)
  .write_source_domain_mask(adm0_reg_pcs, wf_v_st[[1]], w_source_mask_target)
  w_values <- wf_w_db4idw[, target_colsw, drop = FALSE]
  w_bundle <- list(
    demand_rows = nrow(wf_w_db4idw),
    first_year_demand = sum(w_values[[1]], na.rm = TRUE),
    all_years_demand = sum(as.matrix(w_values), na.rm = TRUE),
    demand_file = .manifest_path(w_job_id, basename(w_demand_target)),
    location_file = .manifest_path(w_job_id, basename(w_location_target)),
    source_mask_file = .manifest_path(w_job_id, basename(w_source_mask_target))
  )
  add_manifest_row(
    w_job_id, "w", all_iso3, NA_integer_, all_iso3,
    "regional_W_unchanged", w_bundle
  )

  write_v_job <- function(job_id, demand_iso3, importer_v,
                          allowed_source_iso3, rule) {
    job_db <- wf_v_db4idw %>%
      dplyr::filter(GID_0 %in% demand_iso3)
    if (nrow(job_db) == 0L) {
      add_manifest_row(
        job_id, "v", demand_iso3, importer_v, allowed_source_iso3,
        rule, status = "skipped_no_positive_V_demand"
      )
      return(invisible(NULL))
    }

    source_polygons <- adm0_reg_pcs[
      adm0_reg_pcs$GID_0 %in% allowed_source_iso3,
    ]
    bundle_files <- .write_directional_idw_bundle(
      demand_db = job_db %>% dplyr::select(-GID_0),
      channel = "v",
      template = wf_v_st[[1]],
      job_dir = file.path(hc_jobs_dir, job_id),
      scenario_prefix = scenario_prefix,
      source_polygons_pcs = source_polygons
    )
    v_values <- job_db[, target_colsv, drop = FALSE]
    bundle <- list(
      demand_rows = nrow(job_db),
      first_year_demand = sum(v_values[[1]], na.rm = TRUE),
      all_years_demand = sum(as.matrix(v_values), na.rm = TRUE),
      demand_file = .manifest_path(job_id, basename(bundle_files$demand_file)),
      location_file = .manifest_path(job_id, basename(bundle_files$location_file)),
      source_mask_file = .manifest_path(job_id, basename(bundle_files$source_mask_file))
    )
    partitioned_v_dbs[[length(partitioned_v_dbs) + 1L]] <<- job_db
    add_manifest_row(
      job_id, "v", demand_iso3, importer_v, allowed_source_iso3,
      rule, bundle
    )
    invisible(NULL)
  }

  if (length(importer_iso3) > 0L) {
    write_v_job(
      paste0("V_", region_run_code, "_IMPORTERS"),
      importer_iso3,
      1L,
      all_iso3,
      "importer_demand_regional_sources"
    )
  }
  for (domestic_iso in domestic_iso3) {
    write_v_job(
      paste0("V_", domestic_iso, "_DOMESTIC"),
      domestic_iso,
      0L,
      domestic_iso,
      "non_importer_demand_domestic_sources_only"
    )
  }

  if (length(partitioned_v_dbs) == 0L) {
    stop("No positive V demand was assigned to an HC directional job.")
  }
  partitioned_v <- dplyr::bind_rows(partitioned_v_dbs)
  original_v_totals <- colSums(
    wf_v_db4idw[, target_colsv, drop = FALSE],
    na.rm = TRUE
  )
  partitioned_v_totals <- colSums(
    partitioned_v[, target_colsv, drop = FALSE],
    na.rm = TRUE
  )
  if (nrow(partitioned_v) != nrow(wf_v_db4idw) ||
      !isTRUE(all.equal(
        as.numeric(original_v_totals),
        as.numeric(partitioned_v_totals),
        tolerance = 1e-10,
        check.attributes = FALSE
      ))) {
    stop("Directional V jobs do not exactly conserve the prepared regional demand.")
  }

  hc_manifest <- dplyr::bind_rows(manifest_rows)
  write.csv(
    hc_manifest,
    file.path(hc_jobs_dir, "HC_job_manifest.csv"),
    row.names = FALSE,
    quote = TRUE,
    na = ""
  )
  writeLines(
    c(
      paste0("MoFuSS directional IDW handoff for ", region_run_code),
      "",
      "Prepared by 3_demand4IDW_v9.R. IDW was not executed.",
      "Run every manifest row whose RunOnHCCluster value is TRUE.",
      "W remains one regional job and its output is used directly.",
      paste0(
        "V importer demand may use every country in ", region_run_code,
        "; each non-importer V job is restricted to its own country."
      ),
      "Apply source_domain_mask_raw.tif to the corresponding source/friction domain.",
      "Sum all directional V pressure outputs pixel by pixel for each year.",
      "Do not add the top-level compatibility V bundle to those components.",
      "The top-level to_idw files are retained only for the unchanged legacy workflow.",
      "HC_job_manifest.csv is the authoritative job list and combination rule."
    ),
    file.path(hc_jobs_dir, "README_IDW_HANDOFF.txt")
  )

  directional_hc_jobs_created <- TRUE
  message(
    "Prepared ", sum(hc_manifest$RunOnHCCluster),
    " HC IDW jobs for ", region_run_code, ": ",
    paste(hc_manifest$JobID[hc_manifest$RunOnHCCluster], collapse = ", ")
  )
}



# Copy to MoFuSS ----

setwd(countrydir)

unlink(paste0(countrydir,"/In/DemandScenarios/*.*"), 
       recursive= TRUE, force=TRUE)
unlink(paste0(countrydir,"/In/*.*"), 
       recursive= TRUE, force=TRUE)
Sys.sleep(3)

file.copy(from=paste0(demanddir,"/to_idw/locs_raster_w.tif"),
          to=paste0(countrydir,"/In/DemandScenarios"),
          overwrite = TRUE)

file.copy(from=paste0(demanddir,"/to_idw/locs_raster_v.tif"),
          to=paste0(countrydir,"/In/DemandScenarios"),
          overwrite = TRUE)

file.copy(from=paste0(demanddir,"/to_idw/",substr(scenario_ver, 1, 3),"_fwch_w.csv"),
          to=paste0(countrydir,"/In/DemandScenarios"),
          overwrite = TRUE)

file.copy(from=paste0(demanddir,"/to_idw/",substr(scenario_ver, 1, 3),"_fwch_v.csv"),
          to=paste0(countrydir,"/In/DemandScenarios"),
          overwrite = TRUE)

target_hc_jobs_dir <- file.path(countrydir, "In", "DemandScenarios", "HC_jobs")
if (dir.exists(target_hc_jobs_dir)) {
  unlink(target_hc_jobs_dir, recursive = TRUE, force = TRUE)
}

if (isTRUE(directional_hc_jobs_created)) {
  source_hc_jobs_dir <- normalizePath(
    file.path(demanddir, "to_idw", "HC_jobs"),
    winslash = "/",
    mustWork = TRUE
  )
  hc_job_files <- list.files(
    source_hc_jobs_dir,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE,
    include.dirs = FALSE
  )
  if (length(hc_job_files) == 0L) {
    stop("Directional HC job directory was created but contains no files.")
  }
  relative_hc_job_files <- substring(
    hc_job_files,
    nchar(source_hc_jobs_dir) + 2L
  )
  target_hc_job_files <- file.path(target_hc_jobs_dir, relative_hc_job_files)
  invisible(lapply(
    unique(dirname(target_hc_job_files)),
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  ))
  copied_hc_job_files <- file.copy(
    from = hc_job_files,
    to = target_hc_job_files,
    overwrite = TRUE
  )
  if (!all(copied_hc_job_files)) {
    stop(
      "Could not copy every directional HC job file into In/DemandScenarios/HC_jobs."
    )
  }
}


# Cubify results for easy handling ----
if (cube_rasters == 1){
  
  library(terra)
  
  build_fuel_cubes <- function(in_dir, out_subdir) {
    
    ## Create output directory----
    out_dir <- file.path(in_dir, out_subdir)
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
    }
    
    ## List tif files----
    tif_files <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)
    
    # ignore already-created cubes
    tif_files <- tif_files[!grepl("_cube\\.tif$", tif_files)]
    
    file_names <- basename(tif_files)
    
    pattern <- "^WorldPop_(.+)_([0-9]{4})_(demand|users)\\.tif$"
    
    matches <- regexec(pattern, file_names)
    parsed  <- regmatches(file_names, matches)
    
    ok <- lengths(parsed) > 0
    tif_files  <- tif_files[ok]
    file_names <- file_names[ok]
    parsed     <- parsed[ok]
    
    info <- data.frame(
      file = tif_files,
      fuel = sapply(parsed, `[`, 2),
      year = as.integer(sapply(parsed, `[`, 3)),
      type = sapply(parsed, `[`, 4),
      stringsAsFactors = FALSE
    )
    
    groups <- split(info, list(info$type, info$fuel), drop = TRUE)
    
    for (g in groups) {
      
      g <- g[order(g$year), ]
      
      fuel_i <- unique(g$fuel)
      type_i <- unique(g$type)
      
      message("Building cube: ", fuel_i, " (", type_i, ")")
      
      r_cube <- rast(g$file)
      names(r_cube) <- paste0("y", g$year)
      
      out_file <- file.path(
        out_dir,
        paste0("WorldPop_", fuel_i, "_", type_i, "_cube.tif")
      )
      
      writeRaster(
        r_cube,
        out_file,
        overwrite = TRUE,
        wopt = list(
          gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES")
        )
      )
      
      message("Saved: ", out_file)
    }
  }
  
  ## Run for both folders ----
  build_fuel_cubes(demand_dir, "cube_raster_dem")
  build_fuel_cubes(pop_dir,    "cube_raster_pop")
  
}

# End of script ----

