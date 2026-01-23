# Copyright 2025 Stockholm Environment Institute ----

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# MoFuSS
# Version 5
# Date: Dec 2025

# 2dolist ----
# FIX THE MASK ISSUE WITH LINUX, THAT WAS PATCHED FOR THE MOMENT!
# ALLOW OTHER SCENARIOS: Start in line 183
# VERY IMPORTANT TO DEFINE A SOLID WORKFLOW FOR REGIONALIZING COUNTRIES, e.g. Zambia
# Remove any reference to HSRL 

# Internal parameters ----
optimizeD = 0
temdirdefined = 1

# Load libraries ----
library(conflicted)

library(terra)
# terraOptions(steps = 55)
if (temdirdefined == 1) {
  terraOptions(tempdir = rTempdir)
  # List all files and directories inside the folder
  contents <- list.files(rTempdir, full.names = TRUE, recursive = TRUE)
  # Delete the contents but keep the folder
  unlink(contents, recursive = TRUE, force = TRUE)
}
# terraOptions(memfrac=0.9)
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

# Reads WHO dataset
if (subcountry != 1) {
  whodb <- read_excel("demand_in/A_LMIC_Estimates_2050_popmedian.xlsx")
  # undb <- read_excel("admin_regions/UN_Rural-Urban_Pop_projections_formatted.xlsx") # https://population.un.org/wpp/Download/Standard/Population/
  whodb <- whodb %>%
    # Remove anything starting with "total" (any capitalization)
    dplyr::filter(!str_detect(fuel, regex("^total", ignore_case = TRUE))) %>%
    
    # Remove rows where area == "Overall" (any capitalization)
    dplyr::filter(!str_detect(area, regex("^overall$", ignore_case = TRUE))) %>%
    
    mutate(
      # 1. trim spaces and lowercase everything first
      fuel = tolower(trimws(fuel)),
      
      # 2. replace "biomass" → "fuelwood"
      fuel = if_else(fuel == "biomass", "fuelwood", fuel),
      
      # 3. replace "electric" → "electricity"
      fuel = if_else(fuel == "electric", "electricity", fuel),
      
      # 4. Capitalize first letter after all replacements
      fuel = str_to_title(fuel)
    )
terra::unique(whodb$fuel)
terra::unique(whodb$area)
}

getwd()
poprast <- paste0("demand_in/",pop_map_name) 

# Define scenarios ----
detect_delimiter <- function(file) {
  line1 <- readLines(file, n = 1)
  if (stringr::str_detect(line1, ";")) return(";")
  return(",")
}

read_wfdb <- function(file) {
  delim <- detect_delimiter(file)
  readr::read_delim(file, delim = delim, show_col_types = FALSE)
}

# Define scenarios ----
if (scenario_ver == "BaU") {
  wfdb <- read_wfdb("demand_in/cons_fuels_years.csv")
  
} else if (scenario_ver == "ICS") {
  wfdb <- read_wfdb("demand_in/cons_fuels_years_proj.csv")
  
} else if (scenario_ver == "BaU_vehicle_only") {
  wfdb <- read_wfdb("demand_in/cons_fuels_years_charc_and_urb_fw_only.csv")
  
} else if (scenario_ver == "BaU_walking_only") {
  wfdb <- read_wfdb("demand_in/cons_fuels_years_rural_fw_only.csv")
  
} else if (scenario_ver == "BaU_lusaka_notlusaka") {
  wfdb <- read_wfdb("demand_in/cons_fuels_years_BAU_Lusaka-NotLusaka.csv")
  
} else if (scenario_ver == "ICS1_lusaka_notlusaka") {
  wfdb <- read_wfdb("demand_in/cons_fuels_years_Proj1_Lusaka-NotLusaka.csv")
  
} else if (scenario_ver == "ICS2_lusaka_notlusaka") {
  wfdb <- read_wfdb("demand_in/cons_fuels_years_Proj2_Lusaka-NotLusaka.csv")
  
} else if (scenario_ver == "ICS3_lusaka_notlusaka") {
  wfdb <- read_wfdb("demand_in/cons_fuels_years_Proj3_Lusaka-NotLusaka.csv")
  
} else if (scenario_ver == "MWI_BAU_fuel_cons") {
  wfdb <- read_wfdb("demand_in/MWI_BAU_fuel_cons.csv")
  
} else if (scenario_ver == "MWI_BAU_chyield") {
  wfdb <- read_wfdb("demand_in/MWI_BAU_fuel_cons_chyields4unfccc.csv")
  
}
unique(wfdb$fuel)

# Make all fuel types Title caps and add consecutive numbers to GID_0 _1 _2 etc
wfdb <- wfdb %>%
  mutate(
    # 1. trim spaces and lowercase everything first
    fuel = tolower(trimws(fuel)),
    
    # 2. replace "biomass" → "fuelwood"
    fuel = if_else(fuel == "biomass", "fuelwood", fuel),
    
    # 3. replace "electric" → "electricity"
    fuel = if_else(fuel == "electric", "electricity", fuel),
    
    # 4. capitalize first letter only ONCE, after all replacements
    fuel = str_to_title(fuel),
    
    # 5. add consecutive numbers to GID_0 _1 _2 etc
    iso3 = paste0(iso3, "_", dense_rank(country))
  )

unique(wfdb$fuel)
head(wfdb)
print(scenario_ver) # save as text to recover later down the river

# Comment this eventually
if (scenario_ver %in% c("BaU", "ICS")) {
  if (!identical(unique(wfdb$fuel), unique(whodb$fuel))) {
    stop("Fuel categories do not match for this subcountry.")
  }
}

setwd(countrydir)
write.table(scenario_ver, "LULCC/TempTables/scenario_ver.txt")
SceVer <- read.table("LULCC/TempTables/scenario_ver.txt") %>% .$x
write.table(byregion, "LULCC/TempTables/region_ext.txt")
reg_ext <- read.table("LULCC/TempTables/region_ext.txt") %>% .$x
setwd(demanddir)

# Time period
annos.list2 <- c(2010:end_year) 
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
  # kml_file_path <- Sys.glob(paste0(countrydir,"/LULCC/SourceData/InVector_GCS/",aoi_poly_file))
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
  kml_file_path <- Sys.glob(paste0(countrydir,"/LULCC/SourceData/InVector_GCS/",aoi_poly_file))
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
  
  totpopWHO <- whodb %>% 
    dplyr::filter(year == yr) %>%
    dplyr::filter(area %in% c("Urban", "Rural")) %>%
    group_by(iso3) %>% 
    summarise(
      sum_pop = sum(pop) * 1000,
      .groups = "drop"
    )

  # Reads furb in 2018 from WHO dataset
  whodb_join <- whodb %>%
    dplyr::select(iso3, country) %>%
    terra::unique()
  
  furb_who <- whodb %>%
    dplyr::filter(grepl(yr, year)) %>%
    dplyr::filter(grepl('Urban', area)) %>%
    group_by(iso3) %>% 
    summarise(urb_pop=sum(pop)*1000,
              .groups = 'drop') %>%
    left_join(totpopWHO, ., by="iso3") %>% 
    mutate(furb = urb_pop/sum_pop) %>%
    left_join(whodb_join, ., by = "iso3") %>%
    dplyr::select(iso3, country, furb) %>%
    rename(GID_0 = iso3,
           NAME_0 = country)
  
  # furb_who %>%
  #   dplyr::filter(GID_0 == "ZMB")
  
} else if (subcountry == 1) {
  
  totpoprob <- wfdb %>%
    dplyr::filter(grepl(yr, year)) %>%
    dplyr::filter(area %in% c("Urban", "Rural")) %>%
    group_by(iso3) %>%
    summarise(sum_pop=sum(people)*1000, 
              .groups = 'drop')
  
  furb_rob <- wfdb %>%
    dplyr::filter(year == yr, area %in% c("Urban", "Rural")) %>%
    dplyr::group_by(iso3, country) %>%   # <-- key fix: include country (split)
    dplyr::summarise(
      urb_pop = sum(people[area == "Urban"], na.rm = TRUE) * 1000,
      rur_pop = sum(people[area == "Rural"], na.rm = TRUE) * 1000,
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
    pop0_reg <- mask(pop0_K, adm0_reg) #THIS BREAKS IN UBUNTU
  } else if(os == "Linux") {
    pop0_reg <- pop0_K
  }
  plot(pop0_reg, main=paste0("Overlapping with your AoI")) #,mofuss_region2))
  lines(adm0_reg)
  Sys.sleep(10)
  
} else if (byregion == "Global" & aoi_poly == 0) {
  print("***NOW RUNNING GLOBAL DEMAND SCENARIOS - Global***")
  adm0_reg <- mofuss_regions0_gpkg
  pop0_K <- crop(pop0, ext(adm0_reg) + .01)
  if (os == "Windows") {
    pop0_reg <- mTeask(pop0_K, adm0_reg) #THIS BREAKS IN UBUNTU
  } else if(os == "Linux") {
    pop0_reg <- pop0_K
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
    pop0_reg <- mask(pop0_K, adm0_reg) #THIS BREAKS IN UBUNTU
  } else if(os == "Linux") {
    pop0_reg <- pop0_K
  }
  plot(pop0_reg,main=c("Region to be processed"))
  lines(adm0_reg)
  Sys.sleep(10)
  
} else if (byregion == "Regional" & aoi_poly == 0) {
  print("***NOW RUNNING REGION DEMAND SCENARIOS - Regional***")
  adm0_reg <- mofuss_regions0_gpkg %>% 
    dplyr::filter(grepl(mofuss_region, mofuss_reg))
  # plot(pop0)
  # lines(adm0_reg, lwd=2)
  pop0_K <- crop(pop0, ext(adm0_reg) + .01)
  if (os == "Windows") {
    pop0_reg <- mask(pop0_K, adm0_reg) # THIS BREAKS IN UBUNTU NA WINDOWS AS ELL
  } else if(os == "Linux") {
    pop0_reg <- pop0_K
  }
  plot(pop0_reg,main=c("Region to be processed"))
  lines(adm0_reg)
  Sys.sleep(10)
  
} else if (byregion == "Country" & aoi_poly == 0 & subcountry != 1) {
  print("***NOW RUNNING COUNTRY DEMAND SCENARIOS - Country***")
  adm0_reg <- mofuss_regions0_gpkg %>% 
    dplyr::filter(GID_0 == mofuss_region) # Check if multiple countries or values is doable
  pop0_K <- crop(pop0, ext(adm0_reg) + .01)
  if (os == "Windows") {
    pop0_reg <- mask(pop0_K, adm0_reg) #THIS BREAKS IN UBUNTU
  } else if(os == "Linux") {
    pop0_reg <- pop0_K
  }
  plot(pop0_reg, main=paste0("You selected ",mofuss_region))
  lines(adm0_reg)
  Sys.sleep(10)
  
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
    pop0_reg <- mask(pop0_K, adm0_reg) #THIS BREAKS IN UBUNTU
  } else if(os == "Linux") {
    pop0_reg <- pop0_K
  }
  plot(pop0_reg, main=paste0("You selected ",region2BprocessedCtry_iso))
  lines(adm0_reg)
  Sys.sleep(10)
  
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
  # i = "SLV"
  print(i)
  if (subcountry != 1) {
    ctry_furb <- furb_who %>%
      dplyr::filter(GID_0 == i) %>%
      pull(furb)
    who_ctry_pop <- totpopWHO %>%
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
  plot(pop0_ctry_ras, main=i, xlab = "Long", ylab = "Lat")
  lines(ctry_vector, lwd=0.2)
  Sys.sleep(5)
  dev.off()
  
  totpop <- global(pop0_ctry_ras, "sum", na.rm=TRUE) %>%
    pull(sum)
  urbpop <- totpop * ctry_furb 
  rurpop <- totpop - urbpop
  totpop
  urbpop
  rurpop
  
  if (subcountry != 1) {
    pop0_ctry_rasadj <- pop0_ctry_ras*who_ctry_pop/totpop
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
    # j=2050
    
    gc()
    terraOptions(memfrac=0.9)
    print(j)
    
    if (subcountry != 1) {
      
      furb_who.anno <- whodb %>%
        dplyr::filter(
          iso3 == i,
          grepl(j, year)
        ) %>%
        group_by(iso3) %>%
        summarise(
          urb_frac = sum(pop[area == "Urban"]) /
            sum(pop[area %in% c("Urban", "Rural")]),
          .groups = "drop"
        ) %>%
        pull(urb_frac)
      
      totpopWHO_annual <- whodb %>% 
        dplyr::filter(area %in% c("Urban", "Rural")) %>%
        group_by(iso3, year) %>% 
        summarise(
          sum_pop = sum(pop) * 1000,
          .groups = 'drop'
        )
      
      who_ctry_pop_annual <- totpopWHO_annual %>%
        dplyr::filter(iso3 == i) %>%
        dplyr::filter(year == j) %>%
        pull(sum_pop)
      
      pop0_ctry_rasadj.anno<- pop0_ctry_ras*who_ctry_pop_annual/totpop
      totpopadj.anno <- global(pop0_ctry_rasadj.anno, "sum", na.rm=TRUE) %>%
        pull(sum)
      urbpopadj.anno <- totpopadj.anno * furb_who.anno
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
          urb_frac = sum(people[grepl("Urban", area)]) / sum(people),
          .groups = "drop"
        ) %>%
        dplyr::pull(urb_frac)
      
      totpopROB_annual <- wfdb %>% 
        group_by(iso3,year) %>% 
        summarise(sum_pop=sum(people)*1000,
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
    if (pop_ver == "HSRL") {
      vec.anno <- as_tibble(pop0_ctry_rasadj.anno, na.rm = TRUE) %>% 
        arrange(desc(.)) %>%
        dplyr::select(matches("HSRL$")) %>%  # Select columns ending with "HSRL"
        pull(1)
    } else if (pop_ver == "WorldPop") {
      vec.anno <- as_tibble(pop0_ctry_rasadj.anno, na.rm = TRUE) %>% 
        arrange(desc(.)) %>%
        dplyr::select(matches("WorldPop$")) %>%  # Select columns ending with "WorldPop"
        pull(1)
    }
    
    #### Manual tuning of urban/rural ratio
    # Some countries are ill-defined towards rural/urban population, such as the case of Nepal,
    # in which could be possible that urban population accounts for more than what 
    # the WHO dataset says.
    print(paste0("Manual tuning of urban/rural ratio: ",urb_shift_factor))
    
    ix.anno <- length(which(cumsum(vec.anno) <= urbpopadj.anno)) * urb_shift_factor
    vec.anno[ix.anno] #Valor de corte
    
    # filtra por el umbral
    if (pop_ver == "HSRL") {
      # First, find the column name that ends with "HSRL"
      column_name <- names(pop0_ctry_rasadj.anno)[grepl("HSRL$", names(pop0_ctry_rasadj.anno))]
      column_name <- column_name[1]
      # Convert the column name to a symbol
      column_symbol <- sym(column_name)
      # Now, use `filter()` dynamically
      urbanpopulation.anno <- pop0_ctry_rasadj.anno %>%
        dplyr::filter(!!column_symbol > vec.anno[ix.anno])
    } else if (pop_ver == "WorldPop") {
      # First, find the column name that ends with "WorldPop"
      column_name <- names(pop0_ctry_rasadj.anno)[grepl("WorldPop$", names(pop0_ctry_rasadj.anno))]
      column_name <- column_name[1]
      # Convert the column name to a symbol
      column_symbol <- sym(column_name)
      # Now, use `filter()` dynamically
      urbanpopulation.anno <- pop0_ctry_rasadj.anno %>%
        dplyr::filter(!!column_symbol > vec.anno[ix.anno])
    }
    
    # terra::writeRaster(urbanpopulation, paste0("population_temp/",pop_ver,"_",i,"_",j,"_urbpop.tif"), filetype = "GTiff", overwrite = TRUE)
    m_urb <- c(-Inf, 0, NA,
               0, Inf, 2)
    rcl_urb <- matrix(m_urb, ncol=3, byrow=TRUE)
    urbanpopulationR.anno <- urbanpopulation.anno %>%
      classify(rcl_urb, include.lowest=TRUE)
    # terra::writeRaster(urbanpopulation, paste0("population_temp/",pop_ver,"_",i,"_",j,"_urbpopR.tif"), filetype = "GTiff", overwrite = TRUE)
    
    if (pop_ver == "HSRL") {
      # First, find the column name that ends with "HSRL"
      column_name <- names(pop0_ctry_rasadj.anno)[grepl("HSRL$", names(pop0_ctry_rasadj.anno))]
      column_name <- column_name[1]
      # Convert the column name to a symbol
      column_symbol <- sym(column_name)
      # Now, use `filter()` dynamically
      ruralpopulation.anno <- pop0_ctry_rasadj.anno %>%
        dplyr::filter(!!column_symbol <= vec.anno[ix.anno])
    } else if (pop_ver == "WorldPop") {
      # First, find the column name that ends with "WorldPop"
      column_name <- names(pop0_ctry_rasadj.anno)[grepl("WorldPop$", names(pop0_ctry_rasadj.anno))]
      column_name <- column_name[1]
      # Convert the column name to a symbol
      column_symbol <- sym(column_name)
      # Now, use `filter()` dynamically
      ruralpopulation.anno <- pop0_ctry_rasadj.anno %>%
        dplyr::filter(!!column_symbol <= vec.anno[ix.anno])
    }
    
    # terra::writeRaster(ruralpopulation, paste0("population_temp/",pop_ver,"_",i,"_",j,"_rurpop.tif"), filetype = "GTiff", overwrite = TRUE)
    m_rur <- c(-Inf, 0, NA,
               0, Inf, 1)
    rcl_rur <- matrix(m_rur, ncol=3, byrow=TRUE)
    ruralpopulationR.anno <- ruralpopulation.anno %>%
      classify(rcl_rur, include.lowest=TRUE)
    # terra::writeRaster(ruralpopulation, paste0("population_temp/",pop_ver,"_",i,"_",j,"_rurpopR.tif"), filetype = "GTiff", overwrite = TRUE)
    
    rururbpopulationR.anno <- merge(urbanpopulationR.anno, ruralpopulationR.anno)
    if (pop_ver == "HSRL") {
      rururbpopulationR_plot.anno <- rururbpopulationR.anno %>%
        mutate(!!column_name := recode(!!column_symbol,
                                       `1` = "Rural",
                                       `2` = "Urban"))
    } else if (pop_ver == "WorldPop") {
      rururbpopulationR_plot.anno <- rururbpopulationR.anno %>%
        mutate(!!column_name := recode(!!column_symbol,
                                       `1` = "Rural",
                                       `2` = "Urban"))
    }
    
    plot(rururbpopulationR_plot.anno, main=paste0(i," : ",j))
    lines(ctry_vector, lwd=2)
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
    
    ### Spread population (whodb) and demand (wfdb) by ENERGY CARRIER use and urban vs rural ----
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
    whodb, wfdb,
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
      # allowed_whodb <- c("Kerosene","Gas","Electricity","Biomass","Charcoal","Coal")
      allowed_whodb <- unique(wfdb$fuel)

      # allowed_wfdb  <- c("fuelwood","charcoal","imp_fuelwood","imp_charcoal",
      #                    "gas","kerosene","electric","pellets","ethanol","biogas","other")
      allowed_wfdb <- unique(wfdb$fuel)
      
      if (subcountry != 1) {
        if (!(fuel_name %in% allowed_whodb)) {
          stop(sprintf("For subcountry != 1, fuel must be one of: %s",
                       paste(allowed_whodb, collapse = ", ")))
        }
      } else {
        if (!(fuel_name %in% allowed_wfdb)) {
          stop(sprintf("For subcountry == 1, fuel must be one of: %s",
                       paste(allowed_wfdb, collapse = ", ")))
        }
        # if (!(tolower(fuel_name) %in% tolower(allowed_wfdb))) {
        #   stop(sprintf("For subcountry == 1, fuel must be one of: %s",
        #                paste(allowed_wfdb, collapse = ", ")))
        # }
      }
      
      # POP source depends on subcountry
      if (subcountry != 1) {
        pop_tbl <- whodb %>%
          dplyr::filter(grepl(i, .data$iso3)) %>%
          dplyr::filter(.match_fuel(.data$fuel, fuel_name)) %>%   # exact & case-insensitive
          dplyr::filter(grepl(j, .data$year)) %>%
          dplyr::filter(grepl("Rur|Urb", .data$area))
        pop_col_name <- "pop"
      } else {
        pop_tbl <- wfdb %>%
          dplyr::filter(grepl(i, .data$iso3)) %>%
          dplyr::filter(.match_fuel(.data$fuel, fuel_name)) %>%
          dplyr::filter(grepl(j, .data$year)) %>%
          dplyr::filter(grepl("Rur|Urb", .data$area))
        pop_col_name <- "people"
      }
      
      # ---- (B) DEMAND TABLE: always from wfdb (exact & case-insensitive)
      # WHO branch uses WHO labels in wfdb too → match as-is (no mapping).
      if (is.null(wfdb_fuels_for_demand)) {
        demand_tbl <- wfdb %>%
          dplyr::filter(grepl(i, .data$iso3)) %>%
          dplyr::filter(.data$year == as.numeric(j)) %>%
          dplyr::filter(grepl("Rur|Urb", .data$area)) %>%
          dplyr::filter(tolower(trimws(.data$fuel)) == tolower(trimws(fuel_name)))
      } else {
        demand_tbl <- wfdb %>%
          dplyr::filter(grepl(i, .data$iso3)) %>%
          dplyr::filter(.data$year == as.numeric(j)) %>%
          dplyr::filter(grepl("Rur|Urb", .data$area)) %>%
          dplyr::filter(tolower(trimws(.data$fuel)) %in% tolower(trimws(wfdb_fuels_for_demand)))
      }
      
      # ---- Urban users
      biourb_total_people <- pop_tbl %>%
        dplyr::filter(grepl("Urb", .data$area)) %>%
        pull(all_of(pop_col_name)) %>%
        sum(na.rm = TRUE) * 1000
      
      urbbio_Sctry.anno <- if (urbpopmap.anno == 0) {
        urbanpopulation.anno * 0
      } else {
        (urbanpopulation.anno * biourb_total_people) / urbpopmap.anno
      }
      
      # ---- Urban demand
      biourb_d_tons <- demand_tbl %>%
        dplyr::filter(grepl("Urb", .data$area)) %>%
        pull(all_of(demand_col)) %>%
        sum(na.rm = TRUE)
      
      urbbioDem_Sctry.anno <- if (urbpopmap.anno == 0) {
        urbanpopulation.anno * 0
      } else {
        urbanpopulation.anno * biourb_d_tons / urbpopmap.anno
      }
      
      # ---- Rural users
      biorur_total_people <- pop_tbl %>%
        dplyr::filter(grepl("Rur", .data$area)) %>%
        pull(all_of(pop_col_name)) %>%
        sum(na.rm = TRUE) * 1000
      
      rurbio_Sctry.anno <- if (rurpopmap.anno == 0) {
        ruralpopulation.anno * 0
      } else {
        ruralpopulation.anno * biorur_total_people / rurpopmap.anno
      }
      
      # ---- Rural demand
      biorur_d_tons <- demand_tbl %>%
        dplyr::filter(grepl("Rur", .data$area)) %>%
        pull(all_of(demand_col)) %>%
        sum(na.rm = TRUE)
      
      rurbioDem_Sctry.anno <- if (rurpopmap.anno == 0) {
        ruralpopulation.anno * 0
      } else {
        ruralpopulation.anno * biorur_d_tons / rurpopmap.anno
      }
      
      # ---- Merge & write
      users_raster  <- merge(rurbio_Sctry.anno,   urbbio_Sctry.anno)
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
      fuels_who <- unique(wfdb$fuel)
      
      results <- lapply(fuels_who, function(fu)
        compute_fuel_maps(
          fuel_name = fu,              # ← use the new argument name explicitly
          subcountry = subcountry,     # != 1 here
          i = i, j = j,
          whodb = whodb, wfdb = wfdb,
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
      
      save_wf_aggregates_who <- function(
    i, j,
    whodb, wfdb, demand_col,
    subcountry = 0,  # anything != 1
    urbanpopulation.anno, ruralpopulation.anno,
    urbpopmap.anno, rurpopmap.anno,
    pop_ver,
    out_dir = "demand_temp"
      ) {
        stopifnot(subcountry != 1)
        
        get_dem_who <- function(who_fuel) {
          res <- compute_fuel_maps(
            fuel_name = who_fuel,        # WHO label used both in whodb and wfdb
            subcountry = subcountry,     # != 1
            i = i, j = j,
            whodb = whodb, wfdb = wfdb,
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
        
        safe_get_dem_who <- function(fuel_name) {
          if (fuel_name %in% fuels_avail) {
            get_dem_who(fuel_name)
          } else {
            NULL
          }
        }
        
        fw  <- safe_get_dem_who("Fuelwood")
        ifw <- safe_get_dem_who("Imp_fuelwood")
        ch  <- safe_get_dem_who("Charcoal")
        ich <- safe_get_dem_who("Imp_charcoal")
        
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
      
      
      save_wf_aggregates_who(
        i = i, j = j,
        whodb = whodb, wfdb = wfdb, demand_col = demand_col,
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
          whodb = whodb, wfdb = wfdb,
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
    whodb, wfdb, demand_col,
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
            whodb = whodb, wfdb = wfdb,
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
        
        fw  <- safe_get_dem("Fuelwood")
        ifw <- safe_get_dem("Imp_fuelwood")
        ch  <- safe_get_dem("Charcoal")
        ich <- safe_get_dem("Imp_charcoal")
        
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
        whodb = whodb, wfdb = wfdb, demand_col = demand_col,
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
  merge_across_areas_who <- function(
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
    # filename tags are lowercased by compute_fuel_maps()
    fuel_tags_users  <- tolower(fuels_users)
    fuel_tags_demand <- tolower(fuels_demand)

    for (k in years) {
      # ---- USERS (by fuel)
      for (fu_tag in fuel_tags_users) {
        patt_u <- .build_pattern(pop_ver, k, fu_tag, "users")
        files_u <- list.files(in_pop_dir, pattern = patt_u, full.names = TRUE)
        out_u <- file.path(out_pop_dir, sprintf("%s_%s_%s_users.tif", pop_ver, fu_tag, k))
        .merge_and_write(files_u, out_u)
      }

      # ---- DEMAND (by fuel)
      for (fu_tag in fuel_tags_demand) {
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

  merge_across_areas_who(
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
## Walking ----
if (optimizeD == 1) {
  keep(annos, optimizeD, , country, countrydir, #endpath,
       githubdir, country, countrydir, demanddir, admindir, emissionsdir, rTempdir, 
       proj_gcs, epsg_gcs, proj_pcs, epsg_pcs, proj_authority, GEE_scale,
       byregion, scenario_ver, pop_ver, mofuss_region, rTempdir, sure=TRUE) # shows you which variables will not be removed
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
layer_namesw <- if (!is.null(names(wf_w_st)) && all(nzchar(names(wf_w_st))))
  names(wf_w_st) else paste0("lyr", seq_len(nlyr(wf_w_st)))

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
  mutate_at(c('ID'), as.numeric)
locs_raster_w <- rast(newlocs_w, type="xyz", crs=crs(wf_w_st[[1]]), digits=0)
terra::writeRaster(locs_raster_w,"to_idw/locs_raster_w.tif",
                   filetype = "GTiff", overwrite = TRUE)

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
       byregion, scenario_ver, pop_ver, mofuss_region, rTempdir, sure=TRUE)
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
layer_namesv <- if (!is.null(names(wf_v_st)) && all(nzchar(names(wf_v_st))))
  names(wf_v_st) else paste0("lyr", seq_len(nlyr(wf_v_st)))

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
  dplyr::select(x,y,ID)
locs_raster_v <- rast(newlocs_v, type="xyz", crs=crs(wf_v_st[[1]]), digits=0)
terra::writeRaster(locs_raster_v,"to_idw/locs_raster_v.tif",
                   filetype = "GTiff", overwrite = TRUE)

wf_v_db4idw %>%
  dplyr::select(!c(x,y,centroids)) %>%
  mutate_if(is.character, as.numeric) %>%
  # mutate(across(where(is.numeric), round, 6)) %>%
  mutate_if(is.numeric, round, 6) %>%
  write.csv(paste0("to_idw/",substr(scenario_ver, 1, 3),"_fwch_v.csv"), row.names=FALSE, quote=FALSE)



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

# End of script ----

