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
# Version 4
# Date: Sep 2025

# 2dolist ----
# FIX THE MASK ISSUE WITH LINUX, THAT WAS PATCHED FOR THE MOMENT!
# ALLOW OTHER SCENARIOS: Start in line 183
# VERY IMPORTANT TO DEFINE A SOLID WORKFLOW FOR REGIONALIZING COUNTRIES, e.g. Zambia

# Internal parameters ----
optimizeD = 0
temdirdefined = 1
urb_shift_factor <- 1 # Only works with byregion == Country (Check code lines 89-91 before adjusting this).
# For Nepal use 10.
# # Select MoFuSS platform:
# webmofuss = 1 # "1" is  web-MoFuSS running in our Ubuntu server, "0" is local host (Windows or Linux)
# source(paste0(scriptsmofuss,"00_webmofuss.R"))

# Load libraries ----
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
library(gdata)
library(ggplot2)
#library(hacksaw)
#library(mapview)
library(raster)
library(readxl)
library(rlang)
library(sf)
library(svDialogs)
library(tibble)
library(tictoc)
library(tidyterra)
library(tidyverse)

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
whodb <- read_excel("demand_in/A_LMIC_Estimates_2050_popmedian.xlsx")
# undb <- read_excel("admin_regions/UN_Rural-Urban_Pop_projections_formatted.xlsx") # https://population.un.org/wpp/Download/Standard/Population/
# terra::unique(whodb$fuel)
# terra::unique(whodb$year)
# terra::unique(whodb$iso3)

poprast <- paste0("demand_in/",pop_map_name) 

# Define scenarios ----
if (scenario_ver == "BaU") {
  wfdb <- read_csv("demand_in/cons_fuels_years.csv")
} else if (scenario_ver == "ICS") {
  wfdb <- read_csv("demand_in/cons_fuels_years_proj.csv")
} else if (scenario_ver == "BaU_vehicle_only") {
  wfdb <- read_csv("demand_in/cons_fuels_years_charc_and_urb_fw_only.csv")
} else if (scenario_ver == "BaU_walking_only") {
  wfdb <- read_csv("demand_in/cons_fuels_years_rural_fw_only.csv")
} else if (scenario_ver == "BaU_lusaka_notlusaka") {
  wfdb <- read_csv("demand_in/cons_fuels_years_BAU_Lusaka-NotLusaka.csv")
} else if (scenario_ver == "ICS1_lusaka_notlusaka") {
  wfdb <- read_csv("demand_in/cons_fuels_years_Proj1_Lusaka-NotLusaka.csv")
} else if (scenario_ver == "ICS2_lusaka_notlusaka") {
  wfdb <- read_csv("demand_in/cons_fuels_years_Proj2_Lusaka-NotLusaka.csv")
}
unique(wfdb$fuel)

# Unify "fuelwood" to "biomass" # Consider the other way round but we'll need to debug downstream
if (subcountry == 1) {
  wfdb <- wfdb %>%
    mutate(
      fuel = case_when(
        tolower(fuel) == "fuelwood" ~ "Biomass",
        tolower(fuel) == "charcoal" ~ "Charcoal",
        TRUE ~ fuel
      ),
      iso3 = paste0(iso3, "_", dense_rank(country))
    )
}
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
annos.list2 <- c(2010:end_year) 
annos <- annos.list2[!annos.list2 %in% yr]

# Save in LULCC/TempTables to replace parameters years
setwd(countrydir)
write.table(annos, "LULCC/TempTables/annos.txt")
annostxt <- read.table("LULCC/TempTables/annos.txt") %>% .$x 
setwd(demanddir)

# Select a region
if (aoi_poly == 1) {
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
  # Find the GID_0 with the largest total overlapping area
  largest_overlap <- overlap_summary[which.max(overlap_summary$total_area), ]

  # Find the NAME_0 corresponding to the largest_overlap GID_0
  matching_row <- mofuss_regions0_gpkg[mofuss_regions0_gpkg$GID_0 == largest_overlap$GID_0, ]
  # Extract the NAME_0 value
  mofuss_region <- matching_row$GID_0
  mofuss_region_kml <- matching_row$GID_0

    # Print the result
  cat("The GID_0 with the largest overlap is:", largest_overlap$GID_0, "\n")
  cat("Overlapping area:", largest_overlap$total_area, "km²\n") 

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
  dplyr::filter(grepl('Total', fuel)) %>%
  dplyr::filter(grepl(yr, year)) %>%
  dplyr::filter(!grepl('Over', area)) %>%
  group_by(iso3) %>% 
  summarise(sum_pop=sum(pop)*1000,
            .groups = 'drop')

# Reads furb in 2018 from WHO dataset
whodb_join <- whodb %>%
  dplyr::select(iso3, country) %>%
  terra::unique()

furb_who <- whodb %>% # algo pasa con algunas librerias rio abajo que rompen esta parte si ya estan cargadas
  dplyr::filter(grepl('Total', fuel)) %>%
  dplyr::filter(grepl(yr, year)) %>%
  dplyr::filter(grepl('Urban', area)) %>%
  group_by(iso3) %>% 
  summarise(urb_pop=sum(pop)*1000,
            .groups = 'drop') %>%
  left_join(totpopWHO, ., by="iso3") %>% 
  mutate(furb = round(urb_pop/sum_pop,2)) %>%
  left_join(whodb_join, ., by = "iso3") %>%
  dplyr::select(iso3, country, furb) %>%
  rename(GID_0 = iso3,
         NAME_0 = country)

furb_who %>%
  dplyr::filter(GID_0 == "ZMB")

} else if (subcountry == 1) {
  
  totpoprob <- wfdb %>%
    # dplyr::filter(grepl('Total', fuel)) %>%
    dplyr::filter(grepl(yr, year)) %>%
    # dplyr::filter(!grepl('Over', area)) %>%
    group_by(iso3) %>%
    summarise(sum_pop=sum(people)*1000000, #Ask Rob to fix the x 1,000,000 for x1,000 and "pop" instead of people
              .groups = 'drop')

  # Reads furb in 2018(????) from Rob dataset
  robdb_join <- wfdb %>%
    dplyr::select(iso3, country) %>%
    terra::unique()
  
  furb_rob <- wfdb %>% # algo pasa con algunas librerias rio abajo que rompen esta parte si ya estan cargadas
    # dplyr::filter(grepl('Total', fuel)) %>%
    dplyr::filter(grepl(yr, year)) %>%
    dplyr::filter(grepl('Urban', area)) %>%
    group_by(iso3) %>% 
    summarise(urb_pop=sum(people)*1000000,
              .groups = 'drop') %>%
    left_join(totpoprob, ., by="iso3") %>% 
    mutate(furb = round(urb_pop/sum_pop,2)) %>%
    left_join(robdb_join, ., by = "iso3") %>%
    dplyr::select(iso3, country, furb) %>%
    rename(GID_0 = iso3,
           NAME_0 = country)
  
  furb_rob 

}

# La suma total en la resolución nativa: HRSL: 56,861,964.76; GPW: 44,953,897.44.
pop0 <- rast(poprast) #in base year

if (aoi_poly == 1) {
  # Handle the case where aoi_poly is 1, regardless of byregion
  cat("aoi_poly is set to 1. This overrides other conditions.\n")
  print("***NOW RUNNING GLOBAL DEMAND SCENARIOS - Polygon***")
  adm0_reg <- mofuss_regions0_gpkg %>% 
    dplyr::filter(GID_0 == mofuss_region_kml) # Check if multiple countries or values is doable
  pop0_K <- crop(pop0, ext(adm0_reg) + .01)
  if (os == "Windows") {
    pop0_reg <- mask(pop0_K, adm0_reg) #THIS BREAKS IN UBUNTU
  } else if(os == "Linux") {
    pop0_reg <- pop0_K
  }
  plot(pop0_reg, main=paste0("You selected ",mofuss_region))
  lines(adm0_reg)
  Sys.sleep(10)
  
} else if (byregion == "Global" & aoi_poly == 0) {
  print("***NOW RUNNING GLOBAL DEMAND SCENARIOS - Global***")
  adm0_reg <- mofuss_regions0_gpkg
  pop0_K <- crop(pop0, ext(adm0_reg) + .01)
  if (os == "Windows") {
    pop0_reg <- mask(pop0_K, adm0_reg) #THIS BREAKS IN UBUNTU
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
      not_rows <- y_lkp %>% filter(grepl("^NOT", .key))  # "Not*" considering ignore_case
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
    matched_vals <- intersect(unique(key_sv), unique(y_lkp$.key))
    
    # Unmatched get the decided not_label
    x$match_name <- ifelse(key_sv %in% matched_vals, x$NAME_2, not_label)
    
    # 5) Dissolve by match_name
    x_diss <- terra::aggregate(x["match_name"], by = "match_name")
    
    # 6) Join attributes from y (GID_0 required; furb optional)
    x_diss$.key <- key(x_diss$match_name)
    bring_cols <- intersect(c("GID_0", "furb"), names(y_lkp))
    if (!"GID_0" %in% bring_cols) stop("y must contain column 'GID_0'.")
    
    lkp_to_merge <- y_lkp %>% select(.key, all_of(bring_cols))
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

  # Quick visual sanity check:
  plot(result_vec, col = rainbow(nrow(result_vec))); result_vec
  result_vec
  
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

# Ask Diana to translate this ugly loop into a split-apply-compile process with apply (mapply?)
# Will be much faster but less easy to debug

for (i in adm0_reg$GID_0) { # start of the for loop ----
  # i ="ZMB_1"
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
  
  totpop <- round(global(pop0_ctry_ras, "sum", na.rm=TRUE),0) %>%
    pull(sum)
  urbpop <- round(totpop * ctry_furb,0) 
  rurpop <- totpop - urbpop
  totpop
  urbpop
  rurpop
  
  if (subcountry != 1) {
    pop0_ctry_rasadj <- pop0_ctry_ras*who_ctry_pop/totpop
  } else if (subcountry == 1) {
    pop0_ctry_rasadj <- pop0_ctry_ras*rob_ctry_pop/totpop
  }
  
  totpopadj <- round(global(pop0_ctry_rasadj, "sum", na.rm=TRUE),0) %>%
    pull(sum)
  urbpopadj <- round(totpopadj * ctry_furb,0)
  rurpopadj <- totpopadj - urbpopadj
  totpopadj
  urbpopadj
  rurpopadj
  terra::writeRaster(pop0_ctry_rasadj, paste0("pop_temp/",pop_ver,"_",i,"_",yr,"_popadj.tif"), filetype = "GTiff", overwrite = TRUE)
  
  for (j in annos) { 
    # i="PNG"
    # j=2010
    
    gc()
    terraOptions(memfrac=0.9)
    print(j)
    
    if (subcountry != 1) {
    totpopWHO_annual <- whodb %>% 
      # dplyr::filter(grepl(i, iso3)) %>% # searchs for the pattern, anywhere within the string
      dplyr::filter(grepl('Total', fuel)) %>% #Porque usar grelp?
      # dplyr::filter(grepl(yr, year)) %>%
      dplyr::filter(!grepl('Over', area)) %>%
      group_by(iso3,year) %>% 
      summarise(sum_pop=sum(pop)*1000,
                .groups = 'drop')
    
    who_ctry_pop_annual <- totpopWHO_annual %>%
      dplyr::filter(iso3 == i) %>%
      dplyr::filter(year == j) %>%
      pull(sum_pop)
    
    pop0_ctry_rasadj.anno<- pop0_ctry_ras*who_ctry_pop_annual/totpop
    totpopadj.anno <- round(global(pop0_ctry_rasadj.anno, "sum", na.rm=TRUE),0) %>%
      pull(sum)
    urbpopadj.anno <- round(totpopadj.anno * ctry_furb,0)
    rurpopadj.anno <- totpopadj.anno - urbpopadj.anno
    totpopadj.anno
    urbpopadj.anno
    rurpopadj.anno
    terra::writeRaster(pop0_ctry_rasadj.anno, paste0("pop_temp/",pop_ver,"_",i,"_",j,"_popadj.tif"), filetype = "GTiff", overwrite = TRUE)
    

    } else if (subcountry == 1) {

    totpopROB_annual <- wfdb %>% 
      # dplyr::filter(grepl(i, iso3)) %>% # searchs for the pattern, anywhere within the string
      # dplyr::filter(grepl('Total', fuel)) %>% #Porque usar grelp?
      # dplyr::filter(grepl(yr, year)) %>%
      # dplyr::filter(!grepl('Over', area)) %>%
      group_by(iso3,year) %>% 
      summarise(sum_pop=sum(people)*1000000,
                .groups = 'drop')
    
    rob_ctry_pop_annual <- totpopROB_annual %>%
      dplyr::filter(iso3 == i) %>%
      dplyr::filter(year == j) %>%
      pull(sum_pop)
    
    pop0_ctry_rasadj.anno<- pop0_ctry_ras*rob_ctry_pop_annual/totpop
    totpopadj.anno <- round(global(pop0_ctry_rasadj.anno, "sum", na.rm=TRUE),0) %>%
      pull(sum)
    urbpopadj.anno <- round(totpopadj.anno * ctry_furb,0)
    rurpopadj.anno <- totpopadj.anno - urbpopadj.anno
    totpopadj.anno
    urbpopadj.anno
    rurpopadj.anno
    terra::writeRaster(pop0_ctry_rasadj.anno, paste0("pop_temp/",pop_ver,"_",i,"_",j,"_popadj.tif"), filetype = "GTiff", overwrite = TRUE)
    
    }
    
  }
  
  # Saca el umbral de corte urbano/rural para el año base de 2018
  if (pop_ver == "HSRL") {
    vec <- as_tibble(pop0_ctry_rasadj, na.rm = TRUE) %>% 
      arrange(desc(.)) %>%
      dplyr::select(matches("HSRL$")) %>%  # Select columns ending with "HSRL"
      pull(1)
  } else if (pop_ver == "WorldPop") {
    vec <- as_tibble(pop0_ctry_rasadj, na.rm = TRUE) %>% 
      arrange(desc(.)) %>%
      dplyr::select(matches("WorldPop$")) %>%  # Select columns ending with "WorldPop"
      pull(1)
  }
  
  # # Manual tuning of urban/rural ratio ----
  # # Some countries are ill-defined towards rural/urban population, such as the case of Nepal,
  # # in which could be possible that urban population accounts for more than what 
  # # the WHO dataset says.
  print(paste0("Manual tuning of urban/rural ratio: ",urb_shift_factor))
  
  ix <- length(which(cumsum(vec) <= urbpopadj)) * urb_shift_factor
  vec[ix] #Valor de corte
  
  # filtra por el umbral
  if (pop_ver == "HSRL") {
    # First, find the column name that ends with "HSRL"
    column_name <- names(pop0_ctry_rasadj)[grepl("HSRL$", names(pop0_ctry_rasadj))]
    column_name <- column_name[1]
    # Convert the column name to a symbol
    column_symbol <- sym(column_name)
    # Now, use `filter()` dynamically
    urbanpopulation <- pop0_ctry_rasadj %>%
      filter(!!column_symbol > vec[ix])
  } else if (pop_ver == "WorldPop") {
    # First, find the column name that ends with "WorldPop"
    column_name <- names(pop0_ctry_rasadj)[grepl("WorldPop$", names(pop0_ctry_rasadj))]
    column_name <- column_name[1]
    # Convert the column name to a symbol
    column_symbol <- sym(column_name)
    # Now, use `filter()` dynamically
    urbanpopulation <- pop0_ctry_rasadj %>%
      filter(!!column_symbol > vec[ix])
  }
  
  # terra::writeRaster(urbanpopulation, paste0("population_temp/",pop_ver,"_",i,"_",yr,"_urbpop.tif"), filetype = "GTiff", overwrite = TRUE)
  m_urb <- c(-Inf, 0, NA,
             0, Inf, 2)
  rcl_urb <- matrix(m_urb, ncol=3, byrow=TRUE)
  urbanpopulationR <- urbanpopulation %>%
    classify(rcl_urb, include.lowest=TRUE)
  # terra::writeRaster(urbanpopulation, paste0("population_temp/",pop_ver,"_",i,"_",yr,"_urbpopR.tif"), filetype = "GTiff", overwrite = TRUE)
  
  if (pop_ver == "HSRL") {
    # First, find the column name that ends with "HSRL"
    column_name <- names(pop0_ctry_rasadj)[grepl("HSRL$", names(pop0_ctry_rasadj))]
    column_name <- column_name[1]
    # Convert the column name to a symbol
    column_symbol <- sym(column_name)
    # Now, use `filter()` dynamically
    ruralpopulation <- pop0_ctry_rasadj %>%
      filter(!!column_symbol <= vec[ix])
  } else if (pop_ver == "WorldPop") {
    # First, find the column name that ends with "WorldPop"
    column_name <- names(pop0_ctry_rasadj)[grepl("WorldPop$", names(pop0_ctry_rasadj))]
    column_name <- column_name[1]
    # Convert the column name to a symbol
    column_symbol <- sym(column_name)
    # Now, use `filter()` dynamically
    ruralpopulation <- pop0_ctry_rasadj %>%
      filter(!!column_symbol <= vec[ix])
  }
  
  # terra::writeRaster(ruralpopulation, paste0("population_temp/",pop_ver,"_",i,"_",yr,"_rurpop.tif"), filetype = "GTiff", overwrite = TRUE)
  m_rur <- c(-Inf, 0, NA,
             0, Inf, 1)
  rcl_rur <- matrix(m_rur, ncol=3, byrow=TRUE)
  ruralpopulationR <- ruralpopulation %>%
    classify(rcl_rur, include.lowest=TRUE)
  # terra::writeRaster(ruralpopulation, paste0("population_temp/",pop_ver,"_",i,"_",yr,"_rurpopR.tif"), filetype = "GTiff", overwrite = TRUE)
  
  rururbpopulationR <- merge(urbanpopulationR, ruralpopulationR)
  if (pop_ver == "HSRL") {
    rururbpopulationR_plot <- rururbpopulationR %>%
      mutate(!!column_name := recode(!!column_symbol,
                                     `1` = "Rural",
                                     `2` = "Urban"))
  } else if (pop_ver == "WorldPop") {
    rururbpopulationR_plot <- rururbpopulationR %>%
      mutate(!!column_name := recode(!!column_symbol,
                                     `1` = "Rural",
                                     `2` = "Urban"))
  }
  
  plot(rururbpopulationR_plot, main=i)
  lines(ctry_vector, lwd=2)
  terra::writeRaster(rururbpopulationR, paste0("pop_temp/",pop_ver,"_",i,"_",yr,"_rururbR.tif"), filetype = "GTiff", overwrite = TRUE)
  
  # Validation
  urbpopmap <- round(global(urbanpopulation, "sum", na.rm=TRUE),0) %>% 
    pull(sum)
  urbpopmap
  urbpopadj
  round((urbpopmap/totpopadj),2)
  
  rurpopmap <- round(global(ruralpopulation, "sum", na.rm=TRUE),0) %>% 
    pull(sum)
  rurpopmap
  rurpopadj
  round((rurpopmap/totpopadj),2)

  # Spread population (whodb) and demand (wfdb) by BIOMASS use and urban vs rural ----
  if (subcountry != 1) {
    biopopWHO <- whodb %>% 
      dplyr::filter(grepl(i, iso3)) %>% # searchs for the pattern, anywhere within the string
      dplyr::filter(grepl('Bio', fuel)) %>%
      dplyr::filter(grepl(yr, year)) %>%
      dplyr::filter(grepl('Rur|Urb', area))
  } else if (subcountry == 1) {
    biopopROB <- wfdb %>% 
      dplyr::filter(grepl(i, iso3)) %>% # searchs for the pattern, anywhere within the string
      dplyr::filter(grepl('Bio', fuel)) %>%
      dplyr::filter(grepl(yr, year)) %>%
      dplyr::filter(grepl('Rur|Urb', area))
  }

  biowfdb <- wfdb %>% 
    dplyr::filter(grepl(i, iso3)) %>% # searchs for the pattern, anywhere within the string
    dplyr::filter(grepl('Bio', fuel)) %>%
    dplyr::filter(grepl(yr, year)) %>%
    dplyr::filter(grepl('Rur|Urb', area))
  
  if (subcountry != 1) {
  biourb <- biopopWHO %>% 
    dplyr::filter(grepl('Urb', area)) %>%
    dplyr::select(pop) %>% 
    sum()*1000
  urbpopmap
  urbbio_Sctry <- urbanpopulation*biourb/urbpopmap
  round(global(urbbio_Sctry, "sum", na.rm=TRUE),0) %>% 
    pull(sum)
  biourb_d_tons <- biowfdb %>% 
    dplyr::filter(grepl('Urb', area)) %>%
    dplyr::select(all_of(demand_col)) %>% 
    sum()
  urbbioDem_Sctry <- urbanpopulation*biourb_d_tons/urbpopmap
  
  biorur <- biopopWHO %>% 
    dplyr::filter(grepl('Rur', area)) %>%
    dplyr::select(pop) %>% 
    sum()*1000  
  rurpopmap
  rurbio_Sctry <- ruralpopulation*biorur/rurpopmap
  round(global(rurbio_Sctry, "sum", na.rm=TRUE),0) %>% 
    pull(sum)
  biorur_d_tons <- biowfdb %>% 
    dplyr::filter(grepl('Rur', area)) %>%
    dplyr::select(all_of(demand_col)) %>% 
    sum()
  rurbioDem_Sctry <- ruralpopulation*biorur_d_tons/rurpopmap
  
  } else if (subcountry == 1) {
  
    biourb <- biopopROB %>% 
      dplyr::filter(grepl('Urb', area)) %>%
      dplyr::select(people) %>% 
      sum()*1000000
    urbpopmap
    urbbio_Sctry <- urbanpopulation*biourb/urbpopmap
    round(global(urbbio_Sctry, "sum", na.rm=TRUE),0) %>% 
      pull(sum)
    biourb_d_tons <- biowfdb %>% 
      dplyr::filter(grepl('Urb', area)) %>%
      dplyr::select(all_of(demand_col)) %>% 
      sum()
    urbbioDem_Sctry <- urbanpopulation*biourb_d_tons/urbpopmap
    
    biorur <- biopopROB %>% 
      dplyr::filter(grepl('Rur', area)) %>%
      dplyr::select(people) %>% 
      sum()*1000000  
    rurpopmap
    rurbio_Sctry <- ruralpopulation*biorur/rurpopmap
    round(global(rurbio_Sctry, "sum", na.rm=TRUE),0) %>% 
      pull(sum)
    biorur_d_tons <- biowfdb %>% 
      dplyr::filter(grepl('Rur', area)) %>%
      dplyr::select(all_of(demand_col)) %>% 
      sum()
    rurbioDem_Sctry <- ruralpopulation*biorur_d_tons/rurpopmap
  
  }
  rururbbio <- merge(rurbio_Sctry,urbbio_Sctry)
  # plot(rururbbio)
  terra::writeRaster(rururbbio, paste0("pop_temp/",pop_ver,"_",i,"_",yr,"_bio_users.tif"), filetype = "GTiff", overwrite = TRUE)
  global(rururbbio, fun="notNA")
  
  rururbbioDem <- merge(rurbioDem_Sctry,urbbioDem_Sctry)
  # plot(rururbbio)
  terra::writeRaster(rururbbioDem, paste0("demand_temp/",pop_ver,"_",i,"_",yr,"_bio_demand.tif"), filetype = "GTiff", overwrite = TRUE)
  global(rururbbioDem, fun="notNA")
  
  bio_percap <- rururbbioDem/rururbbio*1000/365
  # plot(rururbbio)
  terra::writeRaster(bio_percap, paste0("demand_temp/",pop_ver,"_",i,"_",yr,"_bio_percap.tif"), filetype = "GTiff", overwrite = TRUE)
  global(bio_percap, fun="notNA")
  
  # # Spread population (whodb) and demand (wfdb) by CHARCOAL use and urban vs rural ----
  if (subcountry != 1) {
  chapopWHO <- whodb %>% 
    dplyr::filter(grepl(i, iso3)) %>% # searchs for the pattern, anywhere within the string
    dplyr::filter(grepl('Cha', fuel)) %>%
    dplyr::filter(grepl(yr, year)) %>%
    dplyr::filter(grepl('Rur|Urb', area))
  
  } else if (subcountry == 1) {
    chapopROB <- wfdb %>% 
      dplyr::filter(grepl(i, iso3)) %>% # searchs for the pattern, anywhere within the string
      dplyr::filter(grepl('Cha', fuel)) %>%
      dplyr::filter(grepl(yr, year)) %>%
      dplyr::filter(grepl('Rur|Urb', area))
  }
  
  chawfdb <- wfdb %>% 
    dplyr::filter(grepl(i, iso3)) %>% # searchs for the pattern, anywhere within the string
    dplyr::filter(grepl('Cha', fuel)) %>%
    dplyr::filter(grepl(yr, year)) %>%
    dplyr::filter(grepl('Rur|Urb', area))
  
  if (subcountry != 1) {
  chaurb <- chapopWHO %>% 
    dplyr::filter(grepl('Urb', area)) %>%
    dplyr::select(pop) %>% 
    sum()*1000
  urbpopmap 
  urbcha_Sctry <- urbanpopulation*chaurb/urbpopmap
  round(global(urbcha_Sctry, "sum", na.rm=TRUE),0) %>% 
    pull(sum)
  chaurb_d_tons <- chawfdb %>% 
    dplyr::filter(grepl('Urb', area)) %>%
    dplyr::select(all_of(demand_col)) %>% 
    sum()
  urbchaDem_Sctry <- urbanpopulation*chaurb_d_tons/urbpopmap
  
  charur <- chapopWHO %>% 
    dplyr::filter(grepl('Rur', area)) %>%
    dplyr::select(pop) %>% 
    sum()*1000  
  rurpopmap
  rurcha_Sctry <- ruralpopulation*charur/rurpopmap
  round(global(rurcha_Sctry, "sum", na.rm=TRUE),0) %>% 
    pull(sum)
  charur_d_tons <- chawfdb %>% 
    dplyr::filter(grepl('Rur', area)) %>%
    dplyr::select(all_of(demand_col)) %>% 
    sum()
  rurchaDem_Sctry <- ruralpopulation*charur_d_tons/rurpopmap
  
  } else if (subcountry == 1) {
    
    chaurb <- chapopROB %>% 
      dplyr::filter(grepl('Urb', area)) %>%
      dplyr::select(people) %>% 
      sum()*1000000
    urbpopmap 
    urbcha_Sctry <- urbanpopulation*chaurb/urbpopmap
    round(global(urbcha_Sctry, "sum", na.rm=TRUE),0) %>% 
      pull(sum)
    chaurb_d_tons <- chawfdb %>% 
      dplyr::filter(grepl('Urb', area)) %>%
      dplyr::select(all_of(demand_col)) %>% 
      sum()
    urbchaDem_Sctry <- urbanpopulation*chaurb_d_tons/urbpopmap
    
    charur <- chapopROB %>% 
      dplyr::filter(grepl('Rur', area)) %>%
      dplyr::select(people) %>% 
      sum()*1000000  
    rurpopmap
    rurcha_Sctry <- ruralpopulation*charur/rurpopmap
    round(global(rurcha_Sctry, "sum", na.rm=TRUE),0) %>% 
      pull(sum)
    charur_d_tons <- chawfdb %>% 
      dplyr::filter(grepl('Rur', area)) %>%
      dplyr::select(all_of(demand_col)) %>% 
      sum()
    rurchaDem_Sctry <- ruralpopulation*charur_d_tons/rurpopmap
    
  }
  
  rururbcha <- merge(rurcha_Sctry,urbcha_Sctry)
  # plot(rururbcha)
  terra::writeRaster(rururbcha, paste0("pop_temp/",pop_ver,"_",i,"_",yr,"_cha_users.tif"), filetype = "GTiff", overwrite = TRUE)
  global(rururbcha, fun="notNA")
  
  rururbchaDem <- merge(rurchaDem_Sctry,urbchaDem_Sctry)
  # plot(rururbbio)
  terra::writeRaster(rururbchaDem, paste0("demand_temp/",pop_ver,"_",i,"_",yr,"_cha_demand.tif"), filetype = "GTiff", overwrite = TRUE)
  global(rururbchaDem, fun="notNA")
  
  cha_percap <- rururbchaDem/rururbcha*1000/365
  # plot(rururbbio)
  terra::writeRaster(cha_percap, paste0("demand_temp/",pop_ver,"_",i,"_",yr,"_cha_percap.tif"), filetype = "GTiff", overwrite = TRUE)
  global(cha_percap, fun="notNA")
  
  # Save walking or selfgathered woodfuel demand (rurbio) and vehicle or marketed demand (urbbio+urbcha+rurcha) 
  wf_w <- rurbioDem_Sctry
  terra::writeRaster(wf_w, paste0("demand_temp/",pop_ver,"_",i,"_",yr,"_wftons_w.tif"), filetype = "GTiff", overwrite = TRUE)
  global(wf_w, fun="notNA")
  
  wf_v_stack <- c(urbbioDem_Sctry,urbchaDem_Sctry,rurchaDem_Sctry)
  wf_v_stack.sum <- app(wf_v_stack, fun=sum, na.rm = TRUE)
  wf_v <- wf_v_stack.sum
  terra::writeRaster(wf_v, paste0("demand_temp/",pop_ver,"_",i,"_",yr,"_wftons_v.tif"), filetype = "GTiff", overwrite = TRUE)
  global(wf_v, fun="notNA")

  # terra::writeRaster(urbbioDem_Sctry, paste0("demand_temp/",pop_ver,"_",i,"_",yr,"_wftons_urbbio.tif"), filetype = "GTiff", overwrite = TRUE)
  # terra::writeRaster(urbchaDem_Sctry, paste0("demand_temp/",pop_ver,"_",i,"_",yr,"_wftons_urbcha.tif"), filetype = "GTiff", overwrite = TRUE)
  # terra::writeRaster(rurchaDem_Sctry, paste0("demand_temp/",pop_ver,"_",i,"_",yr,"_wftons_rurcha.tif"), filetype = "GTiff", overwrite = TRUE)
  # urbbioDem_Sctry[is.na(urbbioDem_Sctry)] <-0
  # urbchaDem_Sctry[is.na(urbchaDem_Sctry)] <-0
  # rurchaDem_Sctry[is.na(rurchaDem_Sctry)] <-0

  for (j in annos) { 
    # i="AFG"
    # j=2010
    
    gc()
    terraOptions(memfrac=0.9)
    print(j)
    
    # Loop for fuelwood
    biopopWHO.anno <- whodb %>% 
      dplyr::filter(iso3 == i) %>%
      dplyr::filter(fuel == "Biomass") %>%
      # dplyr::filter(grepl(j, year)) %>%
      dplyr::filter(grepl('Rur|Urb', area))
    
    biowfdb.anno <- wfdb %>% 
      dplyr::filter(iso3 == i) %>%
      dplyr::filter(fuel == "Biomass") %>%
      # dplyr::filter(grepl(j, year)) %>%
      dplyr::filter(grepl('Rur|Urb', area))
    
    if (subcountry != 1) {
      biourb.anno <- biopopWHO.anno %>% 
        dplyr::filter(area == "Urban") %>%
        dplyr::filter(year == j) %>%
        dplyr::select(pop) %>% 
        sum()*1000
    } else if (subcountry == 1) {
      biourb.anno <- biowfdb.anno %>% 
        dplyr::filter(area == "Urban") %>%
        dplyr::filter(year == j) %>%
        dplyr::select(people) %>% 
        sum()*1000000
    } 

    urbpopmap
    urbbio_Sctry.anno <- urbanpopulation*biourb.anno/urbpopmap
    round(global(urbbio_Sctry, "sum", na.rm=TRUE),0) %>% 
      pull(sum)
    biourb_d_tons.anno <- biowfdb.anno %>% 
      dplyr::filter(area == "Urban") %>%
      dplyr::filter(year == j) %>%
      dplyr::select(all_of(demand_col)) %>% 
      sum()
    urbpopmap
    urbbioDem_Sctry.anno <- urbanpopulation*biourb_d_tons.anno/urbpopmap
    
    if (subcountry != 1) {
      biorur.anno <- biopopWHO.anno %>% 
        dplyr::filter(area == "Rural") %>%
        dplyr::filter(year == j) %>%
        dplyr::select(pop) %>% 
        sum()*1000
    } else if (subcountry == 1) {
      biorur.anno <- biowfdb.anno %>% 
        dplyr::filter(area == "Rural") %>%
        dplyr::filter(year == j) %>%
        dplyr::select(people) %>% 
        sum()*1000000
    }
    
    rurpopmap
    rurbio_Sctry.anno <- ruralpopulation*biorur.anno/rurpopmap
    round(global(rurbio_Sctry, "sum", na.rm=TRUE),0) %>% 
      pull(sum)
    biorur_d_tons.anno <- biowfdb.anno %>% 
      dplyr::filter(area == "Rural") %>%
      dplyr::filter(year == j) %>%
      dplyr::select(all_of(demand_col)) %>% 
      sum()  
    rurpopmap
    rurbioDem_Sctry.anno <- ruralpopulation*biorur_d_tons.anno/rurpopmap
    
    rururbbio.anno <- merge(rurbio_Sctry.anno,urbbio_Sctry.anno)
    # plot(rururbbio)
    terra::writeRaster(rururbbio.anno, paste0("pop_temp/",pop_ver,"_",i,"_",j,"_bio_users.tif"), filetype = "GTiff", overwrite = TRUE)
    global(rururbbio.anno, fun="notNA")
    
    rururbbioDem.anno <- merge(rurbioDem_Sctry.anno,urbbioDem_Sctry.anno)
    # plot(rururbbio)
    terra::writeRaster(rururbbioDem.anno, paste0("demand_temp/",pop_ver,"_",i,"_",j,"_bio_demand.tif"), filetype = "GTiff", overwrite = TRUE)
    global(rururbbioDem.anno, fun="notNA")
    
    # terra::writeRaster(urbbio_Sctry.anno, paste0("population_temp/",pop_ver,"_",i,"_",j,"_URBbio_users.tif"), filetype = "GTiff", overwrite = TRUE)
    # terra::writeRaster(rurbio_Sctry.anno, paste0("population_temp/",pop_ver,"_",i,"_",j,"_RURbio_users.tif"), filetype = "GTiff", overwrite = TRUE)
    
    # Loop for charcoal
    chapopWHO.anno <- whodb %>% 
      dplyr::filter(iso3 == i) %>%
      dplyr::filter(fuel == "Charcoal") %>%
      # dplyr::filter(grepl(j, year)) %>%
      dplyr::filter(grepl('Rur|Urb', area))
    
    chawfdb.anno <- wfdb %>% 
      dplyr::filter(iso3 == i) %>%
      dplyr::filter(fuel == "Charcoal") %>%
      # dplyr::filter(grepl(j, year)) %>%
      dplyr::filter(grepl('Rur|Urb', area))
    
    if (subcountry != 1) {
      chaurb.anno <- chapopWHO.anno %>% 
        dplyr::filter(area == "Urban") %>%
        dplyr::filter(year == j) %>%
        dplyr::select(pop) %>% 
        sum()*1000
    } else if (subcountry == 1) {
      chaurb.anno <- chawfdb.anno %>% 
        dplyr::filter(area == "Urban") %>%
        dplyr::filter(year == j) %>%
        dplyr::select(people) %>% 
        sum()*1000000
    }
    
    urbpopmap 
    urbcha_Sctry.anno <- urbanpopulation*chaurb.anno/urbpopmap
    round(global(urbcha_Sctry.anno, "sum", na.rm=TRUE),0) %>% 
      pull(sum)
    chaurb_d_tons.anno <- chawfdb.anno %>% 
      dplyr::filter(area == "Urban") %>%
      dplyr::filter(year == j) %>%
      dplyr::select(all_of(demand_col)) %>% 
      sum()
    urbchaDem_Sctry.anno <- urbanpopulation*chaurb_d_tons.anno/urbpopmap
    
    if (subcountry != 1) {
      charur.anno <- chapopWHO.anno %>% 
        dplyr::filter(area == "Rural") %>%
        dplyr::filter(year == j) %>%
        dplyr::select(pop) %>% 
        sum()*1000
    } else if (subcountry == 1) {
      charur.anno <- chawfdb.anno %>% 
        dplyr::filter(area == "Rural") %>%
        dplyr::filter(year == j) %>%
        dplyr::select(people) %>% 
        sum()*1000000
    }

    rurpopmap
    rurcha_Sctry.anno <- ruralpopulation*charur.anno/rurpopmap
    round(global(rurcha_Sctry.anno, "sum", na.rm=TRUE),0) %>% 
      pull(sum)
    charur_d_tons.anno <- chawfdb.anno %>% 
      dplyr::filter(area == "Rural") %>%
      dplyr::filter(year == j) %>%
      dplyr::select(all_of(demand_col)) %>% 
      sum()
    rurchaDem_Sctry.anno <- ruralpopulation*charur_d_tons.anno/rurpopmap
    
    rururbcha.anno <- merge(rurcha_Sctry.anno,urbcha_Sctry.anno)
    # plot(rururbcha)
    terra::writeRaster(rururbcha.anno, paste0("pop_temp/",pop_ver,"_",i,"_",j,"_cha_users.tif"), filetype = "GTiff", overwrite = TRUE)
    global(rururbcha.anno, fun="notNA")
    
    rururbchaDem.anno <- merge(rurchaDem_Sctry.anno,urbchaDem_Sctry.anno)
    # plot(rururbcha)
    terra::writeRaster(rururbchaDem.anno, paste0("demand_temp/",pop_ver,"_",i,"_",j,"_cha_demand.tif"), filetype = "GTiff", overwrite = TRUE)
    global(rururbchaDem.anno, fun="notNA")
    
    # terra::writeRaster(urbcha_Sctry.anno, paste0("population_temp/HSRL_",i,"_",j,"_URBcha_users.tif"), filetype = "GTiff", overwrite = TRUE)
    # terra::writeRaster(rurcha_Sctry.anno, paste0("population_temp/HSRL_",i,"_",j,"_RURcha_users.tif"), filetype = "GTiff", overwrite = TRUE)
    
    # Save walking or selfgathered woodfuel demand (rurbio) and vehicle or marketed demand (urbbio+urbcha+rurcha) 
    wf_w.anno <- rurbioDem_Sctry.anno
    terra::writeRaster(wf_w.anno, paste0("demand_temp/",pop_ver,"_",i,"_",j,"_wftons_w.tif"), filetype = "GTiff", overwrite = TRUE)
    global(wf_w.anno, fun="notNA")
    
    wf_v_stack.anno <- c(urbbioDem_Sctry.anno,urbchaDem_Sctry.anno,rurchaDem_Sctry.anno)
    wf_v_stack.sum.anno <- app(wf_v_stack.anno, fun=sum, na.rm = TRUE)
    wf_v.anno <- wf_v_stack.sum.anno
    terra::writeRaster(wf_v.anno, paste0("demand_temp/",pop_ver,"_",i,"_",j,"_wftons_v.tif"), filetype = "GTiff", overwrite = TRUE)
    global(wf_v.anno, fun="notNA")
  
  }
 
} # end of the for loop ----

# toc()

Sys.sleep(3)

# Load country pop and demand rasters and merge into original region ----
annos__2018 <- sort(c(annos,yr))

for (k in annos__2018) {
  
  
  cha_list <- list.files(path = "pop_temp/",
                         pattern = paste0(k,"_cha.*\\.tif$"), full.names = TRUE)
  
  chaDem_list <- list.files(path = "demand_temp/",
                            pattern = paste0(k,"_cha.*\\.tif$"), full.names = TRUE)
  
  bio_list <- list.files(path = "pop_temp/",
                         pattern = paste0(k,"_bio.*\\.tif$"), full.names = TRUE)
  
  bioDem_list <- list.files(path = "demand_temp/",
                            pattern = paste0(k,"_bio.*\\.tif$"), full.names = TRUE)
  
  pop_list <- list.files(path = "pop_temp/",
                         pattern = paste0(k,"_popadj.*\\.tif$"), full.names = TRUE)
  
  wftons_w_list <- list.files(path = "demand_temp/",
                              pattern = paste0(k,"_wftons_w.*\\.tif$"), full.names = TRUE)
  
  wftons_v_list <- list.files(path = "demand_temp/",
                              pattern = paste0(k,"_wftons_v.*\\.tif$"), full.names = TRUE)
  
  
  if (length(cha_list) > 1) { 
    cha_raster_list <- lapply(cha_list, rast)
    cha_users <- do.call("merge", cha_raster_list)
    terra::writeRaster(cha_users,
                       paste0("pop_out/",pop_ver,"_cha_users_",k,".tif"),
                       filetype = "GTiff", overwrite = TRUE)
    
    chaDem_raster_list <- lapply(chaDem_list, rast)
    chaDem_users <- do.call("merge", chaDem_raster_list)
    terra::writeRaster(chaDem_users,
                       paste0("demand_out/",pop_ver,"_cha_demand_",k,".tif"),
                       filetype = "GTiff", overwrite = TRUE)
    
    bio_raster_list <- lapply(bio_list, rast)
    bio_users <- do.call("merge", bio_raster_list)
    terra::writeRaster(bio_users,
                       paste0("pop_out/",pop_ver,"_bio_users_",k,".tif"),
                       filetype = "GTiff", overwrite = TRUE)
    
    bioDem_raster_list <- lapply(bioDem_list, rast)
    bioDem_users <- do.call("merge", bioDem_raster_list)
    terra::writeRaster(bioDem_users,
                       paste0("demand_out/",pop_ver,"_bio_demand_",k,".tif"),
                       filetype = "GTiff", overwrite = TRUE)
    
    pop_raster_list <- lapply(pop_list, rast)
    pop_users <- do.call("merge", pop_raster_list)
    terra::writeRaster(pop_users,
                       paste0("pop_out/",pop_ver,"_popadj_",k,".tif"),
                       filetype = "GTiff", overwrite = TRUE)
    
    wftons_w_raster_list <- lapply(wftons_w_list, rast)
    wftons_ww <- do.call("merge", wftons_w_raster_list)
    terra::writeRaster(wftons_ww,
                       paste0("demand_out/",pop_ver,"_wftons_w_",k,".tif"),
                       filetype = "GTiff", overwrite = TRUE)
    
    wftons_v_raster_list <- lapply(wftons_v_list, rast)
    wftons_vv <- do.call("merge", wftons_v_raster_list)
    terra::writeRaster(wftons_vv,
                       paste0("demand_out/",pop_ver,"_wftons_v_",k,".tif"),
                       filetype = "GTiff", overwrite = TRUE)
    
    if (k == yr) {
      
      rururb_list <- list.files(path = "pop_temp/",
                                pattern = paste0(yr,"_rururbR.*\\.tif$|general.*\\.tif$"), full.names = TRUE)
      rururb_raster_list <- lapply(rururb_list, rast)
      rururb_ <- do.call("merge", rururb_raster_list)
      terra::writeRaster(rururb_,
                         paste0("pop_out/",pop_ver,"_rururb_",yr,".tif"),
                         filetype = "GTiff", overwrite = TRUE)
      
      percapbio_list <- list.files(path = "demand_temp/",
                                   pattern = paste0(yr,"_bio_percap.*\\.tif$|general.*\\.tif$"), full.names = TRUE)
      percapbio_raster_list <- lapply(percapbio_list, rast)
      percapbio_ <- do.call("merge", percapbio_raster_list)
      terra::writeRaster(percapbio_,
                         paste0("demand_out/",pop_ver,"_percapbio_",yr,".tif"),
                         filetype = "GTiff", overwrite = TRUE)
      
      percapcha_list <- list.files(path = "demand_temp/",
                                   pattern = paste0(yr,"_cha_percap.*\\.tif$|general.*\\.tif$"), full.names = TRUE)
      percapcha_raster_list <- lapply(percapcha_list, rast)
      percapcha_ <- do.call("merge", percapcha_raster_list)
      terra::writeRaster(percapcha_,
                         paste0("demand_out/",pop_ver,"_percapcha_",yr,".tif"),
                         filetype = "GTiff", overwrite = TRUE)
      
      wftons_w_list <- list.files(path = "demand_temp/",
                                  pattern = paste0(yr,"_wftons_w.*\\.tif$"), full.names = TRUE)
      wftons_w_raster_list <- lapply(wftons_w_list, rast)
      wftons_ww <- do.call("merge", wftons_w_raster_list)
      terra::writeRaster(wftons_ww,
                         paste0("demand_out/",pop_ver,"_wftons_w_",yr,".tif"),
                         filetype = "GTiff", overwrite = TRUE)
      
      wftons_v_list <- list.files(path = "demand_temp/",
                                  pattern = paste0(yr,"_wftons_v.*\\.tif$"), full.names = TRUE)
      wftons_v_raster_list <- lapply(wftons_v_list, rast)
      wftons_vv <- do.call("merge", wftons_v_raster_list)
      terra::writeRaster(wftons_vv,
                         paste0("demand_out/",pop_ver,"_wftons_v_",yr,".tif"),
                         filetype = "GTiff", overwrite = TRUE)
      
      
    }
    
    
  } else { 
    
    rast(cha_list) %>%
      terra::writeRaster(paste0("pop_out/",pop_ver,"_cha_users_",k,".tif"),
                         filetype = "GTiff", overwrite = TRUE)
    
    rast(chaDem_list) %>%
      terra::writeRaster(paste0("demand_out/",pop_ver,"_cha_demand_",k,".tif"),
                         filetype = "GTiff", overwrite = TRUE)
    
    rast(bio_list) %>%
      terra::writeRaster(paste0("pop_out/",pop_ver,"_bio_users_",k,".tif"),
                         filetype = "GTiff", overwrite = TRUE)
    
    rast(bioDem_list) %>%
      terra::writeRaster(paste0("demand_out/",pop_ver,"_bio_demand_",k,".tif"),
                         filetype = "GTiff", overwrite = TRUE)
    
    rast(pop_list) %>%
      terra::writeRaster(paste0("pop_out/",pop_ver,"_popadj_",k,".tif"),
                         filetype = "GTiff", overwrite = TRUE)
    
    rast(wftons_w_list) %>%
      terra::writeRaster(paste0("demand_out/",pop_ver,"_wftons_w_",k,".tif"),
                         filetype = "GTiff", overwrite = TRUE)
    
    rast(wftons_v_list) %>%
      terra::writeRaster(paste0("demand_out/",pop_ver,"_wftons_v_",k,".tif"),
                         filetype = "GTiff", overwrite = TRUE)
    
    
    if (k == yr) {
      rururb_list <- list.files(path = "pop_temp/",
                                pattern = paste0(yr,"_rururbR.*\\.tif$"), full.names = TRUE)
      rast(rururb_list) %>%
        terra::writeRaster(paste0("pop_out/",pop_ver,"_rururb_",yr,".tif"),
                           filetype = "GTiff", overwrite = TRUE)
    }
    
  }
  
}

# Save in a format ingestible by MoFuSS (IDW C++ script) ----
# setwd(demanddir)
# Important to remove zeros from both  / Moreover for subcountry == 1 

# Walking
if (optimizeD == 1) {
  keep(annos__2018, optimizeD, gitlabdir, country, countrydir, #endpath,
       gitlabdir, country, countrydir, demanddir, admindir, emissionsdir, rTempdir, 
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

# Old version
# wf_w_db <- as.data.frame(wf_w_st, row.names="ID",  na.rm=TRUE, xy = TRUE, centroids = TRUE )
# head(wf_w_db) 

# New version
# Extract x and y coordinates from the first layer
xy_coords <- as.data.frame(wf_w_st[[1]], xy = TRUE)[, c("x", "y")]

# Initialize the data frame with x, y, and centroids columns
wf_w_dbx <- xy_coords
wf_w_dbx$centroids <- TRUE

# Loop through each layer and bind the results as columns
for (i in 1:nlyr(wf_w_st)) {
  temp_dfw <- as.data.frame(wf_w_st[[i]], na.rm = TRUE)
  # Bind the data frame column-wise
  wf_w_dbx <- cbind(wf_w_dbx, temp_dfw)
  rm(temp_dfw)
  gc()
}
wf_w_db <- wf_w_dbx %>%
  relocate(centroids, .after = last_col())

# Output result
head(wf_w_db) # Check the structure
# all.equal(wf_w_db, wf_w_db2)
colnames(wf_w_db) <- c("x","y",paste0(annos__2018,"_fw_w"),"centroids")
wf_w_db4idw_prezero <- tibble::rownames_to_column(wf_w_db, "ID")
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

# Vehicle
if (optimizeD == 1) {
  keep(annos__2018, optimizeD, gitlabdir, country, countrydir, #endpath,
       gitlabdir, country, countrydir, demanddir, admindir, emissionsdir, rTempdir, 
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

# Old version
# wf_v_db <- as.data.frame(wf_v_st, row.names="ID",  na.rm=TRUE, xy = TRUE, centroids = TRUE )
# head(wf_v_db) 

# New version
# Extract x and y coordinates from the first layer
xy_coords <- as.data.frame(wf_v_st[[1]], xy = TRUE)[, c("x", "y")]

# Initialize the data frame with x, y, and centroids columns
wf_v_dbx <- xy_coords
wf_v_dbx$centroids <- TRUE

# Loop through each layer and bind the results as columns
for (i in 1:nlyr(wf_v_st)) {
  temp_dfv <- as.data.frame(wf_v_st[[i]], na.rm = TRUE)
  # Bind the data frame column-wise
  wf_v_dbx <- cbind(wf_v_dbx, temp_dfv)
  rm(temp_dfv)
  gc()
}
wf_v_db <- wf_v_dbx %>%
  relocate(centroids, .after = last_col())

# Output result
head(wf_v_db) # Check the structure
# all.equal(wf_w_db, wf_w_db2)
colnames(wf_v_db) <- c("x","y",paste0(annos__2018,"_fw_v"),"centroids")
wf_v_db4idw_prezero <- tibble::rownames_to_column(wf_v_db, "ID") 
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

