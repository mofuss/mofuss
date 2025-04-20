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
# Version 3
# Date: Mar 2024

# 2dolist ----
# Fix for linux cluster
# Land Use Land Cover Module
# Improve add_subadmin YES/NO, check it works as mask == analysis for any of the four scales: Global, Continental, Regional, Country.
# Description VS description 
# if (webmofuss == 1){
# bind_cols (polykml, dfx) %>%
#   st_zm() %>%
#   subset(select=-c(Name,description)) -> userarea_GCS
# } else if (webmofuss == 0){
#   bind_cols (polykml, dfx) %>%
#     st_zm() %>%
#     subset(select=-c(Name,description)) -> userarea_GCS
# }
# userarea <- st_transform(userarea_GCS, epsg_pcs)
# # Land Use Land Cover Module ####
# if (webmofuss == 0) {
#   
#   if (LULCt1map == "YES"){
#     dir.create("LULCC/lucdynamics_luc1")
#     dir.create("LULCC/lucdynamics_luc1/out_lulcc")
#     lulcc.egoml <- list.files (paste0(gitlabdir, "/localhost/scripts/LULCC/LULCt1_c/"))
#     file.copy(from=paste0(gitlabdir, "/localhost/scripts/LULCC/LULCt1_c/",lulcc.egoml), 
#               to=paste0(countrydir, "/LULCC/lucdynamics_luc1"), 
#               overwrite = TRUE)
#     system(paste0(countrydir, "/LULCC/lucdynamics_luc1/LULCC_blackbox_scripts2.bat"))
#     sh: 1: /home/mofuss/haiti_linux-c3/LULCC/lucdynamics_luc1/LULCC_blackbox_scripts2.bat: Permission denied
# AGB maps ---- # ADD TWO MORE MAPS
# 
# FIX ECOREGIONS FOR REGIONAL AND AOI == 1 and subadmin =YES LINES 464 - 467
#
# AGB maps ---- # ADD TWO MORE MAPS



# Internal parameters ----
temdirdefined = 1
# Attraction buffer zones (in linear meters)
w0 = 0 
w1 = 50000 #250000
w2 = 100000 #500000
w3 = 150000 #750000
w4 = 200000 #1000000

# # Select MoFuSS platform:
# webmofuss = 1 # "1" is  web-MoFuSS running in our Ubuntu server, "0" is localcal host (Windows or Linux)
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
library(dplyr)
library(fasterize)
library(gitlabr)
library(glue)
library(igraph)
library(inline)
library(purrr)
library(raster)
library(readr)
library(readxl)
library(rgl)
library(sf)
library(spam)
library(stars)
library(svDialogs)
library(tcltk)
library(terra)
library(tictoc)
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
  pull(ParCHR) -> proj_gcs #GCSproj

country_parameters %>%
  dplyr::filter(Var == "epsg_gcs") %>%
  pull(ParCHR) %>%
  as.integer(.) -> epsg_gcs #epsg_gcs

country_parameters %>%
  dplyr::filter(Var == "proj_pcs") %>%
  pull(ParCHR) -> proj_pcs #UTMproj

country_parameters %>%
  dplyr::filter(Var == "epsg_pcs") %>%
  pull(ParCHR) %>%
  as.integer(.) -> epsg_pcs #epsg_utm

country_parameters %>%
  dplyr::filter(Var == "proj_authority") %>%
  pull(ParCHR) -> proj_authority

country_parameters %>%
  dplyr::filter(Var == "LULCt1map") %>%
  pull(ParCHR) -> LULCt1map

country_parameters %>%
  dplyr::filter(Var == "LULCt2map") %>%
  pull(ParCHR) -> LULCt2map

country_parameters %>%
  dplyr::filter(Var == "LULCt3map") %>%
  pull(ParCHR) -> LULCt3map

country_parameters %>%
  dplyr::filter(Var == "nameuser") %>%
  pull(ParCHR) -> nameuser

country_parameters %>%
  dplyr::filter(Var == "ads") %>%
  pull(ParCHR) -> ads

country_parameters %>%
  dplyr::filter(Var == "ads_ctry") %>%
  pull(ParCHR) -> ads_ctry

country_parameters %>%
  dplyr::filter(Var == "GEE_scale") %>%
  pull(ParCHR) %>%
  as.integer(.) -> resolution

country_parameters %>%
  dplyr::filter(Var == "aoi_poly") %>%
  pull(ParCHR) %>%
  as.integer(.) -> aoi_poly

country_parameters %>%
  dplyr::filter(Var == "scenario_ver") %>%
  pull(ParCHR) -> scenario_ver

country_parameters %>%
  dplyr::filter(Var == "byregion") %>%
  pull(ParCHR) -> byregion

country_parameters %>%
  dplyr::filter(Var == "add_subadmin") %>%
  pull(ParCHR) -> add_subadmin

# Select a region
if (aoi_poly == 1) {
  # Handle the case where aoi_poly is 1, regardless of byregion
  cat("aoi_poly is set to 1. This overrides other conditions.\n")
  
  country_parameters %>%
    dplyr::filter(Var == "aoi_poly_file") %>%
    pull(ParCHR) -> aoi_poly_file
  
  # Define file paths
  kml_file_path <- Sys.glob(paste0(countrydir,"/LULCC/SourceData/InVector_GCS/",aoi_poly_file))
  # Read the SpatVector files
  kml_data <- vect(kml_file_path) # Read the .kml file
  # plot(kml_data)
  # Reassign the CRS of kml_data to match mofuss_regions0_gpkg
  
  # Re-read mofuss_regions0_gpkg for the case the object does not exist
  if (!exists("mofuss_regions0_gpkg")) {
    mofuss_regions0_gpkg <- vect(st_read(paste0(demanddir,"/demand_in/mofuss_regions0.gpkg")))
  }
  
  crs(kml_data) <- crs(mofuss_regions0_gpkg)
  # Ensure both layers are in the same projection
  if (!crs(mofuss_regions0_gpkg) == crs(kml_data)) {
    stop("Projections do not match!")
  }
  # Intersect the two layers to calculate the overlapping areas
  overlap <- try(terra::intersect(kml_data, mofuss_regions0_gpkg), silent = TRUE)
  # Check if the result is valid
  if (inherits(overlap, "try-error") || is.null(overlap) || length(overlap) == 0) {
    stop("No valid overlap found between the KML file and the GPKG regions.")
  }
  
  # Calculate area
  overlap$area <- terra::expanse(overlap, unit = "km")
  
  # Extract
  overlap_df <- as.data.frame(overlap)
  
  # Build summary
  overlap_summary <- overlap_df %>%
    dplyr::select(GID_0, area) %>%
    dplyr::mutate(total_area = area) %>%
    dplyr::select(GID_0, total_area)
  # Check if overlap_summary is empty
  if (nrow(overlap_summary) == 0) {
    stop("No overlapping regions found.")
  }
  
  # Find largest
  largest_overlap <- overlap_summary %>%
    arrange(desc(total_area)) %>%
    slice_head(n = 1)
  
  # Match to country
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
  
} else if (byregion == "Regional" & aoi_poly == 0) {
  country_parameters %>%
    dplyr::filter(Var == "region2BprocessedReg") %>%
    pull(ParCHR) -> mofuss_region
  
} else if (byregion == "Country" & aoi_poly == 0) {
  country_parameters %>%
    dplyr::filter(Var == "region2BprocessedCtry_iso") %>%
    pull(ParCHR) -> mofuss_region
  
} else {
  # Handle any other conditions if necessary
  cat("No specific conditions met.\n")
}

# Define paths and patterns to delete
paths_to_clean <- list(
  "Debugging/*.*",
  "Temp/*.*",
  "HTML_animation*/*",
  "*.Rout",
  # "*.txt", # Uncomment to erase specific txt files
  "*.log",
  "Mofuss_Summary_Report.aux",
  "Mofuss_Summary_Report.lof",
  "Mofuss_Summary_Report.lot",
  "Mofuss_Summary_Report.out",
  "Mofuss_Summary_Report.toc",
  "In/*.*",
  "In/IDW_boost/*.*",
  "Out*/*",
  "LaTeX/*.pdf",
  "LaTeX/*.mp4",
  "LaTeX/*.csv",
  "LaTeX/SimLength.txt",
  "LaTeX/MCruns.txt",
  "Summary_Report/*.*",
  "Logs/*.*",
  "LULCC/TempRaster/*.*",
  "LULCC/TempTables/Friction_drivingoverroads.csv",
  "LULCC/TempTables/Friction_drivingoverroads_calcdist.csv",
  "LULCC/TempTables/Friction_rivers_reclass.csv",
  "LULCC/TempTables/Friction_rivers_reclass_calcdist.csv",
  "LULCC/TempTables/Friction_lakes_reclass.csv",
  "LULCC/TempTables/Friction_lakes_reclass_calcdist.csv",
  "LULCC/TempTables/Friction_borders_reclass.csv",
  "LULCC/TempTables/Friction_borders_reclass_calcdist.csv",
  "LULCC/TempTables/Friction_walkingcrosscountry.csv",
  "LULCC/TempTables/Friction_walkingoverroads.csv",
  "LULCC/TempTables/Supply_parameters.csv",
  "LULCC/TempTables/Resolution.csv",
  "LULCC/TempVector/*.*",
  "LULCC/TempVector_GCS/*.*",
  # "LULCC//*.txt", # Uncomment to delete specific txt files
  "LULCC/*.csv",	
  "LULCC/InTables//fwuse*.*",
  "LULCC/CroppingCountrySpecific.R",
  "LULCC/CroppingCountrySpecific.Rout"
)

# Apply unlink to all paths
walk(paths_to_clean, ~ unlink(.x, recursive = TRUE, force = TRUE))

# Define directories to remove if they exist
dirs_to_check <- c(
  "LULCC/lucdynamics_luc1", 
  "LULCC/lucdynamics_luc2", 
  "LULCC/lucdynamics_luc3"
)

# Check and remove directories if they exist
walk(dirs_to_check, ~ if (dir.exists(.x)) unlink(.x, recursive = TRUE, force = TRUE))

Country <- readLines("LULCC/TempTables/Country.txt")

flist <- list.files("LULCC/SourceData/InTables")
file.copy(paste0("LULCC/SourceData/InTables/",flist), "LULCC/TempTables")

## Read supply parameters table, checking if its delimiter is comma or semicolon ####

growth_parameters1 <- if (file.exists("LULCC/TempTables/growth_parameters1.csv") == TRUE) {
  read_csv("LULCC/TempTables/growth_parameters1.csv") %>% 
    {if(is.null(.$TOF[1])) read_csv2("LULCC/TempTables/growth_parameters1.csv") else .} 
}

growth_parameters2 <- if (file.exists("LULCC/TempTables/growth_parameters2.csv") == TRUE) {
  read_csv("LULCC/TempTables/growth_parameters2.csv") %>% 
    {if(is.null(.$TOF[1])) read_csv2("LULCC/TempTables/growth_parameters2.csv") else .}
}

growth_parameters3 <- if (file.exists("LULCC/TempTables/growth_parameters3.csv") == TRUE) {
  read_csv("LULCC/TempTables/growth_parameters3.csv") %>% 
    {if(is.null(.$TOF[1])) read_csv2("LULCC/TempTables/growth_parameters3.csv") else .}
}

# Save resolution as csv for down stream uses
write.csv(resolution, "LULCC/TempTables/Resolution.csv")

#save user data as txt for down stream uses
writeLines(paste0(gsub("_"," ",nameuser),", from the ",
                  gsub("_"," ",ads),", in ", 
                  gsub("_"," ",ads_ctry),","),"LULCC/TempTables/UserData.txt", useBytes=T)
readLines("LULCC/TempTables/UserData.txt")

# AoI SELECTION **STARTS** ----

if (aoi_poly == 1) {
  # Handle the case where aoi_poly is 1, regardless of byregion
  cat("aoi_poly is set to 1. This overrides other conditions.\n")
  
  country_parameters %>%
    dplyr::filter(Var == "aoi_poly_file") %>%
    pull(ParCHR) -> aoi_poly_file
  
  # Define file paths
  kml_file_path <- Sys.glob(paste0(countrydir,"/LULCC/SourceData/InVector_GCS/",aoi_poly_file))
  # Read the SpatVector files
  # kml_data <- vect(kml_file_path) # Read the .kml file (Terra)
  polykml <- st_read(kml_file_path) # Read the .kml file (SF)

  dfx =  data.frame(1,"GoogleEarthPoly")
  colnames(dfx) <- c( country_parameters %>%
                        dplyr::filter(Var == "ext_analysis_ID") %>%
                        pull(ParCHR), country_parameters %>%
                        dplyr::filter(Var == "ext_analysis_NAME") %>%
                        pull(ParCHR))
  # if (webmofuss == 1){
  #   bind_cols (polykml, dfx) %>%
  #     st_zm() %>%
  #     subset(select=-c(Name,description)) -> userarea_GCS
  # } else if (webmofuss == 0){
  #   bind_cols (polykml, dfx) %>%
  #     st_zm() %>%
  #     subset(select=-c(Name,description)) -> userarea_GCS
  # }
  userarea_GCS <- bind_cols(polykml, dfx) %>%
    st_zm() %>%
    dplyr::select(-Name, -matches("description", ignore.case = TRUE))
  userarea <- st_transform(userarea_GCS, epsg_pcs)

  # Vector masks and extents Google Polygon
  setwd(admindir)
  extent_mask0 <- vect(st_read("regions_adm0_p/mofuss_regions0_p.gpkg")) %>%
    terra::subset(.$GID_0 == mofuss_region)
  mask <- st_as_sf(extent_mask0) %>%
    dplyr::mutate(ID = 1)
  # terra::writeVector(extent_mask0, "InVector/extent_mask.gpkg", overwrite = TRUE)
  setwd(countrydir)
  
  # mask <- userarea
  analysisshp <- st_intersection(mask, userarea)
  analysisshp_GCS <- st_transform(analysisshp, epsg_gcs)

  st_write(userarea_GCS, "LULCC/TempVector_GCS/userarea_GCS.gpkg", delete_layer=TRUE)
  st_write(userarea, "LULCC/TempVector/userarea.gpkg", delete_layer=TRUE)
  
  mask_GCS<-st_transform(mask,epsg_gcs)
  st_write (analysisshp,"LULCC/TempVector/ext_analysis.gpkg", delete_layer=TRUE)
  st_write (mask_GCS,"LULCC/TempVector_GCS/mask_gcs.gpkg", delete_layer=TRUE)
  st_write (analysisshp_GCS,"LULCC/TempVector_GCS/ext_analysis_gcs.gpkg", delete_layer=TRUE)
  st_write(mask, "LULCC/SourceData/InVector/extent_mask.gpkg", delete_layer=TRUE)
  
  setwd(admindir)
  ecoregions0 <- vect("ecoregions_p/ecoregions2017_p.gpkg", layer = "ecoregions_mofuss") %>%
    terra::subset(.$GID_0 == mofuss_region)
  terra::writeVector(ecoregions0, paste0(countrydir,"/LULCC/SourceData/InVector/ecoregions.gpkg"), overwrite = TRUE)
  # Save as shapefile
  writeVector(ecoregions0, filename = paste0(countrydir,"/LULCC/SourceData/InVector/ecoregions.shp"), filetype = "ESRI Shapefile", overwrite = TRUE)
  setwd(countrydir)
  
} else if (aoi_poly == 0) {
  
  setwd(admindir)
  
  unlink("InVector/*.*")
  
  if (byregion == "Continental"){ ## Continent ---- THIS LEVEL REQUIERS TO BE UPDATED!!!
    
    extent_mask0 <- st_read("regions_adm0_p/mofuss_regions0_p.gpkg") %>%
      dplyr::filter(grepl(mofuss_region,mofuss_reg)) %>%
      dplyr::mutate(ID = seq(1:nrow(.)))
    st_write(extent_mask0, "InVector/extent_mask.gpkg", overwrite = TRUE)
    
    ecoregions0 <- vect("ecoregions_p/ecoregions2017_p.gpkg", layer = "ecoregions_mofuss") %>%
      terra::subset(.$GID_0 == mofuss_region)
    terra::writeVector(ecoregions0, "InVector/ecoregions.gpkg", overwrite = TRUE)
    # Save as shapefile
    writeVector(ecoregions0, filename = paste0(countrydir,"/LULCC/SourceData/InVector/ecoregions.shp"), filetype = "ESRI Shapefile", overwrite = TRUE)
    
    countries.list <- extent_mask0 %>%
      as.data.frame() %>%
      dplyr::select(NAME_0) %>%
      unique() %>%
      arrange(NAME_0)
    
    if (add_subadmin == "YES"){ # WARNING: THIS CHUNK IS BROKEN AND STILL USES NAME_0 INSTEAD OF GID_0, WON'T WORK IN WEBMOFUSS
      country.input <- dlgList(as.character(countries.list[ , ]),
                               preselect = "Kenya",
                               multiple = FALSE, # Check if multiple countries or values is doable
                               title = "Choose one country to process",
                               gui = .GUI
      )
      mofuss_country <- country.input$res
      extent_mask0 %>%
        terra::subset(.$NAME_0 == mofuss_country) %>%  # Remember to change in the parameters table (!?)
        sf::st_as_sf() %>%  # Convert to sf object
        st_write("InVector/extent_analysis.gpkg", overwrite = TRUE)
    } else if (add_subadmin == "NO"){
      print("Nothing Happens")
    }
    
    # ADM LEVEL = 1, when running ADM LEVEL = 0
    mask1list <- paste0("regions_adm1_p/",list.files(path = paste0("regions_adm1_p/"),
                                                     pattern = mofuss_region, full.names = FALSE))
    mask1list_sf <- lapply(mask1list, st_read)
    mofuss_regions1_gpkg <- do.call(rbind, mask1list_sf) %>%
      dplyr::mutate(ID = seq(1:nrow(.)))
    st_write(mofuss_regions1_gpkg, "InVector/extent_mask1.gpkg", overwrite = TRUE)
    debugdfadm1 <- mofuss_regions1_gpkg %>% st_drop_geometry()
    
    mofuss_regions1_gpkg %>%
      terra::subset(.$NAME_0 == mofuss_country) %>% # Remember to change in the parameters table FIX
      st_write("InVector/extent_analysis1.gpkg", overwrite = TRUE)
    
    # ADM LEVEL = 2, when running ADM LEVEL = 0 #Slow process, elapsed time:
    mask2list <- paste0("regions_adm2_p/",list.files(path = paste0("regions_adm2_p/"),
                                                     pattern = mofuss_region, full.names = FALSE))
    mask2list_sf <- lapply(mask2list, st_read)
    mofuss_regions2_gpkg <- do.call(rbind, mask2list_sf) %>%
      dplyr::mutate(ID = seq(1:nrow(.)))
    st_write(mofuss_regions2_gpkg, "InVector/extent_mask2.gpkg", overwrite = TRUE)
    debugdfadm2 <- mofuss_regions2_gpkg %>% st_drop_geometry()
    
    mofuss_regions2_gpkg %>%
      terra::subset(.$NAME_0 == mofuss_country) %>% # Remember to change in the parameters table FIX
      st_write("InVector/extent_analysis2.gpkg", overwrite = TRUE)
    
  }
  
  if (byregion == "Regional"){ ## Regional ----
    
    extent_mask0 <- vect(st_read(paste0("regions_adm0_p/",mofuss_region,"_p.gpkg")))
    terra::writeVector(extent_mask0, "InVector/extent_mask.gpkg", overwrite = TRUE)
    
    # Para que esto funciona debo primero asegurarme que ecoregions tiene el campo de las regiones de mofuss
    ecoregions0 <- vect("ecoregions_p/ecoregions2017_p.gpkg", layer = "ecoregions_mofuss") %>% 
      terra::subset(.$mofuss_reg == mofuss_region)
    terra::writeVector(ecoregions0, "InVector/ecoregions.gpkg", overwrite = TRUE)
    # Save as shapefile
    # Define full shapefile path without extension
    shp_path <- paste0(countrydir, "/LULCC/SourceData/InVector/ecoregions")
    
    # Delete existing files if they exist
    shp_extensions <- c(".shp", ".shx", ".dbf", ".prj", ".cpg")
    shp_files <- paste0(shp_path, shp_extensions)
    file.remove(shp_files[file.exists(shp_files)])
    
    # Now write the shapefile
    writeVector(ecoregions0, filename = paste0(shp_path, ".shp"), filetype = "ESRI Shapefile")
    
    countries.list <- extent_mask0 %>%
      as.data.frame() %>%
      dplyr::select(NAME_0) %>%
      unique() %>%
      arrange(NAME_0)
    
    if (add_subadmin == "YES"){ 
    country.input <- dlgList(as.character(countries.list[ , ]),
                             preselect = "Kenya",
                             multiple = FALSE, # Check if multiple countries or values is doable
                             title = "Choose one country to process",
                             gui = .GUI
    )
    mofuss_country <- country.input$res

    extent_mask0 %>%
      terra::subset(.$NAME_0 == mofuss_country) %>% #Remember to change in the parameters table
      terra::writeVector("InVector/extent_analysis.gpkg", overwrite = TRUE)

    } else if (add_subadmin == "NO"){
      print("Nothing Happens")
    }
    
    # ADM LEVEL = 1, when running ADM LEVEL = 0
    mofuss_regions1_gpkg <- vect("regions_adm1_p/mofuss_regions1_p.gpkg")
    mofuss_regions1 <- as.data.frame(mofuss_regions1_gpkg)
    
    mofuss_region1 <- gsub(0,1, mofuss_region)
    extent_mask1 <- vect(paste0("regions_adm1_p/",mofuss_region1,"_p.gpkg"))
    terra::writeVector(extent_mask1, "InVector/extent_mask1.gpkg", overwrite = TRUE)
    
    if (add_subadmin == "YES") {
      extent_mask1 %>%
        terra::subset(.$NAME_0 == mofuss_country) %>% #Remember to change in the parameters table
        terra::writeVector("InVector/extent_analysis1.gpkg", overwrite = TRUE)
    }
    
    mofuss_region2 <- gsub(0,2, mofuss_region)
    extent_mask2 <- vect(paste0("regions_adm2_p/",mofuss_region2,"_p.gpkg"))
    terra::writeVector(extent_mask2, "InVector/extent_mask2.gpkg", overwrite = TRUE)
    
    if (add_subadmin == "YES") {
      extent_mask2 %>%
        terra::subset(.$NAME_0 == mofuss_country) %>% #Remember to change in the parameters table
        terra::writeVector("InVector/extent_analysis2.gpkg", overwrite = TRUE)
    }
    
  }
  
  if (byregion == "Country"){ ## Country ----
    
    extent_mask0 <- vect(st_read("regions_adm0_p/mofuss_regions0_p.gpkg")) %>%
      terra::subset(.$GID_0 == mofuss_region)
    terra::writeVector(extent_mask0, "InVector/extent_mask.gpkg", overwrite = TRUE)
    
    extent_mask1 <- vect(st_read("regions_adm1_p/mofuss_regions1_p.gpkg")) %>%
      terra::subset(.$GID_0 == mofuss_region)
    terra::writeVector(extent_mask1, "InVector/extent_mask1.gpkg", overwrite = TRUE)
    
    extent_mask2 <- vect(st_read("regions_adm2_p/mofuss_regions2_p.gpkg")) %>%
      terra::subset(.$GID_0 == mofuss_region)
    terra::writeVector(extent_mask2, "InVector/extent_mask2.gpkg", overwrite = TRUE)
    
    ecoregions0 <- vect("ecoregions_p/ecoregions2017_p.gpkg", layer = "ecoregions_mofuss") %>%
      terra::subset(.$GID_0 == mofuss_region)
    terra::writeVector(ecoregions0, "InVector/ecoregions.gpkg", overwrite = TRUE)
    # Save as shapefile
    # Define full shapefile path without extension
    shp_path <- paste0(countrydir, "/LULCC/SourceData/InVector/ecoregions")
    
    # Delete existing files if they exist
    shp_extensions <- c(".shp", ".shx", ".dbf", ".prj", ".cpg")
    shp_files <- paste0(shp_path, shp_extensions)
    file.remove(shp_files[file.exists(shp_files)])
    
    # Now write the shapefile
    writeVector(ecoregions0, filename = paste0(shp_path, ".shp"), filetype = "ESRI Shapefile")

    if (add_subadmin == "YES") {
      
      adm1.list <- extent_mask1 %>%
        as.data.frame() %>%
        dplyr::select(NAME_1) %>%
        unique() %>%
        arrange(NAME_1)
      
      adm1.input <- dlgList(as.character(adm1.list[ , ]),
                            # preselect = "Kenya",
                            multiple = FALSE, # Check if multiple countries or values is doable
                            title = "Choose one polygon to process",
                            gui = .GUI
      )
      adm1_country <- adm1.input$res
      
      extent_mask1 %>%
        terra::subset(.$NAME_1 == adm1_country) %>%
        terra::writeVector("InVector/extent_analysis.gpkg", overwrite = TRUE)
      
      extent_mask2 %>%
        terra::subset(.$NAME_1 == adm1_country) %>%
        terra::writeVector("InVector/extent_analysis1.gpkg", overwrite = TRUE)
      
      extent_mask2 %>% # Repeats previous admin 2 level for being a country crop
        terra::subset(.$NAME_1 == adm1_country) %>%
        terra::writeVector("InVector/extent_analysis2.gpkg", overwrite = TRUE)
    } else {
      # terra::writeVector(extent_mask, "InVector/extent_analysis.gpkg", overwrite = TRUE)
      # terra::writeVector(extent_mask1, "InVector/extent_analysis1.gpkg", overwrite = TRUE)
      # terra::writeVector(extent_mask2, "InVector/extent_analysis2.gpkg", overwrite = TRUE)
    }

  }
  
  # Copy to MoFuSS ----
  setwd(countrydir)
  
  #ADM 0
  file.copy(from=paste0(admindir,"/InVector/extent_mask.gpkg"),
            to=paste0(countrydir,"/LULCC/SourceData/InVector"),
            overwrite = TRUE)

  # file.copy(from=paste0(admindir,"/InVector/extent_analysis.gpkg"),
  #           to=paste0(countrydir,"/LULCC/SourceData/InVector"),
  #           overwrite = TRUE)
  
  #ADM 1
  file.copy(from=paste0(admindir,"/InVector/extent_mask1.gpkg"),
            to=paste0(countrydir,"/LULCC/SourceData/InVector"),
            overwrite = TRUE)
  
  # file.copy(from=paste0(admindir,"/InVector/extent_analysis1.gpkg"),
  #           to=paste0(countrydir,"/LULCC/SourceData/InVector"),
  #           overwrite = TRUE)
  
  #ADM 2
  file.copy(from=paste0(admindir,"/InVector/extent_mask2.gpkg"),
            to=paste0(countrydir,"/LULCC/SourceData/InVector"),
            overwrite = TRUE)
  
  # file.copy(from=paste0(admindir,"/InVector/extent_analysis2.gpkg"),
  #           to=paste0(countrydir,"/LULCC/SourceData/InVector"),
  #           overwrite = TRUE)
  
  
  file.copy(from=paste0(admindir,"/InVector/ecoregions.gpkg"),
            to=paste0(countrydir,"/LULCC/SourceData/InVector"),
            overwrite = TRUE)

  if (add_subadmin == "YES") {
    #ADM 0
    file.copy(from=paste0(admindir,"/InVector/extent_analysis.gpkg"),
              to=paste0(countrydir,"/LULCC/SourceData/InVector"),
              overwrite = TRUE)
    #ADM 1
    file.copy(from=paste0(admindir,"/InVector/extent_analysis1.gpkg"),
              to=paste0(countrydir,"/LULCC/SourceData/InVector"),
              overwrite = TRUE)
    #ADM 2
    file.copy(from=paste0(admindir,"/InVector/extent_analysis2.gpkg"),
              to=paste0(countrydir,"/LULCC/SourceData/InVector"),
              overwrite = TRUE)
    userarea <- st_read("LULCC/SourceData/InVector/extent_analysis.gpkg")
    userarea_GCS <- st_transform(userarea, epsg_gcs)
    
    userarea1 <- st_read("LULCC/SourceData/InVector/extent_analysis1.gpkg")
    userarea_GCS1 <- st_transform(userarea, epsg_gcs)
    
    userarea2 <- st_read("LULCC/SourceData/InVector/extent_analysis2.gpkg")
    userarea_GCS2 <- st_transform(userarea, epsg_gcs)
    
  } else {
    
    userarea <- st_read("LULCC/SourceData/InVector/extent_mask.gpkg")
    userarea_GCS <- st_transform(userarea, epsg_gcs)
    
    userarea1 <- st_read("LULCC/SourceData/InVector/extent_mask1.gpkg")
    userarea_GCS1 <- st_transform(userarea, epsg_gcs)
    
    userarea2 <- st_read("LULCC/SourceData/InVector/extent_mask2.gpkg")
    userarea_GCS2 <- st_transform(userarea, epsg_gcs)
    
  }
  
  # Vector masks and extents GADM_AOI
  mask <- userarea
  analysisshp <- st_intersection(mask, userarea)
  analysisshp_GCS <- st_transform(analysisshp, epsg_gcs)
  
  mask1 <- userarea1
  analysisshp1 <- st_intersection(mask1, userarea1)
  analysisshp_GCS1 <- st_transform(analysisshp1, epsg_gcs)
  
  mask2 <- userarea2
  analysisshp2 <- st_intersection(mask2, userarea2)
  analysisshp_GCS2 <- st_transform(analysisshp2, epsg_gcs)
  
  st_write(userarea_GCS, "LULCC/TempVector_GCS/userarea_GCS.gpkg", delete_layer=TRUE)
  st_write(userarea, "LULCC/TempVector/userarea.gpkg", delete_layer=TRUE)
  
  st_write(userarea_GCS1, "LULCC/TempVector_GCS/userarea_GCS1.gpkg", delete_layer=TRUE)
  st_write(userarea1, "LULCC/TempVector/userarea1.gpkg", delete_layer=TRUE)
  
  st_write(userarea_GCS2, "LULCC/TempVector_GCS/userarea_GCS2.gpkg", delete_layer=TRUE)
  st_write(userarea2, "LULCC/TempVector/userarea2.gpkg", delete_layer=TRUE)
  
  # For future use in figures and IDW_Boost  ####
  mask_GCS<-st_transform(mask,epsg_gcs)
  st_write (analysisshp,"LULCC/TempVector/ext_analysis.gpkg", delete_layer=TRUE)
  st_write (mask_GCS,"LULCC/TempVector_GCS/mask_gcs.gpkg", delete_layer=TRUE)
  st_write (analysisshp_GCS,"LULCC/TempVector_GCS/ext_analysis_gcs.gpkg", delete_layer=TRUE)
  mask_GCS1<-st_transform(mask1,epsg_gcs)
  st_write (analysisshp1,"LULCC/TempVector/ext_analysis1.gpkg", delete_layer=TRUE)
  st_write (mask_GCS1,"LULCC/TempVector_GCS/mask_gc1s.gpkg", delete_layer=TRUE)
  st_write (analysisshp_GCS1,"LULCC/TempVector_GCS/ext_analysis_gcs1.gpkg", delete_layer=TRUE)
  mask_GCS2<-st_transform(mask2,epsg_gcs)
  st_write (analysisshp2,"LULCC/TempVector/ext_analysis2.gpkg", delete_layer=TRUE)
  st_write (mask_GCS2,"LULCC/TempVector_GCS/mask_gcs2.gpkg", delete_layer=TRUE)
  st_write (analysisshp_GCS2,"LULCC/TempVector_GCS/ext_analysis_gcs2.gpkg", delete_layer=TRUE)
  
} else {
  print("NO SELECTION AREA")
}
# AoI SELECTION **ENDS** ----

# Create a raster mask of a certain size size, extent, resolution and projection as a provided raster template for the following operations. ####

# Convert vector to SpatRaster with specified resolution
userarea_ras <- rast(userarea, resolution = resolution)

# Ensure the correct CRS is assigned
crs(userarea_ras) <- paste0("+",proj_pcs)

# Rasterize the userarea using the specified field
userarea_ras <- rasterize(
  userarea,
  userarea_ras,
  field = country_parameters %>%
    filter(Var == "ext_analysis_ID") %>%
    pull(ParCHR)
)

# Crop and mask the raster
userarea_r_m <- mask(crop(userarea_ras, mask), mask)

# Ensure the CRS is maintained
crs(userarea_r_m) <- paste0("+",proj_pcs)

# Convert raster values to 1
userarea_r <- (userarea_r_m * 0) + 1

# Ensure final raster has the correct CRS
crs(userarea_r) <- paste0("+",proj_pcs)

# Save the raster
writeRaster(userarea_r, filename="LULCC/TempRaster/mask_c.tif", 
            datatype="INT2S", overwrite=TRUE)

# Ensure mask is a SpatRaster and userarea_r is properly defined
mask_r <- rasterize(
  mask, userarea_r,
  field = country_parameters %>%
    filter(Var == "ext_analysis_ID") %>%
    pull(ParCHR)
)

# Assign correct CRS
crs(mask_r) <- paste0("+",proj_pcs)

# Crop and mask the raster
mask_r_m <- mask(crop(mask_r, userarea_r), userarea_r)

# Ensure CRS is maintained
crs(mask_r_m) <- paste0("+",proj_pcs)

# Write raster to file
writeRaster(mask_r_m, filename = "LULCC/TempRaster/admin_c.tif",
            datatype = "INT2S", overwrite = TRUE)


# Rasterize the ecoregions using the specified field
ecoregions_ras <- rasterize(
  ecoregions0,
  userarea_ras,
  field = country_parameters %>%
    filter(Var == "ecoregions_ID") %>%
    pull(ParCHR)
)

# Assign correct CRS
crs(ecoregions_ras) <- paste0("+",proj_pcs)

# Crop and mask the raster
ecoregions_ras_m <- terra::mask(crop(ecoregions_ras, userarea_r), userarea_r)

# Ensure CRS is maintained
crs(ecoregions_ras_m) <- paste0("+",proj_pcs)

# Write raster to file
writeRaster(ecoregions_ras_m, filename = "LULCC/TempRaster/ecoregions_c.tif",
            datatype = "INT2S", overwrite = TRUE)

if (aoi_poly != 1) {
  
  # Process userarea1
  userarea_ras1 <- rasterize(
    userarea1, userarea_ras,
    field = country_parameters %>%
      filter(Var == "ext_analysis_ID_1") %>%
      pull(ParCHR)
  )
  
  # Assign correct CRS
  crs(userarea_ras1) <- paste0("+",proj_pcs)
  
  userarea_r_m1 <- mask(crop(userarea_ras1, mask), mask)
  userarea_r1 <- (userarea_r_m1 * 0) + 1
  
  # Ensure CRS is maintained
  crs(userarea_r1) <- paste0("+",proj_pcs)
  
  writeRaster(userarea_r1, filename = "LULCC/TempRaster/mask_c1.tif", 
              datatype = "INT2S", overwrite = TRUE)
  
  mask_r1 <- rasterize(
    mask1, userarea_r1,
    field = country_parameters %>%
      filter(Var == "ext_analysis_ID_1") %>%
      pull(ParCHR)
  )
  
  # Assign correct CRS
  crs(mask_r1) <- paste0("+",proj_pcs)
  
  mask_r_m1 <- mask(crop(mask_r1, userarea_r1), userarea_r1)
  
  # Ensure CRS is maintained
  crs(mask_r_m1) <- paste0("+",proj_pcs)
  
  writeRaster(mask_r_m1, filename = "LULCC/TempRaster/admin_c1.tif", 
              datatype = "INT2S", overwrite = TRUE)
  
  # Process userarea2
  userarea_ras2 <- rasterize(
    userarea2, userarea_ras,
    field = country_parameters %>%
      filter(Var == "ext_analysis_ID_2") %>%
      pull(ParCHR)
  )
  
  # Assign correct CRS
  crs(userarea_ras2) <- paste0("+",proj_pcs)
  
  userarea_r_m2 <- mask(crop(userarea_ras2, mask), mask)
  userarea_r2 <- (userarea_r_m2 * 0) + 1
  
  # Ensure CRS is maintained
  crs(userarea_r2) <- paste0("+",proj_pcs)
  
  writeRaster(userarea_r2, filename = "LULCC/TempRaster/mask_c2.tif", 
              datatype = "INT2S", overwrite = TRUE)
  
  mask_r2 <- rasterize(
    mask2, userarea_r2,
    field = country_parameters %>%
      filter(Var == "ext_analysis_ID_2") %>%
      pull(ParCHR)
  )
  
  # Assign correct CRS
  crs(mask_r2) <- paste0("+",proj_pcs)
  
  mask_r_m2 <- mask(crop(mask_r2, userarea_r2), userarea_r2)
  
  # Ensure CRS is maintained
  crs(mask_r_m2) <- paste0("+",proj_pcs)
  
  writeRaster(mask_r_m2, filename = "LULCC/TempRaster/admin_c2.tif", 
              datatype = "INT2S", overwrite = TRUE)
}


# process DEM with TERRA ----
country_parameters %>%
  dplyr::filter(Var == "DTEM_name") %>%
  pull(ParCHR) -> DTEM_name
dtem <- rast(paste0("LULCC/SourceData/InRaster/",DTEM_name))
DEM_r_m <- dtem %>%
  terra::crop(ext(userarea_r)) %>%
  terra::resample(userarea_r, method = "bilinear") %>%
  terra::mask(userarea_r)
names(DEM_r_m) <- "layer_0"
writeRaster(DEM_r_m, filename="LULCC/TempRaster/DEM_c.tif", datatype="INT2S", overwrite=TRUE)

# tree cover, forest loss and forest gain ####
country_parameters %>%
  dplyr::filter(Var == "treecover_name") %>%
  pull(ParCHR) -> treecover_name
tc2000_r_m <- rast(paste0("LULCC/SourceData/InRaster/",treecover_name)) %>%
  terra::crop(ext(userarea_r)) %>%
  terra::resample(userarea_r, method = "bilinear") %>%
  terra::mask(userarea_r)
names(tc2000_r_m) <- "layer_0"
writeRaster(tc2000_r_m, filename="LULCC/TempRaster/tc2000_c.tif", datatype="INT2S", overwrite=TRUE)

country_parameters %>% # Something weird here, it takes too long
  dplyr::filter(Var == "gain_name") %>%
  pull(ParCHR) -> gain_name
gain_r_m <- rast(paste0("LULCC/SourceData/InRaster/",gain_name)) %>%
  terra::crop(ext(userarea_r)) %>%
  terra::resample(userarea_r, method = "bilinear") %>%
  terra::mask(userarea_r)
names(gain_r_m) <- "layer_0"
writeRaster(gain_r_m, filename="LULCC/TempRaster/gain_c.tif", datatype="INT2S", overwrite=TRUE)

country_parameters %>%
  dplyr::filter(Var == "lossyear_name") %>%
  pull(ParCHR) -> lossyear_name
lossyear_r_m <- rast(paste0("LULCC/SourceData/InRaster/",lossyear_name)) %>%
  terra::crop(ext(userarea_r)) %>%
  terra::resample(userarea_r, method = "bilinear") %>%
  terra::mask(userarea_r)
names(lossyear_r_m) <- "layer_0"
writeRaster(lossyear_r_m, filename="LULCC/TempRaster/lossyear_c.tif", datatype="INT2S", overwrite=TRUE)

# memory.limit(size=56000)
# Loss maps annualizations for LULCC modeling ####
lossyear_r_m %>%
  {.} %in% 1:20 %>%
  writeRaster(filename="LULCC/TempRaster/lossyear01_20_c.tif", datatype="INT2S", overwrite=TRUE)

lossyear_r_m %>% 
  {.} %in% 1:10 %>%
  writeRaster(filename="LULCC/TempRaster/lossyear01_10_c.tif", datatype="INT2S", overwrite=TRUE)

# Annual losses using apply or map ####
#tic()
seq(1:20) %>% 
  walk(function(i){
    lossyear_r_m[lossyear_r_m == i] <- 1
    lossyear_r_m[lossyear_r_m != i] <- 0
    #lossyear_r_m[lossyear_r_m != i | is.na(lossyear_r_m)] <- 0
    lossyear_r_m %>%
      writeRaster(str_c("LULCC/TempRaster/AnnLoss", str_pad(i, width = 2, pad = "0"),".tif"), datatype = "INT2S",overwrite = T)
    })
#toc()

# Annual losses using apply or map PARALLELIZED --- try in linux ####
# tic()
# plan(multisession, workers = 8)
# seq(1:12) %>%
#   future_map(function(i){
#     lossyear_r_m[lossyear_r_m == i] <- 1
#     lossyear_r_m[lossyear_r_m != i] <- 0
#     #lossyear_r_m[lossyear_r_m != i | is.na(lossyear_r_m)] <- 0
#     lossyear_r_m %>%
#       writeRaster(str_c("TempRaster/AnnLoss", str_pad(i, width = 2, pad = "0"),".tif"), datatype = "INT2S",overwrite = T)
#   })
# toc()

# LULC maps ####
country_parameters %>%
  dplyr::filter(Var == "LULCt1map") %>%
  pull(ParCHR) -> LULCt1map
if (identical(LULCt1map, NA_character_)) {
  print("No LULCt1 map available")
  } else if (LULCt1map == "YES"){
    LULCt1_r_m <- rast(paste0("LULCC/SourceData/InRaster/",country_parameters %>% # NAME IS HARDWIRED! ONE YEAR ONLY
                    dplyr::filter(Var == "LULCt1map_name") %>%
                    pull(ParCHR))) %>%
      terra::crop(ext(userarea_r)) %>%
      terra::resample(userarea_r, "near") %>%
      terra::mask(userarea_r)
    names(LULCt1_r_m) <- "layer_0"
    writeRaster(LULCt1_r_m, filename="LULCC/TempRaster/LULCt1_c.tif", datatype="INT2S", overwrite=TRUE)
    } else {
      "No LULCt1 map available"
    }

country_parameters %>%
  dplyr::filter(Var == "LULCt2map") %>%
  pull(ParCHR) -> LULCt2map
if (identical(LULCt2map, NA_character_)) {
  print("No LULCt2 map available")
} else if (LULCt2map == "YES"){
  LULCt2_r_m <- rast(paste0("LULCC/SourceData/InRaster/",country_parameters %>% # NAME IS HARDWIRED! ONE YEAR ONLY
                              dplyr::filter(Var == "LULCt2map_name") %>%
                              pull(ParCHR))) %>%
    terra::crop(ext(userarea_r)) %>%
    terra::resample(userarea_r, "near") %>%
    terra::mask(userarea_r)
  names(LULCt2_r_m) <- "layer_0"
  writeRaster(LULCt2_r_m, filename="LULCC/TempRaster/LULCt2_c.tif", datatype="INT2S", overwrite=TRUE)
} else {
  "No LULCt2 map available"
}

country_parameters %>%
  dplyr::filter(Var == "LULCt3map") %>%
  pull(ParCHR) -> LULCt3map
if (identical(LULCt3map, NA_character_)) {
  print("No LULCt3 map available")
} else if (LULCt3map == "YES"){
  LULCt3_r_m <- rast(paste0("LULCC/SourceData/InRaster/",country_parameters %>% # NAME IS HARDWIRED! ONE YEAR ONLY
                              dplyr::filter(Var == "LULCt3map_name") %>%
                              pull(ParCHR))) %>%
    terra::crop(ext(userarea_r)) %>%
    terra::resample(userarea_r, "near") %>%
    terra::mask(userarea_r)
  names(LULCt3_r_m) <- "layer_0"
  writeRaster(LULCt3_r_m, filename="LULCC/TempRaster/LULCt3_c.tif", datatype="INT2S", overwrite=TRUE)
} else {
  "No LULCt3 map available"
}

# Reclassify LULC map into TOF (1) and FOR (0) binary map as mask for LULCC analysis mask #### 
if (identical(LULCt1map, NA_character_)) {
  "No LULCt1 map available"
} else if (LULCt1map == "YES") {
  TOFvsFOR_matrix1 <- as.matrix(growth_parameters1[, c("Key*", "TOF")])
  LULCt1_r_m_reclass <- classify(LULCt1_r_m, TOFvsFOR_matrix1)
  writeRaster(LULCt1_r_m_reclass, filename = "LULCC/TempRaster/TOFvsFOR_mask1.tif",
              datatype = "INT2S", overwrite = TRUE)
} else {
  "No LULCt1 map available"
}

if (identical(LULCt2map, NA_character_)) {
  "No LULCt2 map available"
} else if (LULCt2map == "YES") {
  TOFvsFOR_matrix2 <- as.matrix(growth_parameters2[, c("Key*", "TOF")])
  LULCt2_r_m_reclass <- classify(LULCt2_r_m, TOFvsFOR_matrix2)
  writeRaster(LULCt2_r_m_reclass, filename = "LULCC/TempRaster/TOFvsFOR_mask2.tif",
              datatype = "INT2S", overwrite = TRUE)
} else {
  "No LULCt2 map available"
}

if (identical(LULCt3map, NA_character_)) {
  "No LULCt3 map available"
} else if (LULCt3map == "YES") {
  TOFvsFOR_matrix3 <- as.matrix(growth_parameters3[, c("Key*", "TOF")])
  LULCt3_r_m_reclass <- classify(LULCt3_r_m, TOFvsFOR_matrix3)
  writeRaster(LULCt3_r_m_reclass, filename = "LULCC/TempRaster/TOFvsFOR_mask3.tif",
              datatype = "INT2S", overwrite = TRUE)
} else {
  "No LULCt3 map available"
}
  
# AGB maps ---- # ADD TWO MORE MAPS
country_parameters %>%
  dplyr::filter(Var == "AGB1map") %>%
  pull(ParCHR) -> AGB1map
if (identical(country_parameters %>%
              dplyr::filter(Var == "AGB1map") %>%
              pull(ParCHR), NA_character_)) {
  print("No AGB1 map available")
} else if (AGB1map == "YES"){
  if (resolution == 1000) {
    outagb <- rast(paste0("LULCC/SourceData/InRaster/",country_parameters %>%
                    dplyr::filter(Var == "AGB1map_name") %>%
                    pull(ParCHR))) %>%
      terra::crop(ext(userarea_r)) %>%
      terra::resample(userarea_r, "bilinear") %>%
      terra::mask(userarea_r) * ((resolution^2)/(100^2))
    writeRaster(outagb, filename="LULCC/TempRaster//agb_c1.tif", datatype="INT4S", overwrite=TRUE)
  } else if (resolution == 100) { # USES TERRA FOR CTREES MAPS
    outagbnull <- rast(paste0("LULCC/SourceData/InRaster/",country_parameters %>%
                    dplyr::filter(Var == "AGB1map_name") %>%
                    pull(ParCHR)))
    outagb <- ifel(outagbnull < 0, 0, outagbnull) %>%
      terra::crop(ext(rast(userarea_r))) %>%
      terra::resample(rast(userarea_r), "bilinear") %>% 
      app(fun = as.integer)
    writeRaster(outagb, filename="LULCC/TempRaster//agb_c1.tif", datatype="INT4S", overwrite=TRUE)
  }
} else {
  "No AGB1 map available"
}

# Protected Areas ----
if (identical(country_parameters %>%
              dplyr::filter(Var == "npa_raster") %>%
              pull(ParCHR), NA_character_)) { # variable exists with no value
  tic()
  # Read the vector file
  npa <- st_read(paste0("LULCC/SourceData/InVector/", country_parameters %>%
                          filter(Var == "npa_name") %>%
                          pull(ParCHR)))
  # Rasterize using the specified field
  npa_r <- rasterize(npa, userarea_r, field = country_parameters %>%
                       filter(Var == "npa_fieldname") %>%
                       pull(ParCHR))
  # Multiply raster by userarea_r
  npa_c <- npa_r * userarea_r
  # Assign layer name
  names(npa_c) <- "layer_0"
  # Write raster to file
  writeRaster(npa_c, filename = "LULCC/TempRaster/npa_c.tif", 
              datatype = "INT2S", overwrite = TRUE)
  toc()
} else if (country_parameters %>%
           dplyr::filter(Var == "npa_raster") %>%
           pull(ParCHR)  == "YES"){
  print("Processing protected areas from raster format")
  # Load raster using terra
  npa_raster <- rast(paste0("LULCC/SourceData/InRaster/", country_parameters %>%
                              filter(Var == "npa_name_r") %>%
                              pull(ParCHR)))
  
  #Crop, resample, and mask
  npa_c <- terra::mask(
    terra::resample(
      terra::crop(npa_raster, userarea_r), 
      userarea_r, method = "near"  # valid only if you're using terra >= 1.7
    ), 
    userarea_r
  )
  # Assign layer name
  names(npa_c) <- "layer_0"
  # Write raster to file
  writeRaster(npa_c, filename = "LULCC/TempRaster/npa_c.tif", 
              datatype = "INT4S", overwrite = TRUE)
} else if (country_parameters %>%
           dplyr::filter(Var == "npa_raster") %>%
           pull(ParCHR)  == character(0)){ # variable doesn't exist, assume vector format
  tic()
  # Read the vector file
  npa <- st_read(paste0("LULCC/SourceData/InVector/", country_parameters %>%
                          filter(Var == "npa_name") %>%
                          pull(ParCHR)))
  # Rasterize using the specified field
  npa_r <- rasterize(npa, userarea_r, field = country_parameters %>%
                       filter(Var == "npa_fieldname") %>%
                       pull(ParCHR))
  # Multiply raster by userarea_r
  npa_c <- npa_r * userarea_r
  # Assign layer name
  names(npa_c) <- "layer_0"
  # Write raster to file
  writeRaster(npa_c, filename = "LULCC/TempRaster/npa_c.tif", 
              datatype = "INT2S", overwrite = TRUE)
  toc()
} else {
  tic()
  # Read the vector file
  npa <- st_read(paste0("LULCC/SourceData/InVector/", country_parameters %>%
                          filter(Var == "npa_name") %>%
                          pull(ParCHR)))
  # Rasterize using the specified field
  npas_r <- rasterize(npa, userarea_r, field = country_parameters %>%
                        filter(Var == "npa_fieldname") %>%
                        pull(ParCHR))
  # Multiply raster by userarea_r
  npa_c <- npas_r * userarea_r
  # Assign layer name
  names(npa_c) <- "layer_0"
  # Write raster to file
  writeRaster(npa_c, filename = "LULCC/TempRaster/npa_c.tif", 
              datatype = "INT2S", overwrite = TRUE)
  toc()
}

# Rivers ----
# Extract river raster parameter
rivers_raster_param <- country_parameters %>%
  filter(Var == "rivers_raster") %>%
  pull(ParCHR)

if (identical(rivers_raster_param, NA_character_)) {  # Variable exists but has no value
  tic()
  rivers <- st_read(paste0("LULCC/SourceData/InVector/", country_parameters %>%
                             filter(Var == "rivers_name") %>%
                             pull(ParCHR)))
  
  rivers_r <- rasterize(rivers, userarea_r, field = country_parameters %>%
                          filter(Var == "rivers_fieldname") %>%
                          pull(ParCHR))
  
  rivers_c <- rivers_r * userarea_r
  
  writeRaster(rivers_c, filename = "LULCC/TempRaster/rivers_c.tif", 
              datatype = "INT2S", overwrite = TRUE)
  
  toc()
  
} else if (rivers_raster_param == "YES") {  
  print("Processing rivers from raster format")
  
  rivers_raster <- rast(paste0("LULCC/SourceData/InRaster/", country_parameters %>%
                                 filter(Var == "rivers_name_r") %>%
                                 pull(ParCHR)))
  
  rivers_c <- terra::mask(
    terra::resample(
      terra::crop(rivers_raster, userarea_r), 
      userarea_r, method = "near"
    ), 
    userarea_r
  )
  
  writeRaster(rivers_c, filename = "LULCC/TempRaster/rivers_c.tif", 
              datatype = "INT4S", overwrite = TRUE)
  
} else if (length(rivers_raster_param) == 0) {  # Variable doesn't exist, assume vector format
  tic()
  rivers <- st_read(paste0("LULCC/SourceData/InVector/", country_parameters %>%
                             filter(Var == "rivers_name") %>%
                             pull(ParCHR)))
  
  rivers_r <- rasterize(rivers, userarea_r, field = country_parameters %>%
                          filter(Var == "rivers_fieldname") %>%
                          pull(ParCHR))
  
  rivers_c <- rivers_r * userarea_r
  
  writeRaster(rivers_c, filename = "LULCC/TempRaster/rivers_c.tif", 
              datatype = "INT2S", overwrite = TRUE)
  
  toc()
  
} else {  
  tic()
  rivers <- st_read(paste0("LULCC/SourceData/InVector/", country_parameters %>%
                             filter(Var == "rivers_name") %>%
                             pull(ParCHR)))
  
  rivers_r <- rasterize(rivers, userarea_r, field = country_parameters %>%
                          filter(Var == "rivers_fieldname") %>%
                          pull(ParCHR))
  
  rivers_c <- rivers_r * userarea_r
  
  writeRaster(rivers_c, filename = "LULCC/TempRaster/rivers_c.tif", 
              datatype = "INT2S", overwrite = TRUE)
  
  toc()
}


# Lakes ----
# Extract lakes raster parameter
lakes_raster_param <- country_parameters %>%
  filter(Var == "lakes_raster") %>%
  pull(ParCHR)

if (identical(lakes_raster_param, NA_character_)) {  # Variable exists but has no value
  tic()
  lakes <- st_read(paste0("LULCC/SourceData/InVector/", country_parameters %>%
                            filter(Var == "lakes_name") %>%
                            pull(ParCHR)))
  
  lakes_r <- rasterize(lakes, userarea_r, field = country_parameters %>%
                         filter(Var == "lakes_fieldname") %>%
                         pull(ParCHR))
  
  lakes_c <- lakes_r * userarea_r
  
  writeRaster(lakes_c, filename = "LULCC/TempRaster/lakes_c.tif", 
              datatype = "INT2S", overwrite = TRUE)
  
  toc()
  
} else if (lakes_raster_param == "YES") {  
  print("Processing lakes from raster format")
  
  lakes_raster <- rast(paste0("LULCC/SourceData/InRaster/", country_parameters %>%
                                filter(Var == "lakes_name_r") %>%
                                pull(ParCHR)))
  
  lakes_c <- terra::mask(
    terra::resample(
      terra::crop(lakes_raster, userarea_r), 
      userarea_r, method = "near"
    ), 
    userarea_r
  )
  
  writeRaster(lakes_c, filename = "LULCC/TempRaster/lakes_c.tif", 
              datatype = "INT4S", overwrite = TRUE)
  
} else if (length(lakes_raster_param) == 0) {  # Variable doesn't exist, assume vector format
  tic()
  lakes <- st_read(paste0("LULCC/SourceData/InVector/", country_parameters %>%
                            filter(Var == "lakes_name") %>%
                            pull(ParCHR)))
  
  lakes_r <- rasterize(lakes, userarea_r, field = country_parameters %>%
                         filter(Var == "lakes_fieldname") %>%
                         pull(ParCHR))
  
  lakes_c <- lakes_r * userarea_r
  
  writeRaster(lakes_c, filename = "LULCC/TempRaster/lakes_c.tif", 
              datatype = "INT2S", overwrite = TRUE)
  
  toc()
  
} else {  
  tic()
  lakes <- st_read(paste0("LULCC/SourceData/InVector/", country_parameters %>%
                            filter(Var == "lakes_name") %>%
                            pull(ParCHR)))
  
  lakes_r <- rasterize(lakes, userarea_r, field = country_parameters %>%
                         filter(Var == "lakes_fieldname") %>%
                         pull(ParCHR))
  
  lakes_c <- lakes_r * userarea_r
  
  writeRaster(lakes_c, filename = "LULCC/TempRaster/lakes_c.tif", 
              datatype = "INT2S", overwrite = TRUE)
  
  toc()
}


# Roads ----
# Extract roads raster parameter
roads_raster_param <- country_parameters %>%
  filter(Var == "roads_raster") %>%
  pull(ParCHR)

if (identical(roads_raster_param, NA_character_)) {  # Variable exists but has no value
  tic()
  roads <- st_read(paste0("LULCC/SourceData/InVector/", country_parameters %>%
                            filter(Var == "roads_name") %>%
                            pull(ParCHR)))
  
  roads_r <- rasterize(roads, userarea_r, field = country_parameters %>%
                         filter(Var == "roads_fieldname") %>%
                         pull(ParCHR))
  
  roads_c <- roads_r * userarea_r
  
  writeRaster(roads_c, filename = "LULCC/TempRaster/roads_c.tif", 
              datatype = "INT2S", overwrite = TRUE)
  
  toc()
  
} else if (roads_raster_param == "YES") {  
  print("Processing roads from raster format")
  
  roads_raster <- rast(paste0("LULCC/SourceData/InRaster/", country_parameters %>%
                                filter(Var == "roads_name_r") %>%
                                pull(ParCHR)))
  
  roads_c <- terra::mask(
    terra::resample(
      terra::crop(roads_raster, userarea_r), 
      userarea_r, method = "near"
    ), 
    userarea_r
  )
  
  writeRaster(roads_c, filename = "LULCC/TempRaster/roads_c.tif", 
              datatype = "INT4S", overwrite = TRUE)
  
} else if (length(roads_raster_param) == 0) {  # Variable doesn't exist, assume vector format
  tic()
  roads <- st_read(paste0("LULCC/SourceData/InVector/", country_parameters %>%
                            filter(Var == "roads_name") %>%
                            pull(ParCHR)))
  
  roads_r <- rasterize(roads, userarea_r, field = country_parameters %>%
                         filter(Var == "roads_fieldname") %>%
                         pull(ParCHR))
  
  roads_c <- roads_r * userarea_r
  
  writeRaster(roads_c, filename = "LULCC/TempRaster/roads_c.tif", 
              datatype = "INT2S", overwrite = TRUE)
  
  toc()
  
} else {  
  tic()
  roads <- st_read(paste0("LULCC/SourceData/InVector/", country_parameters %>%
                            filter(Var == "roads_name") %>%
                            pull(ParCHR)))
  
  roads_r <- rasterize(roads, userarea_r, field = country_parameters %>%
                         filter(Var == "roads_fieldname") %>%
                         pull(ParCHR))
  
  roads_c <- roads_r * userarea_r
  
  writeRaster(roads_c, filename = "LULCC/TempRaster/roads_c.tif", 
              datatype = "INT2S", overwrite = TRUE)
  
  toc()
}

# Borders ----
# Extract borders raster parameter
borders_raster_param <- country_parameters %>%
  filter(Var == "borders_raster") %>%
  pull(ParCHR)

if (identical(borders_raster_param, NA_character_)) {  # Variable exists but has no value
  tic()
  borders <- st_read(paste0("LULCC/SourceData/InVector/", country_parameters %>%
                              filter(Var == "borders_name") %>%
                              pull(ParCHR)))
  
  borders_r <- rasterize(borders, userarea_r, field = country_parameters %>%
                           filter(Var == "borders_fieldname") %>%
                           pull(ParCHR))
  
  borders_c <- borders_r * userarea_r
  
  writeRaster(borders_c, filename = "LULCC/TempRaster/borders_c.tif", 
              datatype = "INT2S", overwrite = TRUE)
  
  toc()
  
} else if (borders_raster_param == "YES") {  
  print("Processing borders from raster format")
  
  borders_raster <- rast(paste0("LULCC/SourceData/InRaster/", country_parameters %>%
                                  filter(Var == "borders_name_r") %>%
                                  pull(ParCHR)))
  
  borders_c <- terra::mask(
    terra::resample(
      terra::crop(borders_raster, userarea_r), 
      userarea_r, method = "near"
    ), 
    userarea_r
  )
  
  writeRaster(borders_c, filename = "LULCC/TempRaster/borders_c.tif", 
              datatype = "INT4S", overwrite = TRUE)
  
} else if (length(borders_raster_param) == 0) {  # Variable doesn't exist, assume vector format
  tic()
  borders <- st_read(paste0("LULCC/SourceData/InVector/", country_parameters %>%
                              filter(Var == "borders_name") %>%
                              pull(ParCHR)))
  
  borders_r <- rasterize(borders, userarea_r, field = country_parameters %>%
                           filter(Var == "borders_fieldname") %>%
                           pull(ParCHR))
  
  borders_c <- borders_r * userarea_r
  
  writeRaster(borders_c, filename = "LULCC/TempRaster/borders_c.tif", 
              datatype = "INT2S", overwrite = TRUE)
  
  toc()
  
} else {  
  tic()
  borders <- st_read(paste0("LULCC/SourceData/InVector/", country_parameters %>%
                              filter(Var == "borders_name") %>%
                              pull(ParCHR)))
  
  borders_r <- rasterize(borders, userarea_r, field = country_parameters %>%
                           filter(Var == "borders_fieldname") %>%
                           pull(ParCHR))
  
  borders_c <- borders_r * userarea_r
  
  writeRaster(borders_c, filename = "LULCC/TempRaster/borders_c.tif", 
              datatype = "INT2S", overwrite = TRUE)
  
  toc()
}

#################################TERRA#####################################
##### VOY POR ACÁ!! Pasar a a Terra
# Maritime routes ----
if (identical(country_parameters %>%
              dplyr::filter(Var == "Maritime_lyr") %>%
              pull(ParCHR), NA_character_)) { # variable exists with no value
  print ("No Maritime layer") 
} else if (identical(country_parameters %>%
                     dplyr::filter(Var == "maritime_lyr") %>%
                     pull(ParCHR), "YES")) {
  
  maritime<-st_read(paste0("LULCC/SourceData/InVector/", country_parameters %>%
                             dplyr::filter(Var == "maritime_name") %>%
                             pull(ParCHR)))
  maritime_r <- rasterize(maritime, userarea_r, country_parameters %>%
                            dplyr::filter(Var == "maritime_name_ID") %>%
                            pull(ParCHR))
  maritime_c<-maritime_r*userarea_r
  writeRaster(maritime_c, filename="LULCC/TempRaster/maritime_c.tif", datatype="INT2S", overwrite=TRUE)
  
} else {
  "Do nothing"
}

# Attraction routes ---- 
if (identical(country_parameters %>%
              dplyr::filter(Var == "attraction_lyr") %>%
              pull(ParCHR), NA_character_)) { # variable exists with no value
  print ("No Attraction layer") 
} else if (identical(country_parameters %>%
                     dplyr::filter(Var == "attraction_lyr") %>%
                     pull(ParCHR), "YES")) {
  attraction <- st_read(paste0("LULCC/SourceData/InVector/", country_parameters %>%
                               dplyr::filter(Var == "attraction_name") %>%
                               pull(ParCHR))) %>%
    st_make_valid()
  attractionshpcrop<-st_intersection(attraction, userarea)
  # width <- c(0,10000,20000,50000,100000) # Adjusted for East Africa 10 to 1000 km
  widthnames <- c("b0","b1","b2","b3","b4")
  for (wn in widthnames){
    if (wn == "b0") { w = w0 
    } else if (wn == "b1") { w = w1
    } else if (wn == "b2") { w = w2
    } else if (wn == "b3") { w = w3
    }  else if (wn == "b4") { w = w4
    } else { print("error") }
    
    if (nrow(attractionshpcrop)==0) {
      attraction_c1<-rasterize(st_zm(attraction), userarea_r, country_parameters %>%
                                 dplyr::filter(Var == "attraction_name_ID") %>%
                                 pull(ParCHR))
    } else {
      attractionshpcrop_b<-st_buffer(attractionshpcrop, dist=w, nQuadSegs=100)
      attraction_c1<-rasterize(attractionshpcrop_b, userarea_r)
    }
    attraction_c<-attraction_c1*userarea_r
    writeRaster(attraction_c, filename=paste0("LULCC/TempRaster/attraction_c",wn,".tif"), datatype="INT2S", overwrite=TRUE)
  }
} else {
  "Do nothing"
}

# Demand modeling ---- 
# Using Terra
tic("Using terra")

# Read the raster files using terra
locs_name_r_w <- terra::rast("In/DemandScenarios/locs_raster_w.tif")

# Crop, resample, and mask using terra functions
locs_name_r_w <- locs_name_r_w %>%
  terra::crop(userarea_r) %>%
  terra::resample(userarea_r, method = "near") %>%
  terra::mask(userarea_r)

# Write the processed raster to a file
writeRaster(locs_name_r_w, filename = "LULCC/TempRaster/locs_c_w.tif", datatype = "INT4S", overwrite = TRUE)

# Copy the file to the "In" folder
file.copy(from = "LULCC/TempRaster/locs_c_w.tif",
          to = "In",
          overwrite = TRUE)

# Repeat the process for the second raster
locs_name_r_v <- rast("In/DemandScenarios/locs_raster_v.tif")

locs_name_r_v <- locs_name_r_v %>%
  terra::crop(userarea_r) %>%
  terra::resample(userarea_r, method = "near") %>%
  terra::mask(userarea_r)

writeRaster(locs_name_r_v, filename = "LULCC/TempRaster/locs_c_v.tif", datatype = "INT4S", overwrite = TRUE)

file.copy(from = "LULCC/TempRaster/locs_c_v.tif",
          to = "In",
          overwrite = TRUE)
toc()

# Land Use Land Cover Module ####
if (os == "Windows") {  
  
  if (LULCt1map == "YES"){
    dir.create("LULCC/lucdynamics_luc1")
    dir.create("LULCC/lucdynamics_luc1/out_lulcc")
    lulcc.egoml <- list.files(
      paste0(gitlabdir, "/localhost/scripts/LULCC/LULCt1_c"), 
      pattern = "(win241\\.egoml|\\.bat)$", 
      full.names = TRUE
    )
    file.copy(from=lulcc.egoml, 
              to=paste0(countrydir, "/LULCC/lucdynamics_luc1"), 
              overwrite = TRUE)
    system(paste0(countrydir, "/LULCC/lucdynamics_luc1/LULCC_blackbox_scripts2.bat"))
  }

  if (LULCt2map == "YES"){
    dir.create("LULCC/lucdynamics_luc2")
    dir.create("LULCC/lucdynamics_luc2/out_lulcc")
    lulcc.egoml <- list.files(
      paste0(gitlabdir, "/localhost/scripts/LULCC/LULCt2_c"), 
      pattern = "(win241\\.egoml|\\.bat)$", 
      full.names = TRUE
    )
    file.copy(from=lulcc.egoml, 
              to=paste0(countrydir, "/LULCC/lucdynamics_luc2"), 
              overwrite = TRUE)
    system(paste0(countrydir, "/LULCC/lucdynamics_luc2/LULCC_blackbox_scripts2.bat"))
  }
  
  if (LULCt3map == "YES"){
    dir.create("LULCC/lucdynamics_luc3")
    dir.create("LULCC/lucdynamics_luc3/out_lulcc")
    lulcc.egoml <- list.files(
      paste0(gitlabdir, "/localhost/scripts/LULCC/LULCt3_c"), 
      pattern = "(win241\\.egoml|\\.bat)$", 
      full.names = TRUE
    )
    file.copy(from=lulcc.egoml, 
              to=paste0(countrydir, "/LULCC/lucdynamics_luc3"), 
              overwrite = TRUE)
    system(paste0(countrydir, "/LULCC/lucdynamics_luc3/LULCC_blackbox_scripts2.bat"))
  }

} else if (os == "Linux") {  
  
  print("This is linux dinamica luc module")
  if (LULCt1map == "YES"){
    dir.create("LULCC/lucdynamics_luc1")
    dir.create("LULCC/lucdynamics_luc1/out_lulcc")
    lulcc.egoml <- list.files(
      paste0(gitlabdir, "/localhost/scripts/LULCC/LULCt1_c"), 
      pattern = "linux\\.egoml$", 
      full.names = TRUE
    )
    file.copy(from=lulcc.egoml,
              to=paste0(countrydir, "/LULCC/lucdynamics_luc1"),
              overwrite = TRUE)
    lulcc.egoml.local <- list.files(
      paste0(countrydir, "/LULCC/lucdynamics_luc1"), 
      pattern = "linux\\.egoml$",  
      full.names = TRUE
    )
    
    lulcc.egoml.local %>%
      walk(function(i) {
        system(glue('/opt/dinamicaego/DinamicaEGO-8.3.0-Ubuntu.AppImage "{i}"'))
      })
    
  }
  
  if (LULCt2map == "YES"){
    dir.create("LULCC/lucdynamics_luc2")
    dir.create("LULCC/lucdynamics_luc2/out_lulcc")
    lulcc.egoml <- list.files(
      paste0(gitlabdir, "/localhost/scripts/LULCC/LULCt2_c"), 
      pattern = "linux\\.egoml$", 
      full.names = TRUE
    )
    file.copy(from=lulcc.egoml,
              to=paste0(countrydir, "/LULCC/lucdynamics_luc2"),
              overwrite = TRUE)
    lulcc.egoml.local <- list.files(
      paste0(countrydir, "/LULCC/lucdynamics_luc2"), 
      pattern = "linux\\.egoml$",  
      full.names = TRUE
    )

    lulcc.egoml.local %>%
      walk(function(i) {
        system(glue('/opt/dinamicaego/DinamicaEGO-8.3.0-Ubuntu.AppImage "{i}"'))
      })
    
  }
  
  if (LULCt3map == "YES"){
    dir.create("LULCC/lucdynamics_luc3")
    dir.create("LULCC/lucdynamics_luc3/out_lulcc")
    lulcc.egoml <- list.files(
      paste0(gitlabdir, "/localhost/scripts/LULCC/LULCt3_c"), 
      pattern = "linux\\.egoml$", 
      full.names = TRUE
    )
    file.copy(from=lulcc.egoml,
              to=paste0(countrydir, "/LULCC/lucdynamics_luc3"),
              overwrite = TRUE)
    lulcc.egoml.local <- list.files(
      paste0(countrydir, "/LULCC/lucdynamics_luc3"), 
      pattern = "linux\\.egoml$",  
      full.names = TRUE
    )
    
    lulcc.egoml.local %>%
      walk(function(i) {
        system(glue('/opt/dinamicaego/DinamicaEGO-8.3.0-Ubuntu.AppImage "{i}"'))
      })
  }
  
}

# End of script ----


