### ESTO YA CORRIO NO NECESIO SUPUESTOS PRE MOFUSS COMO EN EL DE DEMANDA, PORQUE YA CORRIO!!###


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
# Date: Aug 2025
# Description: Each run will produce all BaU and ICS for a given time period and region defined interactively.

# 2dolist ----

# # Internal parameters ----

###
# NEW PARAMETERS 4 DELTA AGB
demanddir <- "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/demand"
admindir <- "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/admin_regions"
emissionsdir <- "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/emissions"
rTempdir <- "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/rTemp"
bau_dir <- "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/zmb_lusaka_bau_1km_subc"
ics_dir <- "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/zmb_lusaka_ics2_1km_subc"
 
temdirdefined = 1
# string_pattern_yes <- "zmb_lusaka_" #Use adm0 as default. String pattern to be searched when selecting folders for the rasters' geocomputation
# string_pattern_no <- "idw" #Use "idw" as default. String pattern to be searched when selecting folders for the rasters' geocomputation

#***#
startfromscratch = 1 # WARNING: Will erase all temporal folders along with any temp datasets - never too bad
eraseallemissions = 1 # WARNING: Will erase all EMISSIONS OUTPUTS FOLDERS - could be bad
#***#

efchratio <- 6 # wood to charcoal yield

mergecountries = 1 
avoidedemissions = 1
zonalstats = 1

optimize = 0 # geoprocessing optimization

# Define all folders based on node ----
# Detect OS and node name
os <- Sys.info()["sysname"]
node_name <- Sys.info()[["nodename"]]
cat(os,node_name)


# Erase all plots in R Studio
Sys.sleep(2)
for (p in 1:100) {
  if (length(dev.list()!=0)) {
    dev.off()
  }
}
Sys.sleep(3)

# Load packages ----
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
# library(compare)
library(dplyr)
library(fasterize)
library(fs)
library(gdata)
library(hacksaw)
library(mapview)
library(raster)
library(readxl)
library(sf)
library(stringr)
library(svDialogs)
library(tcltk)
library(tibble)
library(tictoc)
library(tidyterra)
library(tidyverse)



setwd(demanddir)

if (startfromscratch == 1){
  
  if (eraseallemissions == 1){
    setwd(emissionsdir)
    unlink("*/", recursive=TRUE) # WARNING as this erases all final emissions results: added eraseallemissions in parameters
    setwd(demanddir)
  }
  
  unlink("pop_maps_byregionE*/", recursive=TRUE)
  unlink("pop_tempE*/", recursive=TRUE)
  unlink("pop_outE*/", recursive=TRUE)
  unlink("demand_tempE*/", recursive=TRUE)
  unlink("demand_temp_summaryE*/", recursive=TRUE)
  unlink("demand_outE*/", recursive=TRUE)
  unlink("demand_out_summaryE*/", recursive=TRUE)
  unlink("to_idwE*/", recursive=TRUE)
  unlink("emissions_temp*/", recursive=TRUE)
  unlink("emissions_out*/", recursive=TRUE)
  unlink("emissions_out_summaryE*/", recursive=TRUE)
  
}

# BaU or ICS ----
scenario.list <- c("BaU", "ICS")
for (scex in scenario.list) { # Start scex loop ----
  # scex = "BaU"
  # scex = "ICS"
  
  unlink(paste0("pop_maps_byregionE_",scex,"/"), recursive=TRUE)
  unlink(paste0("pop_tempE_",scex,"/"), recursive=TRUE)
  unlink(paste0("pop_outE_",scex,"/"), recursive=TRUE)
  unlink(paste0("demand_tempE_",scex,"/"), recursive=TRUE)
  unlink(paste0("demand_temp_summaryE_",scex,"/"), recursive=TRUE)
  unlink(paste0("demand_outE_",scex,"/"), recursive=TRUE)
  unlink(paste0("demand_out_summaryE_",scex,"/"), recursive=TRUE)
  unlink(paste0("to_idwE_",scex,"/"), recursive=TRUE)
  unlink(paste0("emissions_temp_",scex,"/"), recursive=TRUE)
  unlink(paste0("emissions_out_",scex,"/"), recursive=TRUE)
  unlink(paste0("emissions_out_summaryE_",scex,"/"), recursive=TRUE)
  
  if (!dir.exists(paste0("pop_maps_byregionE_",scex))) {dir.create(paste0("pop_maps_byregionE_",scex))}
  if (!dir.exists(paste0("pop_tempE_",scex))) {dir.create(paste0("pop_tempE_",scex))} 
  # if (!dir.exists(paste0("pop_outE_",scex))) {dir.create(paste0("pop_outE_",scex))} 
  if (!dir.exists(paste0("demand_tempE_",scex))) {dir.create(paste0("demand_tempE_",scex))}
  if (!dir.exists(paste0("demand_temp_summaryE_",scex))) {dir.create(paste0("demand_temp_summaryE_",scex))}
  # if (!dir.exists(paste0("demand_outE_",scex))) {dir.create(paste0("demand_outE_",scex))}
  # if (!dir.exists(paste0("to_idwE_",scex))) {dir.create(paste0("to_idwE_",scex))} 
  if (!dir.exists(paste0("emissions_temp_",scex))) {dir.create(paste0("emissions_temp_",scex))}
  if (!dir.exists(paste0("emissions_out_",scex))) {dir.create(paste0("emissions_out_",scex))}
  
  # if (scex == "BaU") {
  #   wfdb <- read_csv("demand_in/cons_fuels_years.csv") # UPDATE WITH NEW DATASET WITH THREE OPTIONS
  #   head(wfdb)
  #   # terra::unique(wfdb$fuel)
  #   demand_col <- "fuel_tons3" #"fuel_tons1" #"fuel_tons3"
  # } else if (scex == "ICS") {
  #   wfdb <- read_csv("demand_in/cons_fuels_years_proj.csv") # UPDATE WITH NEW DATASET WITH THREE OPTIONS
  #   head(wfdb)
  #   # terra::unique(wfdb$fuel)
  #   demand_col <- "fuel_tons3" #Its one to simulate reduction from original one "fuel_tons1" #"fuel_tons3"
  # }
  print(scex) # save as text to recover later down the river

}
  
      #######################################
      #########################################
      ############################################
      ###############################################
      ########################################
      #######################################
      #########################################
      ############################################
      ###############################################
      ########################################
# Read emission factors database
efdb <- read_csv("demand_in/efdb_all.csv")
head(efdb)
      
# fuelschr <- c("Kerosene",       
#               "Gas",            
#               "Electricity",    
#               "Biomass",        
#               "Charcoal",       
#               "Coal")

fuelschr <- c("Biomass",        
              "Charcoal")
f = "Biomass"
i = bau_dir

### Calculate emissions FIRST MONTE CARLO RUN FOR ONE COUNTRY ONLY FOR 2020-2050----

# Read parameters table ----
if (webmofuss == 1) {
  # Read parameters table in webmofuss
  # country_parameters <- read_csv(paste0(i,"/LULCC/DownloadedDatasets/SourceDataGlobal/parameters.csv"))
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
  delimiter <- detect_delimiter(paste0(i,"/LULCC/DownloadedDatasets/SourceDataGlobal/parameters.csv"))
  # Read the CSV file with the detected delimiter
  country_parameters <- read_delim(paste0(i,"/LULCC/DownloadedDatasets/SourceDataGlobal/parameters.csv"), delim = delimiter)
  print(tibble::as_tibble(country_parameters), n=100)
}

country_parameters %>%
  dplyr::filter(Var == "region2BprocessedCtry_iso") %>%
  pull(ParCHR) -> region2BprocessedCtry_iso

efvalueCO2 <- efdb %>%
  dplyr::filter(GID_0 == region2BprocessedCtry_iso) %>%
  dplyr::filter(fueltype == f) %>%
  dplyr::select(CO2)

efvalueCH4 <- efdb %>%
  dplyr::filter(GID_0 == region2BprocessedCtry_iso) %>%
  dplyr::filter(fueltype == f) %>%
  dplyr::select(CH4)

efvalueN2O <- efdb %>%
  dplyr::filter(GID_0 == region2BprocessedCtry_iso) %>%
  dplyr::filter(fueltype == f) %>%
  dplyr::select(N2O)

# If no emission factor is found for the current 'i' and 'f', skip to the next fuel type
if (nrow(efvalueCO2) == 0) {
  message(paste("No emission factor found for fuel", f, "and GID_0", i))
  next  # Skip to the next 'f'
}



      # Calculate emissions w/uncertainty
      # Calculate FIRST for MC=1 and period 2020-2050
      
      
        if (f %in% c("Biomass")) {
          fuelDem_allE_mean <- fuelDem_all * ((efvalueCO2$CO2 * (fnrb_2020_2050_mean / 100)) + efvalueCH4$CH4 + efvalueN2O$N2O)
          
          agb_bau_tn <- terra::rast(paste0(bau_dir,"/debugging_1/Growth_less_harv41.tif"))
          agb_ics_tn <- terra::rast(paste0(ics_dir,"/debugging_1/Growth_less_harv41.tif"))
          
          # SIEMRPE ICS DEBE TENER MAS BIOMASA!  
          delta_agb <- (agb_ics_tn - agb_bau_tn) %>%
            global("sum", na.rm = TRUE) #QUITARLE EL GLOBAL PARA NO PERDER LO ESPACIAL!
          delta_agb
          deltaco2 <- delta_agb * 0.47 * (44/12)
          deltaco2
          # AHORA SI EL HARVEST TOATL por los factors de emisison ponderados por la intervecion
          efvalueCO2
          efvalueCH4
          efvalueN2O   
          Harvest_tot01
          Harvest_tot41 #que porcentaje de esto es acron y que procentaje leña? sera hota de separar??
          
          # CUANDO ES CHARCOAL....o viene tdo junto??? AHHHH
          
          E_meanfromharvest_agb
          E_meanfromharvest_CO2 <- # difference includes regrowth...)
          E_meanfromPIC
          ###
          # DeltaAGB from BaU and ICS into CO2eq + fuelDem_all(harvest from mofuss) * (efvalueCH4$CH4 + efvalueN2O$N2O) # HERE----
          ###
          
          fuelDem_allE_se <- fuelDem_all * ((efvalueCO2$CO2 * (fnrb_2020_2050_se / 100)) + efvalueCH4$CH4 + efvalueN2O$N2O) 
        } else if (f %in% c("Charcoal")) {
          fuelDem_allE_mean <- fuelDem_all * (((efvalueCO2$CO2 * (fnrb_2020_2050_mean / 100)) + efvalueCH4$CH4 + efvalueN2O$N2O) / efchratio ) # This values that divides the sum of ef is to convert from emission per unit of charcoal
          fuelDem_allE_se <- fuelDem_all * (((efvalueCO2$CO2 * (fnrb_2020_2050_mean / 100)) + efvalueCH4$CH4 + efvalueN2O$N2O) / efchratio ) # This values that divides the sum of ef is to convert from emission per unit of charcoal
          # to emissions per unit of wood that is converted to charcoal
        } else {
          fuelDem_allE_mean <- fuelDem_all * (efvalueCO2$CO2 + efvalueCH4$CH4 + efvalueN2O$N2O)
          fuelDem_allE_se <- fuelDem_all * 0
        }

      # Save emissions raster
      terra::writeRaster(fuelDem_allE_mean,
                         paste0("emissions_temp_", scex, "/",pop_ver,"_", i, "_", f, "_emissionsSum_mean.tif"),
                         filetype = "GTiff", overwrite = TRUE)

      terra::writeRaster(fuelDem_allE_se,
                         paste0("emissions_temp_", scex, "/",pop_ver,"_", i, "_", f, "_emissionsSum_se.tif"),
                         filetype = "GTiff", overwrite = TRUE)
    }
    
 
  
#######################################
#########################################
############################################
###############################################
########################################
#######################################
#########################################
############################################
###############################################
########################################
  
  
  ## load country rasters and merge into original region ----
  
  if (mergecountries == 1) { 
    setwd(demanddir)
    
    for (f in fuelschr) {
      # f = "Charcoal"

      emissions_list_mean <- list.files(path = paste0("emissions_temp_",scex,"/"),
                                   pattern = paste0("_",f,".*\\_mean.tif$"), full.names = TRUE)
      emissions_list_se <- list.files(path = paste0("emissions_temp_",scex,"/"),
                                        pattern = paste0("_",f,".*\\_se.tif$"), full.names = TRUE)

      if (length(emissions_list_mean) > 1) { 
        emissions_raster_list_mean <- lapply(emissions_list_mean, rast)
        emissions_users_mean <- do.call("merge", emissions_raster_list_mean)
        terra::writeRaster(emissions_users_mean,
                           paste0("emissions_out_",scex,"/",pop_ver,"_",firstyr,"-",
                                  lastyr,"_",f,"_",scex,"_emissions_mean.tif"),
                           filetype = "GTiff", overwrite = T)
        
        emissions_raster_list_se <- lapply(emissions_list_se, rast)
        emissions_users_se <- do.call("merge", emissions_raster_list_se)
        terra::writeRaster(emissions_users_se,
                           paste0("emissions_out_",scex,"/",pop_ver,"_",firstyr,"-",
                                  lastyr,"_",f,"_",scex,"_emissions_se.tif"),
                           filetype = "GTiff", overwrite = T)
        
      } else {
        
        rast(emissions_list_mean) %>%
          terra::writeRaster(paste0("emissions_out_",scex,"/",pop_ver,"_",firstyr,"-",
                                    lastyr,"_",f,"_",scex,"_emissions_mean.tif"),
                             filetype = "GTiff", overwrite = T)
        rast(emissions_list_se) %>%
          terra::writeRaster(paste0("emissions_out_",scex,"/",pop_ver,"_",firstyr,"-",
                                    lastyr,"_",f,"_",scex,"_emissions_se.tif"),
                             filetype = "GTiff", overwrite = T)
        
      }
      
    }
    
  }
  
  ## final emissions maps ----
  # Sum all fuels and transform to various units
  # Project for TNC
  
  setwd(demanddir)
  
  for (sdm in c("_mean", "_se")){
    # sdm = "_mean"
    # sdm = "_se"
    
    finalemissions_list <- list.files(path = paste0("emissions_out_",scex,"/"),
                                           pattern = paste0(pop_ver,"_.*\\",scex,"_emissions.*\\",sdm,".tif$"), full.names = TRUE)
    finalemissions_raster_list <- lapply(finalemissions_list, rast)
    finalemissions_all <- Reduce("+",finalemissions_raster_list)
    terra::writeRaster(finalemissions_all,
                       paste0("emissions_out_",scex,"/e",firstyr,"-",
                              lastyr,"_",scex,"_tCO2e_gcs",sdm,".tif"),
                       filetype = "GTiff", overwrite = T)
    
    eCO2e_pNoAdj <- finalemissions_all %>%
      terra::project("epsg:3395", method = "bilinear", gdal = FALSE, res=1000) #, threads=TRUE) # Check PROJECTION with TNC
    # Perhaps is better not to project and adjust by area in WSG84 pixels?
    terra::writeRaster(eCO2e_pNoAdj, paste0("emissions_out_",scex,"/e",firstyr,"-",
                                                 lastyr,"_",scex,"_tCO2e_noadj",sdm,".tif"),
                       filetype = "GTiff", overwrite = T)
    
    # Correction due to projection - any year.
    eCO2e_preProj <- finalemissions_all %>%
      raster() %>%
      cellStats(stat='sum', na.rm=TRUE)
    eCO2e_pstProj <- eCO2e_pNoAdj %>%
      raster() %>%
      cellStats(stat='sum', na.rm=TRUE)
    proj_factor_eCO2e <- eCO2e_preProj/eCO2e_pstProj
    # proj_factor_eCO2e = 1
    eCO2e_p <- eCO2e_pNoAdj*proj_factor_eCO2e
    terra::writeRaster(eCO2e_p, paste0("emissions_out_",scex,"/e",firstyr,"-",
                                       lastyr,"_",scex,"_tCO2e",sdm,".tif"),
                       filetype = "GTiff", overwrite = T)
    
    perha <- (res(eCO2e_p)[1]^2)/(100^2)
    peryr <- length(annos) 
    eCO2e_p_unitx <- eCO2e_p / perha
    eCO2e_p_unit <- eCO2e_p_unitx / peryr
    terra::writeRaster(eCO2e_p_unit, paste0("emissions_out_",scex,"/e",firstyr,"-",
                                                 lastyr,"_",scex,"_tCO2e_thay",sdm,".tif"),
                       filetype = "GTiff", overwrite = T)
    
  }
  ## copy emissions factors to TNC temporal results ----
  if (byregion == "Global") {
    # Extract the part after the last "_"
    after_last_underscore <- sub(".*_", "", mofuss_region)
    # Concatenate with a hyphen
    regiontag <- paste0(after_last_underscore)
  } else if (byregion == "Continental") {
    # Extract the part before the first "_"
    before_first_underscore <- sub("_.*", "", mofuss_region)
    # Extract the part after the last "_"
    after_last_underscore <- sub(".*_", "", mofuss_region)
    # Concatenate with a hyphen
    regiontag <- paste0(after_last_underscore,"_",substr(byregion, 1, 3))
  } else if (byregion == "Regional") {
    # Extract the part before the first "_"
    before_first_underscore <- sub("_.*", "", mofuss_region)
    # Extract the part after the last "_"
    after_last_underscore <- sub(".*_", "", mofuss_region)
    # Concatenate with a hyphen
    regiontag <- paste0(before_first_underscore, "-", after_last_underscore,"_",substr(byregion, 1, 3))
  } else if (byregion == "Country") {
    # Extract the part before the first "_"
    before_first_underscore <- sub("_.*", "", mofuss_region)
    # Extract the part after the last "_"
    after_last_underscore <- sub(".*_", "", mofuss_region)
    # Concatenate with a hyphen
    regiontag <- paste0(before_first_underscore, "-", after_last_underscore,"_",substr(byregion, 1, 3))
  }
  print(regiontag)
  
  if (annos[length(annos)] == 2050 & scex == "BaU"){
    print("Copy files to 2050 - BaU")
    unlink(paste0(emissionsdir,"/2050",regiontag,"/BaU"), recursive = TRUE)
    if (!dir.exists(paste0(emissionsdir,"/2050",regiontag,"/BaU"))) {dir.create(paste0(emissionsdir,"/2050",regiontag,"/BaU"), recursive = TRUE)}
    file.copy(from = paste0(demanddir,"/emissions_out_",scex),
              to = paste0(emissionsdir,"/2050",regiontag,"/BaU"),
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
    
  } else if (annos[length(annos)] == 2050 & scex == "ICS"){
    print("Copy files to 2050 - ICS")
    unlink(paste0(emissionsdir,"/2050",regiontag,"/ICS"), recursive = TRUE)
    if (!dir.exists(paste0(emissionsdir,"/2050",regiontag,"/ICS"))) {dir.create(paste0(emissionsdir,"/2050",regiontag,"/ICS"), recursive = TRUE)}
    file.copy(from = paste0(demanddir,"/emissions_out_",scex),
              to = paste0(emissionsdir,"/2050",regiontag,"/ICS"),
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
    
  } else if (annos[length(annos)] == 2035 & scex == "BaU"){
    print("Copy files to 2035 - BaU")
    unlink(paste0(emissionsdir,"/2035/BaU"), recursive = TRUE)
    if (!dir.exists(paste0(emissionsdir,"/2035",regiontag,"/BaU"))) {dir.create(paste0(emissionsdir,"/2035",regiontag,"/BaU"), recursive = TRUE)}
    file.copy(from = paste0(demanddir,"/emissions_out_",scex),
              to = paste0(emissionsdir,"/2035",regiontag,"/BaU"),
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
    
  }  else if (annos[length(annos)] == 2035 & scex == "ICS"){
    print("Copy files to 2035 - ICS")
    unlink(paste0(emissionsdir,"/2035",regiontag,"/ICS"), recursive = TRUE)
    if (!dir.exists(paste0(emissionsdir,"/2035",regiontag,"/ICS"))) {dir.create(paste0(emissionsdir,"/2035",regiontag,"/ICS"), recursive = TRUE)}
    file.copy(from = paste0(demanddir,"/emissions_out_",scex),
              to = paste0(emissionsdir,"/2035",regiontag,"/ICS"),
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
    
  } else {
    print("error: nothing will be saved")
  } 
  
  
  paste0(annos[length(annos)],"-",scex )
  
} # End scex loop ----
# At the end of the scex loop, both scenarios BaU and ICS are ready, for one or many time periods
# 2020-2035 and 2020-2050 for TNC 

# Avoided emissions Bau vs Sce ----

# # This chunk is repeated from above. Uncomment when running this code partially
# if (annos[1] == yr+1) {
#   firstyr <- yr
# } else if (annos[1]-1 > yr) {
#   firstyr <- annos[1]
# } else if (annos[1] < yr) {
#   firstyr <- annos[1]
# } else {
#   print("Error?")
# }
# firstyr
# lastyr <- annos[length(annos)]
# lastyr
# firstyr > yr

simlength <- (lastyr - firstyr)+1
# simlength = 2035 # Test for 2035
print(simlength)

if (avoidedemissions == 1){
  
  for (sdm in c("_mean", "_se")){
    # sdm = "_mean"
    # sdm = "_se"
     
    if (file.exists(paste0(emissionsdir,"/",lastyr,regiontag,"/BaU/emissions_out_BaU/e",firstyr,"-",lastyr,"_BaU_tCO2e_gcs",sdm,".tif")) == TRUE &
        file.exists(paste0(emissionsdir,"/",lastyr,regiontag,"/ICS/emissions_out_ICS/e",firstyr,"-",lastyr,"_ICS_tCO2e_gcs",sdm,".tif")) == TRUE) {
      
      BaU20xxa <- rast(paste0(emissionsdir,"/",lastyr,regiontag,"/BaU/emissions_out_BaU/e",firstyr,"-",lastyr,"_BaU_tCO2e_gcs",sdm,".tif"))
      ICS20xxa <- rast(paste0(emissionsdir,"/",lastyr,regiontag,"/ICS/emissions_out_ICS/e",firstyr,"-",lastyr,"_ICS_tCO2e_gcs",sdm,".tif"))
      if (sdm == "_mean"){
        AvEm20xx_gcs_tpp <- BaU20xxa - ICS20xxa # tpp stands for tonnes per pixel per period
      } else if (sdm == "_se") {
        AvEm20xx_gcs_tpp <- sqrt((BaU20xxa - ICS20xxa)^2)/sqrt(30) # WARNING !!! PROPAGATE THE ERROR CORRECTLY PLEASE - PLACEHOLDER FOR TNC
      }
      terra::writeRaster(AvEm20xx_gcs_tpp, paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_gcs_tpp",sdm,".tif"),
                         filetype = "GTiff", overwrite = T)
      
      AvEm20xx_gcs_tpyr <- (AvEm20xx_gcs_tpp/simlength) # tpyr stands for tonnes per pixel per year
      terra::writeRaster(AvEm20xx_gcs_tpyr, paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_gcs_tpyr",sdm,".tif"),
                         filetype = "GTiff", overwrite = T)
      
      # Compute pixel area in square meters
      areaperpixel_m2 <- terra::cellSize(AvEm20xx_gcs_tpp, unit="m")
      
      # Convert to hectares (1 ha = 10,000 m²)
      areaperpixel <- areaperpixel_m2 / 10000
      
      # Convert emissions to tonnes per hectare per year
      AvEm20xx_gcs_thayr <- (AvEm20xx_gcs_tpp / simlength) / areaperpixel
      
      # Save the resulting raster
      terra::writeRaster(AvEm20xx_gcs_thayr, paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_gcs_thayr",sdm,".tif"),
                         filetype = "GTiff", overwrite = T)
      
      BaU20xxb <- rast(paste0(emissionsdir,"/",lastyr,regiontag,"/BaU/emissions_out_BaU/e",firstyr,"-",lastyr,"_BaU_tCO2e",sdm,".tif"))
      ICS20xxb <- rast(paste0(emissionsdir,"/",lastyr,regiontag,"/ICS/emissions_out_ICS/e",firstyr,"-",lastyr,"_ICS_tCO2e",sdm,".tif"))
      if (sdm == "_mean"){
        AvEm20xx_wm_tpp <- BaU20xxb - ICS20xxb #tpp stands for tonnes per pixel per period
      } else if (sdm == "_se") {
        AvEm20xx_wm_tpp <- sqrt((BaU20xxb - ICS20xxb)^2)/sqrt(30) # # WARNING !!! PROPAGATE THE ERROR CORRECTLY PLEASE - PLACEHOLDER FOR TNC
      }
      terra::writeRaster(AvEm20xx_wm_tpp, paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_wm_tpp",sdm,".tif"),
                         filetype = "GTiff", overwrite = T)
      
      BaU20xxc <- rast(paste0(emissionsdir,"/",lastyr,regiontag,"/BaU/emissions_out_BaU/e",firstyr,"-",lastyr,"_BaU_tCO2e_thay",sdm,".tif"))
      ICS20xxc <- rast(paste0(emissionsdir,"/",lastyr,regiontag,"/ICS/emissions_out_ICS/e",firstyr,"-",lastyr,"_ICS_tCO2e_thay",sdm,".tif"))
      if (sdm == "_mean"){
        AvEm20xx_wm_thay <- BaU20xxc - ICS20xxc #thay stands for tonnes per hectare per yr
      } else if (sdm == "_se") {
        AvEm20xx_wm_thay <- sqrt((BaU20xxc - ICS20xxc)^2)/sqrt(30) # # WARNING !!! PROPAGATE THE ERROR CORRECTLY PLEASE - PLACEHOLDER FOR TNC
      }
      terra::writeRaster(AvEm20xx_wm_thay, paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_wm_thay",sdm,".tif"),
                         filetype = "GTiff", overwrite = T)
      
      
    } else {
      print(paste0("ERROR: One or more files in ",emissionsdir,"/",lastyr,"/ seem to be missing")) # Is this valid? or only one works
    }
    
    
    
    # Zonal statistics for cross-validation and debugging ----
    if (zonalstats == 1){
      
      setwd(admindir)
      # lastyr = 2035
      
      if (byregion == "Global"){
        # print("fix this chunk!") # Not to be used in the short term really...
        adminnew <- st_read("regions_adm0/mofuss_regions0.gpkg") %>%
          dplyr::mutate(zone = 1:100)
        admindb <- adminnew %>% st_drop_geometry()
        head(adminnew)
        sort(adminnew$NAME_0)
        
        adminnew_p <- st_read("regions_adm0_p/mofuss_regions0_p.gpkg") %>%
          dplyr::mutate(zone = 1:100) %>%
          dplyr::mutate(km2_vector = round(st_area(.)/1000000,0)) %>%
          units::drop_units()
        admindb_p <- adminnew_p %>% st_drop_geometry()
        head(adminnew_p)
        sort(adminnew_p$NAME_0)
        
      } else if (byregion == "Continental"){
        adminnew <- st_read("regions_adm0/mofuss_regions0.gpkg") %>%
          dplyr::mutate(zone = 1:100) %>% # WATCHOUT ABOUT THIS! AUTOMATE LAST VALUE (e.g. 99 or 100)
          dplyr::filter(grepl(mofuss_region,mofuss_reg))
        admindb <- adminnew %>% st_drop_geometry()
        head(adminnew)
        sort(adminnew$NAME_0)
        
        adminnew_p <- st_read("regions_adm0_p/mofuss_regions0_p.gpkg") %>%
          dplyr::mutate(zone = 1:100) %>% # WATCHOUT ABOUT THIS! AUTOMATE LAST VALUE (e.g. 99 or 100)
          dplyr::filter(grepl(mofuss_region,mofuss_reg)) %>%
          dplyr::mutate(km2_vector = round(st_area(.)/1000000,0)) %>%
          units::drop_units()
        admindb_p <- adminnew_p %>% st_drop_geometry()
        head(adminnew_p)
        sort(adminnew_p$NAME_0)
        
      } else if (byregion == "Regional"){
        adminnew <- st_read("regions_adm0/mofuss_regions0.gpkg") %>%
          dplyr::mutate (zone = 1:100) %>%
          dplyr::filter (mofuss_reg == mofuss_region)
        admindb <- adminnew %>% st_drop_geometry()
        sort(adminnew$NAME_0)
        
        adminnew_p <- st_read("regions_adm0_p/mofuss_regions0_p.gpkg") %>%
          dplyr::mutate (zone = 1:100) %>%
          dplyr::filter (mofuss_reg == mofuss_region) %>%
          dplyr::mutate(km2_vector = round(st_area(.)/1000000,0)) %>%
          units::drop_units()
        admindb_p <- adminnew_p %>% st_drop_geometry()
        head(adminnew_p)
        sort(adminnew_p$NAME_0)
        
      } else if (byregion == "Country"){
        adminnew <- st_read("regions_adm0/mofuss_regions0.gpkg") %>%
          dplyr::mutate (zone = 1:101) %>%
          dplyr::filter (NAME_0 == mofuss_region)
        admindb <- adminnew %>% st_drop_geometry()
        sort(adminnew$NAME_0)
        
        adminnew_p <- st_read("regions_adm0_p/mofuss_regions0_p.gpkg") %>%
          dplyr::mutate (zone = 1:101) %>%
          dplyr::filter (NAME_0 == mofuss_region) %>%
          dplyr::mutate(km2_vector = round(st_area(.)/1000000,0)) %>%
          units::drop_units()
        admindb_p <- adminnew_p %>% st_drop_geometry()
        sort(adminnew_p$NAME_0)
        
      }
      
      AvEm_gcs_tppr <- raster(paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_gcs_tpp",sdm,".tif"))
      AvEm_gcs_tpyr <- raster(paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_gcs_tpyr",sdm,".tif"))
      AvEm_gcs_thayr <- raster(paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_gcs_thayr",sdm,".tif"))
      AvEm_wm_tppr <- raster(paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_wm_tpp",sdm,".tif"))
      AvEm_wm_thayr <- raster(paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_wm_thay",sdm,".tif"))
      
      # plot(adminnew)
      admin_r <- fasterize(adminnew, AvEm_gcs_tppr, field = "zone") # %>% mask(AvEm2035r)
      admin_rp <- fasterize(adminnew_p, AvEm_wm_tppr, field = "zone") # %>% mask(AvEm2035r)
      # plot(admin_r)
      # plot(admin_rp)
      
      # Summary tables over GCS rasters
      AvEm_gcs_tppr_sum <- as.data.frame(zonal(AvEm_gcs_tppr, admin_r, 'sum')) %>% # CORRECT SUM FOR SE PROPAGATION
        dplyr::left_join(.,adminnew, by = "zone") %>%
        dplyr::select(-zone, -Subregion, -ID, -mofuss_reg,-geom) %>%
        dplyr::mutate(eMtCO2e = round(sum/1000000,2)) %>% # tonnes to megatonnes
        dplyr::mutate(eMtCO2e_yr = round(eMtCO2e/simlength,2)) %>% # period to year
        dplyr::relocate(sum, .after = NAME_0) %>%
        rename_with(., .fn = ~paste0(firstyr,"-",lastyr,"_tpp"), .cols = all_of("sum"))
      AvEm_gcs_tppr_sum
      setwd(emissionsdir)
      write.csv(AvEm_gcs_tppr_sum,paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_gcs_tpp_sum",sdm,".csv"), row.names=FALSE, quote=FALSE)
      
      # Summary tables over PCS rasters
      AvEm_wm_tppr_areaT <- as.data.frame(zonal(admin_rp, admin_rp, 'count'))
      AvEm_wm_tppr_sum <- as.data.frame(zonal(AvEm_wm_tppr, admin_rp, 'sum')) %>%
        dplyr::left_join(.,adminnew_p, by = "zone") %>%
        dplyr::left_join(.,AvEm_wm_tppr_areaT, by = "zone") %>%
        dplyr::rename("km2_raster" = "count") %>%
        dplyr::select(-zone, -Subregion, -ID, -mofuss_reg,-geom) %>%
        dplyr::mutate(eMtCO2e = round(sum/1000000,2)) %>% # tonnes to megatonnes
        dplyr::mutate(eMtCO2e_yr = round(eMtCO2e/simlength,2)) %>% # period to year
        dplyr::mutate(etCO2e_hayr = round(sum/(km2_raster*100)/simlength,4)) %>% # by hectare (entire country) and by year
        dplyr::relocate(sum, .after = NAME_0) %>%
        dplyr::relocate(km2_raster, .after = etCO2e_hayr) %>%
        dplyr::relocate(km2_vector, .after = km2_raster) %>%
        rename_with(AvEm_wm_tppr_sum, .fn = ~paste0(firstyr,"-",lastyr,"_tpp"), .cols = all_of("sum"))
      AvEm_wm_tppr_sum
      write.csv(AvEm_wm_tppr_sum,paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_wm_tpp_sum",sdm,".csv"), row.names=FALSE, quote=FALSE)
      
      ## raster and tables summaries for cross validation ----
      AvEm_gcs_tpyrt <- rast(paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_gcs_tpyr",sdm,".tif"))
      exf1 <- round(AvEm_gcs_tpyrt %>%
                      terra::global(., 'sum', na.rm=TRUE) %>%
                      pull(sum)/1000000,0)
      print(paste0(exf1, " mt/yr for ",lastyr," 1.- Sums all non-null pixel values of a raster in GCS depicting tonnes per pixel per year; and convert them in the code to megatonnes per year = annual avoided emissions in SSA in 2050/35"))
      
      exf2 <- AvEm_gcs_tppr_sum %>%
        summarise("X_validation_gcs" = sum(eMtCO2e_yr)) %>%
        pull(X_validation_gcs) %>%
        round(.,0)
      print(paste0(exf2, " mt/yr for ",lastyr," 2.- Sums all rows of a table (from a GCS layer) showing megatonnes per country per year = annual avoided emissions in SSA in 2050/35"))
      
      AvEm_wm_tpprt <- rast(paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_wm_tpp",sdm,".tif"))
      exf3 <- round(AvEm_wm_tpprt %>%
                      terra::global(., 'sum', na.rm=TRUE) %>%
                      pull(sum)/1000000/simlength,0)
      print(paste0(exf3, " mt/yr for ",lastyr," 3.- Sums all non-null pixel values of a raster in PCS depicting tonnes per km2 per period; and convert them in the code to megatonnes per year = annual avoided emissions in SSA in 2050/35"))
      
      print(paste0(exf3, " mt/yr for ",lastyr," 4.- Sums all rows of a table (from a PCS layer) showing megatonnes per country per year = annual avoided emissions in SSA in 2050/35"))
      exf4 <- AvEm_wm_tppr_sum %>%
        summarise("X_validation_pcs" = sum(eMtCO2e_yr)) %>%
        pull(X_validation_pcs) %>%
        round(.,0)
      
      AvEm_wm_thayrt <- rast(paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_wm_thay",sdm,".tif"))
      exf5 <- round(AvEm_wm_thayrt %>%
                      terra::global(., 'sum', na.rm=TRUE) %>%
                      pull(sum)*100/1000000,0)
      print(paste0(exf5, " mt/yr for ",lastyr," 5.- Sums all non-null pixel values of a raster in PCS depicting tonnes per ha per year; and convert them in the code to megatonnes per year = annual avoided emissions in SSA in 2050/35"))
      
    }
  }
}

# Integrate mean and se into one summary table - THIS IS A PLACEHOLDER FOR TNC - RE CODE THE UNCERTAINTY ANALYSIS

setwd(paste0(lastyr,regiontag))

# Function to merge mean and SE tables while keeping one copy of all "km2" variables
merge_tables <- function(mean_file, se_file, output_file) {
  # Read the data without printing column type messages
  mean_df <- read_csv(mean_file, show_col_types = FALSE)
  se_df <- read_csv(se_file, show_col_types = FALSE)
  
  # Identify common columns (assuming first two are always ID columns)
  common_cols <- names(mean_df)[1:2]
  
  # Identify value columns (excluding common and km2 columns)
  value_cols <- setdiff(names(mean_df), common_cols)
  
  # Identify all "km2" columns
  km2_cols <- value_cols[grepl("km2", value_cols)]
  
  # Remove km2 columns from value_cols to avoid renaming them as mean/se
  value_cols <- setdiff(value_cols, km2_cols)
  
  # Rename columns in SE table
  se_df <- se_df %>%
    rename_with(~ paste0(., "_se"), all_of(value_cols))
  
  # Rename columns in Mean table
  mean_df <- mean_df %>%
    rename_with(~ paste0(., "_mean"), all_of(value_cols))
  
  # Merge tables
  merged_df <- mean_df %>%
    left_join(se_df, by = common_cols)
  
  # Ensure only existing mean-se columns are included
  mean_se_cols <- as.vector(t(outer(value_cols, c("_mean", "_se"), paste0)))
  mean_se_cols <- intersect(mean_se_cols, names(merged_df))  # Only keep existing ones

# Order columns: common columns first, then mean-SE pairs, then one copy of "km2" variables
ordered_cols <- c(common_cols, mean_se_cols, km2_cols)

# Select only existing columns in merged_df
merged_df <- merged_df %>%
  select(all_of(intersect(ordered_cols, names(merged_df))))

# Save the merged table
write_csv(merged_df, output_file)

# Delete original mean and SE files
# file_delete(c(mean_file, se_file))

return(output_file)
}

# Automatically find all mean and SE CSV files
all_files <- dir(pattern = "AE(2035|2050)_.*_(mean|se)\\.csv")

# Extract unique prefixes (without _mean.csv or _se.csv)
table_prefixes <- unique(gsub("_(mean|se)\\.csv$", "", all_files))

# Process each detected table pair
for (prefix in table_prefixes) {
  mean_file <- paste0(prefix, "_mean.csv")
  se_file <- paste0(prefix, "_se.csv")
  output_file <- paste0(prefix, "_merged.csv")
  
  if (file_exists(mean_file) & file_exists(se_file)) {
    output <- merge_tables(mean_file, se_file, output_file)
    print(paste("Merged table saved:", output))
  } else {
    print(paste("Skipping:", prefix, "— missing one of the files"))
  }
}

print("Merging completed and original files deleted!")


#End ----

