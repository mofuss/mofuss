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
# Date: Jan 2025
# Que hace este scripe????

# 2dolist ----
## Faltaría 2010-2050

# Internal parameters ----
temdirdefined <- 1
fixdir <-  0
string_pattern_yes <- "zmb_bau_plant_1km_subc_n" #String pattern to be searched when selecting folders for the rasters' geocomputation
string_pattern_no <- "idw_" #String pattern to be searched when selecting folders for the rasters' geocomputation
 
# Load packages ----
library(dplyr)
library(fs)
library(readxl)
library(tcltk)
library(tibble)
library(tidyverse)
library(terra)
# terraOptions(steps = 55)
if (temdirdefined == 1) {
  # Define the rTemp directory
  setwd(tk_choose.dir(default = getwd(), caption = "Define the rTemp directory"))
  rTempdir <- getwd()
  terraOptions(tempdir = rTempdir)
}
# terraOptions(memfrac=0.9)
# terraOptions(progress=0)

if (fixdir == 1){
  
  # Define a particular directory when needed:
  adm0_dirs <- c(#"C:/Users/aghil/Documents/MoFuSS_FAO_localhost/zmb_bau_1km_subc",
                 "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/zmb_ics2_1km_subc")
                 #"C:/Users/aghil/Documents/MoFuSS_FAO_localhost/zmb_ics2_1km_subc",
                 #"C:/Users/aghil/Documents/MoFuSS_FAO_localhost/zmb_ics2_1km_subc")

} else {
  
  # Define the directory to search
  setwd(tk_choose.dir(default = getwd(), caption = "Define the directory to search"))
  search_path <- getwd()
  
  # List all directories in the specified path
  all_dirs <- dir_ls(search_path, type = "directory")
  
  # Filter directories that match string_pattern_yes and do not match string_pattern_no
  adm0_dirs <- all_dirs[grepl(string_pattern_yes, all_dirs) & !grepl(string_pattern_no, all_dirs)]
  # adm0_dirs <- all_dirs[grepl(string_pattern, all_dirs)]
  # print(class(adm0_dirs))
  # adm0_dirs <- as.character(adm0_dirs)
  # print(class(adm0_dirs))  # Should return only "character"
  adm0_dirs
  
}

# Loop through each adm0 directory----
for (dir in adm0_dirs) {
  #   dir = "E:/rob_first_project"
  print(paste("Processing directory:", dir))
  setwd(dir)
  getwd()
  
  # Read parameters table ----
  read.csv("LULCC/TempTables/Country.csv") %>%
    dplyr::filter(Key. == "1") %>%
    pull(Country) -> country_name
  
  # Specify the directory where the file is located
  parameters_directory <- paste0(getwd(),"/LULCC/DownloadedDatasets/SourceData",country_name)
  
  # Use list.files() to find the file that matches the pattern
  parameters_name <- list.files(path = parameters_directory, pattern = "^parameters.*\\.csv$", full.names = TRUE)
  
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
  delimiter <- detect_delimiter(parameters_name)
  # Read the CSV file with the detected delimiter
  country_parameters <- read_delim(parameters_name, delim = delimiter)
  print(tibble::as_tibble(country_parameters), n=100)
  
  country_parameters %>%
    dplyr::filter(Var == "end_year") %>%
    pull(ParCHR) -> end_year
  
  # Find all "debugging_n" subfolders
  debugging_dirs <- list.dirs(dir, full.names = TRUE, recursive = FALSE)
  debugging_dirs <- debugging_dirs[grepl("debugging_\\d+", basename(debugging_dirs))]
  
  # Loop through each "debugging_n" folder
  
  for (i in 1:length(debugging_dirs)) {
    # i=1
    debug_dir <- debugging_dirs[i]
    # debug_dir = "E:/ASIA_adm0_central_apr2024/debugging_2"
    print(paste("Processing directory", i, ":", debug_dir))
    
    # NRB---- 
    listGlH <- list.files(debug_dir, pattern = "^Growth_less_harv.+[.]tif$",ignore.case=F)
    stackGlH <- rast(paste0(debug_dir,"/",listGlH))
    nlay <- nlyr(stackGlH)
    
    listGx <- list.files(debug_dir, pattern = "^Growth.+[.]tif$",ignore.case=F)
    listG <- listGx[ !grepl("_less_harv", listGx) ]
    stackG <- rast(paste0(debug_dir,"/",listG))
    nlyr(stackG) #for cross checking pattern
    
    nlay_yr <- nlay+2009
    STdyn <- nlay-1
    
    nrb_name_per <- paste("nrb_sum_bin2010", nlay_yr, sep = "_")
    calculated_nrb_per <- stackG[[1]] - stackGlH[[nlay]] # Bin is entire period, do not use in final tables 
    calculated_nrb_per[calculated_nrb_per <= 0] = NA 
    assign(nrb_name_per, calculated_nrb_per)
    
    nrb_name <- paste("nrb_sum_bin2020", nlay_yr, sep = "_")
    calculated_nrb <- stackG[[11]] - stackGlH[[nlay]] # Bin will start in 2020 and end in the final year
    calculated_nrb[calculated_nrb <= 0] = NA 
    assign(nrb_name, calculated_nrb)
    
    if (STdyn == 10){
      nrb_bin2010_2020 <- stackG[[1]] - stackGlH[[nlay]] # Bin will be 2010-2020
      nrb_bin2010_2020[nrb_bin2010_2020 <= 0] = NA
      
    }
    
    if (STdyn == 20){
      nrb_bin2010_2020 <- stackG[[1]] - stackGlH[[10]] # Bin will be 2010-2020
      nrb_bin2010_2020[nrb_bin2010_2020 <= 0] = NA 
      
      nrb_bin2020_2030 <- stackG[[11]] - stackGlH[[nlay]] # Bin will be 2020-2030
      nrb_bin2020_2030[nrb_bin2020_2030 <= 0] = NA 
      
    }
    
    if (STdyn == 25){
      nrb_bin2010_2020 <- stackG[[1]] - stackGlH[[10]] # Bin will be 2010-2020
      nrb_bin2010_2020[nrb_bin2010_2020 <= 0] = NA 
      
      nrb_bin2020_2035 <- stackG[[11]] - stackGlH[[nlay]] # Bin will be 2020-2035
      nrb_bin2020_2035[nrb_bin2020_2035 <= 0] = NA 
      
    }
    
    if (STdyn == 30){ # STdyn = 30 # 2040
      nrb_bin2010_2020 <- stackG[[1]] - stackGlH[[10]] # Bin will be 2010-2020
      nrb_bin2010_2020[nrb_bin2010_2020 <= 0] = NA 
      
      nrb_bin2020_2030 <- stackG[[11]] - stackGlH[[20]] # Bin will be 2020-2030
      nrb_bin2020_2030[nrb_bin2020_2030 <= 0] = NA 
      
      nrb_bin2030_2040 <- stackG[[21]] - stackGlH[[nlay]] # Bin will be 2030-2040
      nrb_bin2030_2040[nrb_bin2030_2040 <= 0] = NA 
      
    } 
    
    if (STdyn == 40){ # STdyn = 40 # 2050
      nrb_bin2010_2020 <- stackG[[1]] - stackGlH[[10]] # Bin will be 2010-2020
      nrb_bin2010_2020[nrb_bin2010_2020 <= 0] = NA 
      
      nrb_bin2020_2030 <- stackG[[11]] - stackGlH[[20]] # Bin will be 2020-2030
      nrb_bin2020_2030[nrb_bin2020_2030 <= 0] = NA
      
      nrb_bin2020_2035 <- stackG[[11]] - stackGlH[[25]] # Bin will be 2020-2035
      nrb_bin2020_2035[nrb_bin2020_2035 <= 0] = NA 
      
      nrb_bin2020_2050 <- stackG[[11]] - stackGlH[[nlay]] # Bin will be 2020-2050
      nrb_bin2020_2050[nrb_bin2020_2050 <= 0] = NA 
      
      nrb_bin2030_2040 <- stackG[[21]] - stackGlH[[30]] # Bin will be 2030-2040
      nrb_bin2030_2040[nrb_bin2030_2040 <= 0] = NA 
      
      nrb_bin2040_2050 <- stackG[[31]] - stackGlH[[nlay]] # Bin will be 2040-2050
      nrb_bin2040_2050[nrb_bin2040_2050 <= 0] = NA 
      
    }
    
    # Harvest----
    
    listharvx_per <- list.files(debug_dir, pattern = "^Harvest_tot.+[.]tif$",ignore.case=F)
    listharv_per <- listharvx_per[ !grepl("_tot_nrb", listharvx_per) ]
    stackhar_per <- rast(paste0(debug_dir,"/",listharv_per))
    nlyr(stackhar_per)
    
    harv_name_per <- paste("harv_sum_bin2010", nlay_yr, sep = "_")
    harvest_st_per <- app(stackhar_per, fun = sum)
    assign(harv_name_per, harvest_st_per)
    
    harv_name <- paste("harv_sum_bin2020", nlay_yr, sep = "_")
    listharv <- listharv_per[11:nlay]
    stackharv <- rast(paste0(debug_dir,"/",listharv))
    nlyr(stackharv)
    
    harvest_st <- app(stackharv, fun=sum)
    assign(harv_name, harvest_st)
    
    if (STdyn == 10){
      listharv_bin2010_2020 <- listharv_per[1:nlay]
      stackharv_bin2010_2020 <- rast(paste0(debug_dir,"/",listharv_bin2010_2020))
      nlyr(stackharv_bin2010_2020)
      harvest_st_bin2010_2020 <- app(stackharv_bin2010_2020, fun=sum)
      
    }
    
    if (STdyn == 20){
      listharv_bin2010_2020 <- listharv_per[1:10]
      stackharv_bin2010_2020 <- rast(paste0(debug_dir,"/",listharv_bin2010_2020))
      nlyr(stackharv_bin2010_2020)
      harvest_st_bin2010_2020 <- app(stackharv_bin2010_2020, fun=sum)
      
      listharv_bin2020_2030 <- listharv_per[11:nlay]
      stackharv_bin2020_2030 <- rast(paste0(debug_dir,"/",listharv_bin2020_2030))
      nlyr(stackharv_bin2020_2030)
      harvest_st_bin2020_2030 <- app(stackharv_bin2020_2030, fun=sum)
      
    }
    
    if (STdyn == 25){
      listharv_bin2010_2020 <- listharv_per[1:10]
      stackharv_bin2010_2020 <- rast(paste0(debug_dir,"/",listharv_bin2010_2020))
      nlyr(stackharv_bin2010_2020)
      harvest_st_bin2010_2020 <- app(stackharv_bin2010_2020, fun=sum)
      
      listharv_bin2020_2035 <- listharv_per[11:nlay]
      stackharv_bin2020_2035 <- rast(paste0(debug_dir,"/",listharv_bin2020_2035))
      nlyr(stackharv_bin2020_2035)
      harvest_st_bin2020_2035 <- app(stackharv_bin2020_2035, fun=sum)
      
    }
    
    if (STdyn == 30){
      listharv_bin2010_2020 <- listharv_per[1:10]
      stackharv_bin2010_2020 <- rast(paste0(debug_dir,"/",listharv_bin2010_2020))
      nlyr(stackharv_bin2010_2020)
      harvest_st_bin2010_2020 <- app(stackharv_bin2010_2020, fun=sum)
      
      listharv_bin2020_2030 <- listharv_per[11:20]
      stackharv_bin2020_2030 <- rast(paste0(debug_dir,"/",listharv_bin2020_2030))
      nlyr(stackharv_bin2020_2030)
      harvest_st_bin2020_2030 <- app(stackharv_bin2020_2030, fun=sum)
      
      listharv_bin2030_2040 <- listharv_per[21:nlay]
      stackharv_bin2030_2040 <- rast(paste0(debug_dir,"/",listharv_bin2030_2040))
      nlyr(stackharv_bin2030_2040)
      harvest_st_bin2030_2040 <- app(stackharv_bin2030_2040, fun=sum)
      
    }
    
    if (STdyn == 40){
      listharv_bin2010_2020 <- listharv_per[1:10]
      stackharv_bin2010_2020 <- rast(paste0(debug_dir,"/",listharv_bin2010_2020))
      nlyr(stackharv_bin2010_2020)
      harvest_st_bin2010_2020 <- app(stackharv_bin2010_2020, fun=sum)
      
      listharv_bin2020_2030 <- listharv_per[11:20]
      stackharv_bin2020_2030 <- rast(paste0(debug_dir,"/",listharv_bin2020_2030))
      nlyr(stackharv_bin2020_2030)
      harvest_st_bin2020_2030 <- app(stackharv_bin2020_2030, fun=sum)
      
      listharv_bin2020_2035 <- listharv_per[11:25]
      stackharv_bin2020_2035 <- rast(paste0(debug_dir,"/",listharv_bin2020_2035))
      nlyr(stackharv_bin2020_2035)
      harvest_st_bin2020_2035 <- app(stackharv_bin2020_2035, fun=sum)
      
      listharv_bin2020_2050 <- listharv_per[11:nlay]
      stackharv_bin2020_2050 <- rast(paste0(debug_dir,"/",listharv_bin2020_2050))
      nlyr(stackharv_bin2020_2050)
      harvest_st_bin2020_2050 <- app(stackharv_bin2020_2050, fun=sum)
      
      listharv_bin2030_2040 <- listharv_per[21:30]
      stackharv_bin2030_2040 <- rast(paste0(debug_dir,"/",listharv_bin2030_2040))
      nlyr(stackharv_bin2030_2040)
      harvest_st_bin2030_2040 <- app(stackharv_bin2030_2040, fun=sum)
      
      listharv_bin2040_2050 <- listharv_per[31:nlay]
      stackharv_bin2040_2050 <- rast(paste0(debug_dir,"/",listharv_bin2040_2050))
      nlyr(stackharv_bin2040_2050)
      harvest_st_bin2040_2050 <- app(stackharv_bin2040_2050, fun=sum)
      
    }
    
    # Store rasters in the lists ----
    
    # Create the directory only if it doesn't exist
    if (!dir.exists("temp_raster_lists")) {
      dir.create("temp_raster_lists")
    }
    
    if (STdyn == 10){
      ## STdyn == 10 ----
      ### nrb_bin2010_2020----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2010_2020_step_", i - 1, ".rds"))) {
        nrb_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/nrb_bin2010_2020_step_", i - 1, ".rds"))
      } else {
        nrb_bin2010_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2010_2020_", i, ".tif")
      writeRaster(nrb_bin2010_2020, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2010_2020_list[[length(nrb_bin2010_2020_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2010_2020_list, file = paste0("temp_raster_lists/nrb_bin2010_2020_step_", i, ".rds"))
      
      ### harvest_st_bin2010_2020 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i - 1, ".rds"))) {
        harvest_st_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2010_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2010_2020_", i, ".tif")
      writeRaster(harvest_st_bin2010_2020, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2010_2020_list[[length(harvest_st_bin2010_2020_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2010_2020_list, file = paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i, ".rds"))
      
      ### agb_2010----
      if (file.exists(paste0("temp_raster_lists/agb_2010_step_", i - 1, ".rds"))) {
        agb_2010_list <- readRDS(paste0("temp_raster_lists/agb_2010_step_", i - 1, ".rds"))
      } else {
        agb_2010_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2010_", i, ".tif")
      agb_2010 <- stackG[[1]]
      writeRaster(agb_2010, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2010_list[[length(agb_2010_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2010_list, file = paste0("temp_raster_lists/agb_2010_step_", i, ".rds"))
      
      ### agb_2020----
      if (file.exists(paste0("temp_raster_lists/agb_2020_step_", i - 1, ".rds"))) {
        agb_2020_list <- readRDS(paste0("temp_raster_lists/agb_2020_step_", i - 1, ".rds"))
      } else {
        agb_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2020_", i, ".tif")
      agb_2020 <- stackG[[10]]
      writeRaster(agb_2020, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2020_list[[length(agb_2020_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2020_list, file = paste0("temp_raster_lists/agb_2020_step_", i, ".rds"))
      
    }
    
    if (STdyn == 20){
      ## STdyn == 20 ----
      ### nrb_bin2010_2020----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2010_2020_step_", i - 1, ".rds"))) {
        nrb_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/nrb_bin2010_2020_step_", i - 1, ".rds"))
      } else {
        nrb_bin2010_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2010_2020_", i, ".tif")
      writeRaster(nrb_bin2010_2020, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2010_2020_list[[length(nrb_bin2010_2020_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2010_2020_list, file = paste0("temp_raster_lists/nrb_bin2010_2020_step_", i, ".rds"))
      
      ### harvest_st_bin2010_2020 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i - 1, ".rds"))) {
        harvest_st_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2010_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2010_2020_", i, ".tif")
      writeRaster(harvest_st_bin2010_2020, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2010_2020_list[[length(harvest_st_bin2010_2020_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2010_2020_list, file = paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i, ".rds"))
      
      ### nrb_bin2020_2030----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2020_2030_step_", i - 1, ".rds"))) {
        nrb_bin2020_2030_list <- readRDS(paste0("temp_raster_lists/nrb_bin2020_2030_step_", i - 1, ".rds"))
      } else {
        nrb_bin2020_2030_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2020_2030_", i, ".tif")
      writeRaster(nrb_bin2020_2030, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2020_2030_list[[length(nrb_bin2020_2030_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2020_2030_list, file = paste0("temp_raster_lists/nrb_bin2020_2030_step_", i, ".rds"))
      
      ### harvest_st_bin2020_2030 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2020_2030_step_", i - 1, ".rds"))) {
        harvest_st_bin2020_2030_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2020_2030_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2020_2030_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2020_2030_", i, ".tif")
      writeRaster(harvest_st_bin2020_2030, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2020_2030_list[[length(harvest_st_bin2020_2030_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2020_2030_list, file = paste0("temp_raster_lists/harvest_st_bin2020_2030_step_", i, ".rds"))
      
      ### agb_2010----
      if (file.exists(paste0("temp_raster_lists/agb_2010_step_", i - 1, ".rds"))) {
        agb_2010_list <- readRDS(paste0("temp_raster_lists/agb_2010_step_", i - 1, ".rds"))
      } else {
        agb_2010_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2010_", i, ".tif")
      agb_2010 <- stackG[[1]]
      writeRaster(agb_2010, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2010_list[[length(agb_2010_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2010_list, file = paste0("temp_raster_lists/agb_2010_step_", i, ".rds"))
      
      ### agb_2020----
      if (file.exists(paste0("temp_raster_lists/agb_2020_step_", i - 1, ".rds"))) {
        agb_2020_list <- readRDS(paste0("temp_raster_lists/agb_2020_step_", i - 1, ".rds"))
      } else {
        agb_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2020_", i, ".tif")
      agb_2020 <- stackG[[10]]
      writeRaster(agb_2020, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2020_list[[length(agb_2020_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2020_list, file = paste0("temp_raster_lists/agb_2020_step_", i, ".rds"))
      
      ### agb_2030----
      if (file.exists(paste0("temp_raster_lists/agb_2030_step_", i - 1, ".rds"))) {
        agb_2030_list <- readRDS(paste0("temp_raster_lists/agb_2030_step_", i - 1, ".rds"))
      } else {
        agb_2030_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2030_", i, ".tif")
      agb_2030 <- stackG[[20]]
      writeRaster(agb_2030, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2030_list[[length(agb_2030_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2030_list, file = paste0("temp_raster_lists/agb_2030_step_", i, ".rds"))
      
    }
    
    if (STdyn == 25){
      ## STdyn == 25 ----
      ### nrb_bin2010_2020----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2010_2020_step_", i - 1, ".rds"))) {
        nrb_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/nrb_bin2010_2020_step_", i - 1, ".rds"))
      } else {
        nrb_bin2010_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2010_2020_", i, ".tif")
      writeRaster(nrb_bin2010_2020, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2010_2020_list[[length(nrb_bin2010_2020_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2010_2020_list, file = paste0("temp_raster_lists/nrb_bin2010_2020_step_", i, ".rds"))
      
      ### harvest_st_bin2010_2020 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i - 1, ".rds"))) {
        harvest_st_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2010_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2010_2020_", i, ".tif")
      writeRaster(harvest_st_bin2010_2020, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2010_2020_list[[length(harvest_st_bin2010_2020_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2010_2020_list, file = paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i, ".rds"))
      
      ### nrb_bin2020_2035----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2020_2035_step_", i - 1, ".rds"))) {
        nrb_bin2020_2035_list <- readRDS(paste0("temp_raster_lists/nrb_bin2020_2035_step_", i - 1, ".rds"))
      } else {
        nrb_bin2020_2035_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2020_2035_", i, ".tif")
      writeRaster(nrb_bin2020_2035, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2020_2035_list[[length(nrb_bin2020_2035_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2020_2035_list, file = paste0("temp_raster_lists/nrb_bin2020_2035_step_", i, ".rds"))
      
      ### harvest_st_bin2020_2035 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2020_2035_step_", i - 1, ".rds"))) {
        harvest_st_bin2020_2035_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2020_2035_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2020_2035_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2020_2035_", i, ".tif")
      writeRaster(harvest_st_bin2020_2035, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2020_2035_list[[length(harvest_st_bin2020_2035_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2020_2035_list, file = paste0("temp_raster_lists/harvest_st_bin2020_2035_step_", i, ".rds"))
      
      ### agb_2010----
      if (file.exists(paste0("temp_raster_lists/agb_2010_step_", i - 1, ".rds"))) {
        agb_2010_list <- readRDS(paste0("temp_raster_lists/agb_2010_step_", i - 1, ".rds"))
      } else {
        agb_2010_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2010_", i, ".tif")
      agb_2010 <- stackG[[1]]
      writeRaster(agb_2010, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2010_list[[length(agb_2010_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2010_list, file = paste0("temp_raster_lists/agb_2010_step_", i, ".rds"))
      
      ### agb_2020----
      if (file.exists(paste0("temp_raster_lists/agb_2020_step_", i - 1, ".rds"))) {
        agb_2020_list <- readRDS(paste0("temp_raster_lists/agb_2020_step_", i - 1, ".rds"))
      } else {
        agb_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2020_", i, ".tif")
      agb_2020 <- stackG[[10]]
      writeRaster(agb_2020, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2020_list[[length(agb_2020_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2020_list, file = paste0("temp_raster_lists/agb_2020_step_", i, ".rds"))
      
      ### agb_2035----
      if (file.exists(paste0("temp_raster_lists/agb_2035_step_", i - 1, ".rds"))) {
        agb_2035_list <- readRDS(paste0("temp_raster_lists/agb_2035_step_", i - 1, ".rds"))
      } else {
        agb_2035_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2035_", i, ".tif")
      agb_2035 <- stackG[[25]]
      writeRaster(agb_2035, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2035_list[[length(agb_2035_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2035_list, file = paste0("temp_raster_lists/agb_2035_step_", i, ".rds"))
    }
    
    if (STdyn == 30){
      ## STdyn == 30 ----
      ### nrb_bin2010_2020----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2010_2020_step_", i - 1, ".rds"))) {
        nrb_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/nrb_bin2010_2020_step_", i - 1, ".rds"))
      } else {
        nrb_bin2010_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2010_2020_", i, ".tif")
      writeRaster(nrb_bin2010_2020, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2010_2020_list[[length(nrb_bin2010_2020_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2010_2020_list, file = paste0("temp_raster_lists/nrb_bin2010_2020_step_", i, ".rds"))
      
      ### harvest_st_bin2010_2020 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i - 1, ".rds"))) {
        harvest_st_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2010_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2010_2020_", i, ".tif")
      writeRaster(harvest_st_bin2010_2020, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2010_2020_list[[length(harvest_st_bin2010_2020_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2010_2020_list, file = paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i, ".rds"))
      
      ### nrb_bin2020_2030----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2020_2030_step_", i - 1, ".rds"))) {
        nrb_bin2020_2030_list <- readRDS(paste0("temp_raster_lists/nrb_bin2020_2030_step_", i - 1, ".rds"))
      } else {
        nrb_bin2020_2030_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2020_2030_", i, ".tif")
      writeRaster(nrb_bin2020_2030, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2020_2030_list[[length(nrb_bin2020_2030_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2020_2030_list, file = paste0("temp_raster_lists/nrb_bin2020_2030_step_", i, ".rds"))
      
      ### harvest_st_bin2020_2030 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2020_2030_step_", i - 1, ".rds"))) {
        harvest_st_bin2020_2030_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2020_2030_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2020_2030_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2020_2030_", i, ".tif")
      writeRaster(harvest_st_bin2020_2030, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2020_2030_list[[length(harvest_st_bin2020_2030_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2020_2030_list, file = paste0("temp_raster_lists/harvest_st_bin2020_2030_step_", i, ".rds"))
      
      ### nrb_bin2030_2040----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2030_2040_step_", i - 1, ".rds"))) {
        nrb_bin2030_2040_list <- readRDS(paste0("temp_raster_lists/nrb_bin2030_2040_step_", i - 1, ".rds"))
      } else {
        nrb_bin2030_2040_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2030_2040_", i, ".tif")
      writeRaster(nrb_bin2030_2040, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2030_2040_list[[length(nrb_bin2030_2040_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2030_2040_list, file = paste0("temp_raster_lists/nrb_bin2030_2040_step_", i, ".rds"))
      
      ### harvest_st_bin2030_2040 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2030_2040_step_", i - 1, ".rds"))) {
        harvest_st_bin2030_2040_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2030_2040_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2030_2040_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2030_2040_", i, ".tif")
      writeRaster(harvest_st_bin2030_2040, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2030_2040_list[[length(harvest_st_bin2030_2040_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2030_2040_list, file = paste0("temp_raster_lists/harvest_st_bin2030_2040_step_", i, ".rds"))
      
      ### agb_2010----
      if (file.exists(paste0("temp_raster_lists/agb_2010_step_", i - 1, ".rds"))) {
        agb_2010_list <- readRDS(paste0("temp_raster_lists/agb_2010_step_", i - 1, ".rds"))
      } else {
        agb_2010_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2010_", i, ".tif")
      agb_2010 <- stackG[[1]]
      writeRaster(agb_2010, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2010_list[[length(agb_2010_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2010_list, file = paste0("temp_raster_lists/agb_2010_step_", i, ".rds"))
      
      ### agb_2020----
      if (file.exists(paste0("temp_raster_lists/agb_2020_step_", i - 1, ".rds"))) {
        agb_2020_list <- readRDS(paste0("temp_raster_lists/agb_2020_step_", i - 1, ".rds"))
      } else {
        agb_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2020_", i, ".tif")
      agb_2020 <- stackG[[10]]
      writeRaster(agb_2020, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2020_list[[length(agb_2020_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2020_list, file = paste0("temp_raster_lists/agb_2020_step_", i, ".rds"))
      
      ### agb_2030----
      if (file.exists(paste0("temp_raster_lists/agb_2030_step_", i - 1, ".rds"))) {
        agb_2030_list <- readRDS(paste0("temp_raster_lists/agb_2030_step_", i - 1, ".rds"))
      } else {
        agb_2030_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2030_", i, ".tif")
      agb_2030 <- stackG[[20]]
      writeRaster(agb_2030, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2030_list[[length(agb_2030_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2030_list, file = paste0("temp_raster_lists/agb_2030_step_", i, ".rds"))
      
      ### agb_2040----
      if (file.exists(paste0("temp_raster_lists/agb_2040_step_", i - 1, ".rds"))) {
        agb_2040_list <- readRDS(paste0("temp_raster_lists/agb_2040_step_", i - 1, ".rds"))
      } else {
        agb_2040_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2040_", i, ".tif")
      agb_2040 <- stackG[[30]]
      writeRaster(agb_2040, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2040_list[[length(agb_2040_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2040_list, file = paste0("temp_raster_lists/agb_2040_step_", i, ".rds"))
    }
    
    if (STdyn == 40){
      ## STdyn == 40 ----
      ### nrb_bin2010_2020----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2010_2020_step_", i - 1, ".rds"))) {
        nrb_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/nrb_bin2010_2020_step_", i - 1, ".rds"))
      } else {
        nrb_bin2010_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2010_2020_", i, ".tif")
      writeRaster(nrb_bin2010_2020, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2010_2020_list[[length(nrb_bin2010_2020_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2010_2020_list, file = paste0("temp_raster_lists/nrb_bin2010_2020_step_", i, ".rds"))
      
      ### harvest_st_bin2010_2020 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i - 1, ".rds"))) {
        harvest_st_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2010_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2010_2020_", i, ".tif")
      writeRaster(harvest_st_bin2010_2020, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2010_2020_list[[length(harvest_st_bin2010_2020_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2010_2020_list, file = paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i, ".rds"))
      
      ### nrb_bin2020_2030----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2020_2030_step_", i - 1, ".rds"))) {
        nrb_bin2020_2030_list <- readRDS(paste0("temp_raster_lists/nrb_bin2020_2030_step_", i - 1, ".rds"))
      } else {
        nrb_bin2020_2030_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2020_2030_", i, ".tif")
      writeRaster(nrb_bin2020_2030, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2020_2030_list[[length(nrb_bin2020_2030_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2020_2030_list, file = paste0("temp_raster_lists/nrb_bin2020_2030_step_", i, ".rds"))
      
      ### harvest_st_bin2020_2030 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2020_2030_step_", i - 1, ".rds"))) {
        harvest_st_bin2020_2030_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2020_2030_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2020_2030_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2020_2030_", i, ".tif")
      writeRaster(harvest_st_bin2020_2030, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2020_2030_list[[length(harvest_st_bin2020_2030_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2020_2030_list, file = paste0("temp_raster_lists/harvest_st_bin2020_2030_step_", i, ".rds"))
      
      ### nrb_bin2020_2035----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2020_2035_step_", i - 1, ".rds"))) {
        nrb_bin2020_2035_list <- readRDS(paste0("temp_raster_lists/nrb_bin2020_2035_step_", i - 1, ".rds"))
      } else {
        nrb_bin2020_2035_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2020_2035_", i, ".tif")
      writeRaster(nrb_bin2020_2035, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2020_2035_list[[length(nrb_bin2020_2035_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2020_2035_list, file = paste0("temp_raster_lists/nrb_bin2020_2035_step_", i, ".rds"))
      
      ### harvest_st_bin2020_2035 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2020_2035_step_", i - 1, ".rds"))) {
        harvest_st_bin2020_2035_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2020_2035_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2020_2035_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2020_2035_", i, ".tif")
      writeRaster(harvest_st_bin2020_2035, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2020_2035_list[[length(harvest_st_bin2020_2035_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2020_2035_list, file = paste0("temp_raster_lists/harvest_st_bin2020_2035_step_", i, ".rds"))
      
      
      ### nrb_bin2020_2050----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2020_2050_step_", i - 1, ".rds"))) {
        nrb_bin2020_2050_list <- readRDS(paste0("temp_raster_lists/nrb_bin2020_2050_step_", i - 1, ".rds"))
      } else {
        nrb_bin2020_2050_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2020_2050_", i, ".tif")
      writeRaster(nrb_bin2020_2050, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2020_2050_list[[length(nrb_bin2020_2050_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2020_2050_list, file = paste0("temp_raster_lists/nrb_bin2020_2050_step_", i, ".rds"))
      
      ### harvest_st_bin2020_2050 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2020_2050_step_", i - 1, ".rds"))) {
        harvest_st_bin2020_2050_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2020_2050_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2020_2050_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2020_2050_", i, ".tif")
      writeRaster(harvest_st_bin2020_2050, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2020_2050_list[[length(harvest_st_bin2020_2050_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2020_2050_list, file = paste0("temp_raster_lists/harvest_st_bin2020_2050_step_", i, ".rds"))
      
      ### nrb_bin2030_2040----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2030_2040_step_", i - 1, ".rds"))) {
        nrb_bin2030_2040_list <- readRDS(paste0("temp_raster_lists/nrb_bin2030_2040_step_", i - 1, ".rds"))
      } else {
        nrb_bin2030_2040_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2030_2040_", i, ".tif")
      writeRaster(nrb_bin2030_2040, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2030_2040_list[[length(nrb_bin2030_2040_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2030_2040_list, file = paste0("temp_raster_lists/nrb_bin2030_2040_step_", i, ".rds"))
      
      ### harvest_st_bin2030_2040 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2030_2040_step_", i - 1, ".rds"))) {
        harvest_st_bin2030_2040_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2030_2040_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2030_2040_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2030_2040_", i, ".tif")
      writeRaster(harvest_st_bin2030_2040, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2030_2040_list[[length(harvest_st_bin2030_2040_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2030_2040_list, file = paste0("temp_raster_lists/harvest_st_bin2030_2040_step_", i, ".rds"))
      
      ### nrb_bin2040_2050----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2040_2050_step_", i - 1, ".rds"))) {
        nrb_bin2040_2050_list <- readRDS(paste0("temp_raster_lists/nrb_bin2040_2050_step_", i - 1, ".rds"))
      } else {
        nrb_bin2040_2050_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2040_2050_", i, ".tif")
      writeRaster(nrb_bin2040_2050, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2040_2050_list[[length(nrb_bin2040_2050_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2040_2050_list, file = paste0("temp_raster_lists/nrb_bin2040_2050_step_", i, ".rds"))
      
      ### harvest_st_bin2040_2050 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2040_2050_step_", i - 1, ".rds"))) {
        harvest_st_bin2040_2050_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2040_2050_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2040_2050_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2040_2050_", i, ".tif")
      writeRaster(harvest_st_bin2040_2050, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2040_2050_list[[length(harvest_st_bin2040_2050_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2040_2050_list, file = paste0("temp_raster_lists/harvest_st_bin2040_2050_step_", i, ".rds"))
      
      ### agb_2010----
      if (file.exists(paste0("temp_raster_lists/agb_2010_step_", i - 1, ".rds"))) {
        agb_2010_list <- readRDS(paste0("temp_raster_lists/agb_2010_step_", i - 1, ".rds"))
      } else {
        agb_2010_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2010_", i, ".tif")
      agb_2010 <- stackG[[1]]
      writeRaster(agb_2010, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2010_list[[length(agb_2010_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2010_list, file = paste0("temp_raster_lists/agb_2010_step_", i, ".rds"))
      
      ### agb_2020----
      if (file.exists(paste0("temp_raster_lists/agb_2020_step_", i - 1, ".rds"))) {
        agb_2020_list <- readRDS(paste0("temp_raster_lists/agb_2020_step_", i - 1, ".rds"))
      } else {
        agb_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2020_", i, ".tif")
      agb_2020 <- stackG[[10]]
      writeRaster(agb_2020, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2020_list[[length(agb_2020_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2020_list, file = paste0("temp_raster_lists/agb_2020_step_", i, ".rds"))
      
      ### agb_2030----
      if (file.exists(paste0("temp_raster_lists/agb_2030_step_", i - 1, ".rds"))) {
        agb_2030_list <- readRDS(paste0("temp_raster_lists/agb_2030_step_", i - 1, ".rds"))
      } else {
        agb_2030_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2030_", i, ".tif")
      agb_2030 <- stackG[[20]]
      writeRaster(agb_2030, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2030_list[[length(agb_2030_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2030_list, file = paste0("temp_raster_lists/agb_2030_step_", i, ".rds"))
      
      ### agb_2035----
      if (file.exists(paste0("temp_raster_lists/agb_2035_step_", i - 1, ".rds"))) {
        agb_2035_list <- readRDS(paste0("temp_raster_lists/agb_2035_step_", i - 1, ".rds"))
      } else {
        agb_2035_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2035_", i, ".tif")
      agb_2035 <- stackG[[25]]
      writeRaster(agb_2035, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2035_list[[length(agb_2035_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2035_list, file = paste0("temp_raster_lists/agb_2035_step_", i, ".rds"))
      
      ### agb_2040----
      if (file.exists(paste0("temp_raster_lists/agb_2040_step_", i - 1, ".rds"))) {
        agb_2040_list <- readRDS(paste0("temp_raster_lists/agb_2040_step_", i - 1, ".rds"))
      } else {
        agb_2040_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2040_", i, ".tif")
      agb_2040 <- stackG[[30]]
      writeRaster(agb_2040, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2040_list[[length(agb_2040_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2040_list, file = paste0("temp_raster_lists/agb_2040_step_", i, ".rds"))
      
      ### agb_2050----
      if (file.exists(paste0("temp_raster_lists/agb_2050_step_", i - 1, ".rds"))) {
        agb_2050_list <- readRDS(paste0("temp_raster_lists/agb_2050_step_", i - 1, ".rds"))
      } else {
        agb_2050_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2050_", i, ".tif")
      agb_2050 <- stackG[[40]]
      writeRaster(agb_2050, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2050_list[[length(agb_2050_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2050_list, file = paste0("temp_raster_lists/agb_2050_step_", i, ".rds"))
      
    }
    
    # Get all objects except a few ones and remove them
    objs_to_keep <- c("STdyn", "debug_dir", "debugging_dirs", "dir", "i")
    objs_to_remove <- setdiff(ls(), objs_to_keep)
    rm(list = objs_to_remove)
    
    # Free up memory
    gc()
    
    # Close all raster connections
    closeAllConnections()
    
    # Sleep for 15 seconds
    Sys.sleep(15)
    
  }
  
  # MEAN, SD, and SE ----
  # Define the output directory ----
  output_dir <- paste0(dir,"/OutBaU/webmofuss_results")
  
  # Define configurations for different STdyn values
  configurations <- list(
    "10" = list(
      periods = c("2010_2020"),
      output_suffixes = c("10_20"),
      agb_years = c("2010", "2020")
    ),
    "20" = list(
      periods = c("2010_2020", "2020_2030"),
      output_suffixes = c("10_20", "20_30"),
      agb_years = c("2010", "2020", "2030")
    ),
    "25" = list(
      periods = c("2010_2020", "2020_2035"),
      output_suffixes = c("10_20", "20_35"),
      agb_years = c("2010", "2020", "2035")
    ),
    "30" = list(
      periods = c("2010_2020", "2020_2030", "2020_2035", "2020_2040", "2030_2040"),
      output_suffixes = c("10_20", "20_30", "20_35", "20_40", "30_40"),
      agb_years = c("2010", "2020", "2030", "2035", "2040")
    ),
    "40" = list(
      periods = c("2010_2020", "2020_2030", "2020_2035", "2020_2050", "2030_2040", "2040_2050"),
      output_suffixes = c("10_20", "20_30", "20_35", "20_50", "30_40", "40_50"),
      agb_years = c("2010", "2020", "2030", "2035", "2040", "2050")
    )
  )
  
  # Retrieve configuration based on STdyn
  if (as.character(STdyn) %in% names(configurations)) {
    config <- configurations[[as.character(STdyn)]]
    periods <- config$periods
    output_suffixes <- config$output_suffixes
    agb_years <- config$agb_years
  } else {
    stop("Invalid STdyn value")
  }
  
  # Function to calculate mean, SD, and SE for NRB and Harvest rasters
  process_nrb_harvest <- function(period, output_suffix) {
    # Load raster lists
    nrb_list <- readRDS(paste0("temp_raster_lists/nrb_bin", period, "_step_", i, ".rds"))
    harvest_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin", period, "_step_", i, ".rds"))
    
    # Combine rasters
    nrb_rasters <- do.call(c, lapply(nrb_list, rast))
    harvest_rasters <- do.call(c, lapply(harvest_list, rast))
    
    # Calculate mean, SD, and SE
    n_layers <- nlyr(nrb_rasters)  # Number of layers
    nrb_mean <- app(nrb_rasters, fun = mean)
    nrb_sd <- app(nrb_rasters, fun = sd)
    nrb_se <- nrb_sd / sqrt(n_layers)
    
    harvest_mean <- app(harvest_rasters, fun = mean)
    harvest_sd <- app(harvest_rasters, fun = sd)
    harvest_se <- harvest_sd / sqrt(n_layers)
    
    # Set values <= 0 to NA for harvest rasters
    harvest_mean[harvest_mean <= 0] <- NA
    harvest_sd[harvest_sd <= 0] <- NA
    harvest_se[harvest_se <= 0] <- NA
    
    # Save results
    writeRaster(nrb_mean, file.path(output_dir, paste0("nrb_", output_suffix, "_mean.tif")), overwrite = TRUE)
    writeRaster(nrb_sd, file.path(output_dir, paste0("nrb_", output_suffix, "_sd.tif")), overwrite = TRUE)
    writeRaster(nrb_se, file.path(output_dir, paste0("nrb_", output_suffix, "_se.tif")), overwrite = TRUE)
    
    writeRaster(harvest_mean, file.path(output_dir, paste0("harv_", output_suffix, "_mean.tif")), overwrite = TRUE)
    writeRaster(harvest_sd, file.path(output_dir, paste0("harv_", output_suffix, "_sd.tif")), overwrite = TRUE)
    writeRaster(harvest_se, file.path(output_dir, paste0("harv_", output_suffix, "_se.tif")), overwrite = TRUE)
  }
  
  # Function to calculate mean, SD, and SE for AGB rasters
  process_agb <- function(year) {
    # Load raster list
    agb_list <- readRDS(paste0("temp_raster_lists/agb_", year, "_step_", i, ".rds")) ## OJO SOLO PA COMPARAR AGB
    
    # Combine rasters
    agb_rasters <- do.call(c, lapply(agb_list, rast))
    
    # Calculate mean, SD, and SE
    n_layers <- nlyr(agb_rasters)  # Number of layers
    agb_mean <- app(agb_rasters, fun = mean)
    agb_sd <- app(agb_rasters, fun = sd)
    agb_se <- agb_sd / sqrt(n_layers)
    
    # Save results
    writeRaster(agb_mean, file.path(output_dir, paste0("agb_", year, "_mean.tif")), overwrite = TRUE)
    writeRaster(agb_sd, file.path(output_dir, paste0("agb_", year, "_sd.tif")), overwrite = TRUE)
    writeRaster(agb_se, file.path(output_dir, paste0("agb_", year, "_se.tif")), overwrite = TRUE)
  }
  
  # Process NRB and Harvest for each period
  for (j in seq_along(periods)) {
    process_nrb_harvest(periods[j], output_suffixes[j])
  }
  
  # Process AGB for each year
  for (year in agb_years) {
    process_agb(year)
  }
  
  unlink("temp_raster_lists", recursive= TRUE, force=TRUE)
  
}
