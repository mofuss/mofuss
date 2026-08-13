# Copyright 2027 Stockholm Environment Institute ----

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
# Date: Aug 2026

# Preserves the growth and TOF parameters produced by 7pre_lulcc_v6.R.
# It validates those values instead of silently converting low-K/non-forest
# rows to TOF or overwriting their K values.

# Internal parameters ----
temdirdefined = 1
forced_urban_supply <- 2
forced_urban_supply_sd <- forced_urban_supply
# # Select MoFuSS platform:
# webmofuss = 1 # "1" is  web-MoFuSS running in our Ubuntu server, "0" is localcal host (Windows or Linux)


# source(paste0(scriptsmofuss,"00_webmofuss.R"))

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
library(readr)
library(readxl)
library(svDialogs)
library(tidyverse)

validate_growth_parameters <- function(data, label) {
  required <- c("Key*", "LULC", "rmax", "rmaxSD", "K", "KSD", "TOF")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(label, " is missing columns: ", paste(missing, collapse = ", "))
  }
  numeric_columns <- c("Key*", "rmax", "rmaxSD", "K", "KSD", "TOF")
  non_numeric <- numeric_columns[!vapply(
    data[numeric_columns], is.numeric, logical(1)
  )]
  if (length(non_numeric) > 0) {
    stop(label, " has non-numeric columns: ", paste(non_numeric, collapse = ", "))
  }
  invalid <- data %>%
    dplyr::filter(
      !is.finite(`Key*`) | !is.finite(rmax) | !is.finite(rmaxSD) |
        !is.finite(K) | !is.finite(KSD) | !is.finite(TOF) |
        rmax < 0 | rmaxSD < 0 | KSD < 0 |
        !TOF %in% c(0, 1) |
        (TOF == 0 & K <= 0) | (TOF == 1 & K < 0) |
        (TOF == 1 & (rmax != 0 | rmaxSD != 0))
    )
  if (anyDuplicated(data$`Key*`) || nrow(invalid) > 0) {
    stop(
      label, " contains duplicate keys or invalid growth/TOF parameters."
    )
  }
  invisible(data)
}

setwd(countrydir)
getwd()
country_name

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

# # Specify the directory where the file is located
# parameters_directory <- paste0(getwd(),"/LULCC/DownloadedDatasets/SourceData",country_name)
# 
# # Use list.files() to find the file that matches the pattern
# parameters_name <- list.files(path = parameters_directory, pattern = "^parameters.*\\.xlsx$", full.names = TRUE)
# 
# # Read parameters table ----
# if (webmofuss == 1){
#   country_parameters <- read_csv(paste0("LULCC/DownloadedDatasets/SourceData",country_name,"/",parameters_file))
# } else if (webmofuss == 0){
#   country_parameters <- read_excel(paste0("LULCC/DownloadedDatasets/SourceData",country_name,"/",parameters_file))
# }

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
  dplyr::filter(Var == "LULCt1map") %>%
  pull(ParCHR) -> LULCt1map

country_parameters %>%
  dplyr::filter(Var == "LULCt2map") %>%
  pull(ParCHR) -> LULCt2map

# if (exists("lulccfiles") == FALSE) {
#   choose_directory661 = function(caption = "Choose the directory where land use/cover files are") {
#     if(.Platform$OS.type == "unix")  {
#       setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
#     } else {
#       setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
#     }
#   }
# choose_directory661()
# lulccfiles <- getwd()
# }

# Copy 2 MoFuSS ----
# copy2mofussfiles1 <- list.files(path = paste0(lulccfiles,"/out_gcs/"),
#                                 pattern = ".*\\.tif$", full.names = TRUE)
# for (f1 in copy2mofussfiles1) {
#   file.copy(from=f1,
#             to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster_GCS/"),
#             overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
# }
# 
# copy2mofussfiles2 <- list.files(path = paste0(lulccfiles,"/out_pcs/"),
#                                 pattern = ".*\\.tif$", full.names = TRUE)
# for (f2 in copy2mofussfiles2) {
#   file.copy(from=f2,
#             to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/"),
#             overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
# }
# 
# copy2mofussfiles3 <- list.files(path = paste0(lulccfiles,"/out_pcs/"),
#                                 pattern = ".*\\.csv$", full.names = TRUE)
# for (f3 in copy2mofussfiles3) {
#   file.copy(from=f3,
#             to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables/"),
#             overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
# }

if (LULCt1map == "YES" & LULCt2map == "YES"){
  lucavailablemaps <- c("modis", "copernicus")
} else if (LULCt1map == "YES" & LULCt2map != "YES"){
  lucavailablemaps <- c("modis")
} else if (LULCt1map != "YES" & LULCt2map == "YES"){
  lucavailablemaps <- c("copernicus")
}
lucavailablemaps

for (lucinputdataset in lucavailablemaps) {
# lucinputdataset = "copernicus"
# lucinputdataset = "modis"
setwd(countrydir)

# Prepare the rural urban mask ----
if (lucinputdataset == "modis") {
  
  country_parameters %>%
    dplyr::filter(Var == "LULCt1map_name") %>%
    pull(ParCHR) -> LULCt1map_name
  country_parameters %>%
    dplyr::filter(Var == "LULCt1map_yr") %>%
    pull(ParCHR) %>%
    as.integer(.) -> LULCt1map_yr
  lucmodis_2001_merge_rcl <- rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/pre",LULCt1map_yr,"_v1_",LULCt1map_name))

  rururb_gcs <- rast(paste0(demanddir,"/pop_out/WorldPop_rururbR_2020.tif"))
  rururb_pcs <- rururb_gcs %>% 
    terra::project(lucmodis_2001_merge_rcl, method="near", gdal=TRUE)
  # terra::writeRaster(rururb_pcs, paste0(lulccfiles,"/out_pcs/rururb_pcs.tif"), filetype = "GTiff", datatype="INT2S", overwrite = TRUE)
  
  # Reads growth parameters correctly
  # Define the file path
  file_pathm <- paste0(countrydir, "/LULCC/DownloadedDatasets/SourceData", country_name, "/InTables/growth_parameters_v3_modis.csv")
  
  # Check the first line of the file to determine the delimiter
  first_linem <- readLines(file_pathm, n = 1)
  
  # Determine the delimiter based on the first line
  delimiterm <- ifelse(grepl(";", first_linem), ";", ",")
  
  # Read and validate without changing parameters produced upstream.
  growth_parameters_v3_modis <- read_delim(file_pathm, delim = delimiterm)
  validate_growth_parameters(
    growth_parameters_v3_modis, "MODIS growth-parameter table"
  )
  
  lastid <- nrow(growth_parameters_v3_modis)+1
  rururb_rcl <- data.frame(c(1,2),c(NA,lastid)) %>%
    as.matrix(.,nrow = 2, ncol = 2) %>%
    unname()
  rururb_pcs_rcl <- rururb_pcs %>%
    terra::classify(rururb_rcl, include.lowest = FALSE, right = NA)
    # terra::writeRaster(rururb_pcs_rcl, paste0(lulccfiles,"/out_pcs/rururb_rcl.tif"),
    #                  filetype = "GTiff", overwrite = TRUE)
  
  mask_urbanforced <- !is.na(rururb_pcs_rcl)
  lucmodis_2001_final <- ifel(mask_urbanforced, rururb_pcs_rcl, lucmodis_2001_merge_rcl)
  
  # terra::writeRaster(lucmodis_2010_final, paste0(lulccfiles,"/out_pcs/rururb_rcl2.tif"),
  #                    filetype = "GTiff", overwrite = TRUE)
  terra::writeRaster(lucmodis_2001_final, paste0(countrydir,"/LULCC/DownloadedDatasets/SourceDataGlobal/InRaster/",LULCt1map_name), 
                     filetype = "GTiff", overwrite = TRUE)
  
  growth_parameters_v4 <- growth_parameters_v3_modis %>%
    add_row(tibble_row(
      `Key*` = lastid, LULC = "Urban_Forced", rmax = 0, rmaxSD = 0,
      K = forced_urban_supply, KSD = forced_urban_supply_sd, TOF = 1
    ))
  str(growth_parameters_v4)
  write.csv(growth_parameters_v4, paste0(countrydir,"/LULCC/DownloadedDatasets/SourceDataGlobal/InTables/growth_parameters1.csv"), row.names=FALSE, quote=FALSE)
  write.csv(growth_parameters_v4, paste0(countrydir,"/LULCC/TempTables/growth_parameters1.csv"), row.names=FALSE, quote=FALSE)
  tail(growth_parameters_v4)
  
  } else if (lucinputdataset == "copernicus") {
    
  country_parameters %>%
    dplyr::filter(Var == "LULCt2map_name") %>%
    pull(ParCHR) -> LULCt2map_name
  country_parameters %>%
      dplyr::filter(Var == "LULCt2map_yr") %>%
      pull(ParCHR) %>%
      as.integer(.) -> LULCt2map_yr
  luccopernicus_2015_merge_rcl <- rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/pre",LULCt2map_yr,"_v1_",LULCt2map_name))

  rururb_gcs <- rast(paste0(demanddir,"/pop_out/WorldPop_rururbR_2020.tif"))
  rururb_pcs <- rururb_gcs %>% 
    terra::project(luccopernicus_2015_merge_rcl, method="near", gdal=TRUE)
  # terra::writeRaster(rururb_pcs, paste0(lulccfiles,"/out_pcs/rururb_pcs.tif"), filetype = "GTiff", datatype="INT2S", overwrite = TRUE)
  
  # Reads growth parameters correctly
  # Define the file path
  file_pathc <- paste0(countrydir, "/LULCC/DownloadedDatasets/SourceData", country_name, "/InTables/growth_parameters_v3_copernicus.csv")
  
  # Check the first line of the file to determine the delimiter
  first_linec <- readLines(file_pathc, n = 1)
  
  # Determine the delimiter based on the first line
  delimiterc <- ifelse(grepl(";", first_linec), ";", ",")
  
  # Read and validate without changing parameters produced upstream.
  growth_parameters_v3_copernicus <- read_delim(file_pathc, delim = delimiterc)
  validate_growth_parameters(
    growth_parameters_v3_copernicus, "Copernicus growth-parameter table"
  )
  
  lastid <- nrow(growth_parameters_v3_copernicus)+1
  rururb_rcl <- data.frame(c(1,2),c(NA,lastid)) %>%
    as.matrix(.,nrow = 2, ncol = 2) %>%
    unname()
  rururb_pcs_rcl <- rururb_pcs %>%
    terra::classify(rururb_rcl, include.lowest = FALSE, right = NA)
  # terra::writeRaster(rururb_pcs_rcl, paste0(lulccfiles,"/out_pcs/rururb_rcl.tif"),
  #                    filetype = "GTiff", overwrite = TRUE)
  
  mask_urbanforced <- !is.na(rururb_pcs_rcl)
  luccopernicus_2015_final <- ifel(mask_urbanforced, rururb_pcs_rcl, luccopernicus_2015_merge_rcl)
  
  # terra::writeRaster(luccopernicus_2010_final, paste0(lulccfiles,"/out_pcs/rururb_rcl2.tif"),
  #                    filetype = "GTiff", overwrite = TRUE)
  terra::writeRaster(luccopernicus_2015_final, paste0(countrydir,"/LULCC/DownloadedDatasets/SourceDataGlobal/InRaster/",LULCt2map_name), # Double check in harmonizer
                     filetype = "GTiff", overwrite = TRUE)
  
  growth_parameters_v4 <- growth_parameters_v3_copernicus %>%
    add_row(tibble_row(
      `Key*` = lastid, LULC = "Urban_Forced", rmax = 0, rmaxSD = 0,
      K = forced_urban_supply, KSD = forced_urban_supply_sd, TOF = 1
    ))
  str(growth_parameters_v4)
  write.csv(growth_parameters_v4, paste0(countrydir,"/LULCC/DownloadedDatasets/SourceDataGlobal/InTables/growth_parameters2.csv"), row.names=FALSE, quote=FALSE)
  write.csv(growth_parameters_v4, paste0(countrydir,"/LULCC/TempTables/growth_parameters2.csv"), row.names=FALSE, quote=FALSE)
  tail(growth_parameters_v4)
  
  }

}

# End of script ----
