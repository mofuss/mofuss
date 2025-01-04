# MoFuSS
# Version 3
# Date: Mar 2024

# 2dolist
# Fix automatic bugs from the modis/copernicus 2 growth1 and 2 respectively at this stage to keep the process as clean as possible in the 
# pre_script during the basic construction of the tables.
# Try modis and copernicus f course and re run again from 4th script for all regions

# IF GLOBAL COPY THE TABLE FROM GLOBAL!!!

# Internal parameters
rmax_over_K_ratio <- 0.04 #This fix automatic LUC with wrong.... REVISAR K!!!
charcoal_harv_threshold <- 1 # t/ha
# Select MoFuSS platform:
webmofuss = 1 # "1" is  web-MoFuSS running in our Ubuntu server, "0" is localcal host (Windows or Linux)

# Load libraries ----
library(readxl)
library(tidyverse)
library(terra)
library(dplyr)
library(svDialogs)
library(readr)

setwd(countrydir)
getwd()
country_name

# Read parameters table ----
read.csv("LULCC/TempTables/Country.csv") %>%
  dplyr::filter(Key. == "1") %>%
  pull(Country) -> country_name

# Specify the directory where the file is located
parameters_directory <- paste0(getwd(),"/LULCC/DownloadedDatasets/SourceData",country_name)

# Use list.files() to find the file that matches the pattern
parameters_name <- list.files(path = parameters_directory, pattern = "^parameters.*\\.xlsx$", full.names = TRUE)

# Read parameters table ----
if (webmofuss == 1){
  country_parameters <- read_csv(paste0("LULCC/DownloadedDatasets/SourceData",country_name,"/",parameters_file))
} else if (webmofuss == 0){
  country_parameters <- read_excel(paste0("LULCC/DownloadedDatasets/SourceData",country_name,"/",parameters_file))
}
print(tibble::as_tibble(country_parameters), n=100)

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
setwd(countrydir)

# Prepare the rural urban mask ----
if (lucinputdataset == "modis") {
  
  country_parameters %>%
    dplyr::filter(Var == "LULCt1map_name") %>%
    pull(ParCHR) -> LULCt1map_name
  country_parameters %>%
    dplyr::filter(Var == "LULCt1map_yr") %>%
    pull(ParCHR) -> LULCt1map_yr_pre
  clean_string1 <- gsub("c\\(|\\)", "", LULCt1map_yr_pre)
  string_numbers1 <- strsplit(clean_string1, ",")[[1]]
  LULCt1map_yr <- as.numeric(string_numbers1)
  lucmodis_2010_merge_rcl <- rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/pre2010_v1_",LULCt1map_name))

  rururb_gcs <- rast(paste0(demanddir,"/pop_out/WorldPop_rururb_2020.tif"))
  rururb_pcs <- rururb_gcs %>% 
    terra::project(lucmodis_2010_merge_rcl, method="near", gdal=TRUE)
  # terra::writeRaster(rururb_pcs, paste0(lulccfiles,"/out_pcs/rururb_pcs.tif"), filetype = "GTiff", datatype="INT2S", overwrite = TRUE)
  
  # Reads growth parameters correctly
  # Define the file path
  file_pathm <- paste0(countrydir, "/LULCC/DownloadedDatasets/SourceData", country_name, "/InTables/growth_parameters_v3_modis.csv")
  
  # Check the first line of the file to determine the delimiter
  first_linem <- readLines(file_pathm, n = 1)
  
  # Determine the delimiter based on the first line
  delimiterm <- ifelse(grepl(";", first_linem), ";", ",")
  
  # Read the CSV file with the appropriate delimiter and fix out of bound rmax/K and K below harvestable thresholds (optional)
  growth_parameters_v3_modis <- read_delim(file_pathm, delim = delimiterm) %>%
    mutate(
      K = if_else(K <= 1, (charcoal_harv_threshold + 0.1), K),  # Transform K values
      condition = rmax / K > rmax_over_K_ratio,  # Calculate the condition once
      rmax = if_else(condition, 0, rmax),
      rmaxSD = if_else(condition, 0, rmaxSD),
      TOF = if_else(condition, 1, TOF)
    ) %>%
    dplyr::select(-condition)  # Remove the temporary condition column
  
  lastid <- nrow(growth_parameters_v3_modis)+1
  rururb_rcl <- data.frame(c(1,2),c(NA,lastid)) %>%
    as.matrix(.,nrow = 2, ncol = 2) %>%
    unname()
  rururb_pcs_rcl <- rururb_pcs %>%
    terra::classify(rururb_rcl, include.lowest = FALSE, right = NA)
    # terra::writeRaster(rururb_pcs_rcl, paste0(lulccfiles,"/out_pcs/rururb_rcl.tif"),
    #                  filetype = "GTiff", overwrite = TRUE)
  
  mask_urbanforced <- !is.na(rururb_pcs_rcl)
  lucmodis_2010_final <- ifel(mask_urbanforced, rururb_pcs_rcl, lucmodis_2010_merge_rcl)
  
  # terra::writeRaster(lucmodis_2010_final, paste0(lulccfiles,"/out_pcs/rururb_rcl2.tif"),
  #                    filetype = "GTiff", overwrite = TRUE)
  terra::writeRaster(lucmodis_2010_final, paste0(countrydir,"/LULCC/SourceData/InRaster/",LULCt1map_name), 
                     filetype = "GTiff", overwrite = TRUE)
  
  growth_parameters_v4 <- growth_parameters_v3_modis %>%
    add_row(tibble_row(`Key*`=lastid, LULC="Urban_Forced", rmax=0, rmaxSD=0, K=10, KSD=10, TOF=1))
  str(growth_parameters_v4)
  write.csv(growth_parameters_v4, paste0(countrydir,"/LULCC/SourceData/InTables/growth_parameters1.csv"), row.names=FALSE, quote=FALSE)
  write.csv(growth_parameters_v4, paste0(countrydir,"/LULCC/TempTables/growth_parameters1.csv"), row.names=FALSE, quote=FALSE)
  tail(growth_parameters_v4)
  
  } else if (lucinputdataset == "copernicus") {
    
  country_parameters %>%
    dplyr::filter(Var == "LULCt2map_name") %>%
    pull(ParCHR) -> LULCt2map_name
  country_parameters %>%
    dplyr::filter(Var == "LULCt2map_yr") %>%
    pull(ParCHR) -> LULCt2map_yr_pre
  clean_string2 <- gsub("c\\(|\\)", "", LULCt2map_yr_pre)
  string_numbers2 <- strsplit(clean_string2, ",")[[1]]
  LULCt2map_yr <- as.numeric(string_numbers2)
  luccopernicus_2015_merge_rcl <- rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/pre2015_v1_",LULCt2map_name))

  rururb_gcs <- rast(paste0(demanddir,"/pop_out/WorldPop_rururb_2020.tif"))
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
  
  # Read the CSV file with the appropriate delimiter and fix out of bound rmax/K and K below harvestable thresholds (optional)
  growth_parameters_v3_copernicus <- read_delim(file_pathc, delim = delimiterc) %>%
    mutate(
      K = if_else(K <= 1, (charcoal_harv_threshold + 0.1), K),  # Transform K values
      condition = rmax / K > rmax_over_K_ratio,  # Calculate the condition once
      rmax = if_else(condition, 0, rmax),
      rmaxSD = if_else(condition, 0, rmaxSD),
      TOF = if_else(condition, 1, TOF)
    ) %>%
    dplyr::select(-condition)  # Remove the temporary condition column
  
  lastid <- nrow(growth_parameters_v3_copernicus)+1
  rururb_rcl <- data.frame(c(1,2),c(NA,lastid)) %>%
    as.matrix(.,nrow = 2, ncol = 2) %>%
    unname()
  rururb_pcs_rcl <- rururb_pcs %>%
    terra::classify(rururb_rcl, include.lowest = FALSE, right = NA)
  # terra::writeRaster(rururb_pcs_rcl, paste0(lulccfiles,"/out_pcs/rururb_rcl.tif"),
  #                    filetype = "GTiff", overwrite = TRUE)
  
  mask_urbanforced <- !is.na(rururb_pcs_rcl)
  luccopernicus_2010_final <- ifel(mask_urbanforced, rururb_pcs_rcl, luccopernicus_2015_merge_rcl)
  
  # terra::writeRaster(luccopernicus_2010_final, paste0(lulccfiles,"/out_pcs/rururb_rcl2.tif"),
  #                    filetype = "GTiff", overwrite = TRUE)
  terra::writeRaster(luccopernicus_2010_final, paste0(countrydir,"/LULCC/SourceData/InRaster/",LULCt2map_name), # Double check in harmonizer
                     filetype = "GTiff", overwrite = TRUE)
  
  growth_parameters_v4 <- growth_parameters_v3_copernicus %>%
    add_row(tibble_row(`Key*`=lastid, LULC="Urban_Forced", rmax=0, rmaxSD=0, K=10, KSD=10, TOF=1))
  str(growth_parameters_v4)
  write.csv(growth_parameters_v4, paste0(countrydir,"/LULCC/SourceData/InTables/growth_parameters2.csv"), row.names=FALSE, quote=FALSE)
  write.csv(growth_parameters_v4, paste0(countrydir,"/LULCC/TempTables/growth_parameters2.csv"), row.names=FALSE, quote=FALSE)
  tail(growth_parameters_v4)
  
  }

}
