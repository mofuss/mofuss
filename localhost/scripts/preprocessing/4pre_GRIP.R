# MoFuSS
# Version 3
# Date: Mar 2024

# 2dolist

# Internal parameters

# Load packages ----
library(sf)
library(tictoc)
library(mapview)
library(tidyverse)
library(readxl)
library(hacksaw)
library(rmapshaper)
library(terra)

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

if (exists("gripdir") == FALSE) {
choose_directory11 = function(caption = "Choose the directory where the GRIP original files are located") {
  if(.Platform$OS.type == "unix")  {
    setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
  } else {
    setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
  }
}
choose_directory11()
gripdir <- getwd()
}

setwd(gripdir)

unlink("temp/", recursive = TRUE) # Warning, slow geoprocessing times
unlink("out_gcs/", recursive = TRUE) # Warning, slow geoprocessing times
unlink("out_pcs/", recursive = TRUE) # Warning, slow geoprocessing times
Sys.sleep(5)
if (!dir.exists("temp")) {dir.create("temp")}
if (!dir.exists("out_gcs")) {dir.create("out_gcs")}
if (!dir.exists("out_pcs")) {dir.create("out_pcs")}

grip2 <- st_read("GRIP4_region2.shp")
# st_write(sf_grip2_summarized, "temp/grip2_D.shp", delete_layer=TRUE)

grip3 <- st_read("GRIP4_region3.shp") %>%
  dplyr::select(-OBJECTID)
# st_write(sf_grip3_summarized, "temp/grip3_D.shp", delete_layer=TRUE)

grip5 <- st_read("GRIP4_region5_clip.shp") %>%
  dplyr::select(-OBJECTID, -Shape_Le_1)
# st_write(sf_grip5_summarized, "temp/grip5_D.shp", delete_layer=TRUE)

grip6 <- st_read("GRIP4_region6.shp")
# st_write(sf_grip6_summarized, "temp/grip6_D.shp", delete_layer=TRUE)

country_parameters %>%
  dplyr::filter(Var == "roads_fieldname") %>%
  pull(ParCHR) -> roads_fieldname

grip_summarized <- rbind(grip2, grip3, grip5, grip6) %>%
  group_by_at(roads_fieldname) %>% # Note is group_by_at in order to pass a variable name to dplyr's group_by()
  summarize()
# st_write(grip_summarized, "temp/grip_summarized.shp", delete_layer = TRUE)

# Project
grip_summarized_p <- grip_summarized %>%
  st_transform(paste0(proj_authority,":",epsg_pcs))
st_write(grip_summarized_p, "temp/grip5_pcs.shp", delete_layer=TRUE)
st_write(grip_summarized_p, "out_pcs/grip5_pcs.gpkg", delete_layer=TRUE)

# Rasterize
DTEM_gcs <- terra::rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster_GCS/DTEM_gcs.tif"))
DTEM_pcs <- terra::rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/DTEM_pcs.tif"))

grip_summarizedr <-terra::rasterize(terra::vect(grip_summarized), DTEM_gcs, roads_fieldname)
terra::writeRaster(grip_summarizedr, "out_gcs/grip5_gcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

grip_summarizedr_p <-terra::rasterize(terra::vect(grip_summarized_p), DTEM_pcs, roads_fieldname)
terra::writeRaster(grip_summarizedr_p, "out_pcs/grip5_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

# Copy 2 MoFuSS ----
copy2mofussfiles <- list.files(path = paste0(gripdir,"/out_pcs/"),
                               pattern = ".*\\.tif$", full.names = TRUE)
for (f in copy2mofussfiles) {
  file.copy(from=f, 
            to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

