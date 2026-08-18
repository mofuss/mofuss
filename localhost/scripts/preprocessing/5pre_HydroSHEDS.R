# MoFuSS
# Version 3
# Date: Mar 2024

# 2dolist

# Internal parameters

# Load packages ----
library(terra)
library(sf)
library(tidyverse)
library(tictoc)

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

if (exists("hydrodir") == FALSE) {
choose_directory15 = function(caption = "Choose the directory where the HydroSHEDS original files are located") {
  if(.Platform$OS.type == "unix")  {
    setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
  } else {
    setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
  }
}
choose_directory15()
hydrodir <- getwd()
}

setwd(hydrodir)

unlink("temp/", recursive = TRUE) # Warning, slow geoprocessing times
unlink("out_gcs/", recursive = TRUE) # Warning, slow geoprocessing times
unlink("out_pcs/", recursive = TRUE) # Warning, slow geoprocessing times
Sys.sleep(5)
if (!dir.exists("temp")) {dir.create("temp")}
if (!dir.exists("out_gcs")) {dir.create("out_gcs")}
if (!dir.exists("out_pcs")) {dir.create("out_pcs")}

# Rivers ----
country_parameters %>%
  dplyr::filter(Var == "rivers_fieldname") %>%
  pull(ParCHR) -> rivers_fieldname

hydrorivers <- st_read("HydroRIVERS_v10_shp/hydrorivers_7_clip.shp")
st_write(hydrorivers, "out_gcs/hydrorivers7_gcs.gpkg", delete_layer=TRUE)

# Project
hydrorivers_p <- hydrorivers %>%
  st_transform(paste0(proj_authority,":",epsg_pcs))
st_write(hydrorivers_p, "temp/hydrorivers7_pcs.shp", delete_layer=TRUE)
st_write(hydrorivers_p, "out_pcs/hydrorivers7_pcs.gpkg", delete_layer=TRUE)

# Rasterize
DTEM_gcs <- rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster_GCS/DTEM_gcs.tif"))
DTEM_pcs <- rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/DTEM_pcs.tif"))

hydroriversr <-terra::rasterize(vect(hydrorivers), DTEM_gcs, rivers_fieldname)
terra::writeRaster(hydroriversr, "out_gcs/hydrorivers7_gcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

hydrorivers_pr <-terra::rasterize(vect(hydrorivers_p), DTEM_pcs, rivers_fieldname)
terra::writeRaster(hydrorivers_pr, "out_pcs/hydrorivers7_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

# Lakes ----
country_parameters %>%
  dplyr::filter(Var == "lakes_fieldname") %>%
  pull(ParCHR) -> lakes_fieldname

hydrolakes <- st_read("HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10_clip.shp") %>%
  dplyr::mutate(dissolveID = 1)
st_write(hydrolakes, "out_gcs/hydrolakes_gcs.gpkg", delete_layer=TRUE)

# Project
hydrolakes_p <- hydrolakes %>%
  st_transform(paste0(proj_authority,":",epsg_pcs))
st_write(hydrolakes_p, "temp/hydrolakes_pcs.shp", delete_layer=TRUE)
st_write(hydrolakes_p, "out_pcs/hydrolakes_pcs.gpkg", delete_layer=TRUE)

# Rasterize
hydrolakesr <-terra::rasterize(vect(hydrolakes), DTEM_gcs, lakes_fieldname)
terra::writeRaster(hydrolakesr, "out_gcs/hydrolakes_gcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

hydrolakes_pr <-terra::rasterize(vect(hydrolakes_p), DTEM_pcs, lakes_fieldname)
terra::writeRaster(hydrolakes_pr, "out_pcs/hydrolakes_pcs.tif", # Watch out with the name here, choose projection wisely before
                   filetype = "GTiff", overwrite = TRUE)


# Copy 2 MoFuSS ----
copy2mofussfiles <- list.files(path = paste0(hydrodir,"/out_pcs/"),
                               pattern = ".*\\.tif$", full.names = TRUE)
for (f in copy2mofussfiles) {
  file.copy(from=f, 
            to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}


