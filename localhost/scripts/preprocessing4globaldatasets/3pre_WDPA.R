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

# Select directory were grid original files are
choose_directory12 = function(caption = "Choose the directory where the WDPA original files are located") {
  if(.Platform$OS.type == "unix")  {
    setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
  } else {
    setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
  }
}
choose_directory12()
wdpadir <- getwd()

setwd(wdpadir)

unlink("temp/", recursive = TRUE) # Warning, slow geoprocessing times
unlink("out_gcs/", recursive = TRUE) # Warning, slow geoprocessing times
unlink("out_pcs/", recursive = TRUE) # Warning, slow geoprocessing times
Sys.sleep(5)
if (!dir.exists("temp")) {dir.create("temp")}
if (!dir.exists("out_gcs")) {dir.create("out_gcs")}
if (!dir.exists("out_pcs")) {dir.create("out_pcs")}

npa <- st_read("WDPA_Apr2023_Public_shp-polygons.shp")
st_write(npa, "out_gcs/npa_gcs.gpkg", delete_layer=TRUE)

country_parameters %>%
  dplyr::filter(Var == "npa_name") %>%
  pull(ParCHR) -> npa_name
npa_p <- npa %>%
  st_transform(paste0(proj_authority,":",epsg_pcs))
st_write(npa_p, paste0("out_pcs/",npa_name), delete_layer=TRUE)
# terra::crs(npa)
# terra::crs(npa_p)

# Rasterize
DTEM_gcs <- rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster_GCS/DTEM_gcs.tif"))
DTEM_pcs <- rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/DTEM_pcs.tif"))

country_parameters %>%
  dplyr::filter(Var == "npa_fieldname") %>%
  pull(ParCHR) -> npa_fieldname

npar <-terra::rasterize(vect(npa), DTEM_gcs, npa_fieldname)
terra::writeRaster(npar, "out_gcs/npa_gcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

npar_p <-terra::rasterize(vect(npa_p), DTEM_pcs, npa_fieldname)
terra::writeRaster(npar_p, "out_pcs/npa_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

# Copy 2 MoFuSS ----
copy2mofussfiles <- list.files(path = paste0(wdpadir,"/out_pcs/"),
                               pattern = ".*\\.tif$", full.names = TRUE)
for (f in copy2mofussfiles) {
  file.copy(from=f, 
            to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

