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
library(readxl)

setwd(countrydir)
getwd()
country_name

# Read parameters table ----
country_parameters <- read_excel(paste0("LULCC/DownloadedDatasets/SourceData",country_name,"/",parameters_file))
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

if (exists("borderdir") == FALSE) {
# Select directory were grid original files are
choose_directory15 = function(caption = "Choose the directory where the borders original files are located") {
  if(.Platform$OS.type == "unix")  {
    setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
  } else {
    setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
  }
}
choose_directory15()
borderdir <- getwd()
}

setwd(borderdir)

unlink("temp/", recursive = TRUE) # Warning, slow geoprocessing times
unlink("out_gcs/", recursive = TRUE) # Warning, slow geoprocessing times
unlink("out_pcs/", recursive = TRUE) # Warning, slow geoprocessing times
Sys.sleep(5)
if (!dir.exists("temp")) {dir.create("temp")}
if (!dir.exists("out_gcs")) {dir.create("out_gcs")}
if (!dir.exists("out_pcs")) {dir.create("out_pcs")}

# Borders ----
country_parameters %>%
  dplyr::filter(Var == "borders_fieldname") %>%
  pull(ParCHR) -> borders_fieldname

borders <- st_read(paste0(admindir,"/regions_adm0/mofuss_regions0.gpkg")) %>%
  dplyr::mutate(dissolveID = 1) %>%
  st_cast("MULTILINESTRING")
# borders_l <-  st_cast(borders,"MULTILINESTRING")
st_write(borders, "temp/borders_gcs.shp", delete_layer=TRUE)
st_write(borders, "out_gcs/borders_gcs.gpkg", delete_layer=TRUE)

# Project
borders_p <- borders %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_simplify(dTolerance = 1000) %>% 
  st_buffer(dist = 1000)

st_write(borders_p, "temp/borders_pcs.shp", delete_layer=TRUE)
st_write(borders_p, "out_pcs/borders_pcs.gpkg", delete_layer=TRUE)

# Rasterize
DTEM_gcs <- rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster_GCS/DTEM_gcs.tif"))
DTEM_pcs <- rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/DTEM_pcs.tif"))

bordersr <-terra::rasterize(vect(borders), DTEM_gcs, borders_fieldname)
terra::writeRaster(bordersr, "out_gcs/borders_gcs.tif",
                   filetype = "GTiff", overwrite = TRUE) # It vreaks unless you restart R studuo

borders_pr <-terra::rasterize(vect(borders_p), DTEM_pcs, borders_fieldname)
terra::writeRaster(borders_pr, "out_pcs/borders_pcs.tif", # Watch out with the name here, choose projection wisely before
                   filetype = "GTiff", overwrite = TRUE)

# Copy 2 MoFuSS ----
copy2mofussfiles <- list.files(path = paste0(borderdir,"/out_pcs/"),
                               pattern = ".*\\.tif$", full.names = TRUE)
for (f in copy2mofussfiles) {
  file.copy(from=f, 
            to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}
