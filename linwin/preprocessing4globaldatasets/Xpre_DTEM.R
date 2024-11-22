# MoFuSS
# Version 3
# Date: Dec 2023

# Load packages ####
library(readr)
library(terra)
library(sf)
library(tidyverse)
library(tictoc)

setwd(countrydir)
getwd()

# Read parameters table, checking if its delimiter is comma or semicolon ####
read_csv("LULCC/DownloadedDatasets/SourceDataGlobal/parameters.csv") %>% 
  {if(is.null(.$ParCHR[1])) read_csv2("LULCC/SourceData/parameters.csv") else .} -> country_parameters
# Append "+"
country_parameters$ParCHR[1] <- str_c("+", country_parameters$ParCHR[1])
country_parameters$ParCHR[2] <- str_c("+", country_parameters$ParCHR[2])
print(tbl_df(country_parameters), n=100) ####

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

choose_directory13 = function(caption = "Choose the directory where the DTEM original files are located") {
  if(.Platform$OS.type == "unix")  {
    setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
  } else {
    setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
  }
}
choose_directory13()
demdir <- getwd()

# unlink("out_gcs/*", recursive = TRUE) # Warning, slow geoprocessing times
# unlink("out_pcs/*", recursive = TRUE) # Warning, slow geoprocessing times
unlink("temp/*", recursive = TRUE) # Warning, slow geoprocessing times
if (!dir.exists("temp")) {dir.create("temp")}
if (!dir.exists("out_gcs")) {dir.create("out_gcs")}
if (!dir.exists("out_pcs")) {dir.create("out_pcs")}

DTEM_gcs <- rast("out_gcs/DTEM_gcs.tif") # REPLACE FROM GEE CODE STARTING FROM DOWNLOADED DATASETS

# Set warnings until terra:project works ok
oo <- options(nwarnings=1000)
options()$nwarnings
# options(oo)
# options()$nwarnings

country_parameters %>%
  dplyr::filter(Var == "DTEM_name") %>%
  pull(ParCHR) -> DTEM_name
crs_r <- paste0("+",proj_pcs) # Doblecheck down the river in harmonizer codes how CRS is pasted
DTEM_p <- DTEM_gcs %>% # Mover a una carpeta aparte, igual con mofuss regions
  terra::project(crs_r, method="bilinear", res=1000, mask=TRUE) #, threads=TRUE) 
terra::writeRaster(DTEM_p, paste0("out_pcs/",DTEM_name),
                   filetype = "GTiff", overwrite = TRUE)

# Copy 2 MoFuSS ----
copy2mofussfiles <- list.files(path = paste0(demdir,"/out_pcs/"),
                                         pattern = ".*\\.tif$", full.names = TRUE)
for (f in copy2mofussfiles) {
  file.copy(from=f, 
            to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceDataGlobal/InRaster/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

