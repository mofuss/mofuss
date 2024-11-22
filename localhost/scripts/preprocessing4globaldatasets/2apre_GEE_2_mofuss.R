# MoFuSS
# Version 3
# Date: Mar 2024

# 2dolist
# Add instructions on how to run GEE script first!
# https://code.earthengine.google.com/624d0ec9d48c52791c6dda4b5c656532
# Watch out for 3rd party biomass and bulk download using wget - add instructions adrian

# Internal parameters

# Load packages ----
library(readr)
library(terra)
library(sf)
library(tidyverse)
library(tictoc)

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

country_parameters %>%
  dplyr::filter(Var == "GEE_tyRoi") %>%
  pull(ParCHR) -> GEE_tyRoi

country_parameters %>%
  dplyr::filter(Var == "GEE_country") %>%
  pull(ParCHR) -> GEE_country

country_parameters %>%
  dplyr::filter(Var == "GEE_scale") %>%
  pull(ParCHR) %>%
  as.integer(.) -> GEE_scale

if (GEE_tyRoi == "world"){
  GEE_ext <- "world"
} else if (GEE_tyRoi == "regions"){
  GEE_ext <- "regions"
} else if (GEE_tyRoi == "countries"){
  GEE_ext <- GEE_country
}
GEE_ext

if (exists("geedir") == FALSE) {
  choose_directory21 = function(caption = "Choose the directory where GEE files are") {
    if(.Platform$OS.type == "unix")  {
      setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
    } else {
      setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
    }
  }
  choose_directory21()
  geedir <- getwd()
}

unlink("temp/", recursive = TRUE) # Warning, slow geoprocessing times
unlink("out_gcs/", recursive = TRUE) # Warning, slow geoprocessing times
unlink("out_pcs/", recursive = TRUE) # Warning, slow geoprocessing times
Sys.sleep(5)
if (!dir.exists("temp")) {dir.create("temp")}
if (!dir.exists("out_gcs")) {dir.create("out_gcs")}
if (!dir.exists("out_pcs")) {dir.create("out_pcs")}

if (exists("demdir") == FALSE) {
  choose_directory13 = function(caption = "Choose the directory where the DTEM files will be saved for rastering") {
    if(.Platform$OS.type == "unix")  {
      setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
    } else {
      setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
    }
  }
  choose_directory13()
  demdir <- getwd()
}

unlink("temp/", recursive = TRUE) # Warning, slow geoprocessing times
unlink("out_gcs/", recursive = TRUE) # Warning, slow geoprocessing times
unlink("out_pcs/", recursive = TRUE) # Warning, slow geoprocessing times
Sys.sleep(5)
if (!dir.exists("temp")) {dir.create("temp")}
if (!dir.exists("out_gcs")) {dir.create("out_gcs")}
if (!dir.exists("out_pcs")) {dir.create("out_pcs")}

setwd(geedir)

# DTEM ----
if (GEE_scale == 1000 & GEE_tyRoi == "world"){
  detoeste <- rast(paste0("DEM_SRTM_",GEE_ext,"_1000scale-0000000000-0000000000.tif"))
  deteste <- rast(paste0("DEM_SRTM_",GEE_ext,"_1000scale-0000000000-0000032768.tif"))
  DTEM <- merge(detoeste,deteste)
} else if (GEE_scale == 100 & GEE_tyRoi == "countries") { # For single countries at 100m
  DTEM <- rast(paste0("DEM_SRTM_",GEE_tyRoi,"_",GEE_country,"_",GEE_scale,"scale.tif"))
} else if (GEE_scale == 1000 & GEE_tyRoi == "countries") { # For single countries at 1km
  DTEM <- rast(paste0("DEM_SRTM_",GEE_tyRoi,"_",GEE_country,"_",GEE_scale,"scale.tif"))
}
DTEM_gcs <- setMinMax(DTEM, force=TRUE)
terra::writeRaster(DTEM_gcs, "out_gcs/DTEM_gcs.tif", filetype = "GTiff", overwrite = TRUE)
crs_r <- paste0("+",proj_pcs) 
DTEM_pcs <- DTEM_gcs %>% 
  terra::project(crs_r, method="bilinear", gdal=TRUE, res=GEE_scale)
country_parameters %>%
  dplyr::filter(Var == "DTEM_name") %>%
  pull(ParCHR) -> DTEM_name
terra::writeRaster(DTEM_pcs, paste0("out_pcs/",DTEM_name), filetype = "GTiff", datatype="INT2S", overwrite = TRUE)

## Copy also to DTEM dir for rasterizing down the river ----
file.copy(from="out_gcs/DTEM_gcs.tif", 
          to=paste0(demdir,"/out_gcs/"),  
          overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
file.copy(from="out_pcs/DTEM_pcs.tif", 
          to=paste0(demdir,"/out_pcs/"),  
          overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)

# AGB ----

# #3rd party tests
# getwd()
# ESA_AGB_CII <- rast(paste0("ESA_AGB_CCI_countries_COD_100scale.tif")) # The original GEE script multiplies by 2 to transform into AGB from C
# plot(ESA_AGB_CII)
# #---

country_parameters %>%
  dplyr::filter(Var == "AGB1map_name") %>%
  pull(ParCHR) -> AGB1map_name
country_parameters %>%
  dplyr::filter(Var == "AGB1map_uncer_name") %>%
  pull(ParCHR) -> AGB1map_name_uncer
if (GEE_scale == 1000 & GEE_tyRoi == "world"){
  AGBoeste <- rast(paste0("AGB_DAAC_",GEE_ext,"_1000scale-0000000000-0000000000.tif")) # The original GEE script multiplies by 2 to transform into AGB from C
  AGBeste <- rast(paste0("AGB_DAAC_",GEE_ext,"_1000scale-0000000000-0000023296.tif")) # Export and projected values are in AGB Mg/ha
  merge_AGB_ha <- merge(AGBoeste,AGBeste) %>%
    setMinMax(force=TRUE) %>%
    terra::project(DTEM_pcs, method="bilinear", gdal=TRUE)
} else if (GEE_scale == 100 & GEE_tyRoi == "countries") { # For single countries at 100m
  merge_AGB_ha <- rast(paste0("AGB_DAAC_",GEE_tyRoi,"_",GEE_country,"_",GEE_scale,"scale.tif")) %>%
    setMinMax(force=TRUE) %>%
    terra::project(DTEM_pcs, method="bilinear", gdal=TRUE)
} else if (GEE_scale == 1000 & GEE_tyRoi == "countries") { # For single countries at 1km
  merge_AGB_ha <- rast(paste0("AGB_DAAC_",GEE_tyRoi,"_",GEE_country,"_",GEE_scale,"scale.tif")) %>%
    setMinMax(force=TRUE) %>%
    terra::project(DTEM_pcs, method="bilinear", gdal=TRUE) 
}
terra::writeRaster(merge_AGB_ha[[1]], paste0("out_pcs/",AGB1map_name), filetype = "GTiff", datatype="INT4S", overwrite = TRUE)
terra::writeRaster(merge_AGB_ha[[2]], paste0("out_pcs/",AGB1map_name_uncer), filetype = "GTiff", datatype="INT4S", overwrite = TRUE)
merge_AGB_ha[[1]]

# Forest loss ----
country_parameters %>%
  dplyr::filter(Var == "lossyear_name") %>%
  pull(ParCHR) -> lossyear_name
if (GEE_scale == 1000 & GEE_tyRoi == "world"){
  Lossoeste <- rast(paste0("GFC_Lossyear_",GEE_ext,"_1000scale-0000000000-0000000000.tif"))
  Losseste <- rast(paste0("GFC_Lossyear_",GEE_ext,"_1000scale-0000000000-0000032768.tif"))
  Loss <- merge(Lossoeste,Losseste) 
} else if (GEE_scale == 100 & GEE_tyRoi == "countries") { # For single countries at 100m
  Loss <- rast(paste0("GFC_Lossyear_",GEE_tyRoi,"_",GEE_country,"_",GEE_scale,"scale.tif"))
} else if (GEE_scale == 1000 & GEE_tyRoi == "countries") { # For single countries at 1km
  Loss <- rast(paste0("GFC_Lossyear_",GEE_tyRoi,"_",GEE_country,"_",GEE_scale,"scale.tif"))
}
Loss %>%
  terra::project(DTEM_pcs, method="near", gdal=TRUE) %>%
  terra::writeRaster(paste0("out_pcs/",lossyear_name), filetype = "GTiff", datatype="INT2S", overwrite = TRUE)

# Forest gain ----
country_parameters %>%
  dplyr::filter(Var == "gain_name") %>%
  pull(ParCHR) -> gain_name
if (GEE_scale == 1000 & GEE_tyRoi == "world"){
  Gainoeste <- rast(paste0("GFC_Gain2012_",GEE_ext,"_1000scale-0000000000-0000000000.tif"))
  Gaineste <- rast(paste0("GFC_Gain2012_",GEE_ext,"_1000scale-0000000000-0000032768.tif"))
  Gain <- merge(Gainoeste,Gaineste)
} else if (GEE_scale == 100 & GEE_tyRoi == "countries") { # For single countries at 100m
  Gain <- rast(paste0("GFC_Gain2012_",GEE_tyRoi,"_",GEE_country,"_",GEE_scale,"scale.tif"))
} else if (GEE_scale == 1000 & GEE_tyRoi == "countries") { # For single countries at 1km
  Gain <- rast(paste0("GFC_Gain2012_",GEE_tyRoi,"_",GEE_country,"_",GEE_scale,"scale.tif"))
}
Gain %>%
  terra::project(DTEM_pcs, method="near", gdal=TRUE) %>%
  terra::writeRaster(paste0("out_pcs/",gain_name), filetype = "GTiff", datatype="INT2S", overwrite = TRUE)

# GFC vs GFCC ---- 
country_parameters %>%
  dplyr::filter(Var == "treecover_name") %>%
  pull(ParCHR) -> treecover_name
if (GEE_scale == 1000 & GEE_tyRoi == "world"){
  GFCoeste <- rast(paste0("GFC_Treecover2000_",GEE_ext,"_1000scale-0000000000-0000000000.tif"))
  GFCeste <- rast(paste0("GFC_Treecover2000_",GEE_ext,"_1000scale-0000000000-0000032768.tif"))
  GFC <- merge(GFCoeste[[1]],GFCeste[[1]])
} else if (GEE_scale == 100 & GEE_tyRoi == "countries") { # For single countries at 100m
  GFC <- rast(paste0("GFC_Treecover2000_",GEE_tyRoi,"_",GEE_country,"_",GEE_scale,"scale.tif"))
} else if (GEE_scale == 1000 & GEE_tyRoi == "countries") { # For single countries at 1
  GFC <- rast(paste0("GFC_Treecover2000_",GEE_tyRoi,"_",GEE_country,"_",GEE_scale,"scale.tif"))
}
GFC %>%
  terra::project(DTEM_pcs, method="bilinear", gdal=TRUE) %>%
  terra::writeRaster(paste0("out_pcs/",treecover_name), filetype = "GTiff", datatype="INT2S", overwrite = TRUE)

# GFC ----
tcyears <- c(2000,2005,2010,2015)
# i = 2000
for (i in tcyears){
  if (GEE_scale == 1000 & GEE_tyRoi == "world"){
    tcoeste <- rast(paste0("GFCC_TC",i,"_",GEE_ext,"_1000scale-0000000000-0000000000.tif"))
    tceste <- rast(paste0("GFCC_TC",i,"_",GEE_ext,"_1000scale-0000000000-0000023296.tif"))
    tc <- merge(tcoeste,tceste)
  } else if (GEE_scale == 100 & GEE_tyRoi == "countries") { # For single countries at 100m
    tc <- rast(paste0("GFCC_TC",i,"_",GEE_tyRoi,"_",GEE_country,"_",GEE_scale,"scale.tif"))
  } else if (GEE_scale == 1000 & GEE_tyRoi == "countries") { # For single countries at 1
    tc <- rast(paste0("GFCC_TC",i,"_",GEE_tyRoi,"_",GEE_country,"_",GEE_scale,"scale.tif"))   
  }
  tc[[1]] %>%
    terra::project(DTEM_pcs, method="bilinear", gdal=TRUE) %>%
    terra::writeRaster(paste0("out_pcs/TC",i,"_pcs.tif"), filetype = "GTiff", datatype="INT2S", overwrite = TRUE)
  tc[[2]] %>%
    terra::project(DTEM_pcs, method="bilinear", gdal=TRUE) %>%
    terra::writeRaster(paste0("out_pcs/TCuncer",i,"_pcs.tif"), filetype = "GTiff", datatype="INT2S", overwrite = TRUE)
}

# MODIS LC Type 1 ----
country_parameters %>%
  dplyr::filter(Var == "LULCt1map") %>%
  pull(ParCHR) -> LULCt1map
if (LULCt1map == "YES") {
  country_parameters %>%
    dplyr::filter(Var == "LULCt1map_name") %>%
    pull(ParCHR) -> LULCt1map_name
  country_parameters %>%
    dplyr::filter(Var == "LULCt1map_yr") %>%
    pull(ParCHR) -> LULCt1map_yr_pre
  clean_string1 <- gsub("c\\(|\\)", "", LULCt1map_yr_pre)
  string_numbers1 <- strsplit(clean_string1, ",")[[1]]
  LULCt1map_yr <- as.numeric(string_numbers1)
  for (k in LULCt1map_yr){
    #k = 2010
    if (GEE_scale == 1000 & GEE_tyRoi == "world"){
      # lulc2010oeste <- rast(paste0("Modis_LCType1_",GEE_ext,"_",k,"_1000scale-0000000000-0000000000.tif"))
      # lulc2010este <- rast(paste0("Modis_LCType1_",GEE_ext,"_",k,"_1000scale-0000000000-0000032768.tif"))
      # merge(lulc2010oeste,lulc2010este) %>%
      rast(paste0("Modis_LCType1_",GEE_ext,"_",k,"_1000scale.tif")) %>%
        terra::project(DTEM_pcs, method="near", gdal=TRUE) %>%
        terra::writeRaster(paste0("out_pcs/pre",k,"_",LULCt1map_name), filetype = "GTiff", datatype="INT2S", overwrite = TRUE)
    } else if (GEE_scale == 100 & GEE_tyRoi == "countries") { # For single countries at 100m
      rast(paste0("Modis_LCType1_",GEE_tyRoi,"_",GEE_country,"_",k,"_",GEE_scale,"scale.tif")) %>%
        terra::project(DTEM_pcs, method="near", gdal=TRUE) %>%
        terra::writeRaster(paste0("out_pcs/pre",k,"_",LULCt1map_name), filetype = "GTiff", datatype="INT2S", overwrite = TRUE)
    } else if (GEE_scale == 1000 & GEE_tyRoi == "countries") { # For single countries at 1
      rast(paste0("Modis_LCType1_",GEE_tyRoi,"_",GEE_country,"_",k,"_",GEE_scale,"scale.tif")) %>%
        terra::project(DTEM_pcs, method="near", gdal=TRUE) %>%
        terra::writeRaster(paste0("out_pcs/pre",k,"_",LULCt1map_name), filetype = "GTiff", datatype="INT2S", overwrite = TRUE)    
    }
  }
  
}

# Copernicus CGLS-LC100 ----
country_parameters %>%
  dplyr::filter(Var == "LULCt2map") %>%
  pull(ParCHR) -> LULCt2map
if (LULCt2map == "YES") {
  country_parameters %>%
    dplyr::filter(Var == "LULCt2map_name") %>%
    pull(ParCHR) -> LULCt2map_name
  country_parameters %>%
    dplyr::filter(Var == "LULCt2map_yr") %>%
    pull(ParCHR) -> LULCt2map_yr_pre
  clean_string2 <- gsub("c\\(|\\)", "", LULCt2map_yr_pre)
  string_numbers2 <- strsplit(clean_string2, ",")[[1]]
  LULCt2map_yr <- as.numeric(string_numbers2)
  for (l in LULCt2map_yr){
    if (GEE_scale == 1000 & GEE_tyRoi == "world"){
      # lulc2010oeste <- rast(paste0("Modis_LCType1_",GEE_ext,"_",k,"_1000scale-0000000000-0000000000.tif"))
      # lulc2010este <- rast(paste0("Modis_LCType1_",GEE_ext,"_",k,"_1000scale-0000000000-0000032768.tif"))
      # merge(lulc2010oeste,lulc2010este) %>%
      rast(paste0("Copernicus_2B_",GEE_ext,"_",l,"_1000scale.tif")) %>%
        terra::project(DTEM_pcs, method="near", gdal=TRUE) %>%
        terra::writeRaster(paste0("out_pcs/pre",l,"_",LULCt2map_name), filetype = "GTiff", datatype="INT2S", overwrite = TRUE)
    } else if (GEE_scale == 100 & GEE_tyRoi == "countries") { # For single countries at 100m
      rast(paste0("Copernicus_2B_",GEE_tyRoi,"_",GEE_country,"_",l,"_",GEE_scale,"scale.tif")) %>%
        terra::project(DTEM_pcs, method="near", gdal=TRUE) %>%
        terra::writeRaster(paste0("out_pcs/pre",l,"_",LULCt2map_name), filetype = "GTiff", datatype="INT2S", overwrite = TRUE)
    } else if (GEE_scale == 1000 & GEE_tyRoi == "countries") { # For single countries at 1
      rast(paste0("Copernicus_2B_",GEE_tyRoi,"_",GEE_country,"_",l,"_",GEE_scale,"scale.tif")) %>%
        terra::project(DTEM_pcs, method="near", gdal=TRUE) %>%
        terra::writeRaster(paste0("out_pcs/pre",l,"_",LULCt2map_name), filetype = "GTiff", datatype="INT2S", overwrite = TRUE)
    }  
  }
}

# Add a third, multiyear luc map...
country_parameters %>%
  dplyr::filter(Var == "LULCt3map") %>%
  pull(ParCHR) -> LULCt3map
if (LULCt3map == "YES") {
  country_parameters %>%
    dplyr::filter(Var == "LULCt3map_name") %>%
    pull(ParCHR) -> LULCt3map_name
  country_parameters %>%
    dplyr::filter(Var == "LULCt3map_yr") %>%
    pull(ParCHR) -> LULCt3map_yr_pre
  clean_string <- gsub("c\\(|\\)", "", LULCt3map_yr_pre)
  string_numbers <- strsplit(clean_string, ",")[[1]]
  LULCt3map_yr <- as.numeric(string_numbers)
  LULCt3map_yr
}


# Copy 2 MoFuSS ----
copy2mofussfiles2 <- list.files(path = paste0(geedir,"/out_gcs/"),
                                pattern = ".*\\.tif$", full.names = TRUE)
for (f2 in copy2mofussfiles2) {
  file.copy(from=f2, 
            to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster_GCS/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

copy2mofussfiles <- list.files(path = paste0(geedir,"/out_pcs/"),
                               pattern = ".*\\.tif$", full.names = TRUE)
for (f in copy2mofussfiles) {
  file.copy(from=f, 
            to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

