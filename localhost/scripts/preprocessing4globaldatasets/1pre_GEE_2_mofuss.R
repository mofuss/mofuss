# MoFuSS
# Version 4
# Date: Ago 2025

# 2dolist
# 
# Watch out for 3rd party biomass and bulk download using wget - add instructions adrian

# Internal parameters
# Google Colab link: https://colab.research.google.com/drive/1Ef9XMfen2DFhAhZ87B3gxjyf9QQfLOPH
buf_m <- 10000    # 10 km buffer (meters) for masking DTEM and template

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
EPSG_gcs <- paste0("EPSG:",epsg_gcs)

country_parameters %>%
  dplyr::filter(Var == "proj_pcs") %>%
  pull(ParCHR) -> proj_pcs

country_parameters %>%
  dplyr::filter(Var == "epsg_pcs") %>%
  pull(ParCHR) %>%
  as.integer(.) -> epsg_pcs
EPSG_pcs <- paste0("EPSG:",epsg_pcs)

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

# unlink("temp/", recursive = TRUE) # Warning, slow geoprocessing times
# unlink("out_gcs/", recursive = TRUE) # Warning, slow geoprocessing times
# unlink("out_pcs/", recursive = TRUE) # Warning, slow geoprocessing times
# Sys.sleep(5)
# if (!dir.exists("temp")) {dir.create("temp")}
# if (!dir.exists("out_gcs")) {dir.create("out_gcs")}
# if (!dir.exists("out_pcs")) {dir.create("out_pcs")}

if (exists("terraTempdir") == FALSE) {
  choose_directory31 = function(caption = "Choose a terra Temp directory, preferably on a SSD local disk") {
    if(.Platform$OS.type == "unix")  {
      setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
    } else {
      setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
    }
  }
  choose_directory31()
  terraTempdir <- getwd()
}

# Optional: tune terra temp/memory (adjust tempdir to a fast disk if possible)
terraOptions(
  tempdir = terraTempdir,
  memfrac = 0.8,
  progress = 3
)
unlink(paste0(terraTempdir, "/*"), recursive = TRUE)

setwd(geedir)

# Mask ----
mofuss_regions0_simp <- vect(paste0(admindir,"/regions_adm0/mofuss_regions0_simp.shp"))

## Dissolve countries + 10 km outside buffer (do buffer in meters -> project first)
roi <- aggregate(mofuss_regions0_simp)     # dissolve ALL features into one
roi_3395 <- project(roi, EPSG_pcs)
roi_buf_3395 <- buffer(roi_3395, width = buf_m)

# DTEM ----
pattern_dtem <- paste0("^", GEE_tyRoi, "_SRTM_elevation_", "native-[0-9]+-[0-9]+\\.tif$")
tif_files_dtem <- list.files(geedir, pattern = pattern_dtem, full.names = TRUE)
if (length(tif_files_dtem) == 0) stop("No matching tiles found.")

# 1) Virtual mosaic (fast, low RAM): build a VRT and open it
vrt(tif_files_dtem, filename = "temp/DTEM_native.vrt", overwrite = TRUE)
DTEM_vrt <- rast("temp/DTEM_native.vrt")

# 2) Crop early in source CRS (saves a LOT of time before projection)
# We crop by the buffered ROI reprojected back to raster CRS (lon/lat)
roi_buf_ll <- project(roi_buf_3395, crs(DTEM_vrt))
DTEM_crop_ll <- crop(DTEM_vrt, roi_buf_ll, snap = "out")  # still lon/lat
writeRaster(
  DTEM_crop_ll,
  filename = "out_gcs/DTEM_gcs.tif",
  overwrite = TRUE,
  wopt = list(
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
    datatype = "INT2S",
    NAflag = -32768
  )
)

# 3) Project to EPSG:3395 at 1000 m
# Use a template so you control resolution exactly.
e <- ext(roi_buf_3395)
template_3395 <- rast(ext = e, res = GEE_scale, crs = EPSG_pcs)
template_ll <- project(template_3395, EPSG_gcs) # ~1 km lon/lat grid
area_ll_m2  <- cellSize(template_ll, unit="m")  # m² per cell (true-ish)
area_3395_m2 <- project(area_ll_m2, template_3395, method="bilinear")
writeRaster(area_3395_m2, "temp/pixel_area_trueEarth_3395_1km_m2.tif",
            overwrite=TRUE, wopt=list(gdal=c("COMPRESS=LZW","TILED=YES","BIGTIFF=YES")))

DTEM_3395_1km <- project(
  DTEM_crop_ll,
  template_3395,
  method = "bilinear",
  filename = "out_pcs/DTEM_pcs.tif",
  overwrite = TRUE,
  wopt = list(
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
    datatype = "INT2S",
    NAflag = -32768
  )
)

writeRaster(
  mask(DTEM_3395_1km, roi_buf_3395),
  filename = "out_pcs/DTEM_pcs_masked.tif",
  overwrite = TRUE,
  wopt = list(
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
    datatype = "INT2S",
    NAflag = -32768
  )
)

# # Copy also to DTEM dir for rasterizing down the river ----
# if (exists("demdir") == FALSE) {
#   choose_directory13 = function(caption = "Choose the directory where the DTEM files will be saved for rastering") {
#     if(.Platform$OS.type == "unix")  {
#       setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
#     } else {
#       setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
#     }
#   }
#   choose_directory13()
#   demdir <- getwd()
# }

# AGB ----
## DAAC ----
### Original repo: https://developers.google.com/earth-engine/datasets/catalog/NASA_ORNL_biomass_carbon_density_v1
### Notes: Global Aboveground and Belowground Biomass Carbon Density Maps
country_parameters %>%
  dplyr::filter(Var == "AGB1map") %>%
  pull(ParCHR) -> AGB1map
if (AGB1map == "YES") {
  country_parameters %>%
    dplyr::filter(Var == "AGB1map_name") %>%
    pull(ParCHR) -> AGB1map_name
pattern_daac_agb <- paste0("^", GEE_tyRoi, "_daac_agb_", GEE_scale, "m-[0-9]+-[0-9]+\\.tif$")
tif_files_daac_agb <- list.files(geedir, pattern = pattern_daac_agb, full.names = TRUE)
if (length(tif_files_daac_agb) == 0) stop("No matching tiles found.")
if (length(tif_files_daac_agb) == 1) {
  DAAC <- rast(tif_files_daac_agb)
} else {
  rlist_daac_agb <- lapply(tif_files_daac_agb, rast)
  DAAC <- do.call(merge, rlist_daac_agb)
}
DAAC_agb <- DAAC / 0.47 # See notes above
# DAAC <- setMinMax(DAAC, force=TRUE)
terra::writeRaster(DAAC_agb, paste0("out_pcs/",AGB1map_name), filetype = "GTiff", overwrite = TRUE)
}

## ESA ----
### Original repo: https://gee-community-catalog.org/projects/cci_agb/
### Notes: Above ground biomass (AGB, unit: tons/ha i.e., Mg/ha) (raster dataset). This is defined as the mass, 
### expressed as oven-dry weight of the woody parts (stem, bark, branches and twigs) of all living trees excluding stump and roots
### Per-pixel estimates of above-ground biomass uncertainty expressed as the standard deviation in Mg/ha (raster dataset)
country_parameters %>%
  dplyr::filter(Var == "AGB2map") %>%
  pull(ParCHR) -> AGB2map
if (AGB2map == "YES") {
  country_parameters %>%
    dplyr::filter(Var == "AGB2map_name") %>%
    pull(ParCHR) -> AGB2map_name
  country_parameters %>%
    dplyr::filter(Var == "AGB2map_yr") %>%
    pull(ParCHR) -> AGB2map_yr
  pattern_esa_agb <- paste0("^", GEE_tyRoi, "_esa_agb_", AGB2map_yr,"_",GEE_scale, "m-[0-9]+-[0-9]+\\.tif$")
  tif_files_esa_agb <- list.files(geedir, pattern = pattern_esa_agb, full.names = TRUE)
  if (length(tif_files_esa_agb) == 0) stop("No matching tiles found.")
  if (length(tif_files_esa_agb) == 1) {
    ESA <- rast(tif_files_esa_agb)
  } else {
    rlist_esa_agb <- lapply(tif_files_esa_agb, rast)
    ESA <- do.call(merge, rlist_esa_agb)
  }
  # ESA <- setMinMax(ESA, force=TRUE)
  terra::writeRaster(ESA, paste0("out_pcs/",AGB2map_name), filetype = "GTiff", overwrite = TRUE)
}

# ## WCMC ----
# ### Original repo: https://developers.google.com/earth-engine/datasets/catalog/WCMC_biomass_carbon_density_v1_0
# ### Notes: Tonnes of above and below ground biomass carbon per hectare
# ### Note the dataset doesn't separate AGB from BGB WARNING !!!
# ### Use CTrees outsourced instead for AGB 3 - way much better agb3_c.tif
# country_parameters %>%
#   dplyr::filter(Var == "AGB3map") %>%
#   pull(ParCHR) -> AGB3map
# if (AGB3map == "YES") {
#   country_parameters %>%
#     dplyr::filter(Var == "AGB3map_name") %>%
#     pull(ParCHR) -> AGB3map_name
#   pattern_wcmc_agb <- paste0("^", GEE_tyRoi, "_wcmc_biomass_biomass_tonnes_per_ha_", GEE_scale, "m-[0-9]+-[0-9]+\\.tif$")
#   tif_files_wcmc_agb <- list.files(geedir, pattern = pattern_wcmc_agb, full.names = TRUE)
#   if (length(tif_files_wcmc_agb) == 0) stop("No matching tiles found.")
#   if (length(tif_files_wcmc_agb) == 1) {
#     WCMC <- rast(tif_files_wcmc_agb)
#   } else {
#     rlist_wcmc_agb <- lapply(tif_files_wcmc_agb, rast)
#     WCMC <- do.call(merge, rlist_wcmc_agb)
#   }
#   # WCMC <- setMinMax(WCMC, force=TRUE)
#   terra::writeRaster(WCMC, paste0("out_pcs/",AGB3map_name), filetype = "GTiff", overwrite = TRUE)
# }

# GFC ----
## GFC treecover 2000 ----
pattern_treecover <- paste0("^", GEE_tyRoi, "_GFC_treecover2000_", GEE_scale, "m-[0-9]+-[0-9]+\\.tif$")
tif_files_treecover <- list.files(geedir, pattern = pattern_treecover, full.names = TRUE)
if (length(tif_files_treecover) == 0) stop("No matching tiles found.")
if (length(tif_files_treecover) == 1) {
  GFC_treecover2000 <- rast(tif_files_treecover)
} else {
  rlist_treecover<- lapply(tif_files_treecover, rast)
  GFC_treecover2000 <- do.call(merge, rlist_treecover)
}
# GFC_treecover2000 <- setMinMax(GFC_treecover2000, force=TRUE)
terra::writeRaster(GFC_treecover2000, "out_pcs/treecover2000_pcs.tif", filetype = "GTiff", overwrite = TRUE)

## GFC yearloss ----
pattern_yearloss <- paste0("^", GEE_tyRoi, "_GFC_yearLoss_lossyear_", GEE_scale, "m-[0-9]+-[0-9]+\\.tif$")
tif_files_yearloss <- list.files(geedir, pattern = pattern_yearloss, full.names = TRUE)
if (length(tif_files_yearloss) == 0) stop("No matching tiles found.")
if (length(tif_files_yearloss) == 1) {
  GFC_yearloss <- rast(tif_files_yearloss)
} else {
  rlist_yearloss<- lapply(tif_files_yearloss, rast)
  GFC_yearloss <- do.call(merge, rlist_yearloss)
}
# GFC_yearloss <- setMinMax(GFC_yearloss, force=TRUE)
terra::writeRaster(GFC_yearloss, "out_pcs/lossyear_pcs.tif", filetype = "GTiff", overwrite = TRUE)

## GFC gain ----
pattern_gain <- paste0("^", GEE_tyRoi, "_GFC_gain_", GEE_scale, "m-[0-9]+-[0-9]+\\.tif$")
tif_files_gain <- list.files(geedir, pattern = pattern_gain, full.names = TRUE)
if (length(tif_files_gain) == 0) stop("No matching tiles found.")
if (length(tif_files_gain) == 1) {
  GFC_gain <- rast(tif_files_gain)
} else {
  rlist_gain<- lapply(tif_files_gain, rast)
  GFCgain <- do.call(merge, rlist_gain)
}
# GFCgain <- setMinMax(GFCgain, force=TRUE)
terra::writeRaster(GFCgain, "out_pcs/gain_pcs.tif", filetype = "GTiff", overwrite = TRUE)


# Land Use / Cover  ----
## MODIS LC Type 1 ----
country_parameters %>%
  dplyr::filter(Var == "LULCt1map") %>%
  pull(ParCHR) -> LULCt1map
if (LULCt1map == "YES") {
  country_parameters %>%
    dplyr::filter(Var == "LULCt1map_name") %>%
    pull(ParCHR) -> LULCt1map_name
  country_parameters %>%
    dplyr::filter(Var == "LULCt1map_yr") %>%
    pull(ParCHR) -> LULCt1map_yr
  pattern_modis <- paste0("^", GEE_tyRoi, "_modisLc_", LULCt1map_yr,"_LC_Type1_",GEE_scale, "m-[0-9]+-[0-9]+\\.tif$")
  tif_files_modis <- list.files(geedir, pattern = pattern_modis, full.names = TRUE)
  if (length(tif_files_modis) == 0) stop("No matching tiles found.")
  if (length(tif_files_modis) == 1) {
    MODIS <- rast(tif_files_modis)
  } else {
    rlist_modis <- lapply(tif_files_modis, rast)
    MODIS <- do.call(merge, rlist_modis)
  }
  # MODIS <- setMinMax(MODIS, force=TRUE)
  terra::writeRaster(MODIS, paste0("out_pcs/",LULCt1map_name), filetype = "GTiff", overwrite = TRUE)
}

## Copernicus CGLS-LC100 ----
country_parameters %>%
  dplyr::filter(Var == "LULCt2map") %>%
  pull(ParCHR) -> LULCt2map
if (LULCt2map == "YES") {
  country_parameters %>%
    dplyr::filter(Var == "LULCt2map_name") %>%
    pull(ParCHR) -> LULCt2map_name
  country_parameters %>%
    dplyr::filter(Var == "LULCt2map_yr") %>%
    pull(ParCHR) -> LULCt2map_yr
  pattern_copernicus <- paste0("^", GEE_tyRoi, "_copernicusLc_", LULCt2map_yr,"_classification_discrete_classification_",GEE_scale, "m-[0-9]+-[0-9]+\\.tif$")
  tif_files_copernicus <- list.files(geedir, pattern = pattern_copernicus, full.names = TRUE)
  if (length(tif_files_copernicus) == 0) stop("No matching tiles found.")
  if (length(tif_files_copernicus) == 1) {
    COPERNICUS <- rast(tif_files_copernicus)
  } else {
    rlist_copernicus <- lapply(tif_files_copernicus, rast)
    COPERNICUS <- do.call(merge, rlist_copernicus)
  }
  # COPERNICUS <- setMinMax(COPERNICUS, force=TRUE)
  terra::writeRaster(COPERNICUS, paste0("out_pcs/",LULCt2map_name), filetype = "GTiff", overwrite = TRUE)
}

## Dynamic world ----
country_parameters %>%
  dplyr::filter(Var == "LULCt3map") %>%
  pull(ParCHR) -> LULCt3map
if (LULCt2map == "YES") {
  country_parameters %>%
    dplyr::filter(Var == "LULCt3map_name") %>%
    pull(ParCHR) -> LULCt3map_name
  country_parameters %>%
    dplyr::filter(Var == "LULCt3map_yr") %>%
    pull(ParCHR) -> LULCt3map_yr
  pattern_dw <- paste0("^", GEE_tyRoi, "_dynamic_world_", LULCt3map_yr,"_label_",GEE_scale, "m-[0-9]+-[0-9]+\\.tif$")
  tif_files_dw <- list.files(geedir, pattern = pattern_dw, full.names = TRUE)
  if (length(tif_files_dw) == 0) stop("No matching tiles found.")
  if (length(tif_files_dw) == 1) {
    DW <- rast(tif_files_dw)
  } else {
    rlist_dw <- lapply(tif_files_dw, rast)
    DW <- do.call(merge, rlist_dw)
  }
  # COPERNICUS <- setMinMax(COPERNICUS, force=TRUE)
  terra::writeRaster(DW, paste0("out_pcs/",LULCt3map_name), filetype = "GTiff", overwrite = TRUE)
}


# Copy 2 MoFuSS ----
# copy2mofussfiles2 <- list.files(path = paste0(geedir,"/out_gcs/"),
#                                 pattern = ".*\\.tif$", full.names = TRUE)
# for (f2 in copy2mofussfiles2) {
#   file.copy(from=f2, 
#             to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster_GCS/"),  
#             overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
# }

copy2mofussfiles <- list.files(path = paste0(geedir,"/out_pcs/"),
                               pattern = ".*\\.tif$", full.names = TRUE)
for (f in copy2mofussfiles) {
  file.copy(from=f, 
            to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

