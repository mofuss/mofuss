# MoFuSS
# Version 4
# Date: Ago 2025

# 2dolist
# Watch out for 3rd party biomass and bulk download using wget - add instructions adrian
# Erase temps see below
# Add years for temporal series
## CHECK FOR AREA CORRECTION FOR AGB
# template_ll <- project(template_3395, EPSG_gcs) # ~1 km lon/lat grid
# area_ll_m2  <- cellSize(template_ll, unit="m")  # m² per cell (true-ish)
# area_3395_m2 <- project(area_ll_m2, template_3395, method="bilinear")
# writeRaster(area_3395_m2, "temp/pixel_area_trueEarth_3395_1km_m2.tif",
#             overwrite=TRUE, wopt=list(gdal=c("COMPRESS=LZW","TILED=YES","BIGTIFF=YES")))

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

## Common 1 km template in World Mercator ----
e <- ext(roi_buf_3395)
template_3395 <- rast(ext = e, res = GEE_scale, crs = EPSG_pcs)
# give it dummy values so terra can write it
template_3395 <- init(template_3395, NA)
writeRaster(
  template_3395,
  "temp/template_3395_1km.tif",
  overwrite = TRUE
)
template_ll <- project(template_3395, EPSG_gcs) # ~1 km lon/lat grid
area_ll_m2  <- cellSize(template_ll, unit="m")  # m² per cell (true-ish)
area_3395_m2 <- project(area_ll_m2, template_3395, method="bilinear")
writeRaster(area_3395_m2, "temp/pixel_area_trueEarth_3395_1km_m2.tif",
            overwrite=TRUE, wopt=list(gdal=c("COMPRESS=LZW","TILED=YES","BIGTIFF=YES")))

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
  pattern_daac <- paste0("^", GEE_tyRoi, "_daac_agb_", "native-[0-9]+-[0-9]+\\.tif$")
  tif_files_daac <- list.files(geedir, pattern = pattern_daac, full.names = TRUE)
  if (length(tif_files_daac) == 0) stop("No matching tiles found.")
  
  # 1) Virtual mosaic (fast, low RAM): build a VRT and open it
  vrt(tif_files_daac, filename = "temp/DAAC_native.vrt", overwrite = TRUE)
  DAAC_vrt <- rast("temp/DAAC_native.vrt")
  
  # 2) Crop early in source CRS (saves a LOT of time before projection)
  # We crop by the buffered ROI reprojected back to raster CRS (lon/lat)
  roi_buf_ll <- project(roi_buf_3395, crs(DAAC_vrt))
  DAAC_crop_ll <- crop(DAAC_vrt, roi_buf_ll, snap = "out") / 0.47 # still lon/lat, and see notes above
  
  # writeRaster(
  #   DAAC_crop_ll,
  #   filename = "out_gcs/DAAC_gcs.tif",
  #   overwrite = TRUE,
  #   wopt = list(
  #     gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
  #     datatype = "INT2S",
  #     NAflag = -32768
  #   )
  # )
  
  # 3) Project to EPSG:3395 at 1000 m
  DAAC_3395_1km <- project(
    DAAC_crop_ll,
    template_3395,
    method = "bilinear",
    filename = paste0("out_pcs/",AGB1map_name),
    overwrite = TRUE,
    wopt = list(
      gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
      datatype = "INT2S",
      NAflag = -32768
    )
  )
  
  # writeRaster(
  #   mask(DAAC_3395_1km, roi_buf_3395),
  #   filename = "out_pcs/DAAC_pcs_masked.tif",
  #   overwrite = TRUE,
  #   wopt = list(
  #     gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
  #     datatype = "INT2S",
  #     NAflag = -32768
  #   )
  # )
  
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
  pattern_esa <- paste0("^", GEE_tyRoi, "_esa_agb_", AGB2map_yr,"_native-[0-9]+-[0-9]+\\.tif$")
  
  tif_files_esa <- list.files(geedir, pattern = pattern_esa, full.names = TRUE)
  if (length(tif_files_esa) == 0) stop("No matching tiles found.")
  
  # 1) Virtual mosaic (fast, low RAM): build a VRT and open it
  vrt(tif_files_esa, filename = "temp/ESA_AGB_native.vrt", overwrite = TRUE)
  ESA_AGB_vrt <- rast("temp/ESA_AGB_native.vrt")
  
  # 2) Crop early in source CRS (saves a LOT of time before projection)
  # We crop by the buffered ROI reprojected back to raster CRS (lon/lat)
  roi_buf_ll <- project(roi_buf_3395, crs(ESA_AGB_vrt))
  ESA_AGB_crop_ll <- crop(ESA_AGB_vrt, roi_buf_ll, snap = "out") # still lon/lat, and see notes above
  
  # writeRaster(
  #   ESA_AGB_crop_ll,
  #   filename = "out_gcs/ESA_AGB_gcs.tif",
  #   overwrite = TRUE,
  #   wopt = list(
  #     gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
  #     datatype = "INT2S",
  #     NAflag = -32768
  #   )
  # )
  
  # 3) Project to EPSG:3395 at 1000 m
  ESA_AGB_3395_1km <- project(
    ESA_AGB_crop_ll,
    template_3395,
    method = "bilinear",
    filename = paste0("out_pcs/",AGB2map_name),
    overwrite = TRUE,
    wopt = list(
      gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
      datatype = "INT2S",
      NAflag = -32768
    )
  )
  
  # writeRaster(
  #   mask(ESA_AGB_3395_1km, roi_buf_3395),
  #   filename = "out_pcs/ESA_AGB_pcs_masked.tif",
  #   overwrite = TRUE,
  #   wopt = list(
  #     gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
  #     datatype = "INT2S",
  #     NAflag = -32768
  #   )
  # )
  
}

## WCMC ----
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
#   pattern_wcmc <- paste0("^", GEE_tyRoi, "_wcmc_biomass_biomass_tonnes_per_ha_", GEE_scale, "m-[0-9]+-[0-9]+\\.tif$")

# }

# GFC ----
## Helper: keep only source tiles intersecting the ROI ----
get_intersecting_tiles <- function(tif_files, roi_buf_3395) {
  if (length(tif_files) == 0) stop("No matching tiles found.")
  
  # use first tile to get source CRS
  r0 <- rast(tif_files[1])
  roi_buf_ll <- project(roi_buf_3395, crs(r0))
  roi_ext <- ext(roi_buf_ll)
  
  # extents of all candidate tiles
  tile_exts <- lapply(tif_files, function(f) ext(rast(f)))
  
  # keep only intersecting tiles
  keep <- vapply(tile_exts, function(ex) relate(ex, roi_ext, "intersects"), logical(1))
  tif_files_roi <- tif_files[keep]
  
  if (length(tif_files_roi) == 0) stop("No tiles intersect the ROI.")
  
  return(list(
    tif_files_roi = tif_files_roi,
    roi_buf_ll = roi_buf_ll
  ))
}

## GFC treecover 2000 ----
pattern_treecover <- paste0("^", GEE_tyRoi, "_GFC_treecover2000_native-[0-9]+-[0-9]+\\.tif$")
tif_files_treecover <- list.files(geedir, pattern = pattern_treecover, full.names = TRUE)
if (length(tif_files_treecover) == 0) stop("No matching tiles found.")

# 1) Virtual mosaic (fast, low RAM): build a VRT and open it
vrt(tif_files_treecover, filename = "temp/treecover_native.vrt", overwrite = TRUE)
treecover_vrt <- rast("temp/treecover_native.vrt")

# 2) Crop early in source CRS (saves a LOT of time before projection)
# We crop by the buffered ROI reprojected back to raster CRS (lon/lat)
roi_buf_ll <- project(roi_buf_3395, crs(treecover_vrt))
treecover_crop_ll <- crop(treecover_vrt, roi_buf_ll, snap = "out") # still lon/lat, and see notes above

# writeRaster(
#   treecover_crop_ll,
#   filename = "out_gcs/treecover2000_gcs.tif",
#   overwrite = TRUE,
#   wopt = list(
#     gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
#     datatype = "INT2S",
#     NAflag = -32768
#   )
# )

# 3) Project to EPSG:3395 at 1000 m
# Use a template so you control resolution exactly.
treecover_3395_1km <- project(
  treecover_crop_ll,
  template_3395,
  method = "bilinear",
  filename = "out_pcs/treecover2000_pcs.tif",
  overwrite = TRUE,
  wopt = list(
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
    datatype = "INT2S",
    NAflag = -32768
  )
)

# writeRaster(
#   mask(treecover_3395_1km, roi_buf_3395),
#   filename = "out_pcs/treecover2000_pcs_masked.tif",
#   overwrite = TRUE,
#   wopt = list(
#     gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
#     datatype = "INT2S",
#     NAflag = -32768
#   )
# )

## GFC yearloss ----
pattern_lossyear <- paste0("^", GEE_tyRoi, "_GFC_yearLoss_lossyear_native-[0-9]+-[0-9]+\\.tif$")
tif_files_lossyear <- list.files(geedir, pattern = pattern_lossyear, full.names = TRUE)

sel_lossyear <- get_intersecting_tiles(tif_files_lossyear, roi_buf_3395)
tif_files_lossyear_roi <- sel_lossyear$tif_files_roi
roi_buf_ll <- sel_lossyear$roi_buf_ll

cat("yearloss tiles intersecting ROI:", length(tif_files_lossyear_roi), "\n")

# 1) VRT only for intersecting tiles
vrt(tif_files_lossyear_roi, filename = "temp/lossyear_native.vrt", overwrite = TRUE)
lossyear_vrt <- rast("temp/lossyear_native.vrt")

# 2) Crop early in source CRS
lossyear_crop_ll <- crop(lossyear_vrt, roi_buf_ll, snap = "out")

### STEP A: majority of 30m pixels with value >= 1 ----
# binary mask: 1 if yearloss exists, 0 otherwise
lossyear_any_ll <- ifel(!is.na(lossyear_crop_ll) & lossyear_crop_ll >= 1, 1, 0)

writeRaster(
  lossyear_any_ll,
  filename = "temp/lossyear_any_ll.tif",
  overwrite = TRUE,
  wopt = list(
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
    datatype = "INT1U",
    NAflag = 255
  )
)

Sys.sleep(30)
lossyear_any_ll <- rast("temp/lossyear_any_ll.tif")

# proportion of positive 30m cells inside each 1 km cell
lossyear_frac_1km <- project(
  lossyear_any_ll,
  template_3395,
  method = "mean",
  filename = "temp/lossyear_frac_1km.tif",
  overwrite = TRUE,
  wopt = list(
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
    datatype = "FLT4S",
    NAflag = -9999
  )
)

Sys.sleep(30)
lossyear_frac_1km <- rast("temp/lossyear_frac_1km.tif")

# majority rule: positive only if > 50% of 30m cells have value >= 1
lossyear_majority_1km <- ifel(lossyear_frac_1km > 0.5, 1, 0)

### STEP B: mode of positive years (1:24) only ----
lossyear_pos_ll <- ifel(lossyear_crop_ll >= 1, lossyear_crop_ll, NA)

writeRaster(
  lossyear_pos_ll,
  filename = "temp/lossyear_pos_ll.tif",
  overwrite = TRUE,
  wopt = list(
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
    datatype = "INT1U",
    NAflag = 255
  )
)

Sys.sleep(30)
lossyear_pos_ll <- rast("temp/lossyear_pos_ll.tif")

lossyear_mode_1km <- project(
  lossyear_pos_ll,
  template_3395,
  method = "mode",
  filename = "temp/lossyear_mode_1km.tif",
  overwrite = TRUE,
  wopt = list(
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
    datatype = "INT1U",
    NAflag = 255
  )
)

### STEP C: final 1 km yearloss ----
lossyear_3395_1km <- ifel(lossyear_majority_1km == 1, lossyear_mode_1km, 0)

writeRaster(
  lossyear_3395_1km,
  filename = "out_pcs/lossyear_pcs.tif",
  overwrite = TRUE,
  wopt = list(
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
    datatype = "INT1U",
    NAflag = 255
  )
)

writeRaster(
  mask(lossyear_3395_1km, roi_buf_3395),
  filename = "out_pcs/lossyear_pcs_masked.tif",
  overwrite = TRUE,
  wopt = list(
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
    datatype = "INT1U",
    NAflag = 255
  )
)

## GFC loss ----
pattern_loss <- paste0("^", GEE_tyRoi, "_GFC_loss_native-[0-9]+-[0-9]+\\.tif$")
tif_files_loss <- list.files(geedir, pattern = pattern_loss, full.names = TRUE)

sel_loss <- get_intersecting_tiles(tif_files_loss, roi_buf_3395)
tif_files_loss_roi <- sel_loss$tif_files_roi
roi_buf_ll <- sel_loss$roi_buf_ll

cat("loss tiles intersecting ROI:", length(tif_files_loss_roi), "\n")

# 1) VRT only for intersecting tiles
vrt(tif_files_loss_roi, filename = "temp/loss_native.vrt", overwrite = TRUE)
loss_vrt <- rast("temp/loss_native.vrt")

# 2) Crop early in source CRS
loss_crop_ll <- crop(loss_vrt, roi_buf_ll, snap = "out")

# 3) If source is already 0/1, no pre-processing needed
loss_frac_1km <- project(
  loss_crop_ll,
  template_3395,
  method = "mean",
  filename = "temp/loss_frac_1km.tif",
  overwrite = TRUE,
  wopt = list(
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
    datatype = "FLT4S",
    NAflag = -9999
  )
)

# 4) Majority rule
loss_3395_1km <- ifel(loss_frac_1km > 0.5, 1, 0)

writeRaster(
  loss_3395_1km,
  filename = "out_pcs/loss_pcs.tif",
  overwrite = TRUE,
  wopt = list(
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
    datatype = "INT1U",
    NAflag = 255
  )
)

writeRaster(
  mask(loss_3395_1km, roi_buf_3395),
  filename = "out_pcs/loss_pcs_masked.tif",
  overwrite = TRUE,
  wopt = list(
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
    datatype = "INT1U",
    NAflag = 255
  )
)

## GFC gain ----
pattern_gain <- paste0("^", GEE_tyRoi, "_GFC_gain_native-[0-9]+-[0-9]+\\.tif$")
tif_files_gain <- list.files(geedir, pattern = pattern_gain, full.names = TRUE)

sel_gain <- get_intersecting_tiles(tif_files_gain, roi_buf_3395)
tif_files_gain_roi <- sel_gain$tif_files_roi
roi_buf_ll <- sel_gain$roi_buf_ll

cat("gain tiles intersecting ROI:", length(tif_files_gain_roi), "\n")

# 1) VRT only for intersecting tiles
vrt(tif_files_gain_roi, filename = "temp/gain_native.vrt", overwrite = TRUE)
gain_vrt <- rast("temp/gain_native.vrt")

# 2) Crop early in source CRS
gain_crop_ll <- crop(gain_vrt, roi_buf_ll, snap = "out")

# 3) If source is already 0/1, no pre-processing needed
# project as fraction of positive 30m pixels inside each 1 km pixel
gain_frac_1km <- project(
  gain_crop_ll,
  template_3395,
  method = "mean",
  filename = "temp/gain_frac_1km.tif",
  overwrite = TRUE,
  wopt = list(
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
    datatype = "FLT4S",
    NAflag = -9999
  )
)

# 4) Majority rule
gain_3395_1km <- ifel(gain_frac_1km > 0.5, 1, 0)

writeRaster(
  gain_3395_1km,
  filename = "out_pcs/gain_pcs.tif",
  overwrite = TRUE,
  wopt = list(
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
    datatype = "INT1U",
    NAflag = 255
  )
)

writeRaster(
  mask(gain_3395_1km, roi_buf_3395),
  filename = "out_pcs/gain_pcs_masked.tif",
  overwrite = TRUE,
  wopt = list(
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
    datatype = "INT1U",
    NAflag = 255
  )
)

## GFC datamask ----
pattern_datamask <- paste0("^", GEE_tyRoi, "_GFC_datamask_native-[0-9]+-[0-9]+\\.tif$")
tif_files_datamask <- list.files(geedir, pattern = pattern_datamask, full.names = TRUE)
if (length(tif_files_datamask) == 0) stop("No matching tiles found.")

# 1) Virtual mosaic (fast, low RAM): build a VRT and open it
vrt(tif_files_datamask, filename = "temp/datamask_native.vrt", overwrite = TRUE)
datamask_vrt <- rast("temp/datamask_native.vrt")

# 2) Crop early in source CRS (saves a LOT of time before projection)
# We crop by the buffered ROI reprojected back to raster CRS (lon/lat)
roi_buf_ll <- project(roi_buf_3395, crs(datamask_vrt))
datamask_crop_ll <- crop(datamask_vrt, roi_buf_ll, snap = "out") # still lon/lat, and see notes above
# 
# writeRaster(
#   datamask_crop_ll,
#   filename = "out_gcs/datamask_gcs.tif",
#   overwrite = TRUE,
#   wopt = list(
#     gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
#     datatype = "INT1U",
#     NAflag = 255
#   )
# )

# 3) Project to EPSG:3395 at 1000 m
datamask_3395_1km <- project(
  datamask_crop_ll,
  template_3395,
  method = "near",
  filename = "out_pcs/datamask_pcs.tif",
  overwrite = TRUE,
  wopt = list(
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
    datatype = "INT1U",
    NAflag = 255
  )
)

writeRaster(
  mask(datamask_3395_1km, roi_buf_3395),
  filename = "out_pcs/datamask_pcs_masked.tif",
  overwrite = TRUE,
  wopt = list(
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
    datatype = "INT1U",
    NAflag = 255
  )
)

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
  pattern_modis <- paste0("^", GEE_tyRoi, "_modisLc_", LULCt1map_yr,"_LC_Type1_native-[0-9]+-[0-9]+\\.tif$")
  tif_files_modis <- list.files(geedir, pattern = pattern_modis, full.names = TRUE)
  if (length(tif_files_modis) == 0) stop("No matching tiles found.")
  
  # 1) Virtual mosaic (fast, low RAM): build a VRT and open it
  vrt(tif_files_modis, filename = "temp/modis_native.vrt", overwrite = TRUE)
  modis_vrt <- rast("temp/modis_native.vrt")
  
  # 2) Crop early in source CRS (saves a LOT of time before projection)
  # We crop by the buffered ROI reprojected back to raster CRS (lon/lat)
  roi_buf_ll <- project(roi_buf_3395, crs(modis_vrt))
  modis_crop_native <- crop(modis_vrt, roi_buf_ll, snap = "out") # still lon/lat, and see notes above
  
  # writeRaster(
  #   modis_crop_native,
  #   filename = "out_gcs/modis_gcs.tif",
  #   overwrite = TRUE,
  #   wopt = list(
  #     gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
  #     datatype = "INT2S",
  #     NAflag = -32768
  #   )
  # )
  
  # 3) Project to EPSG:3395 at 1000 m
  modis_3395_1km <- project(
    modis_crop_native,
    template_3395,
    method = "near",
    filename = "out_pcs/modis_pcs.tif",
    overwrite = TRUE,
    wopt = list(
      gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
      datatype = "INT2S",
      NAflag = -32768
    )
  )
  
  writeRaster(
    mask(modis_3395_1km, roi_buf_3395),
    filename = "out_pcs/modis_pcs_masked.tif",
    overwrite = TRUE,
    wopt = list(
      gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
      datatype = "INT2S",
      NAflag = -32768
    )
  )
  
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
  pattern_copernicus <- paste0("^", GEE_tyRoi, "_copernicusLc_", LULCt2map_yr,"_classification_discrete_classification_native-[0-9]+-[0-9]+\\.tif$")
  tif_files_copernicus <- list.files(geedir, pattern = pattern_copernicus, full.names = TRUE)
  if (length(tif_files_copernicus) == 0) stop("No matching tiles found.")
  
  # 1) Virtual mosaic (fast, low RAM): build a VRT and open it
  vrt(tif_files_copernicus, filename = "temp/copernicus_native.vrt", overwrite = TRUE)
  copernicus_vrt <- rast("temp/copernicus_native.vrt")
  
  # 2) Crop early in source CRS (saves a LOT of time before projection)
  # We crop by the buffered ROI reprojected back to raster CRS (lon/lat)
  roi_buf_ll <- project(roi_buf_3395, crs(copernicus_vrt))
  copernicus_crop_native <- crop(copernicus_vrt, roi_buf_ll, snap = "out") # still lon/lat, and see notes above
  
  # writeRaster(
  #   copernicus_crop_native,
  #   filename = "out_gcs/copernicus_gcs.tif",
  #   overwrite = TRUE,
  #   wopt = list(
  #     gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
  #     datatype = "INT2U",
  #     NAflag = 65535
  #   )
  # )
  
  # 3) Project to EPSG:3395 at 1000 m
  copernicus_3395_1km <- project(
    copernicus_crop_native,
    template_3395,
    method = "near",
    filename = "out_pcs/copernicus_pcs.tif",
    overwrite = TRUE,
    wopt = list(
      gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
      datatype = "INT2U",
      NAflag = 65535
    )
  )
  
  writeRaster(
    mask(copernicus_3395_1km, roi_buf_3395),
    filename = "out_pcs/copernicus_pcs_masked.tif",
    overwrite = TRUE,
    wopt = list(
      gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
      datatype = "INT1U",
      NAflag = 255
    )
  )
  
}

## Dynamic world ----
country_parameters %>%
  dplyr::filter(Var == "LULCt3map") %>%
  pull(ParCHR) -> LULCt3map
if (LULCt3map == "YES") {
  country_parameters %>%
    dplyr::filter(Var == "LULCt3map_name") %>%
    pull(ParCHR) -> LULCt3map_name
  country_parameters %>%
    dplyr::filter(Var == "LULCt3map_yr") %>%
    pull(ParCHR) -> LULCt3map_yr
  pattern_dw <- paste0("^", GEE_tyRoi, "_dynamic_world_", LULCt3map_yr,"_label_native-[0-9]+-[0-9]+\\.tif$")
  tif_files_dw <- list.files(geedir, pattern = pattern_dw, full.names = TRUE)
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

