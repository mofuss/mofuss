# MoFuSS
# Version 4
# Date: Ago 2025

# 2dolist
# Check Terra's handling of VRT, sometimes it doesn't work and need to be updated
# Need to restart R and windows before running this script

# Internal parameters
# cpu = "nrb" #"alien" # vs nrb
# if (cpu == "alien") {
#   gdrivedir <- "G:/My Drive/webpages/2024_MoFuSSGlobal_Datasets/"
# } else {
#   gdrivedir <- "G:/Mi Unidad/webpages/2024_MoFuSSGlobal_Datasets/"
# }

# Define all folders based on node ----
# Detect OS and node name
os <- Sys.info()["sysname"]
node_name <- Sys.info()[["nodename"]]
cat(os,node_name)

# Load packages ----
library(terra)
terraOptions(steps = 55)
terraOptions(progress=0)
library(tidyterra)
library(tidyverse)
library(sf)
library(mapview)
library(tictoc)
library(mapview)
library(readxl)
library(hacksaw)

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

if (exists("wpdir") == FALSE) {
  choose_directory213 = function(caption = "Choose the directory where WorldPop (wp) files are") {
    if(.Platform$OS.type == "unix")  {
      setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
    } else {
      setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
    }
  }
  choose_directory213()
  wpdir <- getwd()
}

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

# Optional: tune terra temp/memory (adjust tempdir to a fast disk if possible)
terraOptions(
  tempdir = terraTempdir,
  memfrac = 0.8,
  progress = 3
)
unlink(paste0(terraTempdir, "/*"), recursive = TRUE)

setwd(wpdir)

# WORLDPOP 1km ----
## Global ----
wp_files <- list.files(
  pattern = "2020_UNadj_constrained.*\\.tif$",
  full.names = TRUE
)

# 1) make a raster collection from country rasters
wp_sprc <- sprc(wp_files)

# 2) merge into one real global 100 m raster on disk
wp_100m <- merge(
  wp_sprc,
  filename = file.path("out_gcs/wp_global100m_gcs.tif"),
  overwrite = TRUE,
  wopt = list(gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"))
)

# Re-read WorlPop 100m and World Mercator template 
wp_100m2 <- rast("out_gcs/wp_global100m_gcs.tif")
template_3395   <- rast(paste0(geedir,"/temp/template_3395_1km.tif"))

# 1) source cell area in m2
a_src <- cellSize(
  wp_100m2,
  unit = "m",
  filename = "temp/a_src.tif",
  overwrite = TRUE
)

# 2) counts -> density
dens_src <- wp_100m2 / a_src

# 3) project density to aligned 3395 template
dens_3395 <- project(
  dens_src,
  template_3395,
  method = "bilinear",
  filename = "temp/dens_3395.tif",
  overwrite = TRUE
)

# 4) target cell area in m2
a_tgt   <- rast(paste0(geedir,"/temp/pixel_area_trueEarth_3395_1km_m2.tif"))

# 5) density -> counts per target cell
pop_3395 <- dens_3395 * a_tgt
names(pop_3395) <- "pop_2020"

# 6) save
writeRaster(
  pop_3395,
  "out_pcs/wp_global1000m_pcs.tif",
  overwrite = TRUE,
  wopt = list(gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"))
)

## Copy 2 MoFuSS ----
copy2mofussfiles_wp <- c("out_pcs/wp_global1000m_pcs.tif")
for (f in copy2mofussfiles_wp) {
  file.copy(from=f, 
            to=paste0(demanddir,"/demand_in/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}


# Comparison of methods: new 1km and old1km to 100m lat/long ----
## Manual ----
# Helper to check that the 1km projected raster has good population estimates for 2020
wp_1km2 <- rast("out_pcs/wp_global1000m_pcs.tif")
mofuss_regions_p <- vect(paste0(admindir,"/regions_adm0_p/mofuss_regions0_p.gpkg"))
pol1km <- mofuss_regions_p[mofuss_regions_p$GID_0 == "ZMB", ]
pol1km <- mofuss_regions_p[mofuss_regions_p$NAME_0 == "Bangladesh", ]
plot(pol1km)
# crop + mask merged global raster
r_crop1km <- crop(wp_1km2, pol1km)
r_mask1km <- mask(r_crop1km, pol1km)
# sum from merged raster
sum_global1km <- global(r_mask1km, "sum", na.rm = TRUE)[1,1]

# # Helper to check that the 100m raster has good population estimates for 2020
wp_100m2 <- rast("out_gcs/wp_global100m_gcs.tif")
mofuss_regions <- vect(paste0(admindir,"/regions_adm0/mofuss_regions0.gpkg"))
unique(mofuss_regions$GID_0)
pol100m <- mofuss_regions[mofuss_regions$GID_0 == "ZMB", ]
pol100m <- mofuss_regions[mofuss_regions$NAME_0 == "Bangladesh", ]
plot(pol100m)
# crop + mask merged global raster
r_crop100m <- crop(wp_100m2, pol100m)
r_mask100m <- mask(r_crop100m, pol100m)
# sum from merged raster
sum_global100m <- global(r_mask100m, "sum", na.rm = TRUE)[1,1]
sum_global100m
sum_global1km

# Helper to check that the OLD 1km projected raster has good population estimates for 2020
wp1kmold <- rast("G:/Mi unidad/webpages/2026_MoFuSSGlobal_Datasets/1km_datasets/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/wp_global1000m_pcs.tif")
mofuss_regions1kmold <- vect("G:/Mi unidad/webpages/2026_MoFuSSGlobal_Datasets/1km_datasets/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/mofuss_regions0_p.gpkg")
pol1kmold <- mofuss_regions1kmold[mofuss_regions1kmold$GID_0 == "ZMB", ]
pol1kmold <- mofuss_regions1kmold[mofuss_regions1kmold$NAME_0 == "Bangladesh", ]
# crop + mask merged global raster
r_crop1kmold <- crop(wp1kmold, pol1kmold)
r_mask1kmold <- mask(r_crop1kmold, pol1kmold)
# sum from merged raster
sum_global_old <- global(r_mask1kmold, "sum", na.rm = TRUE)[1,1]
sum_global_old

## Automatic ----
# --- load rasters ---
wp_100m2  <- rast("out_gcs/wp_global100m_gcs.tif")
wp_1km2   <- rast("out_pcs/wp_global1000m_pcs.tif")
wp1kmold  <- rast("G:/Mi unidad/webpages/2026_MoFuSSGlobal_Datasets/1km_datasets/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/wp_global1000m_pcs.tif")

# --- load polygons ---
mofuss_regions     <- vect(paste0(admindir,"/regions_adm0/mofuss_regions0.gpkg"))
mofuss_regions_p   <- vect(paste0(admindir,"/regions_adm0_p/mofuss_regions0_p.gpkg"))
mofuss_regions_old <- vect("G:/Mi unidad/webpages/2026_MoFuSSGlobal_Datasets/1km_datasets/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/mofuss_regions0_p.gpkg")

# --- list of countries ---
countries <- unique(mofuss_regions$GID_0)

# --- function ---
get_country_sums <- function(iso) {
  
  # polygons
  pol100m <- mofuss_regions[mofuss_regions$GID_0 == iso, ]
  pol1km  <- mofuss_regions_p[mofuss_regions_p$GID_0 == iso, ]
  polold  <- mofuss_regions_old[mofuss_regions_old$GID_0 == iso, ]
  
  # --- 100m ---
  r100 <- mask(crop(wp_100m2, pol100m), pol100m)
  sum100 <- global(r100, "sum", na.rm = TRUE)[1,1]
  
  # --- new 1km ---
  r1km <- mask(crop(wp_1km2, pol1km), pol1km)
  sum1km <- global(r1km, "sum", na.rm = TRUE)[1,1]
  
  # --- old 1km ---
  r_old <- mask(crop(wp1kmold, polold), polold)
  sumold <- global(r_old, "sum", na.rm = TRUE)[1,1]
  
  data.frame(
    iso3 = iso,
    sum_100m = sum100,
    sum_1km_new = sum1km,
    sum_1km_old = sumold,
    diff_new = sum1km - sum100,
    pct_new = 100 * (sum1km - sum100) / sum100,
    diff_old = sumold - sum100,
    pct_old = 100 * (sumold - sum100) / sum100
  )
}

# --- run for all countries ---
results <- do.call(rbind, lapply(countries, get_country_sums))

# --- clean output ---
results$pct_new <- round(results$pct_new, 3)
results$pct_old <- round(results$pct_old, 3)

print(results)

# --- summary stats ---
summary(results$pct_new)
summary(results$pct_old)
