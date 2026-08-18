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
## Merge and rescale to 1km ----
wp_files <- list.files(
  pattern = "2020_UNadj_constrained.*\\.tif$",
  full.names = TRUE
)

# 1) make a raster collection from country rasters
wp_sprc <- sprc(wp_files)

# 2) merge into one 100 m raster (in memory first)
wp_100m <- merge(wp_sprc)

# set layer name
names(wp_100m) <- "pop_2020"

# now write to disk
writeRaster(
  wp_100m,
  "out_gcs/wp_global100m_gcs.tif",
  overwrite = TRUE,
  wopt = list(gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"))
)

# Re-read WorldPop 100m
wp_100m2 <- rast("out_gcs/wp_global100m_gcs.tif")

# 3) Aggregate 10x10 cells using SUM
wp_1km_gcs <- aggregate(
  wp_100m2,
  fact = 10,
  fun = sum,
  na.rm = TRUE
)

# Set layer name BEFORE writing
names(wp_1km_gcs) <- "pop_2020"

# Now write to disk as float for debugging
writeRaster(
  wp_1km_gcs,
  "out_gcs/wp_global1000m_gcs_flt.tif",
  overwrite = TRUE,
  wopt = list(
    datatype = "FLT4S",
    NAflag = -9999,
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES")
  )
)

wp_1km_gcs_int <- round(wp_1km_gcs)
names(wp_1km_gcs_int) <- "pop_2020"

# Now write to disk as rounded integer
writeRaster(
  wp_1km_gcs_int,
  "out_gcs/wp_global1000m_gcs.tif",
  overwrite = TRUE,
  wopt = list(
    datatype = "INT4S",
    NAflag = -9999,
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES")
  )
)

## Project into World Mercator ----
# --- 1. Source raster (counts, GCS) ---
wp_gcs <- rast("out_gcs/wp_global1000m_gcs_flt.tif")

# --- 2. Target grid (Mercator, empty template) ---
template_3395 <- rast(paste0(geedir, "/temp/template_3395_1km.tif"))

# --- 3. The correct method: use method="sum" with a coverage-fraction kernel ---
# terra's project() with method="sum" does area-weighted aggregation, 
# which is mass-conserving by construction.

wp_1km_pcs <- project(
  wp_gcs,
  template_3395,
  method = "sum"
) |>
  clamp(lower = 0) |>
  writeRaster(
    "out_pcs/wp_global1000m_pcs_flt.tif",
    overwrite = TRUE,
    wopt = list(
      datatype = "FLT4S",
      NAflag   = -9999,
      gdal     = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES")
    )
  )

wp_1km_pcs_int <- round(wp_1km_pcs)
names(wp_1km_pcs_int) <- "pop_2020"

writeRaster(
  wp_1km_pcs_int,
  "out_pcs/wp_global1000m_pcs.tif",
  overwrite = TRUE,
  wopt = list(
    datatype = "INT4S",
    NAflag = -9999,
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES")
  )
)

# Comparison of methods: new 1km and old1km to 100m lat/long ----
## Automatic ----
# --- load rasters ---
wp_100m2 <- rast("out_gcs/wp_global100m_gcs.tif")
wp_4326_1km2 <- rast("out_gcs/wp_global1000m_gcs_flt.tif")
wp_3395_1km2 <- rast("out_pcs/wp_global1000m_pcs_flt.tif")
wp_3395_1km2_old <- rast("G:/Mi unidad/webpages/2026_MoFuSSGlobal_Datasets/1km_datasets/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/wp_global1000m_pcs.tif")

# --- load polygons ---
mofuss_regions_4326 <- vect(paste0(admindir, "/regions_adm0/mofuss_regions0.gpkg"))
mofuss_regions_3395 <- vect(paste0(admindir, "/regions_adm0_p/mofuss_regions0_p.gpkg"))

# --- list of countries ---
countries <- unique(mofuss_regions_4326$GID_0)   # Select batch as needed
# countries <- c("IND", "MEX", "BRA", "ZMB", "ZWE", "KEN", "MWI", "GHA", "TZA")
# countries <- c("KEN", "MWI", "GHA", "COL", "TZA")

# --- function ---
get_country_sums <- function(iso) {
  
  # polygons
  pol100m <- mofuss_regions_4326[mofuss_regions_4326$GID_0 == iso, ]
  pol4326 <- mofuss_regions_4326[mofuss_regions_4326$GID_0 == iso, ]
  pol3395 <- mofuss_regions_3395[mofuss_regions_3395$GID_0 == iso, ]
  
  # --- 100m GCS (reference) ---
  r100 <- mask(crop(wp_100m2, pol100m), pol100m)
  sum100 <- global(r100, "sum", na.rm = TRUE)[1,1]
  
  # --- new 1km GCS ---
  r4326 <- mask(crop(wp_4326_1km2, pol4326), pol4326)
  sum4326 <- global(r4326, "sum", na.rm = TRUE)[1,1]
  
  # --- new 1km PCS ---
  r3395 <- mask(crop(wp_3395_1km2, pol3395), pol3395)
  sum3395 <- global(r3395, "sum", na.rm = TRUE)[1,1]
  
  # --- old 1km PCS ---
  r_old <- mask(crop(wp_3395_1km2_old, pol3395), pol3395)
  sumold <- global(r_old, "sum", na.rm = TRUE)[1,1]
  
  data.frame(
    iso3 = iso,
    sum_100m = sum100,
    sum_1km_gcs = sum4326,
    sum_1km_pcs = sum3395,
    sum_1km_pcs_old = sumold,
    diff_gcs = sum4326 - sum100,
    pct_gcs = 100 * (sum4326 - sum100) / sum100,
    diff_pcs = sum3395 - sum100,
    pct_pcs = 100 * (sum3395 - sum100) / sum100,
    diff_old = sumold - sum100,
    pct_old = 100 * (sumold - sum100) / sum100
  )
}

# --- run for all countries ---
results <- do.call(rbind, lapply(countries, get_country_sums))

# --- clean output ---
results$pct_gcs <- round(results$pct_gcs, 3)
results$pct_pcs <- round(results$pct_pcs, 3)
results$pct_old <- round(results$pct_old, 3)

print(results)
write.csv(results, "temp/results_table.csv", row.names = FALSE)

# --- summary stats ---
summary(results$pct_gcs)
summary(results$pct_pcs)
summary(results$pct_old)

sink("temp/results_summary.txt")

cat("Summary pct_gcs:\n")
print(summary(results$pct_gcs))

cat("\nSummary pct_pcs:\n")
print(summary(results$pct_pcs))

cat("\nSummary pct_old:\n")
print(summary(results$pct_old))

sink()

# Copy 2 MoFuSS ----
copy2mofussfiles_wp <- c("out_gcs/wp_global1000m_gcs.tif",
                         "out_pcs/wp_global1000m_pcs.tif")
for (f in copy2mofussfiles_wp) {
  file.copy(from=f, 
            to=paste0(demanddir,"/demand_in/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

