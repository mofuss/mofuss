# Preliminary Step: Check Tile Information
# Before running the full geoprocessing, collect the number of tiles, their dimensions, and file sizes for each yearly folder.
# Code to Get Tile Info

library(terra)
library(dplyr)

# Define the base directory
base_dir <- "E:/agb3rdparties/dap.ceda.ac.uk/neodc/esacci/biomass/data/agb/maps/v5.01/geotiff"

# Get yearly folders
years <- list.dirs(base_dir, recursive = FALSE)

# Function to get raster info
get_raster_info <- function(year_folder) {
  tif_files <- list.files(year_folder, pattern = "\\.tif$", full.names = TRUE)
  info_list <- lapply(tif_files, function(f) {
    r <- rast(f)
    data.frame(
      Year = basename(year_folder),
      File = basename(f),
      Dimensions = paste(nrow(r), "x", ncol(r)),
      Resolution = paste(res(r)[1], "x", res(r)[2]),
      Size_MB = file.size(f) / 1e6
    )
  })
  do.call(rbind, info_list)
}

# Get info for all years
raster_info <- bind_rows(lapply(years, get_raster_info))

# # Display summary
# library(ace_tools)
# ace_tools::display_dataframe_to_user(name = "Raster Tile Info", dataframe = raster_info)

# Optimized Workflow for Processing
# 1. Load Required Libraries

library(terra)
library(sf)

# 2. Define Paths
base_dir <- "E:/agb3rdparties/dap.ceda.ac.uk/neodc/esacci/biomass/data/agb/maps/v5.01/geotiff"
output_dir <- "E:/processed_agb_esacci_v501"
admin_layer <- "D:/admin_regions/regions_adm0/mofuss_regions0.gpkg"
resample_ref <- "E:/agb3rdparties/AGB2010_pcs.tif"


#########################
year <- "2010"
year_folder <- file.path("E:/agb3rdparties/dap.ceda.ac.uk/neodc/esacci/biomass/data/agb/maps/v5.01/geotiff", year)

# Verify if the folder exists
if (!dir.exists(year_folder)) stop("The folder does not exist:", year_folder)

# List all files in the directory
all_files <- list.files(year_folder, full.names = TRUE)

# Print first 10 files
print(head(all_files, 10))

# Ensure correct file pattern
agb_files <- list.files(year_folder, pattern = "AGB-MERGED-100m.*\\.tif$", full.names = TRUE)
agb_sd_files <- list.files(year_folder, pattern = "AGB_SD-MERGED-100m.*\\.tif$", full.names = TRUE)

# If no files found, check recursively
if (length(agb_files) == 0 || length(agb_sd_files) == 0) {
  agb_files <- list.files(year_folder, pattern = "AGB-MERGED-100m.*\\.tif$", full.names = TRUE, recursive = TRUE)
  agb_sd_files <- list.files(year_folder, pattern = "AGB_SD-MERGED-100m.*\\.tif$", full.names = TRUE, recursive = TRUE)
}

cat("AGB Files Found:", length(agb_files), "\n")
print(agb_files)

cat("AGB-SD Files Found:", length(agb_sd_files), "\n")
print(agb_sd_files)


library(gdalUtilities)
library(gdalUtilities)

# Check GDAL version
gdalUtilities::gdalinfo()

gdalinfo --version




# Define output VRT file path
agb_vrt <- "E:/processed_agb/AGB_2010.vrt"
agb_sd_vrt <- "E:/processed_agb/AGB_SD_2010.vrt"

# Create Virtual Raster (VRT) for AGB
gdalbuildvrt(agb_vrt, agb_files, overwrite = TRUE, separate = FALSE)

# Create Virtual Raster (VRT) for AGB-SD
gdalbuildvrt(agb_sd_vrt, agb_sd_files, overwrite = TRUE, separate = FALSE)

# Load VRT as a SpatRaster
agb_mosaic <- rast(agb_vrt)
agb_sd_mosaic <- rast(agb_sd_vrt)

# Plot for verification (optional)
plot(agb_mosaic)
plot(agb_sd_mosaic)


