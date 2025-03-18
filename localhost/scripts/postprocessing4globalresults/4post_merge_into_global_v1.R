# Copyright 2025 Stockholm Environment Institute ----

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# MoFuSS 
# Version 1
# Date: Jan 2025
# Description: Merge all datasets into NatureBase format

# 2dolist ----

# Internal parameters ----
temdirdefined = 1
string_pattern_yes <- "2050" #String pattern to be searched when selecting folders for the rasters' geocomputation
string_pattern_no <- "Cou" #String pattern to be searched when selecting folders for the rasters' geocomputation


# Define all folders based on node ----
# Detect OS and node name
os <- Sys.info()["sysname"]
node_name <- Sys.info()[["nodename"]]
cat(os,node_name)

if (os == "Windows" & node_name == "WINLANASE") {
  #ADD node
  demanddir <- "F:/demand"
  admindir <- "F:/admin_regions"
  emissionsdir <- "F:/emissions"
  rTempdir <- "F:/rTemp"
  
} else if (os == "Windows" & node_name == "ASUSLAP"){
  #ADD node
  demanddir <- "D:/demand"
  admindir <- "D:/admin_regions"
  emissionsdir <- "D:/emissions"
  rTempdir <- "D:/rTemp"
  
} else if (os == "Windows" & node_name == "EDITORIALCIGA"){
  #ADD node
  demanddir <- "E:/demand"
  admindir <- "E:/admin_regions"
  emissionsdir <- "E:/emissions"
  rTempdir <- "E:/rTemp"
  
} else if (os == "Linux" & node_name == "linux-c3"){
  #ADD node
  demanddir <- "/home/mofuss/demand"
  admindir <- "/home/mofuss/admin_regions"
  emissionsdir <- "/home/mofuss/emissions"
  rTempdir <- "/home/mofuss/rTemp"
  
}

# Erase all plots in R Studio
Sys.sleep(2)
for (p in 1:100) {
  if (length(dev.list()!=0)) {
    dev.off()
  }
}
Sys.sleep(3)

# Load packages ----
library(terra)
# terraOptions(steps = 55)
if (temdirdefined == 1) {
  terraOptions(tempdir = rTempdir)
  # List all files and directories inside the folder
  contents <- list.files(rTempdir, full.names = TRUE, recursive = TRUE)
  # Delete the contents but keep the folder
  unlink(contents, recursive = TRUE, force = TRUE)
}
# terraOptions(memfrac=0.9)
# terraOptions(progress=0)
# library(compare)
library(dplyr)
library(fs)
library(readr)
library(tcltk)
library(tibble)
library(tidyverse)

# # Define the directory to search
# setwd(tk_choose.dir(default = getwd(), caption = "Define the emissions directory where results are saved"))
# search_path <- getwd()
search_path <- emissionsdir

# List all directories in the specified path
all_dirs <- dir_ls(search_path, type = "directory")

# Filter directories that match string_pattern_yes and do not match string_pattern_no
emissions_dirs <- all_dirs[grepl(string_pattern_yes, all_dirs) & !grepl(string_pattern_no, all_dirs)]
emissions_dirs #MUST BE AT LEAST TWO?? FIX THIS

# Define the list of files to merge
tables_to_merge <- c(
  paste0("AE",string_pattern_yes,"_wm_tpp_sum_merged.csv"),
  paste0("AE",string_pattern_yes,"_gcs_tpp_sum_merged.csv")
)

# Define the list of files to merge
rasters_to_merge <- c(
  paste0("AE",string_pattern_yes,"_gcs_tpp_mean.tif"),
  paste0("AE",string_pattern_yes,"_gcs_tpp_se.tif"),
  paste0("AE",string_pattern_yes,"_gcs_tpyr_mean.tif"),
  paste0("AE",string_pattern_yes,"_gcs_tpyr_se.tif"),
  paste0("AE",string_pattern_yes,"_gcs_thayr_mean.tif"),
  paste0("AE",string_pattern_yes,"_gcs_thayr_se.tif"),
  paste0("AE",string_pattern_yes,"_wm_thay_mean.tif"),
  paste0("AE",string_pattern_yes,"_wm_thay_se.tif"),
  paste0("AE",string_pattern_yes,"_wm_tpp_mean.tif"),
  paste0("AE",string_pattern_yes,"_wm_tpp_se.tif")
)

# Define the output directory for merged files
merged_dir <- paste0(search_path,"/naturebase")
dir.create(merged_dir, showWarnings = FALSE, recursive = TRUE)
# List all files in the directory containing the pattern
files_to_delete <- list.files(merged_dir, pattern = string_pattern_yes, full.names = TRUE)
# Check if any files were found before deleting
if (length(files_to_delete) > 0) {
  # Remove files
  file.remove(files_to_delete)
  # Print status
  cat("Deleted files:\n", paste(files_to_delete, collapse = "\n"), "\n")
} else {
  cat("No files found containing:", string_pattern_yes, "\n")
}
cat("Deletion process completed!\n")

# Merge CSV files ----
for (file_tab in tables_to_merge[grepl("\\.csv$", tables_to_merge)]) {
  csv_list <- lapply(emissions_dirs, function(dir) {
    file_path <- file.path(dir, file_tab)
    if (file.exists(file_path)) {
      read_csv(file_path, show_col_types = FALSE)
    } else {
      NULL
    }
  })
  
  merged_csv <- bind_rows(csv_list)
  
  # Save merged CSV
  output_csv <- file.path(merged_dir, file_tab)
  write_csv(merged_csv, output_csv)
  message("Saved: ", output_csv)
}

# Merge TIF files ----
# Loop over each file type
for (file_name in rasters_to_merge) {
  # Construct full paths for this file across all emission directories
  raster_paths <- file.path(emissions_dirs, file_name)
  
  # Filter only existing files to avoid errors
  raster_paths <- raster_paths[file.exists(raster_paths)]
  
  if (length(raster_paths) > 0) {
    # Load rasters into a list
    raster_list <- lapply(raster_paths, rast)
    
    # Mosaic rasters
    mosaic_raster <- do.call(terra::mosaic, raster_list)
    
    # Define output filename
    output_file <- file.path(merged_dir, file_name)
    
    # Save the mosaicked raster
    writeRaster(mosaic_raster, output_file, overwrite = TRUE)
    
    # Print status update
    cat("Mosaicked and saved:", output_file, "\n")
  } else {
    cat("No valid files found for:", file_name, "\n")
  }
}

cat("All mosaicking processes completed!\n")

# SE as percentage of mean ----
# List all _se.tif files in the directory
sd_files <- list.files(merged_dir, pattern = "_se.tif$", full.names = TRUE)

# Loop through each _se file and find its corresponding _mean file
for (sd_path in sd_files) {
  # Derive the corresponding _mean file name
  mean_path <- sub("_se.tif$", "_mean.tif", sd_path)
  
  # Check if the corresponding _mean file exists
  if (file.exists(mean_path)) {
    # Load both rasters
    sd_raster <- rast(sd_path)
    mean_raster <- rast(mean_path)
    
    # Avoid division by zero
    mean_raster[mean_raster == 0] <- NA
    
    # Compute absolute percentage SD and round to integers
    seperc_raster <- round(abs(sd_raster / mean_raster) * 100, 0)
    
    # Define output filename (_seperc.tif)
    output_seperc <- sub("_se.tif$", "_seperc.tif", sd_path)
    
    # Save the percentage standard deviation raster
    writeRaster(seperc_raster, output_seperc, overwrite = TRUE)
    
    # Create a categorical raster based on seperc_raster values
    seperc_class <- classify(
      seperc_raster, 
      c(0, 33, 66, 100, Inf),  # Breakpoints
      c(1, 2, 3, 4)  # Categories (0 = High, 1 = Moderate, 2 = Low, 3 = Very Low)
    )
    
    # # This works better but takes way to long
    # # Create categorical raster manually using ifel()
    # seperc_class <- seperc_raster
    # seperc_class <- ifel(seperc_raster > 100, 4, seperc_class)  # Very Low (>100)
    # seperc_class <- ifel(seperc_raster > 66 & seperc_raster <= 100, 3, seperc_class)  # Low (66-100)
    # seperc_class <- ifel(seperc_raster > 33 & seperc_raster <= 66, 2, seperc_class)  # Moderate (33-66)
    # seperc_class <- ifel(seperc_raster >= 0 & seperc_raster <= 33, 1, seperc_class)  # High (0-33)
    
    # Define output filename (_seperc_class.tif)
    output_class <- sub("_se.tif$", "_seperc_class.tif", sd_path)
    
    # Save the categorical raster
    writeRaster(seperc_class, output_class, overwrite = TRUE)
    
    # Print progress
    cat("Processed and saved:", output_seperc, "and", output_class, "\n")
  } else {
    cat("Skipping:", sd_path, "- Corresponding mean file not found.\n")
  }
}


unlink(list.files(merged_dir, pattern = "\\.xml$", full.names = TRUE))

cat("All SD percentage calculations and classifications completed!\n")



