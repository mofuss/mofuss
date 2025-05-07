## Copyright 2025 Stockholm Environment Institute ----

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
# Version 4
# Date: Jan 2025
# Que hace este scripe????

# 2dolist

# Internal parameters ----
temdirdefined = 1
MC = 30
processingversion <- "globalsouth_mofuss_bindingfolder_global_se/"
# replace_node_files <- 0 # This will erase and overwrite all webmofuss results from each region into the GDrive folder
# rasters <- 1
# endyr <- "2050" # 2035/
# scenario <- "bau" # sce1/
# montecarlo <- 30
# bins <- 1

# Define all folders based on node ----
# Detect OS and node name
os <- Sys.info()["sysname"]
node_name <- Sys.info()[["nodename"]]
cat(os,node_name)

if (os == "Windows" & node_name == "WINLANASE") {
  #ADD node
  #demanddir <- "F:/demand"
  #admindir <- "F:/admin_regions"
  #emissionsdir <- "F:/emissions"
  rTempdir <- "F:/rTemp"
  
} else if (os == "Windows" & node_name == "ASUSLAP"){
  #ADD node
  # demanddir <- "D:/demand"
  # admindir <- "D:/admin_regions"
  # emissionsdir <- "D:/emissions"
  rTempdir <- "D:/rTemp"
  
} else if (os == "Windows" & node_name == "WINCIGA"){
  #ADD node
  # demanddir <- "D:/demand"
  # admindir <- "D:/admin_regions"
  # emissionsdir <- "D:/emissions"
  rTempdir <- "D:/rTemp"
  
} else if (os == "Windows" & node_name == "EDITORIALCIGA"){
  #ADD node
  # demanddir <- "E:/demand"
  # admindir <- "E:/admin_regions"
  # emissionsdir <- "E:/emissions"
  rTempdir <- "E:/rTemp"
  
} else if (os == "Linux" & node_name == "linux-c3"){
  #ADD node
  demanddir <- "/home/mofuss/demand"
  admindir <- "/home/mofuss/admin_regions"
  emissionsdir <- "/home/mofuss/emissions"
  rTempdir <- "/home/mofuss/rTemp"
  
} else if (os == "Windows" & node_name == "NRBV1"){
  #ADD node
  #demanddir <- "D:/demand"
  #admindir <- "D:/admin_regions"
  #emissionsdir <- "D:/emissions"
  rTempdir <- "D:/rTemp"
  
}

# Erase all plots in R Studio
Sys.sleep(2)
for (p in 1:100) {
  if (length(dev.list()!=0)) {
    dev.off()
  }
}
Sys.sleep(3)

# Load libraries ----
library(terra)
if (temdirdefined == 1) {
  terraOptions(tempdir = rTempdir)
}
library(dplyr)
library(fs)
library(googletraffic)
library(lwgeom)
library(progress)
library(raster)
library(sf)
library(svDialogs)
library(tcltk)
library(tidyverse)
# library(rmapshaper)
# check_sys_mapshaper()
# system("mapshaper --version")

# Define the directory to search
setwd(tk_choose.dir(default = getwd(), caption = "Define the directory to search"))
search_path <- getwd()

# List all directories in the specified path
all_dirs <- dir_ls(search_path, type = "directory")

# Filter directories containing 'adm0'
adm0_dirs <- all_dirs[grepl("adm0", all_dirs)]
# # Define a particular directory when needed:
# adm0_dirs <- c("E:/ASIA_adm0_bangladesh_apr2024",
#                "E:/ASIA_adm0_bhutan_apr2024",
#                "E:/ASIA_adm0_india_apr2024",
#                "E:/ASIA_adm0_middleeast_apr2024",
#                "E:/ASIA_adm0_pakistan_apr2024")

# # Replace node files ----
# if (replace_node_files == 1){
#   
#   # Loop through each directory and delete all files and subdirectories
#   for (dir in destination_dirs) {
#     # Use recursive = TRUE to delete all subdirectories and files within
#     # Add force = TRUE to ensure even read-only files are removed
#     unlink(dir, recursive = TRUE, force = TRUE)
#   }
#   
#   # Loop through each source and destination directory
#   for (i in seq_along(source_dirs)) {
#     # Check if the destination directory exists; if not, create it
#     if (!dir.exists(paste0(destination_dirs[i],taildir))) {
#       dir.create(paste0(destination_dirs[i],taildir), recursive = TRUE, showWarnings = TRUE)
#     }
#     
#     # List all files in the source directory
#     files <- list.files(paste0(source_dirs[i],taildir), full.names = TRUE)
#     
#     # Copy each file to the destination directory
#     file.copy(files, paste0(destination_dirs[i],taildir), overwrite = TRUE)
#   }
# }

if (os == "Windows" & node_name == "WINLANASE") {
  #ADD node
  Gdrivedir <- "G:/Mi unidad/webpages/2024_MoFuSSGlobal_Datasets/mofussDS_v2/" # Update based on every node
  source_dirs <- basename(adm0_dirs)
  destination_dirs <- paste0(Gdrivedir,processingversion,node_name)
  
} else if (os == "Windows" & node_name == "ASUSLAP"){
  #ADD node
  Gdrivedir <- "G:/Mi unidad/webpages/2024_MoFuSSGlobal_Datasets/mofussDS_v2/" # Update based on every node
  source_dirs <- basename(adm0_dirs)
  destination_dirs <- paste0(Gdrivedir,processingversion,node_name)
  
} else if (os == "Windows" & node_name == "WINCIGA"){
  #ADD node
  Gdrivedir <- "G:/Mi unidad/webpages/2024_MoFuSSGlobal_Datasets/mofussDS_v2/" # Update based on every node
  source_dirs <- basename(adm0_dirs)
  destination_dirs <- paste0(Gdrivedir,processingversion,node_name)
  
} else if (os == "Windows" & node_name == "EDITORIALCIGA"){
  #ADD node
  Gdrivedir <- "G:/My Drive/webpages/2024_MoFuSSGlobal_Datasets/mofussDS_v2/" # Update based on every node
  source_dirs <- basename(adm0_dirs)
  destination_dirs <- paste0(Gdrivedir,processingversion,node_name)
  
} else if (os == "Linux" & node_name == "linux-c3"){
  #ADD node
  
} else if (os == "Windows" & node_name == "NRBV1"){
  #ADD node
  Gdrivedir <- "G:/Mi unidad/webpages/2024_MoFuSSGlobal_Datasets/mofussDS_v2/" # Update based on every node
  source_dirs <- basename(adm0_dirs)
  destination_dirs <- paste0(Gdrivedir,processingversion,node_name)
  
}

# JJ chunk part 1 ----

destination_dir <- destination_dirs
# destination_dir <- "C:/Users/aghil/Documents/JJ_output_test_Asus"
# Create destination directory if it doesn't exist
dir_create(destination_dir)
unlink(paste0(destination_dir,"/*.*"), recursive = TRUE)
Sys.sleep(10)

# Files to mosaic
file_names <- c(
  "Growth11.tif",
  "Growth_less_harv20.tif",
  paste0("Harvest_tot", 11:20, ".tif")
)

# Loop through debugging_1 to debugging_30
for (i in 1:MC) {
  dbg_folder <- paste0("debugging_", i)
  dbg_suffix <- sprintf("_m_d%02d", i)
  
  for (file_name in file_names) {
    rasters_to_merge <- list()
    
    for (adm0 in adm0_dirs) {
      file_path <- file.path(adm0, dbg_folder, file_name)
      if (file_exists(file_path)) {
        r <- rast(file_path)
        
        if (length(rasters_to_merge) == 0) {
          ref_crs <- crs(r)
          rasters_to_merge <- list(r)
        } else {
          r_proj <- project(r, ref_crs)
          rasters_to_merge <- append(rasters_to_merge, list(r_proj))
        }
      }
    }
    
    if (length(rasters_to_merge) > 1) {
      merged_r <- do.call(merge, rasters_to_merge)
      output_name <- sub(".tif$", paste0(dbg_suffix, ".tif"), file_name)
      output_path <- file.path(destination_dir, output_name)
      writeRaster(merged_r, output_path, overwrite = TRUE)
      cat("✅ Saved:", output_path, "\n")
    } else if (length(rasters_to_merge) == 1) {
      # Save single raster as-is
      output_name <- sub(".tif$", paste0(dbg_suffix, ".tif"), file_name)
      output_path <- file.path(destination_dir, output_name)
      writeRaster(rasters_to_merge[[1]], output_path, overwrite = TRUE)
      cat("⚠️ Only one raster found; saved without merge:", output_path, "\n")
    } else {
      cat("⚠️ Skipped (no valid rasters):", file_name, "in", dbg_folder, "\n")
    }
  }
}

# Process final outputs using mosaicked rasters
for (i in 1:MC) {
  dbg_suffix <- sprintf("_m_d%02d", i)
  
  # ---- AGB Loss: Growth_less_harv11 > Growth_less_harv20 ----
  agb11 <- rast(file.path(destination_dir, paste0("Growth11", dbg_suffix, ".tif")))
  agb20 <- rast(file.path(destination_dir, paste0("Growth_less_harv20", dbg_suffix, ".tif")))
  # nrb <- agb11 > agb20
  # nrb_vals <- mask(agb11, nrb, maskvalues = FALSE)
  nrb_bin2020_2030 <- agb11 - agb20 # Bin will be 2020-2030
  nrb_bin2020_2030[nrb_bin2020_2030 <= 0] = NA 
  # plot(nrb_bin2020_2030)
  # nrb_sum_bin2020_2030 <- as.data.frame(zonal(nrb_bin2020_2030, admm, 'sum')) %>%
  #   as.data.table() %>%
  #   setnames(.,"sum", "NRB_2020_2030") %>%
  #   dplyr::select(!zone)
  writeRaster(nrb_bin2020_2030, file.path(destination_dir, paste0("nrb", dbg_suffix, ".tif")), overwrite = TRUE)
  
  # ---- Harvest Sum: Harvest_tot11 to Harvest_tot20 ----
  harv_files <- paste0("Harvest_tot", 11:20, dbg_suffix, ".tif")
  harv_stack <- lapply(harv_files, function(f) rast(file.path(destination_dir, f)))
  harv_sum <- Reduce(`+`, harv_stack)
  writeRaster(harv_sum, file.path(destination_dir, paste0("harv", dbg_suffix, ".tif")), overwrite = TRUE)
  
  # ---- Clean up intermediate files (optional) ----
  files_to_delete <- c(
    paste0("Growth11", dbg_suffix, ".tif"),
    paste0("Growth_less_harv20", dbg_suffix, ".tif"),
    harv_files
  )
  file_delete(file.path(destination_dir, files_to_delete))
  cat("✅ Processed:", dbg_suffix, "\n")
}


# JJ chunk part 2 ----

if (node_name == "WINLANASE") {
  
  mosaic_dir <- paste0(Gdrivedir,processingversion)
  
  # Set main directory and subfolders
  # mosaic_dir <- "G:/Mi unidad/webpages/2024_MoFuSSGlobal_Datasets/webmofussDS_v2/globalsouth_mofuss_bindingfolder_global_se/"
  subfolders <- c("ASUSLAP", "EDITORIALCIGA", "NRBV1", "WINCIGA", "WINLANASE")
  results_dir <- file.path(mosaic_dir, "RESULTS")
  unlink(results_dir, force = TRUE, recursive = TRUE)
  dir_create(results_dir)
  
  # Create the list of 60 filenames
  file_names <- c(
    sprintf("nrb_m_d%02d.tif", 1:MC),
    sprintf("harv_m_d%02d.tif", 1:MC)
  )
  
  # Mosaic and save
  for (f in file_names) {
    rasters_to_merge <- list()
    
    for (subf in subfolders) {
      file_path <- file.path(mosaic_dir, subf, f)
      if (file_exists(file_path)) {
        r <- rast(file_path)
        
        if (length(rasters_to_merge) == 0) {
          ref_crs <- crs(r)
          rasters_to_merge <- list(r)
        } else {
          r_proj <- project(r, ref_crs)
          rasters_to_merge <- append(rasters_to_merge, list(r_proj))
        }
      }
    }
    
    if (length(rasters_to_merge) > 1) {
      merged_r <- do.call(merge, rasters_to_merge)
      writeRaster(merged_r, file.path(results_dir, f), overwrite = TRUE)
      cat("✅ Mosaicked and saved:", f, "\n")
    } else if (length(rasters_to_merge) == 1) {
      writeRaster(rasters_to_merge[[1]], file.path(results_dir, f), overwrite = TRUE)
      cat("⚠️ Only one source file found; saved without merging:", f, "\n")
    } else {
      cat("❌ File not found in any subfolder:", f, "\n")
    }
  }
  
  # Directory where mosaicked results were saved
  results_dir <- file.path(mosaic_dir, "RESULTS")
  
  # Create raster stacks for nrb and harv
  nrb_files <- file.path(results_dir, sprintf("nrb_m_d%02d.tif", 1:MC))
  harv_files <- file.path(results_dir, sprintf("harv_m_d%02d.tif", 1:MC))
  
  # Load into SpatRaster collections
  nrb_stack <- rast(nrb_files)
  harv_stack <- rast(harv_files)
  
  # 1. Get the global sum per layer (30 MC runs)
  nrb_sums_PRE <- global(nrb_stack, sum, na.rm = TRUE)[[1]]
  harv_sums <- global(harv_stack, sum, na.rm = TRUE)[[1]]
  
  (nrb_sums <- nrb_sums_PRE * 1) # 1.368) #WARNING
  
  # 2. Compute mean and standard error
  mean_nrb <- mean(nrb_sums)
  mean_nrb
  se_nrb_PRE   <- sd(nrb_sums) / sqrt(length(nrb_sums))
  
  (se_nrb <- se_nrb_PRE * 1) # 0.92 #WARNING
  
  mean_harv <- mean(harv_sums)
  mean_harv
  se_harv   <- sd(harv_sums) / sqrt(length(harv_sums))
  
  # 3. Compute fNRB for each run, then mean and SE
  fnrb_vals <- nrb_sums / harv_sums
  mean_fnrb <- mean(fnrb_vals)
  se_fnrb   <- sd(fnrb_vals) / sqrt(length(fnrb_vals))
  
  # Alternatively: Propagate SE using error propagation formula for division
  # f = A / B → SE_f = f * sqrt( (SE_A/A)^2 + (SE_B/B)^2 )
  propagated_fnrb_se <- mean_fnrb * sqrt((se_nrb / mean_nrb)^2 + (se_harv / mean_harv)^2)
  
  # 4. Scale and format final values
  # Prepare clean mean and SE values
  mean_vals <- c(
    round(mean_nrb / 1000),               # nrb
    round(mean_harv / 1000),              # harv
    round(mean_fnrb * 100, 2)             # fNRB in %
  )
  
  se_vals <- c(
    round(se_nrb / 1000),                 # nrb
    round(se_harv / 1000),                # harv
    round(propagated_fnrb_se * 100, 2)    # fNRB in %
  )
  
  # Convert nrb and harv to character with no decimals
  mean_vals[1:2] <- format(as.integer(mean_vals[1:2]), nsmall = 0)
  se_vals[1:2] <- format(as.integer(se_vals[1:2]), nsmall = 0)
  
  # Create final data.frame
  summary_df <- data.frame(
    variable = c("nrb", "harv", "fNRB"),
    mean = mean_vals,
    se = se_vals,
    stringsAsFactors = FALSE
  )
  
  print(summary_df, row.names = FALSE)
  
}


