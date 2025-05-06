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
  "Growth_less_harv11.tif",
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
  agb11 <- rast(file.path(destination_dir, paste0("Growth_less_harv11", dbg_suffix, ".tif")))
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
    paste0("Growth_less_harv11", dbg_suffix, ".tif"),
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
  
  # Compute cell-wise mean and standard deviation
  nrb_mean <- global(nrb_stack, mean, na.rm = TRUE)
  nrb_sd   <- global(nrb_stack, sd, na.rm = TRUE)
  harv_mean <- global(harv_stack, mean, na.rm = TRUE)
  harv_sd   <- global(harv_stack, sd, na.rm = TRUE)
  
  # Calculate standard error: SE = SD / sqrt(n)
  n <- MC
  nrb_se <- nrb_sd / sqrt(n)
  harv_se <- harv_sd / sqrt(n)
  
  # Report
  summary_df <- data.frame(
    variable = c("nrb", "harv"),
    mean = c(nrb_mean, harv_mean),
    se = c(nrb_se, harv_se)
  )
  
  print(summary_df)
  
  
}






# #############################################
# 
# 
# 
# 
# # Only run if its Win Lanase----
# if (node == "Win Lanase") {
# Gdrivedir <- "G:/My Drive/webpages/2024_MoFuSSGlobal_Datasets/webmofussDS_v2/" # Update based on every node
# 
# # # Borrar esto!!! Regresar a Win Lanase
# # if (node == "Asus ZenBook") { 
# # Gdrivedir <- "G:/Mi unidad/webpages/2024_MoFuSSGlobal_Datasets/webmofussDS_v2/"
# 
#   
#   # BIND TABLES AND VECTORS ----
#   
#   if (cleanlocal == 1) {
#     localdirv <- paste0(Gdrivedir,version,"/",endyr,"_",scenario,"_",montecarlo,"mc/")
#     localdirv
#     unlink(localdirv, recursive=TRUE)
#     
#     outdir.tables <- paste0(Gdrivedir,version,"/",endyr,"_",scenario,"_",montecarlo,"mc/tables/")
#     outdir.vector <- paste0(Gdrivedir,version,"/",endyr,"_",scenario,"_",montecarlo,"mc/vector/")
#     outdir.raster <- paste0(Gdrivedir,version,"/",endyr,"_",scenario,"_",montecarlo,"mc/raster/")
#     
#     Sys.sleep(10)
#     
#     dir.create(outdir.tables, recursive=TRUE)
#     dir.create(outdir.vector, recursive=TRUE)
#     
#     Sys.sleep(10)
#   }
#   
#   outdir.tables <- paste0(Gdrivedir,version,"/",endyr,"_",scenario,"_",montecarlo,"mc/tables/")
#   outdir.vector <- paste0(Gdrivedir,version,"/",endyr,"_",scenario,"_",montecarlo,"mc/vector/")
#   outdir.raster <- paste0(Gdrivedir,version,"/",endyr,"_",scenario,"_",montecarlo,"mc/raster/")
#   
#   # List all directories within the specified path
#   result_dirs_unfiltered <- list.dirs(path = paste0(Gdrivedir,processingversion), full.names = TRUE, recursive = TRUE)
#   # Filter directories to include only those that end with 'webmofuss_results'
#   result_dirs <- result_dirs_unfiltered[grepl("webmofuss_results$", result_dirs_unfiltered)] # use taildir?
#   
#   # Read water bodies
#   if (exists("wbs")) {
#     print("hydrolakes already exists.")
#   } else {
#     choose_directory4 = function(caption = "Choose the directory where admin_regions files are") {
#       if(.Platform$OS.type == "unix")  {
#         setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
#       } else {
#         setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
#       }
#     }
#     choose_directory4()
#     admindir <- getwd()
#     
#     # wb <- st_read("HydroLAKES_polys_v10_clip.shp")
#     wb <- st_read("hydrolakes_v10.gpkg")
#     sf_use_s2(FALSE)
#     wbs <- wb %>%
#       ms_simplify %>%
#       dplyr::mutate(iddissolve = 1) %>%
#       group_by(iddissolve) %>%
#       summarize()
#   }
#   
#   ## Bind tables ----
#   for (i in admtables){
#     # i <- "adm2"
#     
#     if (bins == 1){
#       filecsvfr <- paste0("/summary_",i,"_fr.csv")
#       resfr <- do.call(rbind, lapply(paste0(result_dirs, filecsvfr), read.csv)) # %>%
#         # filter(!is.na(MC_1MC)) %>%
#         # mutate(fNRB_2020_2030_se = ifelse(fNRB_2020_2030_se >= 100, 100, fNRB_2020_2030_se))
#       write.csv(resfr, paste0(outdir.tables,"/summary_",i,"_fr.csv"))
#       
#       # ###
#       # # Use this chunk to identify anyone table(s) with different number of columns
#       # file_paths <- paste0(result_dirs, filecsvfr)
#       # # Initialize a vector to store the number of columns for each table
#       # column_counts <- numeric(length(file_paths))
#       # names(column_counts) <- file_paths
#       # # Loop through each file path, read the table, and check the number of columns
#       # for (i in seq_along(file_paths)) {
#       #   table <- read.csv(file_paths[i])  # Use read.csv, read.table, read.xlsx, etc., depending on your file type
#       #   column_counts[i] <- ncol(table)
#       #   cat("File:", file_paths[i], "has", ncol(table), "columns\n")
#       # }
#       # # Print all column counts to easily compare
#       # print(column_counts)
#       # ###
#       
#     }
#   }
#   
#   ## Bind vectors ----
#   # Read and bind vectors while masking out water bodies after binding geopackages
#   for (j in admvector){
#     # j = "adm0"
#     filegpkg <- paste0("/mofuss_",j,"_fr.gpkg")
#     bindvector <- do.call(rbind, lapply(paste0(result_dirs, filegpkg), sf::read_sf)) %>% # st_read
#       st_transform(4326) %>%
#       st_difference(., wbs) %>%
#       dplyr::select(-iddissolve) %>%
#       replace(is.na(.), 0) %>%
#       dplyr::mutate(
#         NRB_2020_2050_mean = round((NRB_2020_2050_mean/1000),0),
#         NRB_2020_2050_se = round((NRB_2020_2050_se/1000),0),
#         Harv_2020_2050_mean = round((Harv_2020_2050_mean/1000),0),
#         Harv_2020_2050_se = round((Harv_2020_2050_se/1000),0),
#         # NRB_2020_2050_1MC = round((NRB_2020_2050_1MC/1000),0),
#         # Harv_2020_2050_1MC = round((Harv_2020_2050_1MC/1000),0),
#         #fNRB_2020_2050_se = ifelse(fNRB_2020_2050_se >= 100, 100, fNRB_2020_2050_se),
#         NRB_2020_2030_mean = round((NRB_2020_2030_mean/1000),0),
#         NRB_2020_2030_se = round((NRB_2020_2030_se/1000),0),
#         Harv_2020_2030_mean = round((Harv_2020_2030_mean/1000),0),
#         Harv_2020_2030_se = round((Harv_2020_2030_se/1000),0),
#         # NRB_2020_2030_1MC = round((NRB_2020_2030_1MC/1000),0),
#         # Harv_2020_2030_1MC = round((Harv_2020_2030_1MC/1000),0),
#         #fNRB_2020_2030_se = ifelse(fNRB_2020_2030_se >= 100, 100, fNRB_2020_2030_se),
#         NRB_2030_2040_mean = round((NRB_2030_2040_mean/1000),0),
#         NRB_2030_2040_se = round((NRB_2030_2040_se/1000),0),
#         Harv_2030_2040_mean = round((Harv_2030_2040_mean/1000),0),
#         Harv_2030_2040_se = round((Harv_2030_2040_se/1000),0),
#         # NRB_2030_2040_1MC = round((NRB_2030_2040_1MC/1000),0),
#         # Harv_2030_2040_1MC = round((Harv_2030_2040_1MC/1000),0),
#         #fNRB_2030_2040_se = ifelse(fNRB_2030_2040_se >= 100, 100, fNRB_2030_2040_se),
#         NRB_2040_2050_mean = round((NRB_2040_2050_mean/1000),0),
#         NRB_2040_2050_se = round((NRB_2040_2050_se/1000),0),
#         Harv_2040_2050_mean = round((Harv_2040_2050_mean/1000),0),
#         Harv_2040_2050_se = round((Harv_2040_2050_se/1000),0),
#         # NRB_2040_2050_1MC = round((NRB_2040_2050_1MC/1000),0),
#         # Harv_2040_2050_1MC = round((Harv_2040_2050_1MC/1000),0),
#         #fNRB_2040_2050_se = ifelse(fNRB_2040_2050_se >= 100, 100, fNRB_2040_2050_se)
#       )
#     
#     # Assuming wbs is your multipolygon sf object
#     
#     # Check for validity
#     validity_check <- st_is_valid(bindvector)
#     
#     # Identify invalid geometries
#     invalid_geometries <- bindvector[!validity_check, ]
#     
#     # Print summary of invalid geometries
#     print(invalid_geometries)
#     
#     # Fix invalid geometries
#     bindvector_fixed <- st_make_valid(bindvector)
#     
#     # Verify that all geometries are now valid
#     all_valid <- st_is_valid(bindvector_fixed)
#     if (all(all_valid)) {
#       message("All geometries are now valid.")
#     } else {
#       message("There are still some invalid geometries.")
#     }
#     
#     # # Optionally, if you want to visualize the difference between original and fixed geometries
#     # plot(st_geometry(bindvector), col = 'red', border = 'red')
#     # plot(st_geometry(bindvector_fixed), col = 'green', border = 'green', add = TRUE)
#     
#     # Not saving the non simplified vector result to save space and time
#     st_write(bindvector_fixed, paste0(outdir.vector,"/mofuss_",j,".gpkg"), delete_layer = TRUE)
#     
#     if (simplifypolys == 1) {
#       
#       ### Split the ADM2 vector to speed the simplifying process ----
#       if (j != "adm2") {
#         
#         simplified_bindvector <- bindvector_fixed %>%
#           rmapshaper::ms_simplify(sys = TRUE, sys_mem = 24)
#         st_write(simplified_bindvector, paste0(outdir.vector,"/mofuss_",j,"_simp.gpkg"), delete_layer = TRUE)
#         
#       } else {
#         
#         # Ensure each polygon has a unique ID
#         bindvector_fixed <- bindvector_fixed %>%
#           mutate(idpoly = row_number())
#         
#         # Splitting the data into chunks
#         chunks <- split(bindvector_fixed, cut(seq_len(nrow(bindvector_fixed)), 
#                                               breaks = seq(1, nrow(bindvector_fixed), by = chunk_size), 
#                                               include.lowest = TRUE))
#         
#         # Initialize a list to hold simplified chunks
#         simplified_chunks <- list()
#         
#         # Simplifying each chunk
#         if (useextrasimppara == 1){
#           
#           for (ch in seq_along(chunks)) {
#             simplified_chunks[[ch]] <- rmapshaper::ms_simplify(
#               chunks[[ch]],
#               keep = keep_value,
#               keep_shapes = keep_shapes,
#               snap = snap_tolerance,
#               sys = TRUE,
#               sys_mem = 24
#             )
#             cat(sprintf("Simplified chunk %d/%d\n", ch, length(chunks)))  # Optional: print progress
#           }
#           
#         } else {
#           
#           for (ch in seq_along(chunks)) {
#             simplified_chunks[[ch]] <- rmapshaper::ms_simplify(
#               chunks[[ch]]
#             )
#             cat(sprintf("Simplified chunk %d/%d\n", ch, length(chunks)))  # Optional: print progress
#           }
#           
#         }
#         
#         # Combine the simplified chunks back into one sf object
#         # simplified_bindvector <- do.call(rbind, simplified_chunks)
#         simplified_bindvector <- dplyr::bind_rows(simplified_chunks)
#         
#         # Check for missing IDs
#         original_ids <- bindvector_fixed$idpoly
#         simplified_ids <- simplified_bindvector$idpoly
#         
#         missing_ids <- setdiff(original_ids, simplified_ids)
#         if (length(missing_ids) > 0) {
#           cat("Missing polygon IDs:", missing_ids, "\n")
#         } else {
#           cat("No polygons are missing.\n")
#         }
#         
#         # Optionally, re-assign original CRS if it was lost
#         st_crs(simplified_bindvector) <- st_crs(bindvector_fixed)
#         
#         # simplified_bindvector now contains all the simplified features
#         st_write(simplified_bindvector, paste0(outdir.vector,"/mofuss_",j,"_simp.gpkg"), delete_layer = TRUE)
#         
#       }
#       
#     }
#     
#     #### Start of shapefile chunk: 2BErased ----
#     if (shapefiles == 1){
#       
#       if (longnames != 1) {
#         
#         if (simplifypolys == 1) {
#           bindvectorSshp2 <- simplified_bindvector
#         } else {
#           bindvectorSshp2 <- bindvector_fixed
#         }
#         
#         bindvectorSshp <- bindvectorSshp2 %>%
#           dplyr::select(GID_0,NAME_0,NRB_2020_2030_mean,NRB_2020_2030_se,
#                         Harv_2020_2030_mean,Harv_2020_2030_se,
#                         fNRB_2020_2030_mean,fNRB_2020_2030_se,
#           ) %>%
#           dplyr::rename(NRB_m = NRB_2020_2030_mean,
#                         NRB_se = NRB_2020_2030_se,
#                         Harv_m = Harv_2020_2030_mean,
#                         Harv_se = Harv_2020_2030_se,
#                         fNRB_m = fNRB_2020_2030_mean,
#                         fNRB_se = fNRB_2020_2030_se)
#         
#         if (simplifypolys == 1) {
#           st_write(bindvectorSshp, paste0(outdir.vector,"/mofuss_",j,"_simp.shp"), delete_layer = TRUE)
#         } else {
#           st_write(bindvectorSshp, paste0(outdir.vector,"/mofuss_",j,".shp"), delete_layer = TRUE)
#         }
#         
#       } else if (longnames == 1) {
#         
#         if (simplifypolys == 1) {
#           names(simplified_bindvector)
#           bindvectorSshp <- simplified_bindvector
#         } else {
#           bindvectorSshp <- bindvector_fixed
#         }
#         
#         # Function to ensure unique names within 10 characters
#         abbreviate_unique <- function(names) {
#           # Abbreviate names to a maximum of 7 characters to leave room for suffixes
#           abbr_names <- abbreviate(names, minlength = 5, use.classes = FALSE)
#           
#           # Ensure uniqueness by appending numbers if necessary
#           uniq_abbr_names <- character(length(abbr_names))
#           for (l in seq_along(abbr_names)) {
#             base_name <- substr(abbr_names[l], 1, 7)
#             suffix <- 1
#             new_name <- base_name
#             while(new_name %in% uniq_abbr_names[1:l-1]) {
#               new_name <- paste0(base_name, suffix)
#               suffix <- suffix + 1
#             }
#             uniq_abbr_names[l] <- substr(new_name, 1, 10) # Ensure the final name is not longer than 10 chars
#           }
#           uniq_abbr_names
#         }
#         
#         # Apply the function to rename the columns
#         names(bindvectorSshp) <- abbreviate_unique(names(bindvectorSshp))
#         
#         # Check the new names
#         print(names(bindvectorSshp))
#         
#         # Now save the object as a shapefile
#         if (simplifypolys == 1) {
#           st_write(bindvectorSshp, paste0(outdir.vector,"/mofuss_",j,"abrv_simp.shp"), delete_layer = TRUE)
#         } else {
#           st_write(bindvectorSshp, paste0(outdir.vector,"/mofuss_",j,"abrv.shp"), delete_layer = TRUE)
#         }
#         
#       }
#     }
#     
#     #### End of shapefile chunk ----
#     
#   }
#   
#   ### Pack as one geopackage file ----
#   # j = "adm2"
#   process_files <- function(j, outdir.vector, simplified = FALSE) {  
#     suffix <- ifelse(simplified, "_simp", "")
#     file_levels <- switch(j,
#                           "adm2" = c("adm0", "adm1", "adm2"),
#                           "adm1" = c("adm0", "adm1"),
#                           "adm0" = c("adm0"),
#                           stop("error"))
#     
#     main_gpkg <- paste0(outdir.vector, "mofuss", suffix, ".gpkg")
#     unlink(main_gpkg)
#     
#     for (level in file_levels) {
#       file_path <- paste0(outdir.vector, "mofuss_", level, suffix, ".gpkg")
#       mofuss <- st_read(file_path)
#       st_write(mofuss, main_gpkg, paste0("mofuss_", level, suffix), append = file.exists(main_gpkg))
#       unlink(file_path)
#       st_layers(main_gpkg)
#     }
#   }
#   
#   if (simplifypolys == 1) {
#     
#     if (j %in% c("adm0", "adm1", "adm2")) {
#       # process_files(j, outdir.vector)
#       process_files(j, outdir.vector, simplified = TRUE)
#     } else {
#       print("error")
#     }
#     
#     if (j %in% c("adm0", "adm1", "adm2")) {
#       # process_files(j, outdir.vector)
#       process_files(j, outdir.vector, simplified = FALSE)
#     } else {
#       print("error")
#     }
#     
#   } else {
#     
#     if (j %in% c("adm0", "adm1", "adm2")) {
#       # process_files(j, outdir.vector)
#       process_files(j, outdir.vector, simplified = FALSE)
#     } else {
#       print("error")
#     }
#     
#   }
#   
#   ### Check results and column orders, if needed ----
#   # st_layers(paste0(outdir.vector,"mofuss_simp.gpkg"))
#   # adm0gpkg <- st_read(paste0(outdir.vector,"mofuss.gpkg"), layer = "mofuss_adm0")
#   # adm1gpkg <- st_read(paste0(outdir.vector,"mofuss.gpkg"), layer = "mofuss_adm1")
#   # adm0simpgpkg <- st_read(paste0(outdir.vector,"mofuss_simp.gpkg"), layer = "mofuss_adm0_simp")
#   # adm1simpgpkg <- st_read(paste0(outdir.vector,"mofuss_simp.gpkg"), layer = "mofuss_adm1_simp")
#   # head(adm0gpkg)
#   # head(adm1gpkg)
#   # head(adm0simpgpkg)
#   # head(adm1simpgpkg)
#   
#   # BIND RASTERS ----
#   if (rasters == 1) {
#     unlink(outdir.raster, recursive=TRUE)
#     dir.create(outdir.raster, recursive=TRUE)
#     Sys.sleep(3)
#     
#     ## Read and bind rasters (int) ----
#     # rasterint <- c("/2_NRB01.tif", "/2_CON_TOT01.tif", "/2_CON_NRB01.tif", "/2_IniSt01.tif", 
#     #                "/2_AGBt101.tif", "/aNRBmean.tif", "/aNRBsd.tif")
#     
#     # Add all possibilities
#     rasterint <- c("/agb_2010_mean.tif", "/agb_2010_sd.tif", "/agb_2010_se.tif",
#                    "/agb_2020_mean.tif", "/agb_2020_sd.tif", "/agb_2020_se.tif",
#                    "/agb_2030_mean.tif", "/agb_2030_sd.tif", "/agb_2030_se.tif",
#                    "/agb_2035_mean.tif", "/agb_2035_sd.tif", "/agb_2035_se.tif",
#                    "/agb_2040_mean.tif", "/agb_2040_sd.tif", "/agb_2040_se.tif",
#                    "/agb_2050_mean.tif", "/agb_2050_sd.tif", "/agb_2050_se.tif",
#                    "/harv_20_50_mean.tif", "/harv_20_50_sd.tif", "/harv_20_50_se.tif", "/nrb_20_50_mean.tif", "/nrb_20_50_sd.tif", "/nrb_20_50_se.tif",
#                    "/harv_20_30_mean.tif", "/harv_20_30_sd.tif", "/harv_20_30_se.tif", "/nrb_20_30_mean.tif", "/nrb_20_30_sd.tif", "/nrb_20_30_se.tif",
#                    "/harv_20_35_mean.tif", "/harv_20_35_sd.tif", "/harv_20_35_se.tif", "/nrb_20_35_mean.tif", "/nrb_20_35_sd.tif", "/nrb_20_35_se.tif",
#                    "/harv_30_40_mean.tif", "/harv_30_40_sd.tif", "/harv_30_40_se.tif", "/nrb_30_40_mean.tif", "/nrb_30_40_sd.tif", "/nrb_30_40_se.tif",
#                    "/harv_40_50_mean.tif", "/harv_40_50_sd.tif", "/harv_40_50_se.tif", "/nrb_40_50_mean.tif", "/nrb_40_50_sd.tif", "/nrb_40_50_se.tif")
#     
#     # Function to reproject wbs to match raster CRS
#     reproject_wbs <- function(raster, wbs) {
#       raster_crs <- crs(raster)
#       if (!st_crs(wbs) == raster_crs) {
#         wbs <- st_transform(wbs, crs = raster_crs)
#       }
#       return(wbs)
#     }
#     
#     # Loop through each raster file
#     for (n in rasterint) {
#       # n <- "/agb_2020_mean.tif"
#       # Construct the full paths
#       full_paths <- paste0(result_dirs, n)
#       
#       # Check if the files exist
#       existing_files <- full_paths[file.exists(full_paths)]
#       
#       if (length(existing_files) > 0) {
#         # Print progress message
#         message("Processing raster: ", n)
#         
#         allrasters <- lapply(existing_files, terra::rast)
#         mosaic_raster <- do.call(mosaic, c(allrasters, list(fun = "max")))
#         mosaic_raster[mosaic_raster < 0] = 0
#         
#         # Reproject wbs to match the CRS of the mosaic_raster
#         wbs_projected <- reproject_wbs(mosaic_raster, wbs)
#         
#         # Ensure both raster and vector data have the same CRS
#         if (!st_crs(mosaic_raster) == st_crs(wbs_projected)) {
#           stop("CRS of raster and vector data do not match!")
#         }
#         
#         # Mask the raster with the multipolygon sf object (wbs_projected)
#         masked_raster <- terra::mask(mosaic_raster, wbs_projected, inverse = TRUE)
#         
#         # # Reassign the CRS to the masked raster if it was lost
#         # terra::crs(masked_raster) <- terra::crs(mosaic_raster)
#         
#         # Write the masked raster to disk
#         terra::writeRaster(masked_raster, paste0(outdir.raster, n), datatype = "INT4S", overwrite = TRUE)
#       } else {
#         message("No existing files for: ", n)
#       }
#     }
#   }
# }
# 
# 
