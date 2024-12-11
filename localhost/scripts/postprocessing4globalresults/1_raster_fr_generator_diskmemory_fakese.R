# MoFuSS
# Version 3
# Date: May 2024

# 2dolist
## Faltaría 2010-2050
## ADJUST TO CHANGE SD FOR SE ----

# Internal parameters
fixdir <- 1

# Load packages ----
library(terra)
library(dplyr)
library(readxl)
library(tictoc)
library(fs)
library(tcltk)

tic()

if (fixdir == 1){
  
  # Define a particular directory when needed:
  adm0_dirs <- c("D:/SSA_adm0_madagascar_apr2024")
  # adm0_dirs <- c("E:/ASIA_adm0_central_apr2024")
  #adm0_dirs <- c("F:/ASIA_adm0_china_apr2024")
  # adm0_dirs <- c("F:/ASIA_adm0_mongolia_apr2024",
  #                "F:/ASIA_adm0_nepal_apr2024",
  #                "F:/SSA_adm0_angola_apr2024",
  #                "F:/SSA_adm0_benin_apr2024",
  #                "F:/SSA_adm0_burkinafaso_apr2024",
  #                "F:/SSA_adm0_central_apr2024",
  #                "F:/SSA_adm0_chad_apr2024",
  #                "F:/SSA_adm0_cotedivoire_apr2024",
  #                "F:/SSA_adm0_eastern_apr2024",
  #                "F:/SSA_adm0_ghana_apr2024",
  #                "F:/SSA_adm0_mali_apr2024",
  #                "F:/SSA_adm0_mauritania_apr2024",
  #                "F:/SSA_adm0_niger_apr2024",
  #                "F:/SSA_adm0_northcentral_apr2024",
  #                "F:/SSA_adm0_senegambia_apr2024",
  #                "F:/SSA_adm0_southern_apr2024",
  #                "F:/SSA_adm0_togo_apr2024",
  #                "F:/SSA_adm0_uganda_apr2024",
  #                "F:/SSA_adm0_westcentral_apr2024",
  #                "F:/SSA_adm0_western_apr2024",
  #                "F:/SSA_adm0_westsouthern_apr2024")
  
} else {
  
  # Define the directory to search
  setwd(tk_choose.dir(default = getwd(), caption = "Define the directory to search"))
  search_path <- getwd()
  
  # List all directories in the specified path
  all_dirs <- dir_ls(search_path, type = "directory")
  
  # Filter directories containing 'adm0'
  adm0_dirs <- all_dirs[grepl("adm0", all_dirs)]
  
}

# Loop through each adm0 directory----
for (dir in adm0_dirs) {
  # dir = "F:/SSA_adm0_togo_apr2024"
  # dir = "D:/SSA_adm0_zambia_apr2024"
  # dir = ("E:/ASIA_adm0_central_apr2024")
  # dir = "F:/ASIA_adm0_china_apr2024"
  print(paste("Processing directory:", dir))
  
  # # Initialize lists to store the rasters ----
  # nrb_bin2010_2020_list <- list()
  # harvest_st_bin2010_2020_list <- list()
  # nrb_bin2020_2030_list <- list()
  # harvest_st_bin2020_2030_list <- list()
  # nrb_bin2020_2035_list <- list()
  # harvest_st_bin2020_2035_list <- list()
  # nrb_bin2020_2050_list <- list()
  # harvest_st_bin2020_2050_list <- list()
  # nrb_bin2030_2040_list <- list()
  # harvest_st_bin2030_2040_list <- list()
  # nrb_bin2040_2050_list <- list()
  # harvest_st_bin2040_2050_list <- list()
  # agb_2010_list <- list()
  # agb_2020_list <- list()
  # # agb_2025_list <- list()
  # agb_2030_list <- list()
  # agb_2035_list <- list()
  # agb_2040_list <- list()
  # agb_2050_list <- list()
  
  setwd(dir)
  getwd()
  
  # Read parameters table ----
  read.csv("LULCC/TempTables/Country.csv") %>%
    dplyr::filter(Key. == "1") %>%
    pull(Country) -> country_name
  
  # Specify the directory where the file is located
  parameters_directory <- paste0(getwd(),"/LULCC/DownloadedDatasets/SourceData",country_name)
  
  # Use list.files() to find the file that matches the pattern
  parameters_name <- list.files(path = parameters_directory, pattern = "^parameters.*\\.xlsx$", full.names = TRUE)
  
  # Read the Excel file
  country_parameters <- read_excel(parameters_name)
  print(tibble::as_tibble(country_parameters), n=100)
  
  country_parameters %>%
    dplyr::filter(Var == "end_year") %>%
    pull(ParCHR) -> end_year
  
  # Find all "debugging_n" subfolders
  debugging_dirs <- list.dirs(dir, full.names = TRUE, recursive = FALSE)
  debugging_dirs <- debugging_dirs[grepl("debugging_\\d+", basename(debugging_dirs))]
  
  # Loop through each "debugging_n" folder
  
  for (i in 1:length(debugging_dirs)) {
    # i=1
    debug_dir <- debugging_dirs[i]
    # debug_dir = "E:/ASIA_adm0_central_apr2024/debugging_2"
    print(paste("Processing directory", i, ":", debug_dir))
    
    # NRB---- 
    listGlH <- list.files(debug_dir, pattern = "^Growth_less_harv.+[.]tif$",ignore.case=F)
    stackGlH <- rast(paste0(debug_dir,"/",listGlH))
    nlay <- nlyr(stackGlH)
    
    listGx <- list.files(debug_dir, pattern = "^Growth.+[.]tif$",ignore.case=F)
    listG <- listGx[ !grepl("_less_harv", listGx) ]
    stackG <- rast(paste0(debug_dir,"/",listG))
    nlyr(stackG) #for cross checking pattern
    
    nlay_yr <- nlay+2009
    STdyn <- nlay-1
    
    nrb_name_per <- paste("nrb_sum_bin2010", nlay_yr, sep = "_")
    calculated_nrb_per <- stackG[[1]] - stackGlH[[nlay]] # Bin is entire period, do not use in final tables 
    calculated_nrb_per[calculated_nrb_per <= 0] = NA 
    assign(nrb_name_per, calculated_nrb_per)
    
    nrb_name <- paste("nrb_sum_bin2020", nlay_yr, sep = "_")
    calculated_nrb <- stackG[[11]] - stackGlH[[nlay]] # Bin will start in 2020 and end in the final year
    calculated_nrb[calculated_nrb <= 0] = NA 
    assign(nrb_name, calculated_nrb)
    
    if (STdyn == 10){
      nrb_bin2010_2020 <- stackG[[1]] - stackGlH[[nlay]] # Bin will be 2010-2020
      nrb_bin2010_2020[nrb_bin2010_2020 <= 0] = NA
      
    }
    
    if (STdyn == 20){
      nrb_bin2010_2020 <- stackG[[1]] - stackGlH[[10]] # Bin will be 2010-2020
      nrb_bin2010_2020[nrb_bin2010_2020 <= 0] = NA 
      
      nrb_bin2020_2030 <- stackG[[11]] - stackGlH[[nlay]] # Bin will be 2020-2030
      nrb_bin2020_2030[nrb_bin2020_2030 <= 0] = NA 
      
    }
    
    if (STdyn == 25){
      nrb_bin2010_2020 <- stackG[[1]] - stackGlH[[10]] # Bin will be 2010-2020
      nrb_bin2010_2020[nrb_bin2010_2020 <= 0] = NA 
      
      nrb_bin2020_2035 <- stackG[[11]] - stackGlH[[nlay]] # Bin will be 2020-2035
      nrb_bin2020_2035[nrb_bin2020_2035 <= 0] = NA 
      
    }
    
    if (STdyn == 30){ # STdyn = 30 # 2040
      nrb_bin2010_2020 <- stackG[[1]] - stackGlH[[10]] # Bin will be 2010-2020
      nrb_bin2010_2020[nrb_bin2010_2020 <= 0] = NA 
      
      nrb_bin2020_2030 <- stackG[[11]] - stackGlH[[20]] # Bin will be 2020-2030
      nrb_bin2020_2030[nrb_bin2020_2030 <= 0] = NA 
      
      nrb_bin2030_2040 <- stackG[[21]] - stackGlH[[nlay]] # Bin will be 2030-2040
      nrb_bin2030_2040[nrb_bin2030_2040 <= 0] = NA 
      
    } 
    
    if (STdyn == 40){ # STdyn = 40 # 2050
      nrb_bin2010_2020 <- stackG[[1]] - stackGlH[[10]] # Bin will be 2010-2020
      nrb_bin2010_2020[nrb_bin2010_2020 <= 0] = NA 
      
      nrb_bin2020_2030 <- stackG[[11]] - stackGlH[[20]] # Bin will be 2020-2030
      nrb_bin2020_2030[nrb_bin2020_2030 <= 0] = NA
      
      nrb_bin2020_2035 <- stackG[[11]] - stackGlH[[25]] # Bin will be 2020-2035
      nrb_bin2020_2035[nrb_bin2020_2035 <= 0] = NA 
      
      nrb_bin2020_2050 <- stackG[[11]] - stackGlH[[nlay]] # Bin will be 2020-2050
      nrb_bin2020_2050[nrb_bin2020_2050 <= 0] = NA 
      
      nrb_bin2030_2040 <- stackG[[21]] - stackGlH[[30]] # Bin will be 2030-2040
      nrb_bin2030_2040[nrb_bin2030_2040 <= 0] = NA 
      
      nrb_bin2040_2050 <- stackG[[31]] - stackGlH[[nlay]] # Bin will be 2040-2050
      nrb_bin2040_2050[nrb_bin2040_2050 <= 0] = NA 
      
    }
    
    # Harvest----
    
    listharvx_per <- list.files(debug_dir, pattern = "^Harvest_tot.+[.]tif$",ignore.case=F)
    listharv_per <- listharvx_per[ !grepl("_tot_nrb", listharvx_per) ]
    stackhar_per <- rast(paste0(debug_dir,"/",listharv_per))
    nlyr(stackhar_per)
    
    harv_name_per <- paste("harv_sum_bin2010", nlay_yr, sep = "_")
    harvest_st_per <- app(stackhar_per, fun = sum)
    assign(harv_name_per, harvest_st_per)
    
    harv_name <- paste("harv_sum_bin2020", nlay_yr, sep = "_")
    listharv <- listharv_per[11:nlay]
    stackharv <- rast(paste0(debug_dir,"/",listharv))
    nlyr(stackharv)
    
    harvest_st <- app(stackharv, fun=sum)
    assign(harv_name, harvest_st)
    
    if (STdyn == 10){
      listharv_bin2010_2020 <- listharv_per[1:nlay]
      stackharv_bin2010_2020 <- rast(paste0(debug_dir,"/",listharv_bin2010_2020))
      nlyr(stackharv_bin2010_2020)
      harvest_st_bin2010_2020 <- app(stackharv_bin2010_2020, fun=sum)
      
    }
    
    if (STdyn == 20){
      listharv_bin2010_2020 <- listharv_per[1:10]
      stackharv_bin2010_2020 <- rast(paste0(debug_dir,"/",listharv_bin2010_2020))
      nlyr(stackharv_bin2010_2020)
      harvest_st_bin2010_2020 <- app(stackharv_bin2010_2020, fun=sum)
      
      listharv_bin2020_2030 <- listharv_per[11:nlay]
      stackharv_bin2020_2030 <- rast(paste0(debug_dir,"/",listharv_bin2020_2030))
      nlyr(stackharv_bin2020_2030)
      harvest_st_bin2020_2030 <- app(stackharv_bin2020_2030, fun=sum)
      
    }
    
    if (STdyn == 25){
      listharv_bin2010_2020 <- listharv_per[1:10]
      stackharv_bin2010_2020 <- rast(paste0(debug_dir,"/",listharv_bin2010_2020))
      nlyr(stackharv_bin2010_2020)
      harvest_st_bin2010_2020 <- app(stackharv_bin2010_2020, fun=sum)
      
      listharv_bin2020_2035 <- listharv_per[11:nlay]
      stackharv_bin2020_2035 <- rast(paste0(debug_dir,"/",listharv_bin2020_2035))
      nlyr(stackharv_bin2020_2035)
      harvest_st_bin2020_2035 <- app(stackharv_bin2020_2035, fun=sum)
      
    }
    
    if (STdyn == 30){
      listharv_bin2010_2020 <- listharv_per[1:10]
      stackharv_bin2010_2020 <- rast(paste0(debug_dir,"/",listharv_bin2010_2020))
      nlyr(stackharv_bin2010_2020)
      harvest_st_bin2010_2020 <- app(stackharv_bin2010_2020, fun=sum)
      
      listharv_bin2020_2030 <- listharv_per[11:20]
      stackharv_bin2020_2030 <- rast(paste0(debug_dir,"/",listharv_bin2020_2030))
      nlyr(stackharv_bin2020_2030)
      harvest_st_bin2020_2030 <- app(stackharv_bin2020_2030, fun=sum)
      
      listharv_bin2030_2040 <- listharv_per[21:nlay]
      stackharv_bin2030_2040 <- rast(paste0(debug_dir,"/",listharv_bin2030_2040))
      nlyr(stackharv_bin2030_2040)
      harvest_st_bin2030_2040 <- app(stackharv_bin2030_2040, fun=sum)
      
    }
    
    if (STdyn == 40){
      listharv_bin2010_2020 <- listharv_per[1:10]
      stackharv_bin2010_2020 <- rast(paste0(debug_dir,"/",listharv_bin2010_2020))
      nlyr(stackharv_bin2010_2020)
      harvest_st_bin2010_2020 <- app(stackharv_bin2010_2020, fun=sum)
      
      listharv_bin2020_2030 <- listharv_per[11:20]
      stackharv_bin2020_2030 <- rast(paste0(debug_dir,"/",listharv_bin2020_2030))
      nlyr(stackharv_bin2020_2030)
      harvest_st_bin2020_2030 <- app(stackharv_bin2020_2030, fun=sum)
      
      listharv_bin2020_2035 <- listharv_per[11:25]
      stackharv_bin2020_2035 <- rast(paste0(debug_dir,"/",listharv_bin2020_2035))
      nlyr(stackharv_bin2020_2035)
      harvest_st_bin2020_2035 <- app(stackharv_bin2020_2035, fun=sum)
      
      listharv_bin2020_2050 <- listharv_per[11:nlay]
      stackharv_bin2020_2050 <- rast(paste0(debug_dir,"/",listharv_bin2020_2050))
      nlyr(stackharv_bin2020_2050)
      harvest_st_bin2020_2050 <- app(stackharv_bin2020_2050, fun=sum)
      
      listharv_bin2030_2040 <- listharv_per[21:30]
      stackharv_bin2030_2040 <- rast(paste0(debug_dir,"/",listharv_bin2030_2040))
      nlyr(stackharv_bin2030_2040)
      harvest_st_bin2030_2040 <- app(stackharv_bin2030_2040, fun=sum)
      
      listharv_bin2040_2050 <- listharv_per[31:nlay]
      stackharv_bin2040_2050 <- rast(paste0(debug_dir,"/",listharv_bin2040_2050))
      nlyr(stackharv_bin2040_2050)
      harvest_st_bin2040_2050 <- app(stackharv_bin2040_2050, fun=sum)
      
    }
    
    # Store rasters in the lists ----
    
    # Create the directory only if it doesn't exist
if (!dir.exists("temp_raster_lists")) {
   dir.create("temp_raster_lists")
}
    
    if (STdyn == 10){
      ## STdyn == 10 ----
      ### nrb_bin2010_2020----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2010_2020_step_", i - 1, ".rds"))) {
        nrb_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/nrb_bin2010_2020_step_", i - 1, ".rds"))
      } else {
        nrb_bin2010_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2010_2020_", i, ".tif")
      writeRaster(nrb_bin2010_2020, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2010_2020_list[[length(nrb_bin2010_2020_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2010_2020_list, file = paste0("temp_raster_lists/nrb_bin2010_2020_step_", i, ".rds"))
      
      ### harvest_st_bin2010_2020 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i - 1, ".rds"))) {
        harvest_st_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2010_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2010_2020_", i, ".tif")
      writeRaster(harvest_st_bin2010_2020, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2010_2020_list[[length(harvest_st_bin2010_2020_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2010_2020_list, file = paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i, ".rds"))
      
      ### agb_2010----
      if (file.exists(paste0("temp_raster_lists/agb_2010_step_", i - 1, ".rds"))) {
        agb_2010_list <- readRDS(paste0("temp_raster_lists/agb_2010_step_", i - 1, ".rds"))
      } else {
        agb_2010_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2010_", i, ".tif")
      agb_2010 <- stackG[[1]]
      writeRaster(agb_2010, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2010_list[[length(agb_2010_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2010_list, file = paste0("temp_raster_lists/agb_2010_step_", i, ".rds"))
      
      ### agb_2020----
      if (file.exists(paste0("temp_raster_lists/agb_2020_step_", i - 1, ".rds"))) {
        agb_2020_list <- readRDS(paste0("temp_raster_lists/agb_2020_step_", i - 1, ".rds"))
      } else {
        agb_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2020_", i, ".tif")
      agb_2020 <- stackG[[10]]
      writeRaster(agb_2020, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2020_list[[length(agb_2020_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2020_list, file = paste0("temp_raster_lists/agb_2020_step_", i, ".rds"))
      
    }
    
    if (STdyn == 20){
      ## STdyn == 20 ----
      ### nrb_bin2010_2020----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2010_2020_step_", i - 1, ".rds"))) {
        nrb_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/nrb_bin2010_2020_step_", i - 1, ".rds"))
      } else {
        nrb_bin2010_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2010_2020_", i, ".tif")
      writeRaster(nrb_bin2010_2020, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2010_2020_list[[length(nrb_bin2010_2020_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2010_2020_list, file = paste0("temp_raster_lists/nrb_bin2010_2020_step_", i, ".rds"))
      
      ### harvest_st_bin2010_2020 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i - 1, ".rds"))) {
        harvest_st_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2010_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2010_2020_", i, ".tif")
      writeRaster(harvest_st_bin2010_2020, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2010_2020_list[[length(harvest_st_bin2010_2020_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2010_2020_list, file = paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i, ".rds"))
      
      ### nrb_bin2020_2030----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2020_2030_step_", i - 1, ".rds"))) {
        nrb_bin2020_2030_list <- readRDS(paste0("temp_raster_lists/nrb_bin2020_2030_step_", i - 1, ".rds"))
      } else {
        nrb_bin2020_2030_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2020_2030_", i, ".tif")
      writeRaster(nrb_bin2020_2030, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2020_2030_list[[length(nrb_bin2020_2030_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2020_2030_list, file = paste0("temp_raster_lists/nrb_bin2020_2030_step_", i, ".rds"))
      
      ### harvest_st_bin2020_2030 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2020_2030_step_", i - 1, ".rds"))) {
        harvest_st_bin2020_2030_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2020_2030_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2020_2030_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2020_2030_", i, ".tif")
      writeRaster(harvest_st_bin2020_2030, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2020_2030_list[[length(harvest_st_bin2020_2030_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2020_2030_list, file = paste0("temp_raster_lists/harvest_st_bin2020_2030_step_", i, ".rds"))
      
      ### agb_2010----
      if (file.exists(paste0("temp_raster_lists/agb_2010_step_", i - 1, ".rds"))) {
        agb_2010_list <- readRDS(paste0("temp_raster_lists/agb_2010_step_", i - 1, ".rds"))
      } else {
        agb_2010_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2010_", i, ".tif")
      agb_2010 <- stackG[[1]]
      writeRaster(agb_2010, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2010_list[[length(agb_2010_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2010_list, file = paste0("temp_raster_lists/agb_2010_step_", i, ".rds"))
      
      ### agb_2020----
      if (file.exists(paste0("temp_raster_lists/agb_2020_step_", i - 1, ".rds"))) {
        agb_2020_list <- readRDS(paste0("temp_raster_lists/agb_2020_step_", i - 1, ".rds"))
      } else {
        agb_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2020_", i, ".tif")
      agb_2020 <- stackG[[10]]
      writeRaster(agb_2020, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2020_list[[length(agb_2020_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2020_list, file = paste0("temp_raster_lists/agb_2020_step_", i, ".rds"))
      
      ### agb_2030----
      if (file.exists(paste0("temp_raster_lists/agb_2030_step_", i - 1, ".rds"))) {
        agb_2030_list <- readRDS(paste0("temp_raster_lists/agb_2030_step_", i - 1, ".rds"))
      } else {
        agb_2030_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2030_", i, ".tif")
      agb_2030 <- stackG[[20]]
      writeRaster(agb_2030, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2030_list[[length(agb_2030_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2030_list, file = paste0("temp_raster_lists/agb_2030_step_", i, ".rds"))
      
    }
    
    if (STdyn == 25){
      ## STdyn == 25 ----
      ### nrb_bin2010_2020----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2010_2020_step_", i - 1, ".rds"))) {
        nrb_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/nrb_bin2010_2020_step_", i - 1, ".rds"))
      } else {
        nrb_bin2010_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2010_2020_", i, ".tif")
      writeRaster(nrb_bin2010_2020, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2010_2020_list[[length(nrb_bin2010_2020_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2010_2020_list, file = paste0("temp_raster_lists/nrb_bin2010_2020_step_", i, ".rds"))
      
      ### harvest_st_bin2010_2020 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i - 1, ".rds"))) {
        harvest_st_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2010_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2010_2020_", i, ".tif")
      writeRaster(harvest_st_bin2010_2020, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2010_2020_list[[length(harvest_st_bin2010_2020_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2010_2020_list, file = paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i, ".rds"))
      
      ### nrb_bin2020_2035----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2020_2035_step_", i - 1, ".rds"))) {
        nrb_bin2020_2035_list <- readRDS(paste0("temp_raster_lists/nrb_bin2020_2035_step_", i - 1, ".rds"))
      } else {
        nrb_bin2020_2035_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2020_2035_", i, ".tif")
      writeRaster(nrb_bin2020_2035, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2020_2035_list[[length(nrb_bin2020_2035_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2020_2035_list, file = paste0("temp_raster_lists/nrb_bin2020_2035_step_", i, ".rds"))
      
      ### harvest_st_bin2020_2035 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2020_2035_step_", i - 1, ".rds"))) {
        harvest_st_bin2020_2035_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2020_2035_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2020_2035_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2020_2035_", i, ".tif")
      writeRaster(harvest_st_bin2020_2035, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2020_2035_list[[length(harvest_st_bin2020_2035_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2020_2035_list, file = paste0("temp_raster_lists/harvest_st_bin2020_2035_step_", i, ".rds"))
      
      ### agb_2010----
      if (file.exists(paste0("temp_raster_lists/agb_2010_step_", i - 1, ".rds"))) {
        agb_2010_list <- readRDS(paste0("temp_raster_lists/agb_2010_step_", i - 1, ".rds"))
      } else {
        agb_2010_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2010_", i, ".tif")
      agb_2010 <- stackG[[1]]
      writeRaster(agb_2010, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2010_list[[length(agb_2010_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2010_list, file = paste0("temp_raster_lists/agb_2010_step_", i, ".rds"))
      
      ### agb_2020----
      if (file.exists(paste0("temp_raster_lists/agb_2020_step_", i - 1, ".rds"))) {
        agb_2020_list <- readRDS(paste0("temp_raster_lists/agb_2020_step_", i - 1, ".rds"))
      } else {
        agb_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2020_", i, ".tif")
      agb_2020 <- stackG[[10]]
      writeRaster(agb_2020, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2020_list[[length(agb_2020_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2020_list, file = paste0("temp_raster_lists/agb_2020_step_", i, ".rds"))
      
      ### agb_2035----
      if (file.exists(paste0("temp_raster_lists/agb_2035_step_", i - 1, ".rds"))) {
        agb_2035_list <- readRDS(paste0("temp_raster_lists/agb_2035_step_", i - 1, ".rds"))
      } else {
        agb_2035_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2035_", i, ".tif")
      agb_2035 <- stackG[[25]]
      writeRaster(agb_2035, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2035_list[[length(agb_2035_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2035_list, file = paste0("temp_raster_lists/agb_2035_step_", i, ".rds"))
    }
    
    if (STdyn == 30){
      ## STdyn == 30 ----
      ### nrb_bin2010_2020----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2010_2020_step_", i - 1, ".rds"))) {
        nrb_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/nrb_bin2010_2020_step_", i - 1, ".rds"))
      } else {
        nrb_bin2010_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2010_2020_", i, ".tif")
      writeRaster(nrb_bin2010_2020, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2010_2020_list[[length(nrb_bin2010_2020_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2010_2020_list, file = paste0("temp_raster_lists/nrb_bin2010_2020_step_", i, ".rds"))
      
      ### harvest_st_bin2010_2020 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i - 1, ".rds"))) {
        harvest_st_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2010_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2010_2020_", i, ".tif")
      writeRaster(harvest_st_bin2010_2020, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2010_2020_list[[length(harvest_st_bin2010_2020_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2010_2020_list, file = paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i, ".rds"))
      
      ### nrb_bin2020_2030----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2020_2030_step_", i - 1, ".rds"))) {
        nrb_bin2020_2030_list <- readRDS(paste0("temp_raster_lists/nrb_bin2020_2030_step_", i - 1, ".rds"))
      } else {
        nrb_bin2020_2030_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2020_2030_", i, ".tif")
      writeRaster(nrb_bin2020_2030, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2020_2030_list[[length(nrb_bin2020_2030_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2020_2030_list, file = paste0("temp_raster_lists/nrb_bin2020_2030_step_", i, ".rds"))
      
      ### harvest_st_bin2020_2030 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2020_2030_step_", i - 1, ".rds"))) {
        harvest_st_bin2020_2030_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2020_2030_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2020_2030_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2020_2030_", i, ".tif")
      writeRaster(harvest_st_bin2020_2030, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2020_2030_list[[length(harvest_st_bin2020_2030_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2020_2030_list, file = paste0("temp_raster_lists/harvest_st_bin2020_2030_step_", i, ".rds"))
      
      ### nrb_bin2030_2040----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2030_2040_step_", i - 1, ".rds"))) {
        nrb_bin2030_2040_list <- readRDS(paste0("temp_raster_lists/nrb_bin2030_2040_step_", i - 1, ".rds"))
      } else {
        nrb_bin2030_2040_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2030_2040_", i, ".tif")
      writeRaster(nrb_bin2030_2040, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2030_2040_list[[length(nrb_bin2030_2040_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2030_2040_list, file = paste0("temp_raster_lists/nrb_bin2030_2040_step_", i, ".rds"))
      
      ### harvest_st_bin2030_2040 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2030_2040_step_", i - 1, ".rds"))) {
        harvest_st_bin2030_2040_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2030_2040_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2030_2040_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2030_2040_", i, ".tif")
      writeRaster(harvest_st_bin2030_2040, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2030_2040_list[[length(harvest_st_bin2030_2040_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2030_2040_list, file = paste0("temp_raster_lists/harvest_st_bin2030_2040_step_", i, ".rds"))
      
      ### agb_2010----
      if (file.exists(paste0("temp_raster_lists/agb_2010_step_", i - 1, ".rds"))) {
        agb_2010_list <- readRDS(paste0("temp_raster_lists/agb_2010_step_", i - 1, ".rds"))
      } else {
        agb_2010_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2010_", i, ".tif")
      agb_2010 <- stackG[[1]]
      writeRaster(agb_2010, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2010_list[[length(agb_2010_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2010_list, file = paste0("temp_raster_lists/agb_2010_step_", i, ".rds"))
      
      ### agb_2020----
      if (file.exists(paste0("temp_raster_lists/agb_2020_step_", i - 1, ".rds"))) {
        agb_2020_list <- readRDS(paste0("temp_raster_lists/agb_2020_step_", i - 1, ".rds"))
      } else {
        agb_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2020_", i, ".tif")
      agb_2020 <- stackG[[10]]
      writeRaster(agb_2020, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2020_list[[length(agb_2020_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2020_list, file = paste0("temp_raster_lists/agb_2020_step_", i, ".rds"))
      
      ### agb_2030----
      if (file.exists(paste0("temp_raster_lists/agb_2030_step_", i - 1, ".rds"))) {
        agb_2030_list <- readRDS(paste0("temp_raster_lists/agb_2030_step_", i - 1, ".rds"))
      } else {
        agb_2030_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2030_", i, ".tif")
      agb_2030 <- stackG[[20]]
      writeRaster(agb_2030, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2030_list[[length(agb_2030_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2030_list, file = paste0("temp_raster_lists/agb_2030_step_", i, ".rds"))
      
      ### agb_2040----
      if (file.exists(paste0("temp_raster_lists/agb_2040_step_", i - 1, ".rds"))) {
        agb_2040_list <- readRDS(paste0("temp_raster_lists/agb_2040_step_", i - 1, ".rds"))
      } else {
        agb_2040_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2040_", i, ".tif")
      agb_2040 <- stackG[[30]]
      writeRaster(agb_2040, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2040_list[[length(agb_2040_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2040_list, file = paste0("temp_raster_lists/agb_2040_step_", i, ".rds"))
    }
    
    if (STdyn == 40){
      ## STdyn == 40 ----
      ### nrb_bin2010_2020----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2010_2020_step_", i - 1, ".rds"))) {
        nrb_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/nrb_bin2010_2020_step_", i - 1, ".rds"))
      } else {
        nrb_bin2010_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2010_2020_", i, ".tif")
      writeRaster(nrb_bin2010_2020, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2010_2020_list[[length(nrb_bin2010_2020_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2010_2020_list, file = paste0("temp_raster_lists/nrb_bin2010_2020_step_", i, ".rds"))
      
      ### harvest_st_bin2010_2020 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i - 1, ".rds"))) {
        harvest_st_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2010_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2010_2020_", i, ".tif")
      writeRaster(harvest_st_bin2010_2020, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2010_2020_list[[length(harvest_st_bin2010_2020_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2010_2020_list, file = paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i, ".rds"))
      
      ### nrb_bin2020_2030----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2020_2030_step_", i - 1, ".rds"))) {
        nrb_bin2020_2030_list <- readRDS(paste0("temp_raster_lists/nrb_bin2020_2030_step_", i - 1, ".rds"))
      } else {
        nrb_bin2020_2030_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2020_2030_", i, ".tif")
      writeRaster(nrb_bin2020_2030, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2020_2030_list[[length(nrb_bin2020_2030_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2020_2030_list, file = paste0("temp_raster_lists/nrb_bin2020_2030_step_", i, ".rds"))
      
      ### harvest_st_bin2020_2030 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2020_2030_step_", i - 1, ".rds"))) {
        harvest_st_bin2020_2030_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2020_2030_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2020_2030_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2020_2030_", i, ".tif")
      writeRaster(harvest_st_bin2020_2030, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2020_2030_list[[length(harvest_st_bin2020_2030_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2020_2030_list, file = paste0("temp_raster_lists/harvest_st_bin2020_2030_step_", i, ".rds"))
      
      ### nrb_bin2020_2035----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2020_2035_step_", i - 1, ".rds"))) {
        nrb_bin2020_2035_list <- readRDS(paste0("temp_raster_lists/nrb_bin2020_2035_step_", i - 1, ".rds"))
      } else {
        nrb_bin2020_2035_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2020_2035_", i, ".tif")
      writeRaster(nrb_bin2020_2035, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2020_2035_list[[length(nrb_bin2020_2035_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2020_2035_list, file = paste0("temp_raster_lists/nrb_bin2020_2035_step_", i, ".rds"))
      
      ### harvest_st_bin2020_2035 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2020_2035_step_", i - 1, ".rds"))) {
        harvest_st_bin2020_2035_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2020_2035_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2020_2035_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2020_2035_", i, ".tif")
      writeRaster(harvest_st_bin2020_2035, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2020_2035_list[[length(harvest_st_bin2020_2035_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2020_2035_list, file = paste0("temp_raster_lists/harvest_st_bin2020_2035_step_", i, ".rds"))
      
      
      ### nrb_bin2020_2050----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2020_2050_step_", i - 1, ".rds"))) {
        nrb_bin2020_2050_list <- readRDS(paste0("temp_raster_lists/nrb_bin2020_2050_step_", i - 1, ".rds"))
      } else {
        nrb_bin2020_2050_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2020_2050_", i, ".tif")
      writeRaster(nrb_bin2020_2050, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2020_2050_list[[length(nrb_bin2020_2050_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2020_2050_list, file = paste0("temp_raster_lists/nrb_bin2020_2050_step_", i, ".rds"))
      
      ### harvest_st_bin2020_2050 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2020_2050_step_", i - 1, ".rds"))) {
        harvest_st_bin2020_2050_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2020_2050_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2020_2050_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2020_2050_", i, ".tif")
      writeRaster(harvest_st_bin2020_2050, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2020_2050_list[[length(harvest_st_bin2020_2050_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2020_2050_list, file = paste0("temp_raster_lists/harvest_st_bin2020_2050_step_", i, ".rds"))
      
      ### nrb_bin2030_2040----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2030_2040_step_", i - 1, ".rds"))) {
        nrb_bin2030_2040_list <- readRDS(paste0("temp_raster_lists/nrb_bin2030_2040_step_", i - 1, ".rds"))
      } else {
        nrb_bin2030_2040_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2030_2040_", i, ".tif")
      writeRaster(nrb_bin2030_2040, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2030_2040_list[[length(nrb_bin2030_2040_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2030_2040_list, file = paste0("temp_raster_lists/nrb_bin2030_2040_step_", i, ".rds"))
      
      ### harvest_st_bin2030_2040 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2030_2040_step_", i - 1, ".rds"))) {
        harvest_st_bin2030_2040_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2030_2040_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2030_2040_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2030_2040_", i, ".tif")
      writeRaster(harvest_st_bin2030_2040, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2030_2040_list[[length(harvest_st_bin2030_2040_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2030_2040_list, file = paste0("temp_raster_lists/harvest_st_bin2030_2040_step_", i, ".rds"))
      
      ### nrb_bin2040_2050----
      if (file.exists(paste0("temp_raster_lists/nrb_bin2040_2050_step_", i - 1, ".rds"))) {
        nrb_bin2040_2050_list <- readRDS(paste0("temp_raster_lists/nrb_bin2040_2050_step_", i - 1, ".rds"))
      } else {
        nrb_bin2040_2050_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_nrb <- paste0("temp_raster_lists/nrb_bin2040_2050_", i, ".tif")
      writeRaster(nrb_bin2040_2050, tif_file_nrb, overwrite=TRUE)
      # Store the file path in the list
      nrb_bin2040_2050_list[[length(nrb_bin2040_2050_list) + 1]] <- tif_file_nrb
      # Save the updated list of file paths to disk
      saveRDS(nrb_bin2040_2050_list, file = paste0("temp_raster_lists/nrb_bin2040_2050_step_", i, ".rds"))
      
      ### harvest_st_bin2040_2050 ----
      if (file.exists(paste0("temp_raster_lists/harvest_st_bin2040_2050_step_", i - 1, ".rds"))) {
        harvest_st_bin2040_2050_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2040_2050_step_", i - 1, ".rds"))
      } else {
        harvest_st_bin2040_2050_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_harv <- paste0("temp_raster_lists/harvest_st_bin2040_2050_", i, ".tif")
      writeRaster(harvest_st_bin2040_2050, tif_file_harv, overwrite=TRUE)
      # Store the file path in the list
      harvest_st_bin2040_2050_list[[length(harvest_st_bin2040_2050_list) + 1]] <- tif_file_harv
      # Save the updated list of file paths to disk
      saveRDS(harvest_st_bin2040_2050_list, file = paste0("temp_raster_lists/harvest_st_bin2040_2050_step_", i, ".rds"))
      
      ### agb_2010----
      if (file.exists(paste0("temp_raster_lists/agb_2010_step_", i - 1, ".rds"))) {
        agb_2010_list <- readRDS(paste0("temp_raster_lists/agb_2010_step_", i - 1, ".rds"))
      } else {
        agb_2010_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2010_", i, ".tif")
      agb_2010 <- stackG[[1]]
      writeRaster(agb_2010, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2010_list[[length(agb_2010_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2010_list, file = paste0("temp_raster_lists/agb_2010_step_", i, ".rds"))

      ### agb_2020----
      if (file.exists(paste0("temp_raster_lists/agb_2020_step_", i - 1, ".rds"))) {
        agb_2020_list <- readRDS(paste0("temp_raster_lists/agb_2020_step_", i - 1, ".rds"))
      } else {
        agb_2020_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2020_", i, ".tif")
      agb_2020 <- stackG[[10]]
      writeRaster(agb_2020, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2020_list[[length(agb_2020_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2020_list, file = paste0("temp_raster_lists/agb_2020_step_", i, ".rds"))

      ### agb_2030----
      if (file.exists(paste0("temp_raster_lists/agb_2030_step_", i - 1, ".rds"))) {
        agb_2030_list <- readRDS(paste0("temp_raster_lists/agb_2030_step_", i - 1, ".rds"))
      } else {
        agb_2030_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2030_", i, ".tif")
      agb_2030 <- stackG[[20]]
      writeRaster(agb_2030, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2030_list[[length(agb_2030_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2030_list, file = paste0("temp_raster_lists/agb_2030_step_", i, ".rds"))

      ### agb_2035----
      if (file.exists(paste0("temp_raster_lists/agb_2035_step_", i - 1, ".rds"))) {
        agb_2035_list <- readRDS(paste0("temp_raster_lists/agb_2035_step_", i - 1, ".rds"))
      } else {
        agb_2035_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2035_", i, ".tif")
      agb_2035 <- stackG[[25]]
      writeRaster(agb_2035, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2035_list[[length(agb_2035_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2035_list, file = paste0("temp_raster_lists/agb_2035_step_", i, ".rds"))

      ### agb_2040----
      if (file.exists(paste0("temp_raster_lists/agb_2040_step_", i - 1, ".rds"))) {
        agb_2040_list <- readRDS(paste0("temp_raster_lists/agb_2040_step_", i - 1, ".rds"))
      } else {
        agb_2040_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2040_", i, ".tif")
      agb_2040 <- stackG[[30]]
      writeRaster(agb_2040, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2040_list[[length(agb_2040_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2040_list, file = paste0("temp_raster_lists/agb_2040_step_", i, ".rds"))

      ### agb_2050----
      if (file.exists(paste0("temp_raster_lists/agb_2050_step_", i - 1, ".rds"))) {
        agb_2050_list <- readRDS(paste0("temp_raster_lists/agb_2050_step_", i - 1, ".rds"))
      } else {
        agb_2050_list <- list()
      }
      # Save each SpatRaster to disk
      tif_file_agb <- paste0("temp_raster_lists/agb_2050_", i, ".tif")
      agb_2050 <- stackG[[40]]
      writeRaster(agb_2050, tif_file_agb, overwrite=TRUE)
      # Store the file path in the list
      agb_2050_list[[length(agb_2050_list) + 1]] <- tif_file_agb
      # Save the updated list of file paths to disk
      saveRDS(agb_2050_list, file = paste0("temp_raster_lists/agb_2050_step_", i, ".rds"))
      
    }
    
    # Get all objects except a few ones and remove them
    objs_to_keep <- c("STdyn", "debug_dir", "debugging_dirs", "dir", "i")
    objs_to_remove <- setdiff(ls(), objs_to_keep)
    rm(list = objs_to_remove)
    
    # Free up memory
    gc()
    
    # Close all raster connections
    closeAllConnections()
    
    # Sleep for 15 seconds
    Sys.sleep(15)
    
  }
  
  # Define the output directory----
  ## ADJUST TO CHANGE SD FOR SE ----
  output_dir <- paste0(dir,"/OutBaU/webmofuss_results")
  
  if (STdyn == 10){
    # STdyn == 10 mean and sd ----
    # Calculate mean and standard deviation across all "debugging_n" folders
    # Load the list of file paths from disk
    nrb_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/nrb_bin2010_2020_step_",i,".rds"))
    harvest_st_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i, ".rds"))

    # Load each raster from its path and combine them into a multilayer SpatRaster
    nrb_bin2010_2020_rasters <- lapply(nrb_bin2010_2020_list, rast)
    harvest_st_bin2010_2020_rasters <- lapply(harvest_st_bin2010_2020_list, rast)

    # Combine the list of SpatRasters into a single multilayer SpatRaster
    nrb_bin2010_2020_rasters <- do.call(c, nrb_bin2010_2020_rasters)
    harvest_st_bin2010_2020_rasters <- do.call(c, harvest_st_bin2010_2020_rasters)

    # Calculate the mean and standard deviation across all layers in the SpatRaster
    nrb_bin2010_2020_mean <- app(nrb_bin2010_2020_rasters, fun = mean)
    nrb_bin2010_2020_se <- app(nrb_bin2010_2020_rasters, fun = sd)
    harvest_st_bin2010_2020_mean <- app(harvest_st_bin2010_2020_rasters, fun = mean)
    harvest_st_bin2010_2020_se <- app(harvest_st_bin2010_2020_rasters, fun = sd)
    harvest_st_bin2010_2020_mean[harvest_st_bin2010_2020_mean <= 0] = NA 
    harvest_st_bin2010_2020_se[harvest_st_bin2010_2020_se <= 0] = NA 

    # Save the resulting rasters
    writeRaster(nrb_bin2010_2020_mean, file.path(output_dir, "nrb_10_20_mean.tif"), overwrite=TRUE)
    writeRaster(nrb_bin2010_2020_se, file.path(output_dir, "nrb_10_20_se.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2010_2020_mean, file.path(output_dir, "harv_10_20_mean.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2010_2020_se, file.path(output_dir, "harv_10_20_se.tif"), overwrite=TRUE)

    # Repeat for agb 
    agb_2010_list <- readRDS(paste0("temp_raster_lists/agb_2010_step_", i, ".rds"))
    agb_2020_list <- readRDS(paste0("temp_raster_lists/agb_2020_step_", i, ".rds"))
    
    agb_2010_rasters <- lapply(agb_2010_list, rast)
    agb_2020_rasters <- lapply(agb_2020_list, rast)
    
    agb_2010_rasters <- do.call(c, agb_2010_rasters)
    agb_2020_rasters <- do.call(c, agb_2020_rasters)
    
    agb_2010_mean <- app(agb_2010_rasters, fun = mean)
    agb_2010_se <- app(agb_2010_rasters, fun = sd)
    agb_2020_mean <- app(agb_2020_rasters, fun = mean)
    agb_2020_se <- app(agb_2020_rasters, fun = sd)
    
    writeRaster(agb_2010_mean, file.path(output_dir, "agb_2010_mean.tif"), overwrite=TRUE)
    writeRaster(agb_2010_se, file.path(output_dir, "agb_2010_se.tif"), overwrite=TRUE)
    writeRaster(agb_2020_mean, file.path(output_dir, "agb_2020_mean.tif"), overwrite=TRUE)
    writeRaster(agb_2020_se, file.path(output_dir, "agb_2020_se.tif"), overwrite=TRUE)
    
  }
  
  if (STdyn == 20){
    # STdyn == 20 mean and sd ----
    # Calculate mean and standard deviation across all "debugging_n" folders
    ## 2010_2020 ----
    # Load the list of file paths from disk
    nrb_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/nrb_bin2010_2020_step_",i,".rds"))
    harvest_st_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i, ".rds"))
    
    # Load each raster from its path and combine them into a multilayer SpatRaster
    nrb_bin2010_2020_rasters <- lapply(nrb_bin2010_2020_list, rast)
    harvest_st_bin2010_2020_rasters <- lapply(harvest_st_bin2010_2020_list, rast)
    
    # Combine the list of SpatRasters into a single multilayer SpatRaster
    nrb_bin2010_2020_rasters <- do.call(c, nrb_bin2010_2020_rasters)
    harvest_st_bin2010_2020_rasters <- do.call(c, harvest_st_bin2010_2020_rasters)
    
    # Calculate the mean and standard deviation across all layers in the SpatRaster
    nrb_bin2010_2020_mean <- app(nrb_bin2010_2020_rasters, fun = mean)
    nrb_bin2010_2020_se <- app(nrb_bin2010_2020_rasters, fun = sd)
    harvest_st_bin2010_2020_mean <- app(harvest_st_bin2010_2020_rasters, fun = mean)
    harvest_st_bin2010_2020_se <- app(harvest_st_bin2010_2020_rasters, fun = sd)
    harvest_st_bin2010_2020_mean[harvest_st_bin2010_2020_mean <= 0] = NA 
    harvest_st_bin2010_2020_se[harvest_st_bin2010_2020_se <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2010_2020_mean, file.path(output_dir, "nrb_10_20_mean.tif"), overwrite=TRUE)
    writeRaster(nrb_bin2010_2020_se, file.path(output_dir, "nrb_10_20_se.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2010_2020_mean, file.path(output_dir, "harv_10_20_mean.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2010_2020_se, file.path(output_dir, "harv_10_20_se.tif"), overwrite=TRUE)
    
    ## 2020_2030 ----
    # Load the list of file paths from disk
    nrb_bin2020_2030_list <- readRDS(paste0("temp_raster_lists/nrb_bin2020_2030_step_",i,".rds"))
    harvest_st_bin2020_2030_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2020_2030_step_", i, ".rds"))
    
    # Load each raster from its path and combine them into a multilayer SpatRaster
    nrb_bin2020_2030_rasters <- lapply(nrb_bin2020_2030_list, rast)
    harvest_st_bin2020_2030_rasters <- lapply(harvest_st_bin2020_2030_list, rast)
    
    # Combine the list of SpatRasters into a single multilayer SpatRaster
    nrb_bin2020_2030_rasters <- do.call(c, nrb_bin2020_2030_rasters)
    harvest_st_bin2020_2030_rasters <- do.call(c, harvest_st_bin2020_2030_rasters)
    
    # Calculate the mean and standard deviation across all layers in the SpatRaster
    nrb_bin2020_2030_mean <- app(nrb_bin2020_2030_rasters, fun = mean)
    nrb_bin2020_2030_se <- app(nrb_bin2020_2030_rasters, fun = sd)
    harvest_st_bin2020_2030_mean <- app(harvest_st_bin2020_2030_rasters, fun = mean)
    harvest_st_bin2020_2030_se <- app(harvest_st_bin2020_2030_rasters, fun = sd)
    harvest_st_bin2020_2030_mean[harvest_st_bin2020_2030_mean <= 0] = NA 
    harvest_st_bin2020_2030_se[harvest_st_bin2020_2030_se <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2020_2030_mean, file.path(output_dir, "nrb_20_30_mean.tif"), overwrite=TRUE)
    writeRaster(nrb_bin2020_2030_se, file.path(output_dir, "nrb_20_30_se.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2030_mean, file.path(output_dir, "harv_20_30_mean.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2030_se, file.path(output_dir, "harv_20_30_se.tif"), overwrite=TRUE)
    
    
    # Repeat for agb 
    agb_2010_list <- readRDS(paste0("temp_raster_lists/agb_2010_step_", i, ".rds"))
    agb_2020_list <- readRDS(paste0("temp_raster_lists/agb_2020_step_", i, ".rds"))
    agb_2030_list <- readRDS(paste0("temp_raster_lists/agb_2030_step_", i, ".rds"))
    
    agb_2010_rasters <- lapply(agb_2010_list, rast)
    agb_2020_rasters <- lapply(agb_2020_list, rast)
    agb_2030_rasters <- lapply(agb_2030_list, rast)
    
    agb_2010_rasters <- do.call(c, agb_2010_rasters)
    agb_2020_rasters <- do.call(c, agb_2020_rasters)
    agb_2030_rasters <- do.call(c, agb_2030_rasters)
    
    agb_2010_mean <- app(agb_2010_rasters, fun = mean)
    agb_2010_se <- app(agb_2010_rasters, fun = sd)
    agb_2020_mean <- app(agb_2020_rasters, fun = mean)
    agb_2020_se <- app(agb_2020_rasters, fun = sd)
    agb_2030_mean <- app(agb_2030_rasters, fun = mean)
    agb_2030_se <- app(agb_2030_rasters, fun = sd)
    
    writeRaster(agb_2010_mean, file.path(output_dir, "agb_2010_mean.tif"), overwrite=TRUE)
    writeRaster(agb_2010_se, file.path(output_dir, "agb_2010_se.tif"), overwrite=TRUE)
    writeRaster(agb_2020_mean, file.path(output_dir, "agb_2020_mean.tif"), overwrite=TRUE)
    writeRaster(agb_2020_se, file.path(output_dir, "agb_2020_se.tif"), overwrite=TRUE)
    writeRaster(agb_2030_mean, file.path(output_dir, "agb_2030_mean.tif"), overwrite=TRUE)
    writeRaster(agb_2030_se, file.path(output_dir, "agb_2030_se.tif"), overwrite=TRUE)
    
  }
  
  if (STdyn == 25){
    # STdyn == 25 mean and sd ----
    # Calculate mean and standard deviation across all "debugging_n" folders
    ## 2010_2020 ----
    # Load the list of file paths from disk
    nrb_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/nrb_bin2010_2020_step_",i,".rds"))
    harvest_st_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i, ".rds"))
    
    # Load each raster from its path and combine them into a multilayer SpatRaster
    nrb_bin2010_2020_rasters <- lapply(nrb_bin2010_2020_list, rast)
    harvest_st_bin2010_2020_rasters <- lapply(harvest_st_bin2010_2020_list, rast)
    
    # Combine the list of SpatRasters into a single multilayer SpatRaster
    nrb_bin2010_2020_rasters <- do.call(c, nrb_bin2010_2020_rasters)
    harvest_st_bin2010_2020_rasters <- do.call(c, harvest_st_bin2010_2020_rasters)
    
    # Calculate the mean and standard deviation across all layers in the SpatRaster
    nrb_bin2010_2020_mean <- app(nrb_bin2010_2020_rasters, fun = mean)
    nrb_bin2010_2020_se <- app(nrb_bin2010_2020_rasters, fun = sd)
    harvest_st_bin2010_2020_mean <- app(harvest_st_bin2010_2020_rasters, fun = mean)
    harvest_st_bin2010_2020_se <- app(harvest_st_bin2010_2020_rasters, fun = sd)
    harvest_st_bin2010_2020_mean[harvest_st_bin2010_2020_mean <= 0] = NA 
    harvest_st_bin2010_2020_se[harvest_st_bin2010_2020_se <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2010_2020_mean, file.path(output_dir, "nrb_10_20_mean.tif"), overwrite=TRUE)
    writeRaster(nrb_bin2010_2020_se, file.path(output_dir, "nrb_10_20_se.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2010_2020_mean, file.path(output_dir, "harv_10_20_mean.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2010_2020_se, file.path(output_dir, "harv_10_20_se.tif"), overwrite=TRUE)
    
    ## 2020_2035 ----
    # Load the list of file paths from disk
    nrb_bin2020_2035_list <- readRDS(paste0("temp_raster_lists/nrb_bin2020_2035_step_",i,".rds"))
    harvest_st_bin2020_2035_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2020_2035_step_", i, ".rds"))
    
    # Load each raster from its path and combine them into a multilayer SpatRaster
    nrb_bin2020_2035_rasters <- lapply(nrb_bin2020_2035_list, rast)
    harvest_st_bin2020_2035_rasters <- lapply(harvest_st_bin2020_2035_list, rast)
    
    # Combine the list of SpatRasters into a single multilayer SpatRaster
    nrb_bin2020_2035_rasters <- do.call(c, nrb_bin2020_2035_rasters)
    harvest_st_bin2020_2035_rasters <- do.call(c, harvest_st_bin2020_2035_rasters)
    
    # Calculate the mean and standard deviation across all layers in the SpatRaster
    nrb_bin2020_2035_mean <- app(nrb_bin2020_2035_rasters, fun = mean)
    nrb_bin2020_2035_se <- app(nrb_bin2020_2035_rasters, fun = sd)
    harvest_st_bin2020_2035_mean <- app(harvest_st_bin2020_2035_rasters, fun = mean)
    harvest_st_bin2020_2035_se <- app(harvest_st_bin2020_2035_rasters, fun = sd)
    harvest_st_bin2020_2035_mean[harvest_st_bin2020_2035_mean <= 0] = NA 
    harvest_st_bin2020_2035_se[harvest_st_bin2020_2035_se <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2020_2035_mean, file.path(output_dir, "nrb_20_35_mean.tif"), overwrite=TRUE)
    writeRaster(nrb_bin2020_2035_se, file.path(output_dir, "nrb_20_35_se.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2035_mean, file.path(output_dir, "harv_20_35_mean.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2035_se, file.path(output_dir, "harv_20_35_se.tif"), overwrite=TRUE)
    
    
    # Repeat for agb 
    agb_2010_list <- readRDS(paste0("temp_raster_lists/agb_2010_step_", i, ".rds"))
    agb_2020_list <- readRDS(paste0("temp_raster_lists/agb_2020_step_", i, ".rds"))
    agb_2035_list <- readRDS(paste0("temp_raster_lists/agb_2035_step_", i, ".rds"))
    
    agb_2010_rasters <- lapply(agb_2010_list, rast)
    agb_2020_rasters <- lapply(agb_2020_list, rast)
    agb_2035_rasters <- lapply(agb_2035_list, rast)
    
    agb_2010_rasters <- do.call(c, agb_2010_rasters)
    agb_2020_rasters <- do.call(c, agb_2020_rasters)
    agb_2035_rasters <- do.call(c, agb_2035_rasters)
    
    agb_2010_mean <- app(agb_2010_rasters, fun = mean)
    agb_2010_se <- app(agb_2010_rasters, fun = sd)
    agb_2020_mean <- app(agb_2020_rasters, fun = mean)
    agb_2020_se <- app(agb_2020_rasters, fun = sd)
    agb_2035_mean <- app(agb_2035_rasters, fun = mean)
    agb_2035_se <- app(agb_2035_rasters, fun = sd)
    
    writeRaster(agb_2010_mean, file.path(output_dir, "agb_2010_mean.tif"), overwrite=TRUE)
    writeRaster(agb_2010_se, file.path(output_dir, "agb_2010_se.tif"), overwrite=TRUE)
    writeRaster(agb_2020_mean, file.path(output_dir, "agb_2020_mean.tif"), overwrite=TRUE)
    writeRaster(agb_2020_se, file.path(output_dir, "agb_2020_se.tif"), overwrite=TRUE)
    writeRaster(agb_2035_mean, file.path(output_dir, "agb_2035_mean.tif"), overwrite=TRUE)
    writeRaster(agb_2035_se, file.path(output_dir, "agb_2035_se.tif"), overwrite=TRUE)

    
  }
  
  if (STdyn == 30){
    # STdyn == 30 mean and sd ----
    # Calculate mean and standard deviation across all "debugging_n" folders
    ## 2010_2020 ----
    # Load the list of file paths from disk
    nrb_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/nrb_bin2010_2020_step_",i,".rds"))
    harvest_st_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i, ".rds"))
    
    # Load each raster from its path and combine them into a multilayer SpatRaster
    nrb_bin2010_2020_rasters <- lapply(nrb_bin2010_2020_list, rast)
    harvest_st_bin2010_2020_rasters <- lapply(harvest_st_bin2010_2020_list, rast)
    
    # Combine the list of SpatRasters into a single multilayer SpatRaster
    nrb_bin2010_2020_rasters <- do.call(c, nrb_bin2010_2020_rasters)
    harvest_st_bin2010_2020_rasters <- do.call(c, harvest_st_bin2010_2020_rasters)
    
    # Calculate the mean and standard deviation across all layers in the SpatRaster
    nrb_bin2010_2020_mean <- app(nrb_bin2010_2020_rasters, fun = mean)
    nrb_bin2010_2020_se <- app(nrb_bin2010_2020_rasters, fun = sd)
    harvest_st_bin2010_2020_mean <- app(harvest_st_bin2010_2020_rasters, fun = mean)
    harvest_st_bin2010_2020_se <- app(harvest_st_bin2010_2020_rasters, fun = sd)
    harvest_st_bin2010_2020_mean[harvest_st_bin2010_2020_mean <= 0] = NA 
    harvest_st_bin2010_2020_se[harvest_st_bin2010_2020_se <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2010_2020_mean, file.path(output_dir, "nrb_10_20_mean.tif"), overwrite=TRUE)
    writeRaster(nrb_bin2010_2020_se, file.path(output_dir, "nrb_10_20_se.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2010_2020_mean, file.path(output_dir, "harv_10_20_mean.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2010_2020_se, file.path(output_dir, "harv_10_20_se.tif"), overwrite=TRUE)
    
    ## 2020_2030 ----
    # Load the list of file paths from disk
    nrb_bin2020_2030_list <- readRDS(paste0("temp_raster_lists/nrb_bin2020_2030_step_",i,".rds"))
    harvest_st_bin2020_2030_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2020_2030_step_", i, ".rds"))
    
    # Load each raster from its path and combine them into a multilayer SpatRaster
    nrb_bin2020_2030_rasters <- lapply(nrb_bin2020_2030_list, rast)
    harvest_st_bin2020_2030_rasters <- lapply(harvest_st_bin2020_2030_list, rast)
    
    # Combine the list of SpatRasters into a single multilayer SpatRaster
    nrb_bin2020_2030_rasters <- do.call(c, nrb_bin2020_2030_rasters)
    harvest_st_bin2020_2030_rasters <- do.call(c, harvest_st_bin2020_2030_rasters)
    
    # Calculate the mean and standard deviation across all layers in the SpatRaster
    nrb_bin2020_2030_mean <- app(nrb_bin2020_2030_rasters, fun = mean)
    nrb_bin2020_2030_se <- app(nrb_bin2020_2030_rasters, fun = sd)
    harvest_st_bin2020_2030_mean <- app(harvest_st_bin2020_2030_rasters, fun = mean)
    harvest_st_bin2020_2030_se <- app(harvest_st_bin2020_2030_rasters, fun = sd)
    harvest_st_bin2020_2030_mean[harvest_st_bin2020_2030_mean <= 0] = NA 
    harvest_st_bin2020_2030_se[harvest_st_bin2020_2030_se <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2020_2030_mean, file.path(output_dir, "nrb_20_30_mean.tif"), overwrite=TRUE)
    writeRaster(nrb_bin2020_2030_se, file.path(output_dir, "nrb_20_30_se.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2030_mean, file.path(output_dir, "harv_20_30_mean.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2030_se, file.path(output_dir, "harv_20_30_se.tif"), overwrite=TRUE)
    
    ## 2030_2040 ----
    # Load the list of file paths from disk
    nrb_bin2030_2040_list <- readRDS(paste0("temp_raster_lists/nrb_bin2030_2040_step_",i,".rds"))
    harvest_st_bin2030_2040_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2030_2040_step_", i, ".rds"))
    
    # Load each raster from its path and combine them into a multilayer SpatRaster
    nrb_bin2030_2040_rasters <- lapply(nrb_bin2030_2040_list, rast)
    harvest_st_bin2030_2040_rasters <- lapply(harvest_st_bin2030_2040_list, rast)
    
    # Combine the list of SpatRasters into a single multilayer SpatRaster
    nrb_bin2030_2040_rasters <- do.call(c, nrb_bin2030_2040_rasters)
    harvest_st_bin2030_2040_rasters <- do.call(c, harvest_st_bin2030_2040_rasters)
    
    # Calculate the mean and standard deviation across all layers in the SpatRaster
    nrb_bin2030_2040_mean <- app(nrb_bin2030_2040_rasters, fun = mean)
    nrb_bin2030_2040_se <- app(nrb_bin2030_2040_rasters, fun = sd)
    harvest_st_bin2030_2040_mean <- app(harvest_st_bin2030_2040_rasters, fun = mean)
    harvest_st_bin2030_2040_se <- app(harvest_st_bin2030_2040_rasters, fun = sd)
    harvest_st_bin2030_2040_mean[harvest_st_bin2030_2040_mean <= 0] = NA 
    harvest_st_bin2030_2040_se[harvest_st_bin2030_2040_se <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2030_2040_mean, file.path(output_dir, "nrb_30_40_mean.tif"), overwrite=TRUE)
    writeRaster(nrb_bin2030_2040_se, file.path(output_dir, "nrb_30_40_se.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2030_2040_mean, file.path(output_dir, "harv_30_40_mean.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2030_2040_se, file.path(output_dir, "harv_30_40_se.tif"), overwrite=TRUE)
    
    # Repeat for agb 
    agb_2010_list <- readRDS(paste0("temp_raster_lists/agb_2010_step_", i, ".rds"))
    agb_2020_list <- readRDS(paste0("temp_raster_lists/agb_2020_step_", i, ".rds"))
    agb_2030_list <- readRDS(paste0("temp_raster_lists/agb_2030_step_", i, ".rds"))
    agb_2040_list <- readRDS(paste0("temp_raster_lists/agb_2040_step_", i, ".rds"))
    
    agb_2010_rasters <- lapply(agb_2010_list, rast)
    agb_2020_rasters <- lapply(agb_2020_list, rast)
    agb_2030_rasters <- lapply(agb_2030_list, rast)
    agb_2040_rasters <- lapply(agb_2040_list, rast)
    
    agb_2010_rasters <- do.call(c, agb_2010_rasters)
    agb_2020_rasters <- do.call(c, agb_2020_rasters)
    agb_2030_rasters <- do.call(c, agb_2030_rasters)
    agb_2040_rasters <- do.call(c, agb_2040_rasters)
    
    agb_2010_mean <- app(agb_2010_rasters, fun = mean)
    agb_2010_se <- app(agb_2010_rasters, fun = sd)
    agb_2020_mean <- app(agb_2020_rasters, fun = mean)
    agb_2020_se <- app(agb_2020_rasters, fun = sd)
    agb_2030_mean <- app(agb_2030_rasters, fun = mean)
    agb_2030_se <- app(agb_2030_rasters, fun = sd)
    agb_2040_mean <- app(agb_2040_rasters, fun = mean)
    agb_2040_se <- app(agb_2040_rasters, fun = sd)
    
    writeRaster(agb_2010_mean, file.path(output_dir, "agb_2010_mean.tif"), overwrite=TRUE)
    writeRaster(agb_2010_se, file.path(output_dir, "agb_2010_se.tif"), overwrite=TRUE)
    writeRaster(agb_2020_mean, file.path(output_dir, "agb_2020_mean.tif"), overwrite=TRUE)
    writeRaster(agb_2020_se, file.path(output_dir, "agb_2020_se.tif"), overwrite=TRUE)
    writeRaster(agb_2030_mean, file.path(output_dir, "agb_2030_mean.tif"), overwrite=TRUE)
    writeRaster(agb_2030_se, file.path(output_dir, "agb_2030_se.tif"), overwrite=TRUE)
    writeRaster(agb_2040_mean, file.path(output_dir, "agb_2040_mean.tif"), overwrite=TRUE)
    writeRaster(agb_2040_se, file.path(output_dir, "agb_2040_se.tif"), overwrite=TRUE)
    
  }
  
  if (STdyn == 40){
    # STdyn == 40 mean and sd ----
    # Calculate mean and standard deviation across all "debugging_n" folders
    ## 2010_2020 ----
    # Load the list of file paths from disk
    nrb_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/nrb_bin2010_2020_step_",i,".rds"))
    harvest_st_bin2010_2020_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2010_2020_step_", i, ".rds"))
    
    # Load each raster from its path and combine them into a multilayer SpatRaster
    nrb_bin2010_2020_rasters <- lapply(nrb_bin2010_2020_list, rast)
    harvest_st_bin2010_2020_rasters <- lapply(harvest_st_bin2010_2020_list, rast)
    
    # Combine the list of SpatRasters into a single multilayer SpatRaster
    nrb_bin2010_2020_rasters <- do.call(c, nrb_bin2010_2020_rasters)
    harvest_st_bin2010_2020_rasters <- do.call(c, harvest_st_bin2010_2020_rasters)
    
    # Calculate the mean and standard deviation across all layers in the SpatRaster
    nrb_bin2010_2020_mean <- app(nrb_bin2010_2020_rasters, fun = mean)
    nrb_bin2010_2020_se <- app(nrb_bin2010_2020_rasters, fun = sd)
    harvest_st_bin2010_2020_mean <- app(harvest_st_bin2010_2020_rasters, fun = mean)
    harvest_st_bin2010_2020_se <- app(harvest_st_bin2010_2020_rasters, fun = sd)
    harvest_st_bin2010_2020_mean[harvest_st_bin2010_2020_mean <= 0] = NA 
    harvest_st_bin2010_2020_se[harvest_st_bin2010_2020_se <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2010_2020_mean, file.path(output_dir, "nrb_10_20_mean.tif"), overwrite=TRUE)
    writeRaster(nrb_bin2010_2020_se, file.path(output_dir, "nrb_10_20_se.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2010_2020_mean, file.path(output_dir, "harv_10_20_mean.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2010_2020_se, file.path(output_dir, "harv_10_20_se.tif"), overwrite=TRUE)
    
    ## 2020_2030 ----
    # Load the list of file paths from disk
    nrb_bin2020_2030_list <- readRDS(paste0("temp_raster_lists/nrb_bin2020_2030_step_",i,".rds"))
    harvest_st_bin2020_2030_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2020_2030_step_", i, ".rds"))
    
    # Load each raster from its path and combine them into a multilayer SpatRaster
    nrb_bin2020_2030_rasters <- lapply(nrb_bin2020_2030_list, rast)
    harvest_st_bin2020_2030_rasters <- lapply(harvest_st_bin2020_2030_list, rast)
    
    # Combine the list of SpatRasters into a single multilayer SpatRaster
    nrb_bin2020_2030_rasters <- do.call(c, nrb_bin2020_2030_rasters)
    harvest_st_bin2020_2030_rasters <- do.call(c, harvest_st_bin2020_2030_rasters)
    
    # Calculate the mean and standard deviation across all layers in the SpatRaster
    nrb_bin2020_2030_mean <- app(nrb_bin2020_2030_rasters, fun = mean)
    nrb_bin2020_2030_se <- app(nrb_bin2020_2030_rasters, fun = sd)
    harvest_st_bin2020_2030_mean <- app(harvest_st_bin2020_2030_rasters, fun = mean)
    harvest_st_bin2020_2030_se <- app(harvest_st_bin2020_2030_rasters, fun = sd)
    harvest_st_bin2020_2030_mean[harvest_st_bin2020_2030_mean <= 0] = NA 
    harvest_st_bin2020_2030_se[harvest_st_bin2020_2030_se <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2020_2030_mean, file.path(output_dir, "nrb_20_30_mean.tif"), overwrite=TRUE)
    writeRaster(nrb_bin2020_2030_se, file.path(output_dir, "nrb_20_30_se.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2030_mean, file.path(output_dir, "harv_20_30_mean.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2030_se, file.path(output_dir, "harv_20_30_se.tif"), overwrite=TRUE)
    
    ## 2020_2035 ----
    # Load the list of file paths from disk
    nrb_bin2020_2035_list <- readRDS(paste0("temp_raster_lists/nrb_bin2020_2035_step_",i,".rds"))
    harvest_st_bin2020_2035_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2020_2035_step_", i, ".rds"))
    
    # Load each raster from its path and combine them into a multilayer SpatRaster
    nrb_bin2020_2035_rasters <- lapply(nrb_bin2020_2035_list, rast)
    harvest_st_bin2020_2035_rasters <- lapply(harvest_st_bin2020_2035_list, rast)
    
    # Combine the list of SpatRasters into a single multilayer SpatRaster
    nrb_bin2020_2035_rasters <- do.call(c, nrb_bin2020_2035_rasters)
    harvest_st_bin2020_2035_rasters <- do.call(c, harvest_st_bin2020_2035_rasters)
    
    # Calculate the mean and standard deviation across all layers in the SpatRaster
    nrb_bin2020_2035_mean <- app(nrb_bin2020_2035_rasters, fun = mean)
    nrb_bin2020_2035_se <- app(nrb_bin2020_2035_rasters, fun = sd)
    harvest_st_bin2020_2035_mean <- app(harvest_st_bin2020_2035_rasters, fun = mean)
    harvest_st_bin2020_2035_se <- app(harvest_st_bin2020_2035_rasters, fun = sd)
    harvest_st_bin2020_2035_mean[harvest_st_bin2020_2035_mean <= 0] = NA 
    harvest_st_bin2020_2035_se[harvest_st_bin2020_2035_se <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2020_2035_mean, file.path(output_dir, "nrb_20_35_mean.tif"), overwrite=TRUE)
    writeRaster(nrb_bin2020_2035_se, file.path(output_dir, "nrb_20_35_se.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2035_mean, file.path(output_dir, "harv_20_35_mean.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2035_se, file.path(output_dir, "harv_20_35_se.tif"), overwrite=TRUE)
    
    ## 2020_2050 ----
    # Load the list of file paths from disk
    nrb_bin2020_2050_list <- readRDS(paste0("temp_raster_lists/nrb_bin2020_2050_step_",i,".rds"))
    harvest_st_bin2020_2050_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2020_2050_step_", i, ".rds"))
    
    # Load each raster from its path and combine them into a multilayer SpatRaster
    nrb_bin2020_2050_rasters <- lapply(nrb_bin2020_2050_list, rast)
    harvest_st_bin2020_2050_rasters <- lapply(harvest_st_bin2020_2050_list, rast)
    
    # Combine the list of SpatRasters into a single multilayer SpatRaster
    nrb_bin2020_2050_rasters <- do.call(c, nrb_bin2020_2050_rasters)
    harvest_st_bin2020_2050_rasters <- do.call(c, harvest_st_bin2020_2050_rasters)
    
    # Calculate the mean and standard deviation across all layers in the SpatRaster
    nrb_bin2020_2050_mean <- app(nrb_bin2020_2050_rasters, fun = mean)
    nrb_bin2020_2050_se <- app(nrb_bin2020_2050_rasters, fun = sd)
    harvest_st_bin2020_2050_mean <- app(harvest_st_bin2020_2050_rasters, fun = mean)
    harvest_st_bin2020_2050_se <- app(harvest_st_bin2020_2050_rasters, fun = sd)
    harvest_st_bin2020_2050_mean[harvest_st_bin2020_2050_mean <= 0] = NA 
    harvest_st_bin2020_2050_se[harvest_st_bin2020_2050_se <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2020_2050_mean, file.path(output_dir, "nrb_20_50_mean.tif"), overwrite=TRUE)
    writeRaster(nrb_bin2020_2050_se, file.path(output_dir, "nrb_20_50_se.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2050_mean, file.path(output_dir, "harv_20_50_mean.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2050_se, file.path(output_dir, "harv_20_50_se.tif"), overwrite=TRUE)
    
    ## 2030_2040 ----
    # Load the list of file paths from disk
    nrb_bin2030_2040_list <- readRDS(paste0("temp_raster_lists/nrb_bin2030_2040_step_",i,".rds"))
    harvest_st_bin2030_2040_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2030_2040_step_", i, ".rds"))
    
    # Load each raster from its path and combine them into a multilayer SpatRaster
    nrb_bin2030_2040_rasters <- lapply(nrb_bin2030_2040_list, rast)
    harvest_st_bin2030_2040_rasters <- lapply(harvest_st_bin2030_2040_list, rast)
    
    # Combine the list of SpatRasters into a single multilayer SpatRaster
    nrb_bin2030_2040_rasters <- do.call(c, nrb_bin2030_2040_rasters)
    harvest_st_bin2030_2040_rasters <- do.call(c, harvest_st_bin2030_2040_rasters)
    
    # Calculate the mean and standard deviation across all layers in the SpatRaster
    nrb_bin2030_2040_mean <- app(nrb_bin2030_2040_rasters, fun = mean)
    nrb_bin2030_2040_se <- app(nrb_bin2030_2040_rasters, fun = sd)
    harvest_st_bin2030_2040_mean <- app(harvest_st_bin2030_2040_rasters, fun = mean)
    harvest_st_bin2030_2040_se <- app(harvest_st_bin2030_2040_rasters, fun = sd)
    harvest_st_bin2030_2040_mean[harvest_st_bin2030_2040_mean <= 0] = NA 
    harvest_st_bin2030_2040_se[harvest_st_bin2030_2040_se <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2030_2040_mean, file.path(output_dir, "nrb_30_40_mean.tif"), overwrite=TRUE)
    writeRaster(nrb_bin2030_2040_se, file.path(output_dir, "nrb_30_40_se.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2030_2040_mean, file.path(output_dir, "harv_30_40_mean.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2030_2040_se, file.path(output_dir, "harv_30_40_se.tif"), overwrite=TRUE)
    
    ## 2040_2050 ----
    # Load the list of file paths from disk
    nrb_bin2040_2050_list <- readRDS(paste0("temp_raster_lists/nrb_bin2040_2050_step_",i,".rds"))
    harvest_st_bin2040_2050_list <- readRDS(paste0("temp_raster_lists/harvest_st_bin2040_2050_step_", i, ".rds"))
    
    # Load each raster from its path and combine them into a multilayer SpatRaster
    nrb_bin2040_2050_rasters <- lapply(nrb_bin2040_2050_list, rast)
    harvest_st_bin2040_2050_rasters <- lapply(harvest_st_bin2040_2050_list, rast)
    
    # Combine the list of SpatRasters into a single multilayer SpatRaster
    nrb_bin2040_2050_rasters <- do.call(c, nrb_bin2040_2050_rasters)
    harvest_st_bin2040_2050_rasters <- do.call(c, harvest_st_bin2040_2050_rasters)
    
    # Calculate the mean and standard deviation across all layers in the SpatRaster
    nrb_bin2040_2050_mean <- app(nrb_bin2040_2050_rasters, fun = mean)
    nrb_bin2040_2050_se <- app(nrb_bin2040_2050_rasters, fun = sd)
    harvest_st_bin2040_2050_mean <- app(harvest_st_bin2040_2050_rasters, fun = mean)
    harvest_st_bin2040_2050_se <- app(harvest_st_bin2040_2050_rasters, fun = sd)
    harvest_st_bin2040_2050_mean[harvest_st_bin2040_2050_mean <= 0] = NA 
    harvest_st_bin2040_2050_se[harvest_st_bin2040_2050_se <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2040_2050_mean, file.path(output_dir, "nrb_40_50_mean.tif"), overwrite=TRUE)
    writeRaster(nrb_bin2040_2050_se, file.path(output_dir, "nrb_40_50_se.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2040_2050_mean, file.path(output_dir, "harv_40_50_mean.tif"), overwrite=TRUE)
    writeRaster(harvest_st_bin2040_2050_se, file.path(output_dir, "harv_40_50_se.tif"), overwrite=TRUE)
    
    # Repeat for agb 
    agb_2010_list <- readRDS(paste0("temp_raster_lists/agb_2010_step_", i, ".rds"))
    agb_2020_list <- readRDS(paste0("temp_raster_lists/agb_2020_step_", i, ".rds"))
    agb_2030_list <- readRDS(paste0("temp_raster_lists/agb_2030_step_", i, ".rds"))
    agb_2035_list <- readRDS(paste0("temp_raster_lists/agb_2035_step_", i, ".rds"))
    agb_2040_list <- readRDS(paste0("temp_raster_lists/agb_2040_step_", i, ".rds"))
    agb_2050_list <- readRDS(paste0("temp_raster_lists/agb_2050_step_", i, ".rds"))
    
    agb_2010_rasters <- lapply(agb_2010_list, rast)
    agb_2020_rasters <- lapply(agb_2020_list, rast)
    agb_2030_rasters <- lapply(agb_2030_list, rast)
    agb_2035_rasters <- lapply(agb_2035_list, rast)
    agb_2040_rasters <- lapply(agb_2040_list, rast)
    agb_2050_rasters <- lapply(agb_2050_list, rast)
    
    agb_2010_rasters <- do.call(c, agb_2010_rasters)
    agb_2020_rasters <- do.call(c, agb_2020_rasters)
    agb_2030_rasters <- do.call(c, agb_2030_rasters)
    agb_2035_rasters <- do.call(c, agb_2035_rasters)
    agb_2040_rasters <- do.call(c, agb_2040_rasters)
    agb_2050_rasters <- do.call(c, agb_2050_rasters)
    
    agb_2010_mean <- app(agb_2010_rasters, fun = mean)
    agb_2010_se <- app(agb_2010_rasters, fun = sd)
    agb_2020_mean <- app(agb_2020_rasters, fun = mean)
    agb_2020_se <- app(agb_2020_rasters, fun = sd)
    agb_2030_mean <- app(agb_2030_rasters, fun = mean)
    agb_2030_se <- app(agb_2030_rasters, fun = sd)
    agb_2035_mean <- app(agb_2035_rasters, fun = mean)
    agb_2035_se <- app(agb_2035_rasters, fun = sd)
    agb_2040_mean <- app(agb_2040_rasters, fun = mean)
    agb_2040_se <- app(agb_2040_rasters, fun = sd)
    agb_2050_mean <- app(agb_2050_rasters, fun = mean)
    agb_2050_se <- app(agb_2050_rasters, fun = sd)
    
    writeRaster(agb_2010_mean, file.path(output_dir, "agb_2010_mean.tif"), overwrite=TRUE)
    writeRaster(agb_2010_se, file.path(output_dir, "agb_2010_se.tif"), overwrite=TRUE)
    writeRaster(agb_2020_mean, file.path(output_dir, "agb_2020_mean.tif"), overwrite=TRUE)
    writeRaster(agb_2020_se, file.path(output_dir, "agb_2020_se.tif"), overwrite=TRUE)
    writeRaster(agb_2030_mean, file.path(output_dir, "agb_2030_mean.tif"), overwrite=TRUE)
    writeRaster(agb_2030_se, file.path(output_dir, "agb_2030_se.tif"), overwrite=TRUE)
    writeRaster(agb_2035_mean, file.path(output_dir, "agb_2035_mean.tif"), overwrite=TRUE)
    writeRaster(agb_2035_se, file.path(output_dir, "agb_2035_se.tif"), overwrite=TRUE)
    writeRaster(agb_2040_mean, file.path(output_dir, "agb_2040_mean.tif"), overwrite=TRUE)
    writeRaster(agb_2040_se, file.path(output_dir, "agb_2040_se.tif"), overwrite=TRUE)
    writeRaster(agb_2050_mean, file.path(output_dir, "agb_2050_mean.tif"), overwrite=TRUE)
    writeRaster(agb_2050_se, file.path(output_dir, "agb_2050_se.tif"), overwrite=TRUE)
    

  }
  
  unlink("temp_raster_lists", recursive= TRUE, force=TRUE)
  
}

toc()





