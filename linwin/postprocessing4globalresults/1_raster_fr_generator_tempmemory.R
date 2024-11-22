# MoFuSS
# Version 3
# Date: May 2024

# 2dolist
## Faltaría 2010-2050

# Internal parameters
fixdir <- 0

# Load packages ----
library(raster)
library(dplyr)
library(readxl)
library(tictoc)
library(fs)
library(tcltk)

tic()

if (fixdir == 1){
  
  # Define a particular directory when needed:
  # adm0_dirs <- c("D:/MoFuSS_Nepal_100m_May2024_4Chloris")
  # adm0_dirs <- c("D:/MoFuSS_Nepal_100m_May2024")
  adm0_dirs <- c("E:/ASIA_adm0_india_apr2024")

} else {
  
  # Define the directory to search
  setwd(tk_choose.dir(default = getwd(), caption = "Define the directory to search"))
  search_path <- getwd()
  
  # List all directories in the specified path
  all_dirs <- dir_ls(search_path, type = "directory")
  
  # Filter directories containing 'adm0'
  adm0_dirs <- all_dirs[grepl("adm0", all_dirs)]
  
}

# Loop through each adm0 directory
for (dir in adm0_dirs) {
  dir = "D:/SSA_adm0_zambia_apr2024"
  # dir = "D:/SSA_adm0_kenya_apr2024"
  print(paste("Processing directory:", dir))
  
  # Initialize lists to store the rasters ----
  nrb_bin2010_2020_list <- list()
  harvest_st_bin2010_2020_list <- list()
  nrb_bin2020_2030_list <- list()
  harvest_st_bin2020_2030_list <- list()
  nrb_bin2020_2035_list <- list()
  harvest_st_bin2020_2035_list <- list()
  nrb_bin2020_2050_list <- list()
  harvest_st_bin2020_2050_list <- list()
  nrb_bin2030_2040_list <- list()
  harvest_st_bin2030_2040_list <- list()
  nrb_bin2040_2050_list <- list()
  harvest_st_bin2040_2050_list <- list()
  agb_2010_list <- list()
  agb_2020_list <- list()
  # agb_2025_list <- list()
  agb_2030_list <- list()
  agb_2035_list <- list()
  agb_2040_list <- list() 
  agb_2050_list <- list()
  
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
  
  for (debug_dir in debugging_dirs) {
    # debug_dir = "D:/SSA_adm0_kenya_apr2024/debugging_1"
    
    # NRB 
    listGlH <- list.files(debug_dir, pattern = "^Growth_less_harv.+[.]tif$",ignore.case=F)
    stackGlH <- stack(paste0(debug_dir,"/",listGlH))
    nlay <- nlayers(stackGlH)
    
    listGx <- list.files(debug_dir, pattern = "^Growth.+[.]tif$",ignore.case=F)
    listG <- listGx[ !grepl("_less_harv", listGx) ]
    stackG <- stack(paste0(debug_dir,"/",listG))
    nlayers(stackG) #for cross checking pattern
    
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
    
    # Harvest
    
    listharvx_per <- list.files(debug_dir, pattern = "^Harvest_tot.+[.]tif$",ignore.case=F)
    listharv_per <- listharvx_per[ !grepl("_tot_nrb", listharvx_per) ]
    stackhar_per <- stack(paste0(debug_dir,"/",listharv_per))
    nlayers(stackhar_per)
    
    harv_name_per <- paste("harv_sum_bin2010", nlay_yr, sep = "_")
    harvest_st_per <- stackApply(stackhar_per, indices=1, fun=sum)
    assign(harv_name_per, harvest_st_per)
    
    harv_name <- paste("harv_sum_bin2020", nlay_yr, sep = "_")
    listharv <- listharv_per[11:nlay]
    stackharv <- stack(paste0(debug_dir,"/",listharv))
    nlayers(stackharv)
    
    harvest_st <- stackApply(stackharv, indices=1, fun=sum)
    assign(harv_name, harvest_st)
    
    if (STdyn == 10){
      listharv_bin2010_2020 <- listharv_per[1:nlay]
      stackharv_bin2010_2020 <- stack(paste0(debug_dir,"/",listharv_bin2010_2020))
      nlayers(stackharv_bin2010_2020)
      harvest_st_bin2010_2020 <- stackApply(stackharv_bin2010_2020, indices=1, fun=sum)
      
    }
    
    if (STdyn == 20){
      listharv_bin2010_2020 <- listharv_per[1:10]
      stackharv_bin2010_2020 <- stack(paste0(debug_dir,"/",listharv_bin2010_2020))
      nlayers(stackharv_bin2010_2020)
      harvest_st_bin2010_2020 <- stackApply(stackharv_bin2010_2020, indices=1, fun=sum)
      
      listharv_bin2020_2030 <- listharv_per[11:nlay]
      stackharv_bin2020_2030 <- stack(paste0(debug_dir,"/",listharv_bin2020_2030))
      nlayers(stackharv_bin2020_2030)
      harvest_st_bin2020_2030 <- stackApply(stackharv_bin2020_2030, indices=1, fun=sum)
      
    }
    
    if (STdyn == 25){
      listharv_bin2010_2020 <- listharv_per[1:10]
      stackharv_bin2010_2020 <- stack(paste0(debug_dir,"/",listharv_bin2010_2020))
      nlayers(stackharv_bin2010_2020)
      harvest_st_bin2010_2020 <- stackApply(stackharv_bin2010_2020, indices=1, fun=sum)
      
      listharv_bin2020_2035 <- listharv_per[11:nlay]
      stackharv_bin2020_2035 <- stack(paste0(debug_dir,"/",listharv_bin2020_2035))
      nlayers(stackharv_bin2020_2035)
      harvest_st_bin2020_2035 <- stackApply(stackharv_bin2020_2035, indices=1, fun=sum)
      
    }
    
    if (STdyn == 30){
      listharv_bin2010_2020 <- listharv_per[1:10]
      stackharv_bin2010_2020 <- stack(paste0(debug_dir,"/",listharv_bin2010_2020))
      nlayers(stackharv_bin2010_2020)
      harvest_st_bin2010_2020 <- stackApply(stackharv_bin2010_2020, indices=1, fun=sum)
      
      listharv_bin2020_2030 <- listharv_per[11:20]
      stackharv_bin2020_2030 <- stack(paste0(debug_dir,"/",listharv_bin2020_2030))
      nlayers(stackharv_bin2020_2030)
      harvest_st_bin2020_2030 <- stackApply(stackharv_bin2020_2030, indices=1, fun=sum)
      
      listharv_bin2030_2040 <- listharv_per[21:nlay]
      stackharv_bin2030_2040 <- stack(paste0(debug_dir,"/",listharv_bin2030_2040))
      nlayers(stackharv_bin2030_2040)
      harvest_st_bin2030_2040 <- stackApply(stackharv_bin2030_2040, indices=1, fun=sum)
      
    }
    
    if (STdyn == 40){
      listharv_bin2010_2020 <- listharv_per[1:10]
      stackharv_bin2010_2020 <- stack(paste0(debug_dir,"/",listharv_bin2010_2020))
      nlayers(stackharv_bin2010_2020)
      harvest_st_bin2010_2020 <- stackApply(stackharv_bin2010_2020, indices=1, fun=sum)
      
      listharv_bin2020_2030 <- listharv_per[11:20]
      stackharv_bin2020_2030 <- stack(paste0(debug_dir,"/",listharv_bin2020_2030))
      nlayers(stackharv_bin2020_2030)
      harvest_st_bin2020_2030 <- stackApply(stackharv_bin2020_2030, indices=1, fun=sum)
      
      listharv_bin2020_2035 <- listharv_per[11:25]
      stackharv_bin2020_2035 <- stack(paste0(debug_dir,"/",listharv_bin2020_2035))
      nlayers(stackharv_bin2020_2035)
      harvest_st_bin2020_2035 <- stackApply(stackharv_bin2020_2035, indices=1, fun=sum)
      
      listharv_bin2020_2050 <- listharv_per[11:nlay]
      stackharv_bin2020_2050 <- stack(paste0(debug_dir,"/",listharv_bin2020_2050))
      nlayers(stackharv_bin2020_2050)
      harvest_st_bin2020_2050 <- stackApply(stackharv_bin2020_2050, indices=1, fun=sum)
      
      listharv_bin2030_2040 <- listharv_per[21:30]
      stackharv_bin2030_2040 <- stack(paste0(debug_dir,"/",listharv_bin2030_2040))
      nlayers(stackharv_bin2030_2040)
      harvest_st_bin2030_2040 <- stackApply(stackharv_bin2030_2040, indices=1, fun=sum)
      
      listharv_bin2040_2050 <- listharv_per[31:nlay]
      stackharv_bin2040_2050 <- stack(paste0(debug_dir,"/",listharv_bin2040_2050))
      nlayers(stackharv_bin2040_2050)
      harvest_st_bin2040_2050 <- stackApply(stackharv_bin2040_2050, indices=1, fun=sum)
      
    }
    
    # Store the rasters in the lists
    if (STdyn == 10){
      nrb_bin2010_2020_list[[length(nrb_bin2010_2020_list) + 1]] <- nrb_bin2010_2020
      harvest_st_bin2010_2020_list[[length(harvest_st_bin2010_2020_list) + 1]] <- harvest_st_bin2010_2020
      
      agb_2010_list[[length(agb_2010_list) + 1]] <- stackG[[1]]
      agb_2020_list[[length(agb_2020_list) + 1]] <- stackGlH[[10]]
    }
    
    if (STdyn == 20){
      nrb_bin2010_2020_list[[length(nrb_bin2010_2020_list) + 1]] <- nrb_bin2010_2020
      harvest_st_bin2010_2020_list[[length(harvest_st_bin2010_2020_list) + 1]] <- harvest_st_bin2010_2020
      
      nrb_bin2020_2030_list[[length(nrb_bin2020_2030_list) + 1]] <- nrb_bin2020_2030
      harvest_st_bin2020_2030_list[[length(harvest_st_bin2020_2030_list) + 1]] <- harvest_st_bin2020_2030
      
      agb_2010_list[[length(agb_2010_list) + 1]] <- stackG[[1]]
      agb_2020_list[[length(agb_2020_list) + 1]] <- stackGlH[[10]]
      agb_2030_list[[length(agb_2030_list) + 1]] <- stackGlH[[20]]
    }
    
    if (STdyn == 25){
      nrb_bin2010_2020_list[[length(nrb_bin2010_2020_list) + 1]] <- nrb_bin2010_2020
      harvest_st_bin2010_2020_list[[length(harvest_st_bin2010_2020_list) + 1]] <- harvest_st_bin2010_2020
      
      nrb_bin2020_2035_list[[length(nrb_bin2020_2035_list) + 1]] <- nrb_bin2020_2035
      harvest_st_bin2020_2035_list[[length(harvest_st_bin2020_2035_list) + 1]] <- harvest_st_bin2020_2035
      
      agb_2010_list[[length(agb_2010_list) + 1]] <- stackG[[1]]
      agb_2020_list[[length(agb_2020_list) + 1]] <- stackGlH[[10]]
      agb_2035_list[[length(agb_2035_list) + 1]] <- stackGlH[[25]]
    }
    
    if (STdyn == 30){
      nrb_bin2010_2020_list[[length(nrb_bin2010_2020_list) + 1]] <- nrb_bin2010_2020
      harvest_st_bin2010_2020_list[[length(harvest_st_bin2010_2020_list) + 1]] <- harvest_st_bin2010_2020
      
      nrb_bin2020_2030_list[[length(nrb_bin2020_2030_list) + 1]] <- nrb_bin2020_2030
      harvest_st_bin2020_2030_list[[length(harvest_st_bin2020_2030_list) + 1]] <- harvest_st_bin2020_2030
      
      nrb_bin2030_2040_list[[length(nrb_bin2030_2040_list) + 1]] <- nrb_bin2030_2040
      harvest_st_bin2030_2040_list[[length(harvest_st_bin2030_2040_list) + 1]] <- harvest_st_bin2030_2040
      
      agb_2010_list[[length(agb_2010_list) + 1]] <- stackG[[1]]
      agb_2020_list[[length(agb_2020_list) + 1]] <- stackGlH[[10]]
      agb_2030_list[[length(agb_2030_list) + 1]] <- stackGlH[[20]]
      agb_2040_list[[length(agb_2040_list) + 1]] <- stackGlH[[30]]
    }
    
    if (STdyn == 40){
      nrb_bin2010_2020_list[[length(nrb_bin2010_2020_list) + 1]] <- nrb_bin2010_2020
      harvest_st_bin2010_2020_list[[length(harvest_st_bin2010_2020_list) + 1]] <- harvest_st_bin2010_2020
      
      nrb_bin2020_2030_list[[length(nrb_bin2020_2030_list) + 1]] <- nrb_bin2020_2030
      harvest_st_bin2020_2030_list[[length(harvest_st_bin2020_2030_list) + 1]] <- harvest_st_bin2020_2030
      
      nrb_bin2020_2035_list[[length(nrb_bin2020_2035_list) + 1]] <- nrb_bin2020_2035
      harvest_st_bin2020_2035_list[[length(harvest_st_bin2020_2035_list) + 1]] <- harvest_st_bin2020_2035
      
      nrb_bin2020_2050_list[[length(nrb_bin2020_2050_list) + 1]] <- nrb_bin2020_2050
      harvest_st_bin2020_2050_list[[length(harvest_st_bin2020_2050_list) + 1]] <- harvest_st_bin2020_2050
      
      nrb_bin2030_2040_list[[length(nrb_bin2030_2040_list) + 1]] <- nrb_bin2030_2040
      harvest_st_bin2030_2040_list[[length(harvest_st_bin2030_2040_list) + 1]] <- harvest_st_bin2030_2040
      
      nrb_bin2040_2050_list[[length(nrb_bin2040_2050_list) + 1]] <- nrb_bin2040_2050
      harvest_st_bin2040_2050_list[[length(harvest_st_bin2040_2050_list) + 1]] <- harvest_st_bin2040_2050
      
      agb_2010_list[[length(agb_2010_list) + 1]] <- stackG[[1]]
      agb_2020_list[[length(agb_2020_list) + 1]] <- stackGlH[[10]]
      agb_2030_list[[length(agb_2030_list) + 1]] <- stackGlH[[20]]
      agb_2035_list[[length(agb_2035_list) + 1]] <- stackGlH[[25]]
      agb_2040_list[[length(agb_2040_list) + 1]] <- stackGlH[[30]]
      agb_2050_list[[length(agb_2050_list) + 1]] <- stackGlH[[40]]
    }
    
  }
  
  # Define the output directory
  output_dir <- paste0(dir,"/OutBaU/webmofuss_results")
  
  if (STdyn == 10){
    # Calculate mean and standard deviation across all "debugging_n" folders
    nrb_bin2010_2020_mean <- stack(nrb_bin2010_2020_list) %>% calc(mean)
    nrb_bin2010_2020_sd <- stack(nrb_bin2010_2020_list) %>% calc(sd)
    harvest_st_bin2010_2020_mean <- stack(harvest_st_bin2010_2020_list) %>% calc(mean)
    harvest_st_bin2010_2020_sd <- stack(harvest_st_bin2010_2020_list) %>% calc(sd)
    harvest_st_bin2010_2020_mean[harvest_st_bin2010_2020_mean <= 0] = NA 
    harvest_st_bin2010_2020_sd[harvest_st_bin2010_2020_sd <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2010_2020_mean, file.path(output_dir, "nrb_10_20_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(nrb_bin2010_2020_sd, file.path(output_dir, "nrb_10_20_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2010_2020_mean, file.path(output_dir, "harv_10_20_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2010_2020_sd, file.path(output_dir, "harv_10_20_sd.tif"), format="GTiff", overwrite=TRUE)
    
    agb_2010_mean <- stack(agb_2010_list) %>% calc(mean)
    agb_2010_sd <- stack(agb_2010_list) %>% calc(sd)
    agb_2020_mean <- stack(agb_2020_list) %>% calc(mean)
    agb_2020_sd <- stack(agb_2020_list) %>% calc(sd)
    
    writeRaster(agb_2010_mean, file.path(output_dir, "agb_2010_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2010_sd, file.path(output_dir, "agb_2010_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2020_mean, file.path(output_dir, "agb_2020_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2020_sd, file.path(output_dir, "agb_2020_sd.tif"), format="GTiff", overwrite=TRUE)
    
  }
  
  if (STdyn == 20){
    # Calculate mean and standard deviation across all "debugging_n" folders
    nrb_bin2010_2020_mean <- stack(nrb_bin2010_2020_list) %>% calc(mean)
    nrb_bin2010_2020_sd <- stack(nrb_bin2010_2020_list) %>% calc(sd)
    harvest_st_bin2010_2020_mean <- stack(harvest_st_bin2010_2020_list) %>% calc(mean)
    harvest_st_bin2010_2020_sd <- stack(harvest_st_bin2010_2020_list) %>% calc(sd)
    harvest_st_bin2010_2020_mean[harvest_st_bin2010_2020_mean <= 0] = NA
    harvest_st_bin2010_2020_sd[harvest_st_bin2010_2020_sd <= 0] = NA
    
    # Save the resulting rasters
    writeRaster(nrb_bin2010_2020_mean, file.path(output_dir, "nrb_10_20_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(nrb_bin2010_2020_sd, file.path(output_dir, "nrb_10_20_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2010_2020_mean, file.path(output_dir, "harv_10_20_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2010_2020_sd, file.path(output_dir, "harv_10_20_sd.tif"), format="GTiff", overwrite=TRUE)
    
    # Calculate mean and standard deviation across all "debugging_n" folders
    nrb_bin2020_2030_mean <- stack(nrb_bin2020_2030_list) %>% calc(mean)
    nrb_bin2020_2030_sd <- stack(nrb_bin2020_2030_list) %>% calc(sd)
    harvest_st_bin2020_2030_mean <- stack(harvest_st_bin2020_2030_list) %>% calc(mean)
    harvest_st_bin2020_2030_sd <- stack(harvest_st_bin2020_2030_list) %>% calc(sd)
    harvest_st_bin2020_2030_mean[harvest_st_bin2020_2030_mean <= 0] = NA 
    harvest_st_bin2020_2030_sd[harvest_st_bin2020_2030_sd <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2020_2030_mean, file.path(output_dir, "nrb_20_30_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(nrb_bin2020_2030_sd, file.path(output_dir, "nrb_20_30_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2030_mean, file.path(output_dir, "harv_20_30_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2030_sd, file.path(output_dir, "harv_20_30_sd.tif"), format="GTiff", overwrite=TRUE)
    
    agb_2010_mean <- stack(agb_2010_list) %>% calc(mean)
    agb_2010_sd <- stack(agb_2010_list) %>% calc(sd)
    agb_2020_mean <- stack(agb_2020_list) %>% calc(mean)
    agb_2020_sd <- stack(agb_2020_list) %>% calc(sd)
    agb_2030_mean <- stack(agb_2030_list) %>% calc(mean)
    agb_2030_sd <- stack(agb_2030_list) %>% calc(sd)
    
    writeRaster(agb_2010_mean, file.path(output_dir, "agb_2010_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2010_sd, file.path(output_dir, "agb_2010_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2020_mean, file.path(output_dir, "agb_2020_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2020_sd, file.path(output_dir, "agb_2020_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2030_mean, file.path(output_dir, "agb_2030_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2030_sd, file.path(output_dir, "agb_2030_sd.tif"), format="GTiff", overwrite=TRUE)
    
  }
  
  if (STdyn == 25){
    # Calculate mean and standard deviation across all "debugging_n" folders
    nrb_bin2010_2020_mean <- stack(nrb_bin2010_2020_list) %>% calc(mean)
    nrb_bin2010_2020_sd <- stack(nrb_bin2010_2020_list) %>% calc(sd)
    harvest_st_bin2010_2020_mean <- stack(harvest_st_bin2010_2020_list) %>% calc(mean)
    harvest_st_bin2010_2020_sd <- stack(harvest_st_bin2010_2020_list) %>% calc(sd)
    harvest_st_bin2010_2020_mean[harvest_st_bin2010_2020_mean <= 0] = NA
    harvest_st_bin2010_2020_sd[harvest_st_bin2010_2020_sd <= 0] = NA
    
    # Save the resulting rasters
    writeRaster(nrb_bin2010_2020_mean, file.path(output_dir, "nrb_10_20_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(nrb_bin2010_2020_sd, file.path(output_dir, "nrb_10_20_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2010_2020_mean, file.path(output_dir, "harv_10_20_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2010_2020_sd, file.path(output_dir, "harv_10_20_sd.tif"), format="GTiff", overwrite=TRUE)
    
    # Calculate mean and standard deviation across all "debugging_n" folders
    nrb_bin2020_2035_mean <- stack(nrb_bin2020_2035_list) %>% calc(mean)
    nrb_bin2020_2035_sd <- stack(nrb_bin2020_2035_list) %>% calc(sd)
    harvest_st_bin2020_2035_mean <- stack(harvest_st_bin2020_2035_list) %>% calc(mean)
    harvest_st_bin2020_2035_sd <- stack(harvest_st_bin2020_2035_list) %>% calc(sd)
    harvest_st_bin2020_2035_mean[harvest_st_bin2020_2035_mean <= 0] = NA 
    harvest_st_bin2020_2035_sd[harvest_st_bin2020_2035_sd <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2020_2035_mean, file.path(output_dir, "nrb_20_35_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(nrb_bin2020_2035_sd, file.path(output_dir, "nrb_20_35_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2035_mean, file.path(output_dir, "harv_20_35_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2035_sd, file.path(output_dir, "harv_20_35_sd.tif"), format="GTiff", overwrite=TRUE)
    
    agb_2010_mean <- stack(agb_2010_list) %>% calc(mean)
    agb_2010_sd <- stack(agb_2010_list) %>% calc(sd)
    agb_2020_mean <- stack(agb_2020_list) %>% calc(mean)
    agb_2020_sd <- stack(agb_2020_list) %>% calc(sd)
    agb_2035_mean <- stack(agb_2035_list) %>% calc(mean)
    agb_2035_sd <- stack(agb_2035_list) %>% calc(sd)
    
    writeRaster(agb_2010_mean, file.path(output_dir, "agb_2010_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2010_sd, file.path(output_dir, "agb_2010_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2020_mean, file.path(output_dir, "agb_2020_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2020_sd, file.path(output_dir, "agb_2020_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2035_mean, file.path(output_dir, "agb_2035_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2035_sd, file.path(output_dir, "agb_2035_sd.tif"), format="GTiff", overwrite=TRUE)
    
  }
  
  if (STdyn == 30){
    # Calculate mean and standard deviation across all "debugging_n" folders
    nrb_bin2010_2020_mean <- stack(nrb_bin2010_2020_list) %>% calc(mean)
    nrb_bin2010_2020_sd <- stack(nrb_bin2010_2020_list) %>% calc(sd)
    harvest_st_bin2010_2020_mean <- stack(harvest_st_bin2010_2020_list) %>% calc(mean)
    harvest_st_bin2010_20200_sd <- stack(harvest_st_bin2010_2020_list) %>% calc(sd)
    harvest_st_bin2010_2020_mean[harvest_st_bin2010_2020_mean <= 0] = NA
    harvest_st_bin2010_2020_sd[harvest_st_bin2010_2020_sd <= 0] = NA
    
    # Save the resulting rasters
    writeRaster(nrb_bin2010_2020_mean, file.path(output_dir, "nrb_10_20_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(nrb_bin2010_2020_sd, file.path(output_dir, "nrb_10_20_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2010_2020_mean, file.path(output_dir, "harv_10_20_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2010_2020_sd, file.path(output_dir, "harv_10_20_sd.tif"), format="GTiff", overwrite=TRUE)
    
    # Calculate mean and standard deviation across all "debugging_n" folders
    nrb_bin2020_2030_mean <- stack(nrb_bin2020_2030_list) %>% calc(mean)
    nrb_bin2020_2030_sd <- stack(nrb_bin2020_2030_list) %>% calc(sd)
    harvest_st_bin2020_2030_mean <- stack(harvest_st_bin2020_2030_list) %>% calc(mean)
    harvest_st_bin2020_2030_sd <- stack(harvest_st_bin2020_2030_list) %>% calc(sd)
    harvest_st_bin2020_2030_mean[harvest_st_bin2020_2030_mean <= 0] = NA 
    harvest_st_bin2020_2030_sd[harvest_st_bin2020_2030_sd <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2020_2030_mean, file.path(output_dir, "nrb_20_30_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(nrb_bin2020_2030_sd, file.path(output_dir, "nrb_20_30_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2030_mean, file.path(output_dir, "harv_20_30_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2030_sd, file.path(output_dir, "harv_20_30_sd.tif"), format="GTiff", overwrite=TRUE)
    
    # Calculate mean and standard deviation across all "debugging_n" folders
    nrb_bin2030_2040_mean <- stack(nrb_bin2030_2040_list) %>% calc(mean)
    nrb_bin2030_2040_sd <- stack(nrb_bin2030_2040_list) %>% calc(sd)
    harvest_st_bin2030_2040_mean <- stack(harvest_st_bin2030_2040_list) %>% calc(mean)
    harvest_st_bin2030_2040_sd <- stack(harvest_st_bin2030_2040_list) %>% calc(sd)
    harvest_st_bin2030_2040_mean[harvest_st_bin2030_2040_mean <= 0] = NA 
    harvest_st_bin2030_2040_sd[harvest_st_bin2030_2040_sd <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2030_2040_mean, file.path(output_dir, "nrb_30_40_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(nrb_bin2030_2040_sd, file.path(output_dir, "nrb_30_40_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2030_2040_mean, file.path(output_dir, "harv_30_40_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2030_2040_sd, file.path(output_dir, "harv_30_40_sd.tif"), format="GTiff", overwrite=TRUE)
    
    agb_2010_mean <- stack(agb_2010_list) %>% calc(mean)
    agb_2010_sd <- stack(agb_2010_list) %>% calc(sd)
    agb_2020_mean <- stack(agb_2020_list) %>% calc(mean)
    agb_2020_sd <- stack(agb_2020_list) %>% calc(sd)
    agb_2030_mean <- stack(agb_2030_list) %>% calc(mean)
    agb_2030_sd <- stack(agb_2030_list) %>% calc(sd)
    agb_2040_mean <- stack(agb_2040_list) %>% calc(mean)
    agb_2040_sd <- stack(agb_2040_list) %>% calc(sd)
    
    writeRaster(agb_2010_mean, file.path(output_dir, "agb_2010_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2010_sd, file.path(output_dir, "agb_2010_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2020_mean, file.path(output_dir, "agb_2020_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2020_sd, file.path(output_dir, "agb_2020_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2030_mean, file.path(output_dir, "agb_2030_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2030_sd, file.path(output_dir, "agb_2030_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2040_mean, file.path(output_dir, "agb_2040_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2040_sd, file.path(output_dir, "agb_2040_sd.tif"), format="GTiff", overwrite=TRUE)
    
  }
  
  if (STdyn == 40){
    # Calculate mean and standard deviation across all "debugging_n" folders
    nrb_bin2010_2020_mean <- stack(nrb_bin2010_2020_list) %>% calc(mean)
    nrb_bin2010_2020_sd <- stack(nrb_bin2010_2020_list) %>% calc(sd)
    harvest_st_bin2010_2020_mean <- stack(harvest_st_bin2010_2020_list) %>% calc(mean)
    harvest_st_bin2010_2020_sd <- stack(harvest_st_bin2010_2020_list) %>% calc(sd)
    harvest_st_bin2010_2020_mean[harvest_st_bin2010_2020_mean <= 0] = NA
    harvest_st_bin2010_2020_sd[harvest_st_bin2010_2020_sd <= 0] = NA
    
    # Save the resulting rasters
    writeRaster(nrb_bin2010_2020_mean, file.path(output_dir, "nrb_10_20_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(nrb_bin2010_2020_sd, file.path(output_dir, "nrb_10_20_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2010_2020_mean, file.path(output_dir, "harv_10_20_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2010_2020_sd, file.path(output_dir, "harv_10_20_sd.tif"), format="GTiff", overwrite=TRUE)
    
    # Calculate mean and standard deviation across all "debugging_n" folders
    nrb_bin2020_2030_mean <- stack(nrb_bin2020_2030_list) %>% calc(mean)
    nrb_bin2020_2030_sd <- stack(nrb_bin2020_2030_list) %>% calc(sd)
    harvest_st_bin2020_2030_mean <- stack(harvest_st_bin2020_2030_list) %>% calc(mean)
    harvest_st_bin2020_2030_sd <- stack(harvest_st_bin2020_2030_list) %>% calc(sd)
    harvest_st_bin2020_2030_mean[harvest_st_bin2020_2030_mean <= 0] = NA 
    harvest_st_bin2020_2030_sd[harvest_st_bin2020_2030_sd <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2020_2030_mean, file.path(output_dir, "nrb_20_30_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(nrb_bin2020_2030_sd, file.path(output_dir, "nrb_20_30_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2030_mean, file.path(output_dir, "harv_20_30_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2030_sd, file.path(output_dir, "harv_20_30_sd.tif"), format="GTiff", overwrite=TRUE)
    
    # Calculate mean and standard deviation across all "debugging_n" folders
    nrb_bin2020_2035_mean <- stack(nrb_bin2020_2035_list) %>% calc(mean)
    nrb_bin2020_2035_sd <- stack(nrb_bin2020_2035_list) %>% calc(sd)
    harvest_st_bin2020_2035_mean <- stack(harvest_st_bin2020_2035_list) %>% calc(mean)
    harvest_st_bin2020_2035_sd <- stack(harvest_st_bin2020_2035_list) %>% calc(sd)
    harvest_st_bin2020_2035_mean[harvest_st_bin2020_2035_mean <= 0] = NA 
    harvest_st_bin2020_2035_sd[harvest_st_bin2020_2035_sd <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2020_2035_mean, file.path(output_dir, "nrb_20_35_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(nrb_bin2020_2035_sd, file.path(output_dir, "nrb_20_35_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2035_mean, file.path(output_dir, "harv_20_35_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2035_sd, file.path(output_dir, "harv_20_35_sd.tif"), format="GTiff", overwrite=TRUE)
    
    # Calculate mean and standard deviation across all "debugging_n" folders
    nrb_bin2020_2050_mean <- stack(nrb_bin2020_2050_list) %>% calc(mean)
    nrb_bin2020_2050_sd <- stack(nrb_bin2020_2050_list) %>% calc(sd)
    harvest_st_bin2020_2050_mean <- stack(harvest_st_bin2020_2050_list) %>% calc(mean)
    harvest_st_bin2020_2050_sd <- stack(harvest_st_bin2020_2050_list) %>% calc(sd)
    harvest_st_bin2020_2050_mean[harvest_st_bin2020_2050_mean <= 0] = NA 
    harvest_st_bin2020_2050_sd[harvest_st_bin2020_2050_sd <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2020_2050_mean, file.path(output_dir, "nrb_20_50_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(nrb_bin2020_2050_sd, file.path(output_dir, "nrb_20_50_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2050_mean, file.path(output_dir, "harv_20_50_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2020_2050_sd, file.path(output_dir, "harv_20_50_sd.tif"), format="GTiff", overwrite=TRUE)
    
    # Calculate mean and standard deviation across all "debugging_n" folders
    nrb_bin2030_2040_mean <- stack(nrb_bin2030_2040_list) %>% calc(mean)
    nrb_bin2030_2040_sd <- stack(nrb_bin2030_2040_list) %>% calc(sd)
    harvest_st_bin2030_2040_mean <- stack(harvest_st_bin2030_2040_list) %>% calc(mean)
    harvest_st_bin2030_2040_sd <- stack(harvest_st_bin2030_2040_list) %>% calc(sd)
    harvest_st_bin2030_2040_mean[harvest_st_bin2030_2040_mean <= 0] = NA 
    harvest_st_bin2030_2040_sd[harvest_st_bin2030_2040_sd <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2030_2040_mean, file.path(output_dir, "nrb_30_40_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(nrb_bin2030_2040_sd, file.path(output_dir, "nrb_30_40_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2030_2040_mean, file.path(output_dir, "harv_30_40_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2030_2040_sd, file.path(output_dir, "harv_30_40_sd.tif"), format="GTiff", overwrite=TRUE)
    
    # Calculate mean and standard deviation across all "debugging_n" folders
    nrb_bin2040_2050_mean <- stack(nrb_bin2040_2050_list) %>% calc(mean)
    nrb_bin2040_2050_sd <- stack(nrb_bin2040_2050_list) %>% calc(sd)
    harvest_st_bin2040_2050_mean <- stack(harvest_st_bin2040_2050_list) %>% calc(mean)
    harvest_st_bin2040_2050_sd <- stack(harvest_st_bin2040_2050_list) %>% calc(sd)
    harvest_st_bin2040_2050_mean[harvest_st_bin2040_2050_mean <= 0] = NA 
    harvest_st_bin2040_2050_sd[harvest_st_bin2040_2050_sd <= 0] = NA 
    
    # Save the resulting rasters
    writeRaster(nrb_bin2040_2050_mean, file.path(output_dir, "nrb_40_50_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(nrb_bin2040_2050_sd, file.path(output_dir, "nrb_40_50_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2040_2050_mean, file.path(output_dir, "harv_40_50_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(harvest_st_bin2040_2050_sd, file.path(output_dir, "harv_40_50_sd.tif"), format="GTiff", overwrite=TRUE)
    
    agb_2010_mean <- stack(agb_2010_list) %>% calc(mean)
    agb_2010_sd <- stack(agb_2010_list) %>% calc(sd)
    agb_2020_mean <- stack(agb_2020_list) %>% calc(mean)
    agb_2020_sd <- stack(agb_2020_list) %>% calc(sd)
    agb_2030_mean <- stack(agb_2030_list) %>% calc(mean)
    agb_2030_sd <- stack(agb_2030_list) %>% calc(sd)
    agb_2035_mean <- stack(agb_2035_list) %>% calc(mean)
    agb_2035_sd <- stack(agb_2035_list) %>% calc(sd)
    agb_2040_mean <- stack(agb_2040_list) %>% calc(mean)
    agb_2040_sd <- stack(agb_2040_list) %>% calc(sd)
    agb_2050_mean <- stack(agb_2050_list) %>% calc(mean)
    agb_2050_sd <- stack(agb_2050_list) %>% calc(sd)
    
    writeRaster(agb_2010_mean, file.path(output_dir, "agb_2010_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2010_sd, file.path(output_dir, "agb_2010_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2020_mean, file.path(output_dir, "agb_2020_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2020_sd, file.path(output_dir, "agb_2020_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2030_mean, file.path(output_dir, "agb_2030_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2030_sd, file.path(output_dir, "agb_2030_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2035_mean, file.path(output_dir, "agb_2035_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2035_sd, file.path(output_dir, "agb_2035_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2040_mean, file.path(output_dir, "agb_2040_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2040_sd, file.path(output_dir, "agb_2040_sd.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2050_mean, file.path(output_dir, "agb_2050_mean.tif"), format="GTiff", overwrite=TRUE)
    writeRaster(agb_2050_sd, file.path(output_dir, "agb_2050_sd.tif"), format="GTiff", overwrite=TRUE)
    
  }

}

toc()

