# MoFuSS
# Version 4
# Date: Jan 2025

# 2dolist

# Internal parameters
start_from_scratch <- 0 # Set to 0 when the MoFuSS working directory already exists and has data in it
# rm(list=ls(all=TRUE))
# Select MoFuSS platform:
webmofuss = 1 # "1" is  web-MoFuSS running in our Ubuntu server, "0" is localcal host (Windows or Linux)

# Load libraries ----
library(stringr)
library(purrr)
library(tcltk)
library(dplyr)
library(readxl)

# Detect OS
os <- Sys.info()["sysname"]

if (webmofuss == 1) {
  
  # 1km
  gitlabdir <- "C:/Users/aghil/Documents/mofuss"
  countrydir <- "D:/malawi_1000m_bau" # RICARDO: AJUSTA ACA
  country <- "C:\\Users\\aghil\\Documents\\mofuss\\countries\\Global.tif"
  demanddir <- "D:/demand"
  admindir <- "D:/admin_regions"
  emissionsdir <- "D:/emissions"
  rTempdir <- "D:/rTemp"
  parameters_file_path <- "D:/malawi_1000m_bau/LULCC/DownloadedDatasets/SourceDataGlobal/parameters_world1000m.xlsx" # RICARDO: AJUSTA ACA
  
  # Extract the country name
  country_name <- gsub(pattern = "(.*countries[\\])(.*)(.tif.*)", 
                       replacement = "\\2",
                       country)
  
  # geedir <- "G:/Mi unidad/webpages/2024_MoFuSSGlobal_Datasets/GEE2MoFuSS"
  # demdir <- "E:/DTEM"
  # gripdir <- "E:/GRIP"
  # hydrodir <- "E:/HydroSHEDS"
  # borderdir <- "E:/borders"
  # lulccfiles <- "E:/lulcc"
  
  print("Input datasets already in place")
  
  # Rename SourceData main directory ----
  base_path <- "LULCC/DownloadedDatasets"
  # List all directories within the base path
  all_dirs <- list.dirs(path = base_path, full.names = TRUE, recursive = FALSE)
  
  # Identify directories that match the pattern "SourceData*"
  pattern <- "SourceData.*"
  source_dirs <- grep(pattern, all_dirs, value = TRUE)
  
  # Loop through the identified directories and rename them
  for (dir in source_dirs) {
    new_name <- paste0(base_path, "/SourceData", country_name)
    if (file.rename(dir, new_name)) {
      cat("Directory renamed successfully: ", dir, " to ", new_name, "\n")
    } else {
      cat("Failed to rename directory: ", dir, "\n")
    }
  }
  
  
  # Prompt the user to select the "parameters.xlsx" file
  # parameters_file_path <- tk_choose.files(default = "", caption = "Select the 'parameters.xlsx' file", 
  #                                         multi = FALSE, filters = matrix(c("Excel Files", "*.xlsx"), 1, 2))
  parameters_file <- basename(parameters_file_path)
  
  # Check if the user selected a file
  if (nchar(parameters_file_path) > 0 && basename(parameters_file_path) == parameters_file) {
    # Define the destination directory
    destination_dir <- paste0(countrydir, "/LULCC/DownloadedDatasets/SourceData",country_name,"/",parameters_file)
    
    if (identical(destination_dir,parameters_file_path) == TRUE) {
      message("File already in place within MoFuSS working directory")
    } else {
      # Copy the file to the destination directory
      file.copy(from = parameters_file_path, to = destination_dir, overwrite = TRUE)
    }
    
    message("File successfully copied (or read) to (from): ", destination_dir)
  } else {
    message("No valid file selected.")
  }
  
  # Clean MoFuSS working folder ----
  unlink(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables"), recursive= TRUE, force=TRUE)
  unlink(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector"), recursive= TRUE, force=TRUE)
  unlink(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector_GCS"), recursive= TRUE, force=TRUE)
  
  if (!dir.exists(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables"))) {
    dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables"))
  }
  
  if (!dir.exists(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector"))) {
    dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector"))
  }
  
  if (!dir.exists(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector_GCS"))) {
    dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector_GCS"))
  }
  
  # Copy input tables from gitlab repo into MoFuSS working folder ----
  friction2copy <- list.files(
    path = paste0(gitlabdir, "/friction"),
    pattern = "\\.csv$|\\.xlsx$",
    full.names = TRUE)
  
  for (f in friction2copy) {
    file.copy(from=f, 
              to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables/"), 
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  }
  
  # Copy input tables from gitlab repo into MoFuSS working folder ----
  growth2copy <- list.files(path = paste0(gitlabdir, "/global_growth"), 
                            pattern = "\\.csv$|\\.xlsx$", 
                            full.names = TRUE)
  
  # Read the Excel file
  country_parameters <- read_excel(destination_dir)
  print(tibble::as_tibble(country_parameters), n=100)
  
  country_parameters %>%
    dplyr::filter(Var == "GEE_tyRoi") %>%
    pull(ParCHR) -> GEE_tyRoi
  if (GEE_tyRoi == "world") {
    print("Global growth parameters tables copied succesfully")
  } else if (GEE_tyRoi != "world") {
    # Exclude the specific files
    growth2copy <- growth2copy[!basename(growth2copy) %in% c("growth_parameters_v3_copernicus.csv", "growth_parameters_v3_modis.csv")]
  }
  
  for (g in growth2copy) {
    file.copy(from=g, 
              to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables/"), 
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  }
  
  admin2copyv2 <- list.files(path = paste0(gitlabdir, "/admin_regions"), 
                             pattern = "\\.csv$|\\.xlsx$", 
                             full.names = TRUE)
  for (t in admin2copyv2) {
    file.copy(from=t, 
              to=paste0(admindir,"/"), 
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  }
  
} else {
  
  # Function to choose directory
  choose_gitlab_dir <- function() {
    if (os == "Windows") {
      # Use tk_choose.dir for Windows
      gitlabdir <- tcltk::tk_choose.dir(default = normalizePath("~"), caption = "Choose your local GitLab MoFuSS directory")
    } else if (os == "Linux") {
      # Use an alternative for Linux (e.g., `utils::choose.dir`, which works in RStudio)
      gitlabdir <- rstudioapi::selectDirectory(caption = "Choose your local GitLab MoFuSS directory")
    } else {
      stop("Unsupported OS. Directory selection is not implemented for this system.")
    }
    
    # Check if a directory was selected
    if (is.null(gitlabdir) || gitlabdir == "") {
      stop("No directory selected. Exiting.")
    }
    
    # Normalize path and return
    return(normalizePath(gitlabdir))
  }
  
  # Call the function
  gitlabdir <- choose_gitlab_dir()
  
  # Save the selected directory as gitlabdir
  cat("Selected GitLab directory:", gitlabdir, "\n")
  
  # Rebuild this chunk
  if(.Platform$OS.type == "unix")  {
    country<-tk_choose.files(default=paste0(gitlabdir,"/countries/*.*"))
    country[2]
    res <- str_match(country[2], "countries/(.*?).tif")
    country_name <- res[,2]
  } else {
    country<-choose.files(default=paste0(gitlabdir,"/countries/*.*"))
    country
    res <- str_match(country, "countries\\\\s*(.*?)\\s*.tif")
    res[,2]
    # Extract the country name
    country_name <- gsub(pattern = "(.*countries[\\])(.*)(.tif.*)", 
                         replacement = "\\2",
                         country)
  }
  
  if (start_from_scratch == 1) {
    # Prompt the user to select the "parameters.xlsx" file
    parameters_file_path <- tk_choose.files(default = "", caption = "Select the 'parameters.xlsx' file", 
                                            multi = FALSE, filters = matrix(c("Excel Files", "*.xlsx"), 1, 2))
    parameters_file <- basename(parameters_file_path)
    
    # Read parameters table ----
    country_parameters_prelim <- read_excel(parameters_file_path) 
    print(tibble::as_tibble(country_parameters_prelim), n=100)
    
    choose_directory66 = function(caption = "Choose the location where to create your MoFuSS working folder") {
      if(.Platform$OS.type == "unix")  {
        setwd(tk_choose.dir("/", caption = caption))
      } else {
        setwd(choose.dir("/", caption = caption))
      }
    }
    choose_directory66()
    countrydir_prelim <- getwd()
    
    country_parameters_prelim %>%
      dplyr::filter(Var == "GEE_tyRoi") %>%
      pull(ParCHR) -> GEE_tyRoi
    
    country_parameters_prelim %>%
      dplyr::filter(Var == "GEE_country") %>%
      pull(ParCHR) -> GEE_country
    
    if (GEE_tyRoi == "world" | GEE_tyRoi == "regions") {
      regionname <- GEE_tyRoi
    } else {
      regionname <- GEE_country
    }
    
    country_parameters_prelim %>%
      dplyr::filter(Var == "GEE_scale") %>%
      pull(ParCHR) -> GEE_scale
    
    # Get the current date
    current_date <- format(Sys.Date(), "%Y%m%d")  # Format as YYYYMMDD
    
    # Create the string with the current date
    countrydir <- paste0(countrydir_prelim, regionname, "_", GEE_scale, "m_", current_date)
    
    # Print the final string
    print(countrydir)
    
    dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name),recursive = TRUE, showWarnings = FALSE)
    
    # Check if the user selected a file
    if (nchar(parameters_file_path) > 0 && basename(parameters_file_path) == parameters_file) {
      # Define the destination directory
      destination_dir <- paste0(countrydir, "/LULCC/DownloadedDatasets/SourceData",country_name,"/",parameters_file)
      
      # Copy the file to the destination directory
      file.copy(from = parameters_file_path, to = destination_dir, overwrite = TRUE)
      
      message("File successfully copied to: ", destination_dir)
    } else {
      message("No valid file selected.")
    }
    
  } else {
    
    choose_directory2 = function(caption = "Choose your MoFuSS working folder") {
      if(.Platform$OS.type == "unix")  {
        setwd(tk_choose.dir("/", caption = caption))
      } else {
        setwd(choose.dir("/", caption = caption))
      }
    }
    choose_directory2()
    countrydir <- getwd()
    
  }
  
  # endpath <- str_match(countrydir, "^.+MoFuSS_(.*)")[,2]
  # endpath
  
  choose_directory3 = function(caption = "Choose the directory where demand_in files are") {
    if(.Platform$OS.type == "unix")  {
      setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
    } else {
      setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
    }
  }
  choose_directory3()
  demanddir <- getwd()
  
  
  choose_directory4 = function(caption = "Choose the directory where admin_regions files are") {
    if(.Platform$OS.type == "unix")  {
      setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
    } else {
      setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
    }
  }
  choose_directory4()
  admindir <- getwd()
  
  
  choose_directory5 = function(caption = "Choose the directory where emissions outputs will be saved") {
    if(.Platform$OS.type == "unix")  {
      setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
    } else {
      setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
    }
  }
  choose_directory5()
  emissionsdir <- getwd()
  
  choose_directory6 = function(caption = "Choose a folder to store temporal files"){
    if(.Platform$OS.type == "unix")  {
      setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
    } else {
      setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
    }
  }
  choose_directory6()
  rTempdir <- getwd()
  
  
  setwd(countrydir)
  
  # Start from scratch or after all layers are in place ----
  if (start_from_scratch == 1) {
    
    dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables"),recursive = TRUE, showWarnings = FALSE)
    dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster"))
    dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster_GCS"))
    dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector"))
    dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector_GCS"))
    
  } else {
    
    print("Input datasets already in place")
    
    # Rename SourceData main directory ----
    base_path <- "LULCC/DownloadedDatasets"
    # List all directories within the base path
    all_dirs <- list.dirs(path = base_path, full.names = TRUE, recursive = FALSE)
    
    # Identify directories that match the pattern "SourceData*"
    pattern <- "SourceData.*"
    source_dirs <- grep(pattern, all_dirs, value = TRUE)
    
    # Loop through the identified directories and rename them
    for (dir in source_dirs) {
      new_name <- paste0(base_path, "/SourceData", country_name)
      if (file.rename(dir, new_name)) {
        cat("Directory renamed successfully: ", dir, " to ", new_name, "\n")
      } else {
        cat("Failed to rename directory: ", dir, "\n")
      }
    }
    
    
    # Prompt the user to select the "parameters.xlsx" file
    parameters_file_path <- tk_choose.files(default = "", caption = "Select the 'parameters.xlsx' file", 
                                            multi = FALSE, filters = matrix(c("Excel Files", "*.xlsx"), 1, 2))
    parameters_file <- basename(parameters_file_path)
    
    # Check if the user selected a file
    if (nchar(parameters_file_path) > 0 && basename(parameters_file_path) == parameters_file) {
      # Define the destination directory
      destination_dir <- paste0(countrydir, "/LULCC/DownloadedDatasets/SourceData",country_name,"/",parameters_file)
      
      if (identical(destination_dir,parameters_file_path) == TRUE) {
        message("File already in place within MoFuSS working directory")
      } else {
        # Copy the file to the destination directory
        file.copy(from = parameters_file_path, to = destination_dir, overwrite = TRUE)
      }
      
      message("File successfully copied (or read) to (from): ", destination_dir)
    } else {
      message("No valid file selected.")
    }
    
  }
  
  # Clean MoFuSS working folder ----
  unlink(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables"), recursive= TRUE, force=TRUE)
  unlink(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector"), recursive= TRUE, force=TRUE)
  unlink(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector_GCS"), recursive= TRUE, force=TRUE)
  
  if (!dir.exists(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables"))) {
    dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables"))
  }
  
  if (!dir.exists(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector"))) {
    dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector"))
  }
  
  if (!dir.exists(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector_GCS"))) {
    dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector_GCS"))
  }
  
  # Copy input tables from gitlab repo into MoFuSS working folder ----
  friction2copy <- list.files(
    path = paste0(gitlabdir, "/friction"),
    pattern = "\\.csv$|\\.xlsx$",
    full.names = TRUE)
  
  for (f in friction2copy) {
    file.copy(from=f, 
              to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables/"), 
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  }
  
  # Copy input tables from gitlab repo into MoFuSS working folder ----
  growth2copy <- list.files(path = paste0(gitlabdir, "/global_growth"), 
                            pattern = "\\.csv$|\\.xlsx$", 
                            full.names = TRUE)
  
  # Read the Excel file
  country_parameters <- read_excel(destination_dir)
  print(tibble::as_tibble(country_parameters), n=100)
  
  country_parameters %>%
    dplyr::filter(Var == "GEE_tyRoi") %>%
    pull(ParCHR) -> GEE_tyRoi
  if (GEE_tyRoi == "world") {
    print("Global growth parameters tables copied succesfully")
  } else if (GEE_tyRoi != "world") {
    # Exclude the specific files
    growth2copy <- growth2copy[!basename(growth2copy) %in% c("growth_parameters_v3_copernicus.csv", "growth_parameters_v3_modis.csv")]
  }
  
  for (g in growth2copy) {
    file.copy(from=g, 
              to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables/"), 
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  }
  
  admin2copyv2 <- list.files(path = paste0(gitlabdir, "/admin_regions"), 
                             pattern = "\\.csv$|\\.xlsx$", 
                             full.names = TRUE)
  for (t in admin2copyv2) {
    file.copy(from=t, 
              to=paste0(admindir,"/"), 
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  }
  
}
