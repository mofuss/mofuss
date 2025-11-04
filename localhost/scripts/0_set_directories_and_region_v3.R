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
# Version 4
# Date: Jan 2025

# 2dolist ----
# Create the string with the current date, checkk line 300
# Line 604 # demanddir <- file.path(countrydir,"/LULCC/DownloadedDatasets/SourceDataGlobal/demand") #Why was this here, under what situation demanddir is NOT created??

# Internal parameters ----
start_from_scratch <- 0 # Set to 0 when the MoFuSS working directory already exists and has data in it
# rm(list=ls(all=TRUE))
# # Select MoFuSS platform:
# webmofuss = 1 # "1" is  web-MoFuSS running in our Ubuntu server, "0" is localcal host (Windows or Linux)
if (start_from_scratch == 1){webmofuss = 0}
# source(paste0(scriptsmofuss,"00_webmofuss.R"))

# Load libraries ----
library(dplyr)
library(fs)
library(purrr)
library(readxl)
library(stringr)
library(tcltk)

# Detect OS
os <- Sys.info()["sysname"]

# webmofuss = 1 for debugging outside webmofuss server as if it was webmofuss server - KEEP COMMENTING
if (webmofuss == 1) {

  # # from .env for debugging outside webmofuss server as if it was webmofuss server - KEEP COMMENTING
  # getwd()
  # githubdir="/home/yayo/mofuss"
  # # scriptsmofuss=/var/www/html/mofuss/tools/uploads/68e885fec1f77/scripts/
  # countrydir="/home/yayo/Documents/webmofuss"
  # # demanddir=/var/www/html/mofuss/tools/uploads/68e885fec1f77/demand/
  # admindir="/home/yayo/Documents/admin_regions"
  # # emissionsdir=/var/www/html/mofuss/tools/uploads/68e885fec1f77/emissions/
  # # rTempdir=/var/www/html/mofuss/tools/uploads/68e885fec1f77/rTemp/
  # parameters_file_path="/home/yayo/Documents/webmofuss/LULCC/DownloadedDatasets/SourceDataGlobal/parameters.csv"
  # parameters_file="parameters.csv"
  # getwd()
  
  # Read parameters table ----
  country_parameters <- read_csv(parameters_file_path)
  #print(tibble::as_tibble(country_parameters), n=100)
  
  country_parameters %>%
    dplyr::filter(Var == "byregion") %>%
    pull(ParCHR) -> byregion
  
  country_parameters %>%
    dplyr::filter(Var == "region2BprocessedCtry") %>%
    pull(ParCHR) -> region2BprocessedCtry
  
  country_parameters %>%
    dplyr::filter(Var == "GEE_scale") %>%
    pull(ParCHR) -> GEE_scale
  
  if (byregion == "Country" & GEE_scale == 100) {
    country_name <- region2BprocessedCtry
  } else if (byregion %in% c("Global", "Continental", "Regional", "Country") & GEE_scale == 1000) {
    country_name <- "Global"
  } else {
    cat("Error in AoI selection and/or resolution \n")
  }

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
  
  # Copy input tables from github repo into MoFuSS working folder ----
  friction2copy <- list.files(
    path = paste0(githubdir, "/friction"),
    pattern = "\\.csv$|\\.xlsx$",
    full.names = TRUE)
  
  for (f in friction2copy) {
    file.copy(from=f, 
              to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables/"), 
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  }
  
  # Copy input tables from github repo into MoFuSS working folder ----
  growth2copy <- list.files(path = paste0(githubdir, "/global_growth"), 
                            pattern = "\\.csv$|\\.xlsx$", 
                            full.names = TRUE)
  
  # Read parameters table (recognizing the delimiter) ----
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
  
  country_parameters %>%
    dplyr::filter(Var == "GEE_scale") %>%
    pull(ParCHR) -> GEE_scale
  if (GEE_scale == 1000) {
    print("Global growth parameters tables copied succesfully")
  } else if (GEE_scale != 1000) {
    # Exclude the specific files
    growth2copy <- growth2copy[!basename(growth2copy) %in% c("growth_parameters_v3_copernicus.csv", "growth_parameters_v3_modis.csv")]
  }
  
  for (g in growth2copy) {
    file.copy(from=g, 
              to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables/"), 
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  }
  
  admin2copyv2 <- list.files(path = paste0(githubdir, "/admin_regions"), 
                             pattern = "\\.csv$|\\.xlsx$", 
                             full.names = TRUE)
  for (t in admin2copyv2) {
    file.copy(from=t, 
              to=paste0(admindir,"/"), 
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  }
  
} else {
  
  # Function to choose directory
  choose_github_dir <- function() {
    if (os == "Windows") {
      # Use tk_choose.dir for Windows
      githubdir <- tcltk::tk_choose.dir(default = normalizePath("~"), caption = "Choose your local GitHub MoFuSS directory")
    } else if (os == "Linux") {
      # Use an alternative for Linux (e.g., `utils::choose.dir`, which works in RStudio)
      githubdir <- rstudioapi::selectDirectory(caption = "Choose your local github MoFuSS directory")
    } else {
      stop("Unsupported OS. Directory selection is not implemented for this system.")
    }
    
    # Check if a directory was selected
    if (is.null(githubdir) || githubdir == "") {
      stop("No directory selected. Exiting.")
    }
    
    # Normalize path and return
    return(normalizePath(githubdir))
  }
  
  # Call the function
  githubdir <- choose_github_dir()
  
  # Save the selected directory as githubdir
  cat("Selected github directory:", githubdir, "\n")
  
  if (start_from_scratch == 1) {
    # Prompt the user to select the "parameters" file
    parameters_file_path <- tk_choose.files(default = "", caption = "Select the 'parameters' file", 
                                            multi = FALSE, filters = matrix(c("CSV Files", "*.csv"), 1, 2))
    parameters_file <- basename(parameters_file_path)
    
    # Read parameters table (recognizing the delimiter) ----
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
    
    choose_directory66 = function(caption = "Choose the location where to create your MoFuSS working folder") {
      if(.Platform$OS.type == "unix")  {
        setwd(tk_choose.dir("/", caption = caption))
      } else {
        setwd(choose.dir("/", caption = caption))
      }
    }
    choose_directory66()
    countrydir_prelim <- getwd()
    
    country_parameters %>%
      dplyr::filter(Var == "GEE_tyRoi") %>%
      pull(ParCHR) -> GEE_tyRoi
    
    country_parameters %>%
      dplyr::filter(Var == "GEE_country") %>%
      pull(ParCHR) -> GEE_country
    
    if (GEE_tyRoi == "world" | GEE_tyRoi == "regions") {
      regionname <- GEE_tyRoi
    } else {
      regionname <- GEE_country
    }
    
    country_parameters %>%
      dplyr::filter(Var == "GEE_scale") %>%
      pull(ParCHR) -> GEE_scale
    
    country_parameters %>%
      dplyr::filter(Var == "byregion") %>%
      pull(ParCHR) -> byregion
    
    country_parameters %>%
      dplyr::filter(Var == "region2BprocessedCtry") %>%
      pull(ParCHR) -> region2BprocessedCtry
    
    if (byregion == "Country" & GEE_scale == 100) {
      country_name <- region2BprocessedCtry
    } else if (byregion %in% c("Global", "Continental", "Regional", "Country") & GEE_scale == 1000) {
      country_name <- "Global"
    } else {
      cat("Error in AoI selection and/or resolution \n")
    }
    
    # Get the current date
    current_date <- format(Sys.Date(), "%Y%m%d")  # Format as YYYYMMDD
    
    # # Create the string with the current date
    # if (os == "Linux") {
    #   countrydir <- paste0(countrydir_prelim, "/", regionname, "_", GEE_scale, "m_", current_date)
    # } else if (os == "Windows") {
    #   countrydir <- paste0(countrydir_prelim, "/", regionname, "_", GEE_scale, "m_", current_date)
    # }
    if (os == "Linux") {
      countrydir <- paste0(countrydir_prelim, regionname, "_", GEE_scale, "m_", current_date)
    } else if (os == "Windows") {
      countrydir <- paste0(countrydir_prelim, regionname, "_", GEE_scale, "m_", current_date)
    }
    
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
  getwd()
    # New version now has demand folder within the MoFuSS working directory 
    if (GEE_scale == 100) {
      dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/demand100m"),recursive = TRUE, showWarnings = FALSE)
      demanddir <- paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/demand100m")
    } else {
      dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/demand"),recursive = TRUE, showWarnings = FALSE)
      demanddir <- paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/demand")
      }

    print(demanddir)
    
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
  
  # choose_directory3 = function(caption = "Choose the directory where demand_in files are") {
  #   if(.Platform$OS.type == "unix")  {
  #     setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
  #   } else {
  #     setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
  #   }
  # }
  # choose_directory3()
  #demanddir <- getwd()
  
  
  if  (start_from_scratch != 1) {
    # New version now has demand folder within the MoFuSS working directory 
    # Base path
    base_path <- file.path(countrydir, "LULCC/DownloadedDatasets")
    # 1. Find any directory starting with "SourceData"
    source_dir <- dir_ls(base_path, type = "directory", regexp = "SourceData.*$")
    # 2. Inside that, list only immediate subfolders
    subdirs <- dir_ls(source_dir, type = "directory", recurse = FALSE)
    # keep those whose *basename* starts with "demand"
    cand <- subdirs[startsWith(tolower(path_file(subdirs)), "demand")]
    if (length(cand) == 0) {
      stop("No folder starting with 'demand' found in: ", source_dir)
    }
    # 3. Assign to demanddir
    demanddir <- cand
  }
  print(demanddir)
  
  choose_directory4 = function(caption = "Choose the directory where admin_regions files are") {
    if(.Platform$OS.type == "unix")  {
      setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
    } else {
      setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
    }
  }
  choose_directory4()
  admindir <- getwd()
  
  # choose_directory5 = function(caption = "Choose the directory where emissions outputs will be saved") {
  #   if(.Platform$OS.type == "unix")  {
  #     setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
  #   } else {
  #     setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
  #   }
  # }
  # choose_directory5()
  # emissionsdir <- getwd()
  
  # choose_directory6 = function(caption = "Choose a folder to store temporal files"){
  #   if(.Platform$OS.type == "unix")  {
  #     setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
  #   } else {
  #     setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
  #   }
  # }
  # choose_directory6()
  # unlink(paste0(countrydir,"/rTemp"), recursive = TRUE, force = TRUE) 
  # dir.create(paste0(countrydir,"/rTemp"))
  # rTempdir <- paste0(countrydir,"/rTemp")
  
  setwd(countrydir)
  
  # Start from scratch or after all layers are in place ----
  if (start_from_scratch == 1) {
    
    dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables"),recursive = TRUE, showWarnings = FALSE)
    dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster"))
    dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster_GCS"))
    dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector"))
    dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector_GCS"))
    
  } else {
    
    # Prompt the user to select the "parameters" file
    parameters_file_path <- tk_choose.files(default = "", caption = "Select the 'parameters' file", 
                                            multi = FALSE, filters = matrix(c("CSV Files", "*.csv"), 1, 2))
    parameters_file <- basename(parameters_file_path)
    
    # Read parameters table (recognizing the delimiter) ----
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
    
    country_parameters %>%
      dplyr::filter(Var == "byregion") %>%
      pull(ParCHR) -> byregion
    
    country_parameters %>%
      dplyr::filter(Var == "region2BprocessedCtry") %>%
      pull(ParCHR) -> region2BprocessedCtry
    
    country_parameters %>%
      dplyr::filter(Var == "GEE_scale") %>%
      pull(ParCHR) -> GEE_scale
    
    if (byregion == "Country" & GEE_scale == 100) {
      country_name <- region2BprocessedCtry
    } else if (byregion %in% c("Global", "Continental", "Regional", "Country") & GEE_scale == 1000) {
      country_name <- "Global"
    } else {
      cat("Error in AoI selection and/or resolution \n")
    }
    
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
  
  # # Clean MoFuSS working folder ----
  # unlink(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables"), recursive= TRUE, force=TRUE)
  # unlink(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector"), recursive= TRUE, force=TRUE)
  # # unlink(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector_GCS"), recursive= TRUE, force=TRUE)
  # 
  # if (!dir.exists(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables"))) {
  #   dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables"))
  # }
  # 
  # if (!dir.exists(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector"))) {
  #   dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector"))
  # }
  # 
  # # if (!dir.exists(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector_GCS"))) {
  # #   dir.create(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InVector_GCS"))
  # # }
  # 
  # # Copy input tables from github repo into MoFuSS working folder ----
  # friction2copy <- list.files(
  #   path = paste0(githubdir, "/friction"),
  #   pattern = "\\.csv$|\\.xlsx$",
  #   full.names = TRUE)
  # 
  # for (f in friction2copy) {
  #   file.copy(from=f, 
  #             to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables/"), 
  #             overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  # }
  # 
  # demandtables2copy <- list.files(
  #   path = paste0(githubdir, "/demand_tables"),
  #   pattern = "\\.csv$|\\.xlsx$",
  #   full.names = TRUE)
  # 
  # dir.create(paste0(demanddir,"/demand_in"), recursive =TRUE)
  # for (dem in demandtables2copy) {
  #   file.copy(from=dem, 
  #             
  #             to=paste0(demanddir,"/demand_in"), 
  #             overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  # }
  # 
  # # Copy input tables from github repo into MoFuSS working folder ----
  # growth2copy <- list.files(path = paste0(githubdir, "/global_growth"), 
  #                           pattern = "\\.csv$|\\.xlsx$", 
  #                           full.names = TRUE)
  # 
  # # Read parameters table (recognizing the delimiter) ----
  # detect_delimiter <- function(file_path) {
  #   # Read the first line of the file
  #   first_line <- readLines(file_path, n = 1)
  #   # Check if the first line contains ',' or ';'
  #   if (grepl(";", first_line)) {
  #     return(";")
  #   } else {
  #     return(",")
  #   }
  # }
  # # Detect the delimiter
  # delimiter <- detect_delimiter(parameters_file_path)
  # # Read the CSV file with the detected delimiter
  # country_parameters <- read_delim(parameters_file_path, delim = delimiter)
  # print(tibble::as_tibble(country_parameters), n=100)
  # 
  # country_parameters %>%
  #   dplyr::filter(Var == "GEE_scale") %>%
  #   pull(ParCHR) -> GEE_scale
  # if (GEE_scale == 1000) {
  #   print("Global growth parameters tables copied succesfully")
  # } else if (GEE_scale != 1000) {
  #   # Exclude the specific files
  #   growth2copy <- growth2copy[!basename(growth2copy) %in% c("growth_parameters_v3_copernicus.csv", "growth_parameters_v3_modis.csv")]
  # }
  # 
  # for (g in growth2copy) {
  #   file.copy(from=g, 
  #             to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables/"), 
  #             overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  # }
  # 
  # admin2copyv2 <- list.files(path = paste0(githubdir, "/admin_regions"), 
  #                            pattern = "\\.csv$|\\.xlsx$", 
  #                            full.names = TRUE)
  # for (t in admin2copyv2) {
  #   file.copy(from=t, 
  #             to=paste0(admindir,"/"), 
  #             overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  # }
  
}


# Copy admin csv ----
admin2copyv2 <- list.files(path = paste0(githubdir, "/admin_regions"),
                           pattern = "\\.csv$|\\.xlsx$",
                           full.names = TRUE)
for (t in admin2copyv2) {
  file.copy(from=t,
            to=paste0(admindir,"/"),
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

# Define Terra temporal directory ----
rTempdir <- file.path(countrydir, "rTemp")

# Check if directory exists, otherwise create it
if (!dir.exists(rTempdir)) {
  dir.create(rTempdir, recursive = TRUE)
}

# Define Demand directory ----
# demanddir <- file.path(countrydir,"/LULCC/DownloadedDatasets/SourceDataGlobal/demand") #Why was this here, under what situation demanddir is NOT created??
demanddir

# Check if directory exists, otherwise create it
if (!dir.exists(demanddir)) {
  dir.create(demanddir, recursive = TRUE)
}

# End of script ----

