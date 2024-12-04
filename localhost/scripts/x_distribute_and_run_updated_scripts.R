# MoFuSS
# Version 3
# Date: Jul 2024


# 2dolist


# Internal parameters
fixdir <- 0

# Load packages ----
library(raster)
library(dplyr)
library(readxl)
library(tictoc)
library(fs)
library(tcltk)

if (fixdir == 1){
  
  # Define a particular directory when needed:
  # adm0_dirs <- c("D:/MoFuSS_Nepal_100m_May2024_4Chloris")
  # adm0_dirs <- c("D:/MoFuSS_Nepal_100m_May2024")
  adm0_dirs <- c("D:/OCEANIA_adm0_png_sep2024")
  
} else {
  
  # Define the directory to search
  setwd(tk_choose.dir(default = getwd(), caption = "Define the directory to search"))
  search_path <- getwd()
  
  # List all directories in the specified path
  all_dirs <- dir_ls(search_path, type = "directory")
  
  # Filter directories containing 'adm0' but not 'idw'
  adm0_dirs <- all_dirs[grepl("adm0", all_dirs) & !grepl("idw", all_dirs) & !grepl("regions", all_dirs)]
  adm0_dirs
}

#Set working directories
setwd(normalizePath("~"))
getwd()
setwd(tk_choose.dir(default = getwd(), caption = "Choose your local gitlab mofuss directory"))
gitlabdir <- getwd()

# Copy mofuss files in gitlab to all working directories (adm0)---- 
# Define the list of files to copy
files_to_copy <- c(
  # paste0(gitlabdir, "/localhost/scripts/7_FW_dyn_lulcc_Sc16b_luc1.egoml"),
  # paste0(gitlabdir, "/localhost/scripts/7_FW_dyn_lulcc_Sc16b_luc2.egoml"),
  paste0(gitlabdir, "/localhost/scripts/rnorm2.R"),
  # paste0(gitlabdir, "/localhost/scripts/maps_animations4.R"),
  paste0(gitlabdir, "/localhost/scripts/maps_animations5.R"),
  paste0(gitlabdir, "/localhost/scripts/NRB_graphs_datasets2.R")
)

# Loop through each directory in adm0_dirs
for (dir in adm0_dirs) {
  # dir <- "D:/OCEANIA_adm0_png_sep2024"
  # Loop through each file to copy
  for (file in files_to_copy) {
    # Define the destination file path
    dest_file <- file.path(dir, basename(file))
    # Copy the file to the destination directory
    file.copy(file, dest_file, overwrite = TRUE)
  }
}

# Execute certain scripts across all directories---

# External script path
script_path <- paste0(gitlabdir,"/localhost/scripts/maps_animations5.R")

# Loop through each directory and execute the script
for (dir in adm0_dirs) {
  cat("Processing directory:", dir, "\n")
  
  # Change the working directory
  setwd(dir)
  
  # Run the external script
  tryCatch(
    {
      source(script_path)
      cat("Successfully processed:", dir, "\n")
    },
    error = function(e) {
      cat("Error in processing:", dir, "\n", e, "\n")
    }
  )
}



 