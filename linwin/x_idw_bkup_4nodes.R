# MoFuSS
# Version 3
# Date: May 2024

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
library(stringr)

if (fixdir == 1){
  
  # Define a particular directory when needed:
  # adm0_dirs <- c("D:/MoFuSS_Nepal_100m_May2024_4Chloris")
  # adm0_dirs <- c("D:/MoFuSS_Nepal_100m_May2024")
  adm0_dirs <- c("D:/ASIA_adm0_indonesia_aug2024", 
                 "D:/ASIA_adm0_phillipines_aug2024", 
                 "D:/ASIA_adm0_seasia_aug2024",
                 "D:/HND_100m_20240809", 
                 "D:/SSA_adm0_malawi_aug2024_cal",
                 "D:/world_1000m_20240818")
  setwd(tk_choose.dir(default = getwd(), caption = "Define the directory to search"))
  search_path <- getwd()
  
} else {
  
  # Define the directory to search
  setwd(tk_choose.dir(default = getwd(), caption = "Define the directory to search"))
  search_path <- getwd()
  
  # List all directories in the specified path
  all_dirs <- dir_ls(search_path, type = "directory")
  
  # Filter directories containing 'adm0'
  adm0_dirs <- all_dirs[grepl("adm0", all_dirs)]

}


# Iterate over each directory
for (dir in adm0_dirs) {

  # Define the source directory containing the files to be checked
  source_dir <- file.path(dir, "In")
  
  # Check if the source directory exists
  if (dir_exists(source_dir)) {
    
    # List files in the source directory that match the pattern
    files_to_copy <- dir_ls(source_dir, regexp = "IDW_C")

    # Proceed only if there are files to copy
    if (length(files_to_copy) > 0) {
      
      # Create the new directory name
      new_dir <- file.path(dirname(dir), paste0("idw_", basename(dir)))
      
      # Create the new directory
      dir_create(new_dir)
      
      # Copy each file to the new directory
      file_copy(files_to_copy, new_dir)
    }
  }
}

print("Folders created and files copied successfully.")

# Define the path to the directory
directory_path <- search_path

# List all items in the directory and filter to only directories
all_folders <- list.dirs(path = directory_path, full.names = TRUE, recursive = FALSE)

# Filter folders containing both "idw" and "adm0"
filtered_folders <- all_folders[str_detect(all_folders, "idw") & str_detect(all_folders, "adm0")]

# Rename each folder by replacing "adm0" with "amdidw"
for (folder in filtered_folders) {
  # Extract the base name of the folder (without the full path)
  base_folder_name <- basename(folder)
  # Create the new folder name
  new_base_name <- str_replace(base_folder_name, "adm0", "admidw")
  # Combine the new base name with the original directory path
  new_folder_name <- file.path(dirname(folder), new_base_name)
  # Rename the folder
  file.rename(from = folder, to = new_folder_name)
}

# Print the renamed folders for verification
renamed_folders <- str_replace(filtered_folders, "adm0", "admidw")
print(renamed_folders)

print("Folders renamed successfully.")