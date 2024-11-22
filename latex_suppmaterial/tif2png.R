setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load necessary library
library(fs)
library(tcltk)
library(magick)

# Define the directory to search
setwd(tk_choose.dir(default = getwd(), caption = "Define the directory to search"))
search_path <- getwd()

# List all directories in the specified path
all_dirs <- dir_ls(search_path, type = "directory")

# Filter directories containing 'adm0'
adm0_dirs <- all_dirs[grepl("adm0", all_dirs)]

# Write the filtered directory names to a text file
# writeLines(basename(adm0_dirs), "directories.txt")
# cat("Directories listed in 'directories.txt'\n")

# Define the list of source and destination directories
source_dirs <- paste0(adm0_dirs,"/OutBaU")
dest_dirs <- paste0(adm0_dirs,"/OutBaU/png")

# Ensure source and destination directories have the same length
if (length(source_dirs) != length(dest_dirs)) {
  stop("Source and destination directories lists must have the same length.")
}

# Loop through each directory pair and convert the files
for (i in seq_along(source_dirs)) {
  source_dir <- source_dirs[i]
  dest_dir <- dest_dirs[i]
  
  # Get the .tif file path
  tif_file <- file.path(source_dir, "NRBsd.tif")  # Assuming the .tif file name is "NRBsd.tif"
  
  # Check if the .tif file exists
  if (file.exists(tif_file)) {
    # Load the .tif file
    img <- image_read(tif_file)
    
    # Create the destination file path by replacing .tif with .png
    png_file <- file.path(dest_dir, "NRBsd.png")
    
    # Save the image as .png
    image_write(img, path = png_file, format = "png")
  } else {
    cat(paste("File not found:", tif_file, "\n"))
  }
}

cat("Conversion completed!\n")
