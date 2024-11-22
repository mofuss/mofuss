library(terra)

# Set the paths to your folders
disk_dir <- "C:/Users/aghil/Documents/webmofuss_results_disk"
temp_dir <- "C:/Users/aghil/Documents//webmofuss_results_temp"

# Get the list of raster files in both directories
disk_rasters <- list.files(disk_dir, pattern = "\\.tif$", full.names = TRUE)
temp_rasters <- list.files(temp_dir, pattern = "\\.tif$", full.names = TRUE)

# Ensure both directories have the same number of files
if (length(disk_rasters) != length(temp_rasters)) {
  stop("The number of .tif files in both folders is different!")
}

# Function to compare two rasters up to 99% identical, rounding to 2 decimals
compare_rasters_99 <- function(disk_raster, temp_raster, tolerance = 0.99, decimals = 2) {
  # Load the rasters
  r_disk <- rast(disk_raster)
  r_temp <- rast(temp_raster)
  
  # Check if dimensions are identical
  if (!compareGeom(r_disk, r_temp, stopOnError = FALSE)) {
    return(FALSE)  # Different size or resolution
  }
  
  # Get raster values and round to the specified number of decimals
  values_disk <- round(values(r_disk), decimals)
  values_temp <- round(values(r_temp), decimals)
  
  # Ensure the rasters have the same number of cells
  if (length(values_disk) != length(values_temp)) {
    return(FALSE)  # Different number of cells
  }
  
  # Compare values, ignoring NA
  matching_values <- sum(values_disk == values_temp, na.rm = TRUE)
  total_values <- sum(!is.na(values_disk) & !is.na(values_temp))
  
  # Calculate the percentage of matching values
  percentage_identical <- matching_values / total_values
  
  return(percentage_identical >= tolerance)
}

# Compare each pair of rasters
results <- sapply(seq_along(disk_rasters), function(i) {
  disk_raster <- disk_rasters[i]
  temp_raster <- temp_rasters[i]
  
  # Compare the corresponding rasters (up to 99%) and round to 2 decimals
  identical_rasters <- compare_rasters_99(disk_raster, temp_raster, tolerance = 0.99, decimals = 2)
  
  return(identical_rasters)
})

# Create a report of identical and non-identical rasters
names(results) <- basename(disk_rasters)
print(results)

# # Save the results to a CSV file
# write.csv(data.frame(Raster = names(results), Identical_99_2dec = results), "raster_comparison_results_99_2dec.csv", row.names = FALSE)


