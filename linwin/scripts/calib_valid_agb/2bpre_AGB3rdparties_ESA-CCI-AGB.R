# MoFuSS
# Version 4
# Date: Sep 2024

# 2dolist
# Watch out for 3rd party biomass and bulk download using wget - add instructions adrian
# Loop through available years?
# mask for whatever region - very important
# if gcs exist then

# Internal parameters
# NOT INTENDED TO BE RUN ON A REGULAR BASIS BUT CASE BY CASE SCENARIO
yearbio <- 2020
resbio <- 1000 # 100 or 1000 (in meters)
sameregion_diffyear <- "yes" # "yes or "no"

# Load packages ----
library(terra)
library(tidyverse)
library(tcltk)

# Define file paths
setwd(normalizePath("~"))
getwd()
setwd(tk_choose.dir(default = getwd(), caption = paste0("Choose where your ESA CCI AGB files for the year ",yearbio," are located")))
input_dir <- getwd()

choose_directory955 = function(caption = paste0("Choose your output directory for the year ",yearbio)) {
  if(.Platform$OS.type == "unix")  {
    setwd(tk_choose.dir("/", caption = caption))
  } else {
    setwd(choose.dir("/", caption = caption))
  }
}
choose_directory955()
output_dir <- getwd()

if (sameregion_diffyear == "yes") {  
  if (exists("mask_file") == FALSE) {
    mask_file <- tk_choose.files(default = "", caption = "Select the mask .gpkg file", 
                                 multi = FALSE, filters = matrix(c("Geopackage files", "*.gpkg"), 1, 2))
  }
} else {
  mask_file <- tk_choose.files(default = "", caption = "Select the mask .gpkg file", 
                               multi = FALSE, filters = matrix(c("Geopackage files", "*.gpkg"), 1, 2))
}

if (basename(mask_file) == "mofuss_regions0.gpkg") {
  ext_name <- "world"
} else {
  extracted_string <- sub(".*adm0_(.*)\\.gpkg", "\\1", basename(mask_file))
  print(extracted_string)
  ext_name <- extracted_string
}

if (sameregion_diffyear == "yes") { 
  if (exists("DTEM_gcs") == FALSE) {
    DTEM_gcs <- tk_choose.files(default = "", caption = "Select the DTEM GCS .tif file", 
                                multi = FALSE, filters = matrix(c("DTEM raster files", "*.tif"), 1, 2))
  }
  if (exists("DTEM_pcs") == FALSE) {
    DTEM_pcs <- tk_choose.files(default = "", caption = "Select the DTEM PCS .tif file", 
                                multi = FALSE, filters = matrix(c("DTEM raster files", "*.tif"), 1, 2))
  }
} else {
  DTEM_gcs <- tk_choose.files(default = "", caption = "Select the DTEM GCS .tif file", 
                              multi = FALSE, filters = matrix(c("DTEM raster files", "*.tif"), 1, 2))
  DTEM_pcs <- tk_choose.files(default = "", caption = "Select the DTEM PCS .tif file", 
                              multi = FALSE, filters = matrix(c("DTEM raster files", "*.tif"), 1, 2))
}
#DTEM_gcs <- rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster_GCS/DTEM_gcs.tif"))
#DTEM_pcs <- rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/DTEM_pcs.tif"))

# If AGB exists as GCS, then project it, otherwise create it ---- 
if (file.exists(paste0(output_dir,"/out_gcs/",ext_name,"_agb_",yearbio,"_c.tif")) == TRUE) {
  
  # Compare resolutions
  if (res(rast(DTEM_pcs))[1] == resbio) {
    # If resolutions are equal, proceed with the rest of the code
    print("Resolutions are equal. Proceeding with the code.")
    
    # AGB
    if (file.exists(paste0(output_dir,"/out_pcs/",ext_name,"_agb_",yearbio,"_c.tif")) == FALSE) {
    
    rast(paste0(output_dir,"/out_gcs/",ext_name,"_agb_",yearbio,"_c.tif")) %>%
      terra::project(rast(DTEM_pcs), method="bilinear", gdal=TRUE) %>%
      writeRaster(paste0(output_dir,"/out_pcs/",ext_name,"_agb_",yearbio,"_c.tif"), overwrite = TRUE)
    
    # SD
    rast(paste0(output_dir,"/out_gcs/",ext_name,"_sd_",yearbio,"_c.tif")) %>%
      terra::project(rast(DTEM_pcs), method="bilinear", gdal=TRUE) %>%
      writeRaster(paste0(output_dir,"/out_pcs/",ext_name,"_sd_",yearbio,"_c.tif"), overwrite = TRUE)
    
    } else {
      
      print("All AGB and SD maps are already in place and ready to be used")
      
    }
    
  } else {
    # If resolutions are not equal, stop and return a message
    stop("The resolutions of the rasters are not equal. Code execution stopped.")
  }
  
} else {
  
  unlink(paste0(output_dir,"/temp/"), recursive = TRUE) # Warning, slow geoprocessing times
  unlink(paste0(output_dir,"/out_gcs/"), recursive = TRUE) # Warning, slow geoprocessing times
  unlink(paste0(output_dir,"/out_pcs/"), recursive = TRUE) # Warning, slow geoprocessing times
  Sys.sleep(3)
  if (!dir.exists(paste0(output_dir,"/temp/"))) {dir.create(paste0(output_dir,"/temp/"))}
  if (!dir.exists(paste0(output_dir,"/out_gcs/"))) {dir.create(paste0(output_dir,"/out_gcs/"))}
  if (!dir.exists(paste0(output_dir,"/out_pcs/"))) {dir.create(paste0(output_dir,"/out_pcs/"))}
  
  # List AGB and SD rasters
  agb_files <- list.files(input_dir, pattern = paste0("AGB-MERGED-100m-",yearbio,"-fv5.0.tif$"), full.names = TRUE)
  sd_files <- list.files(input_dir, pattern = paste0("AGB_SD-MERGED-100m-",yearbio,"-fv5.0.tif$"), full.names = TRUE)
  
  # Load the polygon mask
  mask <- vect(mask_file)

  # Function to filter raster tiles that intersect with the mask
  filter_tiles <- function(raster_files, mask) {
    raster_files[sapply(raster_files, function(file) {
      rast_100m <- rast(file)
      !is.null(terra::intersect(terra::ext(rast_100m), terra::ext(mask)))  # Check if raster intersects with mask extent
    })]
  }
  
  # Filter AGB and SD tiles
  agb_files_filtered <- filter_tiles(agb_files, mask)
  sd_files_filtered <- filter_tiles(sd_files, mask)
  
  # Read the reference raster (DTEM)
  dtem_gcs <- rast(DTEM_gcs)
  dtem_pcs <- rast(DTEM_pcs)
  
  # Rescale only the filtered AGB rasters with progress tracking
  total_agb <- length(agb_files_filtered)
  rescaled_agb <- lapply(seq_along(agb_files_filtered), function(i) {
    file <- agb_files_filtered[[i]]
    message(paste("Processing AGB file", i, "out of", total_agb))
    rast_100m <- rast(file)
    if (resbio == 1000) {
      resample(rast_100m, dtem_gcs, method = "bilinear")  # Rescale to 1km if resbio = 1000
    } else { 
      rast_100m
    }
  })
  
  # Rescale only the filtered SD rasters with progress tracking
  total_sd <- length(sd_files_filtered)
  rescaled_sd <- lapply(seq_along(sd_files_filtered), function(i) {
    file <- sd_files_filtered[[i]]
    message(paste("Processing SD file", i, "out of", total_sd))
    rast_100m <- rast(file)
    if (resbio == 1000) {
      resample(rast_100m, dtem_gcs, method = "bilinear")  # Rescale to 1km if resbio = 1000
    } else { 
      rast_100m
    }
  })
  
  # Define a function to merge tiles in chunks and write them incrementally to disk
  merge_and_write <- function(raster_list, output_path, chunk_dir, chunk_size = 5) {
    # Create chunk directory if it doesn't exist
    if (!dir.exists(chunk_dir)) {
      dir.create(chunk_dir, recursive = TRUE)
    }
    
    # Split the raster list into chunks
    raster_chunks <- split(raster_list, ceiling(seq_along(raster_list) / chunk_size))
    
    # Merge each chunk and write to disk incrementally
    for (i in seq_along(raster_chunks)) {
      message(paste("Merging chunk", i, "out of", length(raster_chunks)))
      
      # Check if there's only one raster in the chunk
      if (length(raster_chunks[[i]]) == 1) {
        chunk_mosaic <- raster_chunks[[i]][[1]]  # No need to merge, use the raster directly
      } else {
        chunk_mosaic <- do.call(merge, raster_chunks[[i]])
      }
      
      # Write chunk to a temporary file in the specified chunk directory
      chunk_file <- paste0(chunk_dir, "/chunk_", i, ".tif")
      
      # Write the chunk to disk
      writeRaster(chunk_mosaic, chunk_file, filetype = "GTiff", gdal = c("COMPRESS=LZW", "BIGTIFF=YES"), overwrite = TRUE)
    }
    
    # Now merge all the chunks into the final mosaic
    chunk_files <- list.files(chunk_dir, pattern = "chunk_\\d+.tif$", full.names = TRUE)
    
    # If there's only one chunk, no need to merge
    if (length(chunk_files) == 1) {
      final_mosaic <- rast(chunk_files[[1]])
    } else {
      final_mosaic <- do.call(merge, lapply(chunk_files, rast))
    }
    
    # Write the final mosaic
    writeRaster(final_mosaic, output_path, filetype = "GTiff", gdal = c("COMPRESS=LZW", "BIGTIFF=YES"), overwrite = TRUE)
  }
  
  # Use the function to merge AGB and SD rasters in chunks
  agb_output_path <- file.path(output_dir, paste0("/temp/",ext_name,"_agb_",yearbio,".tif"))
  sd_output_path <- file.path(output_dir, paste0("/temp/",ext_name,"_sd_",yearbio,".tif"))
  
  # Create separate directories for the AGB and SD chunks
  agb_chunk_dir <- file.path(output_dir, "/temp/agb_chunks")
  sd_chunk_dir <- file.path(output_dir, "/temp/sd_chunks")
  
  merge_and_write(rescaled_agb, agb_output_path, agb_chunk_dir)
  merge_and_write(rescaled_sd, sd_output_path, sd_chunk_dir)
  
  agb_xxx <- rast(paste0(output_dir,"/temp/",ext_name,"_agb_",yearbio,".tif")) %>%
    crop(.,mask) %>%
    mask(mask)
  writeRaster(agb_xxx, paste0(output_dir,"/out_gcs/",ext_name,"_agb_",yearbio,"_c.tif"), overwrite = TRUE)
  
  sd_xxx <- rast(paste0(output_dir,"/temp/",ext_name,"_sd_",yearbio,".tif")) %>%
    crop(.,mask) %>%
    mask(mask)
  writeRaster(sd_xxx, paste0(output_dir,"/out_gcs/",ext_name,"_sd_",yearbio,"_c.tif"), overwrite = TRUE)
  
}

# # Copy 2 MoFuSS ----
# copy2mofussfiles5 <- list.files(path = paste0(output_dir,"/out_gcs/"),
#                                 pattern = ".*\\.tif$", full.names = TRUE)
# for (f5 in copy2mofussfiles5) {
#   file.copy(from=f5, 
#             to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster_GCS/"),  
#             overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
# }
# 
# copy2mofussfiles4 <- list.files(path = paste0(output_dir,"/out_pcs/"),
#                                 pattern = ".*\\.tif$", full.names = TRUE)
# for (f4 in copy2mofussfiles4) {
#   file.copy(from=f4, 
#             to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/"),  
#             overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
# }

