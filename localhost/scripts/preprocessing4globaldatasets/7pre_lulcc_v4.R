# MoFuSS
# Version 3
# Date: Mar 2024
# References for IPCC values, to be reconsidered eventually
# https://www.ipcc-nggip.iges.or.jp/public/2019rf/index.html
# https://www.ipcc-nggip.iges.or.jp/public/2019rf/pdf/4_Volume4/19R_V4_Ch04_Forest%20Land.pdf

# 2dolist
# Check values less than 1 in K and KSD, or under MoFuSS harvestable threshold

# Internal parameters
plot_curves = 0
# Determine the number of chunks (adjust based on available memory)
n_chunks <- 500

# Load packages ----
library(readr)
# library(raster)
library(terra)
terraOptions(steps = 55)
terraOptions(progress=0)
library(sf)
library(tidyverse)
library(tictoc)
library(tidyr)
library(sp)
library(truncnorm)
library(ggplot2)
library(svDialogs)
library(readxl)
library(tcltk)

setwd(countrydir)
getwd()
country_name

# Read parameters table ----
country_parameters <- read_excel(paste0("LULCC/DownloadedDatasets/SourceData",country_name,"/",parameters_file))
print(tibble::as_tibble(country_parameters), n=100)

country_parameters %>%
  dplyr::filter(Var == "proj_gcs") %>%
  pull(ParCHR) -> proj_gcs

country_parameters %>%
  dplyr::filter(Var == "epsg_gcs") %>%
  pull(ParCHR) %>%
  as.integer(.) -> epsg_gcs

country_parameters %>%
  dplyr::filter(Var == "proj_pcs") %>%
  pull(ParCHR) -> proj_pcs

country_parameters %>%
  dplyr::filter(Var == "epsg_pcs") %>%
  pull(ParCHR) %>%
  as.integer(.) -> epsg_pcs

country_parameters %>%
  dplyr::filter(Var == "proj_authority") %>%
  pull(ParCHR) -> proj_authority

country_parameters %>%
  dplyr::filter(Var == "LULCt1map") %>%
  pull(ParCHR) -> LULCt1map

country_parameters %>%
  dplyr::filter(Var == "LULCt2map") %>%
  pull(ParCHR) -> LULCt2map

country_parameters %>%
  dplyr::filter(Var == "pdecil") %>%
  pull(ParCHR) %>%
  as.numeric(.) -> pdecil

if (exists("lulccfiles") == FALSE) {
  choose_directory71 = function(caption = "Choose the directory where land use/cover files are") {
    if(.Platform$OS.type == "unix")  {
      setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
    } else {
      setwd(choose.dir("/home/mofuss/Documents", caption = caption))
    }
  }
  choose_directory71()
  lulccfiles <- getwd()
}

# Reads input datasets from parameters csv
if (LULCt1map == "YES" & LULCt2map == "YES"){
  lucavailablemaps <- c("modis", "copernicus")
} else if (LULCt1map == "YES" & LULCt2map != "YES"){
  lucavailablemaps <- c("modis")
} else if (LULCt1map != "YES" & LULCt2map == "YES"){
  lucavailablemaps <- c("copernicus")
}
lucavailablemaps

unlink("temp/", recursive = TRUE) # Warning, slow geoprocessing times
unlink("out_gcs/", recursive = TRUE) # Warning, slow geoprocessing times
unlink("out_pcs/", recursive = TRUE) # Warning, slow geoprocessing times
Sys.sleep(5)
if (!dir.exists("temp")) {dir.create("temp")}
if (!dir.exists("out_gcs")) {dir.create("out_gcs")}
if (!dir.exists("out_pcs")) {dir.create("out_pcs")}

for (lucinputdataset in lucavailablemaps) {
  # lucinputdataset = "modis"
  # lucinputdataset = "copernicus"
  setwd(lulccfiles)
  getwd()
  # Rasterize
  DTEM_gcs <- rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster_GCS/DTEM_gcs.tif"))
  DTEM_pcs <- rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/DTEM_pcs.tif"))
  
  # Regions ----  
  regions_adm0_p <- st_read(paste0(admindir,"/regions_adm0_p/mofuss_regions0_p.gpkg")) %>%
    dplyr::mutate(continent = case_when(
      startsWith(mofuss_reg, "ASIA") ~ 3, #"ASIA",
      startsWith(mofuss_reg, "LATAM") ~ 2, # "LATAM",
      startsWith(mofuss_reg, "SSA") ~ 1, # "SSA",
      startsWith(mofuss_reg, "NorAfr") ~ 4, # "NorAfr",
      startsWith(mofuss_reg, "OCEANIA") ~ 5, # "NorAfr",
    ))
  regions_adm0_p_df <- as.data.frame(regions_adm0_p) %>%
    dplyr::select(-geom)
  
  regionsr_p <-raster::rasterize(vect(regions_adm0_p), DTEM_pcs, "continent")
  regionsr_p_scale <- regionsr_p * 10000
  terra::writeRaster(regionsr_p_scale, "temp/regions10000_pcs.tif",
                     filetype = "GTiff", overwrite = TRUE)
  
  # Global Ecological Zones ----
  gez <- st_read("gez_2010_wgs84.shp")
  st_write(gez, "temp/gez_gcs.gpkg", delete_layer=TRUE)
  
  country_parameters %>%
    dplyr::filter(Var == "gez_name") %>%
    pull(ParCHR) -> gez_name
  gez_p <- gez %>%
    st_transform(paste0(proj_authority,":",epsg_pcs))
  st_write(gez_p, paste0("temp/",gez_name), delete_layer=TRUE)
  # terra::crs(gez)
  # terra::crs(gez_p)
  country_parameters %>%
    dplyr::filter(Var == "gez_fieldname") %>%
    pull(ParCHR) -> gez_fieldname
  gezr <-terra::rasterize(vect(gez), DTEM_gcs, gez_fieldname)
  terra::writeRaster(gezr, "temp/gez_gcs.tif",
                     filetype = "GTiff", overwrite = TRUE)
  
  gez_p_df <- as.data.frame(gez_p) %>%
    dplyr::select(-geometry)
  
  gezr_p <- terra::rasterize(vect(gez_p), DTEM_pcs, gez_fieldname)
  gezr_p_scale <- gezr_p * 100
  terra::writeRaster(gezr_p_scale, "temp/gez100_pcs.tif",
                     filetype = "GTiff", overwrite = TRUE)
  
  
  # Land use/cover ----
  if (lucinputdataset == "modis") {
    
    ## MODIS: MCD12Q1.061 MODIS Land Cover Type Yearly Global 500m ----
    lucmodis_cat <- read_csv(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables/luc_modis_categories.csv"))
    
    country_parameters %>%
      dplyr::filter(Var == "LULCt1map_name") %>%
      pull(ParCHR) -> LULCt1map_name
    country_parameters %>%
      dplyr::filter(Var == "LULCt1map_yr") %>%
      pull(ParCHR) -> LULCt1map_yr_pre
    clean_string1 <- gsub("c\\(|\\)", "", LULCt1map_yr_pre)
    string_numbers1 <- strsplit(clean_string1, ",")[[1]]
    LULCt1map_yr <- as.numeric(string_numbers1)
    
    modis_raster_files <- list.files(path = paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster"), pattern = "\\_modis_lc_type1_pcs.tif$", full.names = TRUE)
    for (k in LULCt1map_yr){
      # Generate a unique variable name for each raster
      var_name_modis <- paste0("lucmodis_",k)
      
      # Read the raster file
      modis_raster_datam <- terra::rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/pre",k,"_",LULCt1map_name))
      
      # Assign the raster data to a dynamically named variable
      assign(var_name_modis, modis_raster_datam)
      print(var_name_modis)
    }
    
  } else if (lucinputdataset == "copernicus") {
    ## Copernicus CGLS-LC100 ----
    luccopernicus_cat_vx <- read_csv(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables/luc_copernicus_categories.csv"))
    luccopernicus_cat <- luccopernicus_cat_vx %>%
      dplyr::select(luc_code, luc_cat, TOF_luc)
    
    country_parameters %>%
      dplyr::filter(Var == "LULCt2map_name") %>%
      pull(ParCHR) -> LULCt2map_name
    country_parameters %>%
      dplyr::filter(Var == "LULCt2map_yr") %>%
      pull(ParCHR) -> LULCt2map_yr_pre
    clean_string2 <- gsub("c\\(|\\)", "", LULCt2map_yr_pre)
    string_numbers2 <- strsplit(clean_string2, ",")[[1]]
    LULCt2map_yr <- as.numeric(string_numbers2)
    
    copernicus_raster_files <- list.files(path = paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster"), pattern = "\\_cgls_lc100_pcs.tif$", full.names = TRUE)
    for (l in LULCt2map_yr){
      # Generate a unique variable name for each raster
      var_name_cop <- paste0("luccopernicus_",l)
      
      # Read the raster file
      copernicus_raster_datac <- terra::rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/pre",l,"_",LULCt2map_name))
      
      # Assign the raster data to a dynamically named variable
      assign(var_name_cop, copernicus_raster_datac)
      print(var_name_cop)
    }
  } else if (lucinputdataset == "3rdmap") {
    # Add a third, multiyear luc map...
  }
  
  # Merge
  
  if (lucinputdataset == "modis") {
    
    lucmodis_2010_merge <- regionsr_p_scale + gezr_p_scale + lucmodis_2010
    terra::writeRaster(lucmodis_2010_merge, "temp/lucmodis_2010_merge_pcs.tif",
                       filetype = "GTiff", overwrite = TRUE)
    # lucmodis_2010_merge <- rast("temp/lucmodis_2010_merge_pcs.tif")
    ncell(lucmodis_2010_merge)
    lucmodis_2010_poly <- as.polygons(lucmodis_2010_merge)
    as.data.frame(lucmodis_2010_poly)
    psample <- lucmodis_2010_poly[1:10,]
    as.data.frame(psample)
    head(psample)
    
    # Build dataset modis
    luc_2010_dfv0 <- unique(lucmodis_2010_merge)
    luc_2010_df <- luc_2010_dfv0 %>%
      dplyr::mutate(chr_code = as.character(continent)) %>%
      separate(chr_code, into = c('reg_code', 'temp'), sep = 1) %>%
      separate(temp, into = c('gez_code', 'luc_code'), sep = 2) %>%
      dplyr::mutate_at(c('reg_code','gez_code','luc_code'), as.numeric)
    head(luc_2010_df)
    
    region_f <- data.frame(reg_code = sort(unique(regions_adm0_p_df$continent)),
                           reg_chr = c("SSA", "LATAM", "ASIA", "NorAfr", "OCEANIA")) %>%
      dplyr::mutate_at(c('reg_code'), as.numeric)
    
    growth_para_v1 <- inner_join(luc_2010_df, region_f, by = "reg_code") %>%
      dplyr::inner_join(gez_p_df, by = "gez_code") %>%
      dplyr::inner_join(lucmodis_cat, by = "luc_code", multiple = "all") %>%
      tidyr::unite("reg_gez_luc", c(reg_chr, gez_name, luc_cat), sep= "_", remove = FALSE) %>%
      tidyr::unite("reg_gez", c(reg_chr, gez_name), sep= "_", remove = FALSE) %>%
      dplyr::rename(IDorig = continent) %>%
      dplyr::relocate(reg_gez, .after = luc_cat) %>%
      dplyr::relocate(reg_gez_luc, .after = reg_gez)
    head(growth_para_v1)
    # Find duplicates
    dup <- growth_para_v1 %>% 
      group_by(IDorig) %>% 
      filter(n()>1)
    as.data.frame(unique(growth_para_v1$reg_gez)) %>%
      dplyr::rename(reg_gez = "unique(growth_para_v1$reg_gez)") %>%
      write.csv("temp/gez_gcs.csv") # Warning, these categories are exported to match IPCC values - IS NOT AUTOMATED
    
  } else if (lucinputdataset == "copernicus") {
    
    rclmat_cop <- luccopernicus_cat_vx %>%
      dplyr::select("luc_code_orig", "luc_code") %>%
      unname() %>%
      as.matrix()
    luccopernicus_2015_rcr <- classify(luccopernicus_2015[[1]], rclmat_cop, include.lowest=TRUE) # First band only as Copernicus might have many after Diana's change
    
    luccopernicus_2015_merge <- regionsr_p_scale + gezr_p_scale + luccopernicus_2015_rcr
    terra::writeRaster(luccopernicus_2015_merge, "temp/luccop_2015_merge_pcs.tif",
                       filetype = "GTiff", overwrite = TRUE)
    # lucmodis_2010_merge <- rast("temp/lucmodis_2010_merge_pcs.tif")
    ncell(luccopernicus_2015_merge)
    luccopernicus_2015_poly <- as.polygons(luccopernicus_2015_merge)
    as.data.frame(luccopernicus_2015_poly)
    psample_c <- luccopernicus_2015_poly[1:10,]
    as.data.frame(psample_c)
    head(psample_c)
    
    # Build dataset copernicus
    luc_2015_dfv0_cop <- unique(luccopernicus_2015_merge)
    luc_2015_df_cop <- luc_2015_dfv0_cop %>%
      dplyr::mutate(chr_code = as.character(continent)) %>%
      separate(chr_code, into = c('reg_code', 'temp'), sep = 1) %>%
      separate(temp, into = c('gez_code', 'luc_code'), sep = 2) %>%
      dplyr::mutate_at(c('reg_code','gez_code','luc_code'), as.numeric)
    head(luc_2015_df_cop)
    
    region_f <- data.frame(reg_code = sort(unique(regions_adm0_p_df$continent)),
                           reg_chr = c("SSA", "LATAM", "ASIA", "NorAfr", "OCEANIA")) %>%
      dplyr::mutate_at(c('reg_code'), as.numeric)
    
    growth_para_v1 <- inner_join(luc_2015_df_cop, region_f, by = "reg_code") %>%
      dplyr::inner_join(gez_p_df, by = "gez_code") %>%
      dplyr::inner_join(luccopernicus_cat, by = "luc_code", multiple = "all") %>%
      tidyr::unite("reg_gez_luc", c(reg_chr, gez_name, luc_cat), sep= "_", remove = FALSE) %>%
      tidyr::unite("reg_gez", c(reg_chr, gez_name), sep= "_", remove = FALSE) %>%
      dplyr::rename(IDorig = continent) %>%
      dplyr::relocate(reg_gez, .after = luc_cat) %>%
      dplyr::relocate(reg_gez_luc, .after = reg_gez)
    head(growth_para_v1)
    # Find duplicates
    dup <- growth_para_v1 %>% 
      group_by(IDorig) %>% 
      filter(n()>1)
    as.data.frame(unique(growth_para_v1$reg_gez)) %>%
      dplyr::rename(reg_gez = "unique(growth_para_v1$reg_gez)") %>%
      write.csv("temp/gez_gcs.csv") # Warning, these categories are exported to match IPCC values - IS NOT AUTOMATED
    
  }
  
  # AGB stats ----
  if (lucinputdataset == "modis") {
    luc_poly <- lucmodis_2010_poly
  } else if (lucinputdataset == "copernicus") {
    luc_poly <- luccopernicus_2015_poly
  }
  
  country_parameters %>%
    dplyr::filter(Var == "AGB1map_name") %>%
    pull(ParCHR) -> AGB1map_name
  agb4statsnull <- rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/",AGB1map_name))
  agb4stats <- ifel(agb4statsnull < 0, 0, agb4statsnull) %>%
    app(fun = as.integer)
  names(agb4stats) <- "agb"
  
  country_parameters %>%
    dplyr::filter(Var == "AGB1map_uncer_name") %>%
    pull(ParCHR) -> AGB1map_uncer_name
  agb4stats_uncernull <- rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/",AGB1map_uncer_name))
  agb4stats_uncer <- ifel(agb4stats_uncernull < 0, NA, agb4stats_uncernull) %>%
    app(fun = as.integer)
  names(agb4stats_uncer) <- "agb_uncertainty" 
  
  m <- c(-1, 0, NA)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  
  agb4stats_rcr <- classify(agb4stats, rclmat, include.lowest=TRUE)
  terra::writeRaster(agb4stats_rcr, "temp/agb_rc_pcs.tif",
                     filetype = "GTiff", overwrite = TRUE)
  # agb4stats_rcr <- rast("temp/agb_rc_pcs.tif") # From 1 to max Mg/ha, already reclassified
  
  agb4stats_rcr_uncer <- classify(agb4stats_uncer, rclmat, include.lowest=TRUE)
  terra::writeRaster(agb4stats_rcr_uncer, "temp/agb_rc_uncer_pcs.tif",
                     filetype = "GTiff", overwrite = TRUE)
  # agb4stats_rcr_uncer <- rast("temp/agb_rc_uncer_pcs.tif") # From 1 to max Mg/ha, already reclassified
  
  # Function to calculate the mean of a quantile by zone
  pct_mean <- function(x, p=pdecil, na.rm = TRUE) {
    # Calculate the quantile value
    q_val <- quantile(x, p, na.rm = na.rm)
    # Compute the mean of all values in x that are more than or equal to the quantile value
    mean_val <- round(mean(x[x >= q_val], na.rm = na.rm),0)
    return(mean_val)
  }

  # # Determine the number of chunks (adjust based on available memory)
  # n_chunks <- 30
  
  # Calculate the indices for splitting
  chunk_indices <- split(seq_len(nrow(luc_poly)), cut(seq_len(nrow(luc_poly)), n_chunks, labels = FALSE))
  
  # Split the SpatVector into a list of smaller SpatVectors
  luc_poly_chunks <- lapply(chunk_indices, function(indices) {
    luc_poly[indices, ]
  })
  
  # Check the result
  length(luc_poly_chunks) # Should return 10
  
  # Initialize an empty list to store results
  results_list_pctmean <- list()
  
  # # Process each chunk
  # for (i in seq_along(luc_poly_chunks)) {
  #   cat("Processing chunk", i, "of", length(luc_poly_chunks), "\n")
  #   chunk <- luc_poly_chunks[[i]]
  #   if (nrow(chunk) == 0) next
  #   result_pctmean <- tryCatch({
  #     terra::extract(agb4stats_rcr, chunk, fun = pct_mean, bind = TRUE)
  #   }, error = function(e) {
  #     cat("Error in chunk", i, ":", conditionMessage(e), "\n")
  #     NULL
  #   })
  #   if (!is.null(result_pctmean)) {
  #     results_list_pctmean[[i]] <- result_pctmean
  #     saveRDS(result_pctmean, paste0("result_chunk_", i, ".rds"))
  #   }
  #   rm(result_pctmean, chunk)
  #   gc()
  # }
  
  
  # Define chunks to skip
  skip_chunks <- c(2) # Add any other chunk numbers if needed
  
  for (i in seq_along(luc_poly_chunks)) {
    if (i %in% skip_chunks) {
      cat("Skipping problematic chunk", i, "\n")
      next
    }
    cat("Processing chunk", i, "of", length(luc_poly_chunks), "\n")
    chunk <- luc_poly_chunks[[i]]
    if (nrow(chunk) == 0) {
      cat("Skipping chunk", i, "because it has no polygons.\n")
      next
    }
    cat("Number of polygons in this chunk:", nrow(chunk), "\n")
    result_pctmean <- tryCatch({
      terra::extract(agb4stats_rcr, chunk, fun = pct_mean, bind = TRUE)
    }, error = function(e) {
      cat("Error in chunk", i, ":", conditionMessage(e), "\n")
      NULL
    })
    if (!is.null(result_pctmean)) {
      results_list_pctmean[[i]] <- result_pctmean
      saveRDS(result_pctmean, paste0("result_chunk_", i, ".rds"))
    }
    rm(result_pctmean, chunk)
    gc()
  }
  
  
  # # Process each chunk
  # for (i in seq_along(luc_poly_chunks)) {
  #   # Print the index of the current chunk
  #   cat("Processing chunk", i, "of", length(luc_poly_chunks), "\n")
  #   # Optionally, print more details about the chunk (e.g., number of geometries)
  #   cat("Number of polygons in this chunk:", nrow(luc_poly_chunks[[i]]), "\n")
  #   # Extract raster values for the current chunk
  #   chunk <- luc_poly_chunks[[i]]
  #   result_pctmean <- terra::extract(agb4stats_rcr, chunk, fun=pct_mean, bind=TRUE)
  #   # Store the result
  #   results_list_pctmean[[i]] <- result_pctmean
  # }
  
  # Combine all results into a single data frame
  agb_mean_decilv0 <- do.call(rbind, results_list_pctmean)

  # # Compare single vs chunk geoprocessing ----
  # # Combine all results into a single data frame
  # agb_mean_decilv0_chunk <- do.call(rbind, results_list)
  # 
  # agb_mean_decilv0_old <- terra::extract(agb4stats_rcr, luc_poly, fun=pct_mean, bind=TRUE)
  # 
  # 
  # # Compare CRS
  # identical(terra::crs(agb_mean_decilv0_chunk), terra::crs(agb_mean_decilv0_old))  # TRUE if they have the same CRS
  # 
  # # Compare Extent
  # identical(terra::ext(agb_mean_decilv0_chunk), terra::ext(agb_mean_decilv0_old))  # TRUE if extents are identical
  # 
  # # Compare Number of Features
  # identical(nrow(agb_mean_decilv0_chunk), nrow(agb_mean_decilv0_old))  # TRUE if they have the same number of geometries
  # 
  # # Extract attribute tables
  # attributes1 <- terra::values(agb_mean_decilv0_chunk)
  # attributes2 <- terra::values(agb_mean_decilv0_old)
  # 
  # # Check if the data frames are identical
  # attributes_check <- all.equal(attributes1, attributes2)
  # if (isTRUE(attributes_check)) {
  #   cat("Attributes are identical.\n")
  # } else {
  #   cat("Attribute discrepancies:\n", attributes_check, "\n")
  # }
  # 
  # # Optionally, find mismatched rows
  # mismatched_rows <- which(!apply(attributes1 == attributes2, 1, all, na.rm = TRUE))
  # if (length(mismatched_rows) > 0) {
  #   cat("Mismatched rows:\n")
  #   print(attributes1[mismatched_rows, ])
  #   print(attributes2[mismatched_rows, ])
  # }
  # 
  # geometry_check <- terra::all.equal(agb_mean_decilv0_chunk, agb_mean_decilv0_old)
  # if (isTRUE(geometry_check)) {
  #   cat("Geometries are identical.\n")
  # } else {
  #   cat("Geometry discrepancies:\n", geometry_check, "\n")
  # }
  
  agb_mean_decilv0 <- terra::extract(agb4stats_rcr, luc_poly, fun=pct_mean, bind=TRUE)
  agb_mean_decil <- agb_mean_decilv0 %>%
    as.data.frame() %>%
    dplyr::rename(IDorig = continent,
                  agb_mean_Tdecil = agb) %>%
    dplyr::inner_join(growth_para_v1, by = "IDorig") %>%
    dplyr::select(-reg_code:-reg_gez_luc, -TOF_luc) %>%
    replace(., is.na(.), 1) # Replace  NaN for no pixels in category!
  head(agb_mean_decil)
  
  # Function to calculate the SD of a quantile by zone
  pct_sd <- function(x, p=pdecil, na.rm = TRUE) {
    # Calculate the quantile value
    q_val <- quantile(x, p, na.rm = na.rm)
    # Compute the standrad deviation of all values in x that are more than or equal to the quantile value
    sd_val <- round(sd(x[x >= q_val], na.rm = na.rm),0)
    return(sd_val)
  }
  
  # Initialize an empty list to store results
  results_list_pctsd <- list()
  
  # Process each chunk
  for (i in seq_along(luc_poly_chunks)) {
    # Print the index of the current chunk
    cat("Processing chunk", i, "of", length(luc_poly_chunks), "\n")
    # Optionally, print more details about the chunk (e.g., number of geometries)
    cat("Number of polygons in this chunk:", nrow(luc_poly_chunks[[i]]), "\n")
    # Extract raster values for the current chunk
    chunk <- luc_poly_chunks[[i]]
    result_pctsd <- terra::extract(agb4stats_rcr, chunk, fun=pct_sd, bind=TRUE)
    # Store the result
    results_list_pctsd[[i]] <- result_pctsd
  }
  
  # Combine all results into a single data frame
  agb_sd_decilv0 <- do.call(rbind, results_list_pctsd)
  agb_sd_decil <- agb_sd_decilv0 %>%
    as.data.frame() %>%
    dplyr::rename(IDorig = continent,
                  agb_sd_Tdecil = agb) %>%
    dplyr::inner_join(growth_para_v1, by = "IDorig") %>%
    dplyr::select(-reg_code:-reg_gez_luc, -TOF_luc) %>%
    replace(., is.na(.), 1) # Replace  NaN for no pixels in category!
  head(agb_sd_decil)
  
  # Function to calculate n of a quantile by zone
  pct_n <- function(x, p=pdecil, na.rm = TRUE) {
    # Calculate the quantile value
    q_val <- quantile(x, p, na.rm = na.rm)
    # Compute the standrad deviation of all values in x that are more than or equal to the quantile value
    n_val <- n_val <- sum(x >= q_val, na.rm = na.rm)
    return(n_val)
  }
  
  # Initialize an empty list to store results
  results_list_pctn <- list()
  
  # Process each chunk
  for (i in seq_along(luc_poly_chunks)) {
    # Print the index of the current chunk
    cat("Processing chunk", i, "of", length(luc_poly_chunks), "\n")
    # Optionally, print more details about the chunk (e.g., number of geometries)
    cat("Number of polygons in this chunk:", nrow(luc_poly_chunks[[i]]), "\n")
    # Extract raster values for the current chunk
    chunk <- luc_poly_chunks[[i]]
    result_pctn <- terra::extract(agb4stats_rcr, chunk, fun=pct_n, bind=TRUE)
    # Store the result
    results_list_pctn[[i]] <- result_pctn
  }
  
  # Combine all results into a single data frame
  agb_n_decilv0 <- do.call(rbind, results_list_pctn)
  agb_n_decil <- agb_n_decilv0 %>%
    as.data.frame() %>%
    dplyr::rename(IDorig = continent,
                  agb_n_Tdecil = agb) %>%
    dplyr::inner_join(growth_para_v1, by = "IDorig") %>%
    dplyr::select(-reg_code:-reg_gez_luc, -TOF_luc) %>%
    replace(., is.na(.), 1) # Replace  NaN for no pixels in category!
  head(agb_n_decil)
  
  # WATCH OUT HERE fun='mean' ---- its ok
  
  agb_meanv0 <- terra::extract(agb4stats_rcr, luc_poly, fun='mean', na.rm = TRUE, bind=TRUE)
  agb_meanv1 <- agb_meanv0  %>%
    as.data.frame() %>%
    round(.,0) %>%
    dplyr::rename(IDorig = continent,
                  agb_mean = agb) %>%
    dplyr::inner_join(growth_para_v1, by = "IDorig") %>%
    dplyr::select(-reg_code:-reg_gez_luc, -TOF_luc) %>%
    replace(., is.na(.), 1) # Replace  NaN for no pixels in category!
  head(agb_meanv1)
  
  agb_sdv0 <- terra::extract(agb4stats_rcr, luc_poly, fun='sd', na.rm = TRUE, bind=TRUE)
  agb_sdv1 <- agb_sdv0  %>%
    as.data.frame() %>%
    round(.,0) %>%
    dplyr::rename(IDorig = continent,
                  agb_sd = agb) %>%
    dplyr::inner_join(growth_para_v1, by = "IDorig") %>%
    dplyr::select(-reg_code:-reg_gez_luc, -TOF_luc) %>%
    replace(., is.na(.), 1) # Replace  NaN for no pixels in category!
  head(agb_sdv1)
  
  agb_maxv0 <- terra::extract(agb4stats_rcr, luc_poly, fun='max', na.rm = TRUE, bind=TRUE)
  agb_maxv1 <- agb_maxv0 %>%
    as.data.frame() %>%
    round(.,0) %>%
    dplyr::rename(IDorig = continent,
                  agb_max = agb) %>%
    dplyr::inner_join(growth_para_v1, by = "IDorig") %>%
    dplyr::select(-reg_code:-luc_cat) %>%
    replace(., is.na(.), 1) %>%
    mutate(agb_max = replace(agb_max, agb_max == -Inf, 1))
  head(agb_maxv1)
  
  growth_parameters_v0 <- agb_mean_decil %>%
    dplyr::inner_join(agb_sd_decil, by = "IDorig") %>%
    dplyr::inner_join(agb_n_decil, by = "IDorig") %>%
    dplyr::inner_join(agb_meanv1, by = "IDorig") %>%
    dplyr::inner_join(agb_sdv1, by = "IDorig") %>%
    dplyr::inner_join(agb_maxv1, by = "IDorig")
  head(growth_parameters_v0)
  
  # crosscheck n in T decil
  sum(growth_parameters_v0$agb_n_Tdecil)
  
  
  # Join with IPCC values and derive growth rates ----
  ipcc_growth_and_stock_2019 <- read_excel(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables/ipcc_growth_and_stock_2019.xlsx")) %>% # Eventually read from excel
    dplyr::select(-obs)
  
  growth_parameters_v1 <- growth_parameters_v0 %>% 
    dplyr::inner_join(ipcc_growth_and_stock_2019, by = "reg_gez") %>%
    dplyr::mutate(TOF = case_when(agb_mean_Tdecil == 1 ~ 1,
                                  agb_sd_Tdecil == 1 ~ 1,
                                  agb_n_Tdecil == 0 ~ 1,
                                  agb_mean_Tdecil > 0 ~ pmax(TOF_luc, TOF_gez))) %>%
    rowwise() %>%
    dplyr::mutate(icamax = case_when(TOF == 0 ~ max(c_across(c(ica_l20y, ica_m20y, ica_primary)), na.rm = TRUE),
                                     TOF == 1  ~ NA)) %>%
    dplyr::mutate(icamaxSD = case_when(TOF == 0 ~ max(c_across(c(icaSD_l20y, icaSD_m20y, icaSD_primary)), na.rm = TRUE),
                                       TOF == 1  ~ NA)) %>%
    ungroup() %>%
    as.data.frame() %>%
    dplyr::mutate(r = case_when(TOF == 0  ~ round((icamax * 4 / agb_mean_Tdecil),2),
                                TOF == 1  ~ 0)) %>%
    dplyr::mutate(rSD = case_when(TOF == 0  ~ round((icamaxSD * 4 / agb_mean_Tdecil),2),
                                  TOF == 1  ~ 0)) %>%
    dplyr::mutate(K = case_when(TOF == 0  ~ agb_mean_Tdecil,
                                TOF == 1  ~ agb_mean)) %>%
    dplyr::mutate(KSD = case_when(TOF == 0  ~ agb_sd_Tdecil,
                                  TOF == 1  ~ agb_sd)) %>%
    dplyr::relocate(TOF, .after=KSD) %>%
    arrange(IDorig)
  
  # Use this code to revise how AGB stats and IPCC values are integrated into MoFuSS growth curves.
  # Specify the range of columns
  columns_range <- 11:22
  
  # Filter rows where all specified columns are NA
  rows_with_all_na <- growth_parameters_v1[rowSums(is.na(growth_parameters_v1[, columns_range])) == length(columns_range), ]
  
  # Print the filtered rows
  print(rows_with_all_na)
  # When weighting by ncell, then whacky values are really outsiders with few pixels. Up grading to better versions of AGB maps will make this tables 
  # more robust. Is important to filter MoFuSS broke parameters such as r >>> k or K below the threshold. Check with India bkup.
  
  growth_parameters_v2 <- growth_parameters_v1 %>%
    dplyr::mutate(Key = row_number()) %>%
    dplyr::relocate(Key, .before = IDorig)
  
  if (lucinputdataset == "modis") {
    
    rcl_modis <- growth_parameters_v2 %>% 
      dplyr::select(IDorig, Key) %>% 
      mutate(across(
        .cols = matches('IDorig'),
        .fns = ~ as.integer(.x))) %>%
      as.matrix() %>%
      unname()
    
    lucmodis_2010_merge_rcl <- lucmodis_2010_merge %>%
      terra::classify(rcl_modis, include.lowest = FALSE, right = NA)
    terra::writeRaster(lucmodis_2010_merge_rcl, paste0("out_pcs/pre2010_v1_",LULCt1map_name),
                       filetype = "GTiff", overwrite = TRUE) #Watch out as the year is still hardwired here
    
    growth_parameters_v3b <- growth_parameters_v2 %>%
      dplyr::select(c("Key",
                      "reg_gez_luc",
                      "r",
                      "rSD",
                      "K",
                      "KSD",
                      "TOF")) %>%
      dplyr::rename("Key*" = "Key",
                    "LULC" = "reg_gez_luc", 
                    "rmax" = "r", # Consider using only r, as rmax is misleading, it would be ICAmax
                    "rmaxSD" = "rSD")
    
    growth_parameters_v3 <- growth_parameters_v3b
    # growth_parameters_v3 <- growth_parameters_v3b %>%
    #   mutate(
    #     rmax = if_else(rmax / K > 0.04, 0, rmax),
    #     rmaxSD = if_else(rmax / K > 0.04, 0, rmaxSD),
    #     TOF = if_else(rmax / K > 0.04, 1, TOF)
    #   )
    
    write.csv(growth_parameters_v3, "out_pcs/growth_parameters_v3_modis.csv", row.names=FALSE, quote=FALSE)
    
    
  } else if (lucinputdataset == "copernicus") {
    
    rcl_copernicus <- growth_parameters_v2 %>%
      dplyr::select(IDorig, Key) %>%
      mutate(across(
        .cols = matches('IDorig'),
        .fns = ~ as.integer(.x))) %>%
      as.matrix() %>%
      unname()
    
    
    luccopernicus_2015_merge_rcl <- luccopernicus_2015_merge %>%
      terra::classify(rcl_copernicus, include.lowest = FALSE, right = NA)
    terra::writeRaster(luccopernicus_2015_merge_rcl, paste0("out_pcs/pre2015_v1_",LULCt2map_name),
                       filetype = "GTiff", overwrite = TRUE) # Watch out as the year is still hardwired here
    
    
    growth_parameters_v3b <- growth_parameters_v2 %>%
      dplyr::select(c("Key",
                      "reg_gez_luc",
                      "r",
                      "rSD",
                      "K",
                      "KSD",
                      "TOF")) %>%
      dplyr::rename("Key*" = "Key",
                    "LULC" = "reg_gez_luc",
                    "rmax" = "r", # Consider using only r, as rmax is misleading, it would be ICAmax
                    "rmaxSD" = "rSD")
    
    growth_parameters_v3 <- growth_parameters_v3b
    # growth_parameters_v3 <- growth_parameters_v3b %>%
    #   mutate(
    #     rmax = if_else(rmax / K > 0.04, 0, rmax),
    #     rmaxSD = if_else(rmax / K > 0.04, 0, rmaxSD),
    #     TOF = if_else(rmax / K > 0.04, 1, TOF)
    #   )
    
    write.csv(growth_parameters_v3, "out_pcs/growth_parameters_v3_copernicus.csv", row.names=FALSE, quote=FALSE)
    
  }
  
}

# Copy 2 MoFuSS ----
copy2mofussfiles1 <- list.files(path = paste0(lulccfiles,"/out_gcs/"),
                                pattern = ".*\\.tif$", full.names = TRUE)
for (f1 in copy2mofussfiles1) {
  file.copy(from=f1,
            to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster_GCS/"),
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

copy2mofussfiles2 <- list.files(path = paste0(lulccfiles,"/out_pcs/"),
                                pattern = ".*\\.tif$", full.names = TRUE)
for (f2 in copy2mofussfiles2) {
  file.copy(from=f2,
            to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/"),
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

copy2mofussfiles3 <- list.files(path = paste0(lulccfiles,"/out_pcs/"),
                                pattern = ".*\\.csv$", full.names = TRUE)
for (f3 in copy2mofussfiles3) {
  file.copy(from=f3,
            to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables/"),
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}


# Plot random pixels growth curves / start from dataset ----
if (plot_curves == 1) {
  
  #set number of growth curves
  ncurves <- 5
  
  # Filter out rows
  sampled_data <- growth_parameters_v1 %>%
    filter(r != -Inf) %>%
    filter(agb_n_Tdecil >= 30) %>% 
    filter(TOF == 0) %>% 
    group_by(reg_gez) %>%
    sample_n(1) %>%
    ungroup()
  
  # Randomly select 5 different ecozones, if there are more than 5 ecozones
  final_sample <- sampled_data %>%
    sample_n(min(ncurves, n())) %>%
    as.data.frame() %>%
    dplyr::select(IDorig, reg_gez_luc, agb_mean, r, rSD, K, KSD)
  
  # Display the final sample
  final_sample$color <- sapply(1:ncurves, function(x) rgb(runif(1), runif(1), runif(1)))
  final_sample
  
  ## Tests some growth functions PART 1 ----
  
  # Define the growth function
  growth_function1 <- function(agbt0, r, K, t) {
    agbt <- numeric(length = length(t))
    agbt[1] <- agbt0
    for (i in 2:length(t)) {
      agbt[i] <- agbt[i-1] + agbt[i-1] * r * (1 - agbt[i-1] / K)
    }
    return(agbt)
  }
  
  # Time sequence
  t <- 2010:2050
  
  # Data frame to store results
  results <- data.frame()
  
  # Calculate growth for each scenario
  for (i in 1:nrow(final_sample)) {
    agbt <- growth_function1(final_sample$agb_mean[i], final_sample$r[i], final_sample$K[i], t)
    scenario_data <- data.frame(Time = t, AGBT = agbt, Scenario = final_sample$reg_gez_luc[i])
    results <- rbind(results, scenario_data)
  }
  
  # Plot
  ggplot(results, aes(x = Time, y = AGBT, color = Scenario)) +
    geom_line() +
    scale_color_manual(values = final_sample$color) +
    labs(title = paste0("Forest Growth over Time for ",ncurves," randomly selected con+gez+luc categories"), 
         subtitle = "AGB at year 2010 is represened by the mean value of the category in 2010", 
         caption = "con = Continent; gez = Ecological Zone; \ 
       luc = Modis land use/cover type 1 category",
         x = "Time (t)", y = "Aboveground Biomass (AGB)", color = "Scenario") +
    theme_minimal()
  
  ## Tests some growth functions PART 2 ----
  
  # Define the growth function
  growth_function2 <- function(agbt0, r, K, t) {
    agbt <- numeric(length = length(t))
    agbt[1] <- agbt0
    for (i in 2:length(t)) {
      agbt[i] <- agbt[i-1] + agbt[i-1] * r * (1 - agbt[i-1] / K)
    }
    return(agbt)
  }
  
  # Time sequence
  t <- 2010:2050
  
  # Number of simulations per scenario
  num_simulations <- 10
  
  # Prepare a dataframe to hold all simulation results
  simulation_results <- data.frame()
  
  # Simulate growth for each scenario
  for (i in 1:nrow(final_sample)) {
    for (sim in 1:num_simulations) {
      # Generate r and K values based on their means and SDs using truncated normal distribution
      r_sim <- rtruncnorm(1, a = 0, b = Inf, mean = final_sample$r[i], sd = final_sample$rSD[i])
      K_sim <- rtruncnorm(1, a = 0, b = Inf, mean = final_sample$K[i], sd = final_sample$KSD[i])
      
      # Simulate growth
      agbt_sim <- growth_function2(final_sample$agb_mean[i], r_sim, K_sim, t)
      sim_data <- data.frame(Time = t, AGBT = agbt_sim, Scenario = final_sample$reg_gez_luc[i], Simulation = sim)
      simulation_results <- rbind(simulation_results, sim_data)
    }
  }
  
  # Calculate mean and sd for AGBT at each time point for each scenario
  library(dplyr)
  summary_results <- simulation_results %>%
    group_by(Time, Scenario) %>%
    summarise(MeanAGBT = mean(AGBT), SdAGBT = sd(AGBT), .groups = 'drop')
  
  # Plot with uncertainty
  ggplot(summary_results, aes(x = Time, y = MeanAGBT, group = Scenario, color = Scenario)) +
    geom_line() +
    geom_ribbon(aes(ymin = MeanAGBT - SdAGBT, ymax = MeanAGBT + SdAGBT, fill = Scenario), alpha = 0.3) +
    scale_color_manual(values = final_sample$color) +
    labs(title = "Forest Growth over Time with Uncertainty (100 Monte Carlo simulations)", x = "Time (t)", y = "Aboveground Biomass (AGBT)", color = "Scenario") +
    theme_minimal()
  
}

