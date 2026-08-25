# SPDX-License-Identifier: Apache-2.0
#
# Copyright 2025-2027 Universidad Nacional Autónoma de México
# and Stockholm Environment Institute
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# https://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# MoFuSS ----
# Script: 4_produce_growth_and_stock_csv_v2.R
# Version: 2
# Date: Aug 2026
# Execution: Source from RStudio; Dinamica EGO does not invoke this script directly.
#
# Purpose: Validate and preserve the forest-growth, stock and trees-outside-
# forests parameters produced by preprocessing without silently rewriting them.
# Inputs: Preprocessed growth/stock objects and rasters plus inherited workspace paths.
# Outputs: Validated growth and stock CSV inputs for downstream harmonization/modeling.
# Side effects: Clears the configured Terra temporary directory and overwrites
# generated parameter tables.

# 2dolist ----

# Internal parameters ----
temdirdefined = 1
forced_urban_parameter_digits <- 4L

# Load libraries ----
library(conflicted)

library(terra)
# terraOptions(steps = 55)
if (temdirdefined == 1) {
  # This script and the harmonizer are sourced into one R session. Keep
  # headroom for GDAL and for objects created by the preceding demand scripts.
  terraOptions(tempdir = rTempdir, memfrac = 0.5)
  # List all files and directories inside the folder
  contents <- list.files(rTempdir, full.names = TRUE, recursive = TRUE)
  # Delete the contents but keep the folder
  unlink(contents, recursive = TRUE, force = TRUE)
}
# terraOptions(progress=0)
library(dplyr)
library(readr)
library(readxl)
library(svDialogs)
library(tidyverse)

validate_growth_parameters <- function(data, label) {
  required <- c("Key*", "LULC", "rmax", "rmaxSD", "K", "KSD", "TOF")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(label, " is missing columns: ", paste(missing, collapse = ", "))
  }
  numeric_columns <- c("Key*", "rmax", "rmaxSD", "K", "KSD", "TOF")
  non_numeric <- numeric_columns[!vapply(
    data[numeric_columns], is.numeric, logical(1)
  )]
  if (length(non_numeric) > 0) {
    stop(label, " has non-numeric columns: ", paste(non_numeric, collapse = ", "))
  }
  key_values <- data[["Key*"]]
  invalid_keys <- anyNA(key_values) || any(!is.finite(key_values)) ||
    any(key_values < 1) || any(key_values > .Machine$integer.max) ||
    any(key_values != floor(key_values))
  invalid <- data %>%
    dplyr::filter(
      !is.finite(`Key*`) | !is.finite(rmax) | !is.finite(rmaxSD) |
        !is.finite(K) | !is.finite(KSD) | !is.finite(TOF) |
        rmax < 0 | rmaxSD < 0 | KSD < 0 |
        !TOF %in% c(0, 1) |
        (TOF == 0 & K <= 0) | (TOF == 1 & K < 0) |
        (TOF == 1 & (rmax != 0 | rmaxSD != 0))
    )
  if (invalid_keys || anyDuplicated(data$`Key*`) || nrow(invalid) > 0) {
    stop(
      label, " contains duplicate keys or invalid growth/TOF parameters."
    )
  }
  invisible(data)
}

native_urban_label <- function(dataset) {
  switch(
    tolower(dataset),
    modis = "Urban and Built-up Lands",
    copernicus = "Urban built up",
    stop("Unsupported LULC dataset: ", dataset)
  )
}

assert_current_tof_policy <- function(data, dataset) {
  urban_suffix <- paste0("_", native_urban_label(dataset))
  urban <- data[endsWith(as.character(data$LULC), urban_suffix), ]
  water_pattern <- switch(
    tolower(dataset),
    modis = "_Water Bodies$",
    copernicus = "_(Permanent water bodies|Oceans seas)$"
  )
  water <- data[grepl(water_pattern, as.character(data$LULC)), ]
  legacy_urban <- nrow(urban) > 0 && all(
    abs(urban$K - 2) < 1e-12 & abs(urban$KSD - 2) < 1e-12
  )
  legacy_water <- nrow(water) > 0 && all(
    abs(water$K - 10) < 1e-12 & abs(water$KSD - 10) < 1e-12
  )
  if (legacy_urban || legacy_water) {
    stop(
      toupper(dataset),
      " growth parameters still use the legacy fixed Urban/Water TOF policy. ",
      "Regenerate the growth table and its matching classified raster with ",
      "7pre_lulcc_v6.R before running the country pipeline."
    )
  }
  invisible(data)
}

derive_forced_urban_parameters <- function(
  growth_parameters, base_key_raster, forced_urban_mask, dataset
) {
  urban_suffix <- paste0("_", native_urban_label(dataset))
  parameter_rows <- growth_parameters %>%
    dplyr::transmute(
      base_key = as.numeric(`Key*`),
      zone = sub("_[^_]+$", "", as.character(LULC)),
      K = as.numeric(K), KSD = as.numeric(KSD), TOF = as.numeric(TOF),
      is_native_urban = endsWith(as.character(LULC), urban_suffix)
    )
  urban_rows <- parameter_rows %>%
    dplyr::filter(is_native_urban, TOF == 1, is.finite(K), is.finite(KSD)) %>%
    dplyr::select(zone, urban_K = K, urban_KSD = KSD)
  if (!nrow(urban_rows) || anyDuplicated(urban_rows$zone)) {
    stop(
      toupper(dataset),
      " needs exactly one valid native Urban parameter row per represented zone."
    )
  }

  positive_cv <- with(
    urban_rows[urban_rows$urban_K > 0, ],
    urban_KSD / urban_K
  )
  positive_cv <- positive_cv[is.finite(positive_cv) & positive_cv >= 0]
  if (length(positive_cv)) {
    urban_cv <- stats::median(positive_cv)
  } else if (all(urban_rows$urban_K == 0 & urban_rows$urban_KSD == 0)) {
    urban_cv <- 0
  } else {
    stop(toupper(dataset), " native Urban rows do not define a usable TOF CV.")
  }

  forced_keys <- terra::ifel(forced_urban_mask, base_key_raster, NA)
  forced_frequency <- as.data.frame(terra::freq(forced_keys))
  if (!nrow(forced_frequency)) {
    fallback_K <- mean(urban_rows$urban_K)
    message(
      toupper(dataset),
      ": no forced-urban pixels were present; Urban_Forced uses the unweighted ",
      "mean of native Urban annual supplies."
    )
    return(list(
      K = round(fallback_K, forced_urban_parameter_digits),
      KSD = round(fallback_K * urban_cv, forced_urban_parameter_digits),
      matched_fraction = NA_real_
    ))
  }
  if (!all(c("value", "count") %in% names(forced_frequency))) {
    names(forced_frequency)[(ncol(forced_frequency) - 1L):ncol(forced_frequency)] <-
      c("value", "count")
  }

  forced_by_zone <- forced_frequency %>%
    dplyr::transmute(
      base_key = as.numeric(value), pixel_count = as.numeric(count)
    ) %>%
    dplyr::left_join(
      parameter_rows %>% dplyr::select(base_key, zone),
      by = "base_key"
    ) %>%
    dplyr::left_join(urban_rows, by = "zone")
  if (any(!is.finite(forced_by_zone$pixel_count)) ||
      sum(forced_by_zone$pixel_count) <= 0) {
    stop(toupper(dataset), " forced-urban footprint has invalid pixel counts.")
  }

  matched <- is.finite(forced_by_zone$urban_K)
  matched_fraction <- sum(forced_by_zone$pixel_count[matched]) /
    sum(forced_by_zone$pixel_count)
  if (any(matched)) {
    fallback_K <- weighted.mean(
      forced_by_zone$urban_K[matched], forced_by_zone$pixel_count[matched]
    )
  } else {
    fallback_K <- mean(urban_rows$urban_K)
  }
  if (any(!matched)) {
    warning(
      sprintf(
        "%s: %.2f%% of forced-urban pixels lack a native Urban row for their zone; using the matched-footprint mean for those pixels.",
        toupper(dataset), 100 * (1 - matched_fraction)
      ),
      call. = FALSE
    )
    forced_by_zone$urban_K[!matched] <- fallback_K
  }

  forced_K <- weighted.mean(
    forced_by_zone$urban_K, forced_by_zone$pixel_count
  )
  list(
    K = round(forced_K, forced_urban_parameter_digits),
    KSD = round(forced_K * urban_cv, forced_urban_parameter_digits),
    matched_fraction = matched_fraction
  )
}

setwd(countrydir)
getwd()
country_name

# Read parameters table ----
if (webmofuss == 1) {
  # Read parameters table in webmofuss
  country_parameters <- read_csv(parameters_file_path)
} else if(webmofuss == 0) {
  # Read parameters table (recognizing the delimiter)
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
}

# # Specify the directory where the file is located
# parameters_directory <- paste0(getwd(),"/LULCC/DownloadedDatasets/SourceData",country_name)
# 
# # Use list.files() to find the file that matches the pattern
# parameters_name <- list.files(path = parameters_directory, pattern = "^parameters.*\\.xlsx$", full.names = TRUE)
# 
# # Read parameters table ----
# if (webmofuss == 1){
#   country_parameters <- read_csv(paste0("LULCC/DownloadedDatasets/SourceData",country_name,"/",parameters_file))
# } else if (webmofuss == 0){
#   country_parameters <- read_excel(paste0("LULCC/DownloadedDatasets/SourceData",country_name,"/",parameters_file))
# }

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

# if (exists("lulccfiles") == FALSE) {
#   choose_directory661 = function(caption = "Choose the directory where land use/cover files are") {
#     if(.Platform$OS.type == "unix")  {
#       setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
#     } else {
#       setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
#     }
#   }
# choose_directory661()
# lulccfiles <- getwd()
# }

# Copy 2 MoFuSS ----
# copy2mofussfiles1 <- list.files(path = paste0(lulccfiles,"/out_gcs/"),
#                                 pattern = ".*\\.tif$", full.names = TRUE)
# for (f1 in copy2mofussfiles1) {
#   file.copy(from=f1,
#             to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster_GCS/"),
#             overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
# }
# 
# copy2mofussfiles2 <- list.files(path = paste0(lulccfiles,"/out_pcs/"),
#                                 pattern = ".*\\.tif$", full.names = TRUE)
# for (f2 in copy2mofussfiles2) {
#   file.copy(from=f2,
#             to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/"),
#             overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
# }
# 
# copy2mofussfiles3 <- list.files(path = paste0(lulccfiles,"/out_pcs/"),
#                                 pattern = ".*\\.csv$", full.names = TRUE)
# for (f3 in copy2mofussfiles3) {
#   file.copy(from=f3,
#             to=paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables/"),
#             overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
# }

if (LULCt1map == "YES" & LULCt2map == "YES"){
  lucavailablemaps <- c("modis", "copernicus")
} else if (LULCt1map == "YES" & LULCt2map != "YES"){
  lucavailablemaps <- c("modis")
} else if (LULCt1map != "YES" & LULCt2map == "YES"){
  lucavailablemaps <- c("copernicus")
}
lucavailablemaps

for (lucinputdataset in lucavailablemaps) {
# lucinputdataset = "copernicus"
# lucinputdataset = "modis"
setwd(countrydir)

# Prepare the rural urban mask ----
if (lucinputdataset == "modis") {
  
  country_parameters %>%
    dplyr::filter(Var == "LULCt1map_name") %>%
    pull(ParCHR) -> LULCt1map_name
  country_parameters %>%
    dplyr::filter(Var == "LULCt1map_yr") %>%
    pull(ParCHR) %>%
    as.integer(.) -> LULCt1map_yr
  lucmodis_2001_merge_rcl <- rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/pre",LULCt1map_yr,"_v1_",LULCt1map_name))

  rururb_gcs <- rast(paste0(demanddir,"/pop_out/WorldPop_rururbR_2020.tif"))
  rururb_pcs <- rururb_gcs %>% 
    terra::project(lucmodis_2001_merge_rcl, method="near", gdal=TRUE)
  # terra::writeRaster(rururb_pcs, paste0(lulccfiles,"/out_pcs/rururb_pcs.tif"), filetype = "GTiff", datatype="INT2S", overwrite = TRUE)
  
  # Reads growth parameters correctly
  # Define the file path
  file_pathm <- paste0(countrydir, "/LULCC/DownloadedDatasets/SourceData", country_name, "/InTables/growth_parameters_v3_modis.csv")
  
  # Check the first line of the file to determine the delimiter
  first_linem <- readLines(file_pathm, n = 1)
  
  # Determine the delimiter based on the first line
  delimiterm <- ifelse(grepl(";", first_linem), ";", ",")
  
  # Read and validate without changing parameters produced upstream.
  growth_parameters_v3_modis <- read_delim(file_pathm, delim = delimiterm)
  validate_growth_parameters(
    growth_parameters_v3_modis, "MODIS growth-parameter table"
  )
  assert_current_tof_policy(growth_parameters_v3_modis, "modis")
  
  lastid <- max(as.integer(growth_parameters_v3_modis[["Key*"]])) + 1L
  rururb_rcl <- data.frame(c(1,2),c(NA,lastid)) %>%
    as.matrix(.,nrow = 2, ncol = 2) %>%
    unname()
  rururb_pcs_rcl <- rururb_pcs %>%
    terra::classify(rururb_rcl, include.lowest = FALSE, right = NA)
    # terra::writeRaster(rururb_pcs_rcl, paste0(lulccfiles,"/out_pcs/rururb_rcl.tif"),
    #                  filetype = "GTiff", overwrite = TRUE)
  
  mask_urbanforced <- !is.na(rururb_pcs_rcl)
  forced_urban_parameters <- derive_forced_urban_parameters(
    growth_parameters_v3_modis,
    lucmodis_2001_merge_rcl,
    mask_urbanforced,
    "modis"
  )
  lucmodis_2001_final <- ifel(mask_urbanforced, rururb_pcs_rcl, lucmodis_2001_merge_rcl)
  
  # terra::writeRaster(lucmodis_2010_final, paste0(lulccfiles,"/out_pcs/rururb_rcl2.tif"),
  #                    filetype = "GTiff", overwrite = TRUE)
  terra::writeRaster(lucmodis_2001_final, paste0(countrydir,"/LULCC/DownloadedDatasets/SourceDataGlobal/InRaster/",LULCt1map_name), 
                     filetype = "GTiff", overwrite = TRUE)
  
  growth_parameters_v4 <- growth_parameters_v3_modis %>%
    add_row(tibble_row(
      `Key*` = lastid, LULC = "Urban_Forced", rmax = 0, rmaxSD = 0,
      K = forced_urban_parameters$K,
      KSD = forced_urban_parameters$KSD,
      TOF = 1
    ))
  validate_growth_parameters(growth_parameters_v4, "Final MODIS growth-parameter table")
  str(growth_parameters_v4)
  write.csv(growth_parameters_v4, paste0(countrydir,"/LULCC/DownloadedDatasets/SourceDataGlobal/InTables/growth_parameters1.csv"), row.names=FALSE, quote=FALSE)
  write.csv(growth_parameters_v4, paste0(countrydir,"/LULCC/TempTables/growth_parameters1.csv"), row.names=FALSE, quote=FALSE)
  tail(growth_parameters_v4)

  # These rasters use the 453-million-cell global grid and are not consumed as
  # live objects downstream; the harmonizer reopens the written products.
  # Release them now instead of carrying several GB into AGB processing.
  rm(
    lucmodis_2001_merge_rcl, rururb_gcs, rururb_pcs, rururb_pcs_rcl,
    mask_urbanforced, lucmodis_2001_final
  )
  invisible(gc())
  
  } else if (lucinputdataset == "copernicus") {
    
  country_parameters %>%
    dplyr::filter(Var == "LULCt2map_name") %>%
    pull(ParCHR) -> LULCt2map_name
  country_parameters %>%
      dplyr::filter(Var == "LULCt2map_yr") %>%
      pull(ParCHR) %>%
      as.integer(.) -> LULCt2map_yr
  luccopernicus_2015_merge_rcl <- rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/pre",LULCt2map_yr,"_v1_",LULCt2map_name))

  rururb_gcs <- rast(paste0(demanddir,"/pop_out/WorldPop_rururbR_2020.tif"))
  rururb_pcs <- rururb_gcs %>% 
    terra::project(luccopernicus_2015_merge_rcl, method="near", gdal=TRUE)
  # terra::writeRaster(rururb_pcs, paste0(lulccfiles,"/out_pcs/rururb_pcs.tif"), filetype = "GTiff", datatype="INT2S", overwrite = TRUE)
  
  # Reads growth parameters correctly
  # Define the file path
  file_pathc <- paste0(countrydir, "/LULCC/DownloadedDatasets/SourceData", country_name, "/InTables/growth_parameters_v3_copernicus.csv")
  
  # Check the first line of the file to determine the delimiter
  first_linec <- readLines(file_pathc, n = 1)
  
  # Determine the delimiter based on the first line
  delimiterc <- ifelse(grepl(";", first_linec), ";", ",")
  
  # Read and validate without changing parameters produced upstream.
  growth_parameters_v3_copernicus <- read_delim(file_pathc, delim = delimiterc)
  validate_growth_parameters(
    growth_parameters_v3_copernicus, "Copernicus growth-parameter table"
  )
  assert_current_tof_policy(growth_parameters_v3_copernicus, "copernicus")
  
  lastid <- max(as.integer(growth_parameters_v3_copernicus[["Key*"]])) + 1L
  rururb_rcl <- data.frame(c(1,2),c(NA,lastid)) %>%
    as.matrix(.,nrow = 2, ncol = 2) %>%
    unname()
  rururb_pcs_rcl <- rururb_pcs %>%
    terra::classify(rururb_rcl, include.lowest = FALSE, right = NA)
  # terra::writeRaster(rururb_pcs_rcl, paste0(lulccfiles,"/out_pcs/rururb_rcl.tif"),
  #                    filetype = "GTiff", overwrite = TRUE)
  
  mask_urbanforced <- !is.na(rururb_pcs_rcl)
  forced_urban_parameters <- derive_forced_urban_parameters(
    growth_parameters_v3_copernicus,
    luccopernicus_2015_merge_rcl,
    mask_urbanforced,
    "copernicus"
  )
  luccopernicus_2015_final <- ifel(mask_urbanforced, rururb_pcs_rcl, luccopernicus_2015_merge_rcl)
  
  # terra::writeRaster(luccopernicus_2010_final, paste0(lulccfiles,"/out_pcs/rururb_rcl2.tif"),
  #                    filetype = "GTiff", overwrite = TRUE)
  terra::writeRaster(luccopernicus_2015_final, paste0(countrydir,"/LULCC/DownloadedDatasets/SourceDataGlobal/InRaster/",LULCt2map_name), # Double check in harmonizer
                     filetype = "GTiff", overwrite = TRUE)
  
  growth_parameters_v4 <- growth_parameters_v3_copernicus %>%
    add_row(tibble_row(
      `Key*` = lastid, LULC = "Urban_Forced", rmax = 0, rmaxSD = 0,
      K = forced_urban_parameters$K,
      KSD = forced_urban_parameters$KSD,
      TOF = 1
    ))
  validate_growth_parameters(
    growth_parameters_v4, "Final Copernicus growth-parameter table"
  )
  str(growth_parameters_v4)
  write.csv(growth_parameters_v4, paste0(countrydir,"/LULCC/DownloadedDatasets/SourceDataGlobal/InTables/growth_parameters2.csv"), row.names=FALSE, quote=FALSE)
  write.csv(growth_parameters_v4, paste0(countrydir,"/LULCC/TempTables/growth_parameters2.csv"), row.names=FALSE, quote=FALSE)
  tail(growth_parameters_v4)

  rm(
    luccopernicus_2015_merge_rcl, rururb_gcs, rururb_pcs, rururb_pcs_rcl,
    mask_urbanforced, luccopernicus_2015_final
  )
  invisible(gc())
  
  }

}

# End of script ----
