# SPDX-License-Identifier: Apache-2.0
# Woodman annual LULC, TOF and forest-transition maps on the country grid.
# Source after 5_harmonizer_v8.R, which defines userarea_r and the static
# LULCt1_c.tif map. This script runs only when LULCt1map_dataset=woodman.

library(dplyr)
library(terra)

woodman_parameter <- function(name, default = NULL) {
  value <- country_parameters %>%
    dplyr::filter(Var == name) %>% pull(ParCHR)
  if (!length(value)) return(default)
  if (length(value) != 1L || is.na(value[[1L]]) ||
      !nzchar(trimws(value[[1L]]))) {
    stop("Invalid Woodman parameter: ", name)
  }
  trimws(as.character(value[[1L]]))
}

if (tolower(woodman_parameter("LULCt1map_dataset", "modis")) == "woodman") {
  if (!exists("userarea_r", inherits = TRUE) ||
      !exists("align_raster_to_template", mode = "function")) {
    stop("Run 5_harmonizer_v8.R before Woodman annual harmonization.")
  }
  series_dir <- woodman_parameter("woodman_series_dir")
  if (grepl("[/\\\\]", series_dir) || series_dir %in% c(".", "..")) {
    stop("woodman_series_dir must be a single directory name.")
  }
  start_year <- as.integer(woodman_parameter("start_year"))
  end_year <- as.integer(woodman_parameter("end_year"))
  if (is.na(start_year) || is.na(end_year) || start_year != 2000L ||
      end_year > 2050L || end_year < start_year) {
    stop("Woodman annual maps require start_year=2000 and end_year<=2050.")
  }

  source_data <- file.path(
    countrydir, "LULCC", "DownloadedDatasets", paste0("SourceData", country_name)
  )
  global_data <- file.path(
    countrydir, "LULCC", "DownloadedDatasets", "SourceDataGlobal"
  )
  zone_path <- file.path(global_data, "InRaster", "woodman_zone_pcs.tif")
  key_path <- file.path(source_data, "InTables", "woodman_key_crosswalk.csv")
  growth_path <- file.path(countrydir, "LULCC", "TempTables", "growth_parameters1.csv")
  base_path <- file.path(countrydir, "LULCC", "TempRaster", "LULCt1_c.tif")
  required <- c(zone_path, key_path, growth_path, base_path)
  missing <- required[!file.exists(required)]
  if (length(missing)) stop("Missing Woodman prerequisite: ", missing[[1L]])

  keys <- read.csv(key_path, stringsAsFactors = FALSE)
  growth <- read.csv(growth_path, check.names = FALSE, stringsAsFactors = FALSE)
  if (!all(c("IDorig", "Key", "TOF", "luc_code") %in% names(keys)) ||
      !all(c("Key*", "LULC", "TOF") %in% names(growth)) ||
      anyDuplicated(keys$IDorig) || anyDuplicated(keys$Key)) {
    stop("Woodman key crosswalk or growth table is incomplete or duplicated.")
  }
  forced <- growth[growth$LULC == "Urban_Forced", ]
  if (nrow(forced) != 1L || forced$TOF[[1L]] != 1L) {
    stop("Exactly one Urban_Forced TOF growth row is required.")
  }
  forced_key <- as.integer(forced[["Key*"]][[1L]])
  base_key <- terra::rast(base_path)
  forced_mask <- base_key == forced_key
  zone <- align_raster_to_template(
    terra::rast(zone_path), userarea_r, method = "near"
  )
  key_matrix <- as.matrix(keys[, c("IDorig", "Key")])
  tof_matrix <- as.matrix(growth[, c("Key*", "TOF")])
  forest_keys <- as.integer(keys$Key[keys$luc_code %in% c(44L, 45L)])
  previous_forest <- NULL
  output_dir <- file.path(countrydir, "LULCC", "TempRaster")

  for (year in 2000:end_year) {
    annual_path <- file.path(
      global_data, "InRaster", series_dir,
      sprintf("woodman_luc_%d_gcs.tif", year)
    )
    if (!file.exists(annual_path)) {
      stop("Missing Woodman annual map: ", annual_path)
    }
    luc <- align_raster_to_template(
      terra::rast(annual_path), userarea_r, method = "near"
    )
    combined <- zone + luc
    annual_key <- terra::classify(
      combined, key_matrix, right = NA, others = NA
    )
    unresolved <- terra::ifel(
      !is.na(luc) & !is.na(zone) & is.na(annual_key), 1, NA
    )
    unresolved_n <- terra::global(unresolved, "sum", na.rm = TRUE)[[1L, 1L]]
    if (is.finite(unresolved_n) && unresolved_n > 0) {
      stop(year, " has ", unresolved_n,
           " Woodman cells without growth-parameter keys.")
    }
    # Year 2000 must match the calibrated initial-stock map exactly. In later
    # years the static forced-urban footprint overrides Woodman transitions.
    key <- if (year == 2000L) base_key else
      terra::ifel(forced_mask, forced_key, annual_key)
    tof <- terra::classify(key, tof_matrix, right = NA, others = NA)
    forest <- terra::ifel(
      is.na(key), NA, terra::ifel(key %in% forest_keys, 1, 0)
    )
    if (is.null(previous_forest)) {
      transition <- terra::ifel(is.na(forest), NA, 0)
    } else {
      transition <- terra::ifel(
        is.na(forest) | is.na(previous_forest), NA,
        terra::ifel(previous_forest == 1 & forest == 0, 1,
                    terra::ifel(previous_forest == 0 & forest == 1, 2, 0))
      )
    }
    terra::writeRaster(
      key, file.path(output_dir, sprintf("LULCt1_c_%d.tif", year)),
      datatype = "INT2S", overwrite = TRUE,
      wopt = list(gdal = c("COMPRESS=LZW"))
    )
    terra::writeRaster(
      tof, file.path(output_dir, sprintf("TOFvsFOR_mask1_%d.tif", year)),
      datatype = "INT2S", overwrite = TRUE,
      wopt = list(gdal = c("COMPRESS=LZW"))
    )
    terra::writeRaster(
      transition,
      file.path(output_dir, sprintf("WoodmanTransition_%d.tif", year)),
      datatype = "INT2S", overwrite = TRUE,
      wopt = list(gdal = c("COMPRESS=LZW"))
    )
    previous_forest <- forest
    message("Prepared Woodman LULC, TOF and transition maps for ", year)
  }
}
