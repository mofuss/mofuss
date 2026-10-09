# SPDX-License-Identifier: Apache-2.0
# Woodman annual LULC, TOF and forest-transition maps on the country grid.
# Source after 5_harmonizer_v8.R, which defines userarea_r and the static
# LULCt<channel>_c.tif map. LUC1 is MODIS and LUC3 is Woodman.

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

woodman_optional_parameter <- function(name, default) {
  value <- country_parameters$ParCHR[
    !is.na(country_parameters$Var) & country_parameters$Var == name
  ]
  if (!length(value)) return(default)
  if (length(value) != 1L) stop("Duplicate Woodman parameter: ", name)
  if (is.na(value[[1L]]) || !nzchar(trimws(value[[1L]]))) return(default)
  trimws(as.character(value[[1L]]))
}

modis_luc1 <- toupper(woodman_optional_parameter("LULCt1map", "NO")) == "YES"
woodman_luc3 <- toupper(woodman_optional_parameter("LULCt3map", "NO")) == "YES"

prepare_woodman_annual_maps <- function() {
  if (!woodman_luc3) return(invisible(NULL))
  woodman_slot <- 3L
  if (!exists("userarea_r", inherits = TRUE) ||
      !exists("align_raster_to_template", mode = "function")) {
    stop("Run 5_harmonizer_v8.R before Woodman annual harmonization.")
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
  growth_path <- file.path(
    countrydir, "LULCC", "TempTables",
    sprintf("growth_parameters%d.csv", woodman_slot)
  )
  base_path <- file.path(
    countrydir, "LULCC", "TempRaster",
    sprintf("LULCt%d_c.tif", woodman_slot)
  )
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
  # Raster values live outside R's managed heap. Several nested operations can
  # exhaust RAM before R decides to collect their external pointers. Keep this
  # stage on disk and bound each operation independently of the preceding scripts.
  prior_options <- terra::terraOptions(print = FALSE)[
    c("tempdir", "memfrac", "memmin", "memmax", "todisk")
  ]
  scratch <- tempfile("woodman_annual_", tmpdir = prior_options$tempdir)
  if (!dir.create(scratch, recursive = TRUE)) {
    stop("Could not create Woodman temporary directory: ", scratch)
  }
  on.exit({
    do.call(terra::terraOptions, prior_options)
    invisible(gc())
    unlink(scratch, recursive = TRUE)
  }, add = TRUE)
  terra::terraOptions(tempdir = scratch, memfrac = 0.25, memmax = 1,
                      memmin = 0, todisk = TRUE)
  invisible(gc())
  forced_key <- as.integer(forced[["Key*"]][[1L]])
  base_key <- terra::rast(base_path)
  forced_mask <- base_key == forced_key
  zone <- align_raster_to_template(
    terra::rast(zone_path), userarea_r, method = "near"
  )
  key_matrix <- as.matrix(keys[, c("IDorig", "Key")])
  tof_matrix <- as.matrix(growth[, c("Key*", "TOF")])
  forest_keys <- as.integer(keys$Key[keys$luc_code %in% c(44L, 45L)])
  annual_overwrite <- !exists("woodman_no_overwrite", inherits = TRUE) ||
    !isTRUE(get("woodman_no_overwrite", inherits = TRUE))
  output_dir <- file.path(countrydir, "LULCC", "TempRaster")

  years <- 2000:end_year
  pcs_paths <- file.path(
    global_data, "InRaster", sprintf("woodman_luc_%d_pcs.tif", years)
  )
  missing_pcs <- pcs_paths[!file.exists(pcs_paths)]
  if (length(missing_pcs)) {
    stop("Missing projected Woodman annual input: ", missing_pcs[[1L]],
         ". Publish v7 out_pcs to the input seed before country preparation.")
  }
  annual_paths <- pcs_paths
  message("Preparing Woodman LUC", woodman_slot, " annual maps from ",
          dirname(annual_paths[[1L]]))

  # v14 uses one annual filename contract for either selectable LUC channel.
  # MODIS is static, so its annual aliases and zero transitions preserve the
  # v13 supply behavior when the wizard's LUC selector is set to 1.
  if (modis_luc1) {
    modis_luc <- file.path(output_dir, "LULCt1_c.tif")
    modis_tof <- file.path(output_dir, "TOFvsFOR_mask1.tif")
    missing_modis <- c(modis_luc, modis_tof)[!file.exists(c(modis_luc, modis_tof))]
    if (length(missing_modis)) stop("Missing MODIS LUC1 map: ", missing_modis[[1L]])
    modis_zero_path <- file.path(scratch, "modis_zero.tif")
    terra::ifel(
      is.na(terra::rast(modis_luc)), NA, 0, filename = modis_zero_path,
      wopt = list(datatype = "INT2S", gdal = c("COMPRESS=LZW"))
    )
    for (year in years) {
      targets <- file.path(output_dir, c(
        sprintf("LULCt1_c_%d.tif", year),
        sprintf("TOFvsFOR_mask1_%d.tif", year),
        sprintf("LULCt1_transition_%d.tif", year)
      ))
      sources <- c(modis_luc, modis_tof, modis_zero_path)
      copied <- file.copy(sources, targets, overwrite = annual_overwrite,
                          copy.mode = TRUE)
      if (!all(copied)) stop("Could not prepare annual MODIS map: ",
                            targets[which(!copied)[[1L]]])
    }
    message("Prepared static MODIS LUC1/TOF aliases and zero transitions for ",
            length(years), " simulation years")
  }

  # A function scope releases all annual intermediates when it returns. Only
  # filenames for the previous forest/TOF masks survive into the next year.
  prepare_year <- function(i, previous) {
    year <- years[[i]]
    previous_forest <- if (is.null(previous)) NULL else
      terra::rast(previous$forest)
    previous_tof <- if (is.null(previous)) NULL else
      terra::rast(previous$tof)
    luc <- align_raster_to_template(
      terra::rast(annual_paths[[i]]), userarea_r, method = "near"
    )
    luc <- terra::ifel(luc == 0, NA, luc)
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
      # Forest loss/gain take priority. Remaining TOF gains and losses are
      # codes 3 and 4; all other valid pixels keep code 0.
      transition <- terra::ifel(
        is.na(forest) | is.na(previous_forest), NA,
        terra::ifel(previous_forest == 1 & forest == 0, 1,
                    terra::ifel(
                      previous_forest == 0 & forest == 1, 2,
                      terra::ifel(
                        is.na(previous_tof) | is.na(tof), 0,
                        terra::ifel(
                          previous_tof == 0 & tof == 1, 3,
                          terra::ifel(previous_tof == 1 & tof == 0, 4, 0)
                        )
                      )
                    ))
      )
    }
    terra::writeRaster(
      key, file.path(output_dir,
                     sprintf("LULCt%d_c_%d.tif", woodman_slot, year)),
      datatype = "INT2S", overwrite = annual_overwrite,
      wopt = list(gdal = c("COMPRESS=LZW"))
    )
    tof_path <- file.path(output_dir,
                          sprintf("TOFvsFOR_mask%d_%d.tif", woodman_slot, year))
    terra::writeRaster(
      tof, tof_path,
      datatype = "INT2S", overwrite = annual_overwrite,
      wopt = list(gdal = c("COMPRESS=LZW"))
    )
    terra::writeRaster(
      transition,
      file.path(output_dir, sprintf("LULCt3_transition_%d.tif", year)),
      datatype = "INT2S", overwrite = annual_overwrite,
      wopt = list(gdal = c("COMPRESS=LZW"))
    )
    forest_path <- file.path(scratch, sprintf("forest_%d.tif", year))
    terra::writeRaster(forest, forest_path, datatype = "INT2S",
                       wopt = list(gdal = c("COMPRESS=LZW")))
    list(forest = forest_path, tof = tof_path)
  }
  previous <- NULL
  for (i in seq_along(years)) {
    year <- years[[i]]
    year_scratch <- file.path(scratch, as.character(year))
    if (!dir.create(year_scratch)) {
      stop("Could not create annual Woodman temporary directory: ", year_scratch)
    }
    terra::terraOptions(tempdir = year_scratch)
    next_state <- prepare_year(i, previous)
    invisible(gc())
    terra::terraOptions(tempdir = scratch)
    unlink(year_scratch, recursive = TRUE)
    if (!is.null(previous)) unlink(previous$forest)
    previous <- next_state
    message("Prepared Woodman LUC", woodman_slot,
            ", TOF and transition maps for ", year)
  }
  invisible(NULL)
}

prepare_woodman_annual_maps()
