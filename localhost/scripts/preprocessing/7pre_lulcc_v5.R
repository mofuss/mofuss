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

# MoFuSS global LULC growth-parameter preprocessing
# Version 5
# Date: Aug 2026

# References for IPCC values, to be reconsidered eventually
# https://www.ipcc-nggip.iges.or.jp/public/2019rf/index.html
# https://www.ipcc-nggip.iges.or.jp/public/2019rf/pdf/4_Volume4/19R_V4_Ch04_Forest%20Land.pdf

# Internal parameters
if (!exists("plot_curves", inherits = FALSE)) plot_curves <- 1
if (!exists("publish_lulcc_outputs", inherits = FALSE)) publish_lulcc_outputs <- TRUE

# AGB input: 1 = NASA/ORNL, 2 = ESA CCI, 3 = CTrees.
agb_map_id <- 3L
if (
  length(agb_map_id) != 1 || !is.numeric(agb_map_id) || is.na(agb_map_id) ||
    agb_map_id != as.integer(agb_map_id) || !agb_map_id %in% 1:3
) {
  stop("agb_map_id must be one of: 1, 2, 3.")
}
agb_map_id <- as.integer(agb_map_id)

# Plot controls: edit these values before running the full script, or assign
# them immediately before rerunning only the final plotting block.
plot_dataset <- "MODIS" # MODIS or COPERNICUS
plot_region <- "ASIA"    # GLOBAL, LATAM, ASIA, SSA or OCEANIA
plot_seed <- 155L        # Reproducible curve selection and simulations
plot_dataset <- toupper(trimws(plot_dataset))
plot_region <- toupper(trimws(plot_region))
valid_plot_datasets <- c("MODIS", "COPERNICUS")
valid_plot_regions <- c("GLOBAL", "LATAM", "ASIA", "SSA", "OCEANIA")
if (!plot_dataset %in% valid_plot_datasets) {
  stop(
    "plot_dataset must be one of: ",
    paste(valid_plot_datasets, collapse = ", ")
  )
}
if (!plot_region %in% valid_plot_regions) {
  stop(
    "plot_region must be one of: ",
    paste(valid_plot_regions, collapse = ", ")
  )
}
if (length(plot_seed) != 1 || is.na(plot_seed) || plot_seed != as.integer(plot_seed)) {
  stop("plot_seed must be one integer value.")
}

# Load packages ----
library(terra)
terraOptions(steps = 55)
terraOptions(progress=0)
library(sf)
library(tidyverse)
library(truncnorm)
library(readxl)
library(tcltk)

# Fast, exact zonal summaries for an integer AGB raster ----
#
# The previous implementation polygonized the categorical zone raster and then
# intersected those polygons with the AGB raster six times. Because both rasters
# use the same grid, a zone-by-AGB frequency table contains all information
# needed for the same statistics without any polygon geometry.
weighted_order_stat <- function(value, count, rank) {
  value[which(cumsum(count) >= rank)[1L]]
}

weighted_sample_sd <- function(value, count, mean_value) {
  n <- sum(count)
  if (n < 2) return(NA_real_)
  sqrt(sum(count * (value - mean_value)^2) / (n - 1))
}

summarise_agb_histogram_group <- function(value, count, p) {
  keep <- is.finite(value) & is.finite(count) & count > 0
  value <- value[keep]
  count <- count[keep]
  ord <- order(value)
  value <- value[ord]
  count <- count[ord]
  n <- sum(count)

  # Equivalent to stats::quantile(..., type = 7), R's default.
  h <- (n - 1) * p + 1
  j <- floor(h)
  g <- h - j
  q_value <-
    (1 - g) * weighted_order_stat(value, count, j) +
    g * weighted_order_stat(value, count, ceiling(h))

  upper <- value >= q_value
  upper_n <- sum(count[upper])
  upper_mean_raw <- sum(value[upper] * count[upper]) / upper_n
  mean_raw <- sum(value * count) / n

  data.frame(
    # K uses the percentile itself. Averaging the values above the percentile
    # would make K sensitive to the extreme upper tail that p is intended to
    # exclude. Keep the historical column name for downstream compatibility.
    agb_mean_Tdecil = round(q_value, 0),
    agb_sd_Tdecil = round(weighted_sample_sd(
      value[upper], count[upper], upper_mean_raw
    ), 0),
    agb_n_Tdecil = upper_n,
    agb_n = n,
    agb_mean = round(mean_raw, 0),
    agb_sd = round(weighted_sample_sd(value, count, mean_raw), 0),
    agb_max = max(value)
  )
}

summarise_agb_by_zone <- function(zone_raster, agb_raster, p) {
  if (!isTRUE(terra::compareGeom(
    zone_raster, agb_raster,
    crs = TRUE, ext = TRUE, rowcol = TRUE, res = TRUE,
    stopOnError = FALSE
  ))) {
    stop(
      "The LULC-zone and AGB rasters do not share an identical grid. ",
      "Resample/project the AGB raster to DTEM_pcs before calculating stats."
    )
  }

  names(zone_raster) <- "IDorig"
  names(agb_raster) <- "agb"
  histogram <- terra::crosstab(
    c(zone_raster, agb_raster),
    long = TRUE,
    useNA = FALSE
  )
  histogram <- histogram[
    is.finite(histogram$agb) & histogram$agb > 0 & histogram$n > 0,
  ]
  if (nrow(histogram) == 0) {
    stop("No positive AGB cells overlap the LULC-zone raster.")
  }

  groups <- split(histogram, histogram$IDorig)
  rows <- lapply(groups, function(x) {
    cbind(
      IDorig = x$IDorig[1],
      summarise_agb_histogram_group(x$agb, x$n, p)
    )
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

max_ica_with_matched_sd <- function(data) {
  ica <- as.matrix(data[, c("ica_l20y", "ica_m20y", "ica_primary")])
  ica_sd <- as.matrix(data[, c(
    "icaSD_l20y", "icaSD_m20y", "icaSD_primary"
  )])
  all_missing <- rowSums(!is.na(ica)) == 0
  choice <- max.col(replace(ica, is.na(ica), -Inf), ties.method = "first")
  row_id <- seq_len(nrow(data))
  icamax <- ica[cbind(row_id, choice)]
  icamax_sd <- ica_sd[cbind(row_id, choice)]
  icamax[all_missing] <- NA_real_
  icamax_sd[all_missing] <- NA_real_
  data.frame(icamax = icamax, icamaxSD = icamax_sd)
}

validate_two_digit_code <- function(x, label) {
  if (
    anyNA(x) || anyDuplicated(x) ||
      any(x < 0 | x > 99 | x != as.integer(x))
  ) {
    stop(label, " must contain unique integer codes from 0 through 99.")
  }
  invisible(TRUE)
}

get_required_parameter <- function(data, variable) {
  value <- data$ParCHR[data$Var == variable]
  if (length(value) != 1 || is.na(value) || !nzchar(trimws(value))) {
    stop("Parameter ", variable, " must contain exactly one non-empty value.")
  }
  trimws(value)
}

prepare_agb_for_analysis <- function(source_path) {
  source <- terra::rast(source_path)
  if (terra::nlyr(source) != 1) {
    stop("The selected AGB raster must have exactly one layer: ", source_path)
  }

  integer_types <- c("INT1U", "INT1S", "INT2U", "INT2S", "INT4U", "INT4S")
  if (terra::datatype(source) %in% integer_types) {
    message("Selected AGB raster is already integer; no rounding is needed.")
  } else {
    message(
      "Rounding selected AGB values for this calculation only; ",
      "no persistent AGB TIFF will be written."
    )
    source <- round(source)
  }

  source_minmax <- terra::minmax(source)
  if (!all(is.finite(source_minmax[, 1]))) {
    stop("The selected AGB raster has no finite values: ", source_path)
  }
  names(source) <- "agb"
  source
}

format_growth_parameters <- function(data) {
  data %>%
    dplyr::select(Key, reg_gez_luc, r, rSD, K, KSD, TOF) %>%
    dplyr::rename(
      `Key*` = Key,
      LULC = reg_gez_luc,
      rmax = r,
      rmaxSD = rSD
    )
}

setwd(countrydir)

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
if (length(pdecil) != 1 || !is.finite(pdecil) || pdecil < 0 || pdecil > 1) {
  stop("pdecil must be one finite probability between 0 and 1.")
}

# Select one of AGB1map, AGB2map or AGB3map for all growth calculations.
selected_agb_prefix <- paste0("AGB", agb_map_id, "map")
selected_agb_enabled <- get_required_parameter(
  country_parameters,
  selected_agb_prefix
)
if (toupper(selected_agb_enabled) != "YES") {
  stop(selected_agb_prefix, " must be YES to use this AGB input.")
}
selected_agb_name <- get_required_parameter(
  country_parameters,
  paste0(selected_agb_prefix, "_name")
)
selected_agb_year <- get_required_parameter(
  country_parameters,
  paste0(selected_agb_prefix, "_yr")
)
selected_agb_path <- file.path(
  countrydir,
  "LULCC", "DownloadedDatasets", paste0("SourceData", country_name),
  "InRaster", selected_agb_name
)
if (!file.exists(selected_agb_path)) {
  stop("Selected AGB raster does not exist: ", selected_agb_path)
}
message(
  "Selected AGB input: AGB", agb_map_id,
  " | year ", selected_agb_year,
  " | ", normalizePath(selected_agb_path, winslash = "/")
)

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
} else {
  stop("At least one of LULCt1map or LULCt2map must be YES.")
}
temp_dir <- file.path(lulccfiles, "temp")
out_gcs_dir <- file.path(lulccfiles, "out_gcs")
out_pcs_dir <- file.path(lulccfiles, "out_pcs")
out_figure_dir <- file.path(lulccfiles, "out_figures")
unlink(temp_dir, recursive = TRUE)
unlink(out_gcs_dir, recursive = TRUE)
unlink(out_pcs_dir, recursive = TRUE)
dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_gcs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_pcs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figure_dir, recursive = TRUE, showWarnings = FALSE)
setwd(lulccfiles)

agb4stats_rcr <- NULL
growth_parameters_by_dataset <- list()
curve_start_year_by_dataset <- list()
region_f <- data.frame(
  reg_code = 1:5,
  reg_chr = c("SSA", "LATAM", "ASIA", "NorAfr", "OCEANIA")
)

for (lucinputdataset in lucavailablemaps) {
  setwd(lulccfiles)
  # Rasterize
  DTEM_gcs <- rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster_GCS/DTEM_gcs.tif"))
  DTEM_pcs <- rast(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InRaster/DTEM_pcs.tif"))
  
  # Regions ----
  regions_adm0_p <- st_read(
    paste0(admindir, "/regions_adm0/mofuss_regions0.gpkg"),
    quiet = TRUE
  ) %>%
    st_transform(crs = epsg_pcs) %>%
    dplyr::mutate(
      continent = dplyr::case_when(
        startsWith(mofuss_reg, "ASIA")    ~ 3L,
        startsWith(mofuss_reg, "LATAM")   ~ 2L,
        startsWith(mofuss_reg, "SSA")     ~ 1L,
        startsWith(mofuss_reg, "NorAfr")  ~ 4L,
        startsWith(mofuss_reg, "OCEANIA") ~ 5L,
        TRUE                              ~ NA_integer_
      )
    )
  regionsr_p <- terra::rasterize(vect(regions_adm0_p), DTEM_pcs, "continent")
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
  country_parameters %>%
    dplyr::filter(Var == "gez_fieldname") %>%
    pull(ParCHR) -> gez_fieldname
  gezr <-terra::rasterize(vect(gez), DTEM_gcs, gez_fieldname)
  terra::writeRaster(gezr, "temp/gez_gcs.tif",
                     filetype = "GTiff", overwrite = TRUE)
  
  gez_p_df <- as.data.frame(gez_p) %>%
    dplyr::select(-geometry)
  validate_two_digit_code(unique(gez_p_df[[gez_fieldname]]), gez_fieldname)
  
  gezr_p <- terra::rasterize(vect(gez_p), DTEM_pcs, gez_fieldname)
  gezr_p_scale <- gezr_p * 100
  terra::writeRaster(gezr_p_scale, "temp/gez100_pcs.tif",
                     filetype = "GTiff", overwrite = TRUE)
  
  
  # Land use/cover ----
  if (lucinputdataset == "modis") {
    
    ## MODIS: MCD12Q1.061 MODIS Land Cover Type Yearly Global 500m ----
    lucmodis_cat <- read_csv(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables/luc_modis_categories.csv"))
    validate_two_digit_code(lucmodis_cat$luc_code, "MODIS luc_code")
    
    country_parameters %>%
      dplyr::filter(Var == "LULCt1map_name") %>%
      pull(ParCHR) -> LULCt1map_name
    country_parameters %>%
      dplyr::filter(Var == "LULCt1map_yr") %>%
      pull(ParCHR) -> LULCt1map_yr_pre
    clean_string1 <- gsub("c\\(|\\)", "", LULCt1map_yr_pre)
    string_numbers1 <- strsplit(clean_string1, ",")[[1]]
    LULCt1map_yr <- as.numeric(string_numbers1)
    if (anyNA(LULCt1map_yr) || length(LULCt1map_yr) == 0) {
      stop("LULCt1map_yr must contain at least one numeric year.")
    }
    modis_base_year <- min(LULCt1map_yr)
    lucmodis_baseline <- terra::rast(paste0(
      countrydir, "/LULCC/DownloadedDatasets/SourceData", country_name,
      "/InRaster/pre", modis_base_year, "_", LULCt1map_name
    ))
    
  } else if (lucinputdataset == "copernicus") {
    ## Copernicus CGLS-LC100 ----
    luccopernicus_cat_vx <- read_csv(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables/luc_copernicus_categories.csv"))
    luccopernicus_cat <- luccopernicus_cat_vx %>%
      dplyr::select(luc_code, luc_cat, TOF_luc)
    validate_two_digit_code(luccopernicus_cat$luc_code, "Copernicus luc_code")
    
    country_parameters %>%
      dplyr::filter(Var == "LULCt2map_name") %>%
      pull(ParCHR) -> LULCt2map_name
    country_parameters %>%
      dplyr::filter(Var == "LULCt2map_yr") %>%
      pull(ParCHR) -> LULCt2map_yr_pre
    clean_string2 <- gsub("c\\(|\\)", "", LULCt2map_yr_pre)
    string_numbers2 <- strsplit(clean_string2, ",")[[1]]
    LULCt2map_yr <- as.numeric(string_numbers2)
    if (anyNA(LULCt2map_yr) || length(LULCt2map_yr) == 0) {
      stop("LULCt2map_yr must contain at least one numeric year.")
    }
    copernicus_base_year <- min(LULCt2map_yr)
    luccopernicus_baseline <- terra::rast(paste0(
      countrydir, "/LULCC/DownloadedDatasets/SourceData", country_name,
      "/InRaster/pre", copernicus_base_year, "_", LULCt2map_name
    ))
  }
  
  # Merge
  
  if (lucinputdataset == "modis") {
    
    lucmodis_merge <- regionsr_p_scale + gezr_p_scale + lucmodis_baseline
    terra::writeRaster(lucmodis_merge, paste0(
      "temp/lucmodis_", modis_base_year, "_merge_pcs.tif"
    ),
                       filetype = "GTiff", overwrite = TRUE)
    # Build dataset modis
    luc_2001_dfv0 <- unique(lucmodis_merge)
    luc_2001_df <- luc_2001_dfv0 %>%
      dplyr::mutate(
        reg_code = continent %/% 10000,
        gez_code = (continent %% 10000) %/% 100,
        luc_code = continent %% 100
      )
    growth_para_v1 <- inner_join(luc_2001_df, region_f, by = "reg_code") %>%
      dplyr::inner_join(gez_p_df, by = "gez_code") %>%
      dplyr::inner_join(lucmodis_cat, by = "luc_code", multiple = "all") %>%
      tidyr::unite("reg_gez_luc", c(reg_chr, gez_name, luc_cat), sep= "_", remove = FALSE) %>%
      tidyr::unite("reg_gez", c(reg_chr, gez_name), sep= "_", remove = FALSE) %>%
      dplyr::rename(IDorig = continent) %>%
      dplyr::relocate(reg_gez, .after = luc_cat) %>%
      dplyr::relocate(reg_gez_luc, .after = reg_gez)
    as.data.frame(unique(growth_para_v1$reg_gez)) %>%
      dplyr::rename(reg_gez = "unique(growth_para_v1$reg_gez)") %>%
      write.csv("temp/gez_gcs.csv") # Warning, these categories are exported to match IPCC values - IS NOT AUTOMATED
    
  } else if (lucinputdataset == "copernicus") {
    
    rclmat_cop <- luccopernicus_cat_vx %>%
      dplyr::select("luc_code_orig", "luc_code") %>%
      unname() %>%
      as.matrix()
    luccopernicus_baseline_rcr <- classify(
      luccopernicus_baseline[[1]], rclmat_cop, include.lowest = TRUE
    ) # First band only as Copernicus might have many after Diana's change
    
    luccopernicus_merge <- regionsr_p_scale + gezr_p_scale + luccopernicus_baseline_rcr
    terra::writeRaster(luccopernicus_merge, paste0(
      "temp/luccop_", copernicus_base_year, "_merge_pcs.tif"
    ),
                       filetype = "GTiff", overwrite = TRUE)
    # Build dataset copernicus
    luc_2015_dfv0_cop <- unique(luccopernicus_merge)
    luc_2015_df_cop <- luc_2015_dfv0_cop %>%
      dplyr::mutate(
        reg_code = continent %/% 10000,
        gez_code = (continent %% 10000) %/% 100,
        luc_code = continent %% 100
      )
    growth_para_v1 <- inner_join(luc_2015_df_cop, region_f, by = "reg_code") %>%
      dplyr::inner_join(gez_p_df, by = "gez_code") %>%
      dplyr::inner_join(luccopernicus_cat, by = "luc_code", multiple = "all") %>%
      tidyr::unite("reg_gez_luc", c(reg_chr, gez_name, luc_cat), sep= "_", remove = FALSE) %>%
      tidyr::unite("reg_gez", c(reg_chr, gez_name), sep= "_", remove = FALSE) %>%
      dplyr::rename(IDorig = continent) %>%
      dplyr::relocate(reg_gez, .after = luc_cat) %>%
      dplyr::relocate(reg_gez_luc, .after = reg_gez)
    as.data.frame(unique(growth_para_v1$reg_gez)) %>%
      dplyr::rename(reg_gez = "unique(growth_para_v1$reg_gez)") %>%
      write.csv("temp/gez_gcs.csv") # Warning, these categories are exported to match IPCC values - IS NOT AUTOMATED
    
  }
  
  # AGB stats ----
  if (lucinputdataset == "modis") {
    luc_zone_raster <- lucmodis_merge
  } else if (lucinputdataset == "copernicus") {
    luc_zone_raster <- luccopernicus_merge
  }
  
  if (is.null(agb4stats_rcr)) {
    agb4stats_rcr <- prepare_agb_for_analysis(selected_agb_path)
  }
  message("Calculating exact AGB statistics from a zone-by-AGB histogram.")
  agb_stats <- summarise_agb_by_zone(
    luc_zone_raster,
    agb4stats_rcr,
    p = pdecil
  )

  if (anyDuplicated(growth_para_v1$IDorig)) {
    stop("IDorig is not unique after joining region, GEZ and LULC tables.")
  }

  growth_parameters_v0 <- growth_para_v1 %>%
    dplyr::left_join(agb_stats, by = "IDorig") %>%
    dplyr::mutate(
      agb_n_Tdecil = dplyr::coalesce(agb_n_Tdecil, 0),
      agb_n = dplyr::coalesce(agb_n, 0),
      agb_mean_Tdecil = dplyr::coalesce(agb_mean_Tdecil, 1),
      agb_sd_Tdecil = dplyr::coalesce(agb_sd_Tdecil, 1),
      agb_mean = dplyr::coalesce(agb_mean, 1),
      agb_sd = dplyr::coalesce(agb_sd, 1),
      agb_max = dplyr::coalesce(agb_max, 1)
    )

  # Join with IPCC values and derive growth rates ----
  ipcc_growth_and_stock_2019 <- read_excel(paste0(countrydir,"/LULCC/DownloadedDatasets/SourceData",country_name,"/InTables/ipcc_growth_and_stock_2019.xlsx")) %>% # Eventually read from excel
    dplyr::select(-obs)
  
  growth_parameters_joined <- growth_parameters_v0 %>%
    dplyr::left_join(ipcc_growth_and_stock_2019, by = "reg_gez")

  missing_ipcc <- unique(
    growth_parameters_joined$reg_gez[is.na(growth_parameters_joined$TOF_gez)]
  )
  if (length(missing_ipcc) > 0) {
    stop(
      "Missing IPCC rows for reg_gez: ",
      paste(missing_ipcc, collapse = ", ")
    )
  }

  matched_ica <- max_ica_with_matched_sd(growth_parameters_joined)
  growth_parameters_v1 <- dplyr::bind_cols(
    growth_parameters_joined,
    matched_ica
  ) %>%
    dplyr::mutate(
      # A rounded biomass statistic equal to 1 is a valid value, not a
      # missing-data flag. Only a zero cell count triggers the fallback.
      TOF = dplyr::case_when(
        agb_n_Tdecil == 0 ~ 1,
        TRUE ~ pmax(TOF_luc, TOF_gez)
      ),
      icamax = dplyr::if_else(TOF == 0, icamax, NA_real_),
      icamaxSD = dplyr::if_else(TOF == 0, icamaxSD, NA_real_),
      r = dplyr::if_else(
        TOF == 0,
        round(icamax * 4 / agb_mean_Tdecil, 2),
        0
      ),
      rSD = dplyr::if_else(
        TOF == 0,
        round(icamaxSD * 4 / agb_mean_Tdecil, 2),
        0
      ),
      K = dplyr::if_else(TOF == 0, agb_mean_Tdecil, agb_mean),
      KSD = dplyr::if_else(TOF == 0, agb_sd_Tdecil, agb_sd)
    ) %>%
    dplyr::relocate(TOF, .after = KSD) %>%
    dplyr::arrange(IDorig) %>%
    as.data.frame()

  invalid_growth <- growth_parameters_v1 %>%
    dplyr::filter(
      !is.finite(r) | !is.finite(rSD) | !is.finite(K) | !is.finite(KSD) |
        K <= 0 | KSD < 0
    )
  if (nrow(invalid_growth) > 0) {
    stop(
      "Non-finite or invalid growth parameters were generated for ",
      nrow(invalid_growth), " LULC classes."
    )
  }

  growth_parameters_by_dataset[[lucinputdataset]] <- growth_parameters_v1
  curve_start_year_by_dataset[[lucinputdataset]] <- if (
    lucinputdataset == "modis"
  ) modis_base_year else copernicus_base_year
  
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
    
    lucmodis_merge_rcl <- lucmodis_merge %>%
      terra::classify(rcl_modis, include.lowest = FALSE, right = NA)
    terra::writeRaster(
      lucmodis_merge_rcl,
      paste0("out_pcs/pre", modis_base_year, "_v1_", LULCt1map_name),
      filetype = "GTiff", overwrite = TRUE
    )
    
    growth_parameters_v3 <- format_growth_parameters(growth_parameters_v2)
    
    write.csv(growth_parameters_v3, "out_pcs/growth_parameters_v3_modis.csv", row.names=FALSE, quote=FALSE)
    
    
  } else if (lucinputdataset == "copernicus") {
    
    rcl_copernicus <- growth_parameters_v2 %>%
      dplyr::select(IDorig, Key) %>%
      mutate(across(
        .cols = matches('IDorig'),
        .fns = ~ as.integer(.x))) %>%
      as.matrix() %>%
      unname()
    
    
    luccopernicus_merge_rcl <- luccopernicus_merge %>%
      terra::classify(rcl_copernicus, include.lowest = FALSE, right = NA)
    terra::writeRaster(
      luccopernicus_merge_rcl,
      paste0("out_pcs/pre", copernicus_base_year, "_v1_", LULCt2map_name),
      filetype = "GTiff", overwrite = TRUE
    )
    
    
    growth_parameters_v3 <- format_growth_parameters(growth_parameters_v2)
    
    write.csv(growth_parameters_v3, "out_pcs/growth_parameters_v3_copernicus.csv", row.names=FALSE, quote=FALSE)
    
  }
  
}

# Copy 2 MoFuSS ----
if (publish_lulcc_outputs) {
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
} else {
  message(
    "publish_lulcc_outputs is FALSE; validation outputs remain in ",
    normalizePath(file.path(lulccfiles, "out_pcs"), winslash = "/")
  )
}


# Plot random growth-parameter categories ----
if (plot_curves == 1) {
  ncurves <- 5
  num_simulations <- 10
  set.seed(plot_seed)

  selected_plot_dataset <- tolower(plot_dataset)
  available_plot_datasets <- names(growth_parameters_by_dataset)
  if (!selected_plot_dataset %in% available_plot_datasets) {
    stop(
      "plot_dataset = ", plot_dataset, " was not processed. Available: ",
      paste(toupper(available_plot_datasets), collapse = ", "), "."
    )
  }

  plot_growth_parameters <-
    growth_parameters_by_dataset[[selected_plot_dataset]]
  curve_start_year <-
    curve_start_year_by_dataset[[selected_plot_dataset]]
  dataset_label <- plot_dataset

  curve_candidates <- plot_growth_parameters %>%
    dplyr::filter(
      is.finite(r),
      is.finite(K),
      K > 0,
      agb_n_Tdecil >= 30,
      TOF == 0
    )

  if (plot_region != "GLOBAL") {
    curve_candidates <- curve_candidates %>%
      dplyr::filter(reg_chr == plot_region)
  }

  # Take at most one LULC class per continent-ecozone before the final sample.
  sampled_data <- curve_candidates %>%
    dplyr::group_by(reg_gez) %>%
    dplyr::slice_sample(n = 1) %>%
    dplyr::ungroup()

  if (nrow(sampled_data) == 0) {
    stop(
      "No eligible ", plot_dataset,
      " growth curves for plot_region = ", plot_region,
      ". Check TOF and agb_n_Tdecil filters."
    )
  }

  final_sample <- sampled_data %>%
    dplyr::slice_sample(n = min(ncurves, nrow(sampled_data))) %>%
    dplyr::select(IDorig, reg_gez_luc, agb_mean, r, rSD, K, KSD) %>%
    as.data.frame()

  publication_palette <- c(
    "#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00"
  )
  final_sample$color <- publication_palette[seq_len(nrow(final_sample))]
  print(final_sample)

  curve_colors <- stats::setNames(
    final_sample$color,
    final_sample$reg_gez_luc
  )
  format_scenario_label <- function(x) {
    x %>%
      gsub("_", " - ", ., fixed = TRUE) %>%
      stringr::str_wrap(width = 42)
  }
  publication_theme <- theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9, face = "bold"),
      plot.title = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 10),
      plot.margin = margin(8, 8, 8, 8)
    )

  growth_function <- function(agbt0, r, K, time) {
    agbt <- numeric(length(time))
    agbt[1] <- agbt0
    if (length(time) > 1) {
      for (i in 2:length(time)) {
        agbt[i] <- agbt[i - 1] +
          agbt[i - 1] * r * (1 - agbt[i - 1] / K)
      }
    }
    agbt
  }

  time <- curve_start_year:2050

  deterministic_results <- do.call(rbind, lapply(
    seq_len(nrow(final_sample)),
    function(i) {
      data.frame(
        Time = time,
        AGBT = growth_function(
          final_sample$agb_mean[i],
          final_sample$r[i],
          final_sample$K[i],
          time
        ),
        Scenario = final_sample$reg_gez_luc[i]
      )
    }
  ))

  deterministic_plot <- ggplot(
    deterministic_results,
    aes(x = Time, y = AGBT, color = Scenario)
  ) +
    geom_line(linewidth = 0.7) +
    scale_color_manual(
      values = curve_colors,
      labels = format_scenario_label
    ) +
    guides(color = guide_legend(ncol = 2, byrow = TRUE)) +
    labs(
      title = paste(dataset_label, "forest growth curves -", plot_region),
      subtitle = paste(
        nrow(final_sample),
        "randomly selected continent-ecozone-LULC categories"
      ),
      x = "Time",
      y = "Aboveground Biomass (AGB)",
      color = "continent-ecozone-lulc"
    ) +
    publication_theme
  print(deterministic_plot)

  draw_nonnegative <- function(mean_value, sd_value) {
    if (is.finite(sd_value) && sd_value > 0) {
      rtruncnorm(1, a = 0, b = Inf, mean = mean_value, sd = sd_value)
    } else {
      mean_value
    }
  }

  simulation_index <- expand.grid(
    sample_row = seq_len(nrow(final_sample)),
    Simulation = seq_len(num_simulations)
  )
  simulation_results <- do.call(rbind, lapply(
    seq_len(nrow(simulation_index)),
    function(j) {
      i <- simulation_index$sample_row[j]
      r_sim <- draw_nonnegative(final_sample$r[i], final_sample$rSD[i])
      K_sim <- draw_nonnegative(final_sample$K[i], final_sample$KSD[i])
      data.frame(
        Time = time,
        AGBT = growth_function(final_sample$agb_mean[i], r_sim, K_sim, time),
        Scenario = final_sample$reg_gez_luc[i],
        Simulation = simulation_index$Simulation[j]
      )
    }
  ))

  summary_results <- simulation_results %>%
    dplyr::group_by(Time, Scenario) %>%
    dplyr::summarise(
      MeanAGBT = mean(AGBT),
      SdAGBT = sd(AGBT),
      .groups = "drop"
    )

  uncertainty_plot <- ggplot(
    summary_results,
    aes(x = Time, y = MeanAGBT, group = Scenario, color = Scenario)
  ) +
    geom_ribbon(
      aes(
        ymin = MeanAGBT - SdAGBT,
        ymax = MeanAGBT + SdAGBT,
        fill = Scenario
      ),
      alpha = 0.3,
      color = NA
    ) +
    geom_line(linewidth = 0.7) +
    scale_color_manual(
      values = curve_colors,
      labels = format_scenario_label
    ) +
    scale_fill_manual(
      values = curve_colors,
      labels = format_scenario_label
    ) +
    guides(
      color = guide_legend(ncol = 2, byrow = TRUE),
      fill = guide_legend(ncol = 2, byrow = TRUE)
    ) +
    labs(
      title = paste(
        dataset_label, "forest growth with uncertainty -", plot_region
      ),
      subtitle = paste(num_simulations, "Monte Carlo simulations per curve"),
      x = "Time",
      y = "Aboveground Biomass (AGB)",
      color = "continent-ecozone-lulc",
      fill = "continent-ecozone-lulc"
    ) +
    publication_theme
  print(uncertainty_plot)

  plot_file_stub <- paste(
    "growth_curves",
    tolower(plot_dataset),
    tolower(plot_region),
    sep = "_"
  )
  deterministic_plot_path <- file.path(
    out_figure_dir,
    paste0(plot_file_stub, "_deterministic.png")
  )
  uncertainty_plot_path <- file.path(
    out_figure_dir,
    paste0(plot_file_stub, "_uncertainty.png")
  )

  ggsave(
    deterministic_plot_path,
    deterministic_plot,
    width = 7.2,
    height = 8.5,
    units = "in",
    dpi = 300,
    bg = "white"
  )
  ggsave(
    uncertainty_plot_path,
    uncertainty_plot,
    width = 7.2,
    height = 8.5,
    units = "in",
    dpi = 300,
    bg = "white"
  )
  message(
    "Saved 300 dpi curve figures to ",
    normalizePath(out_figure_dir, winslash = "/")
  )
}
