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
# Script: 3_mofuss_agb_validation_v3.R
# Version: 3
# Date: August 2026
# Execution: Use regular RStudio Source. With INTERACTIVE = FALSE, the script
# can also be run directly with Rscript from PowerShell/a terminal.
#
# Purpose: Validate BAU and ICS/CCTS intervention AGB trajectories and endpoint changes against
# CTrees observations, comparing capped and uncapped versions of each scenario.
# Inputs: Four completed BAU and intervention x capped/uncapped working folders, every
# debugging_N annual AGB series, CTrees AGB rasters, admin boundaries, and lakes.
# Outputs: Complete BAU and intervention validation sets under the corresponding
# <drive>/mofuss_postprocessing/<analysis>/validation directory.
# Side effects: Cleanly rebuilds only its exact per-scenario validation output
# folders; it does not modify the MoFuSS working folders.

# 2dolist ----

# Validation design notes ----

###############################################################################
##  MoFuSS simulated  vs  CTrees observed  ABOVEGROUND BIOMASS (AGB) validation
##  -------------------------------------------------------------------------
##  * national AGB trajectories (observed vs capped vs uncapped regrowth)
##  * spatial maps of AGB change BASE_YEAR-END_YEAR
##  * pixel-level agreement (r / RMSE / bias)
##
##  Design notes (important):
##   - Observed maps are CLIPPED to the country outline (userarea1.gpkg) so they
##     match the MoFuSS domain (the projected CTrees mosaic is NOT pre-clipped and
##     otherwise leaks ~1/3 extra cells from neighbouring countries).
##   - Each simulation configuration retains its own non-NULL footprint. Observed
##     and capped use their shared mask; uncapped keeps its smaller valid mask.
##     National figures show one observed series (on the capped/observed mask), so
##     uncapped's lower absolute baseline explicitly includes its smaller coverage.
##   - If a run has several Monte-Carlo folders (debugging_1..N) ALL trajectories
##     are drawn; the maps and pixel scatter use debugging_1.
##   - MoFuSS Growth_less_harv rasters are extensive (MgDM/cell). Code 01 is
##     BASE_YEAR, code 02 is BASE_YEAR+1, etc. They must never be bilinearly
##     resampled. CTrees is intensive (MgDM/ha), so bilinear alignment is valid.
##   - Gross NRB is sum(max(AGB_start - AGB_end, 0)) by pixel. Net NRB is
##     max(0, sum(AGB_start - AGB_end)) over the requested national footprint.
##     Intermediate years do not enter either calculation.
##   - HydroLAKES cells are excluded from the validation domain for all sources.
##     This affects maps, trajectories, NRB totals, and pixel statistics equally.
##   - Physical totals use cell-specific geodesic hectares from terra::cellSize;
##     CSVs also retain MoFuSS's xres^2-area totals for internal mass-balance use.
##
##  Requirements:  install.packages(c("terra","ggplot2"))
###############################################################################

# Load libraries ----

if (!requireNamespace("terra",   quietly = TRUE)) stop("Please install 'terra':   install.packages('terra')")
if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install 'ggplot2': install.packages('ggplot2')")
## All terra/ggplot2 calls below are explicitly namespaced. This prevents the
## `conflicted` package (or an attached raster/dplyr/glue/gdata package) from
## making function dispatch depend on the user's current RStudio session.

# Internal parameters ----

###############################################################################
## 1. CONFIGURATION
###############################################################################
INTERACTIVE <- FALSE

# Supplied centrally by 0calib_valid_agb_pipeline_v1.R. Empty/NA defaults
# prevent stale computer-specific paths and hidden analysis conventions.
WORKING_DIRS <- character()
SPINUP_YEARS <- NA_integer_
OBS_TYPE <- "projected"      # projected (MgDM/ha) or latlong (MgCO2/ha)
OBS_DIR_INPUT <- ""
ADMIN_VECTOR <- ""
POSTPROCESSING_ROOT <- ""    # inferred from the common working-folder parent
DRY_RUN <- FALSE
CLIP_OBS_TO_COUNTRY <- TRUE
EXCLUDE_HYDROLAKES <- TRUE
HYDROLAKES_RASTER <- ""
BASE_YEAR <- 2000L
END_YEAR <- 2025L
SIM_END_YEAR <- 2050L
CARBON_FRACTION <- 0.47

stopf <- function(...) stop(sprintf(...), call. = FALSE)
safe_id <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) stop("Could not construct a safe identifier.", call. = FALSE)
  x
}
path_key <- function(x) {
  y <- normalizePath(x, winslash = "/", mustWork = FALSE)
  if (.Platform$OS.type == "windows") tolower(y) else y
}
NODATA    <- -9999

parse_bool <- function(x, label) {
  value <- tolower(trimws(as.character(x)))
  if (value %in% c("true", "t", "1", "yes", "y")) return(TRUE)
  if (value %in% c("false", "f", "0", "no", "n")) return(FALSE)
  stopf("%s must be true or false; got: %s", label, x)
}
parse_integer <- function(x, label, minimum = 0L) {
  numeric_value <- suppressWarnings(as.numeric(x))
  integer_value <- suppressWarnings(as.integer(x))
  if (length(integer_value) != 1L || is.na(integer_value) ||
      !is.finite(numeric_value) || numeric_value != integer_value || integer_value < minimum) {
    stopf("%s must be one integer >= %d; got: %s", label, minimum, x)
  }
  integer_value
}

for (arg in commandArgs(trailingOnly = TRUE)) {
  value <- function(prefix) sub(paste0("^", prefix), "", arg)
  if (arg %in% c("--help", "-h")) {
    cat(paste0(
      "Usage: Rscript 3_mofuss_agb_validation_v3.R [options]\n",
      "  --working-dir=DIR       Repeat exactly four times\n",
      "  --spinup-years=N\n",
      "  --obs-type=projected|latlong --obs-dir=DIR\n",
      "  --admin-vector=GPKG\n",
      "  --base-year=YYYY --end-year=YYYY --sim-end-year=YYYY\n",
      "  --clip-obs-to-country=true|false\n",
      "  --exclude-hydrolakes=true|false [--hydrolakes-raster=FILE]\n",
      "  --carbon-fraction=N --dry-run\n"
    ))
    quit(save = "no", status = 0L, runLast = FALSE)
  } else if (startsWith(arg, "--working-dir=")) {
    WORKING_DIRS <- c(WORKING_DIRS, value("--working-dir="))
  } else if (startsWith(arg, "--spinup-years=")) {
    SPINUP_YEARS <- parse_integer(value("--spinup-years="), "--spinup-years")
  } else if (startsWith(arg, "--obs-type=")) {
    OBS_TYPE <- tolower(value("--obs-type="))
  } else if (startsWith(arg, "--obs-dir=")) {
    OBS_DIR_INPUT <- value("--obs-dir=")
  } else if (startsWith(arg, "--admin-vector=")) {
    ADMIN_VECTOR <- value("--admin-vector=")
  } else if (startsWith(arg, "--base-year=")) {
    BASE_YEAR <- parse_integer(value("--base-year="), "--base-year")
  } else if (startsWith(arg, "--end-year=")) {
    END_YEAR <- parse_integer(value("--end-year="), "--end-year")
  } else if (startsWith(arg, "--sim-end-year=")) {
    SIM_END_YEAR <- parse_integer(value("--sim-end-year="), "--sim-end-year")
  } else if (startsWith(arg, "--clip-obs-to-country=")) {
    CLIP_OBS_TO_COUNTRY <- parse_bool(value("--clip-obs-to-country="), "--clip-obs-to-country")
  } else if (startsWith(arg, "--exclude-hydrolakes=")) {
    EXCLUDE_HYDROLAKES <- parse_bool(value("--exclude-hydrolakes="), "--exclude-hydrolakes")
  } else if (startsWith(arg, "--hydrolakes-raster=")) {
    HYDROLAKES_RASTER <- value("--hydrolakes-raster=")
  } else if (startsWith(arg, "--carbon-fraction=")) {
    CARBON_FRACTION <- suppressWarnings(as.numeric(value("--carbon-fraction=")))
  } else if (identical(arg, "--dry-run")) {
    DRY_RUN <- TRUE
  } else if (startsWith(arg, "--")) {
    stopf("Unknown option: %s", arg)
  } else {
    WORKING_DIRS <- c(WORKING_DIRS, arg)
  }
}

if (is.na(SPINUP_YEARS)) stop("--spinup-years is required.", call. = FALSE)
if (!OBS_TYPE %in% c("projected", "latlong")) {
  stop("--obs-type must be projected or latlong.", call. = FALSE)
}
if (!dir.exists(OBS_DIR_INPUT)) stopf("Observed AGB directory does not exist: %s", OBS_DIR_INPUT)
OBS_DIR_INPUT <- normalizePath(OBS_DIR_INPUT, winslash = "/", mustWork = TRUE)
OBS_LL_DIR <- OBS_DIR_INPUT
OBS_PROJ_DIR <- OBS_DIR_INPUT
if (!file.exists(ADMIN_VECTOR) || dir.exists(ADMIN_VECTOR)) {
  stopf("Admin GeoPackage does not exist: %s", ADMIN_VECTOR)
}
ADMIN_VECTOR <- normalizePath(ADMIN_VECTOR, winslash = "/", mustWork = TRUE)
if (END_YEAR <= BASE_YEAR) stop("--end-year must be later than --base-year.", call. = FALSE)
if (SIM_END_YEAR < END_YEAR) stop("--sim-end-year cannot precede --end-year.", call. = FALSE)
if (!is.finite(CARBON_FRACTION) || CARBON_FRACTION <= 0) {
  stop("--carbon-fraction must be a positive number.", call. = FALSE)
}

## LAT/LONG observed unit conversion MgCO2/ha -> MgDM/ha.
CO2_TO_DM       <- (12/44) / CARBON_FRACTION

## MoFuSS convention: 01 = BASE_YEAR, 02 = BASE_YEAR+1, ..., 51 = BASE_YEAR+50.
sim_file_name <- function(year) sprintf("Growth_less_harv%02d.tif", as.integer(year - BASE_YEAR + 1L))
obs_proj_name <- function(year) sprintf("ctrees_%d_agb_MgDM_ha.tif", year)
obs_ll_name   <- function(year) sprintf("ctrees_global_%d_AGC.tif",  year)

COL_OBS <- "#222222"; COL_CAP <- "#1f77b4"; COL_UNC <- "#d62728"

find_parameters_file <- function(workdir) {
  root <- file.path(workdir, "LULCC", "DownloadedDatasets")
  files <- list.files(root, pattern = "^parameters.*\\.csv$", recursive = TRUE,
                      full.names = TRUE, ignore.case = TRUE)
  if (length(files) != 1L) stopf("Expected exactly one parameters*.csv below %s; found %d.", root, length(files))
  files[[1]]
}
read_run_metadata <- function(workdir) {
  workdir <- normalizePath(workdir, winslash = "/", mustWork = TRUE)
  path <- find_parameters_file(workdir)
  x <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (ncol(x) < 2L) stopf("Parameter table has fewer than two columns: %s", path)
  keys <- trimws(as.character(x[[1]])); vals <- trimws(as.character(x[[2]]))
  value <- function(key) {
    z <- vals[keys == key]; z <- z[!is.na(z) & nzchar(z)]
    if (length(z) != 1L) stopf("Expected one value for '%s' in %s.", key, path)
    z[[1]]
  }
  int_value <- function(key) {
    z <- suppressWarnings(as.integer(value(key)))
    if (is.na(z)) stopf("Parameter '%s' is not an integer in %s.", key, path)
    z
  }
  scenario <- value("scenario_ver")
  uncapped <- int_value("uncapped_regrowth")
  if (!uncapped %in% c(0L, 1L)) stopf("uncapped_regrowth must be 0 or 1 in %s.", path)
  role <- if (grepl("^bau", scenario, ignore.case = TRUE)) {
    "bau"
  } else if (grepl("^(ics|ccts)", scenario, ignore.case = TRUE)) {
    "ccts"
  } else {
    stopf("Could not classify scenario_ver '%s' as BAU or ICS/CCTS intervention in %s.", scenario, path)
  }
  gee_scale <- suppressWarnings(as.numeric(value("GEE_scale")))
  if (!is.finite(gee_scale)) stopf("GEE_scale is not numeric in %s.", path)
  data.frame(
    working_dir = workdir,
    iso3 = toupper(value("region2BprocessedCtry_iso")),
    country = value("region2BprocessedCtry"),
    scenario = scenario,
    role = role,
    mode = if (uncapped == 1L) "uncapped" else "capped",
    model_start = int_value("start_year"), model_end = int_value("end_year"),
    mc_runs = int_value("monte_carlo_runs"),
    gee_scale = gee_scale,
    stringsAsFactors = FALSE
  )
}

WORKING_DIRS <- unique(as.character(WORKING_DIRS[nzchar(WORKING_DIRS)]))
if (length(WORKING_DIRS) != 4L) stopf("This batch requires exactly four working folders; received %d.", length(WORKING_DIRS))
run_metadata <- do.call(rbind, lapply(WORKING_DIRS, read_run_metadata))
common_fields <- c("iso3", "country", "model_start", "model_end", "mc_runs", "gee_scale")
for (field in common_fields) {
  if (length(unique(tolower(as.character(run_metadata[[field]])))) != 1L) {
    stopf("The four working folders disagree on '%s'.", field)
  }
}
combos <- paste(run_metadata$role, run_metadata$mode, sep = "/")
expected_combos <- c("bau/capped", "bau/uncapped", "ccts/capped", "ccts/uncapped")
if (!setequal(combos, expected_combos) || anyDuplicated(combos)) {
  stopf("Expected BAU and ICS/CCTS intervention folders, each capped and uncapped; found: %s", paste(combos, collapse = ", "))
}
scenario_count_by_role <- vapply(split(run_metadata$scenario, run_metadata$role),
                                 function(x) length(unique(tolower(x))), integer(1))
if (any(scenario_count_by_role != 1L)) {
  stop("Capped and uncapped folders must use the same scenario_ver within each BAU/intervention pair.", call. = FALSE)
}
if (anyDuplicated(paste(run_metadata$scenario, run_metadata$mode, sep = "/"))) {
  stop("Scenario/configuration identities are not unique.", call. = FALSE)
}
common_parent <- unique(path_key(dirname(run_metadata$working_dir)))
if (length(common_parent) != 1L) stop("All four working folders must share one parent.", call. = FALSE)
working_parent <- dirname(run_metadata$working_dir[[1]])
postprocessing_root <- if (nzchar(POSTPROCESSING_ROOT)) POSTPROCESSING_ROOT else
  file.path(working_parent, "mofuss_postprocessing")
analysis_id <- paste(
  safe_id(run_metadata$iso3[[1]]), run_metadata$model_start[[1]] + SPINUP_YEARS,
  run_metadata$model_end[[1]], paste0("mc", run_metadata$mc_runs[[1]]), sep = "_"
)
analysis_root <- normalizePath(file.path(postprocessing_root, analysis_id), winslash = "/", mustWork = FALSE)
validation_root <- file.path(analysis_root, "validation", "3_mofuss_agb_validation")

run_validation_pair <- function(COUNTRY, COUNTRY_ISO3, SCENARIO_LABEL, MODEL_END_YEAR, MC_RUNS,
                                CAPPED_DIR, UNCAPPED_DIR, OUT_DIR) {
FILE_PREFIX <- paste(safe_id(COUNTRY_ISO3), safe_id(SCENARIO_LABEL), sep = "_")
DISPLAY_NAME <- paste(COUNTRY, "-", SCENARIO_LABEL)
if (!identical(path_key(dirname(OUT_DIR)), path_key(validation_root)) ||
    !identical(basename(OUT_DIR), safe_id(SCENARIO_LABEL))) {
  stop("Refusing unsafe validation output path: ", OUT_DIR, call. = FALSE)
}

###############################################################################
## 2. INTERACTIVE SELECTION
###############################################################################
ask_dir <- function(prompt, default) {
  if (!INTERACTIVE) return(default)
  if (.Platform$OS.type == "windows") {
    d <- utils::choose.dir(default = gsub("/", "\\\\", default), caption = prompt)
    if (is.na(d)) d <- default
  } else { cat("\n", prompt, "\n[Enter = ", default, "]: ", sep = ""); d <- readline(); if (!nzchar(d)) d <- default }
  gsub("\\\\", "/", d)
}
if (INTERACTIVE) {
  cat("=== MoFuSS AGB validation: observed-data selection ===\n")
  ot <- utils::select.list(c("projected  (MgDM/ha, same grid as MoFuSS)","lat-long   (MgCO2/ha, global EPSG:4326)"),
                           preselect = "projected  (MgDM/ha, same grid as MoFuSS)", title = "Which observed AGB maps?")
  OBS_TYPE <- if (grepl("^lat", ot)) "latlong" else "projected"
  if (OBS_TYPE == "projected") OBS_PROJ_DIR <- ask_dir("Select the observed PROJECTED (MgDM/ha) folder", OBS_PROJ_DIR)
  if (OBS_TYPE == "latlong")   OBS_LL_DIR   <- ask_dir("Select the observed LAT/LONG (MgCO2/ha) folder", OBS_LL_DIR)
}

if (!OBS_TYPE %in% c("projected", "latlong")) stop("OBS_TYPE must be 'projected' or 'latlong'.")
if (END_YEAR <= BASE_YEAR) stop("END_YEAR must be later than BASE_YEAR.")
OBS_DIR  <- if (OBS_TYPE == "latlong") OBS_LL_DIR else OBS_PROJ_DIR
if (!dir.exists(OBS_DIR)) stop("Observed AGB folder not found: ", OBS_DIR)
obs_name <- if (OBS_TYPE == "latlong") obs_ll_name else obs_proj_name
admin_path <- ADMIN_VECTOR
hydrolakes_rel <- file.path("LULCC", "DownloadedDatasets", "SourceDataGlobal",
                            "InRaster", "hydrolakes_pcs.tif")
hydrolakes_candidates <- unique(c(
  if (nzchar(HYDROLAKES_RASTER)) HYDROLAKES_RASTER else character(0),
  file.path(CAPPED_DIR, hydrolakes_rel),
  if (nzchar(UNCAPPED_DIR)) file.path(UNCAPPED_DIR, hydrolakes_rel) else character(0)
))
hydrolakes_path <- hydrolakes_candidates[file.exists(hydrolakes_candidates)][1]
if (EXCLUDE_HYDROLAKES && (length(hydrolakes_path) == 0L || is.na(hydrolakes_path)))
  stop("HydroLAKES display mask not found. Checked:\n  ",
       paste(hydrolakes_candidates, collapse = "\n  "))

cat("\nCountry      :", COUNTRY,
    "\nScenario     :", SCENARIO_LABEL,
    "\nCapped dir   :", CAPPED_DIR,
    "\nUncapped dir :", if (nzchar(UNCAPPED_DIR)) UNCAPPED_DIR else "(none)",
    "\nObserved     :", OBS_TYPE, "->", OBS_DIR,
    "\nBoundary     :", admin_path,
    "\nWater mask  :", if (EXCLUDE_HYDROLAKES) hydrolakes_path else "(disabled)",
    "\nOutput       :", OUT_DIR, "\n\n")

###############################################################################
## 3. HELPERS
###############################################################################
pad_ext <- function(e, f = 0.02) {
  dx <- (terra::xmax(e) - terra::xmin(e)) * f
  dy <- (terra::ymax(e) - terra::ymin(e)) * f
  terra::ext(terra::xmin(e) - dx, terra::xmax(e) + dx,
             terra::ymin(e) - dy, terra::ymax(e) + dy)
}
load_country_boundary <- function(path, iso3) {
  if (!file.exists(path)) stop("Country boundary not found: ", path)
  x <- terra::vect(path)
  if ("GID_0" %in% names(x)) x <- x[as.character(x$GID_0) == iso3, ]
  if (nrow(x) != 1L) {
    stop("Expected one country polygon for ", iso3, " in ", path,
         "; found ", nrow(x), ".")
  }
  x
}
list_mc <- function(workdir) {
  if (!dir.exists(workdir)) stop("MoFuSS folder not found: ", workdir)
  d <- list.dirs(workdir, recursive = FALSE); d <- d[grepl("debugging_\\d+$", d)]
  d <- d[order(as.integer(sub(".*debugging_", "", d)))]
  if (!length(d)) stop("No debugging_N Monte-Carlo folders found in: ", workdir)
  d
}
available_sim_years <- function(mc_dir) {
  f <- list.files(mc_dir, pattern = "^Growth_less_harv[0-9]+\\.tif$")
  idx <- as.integer(sub("^Growth_less_harv([0-9]+)\\.tif$", "\\1", f))
  sort(unique(BASE_YEAR + idx - 1L))
}
validate_sim_coverage <- function(mcdirs, required_years, cfg) {
  for (d in mcdirs) {
    missing <- setdiff(required_years, available_sim_years(d))
    if (length(missing))
      stop(cfg, " / ", basename(d), " is missing simulated year(s): ", paste(missing, collapse = ", "))
  }
}
ref_sim_file <- function() {
  f <- file.path(list_mc(CAPPED_DIR)[1], sim_file_name(BASE_YEAR))
  if (!file.exists(f)) stop("Reference sim file not found: ", f); f
}
align_obs <- function(year, ref) {   # -> MgDM/ha SpatRaster on the reference grid
  f <- file.path(OBS_DIR, obs_name(year))
  if (!file.exists(f)) { warning("Missing observed file: ", f); return(NULL) }
  r <- terra::rast(f); terra::NAflag(r) <- NODATA
  if (terra::nlyr(r) != 1L) stop("Expected a single-band observed raster: ", f)
  if (!nzchar(terra::crs(r))) stop("Observed raster has no CRS; refusing to assume one: ", f)
  if (terra::same.crs(r, ref)) {
    r <- terra::crop(r, pad_ext(terra::ext(ref), 0.02), snap = "out")
    a <- terra::resample(r, ref, method = "bilinear")
  } else {
    box <- terra::project(
      terra::as.polygons(terra::ext(ref), crs = terra::crs(ref)),
      terra::crs(r)
    )
    r <- terra::crop(r, pad_ext(terra::ext(box), 0.05), snap = "out")
    a <- terra::project(r, ref, method = "bilinear")
  }
  if (OBS_TYPE == "latlong") a <- a * CO2_TO_DM
  a[a < 0] <- NA; a
}
water_mask_vec <- function(path, ref) { # TRUE for HydroLAKES cells on ref grid
  r <- terra::rast(path)
  if (terra::nlyr(r) != 1L) stop("Expected a single-band HydroLAKES raster: ", path)
  if (!nzchar(terra::crs(r))) stop("HydroLAKES raster has no CRS: ", path)
  if (terra::same.crs(r, ref)) {
    r <- terra::crop(r, pad_ext(terra::ext(ref), 0.02), snap = "out")
    a <- terra::resample(r, ref, method = "near")
  } else {
    box <- terra::project(
      terra::as.polygons(terra::ext(ref), crs = terra::crs(ref)),
      terra::crs(r)
    )
    r <- terra::crop(r, pad_ext(terra::ext(box), 0.05), snap = "out")
    a <- terra::project(r, ref, method = "near")
  }
  v <- as.numeric(terra::values(a))
  is.finite(v) & v > 0
}
sim_vec <- function(mc_dir, year) {   # MoFuSS AGB in MgDM per CELL, vector on ref grid
  f <- file.path(mc_dir, sim_file_name(year))
  if (!file.exists(f)) stop("Missing MoFuSS AGB raster: ", f)
  rr <- terra::rast(f); terra::NAflag(rr) <- NODATA
  if (terra::nlyr(rr) != 1L) stop("Expected a single-band MoFuSS raster: ", f)
  if (!terra::compareGeom(rr, ref, stopOnError = FALSE))
    stop("MoFuSS raster geometry differs from the reference grid: ", f,
         "\nMoFuSS AGB is MgDM/cell (extensive), so it must not be resampled silently.")
  v <- as.numeric(terra::values(rr)); v[v == NODATA | v < 0] <- NA; v
}
sim_valid_all <- function(mcdirs, sim_years) { # common finite cells across every run/year in a configuration
  ok <- rep(TRUE, terra::ncell(ref))
  for (d in mcdirs) for (y in sim_years) ok <- ok & is.finite(sim_vec(d, y))
  ok
}

###############################################################################
## 4. REFERENCE GRID, COUNTRY MASK, OBSERVED SERIES, PER-CONFIG MASKS
###############################################################################
ref <- terra::rast(ref_sim_file()); terra::NAflag(ref) <- NODATA
if (!nzchar(terra::crs(ref))) stop("Reference MoFuSS raster has no CRS.")
if (terra::is.lonlat(ref) || !grepl("units=m", terra::crs(ref, proj = TRUE), fixed = TRUE))
  stop("The MoFuSS reference grid must use a projected CRS with metre units.")
## MoFuSS converts per-ha inputs to per-cell stocks with xres^2/10,000 (see
## rnorm_v3.R), but EPSG:3395 is not equal-area. Recover simulated density with
## that model convention, then integrate both simulated and observed density
## with each cell's geodesic ground area. NRB CSVs also retain model-native mass
## totals so later demand/harvest work can audit the original MoFuSS mass balance.
model_cell_ha  <- terra::xres(ref)^2 / 1e4
grid_cell_ha   <- prod(terra::res(ref)) / 1e4
ground_area    <- terra::cellSize(ref, unit = "ha")
ground_cell_ha <- as.numeric(terra::values(ground_area))
if (length(ground_cell_ha) != terra::ncell(ref) || any(!is.finite(ground_cell_ha)))
  stop("Could not calculate a finite geodesic area for every reference-grid cell.")
ground_stats   <- terra::global(ground_area, c("min", "max", "mean"), na.rm = TRUE)
years   <- BASE_YEAR:END_YEAR
cat(sprintf(paste0("Reference grid: %d x %d cells | pixel = %.2f x %.2f m | ",
                   "MoFuSS area convention = %.3f ha/cell\n"),
            ncol(ref), nrow(ref), terra::res(ref)[1], terra::res(ref)[2], model_cell_ha))
cat(sprintf("  planar x*y area = %.3f ha | geodesic ground area = %.3f-%.3f ha (mean %.3f)\n",
            grid_cell_ha, ground_stats$min, ground_stats$max, ground_stats$mean))

## country mask (rasterise the boundary onto the reference grid)
country_vec <- rep(TRUE, terra::ncell(ref))
if (CLIP_OBS_TO_COUNTRY && file.exists(admin_path)) {
  adm <- load_country_boundary(admin_path, COUNTRY_ISO3)
  if (!terra::same.crs(adm, ref)) adm <- terra::project(adm, terra::crs(ref))
  adm$burnval <- 1L
  country_vec <- !is.na(as.numeric(terra::values(
    terra::rasterize(adm, ref, field = "burnval", background = NA)
  )))
  cat(sprintf("Observed clipped to country outline: %d cells inside (%s)\n", sum(country_vec), basename(admin_path)))
} else if (CLIP_OBS_TO_COUNTRY) {
  stop("Country boundary not found; national aggregation would be invalid: ", admin_path)
}

## One common land domain for observed and every simulation configuration.
## Keeping country_vec separate preserves the true administrative footprint for
## diagnostics; validation_domain_vec additionally removes HydroLAKES cells.
water_vec <- rep(FALSE, terra::ncell(ref))
if (EXCLUDE_HYDROLAKES) water_vec <- water_mask_vec(hydrolakes_path, ref)
validation_domain_vec <- country_vec & !water_vec
cat(sprintf("HydroLAKES excluded from validation: %s cells inside country\n",
            format(sum(country_vec & water_vec), big.mark = ",", scientific = FALSE)))

cat("Loading observed CTrees maps ...\n")
obs_ha <- list()
for (y in years) {
  a <- align_obs(y, ref)
  v <- if (is.null(a)) rep(NA_real_, terra::ncell(ref)) else as.numeric(terra::values(a))
  v[!validation_domain_vec] <- NA
  obs_ha[[as.character(y)]] <- v
}

capMC <- list_mc(CAPPED_DIR)
uncMC <- if (nzchar(UNCAPPED_DIR)) list_mc(UNCAPPED_DIR) else character(0)
cat(sprintf("Monte-Carlo folders: capped = %d, uncapped = %d\n", length(capMC), length(uncMC)))
if (length(capMC) != MC_RUNS || length(uncMC) != MC_RUNS) {
  stop("Monte-Carlo folder counts do not match monte_carlo_runs=", MC_RUNS,
       ": capped=", length(capMC), ", uncapped=", length(uncMC), ".")
}
required_sim_years <- BASE_YEAR:min(SIM_END_YEAR, MODEL_END_YEAR)
validate_sim_coverage(capMC, required_sim_years, "Capped")
if (length(uncMC)) validate_sim_coverage(uncMC, required_sim_years, "Uncapped")

## observed validity across all years, and per-config PAIRWISE masks
obs_valid <- rep(TRUE, terra::ncell(ref)); for (y in years) obs_valid <- obs_valid & is.finite(obs_ha[[as.character(y)]])
cat("Building per-configuration (pairwise) masks ...\n")
mask_cap <- obs_valid & sim_valid_all(capMC, years)
mask_unc <- if (length(uncMC)) obs_valid & sim_valid_all(uncMC, years) else NULL
cat(sprintf("  observed valid           : %d\n", sum(obs_valid)))
cat(sprintf("  observed n capped        : %d\n", sum(mask_cap)))
if (!is.null(mask_unc)) cat(sprintf("  observed n uncapped      : %d\n", sum(mask_unc)))

obs_start <- obs_ha[[as.character(BASE_YEAR)]]
obs_end   <- obs_ha[[as.character(END_YEAR)]]

###############################################################################
## 5. NATIONAL TRAJECTORIES (one observed series; each config keeps its footprint)
###############################################################################
cat("Computing national trajectories (all Monte-Carlo runs) ...\n")
obs_total <- function(y, m)                                                        # geodesic Mt
  sum(obs_ha[[as.character(y)]][m] * ground_cell_ha[m]) / 1e6
sim_total <- function(vec, m) {                                                   # Mt (vec = per-cell MgDM)
  x <- vec[m]
  if (!length(x) || any(!is.finite(x))) return(NA_real_) # never shrink the footprint silently
  sum((x / model_cell_ha) * ground_cell_ha[m]) / 1e6
}

build_sim_traj <- function(mcdirs, cfg, m) {
  out <- data.frame()
  for (i in seq_along(mcdirs)) {
    tt <- sapply(years, function(y) sim_total(sim_vec(mcdirs[i], y), m))
    out <- rbind(out, data.frame(year = years, total_Mt = tt, series = "Simulated", config = cfg, mc = i))
    cat(sprintf("   %s MC %d/%d\r", cfg, i, length(mcdirs)))
  }
  cat("\n"); out
}
traj <- data.frame(year = years, total_Mt = sapply(years, obs_total, m = mask_cap),
                   series = "Observed", config = "Observed", mc = 0L)
traj <- rbind(traj, build_sim_traj(capMC, "Capped", mask_cap))
if (length(uncMC)) traj <- rbind(traj, build_sim_traj(uncMC, "Uncapped", mask_unc))
traj$change_Mt <- stats::ave(
  traj$total_Mt,
  interaction(traj$series, traj$config, traj$mc, drop = TRUE),
  FUN = function(x) x - x[1]
)
traj$scenario <- SCENARIO_LABEL

# All required observed and simulation inputs have now passed validation. Only
# now replace the exact scenario output folder, preserving older results when
# input preflight fails.
if (dir.exists(OUT_DIR)) {
  status <- unlink(OUT_DIR, recursive = TRUE, force = TRUE)
  if (status != 0L || dir.exists(OUT_DIR) || file.exists(OUT_DIR)) {
    stop("Could not fully remove prior validation output folder: ", OUT_DIR, call. = FALSE)
  }
}
if (!dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE) && !dir.exists(OUT_DIR)) {
  stop("Could not create validation output folder: ", OUT_DIR, call. = FALSE)
}
utils::write.csv(traj, file.path(OUT_DIR, paste0(FILE_PREFIX, "_national_trajectory_allMC.csv")), row.names = FALSE)

###############################################################################
## 6. PIXEL-LEVEL CHANGE (debugging_1) -> per-config stats (pairwise masks)
###############################################################################
cat("Computing pixel-level change (debugging_1) ...\n")
capd1_start <- sim_vec(capMC[1], BASE_YEAR) / model_cell_ha
capd1_end   <- sim_vec(capMC[1], END_YEAR)  / model_cell_ha
uncd1_start <- if (length(uncMC)) sim_vec(uncMC[1], BASE_YEAR) / model_cell_ha else NULL
uncd1_end   <- if (length(uncMC)) sim_vec(uncMC[1], END_YEAR)  / model_cell_ha else NULL
dObs <- obs_end - obs_start
dCap <- capd1_end - capd1_start
dUnc <- if (!is.null(uncd1_end)) uncd1_end - uncd1_start else NULL
## Maps use each model's own endpoint footprint within the common land domain.
dObs[!validation_domain_vec] <- NA; dCap[!validation_domain_vec] <- NA
if (!is.null(dUnc)) dUnc[!validation_domain_vec] <- NA

pix_stats <- function(sim_change, obs_change, m, label) {
  o <- obs_change[m]; s <- sim_change[m]; keep <- is.finite(o) & is.finite(s)
  o <- o[keep]; s <- s[keep]; e <- s - o
  n <- length(o)
  r <- if (n > 2L && stats::sd(o) > 0 && stats::sd(s) > 0) stats::cor(o, s) else NA_real_
  slope <- if (n > 1L && stats::sd(o) > 0) unname(stats::coef(stats::lm(s ~ o))[2]) else NA_real_
  data.frame(config = label, n = n, pearson_r = r, slope = slope,
             rmse_MgDMha = if (n) sqrt(mean(e^2)) else NA_real_,
             bias_MgDMha = if (n) mean(e) else NA_real_,
             obs_mean_change = if (n) mean(o) else NA_real_,
             sim_mean_change = if (n) mean(s) else NA_real_, row.names = NULL)
}
stats <- pix_stats(dCap, dObs, mask_cap, "capped")
if (!is.null(dUnc)) stats <- rbind(stats, pix_stats(dUnc, dObs, mask_unc, "uncapped"))
stats$scenario <- SCENARIO_LABEL
utils::write.csv(stats, file.path(OUT_DIR, paste0(FILE_PREFIX, "_change_pixelwise_stats.csv")), row.names = FALSE)
print(stats, digits = 3)

###############################################################################
## 6b. GROSS AND NET NRB (endpoint comparison only; no threshold)
###############################################################################
cat("Computing gross and net NRB from endpoint AGB ...\n")

nrb_metrics <- function(start, end, domain, source, config, scope, mc, value_unit) {
  keep <- domain & is.finite(start) & is.finite(end)
  if (!any(keep)) stop("No common endpoint cells for NRB: ", source, " / ", config, " / MC", mc)
  if (value_unit == "MgDM_ha") {
    start_density <- start[keep]
    end_density   <- end[keep]
    start_model_mg <- start_density * model_cell_ha
    end_model_mg   <- end_density   * model_cell_ha
  } else if (value_unit == "MgDM_cell") {
    start_model_mg <- start[keep]
    end_model_mg   <- end[keep]
    start_density  <- start_model_mg / model_cell_ha
    end_density    <- end_model_mg   / model_cell_ha
  } else stop("Unknown NRB value_unit: ", value_unit)

  area <- ground_cell_ha[keep]
  start_mg    <- start_density * area
  end_mg      <- end_density   * area
  loss_mg    <- start_mg - end_mg                 # positive = loss; negative = gain
  gross_mg   <- sum(pmax(loss_mg, 0))             # only pixels lower at END_YEAR
  gain_mg    <- sum(pmax(-loss_mg, 0))
  balance_mg <- sum(loss_mg)                      # gross loss minus gross gain
  net_mg     <- max(0, balance_mg)                # national net gain -> zero NRB

  model_loss_mg    <- start_model_mg - end_model_mg
  model_gross_mg   <- sum(pmax(model_loss_mg, 0))
  model_gain_mg    <- sum(pmax(-model_loss_mg, 0))
  model_balance_mg <- sum(model_loss_mg)
  model_net_mg     <- max(0, model_balance_mg)

  data.frame(
    source = source, config = config, scope = scope, mc = as.integer(mc),
    start_year = BASE_YEAR, end_year = END_YEAR,
    cells = sum(keep), area_ha_geodesic = sum(area),
    area_ha_model_convention = sum(keep) * model_cell_ha,
    AGB_start_Mg = sum(start_mg), AGB_end_Mg = sum(end_mg),
    gross_NRB_Mg = gross_mg, gross_NRB_Mt = gross_mg / 1e6,
    gross_AGB_gain_Mg = gain_mg,
    country_balance_Mg = balance_mg,
    net_NRB_Mg = net_mg, net_NRB_Mt = net_mg / 1e6,
    model_area_AGB_start_Mg = sum(start_model_mg),
    model_area_AGB_end_Mg = sum(end_model_mg),
    model_area_gross_NRB_Mg = model_gross_mg,
    model_area_gross_AGB_gain_Mg = model_gain_mg,
    model_area_country_balance_Mg = model_balance_mg,
    model_area_net_NRB_Mg = model_net_mg,
    loss_pixels = sum(loss_mg > 0), gain_pixels = sum(loss_mg < 0),
    stringsAsFactors = FALSE
  )
}

## National/end-point values: observed on all valid CTrees country cells; each
## MoFuSS configuration on its own valid model domain. These are the values to
## retain for later demand comparisons (demand must use the same scope and an
## explicitly chosen geodesic-vs-model-native accounting convention).
nrb_all <- nrb_metrics(obs_start, obs_end, validation_domain_vec, "Observed", "Country",
                       "country_endpoint", 0L, "MgDM_ha")
append_model_nrb <- function(out, mcdirs, cfg) {
  for (i in seq_along(mcdirs)) {
    s0 <- sim_vec(mcdirs[i], BASE_YEAR); s1 <- sim_vec(mcdirs[i], END_YEAR)
    out <- rbind(out, nrb_metrics(s0, s1, validation_domain_vec, "MoFuSS", cfg,
                                  "configuration_endpoint", i, "MgDM_cell"))
  }
  out
}
nrb_all <- append_model_nrb(nrb_all, capMC, "Capped")
if (length(uncMC)) nrb_all <- append_model_nrb(nrb_all, uncMC, "Uncapped")

## Also save strict like-for-like observed/model rows on each MC run's common
## endpoint footprint. This avoids confusing a footprint difference with an AGB
## difference when the NRB values are inspected for validation.
nrb_pairwise <- NULL
append_pairwise_nrb <- function(out, mcdirs, cfg) {
  for (i in seq_along(mcdirs)) {
    s0 <- sim_vec(mcdirs[i], BASE_YEAR); s1 <- sim_vec(mcdirs[i], END_YEAR)
    common <- validation_domain_vec & is.finite(obs_start) & is.finite(obs_end) & is.finite(s0) & is.finite(s1)
    out <- rbind(out,
                 nrb_metrics(obs_start, obs_end, common, "Observed", cfg,
                             "pairwise_common_endpoint", i, "MgDM_ha"),
                 nrb_metrics(s0, s1, common, "MoFuSS", cfg,
                             "pairwise_common_endpoint", i, "MgDM_cell"))
  }
  out
}
nrb_pairwise <- append_pairwise_nrb(nrb_pairwise, capMC, "Capped")
if (length(uncMC)) nrb_pairwise <- append_pairwise_nrb(nrb_pairwise, uncMC, "Uncapped")

summarise_nrb <- function(d) {
  key <- interaction(d$source, d$config, d$scope, drop = TRUE, lex.order = TRUE)
  rows <- lapply(split(d, key), function(z) {
    sd1 <- function(x) if (length(x) > 1L) stats::sd(x) else NA_real_
    data.frame(source = z$source[1], config = z$config[1], scope = z$scope[1],
               mass_area_method = "geodesic_cellSize",
               n_runs = nrow(z), cells_min = min(z$cells), cells_max = max(z$cells),
               area_ha_geodesic_mean = mean(z$area_ha_geodesic),
               AGB_start_Mt_mean = mean(z$AGB_start_Mg) / 1e6,
               AGB_end_Mt_mean = mean(z$AGB_end_Mg) / 1e6,
               gross_NRB_Mg_mean = mean(z$gross_NRB_Mg),
               gross_NRB_Mt_mean = mean(z$gross_NRB_Mt),
               gross_NRB_Mt_sd = sd1(z$gross_NRB_Mt),
               country_balance_Mg_mean = mean(z$country_balance_Mg),
               net_NRB_Mg_mean = mean(z$net_NRB_Mg),
               net_NRB_Mt_mean = mean(z$net_NRB_Mt),
               net_NRB_Mt_sd = sd1(z$net_NRB_Mt),
               model_area_gross_NRB_Mt_mean = mean(z$model_area_gross_NRB_Mg) / 1e6,
               model_area_net_NRB_Mt_mean = mean(z$model_area_net_NRB_Mg) / 1e6,
               row.names = NULL)
  })
  do.call(rbind, rows)
}
nrb_summary <- summarise_nrb(nrb_all)
nrb_all$scenario <- SCENARIO_LABEL
nrb_summary$scenario <- SCENARIO_LABEL
nrb_pairwise$scenario <- SCENARIO_LABEL

utils::write.csv(nrb_all, file.path(OUT_DIR, paste0(FILE_PREFIX, "_NRB_aggregates_allMC.csv")), row.names = FALSE)
utils::write.csv(nrb_summary, file.path(OUT_DIR, paste0(FILE_PREFIX, "_NRB_aggregates_summary.csv")), row.names = FALSE)
utils::write.csv(nrb_pairwise, file.path(OUT_DIR, paste0(FILE_PREFIX, "_NRB_aggregates_pairwise_allMC.csv")), row.names = FALSE)
print(nrb_summary, row.names = FALSE, digits = 5)

###############################################################################
## 7. FIGURE 1 : national AGB change RELATIVE TO BASE_YEAR (single panel, %)
##    All series start at 0% in BASE_YEAR. Percent change divides by each series'
##    own BASE_YEAR baseline. Observed/capped share a mask; uncapped retains its
##    smaller non-NULL footprint.
###############################################################################
cat("Drawing Figure 1 (relative-change trajectories) ...\n")

## percent change vs BASE_YEAR, within each series/config/MC
traj$pct <- stats::ave(
  traj$total_Mt,
  interaction(traj$series, traj$config, traj$mc, drop = TRUE),
  FUN = function(x) 100 * (x - x[1]) / x[1]
)

sim_df <- base::subset(traj, series == "Simulated"); sim_df$grp <- sim_df$config
mean_df <- stats::aggregate(pct ~ year + config, data = sim_df, FUN = mean); mean_df$grp <- mean_df$config
obs_line <- base::subset(traj, series == "Observed"); obs_line$grp <- "Observed"
coverage_note <- if (length(uncMC))
  sprintf("observed + capped = %s cells; uncapped = %s cells (own non-NULL footprint)",
          format(sum(mask_cap), big.mark = ",", scientific = FALSE),
          format(sum(mask_unc), big.mark = ",", scientific = FALSE)) else
  sprintf("observed + capped = %s shared cells", format(sum(mask_cap), big.mark = ",", scientific = FALSE))

g1 <- ggplot2::ggplot() +
  ggplot2::geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
  ggplot2::geom_line(data = sim_df,   ggplot2::aes(year, pct, group = interaction(config, mc), colour = grp),
            linewidth = 0.35, alpha = 0.30) +
  ggplot2::geom_line(data = mean_df,  ggplot2::aes(year, pct, colour = grp), linewidth = 1.3) +
  ggplot2::geom_line(data = obs_line, ggplot2::aes(year, pct, colour = grp), linewidth = 1.2) +
  ggplot2::scale_colour_manual(values = c(Observed = COL_OBS, Capped = COL_CAP, Uncapped = COL_UNC), name = NULL) +
  ggplot2::scale_y_continuous(labels = function(v) paste0(v, "%")) +
  ggplot2::labs(title = sprintf("%s - aboveground biomass change relative to %d", DISPLAY_NAME, BASE_YEAR),
       subtitle = paste0(coverage_note, "; thin = MC runs, bold = MC means / observed"),
       x = "Year", y = sprintf("AGB change vs %d", BASE_YEAR)) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_text(size = 12),
        plot.subtitle = ggplot2::element_text(size = 9))
ggplot2::ggsave(file.path(OUT_DIR, paste0(FILE_PREFIX, "_fig1_national_trajectory.png")), g1,
       width = 8.6, height = 5.2, dpi = 150)

###############################################################################
## 7b. FIGURE 1b : ABSOLUTE total AGB per configuration, BASE_YEAR..SIM_END_YEAR
##     Absolute geodesic Mt with observed/capped on their shared footprint and
##     uncapped on its own smaller footprint. Observed ends at END_YEAR.
###############################################################################
cat("Drawing Figure 1b (absolute total AGB) ...\n")

## Extend only to a year available in every included MC/configuration.
all_mcdirs <- c(capMC, uncMC)
max_available <- vapply(all_mcdirs, function(d) max(available_sim_years(d)), numeric(1))
sim_end  <- min(SIM_END_YEAR, min(max_available))
extra_yr <- if (sim_end > END_YEAR) (END_YEAR + 1):sim_end else integer(0)
cat(sprintf("   common simulation coverage ends %d -> plotting simulations to %d\n",
            min(max_available), sim_end))

## reuse the BASE_YEAR-END_YEAR totals already in `traj`, then read only extra years
extend_cfg <- function(cfg, mcdirs, m) {
  base <- traj[traj$series == "Simulated" & traj$config == cfg, c("year","total_Mt","mc")]
  out  <- data.frame()
  for (i in seq_along(mcdirs)) {
    b  <- base[base$mc == i, c("year","total_Mt")]
    ex <- if (length(extra_yr))
            data.frame(year = extra_yr,
                       total_Mt = sapply(extra_yr, function(y) sim_total(sim_vec(mcdirs[i], y), m)))
          else data.frame(year = integer(0), total_Mt = numeric(0))
    out <- rbind(out, data.frame(rbind(b, ex), series = "Simulated", config = cfg, mc = i))
    cat(sprintf("   %s MC %d/%d\r", cfg, i, length(mcdirs)))
  }
  cat("\n"); out
}
b_df <- extend_cfg("Capped", capMC, mask_cap)
if (length(uncMC)) b_df <- rbind(b_df, extend_cfg("Uncapped", uncMC, mask_unc))
b_df$grp   <- b_df$config
b_mean     <- stats::aggregate(total_Mt ~ year + config, data = b_df, FUN = mean); b_mean$grp <- b_mean$config
obs_b      <- traj[traj$series == "Observed", c("year","total_Mt")]; obs_b$grp <- "Observed"

# Figure 1b extends beyond the observation period, so preserve its underlying
# all-MC values and summary rather than leaving them only inside the PNG.
b_export <- rbind(
  b_df[, c("year", "total_Mt", "series", "config", "mc")],
  data.frame(year = obs_b$year, total_Mt = obs_b$total_Mt, series = "Observed",
             config = "Observed", mc = 0L)
)
b_export$scenario <- SCENARIO_LABEL
extended_groups <- split(b_df, interaction(b_df$year, b_df$config, drop = TRUE))
b_summary <- do.call(rbind, lapply(extended_groups, function(z) {
  finite_values <- z$total_Mt[is.finite(z$total_Mt)]
  data.frame(
    year = z$year[[1]], config = z$config[[1]], n_runs = length(finite_values),
    total_Mt_mean = if (length(finite_values)) mean(finite_values) else NA_real_,
    total_Mt_sd = if (length(finite_values) > 1L) stats::sd(finite_values) else NA_real_,
    scenario = SCENARIO_LABEL,
    row.names = NULL
  )
}))
utils::write.csv(
  b_export,
  file.path(OUT_DIR, paste0(FILE_PREFIX, "_total_AGB_extended_allMC.csv")),
  row.names = FALSE
)
utils::write.csv(
  b_summary,
  file.path(OUT_DIR, paste0(FILE_PREFIX, "_total_AGB_extended_summary.csv")),
  row.names = FALSE
)

## `sim_total()` deliberately returns NA when a future raster is incomplete on
## the fixed validation footprint. Remove those unavailable points explicitly
## before plotting so ggplot does not emit a generic "Removed rows" warning.
b_missing <- sum(!is.finite(b_df$total_Mt))
if (b_missing > 0L)
  cat(sprintf("   Figure 1b: %d unavailable future MC-year totals omitted (fixed footprint retained).\n",
              b_missing))
b_df_plot <- b_df[is.finite(b_df$total_Mt), , drop = FALSE]
b_mean_plot <- b_mean[is.finite(b_mean$total_Mt), , drop = FALSE]
obs_b_plot <- obs_b[is.finite(obs_b$total_Mt), , drop = FALSE]

g1b <- ggplot2::ggplot() +
  ggplot2::geom_line(data = b_df_plot, ggplot2::aes(year, total_Mt, group = interaction(config, mc), colour = grp), linewidth = 0.35, alpha = 0.30) +
  ggplot2::geom_line(data = b_mean_plot, ggplot2::aes(year, total_Mt, colour = grp), linewidth = 1.3) +
  ggplot2::geom_line(data = obs_b_plot, ggplot2::aes(year, total_Mt, colour = grp), linewidth = 1.2) +
  ggplot2::geom_vline(xintercept = END_YEAR, linetype = "dotted", colour = "grey55") +
  ggplot2::scale_colour_manual(values = c(Observed = COL_OBS, Capped = COL_CAP, Uncapped = COL_UNC), name = NULL) +
  ggplot2::labs(title = sprintf("%s - total aboveground biomass by configuration, %d-%d", DISPLAY_NAME, BASE_YEAR, sim_end),
       subtitle = sprintf("%s; observed ends %d (dotted); thin = MC runs, bold = means",
                          coverage_note, END_YEAR),
       x = "Year", y = "Total AGB (Mt dry matter)") +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_text(size = 12),
                 plot.subtitle = ggplot2::element_text(size = 9))
ggplot2::ggsave(file.path(OUT_DIR, paste0(FILE_PREFIX, "_fig1b_total_AGB.png")), g1b,
                width = 9, height = 5.4, dpi = 150)

###############################################################################
## 8. FIGURE 2 : spatial maps of AGB change (debugging_1), each on its OWN footprint
###############################################################################
cat("Drawing Figure 2 (change maps) ...\n")
mk   <- function(vec) { r <- terra::rast(ref); terra::values(r) <- vec; r }
rObs <- mk(dObs); rCap <- mk(dCap); rUnc <- if (!is.null(dUnc)) mk(dUnc) else NULL

panel_ids <- c("A", "B", "C")[seq_len(2L + !is.null(dUnc))]

te   <- terra::ext(terra::trim(rObs))
rObs <- terra::crop(rObs, te)
rCap <- terra::crop(rCap, te)
if (!is.null(rUnc)) rUnc <- terra::crop(rUnc, te)
to_df <- function(r, panel_id) { d <- as.data.frame(r, xy = TRUE, na.rm = FALSE); names(d)[3] <- "value"; d$panel <- panel_id; d }
map_df <- to_df(rObs, panel_ids[1]); map_df <- rbind(map_df, to_df(rCap, panel_ids[2]))
if (!is.null(rUnc)) map_df <- rbind(map_df, to_df(rUnc, panel_ids[3]))
map_df$panel <- factor(map_df$panel, levels = panel_ids); np <- nlevels(map_df$panel)

map_values <- c(dObs[is.finite(dObs)], dCap[is.finite(dCap)])
if (!is.null(dUnc)) map_values <- c(map_values, dUnc[is.finite(dUnc)])
vmax <- as.numeric(stats::quantile(abs(map_values), 0.98, na.rm = TRUE))
if (!is.finite(vmax) || vmax <= 0) vmax <- 1
map_df$value <- pmax(pmin(map_df$value, vmax), -vmax)
divpal <- grDevices::colorRampPalette(c("#a50026","#d73027","#f46d43","#fdae61","#fee08b",
                                         "#ffffbf","#d9ef8b","#a6d96a","#66bd63","#1a9850","#006837"))(100)

adm_layer <- NULL
if (file.exists(admin_path)) {
  adm2 <- load_country_boundary(admin_path, COUNTRY_ISO3)
  if (!terra::same.crs(adm2, ref)) adm2 <- terra::project(adm2, terra::crs(ref))
  gdf  <- as.data.frame(terra::geom(adm2))
  adm_layer <- ggplot2::geom_polygon(
    data = gdf, ggplot2::aes(x = x, y = y, group = interaction(geom, part)),
    inherit.aes = FALSE, fill = NA, colour = "grey20", linewidth = 0.18
  )
}
asp <- as.numeric(
  (terra::xmax(te) - terra::xmin(te)) / (terra::ymax(te) - terra::ymin(te))
)
maph <- 4.4
panel_labels <- data.frame(
  panel = factor(panel_ids, levels = panel_ids),
  x = terra::xmin(te) + 0.025 * (terra::xmax(te) - terra::xmin(te)),
  y = terra::ymax(te) - 0.025 * (terra::ymax(te) - terra::ymin(te))
)

g2 <- ggplot2::ggplot(map_df, ggplot2::aes(x, y, fill = value)) +
  ggplot2::geom_raster() + adm_layer + ggplot2::facet_wrap(~ panel, nrow = 1) +
  ggplot2::geom_label(
    data = panel_labels, ggplot2::aes(x = x, y = y, label = panel),
    inherit.aes = FALSE, hjust = 0, vjust = 1, fontface = "bold", size = 5,
    fill = "white", colour = "black", linewidth = 0.2,
    label.padding = grid::unit(0.18, "lines")
  ) +
  ggplot2::scale_fill_gradientn(colours = divpal, limits = c(-vmax, vmax), na.value = "grey85", name = "AGB change\n(MgDM/ha)") +
  ggplot2::coord_equal(xlim = c(terra::xmin(te), terra::xmax(te)),
                       ylim = c(terra::ymin(te), terra::ymax(te)), expand = FALSE) +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::guides(fill = ggplot2::guide_colourbar(barheight = grid::unit(3.2, "cm"), title.position = "top")) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "grey85", colour = NA), panel.spacing = grid::unit(3, "pt"),
        strip.text = ggplot2::element_blank(), strip.background = ggplot2::element_blank(),
        legend.title = ggplot2::element_text(size = 9))
ggplot2::ggsave(file.path(OUT_DIR, paste0(FILE_PREFIX, "_fig2_change_maps.png")), g2,
       width = np * maph * asp + 1.7, height = maph + 0.3, dpi = 150)
cat("   Figure 2 saved.\n")

###############################################################################
## 9. FIGURE 3 : pixel-level simulated vs observed change (debugging_1, pairwise)
###############################################################################
cat("Drawing Figure 3 (pixel scatter) ...\n")
mksc <- function(dsim, m, lab) { o <- dObs[m]; s <- dsim[m]; k <- is.finite(o) & is.finite(s)
                                 data.frame(obs = o[k], sim = s[k], config = lab) }
sc_df <- mksc(dCap, mask_cap, "MoFuSS - capped regrowth")
if (!is.null(dUnc)) sc_df <- rbind(sc_df, mksc(dUnc, mask_unc, "MoFuSS - uncapped regrowth"))
lim <- range(c(sc_df$obs, sc_df$sim), finite = TRUE)

g3 <- ggplot2::ggplot(sc_df, ggplot2::aes(obs, sim)) +
  ggplot2::geom_bin2d(bins = 120) + ggplot2::scale_fill_viridis_c(trans = "log10", name = "pixel count") +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey25") +
  ggplot2::geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 0.7, formula = y ~ x) +
  ggplot2::coord_equal(xlim = lim, ylim = lim) + ggplot2::facet_wrap(~ config) +
  ggplot2::labs(title = sprintf("%s - pixel-level simulated vs observed AGB change %d-%d (debugging_1; dashed = 1:1)",
                       DISPLAY_NAME, BASE_YEAR, END_YEAR),
       x = "Observed dAGB (MgDM/ha)", y = "Simulated dAGB (MgDM/ha)") +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 11))
ggplot2::ggsave(file.path(OUT_DIR, paste0(FILE_PREFIX, "_fig3_pixel_scatter.png")), g3,
                width = 11, height = 5.2, dpi = 150)

###############################################################################
## 10. SUMMARY (one observed series; configuration-specific endpoint footprints)
###############################################################################
pct_change <- function(a, b) if (length(a) && is.finite(a) && a != 0) 100 * (b - a) / a else NA_real_
summ_row <- function(cfg, ser, cells) {
  d <- traj[traj$config == cfg & traj$series == ser, ]
  a <- stats::aggregate(total_Mt ~ year, d, mean)
  y0 <- a$total_Mt[a$year == BASE_YEAR]; y1 <- a$total_Mt[a$year == END_YEAR]
  data.frame(config = cfg, series = ser, cells = cells, start_year = BASE_YEAR,
             end_year = END_YEAR, AGB_start_Mt = y0, AGB_end_Mt = y1,
             net_change_Mt = y1 - y0, net_change_pct = pct_change(y0, y1))
}
summ <- rbind(summ_row("Observed", "Observed", sum(mask_cap)),
              summ_row("Capped", "Simulated", sum(mask_cap)))
if (length(uncMC)) summ <- rbind(summ, summ_row("Uncapped", "Simulated", sum(mask_unc)))
summ$scenario <- SCENARIO_LABEL
utils::write.csv(summ, file.path(OUT_DIR, paste0(FILE_PREFIX, "_national_summary.csv")), row.names = FALSE)

cat("\n================  SUMMARY  (", DISPLAY_NAME, ", ", BASE_YEAR, "-", END_YEAR, ")  ================\n", sep = "")
print(summ, row.names = FALSE)
cat("\nPixel-level change agreement (debugging_1, pairwise):\n")
print(stats[, c("config","n","pearson_r","rmse_MgDMha","bias_MgDMha")], row.names = FALSE, digits = 3)
cat("\nGross/net NRB aggregates (full endpoint scopes; MC values summarized):\n")
print(nrb_summary, row.names = FALSE, digits = 5)
cat("\nOutputs written to:\n  ", OUT_DIR, "\n", sep = "")
cat("Done.\n")
invisible(normalizePath(OUT_DIR, winslash = "/", mustWork = TRUE))
}

###############################################################################
## 11. BATCH EXECUTION
###############################################################################
run_metadata$scenario_id <- vapply(run_metadata$scenario, safe_id, character(1))
scenario_ids <- unique(run_metadata$scenario_id[order(match(run_metadata$role, c("bau", "ccts")))])
run_metadata$output_dir <- file.path(validation_root, run_metadata$scenario_id)
message("\nResolved four-run AGB validation batch:")
print(run_metadata[, c("scenario", "role", "mode", "working_dir", "output_dir")], row.names = FALSE)
failures <- character()
if (DRY_RUN) {
  message("DRY RUN complete; no validation outputs were changed.")
} else {
  for (scenario_id in scenario_ids) {
    pair <- run_metadata[run_metadata$scenario_id == scenario_id, , drop = FALSE]
    if (nrow(pair) != 2L || !setequal(pair$mode, c("capped", "uncapped"))) {
      failures <- c(failures, sprintf("%s: expected exactly one capped and one uncapped folder", scenario_id))
      next
    }
    capped_dir <- pair$working_dir[pair$mode == "capped"][[1]]
    uncapped_dir <- pair$working_dir[pair$mode == "uncapped"][[1]]
    scenario_label <- pair$scenario[[1]]
    message("\n=== AGB validation pair: ", scenario_label, " ===")
    result <- tryCatch(
      run_validation_pair(
        COUNTRY = run_metadata$country[[1]],
        COUNTRY_ISO3 = run_metadata$iso3[[1]],
        SCENARIO_LABEL = scenario_label,
        MODEL_END_YEAR = run_metadata$model_end[[1]],
        MC_RUNS = run_metadata$mc_runs[[1]],
        CAPPED_DIR = capped_dir,
        UNCAPPED_DIR = uncapped_dir,
        OUT_DIR = pair$output_dir[[1]]
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      failure <- sprintf("%s: %s", scenario_label, conditionMessage(result))
      failures <- c(failures, failure)
      message("FAILED: ", failure)
    }
  }
  if (length(failures)) {
    stop("One or more AGB validation pairs failed:\n  ", paste(failures, collapse = "\n  "), call. = FALSE)
  }
  message("\nAll BAU and intervention AGB validation pairs completed: ", validation_root)
}
###############################################################################
