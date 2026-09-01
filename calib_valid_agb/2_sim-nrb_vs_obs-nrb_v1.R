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
# Script: 2_sim-nrb_vs_obs-nrb_v1.R
# Version: 1
# Date: August 2026
# Execution: Use regular RStudio Source when aoi_mode = "draw" so the Shiny
# selection map can run interactively. With aoi_mode = "country" or "full",
# the script can also be run directly with Rscript from PowerShell/a terminal.
#
# Purpose: Compare MC1 MoFuSS woodfuel harvest and non-renewable biomass with
# observed CTrees endpoint AGB change for BAU/CCTS x capped/uncapped runs.
# Inputs: Four completed MoFuSS working folders, their debugging_1 stock and
# harvest rasters, CTrees endpoint AGB rasters, and an admin-region GeoPackage.
# Outputs: One complete result set per working folder under the corresponding
# <drive>/mofuss_postprocessing/<analysis>/validation directory.
# Side effects: Cleanly rebuilds only its exact per-configuration validation
# output folders; it does not modify the MoFuSS working folders.

# 2dolist ----

# Validation method notes ----

# =============================================================================
#  MoFuSS validation against observed AGB dynamics (CTrees)
#  v5  -  fixed 2010-2020 CTrees versus MoFuSS MC1 fNRB comparison
# =============================================================================
#
#  READ ME FIRST  ------------------------------------------------------------
#
#  WHAT IT DOES
#    Compares Non-Renewable Biomass (NRB) between:
#      * MoFuSS  - MODELLED woodfuel harvest / NRB from debugging_1 only
#      * CTrees  - OBSERVED AGB change from third-party maps (2010 / 2020)
#
#  NRB DEFINITION USED HERE
#      CTrees gross NRB = sum of positive 2010-2020 endpoint losses.
#      CTrees net NRB   = positive part of the signed regional endpoint balance.
#      MoFuSS NRB       = positive endpoint loss from MC1, following MoFuSS's
#                         own 2010-2020 post-processing convention.
#      Intermediate years do not enter the NRB numerators. `nrb_threshold` is
#      used only for pixel-agreement diagnostics and maps, never for fNRB totals.
#
#  WHY AGREEMENT IS ONLY PARTIAL
#      MoFuSS models ONLY woodfuel harvest. Observed change also includes land
#      clearing, fire, etc. Agreement is therefore expected to be closer in
#      areas where woodfuel is a dominant driver - which is exactly why you can
#      restrict the comparison to an Area Of Interest (AOI) drawn on a map.
#
#  WORKFLOW  (runs top-to-bottom, NO manual step in the middle)
#      1. CONFIG    - resolution, country, years/period, AOI mode, threshold
#      2. LOAD      - MoFuSS MC1 endpoint stocks + annual harvest; CTrees AGB
#      3. OBSERVED  - observed AGB loss as a DENSITY (Mg/ha), reprojected
#      4. AOI       - draw a box on a Leaflet map (returns automatically)
#      5. ALIGN     - resample density to MoFuSS grid, then -> Mg/pixel;
#                     retain source footprints for maps and a common footprint
#                     for pixel-by-pixel diagnostics
#      6. METRICS   - magnitude (r/RMSE/MAE, co-detected) + loss/no-loss
#                     agreement (POD/FAR/CSI) + gross/net NRB + three fNRBs
#      7. OUTPUTS   - summary CSV + 4-panel PNG + 4 GeoTIFFs in the MoFuSS Out*
#
#  MASS CONSISTENCY (why the observed side stays a density until the end)
#      Pixel-comparison layers keep observed change in Mg/ha (intensive) through
#      reprojection/resampling and convert to Mg/pixel only on the MoFuSS grid.
#      CTrees gross/net totals are calculated separately on the native CTrees
#      grid with geodesic cell areas, matching the validated observed app.
#
#  ONE DENOMINATOR FOR ALL fNRB VALUES
#      Demand is the total MC1 MoFuSS harvest during 2010-2020 inside the selected
#      country/AOI (Harvest_tot11.tif ... Harvest_tot20.tif). The exact same Mg
#      denominator is used for CTrees gross fNRB, CTrees net fNRB, and MoFuSS
#      fNRB. Percentages are not capped at 100.
#
#  THE OLD PAIN POINT (now fixed)
#      The AOI map used to require running a Shiny app BY HAND in the middle of
#      the script (there was a `stop("Run the shiny app")`). It is now a normal
#      blocking function: the map opens, you draw a rectangle, click a button,
#      and the script continues on its own. If you don't want to draw, set
#      `aoi_mode` to "country" or "full" in the CONFIG block and no map opens.
#
# =============================================================================


# Load libraries ----

# =============================================================================
# 0. PACKAGES
# =============================================================================
# Trimmed to what is actually used. terra handles all raster + vector work,
# so the old sp / raster / rgdal / sf stack is no longer needed.
library(terra)          # rasters + vectors
library(shiny)          # blocking AOI map
library(leaflet)        # the map itself
library(leaflet.extras) # rectangle draw toolbar


# Internal parameters ----

# =============================================================================
# 1. CONFIG  -  the only block you normally need to edit
# =============================================================================

# --- 1a. Pipeline-supplied inputs -------------------------------------------
# Paths and user-facing parameters are supplied by
# 0calib_valid_agb_pipeline_v1.R. Empty/NA defaults prevent stale paths or
# hidden spin-up conventions from being used when this stage is moved.
WORKING_DIRS <- character()
SPINUP_YEARS <- NA_integer_
CTREES_DIR <- ""
ADMIN_VECTOR <- ""
POSTPROCESSING_ROOT <- ""      # inferred from the common working-folder parent
DRY_RUN <- FALSE

# --- 1b. Resolution switch --------------------------------------------------
# "1km"  -> use the 1 km MoFuSS output and 1 km CTrees maps (this dataset).
# "100m" -> use a genuine 100 m MoFuSS output + 100 m CTrees maps (if you have
#            them). `agg_factor` can then coarsen 100 m -> 1 km before comparing
#            (set to 10) or keep native 100 m (set to 1).
resolution <- "1km"        # "1km" or "100m"

agg_factor <- 1L             # set to 10 only for genuine 100 m simulations

START_YEAR <- 2010L
END_YEAR <- 2020L
aoi_mode <- "country"
square_draw_aoi <- TRUE
nrb_threshold <- 100
ctrees_units <- "CO2"

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
      "Usage: Rscript 2_sim-nrb_vs_obs-nrb_v1.R [options]\n",
      "  --working-dir=DIR       Repeat exactly four times\n",
      "  --spinup-years=N\n",
      "  --ctrees-dir=DIR         CTrees fNRB observation folder\n",
      "  --admin-vector=GPKG\n",
      "  --start-year=YYYY --end-year=YYYY\n",
      "  --resolution=1km|100m --agg-factor=N\n",
      "  --aoi-mode=country|full|draw\n",
      "  --square-draw-aoi=true|false\n",
      "  --nrb-threshold=N --ctrees-units=CO2|C\n",
      "  --dry-run\n"
    ))
    quit(save = "no", status = 0L, runLast = FALSE)
  } else if (startsWith(arg, "--working-dir=")) {
    WORKING_DIRS <- c(WORKING_DIRS, value("--working-dir="))
  } else if (startsWith(arg, "--spinup-years=")) {
    SPINUP_YEARS <- parse_integer(value("--spinup-years="), "--spinup-years")
  } else if (startsWith(arg, "--ctrees-dir=")) {
    CTREES_DIR <- value("--ctrees-dir=")
  } else if (startsWith(arg, "--admin-vector=")) {
    ADMIN_VECTOR <- value("--admin-vector=")
  } else if (startsWith(arg, "--start-year=")) {
    START_YEAR <- parse_integer(value("--start-year="), "--start-year")
  } else if (startsWith(arg, "--end-year=")) {
    END_YEAR <- parse_integer(value("--end-year="), "--end-year")
  } else if (startsWith(arg, "--resolution=")) {
    resolution <- value("--resolution=")
  } else if (startsWith(arg, "--agg-factor=")) {
    agg_factor <- parse_integer(value("--agg-factor="), "--agg-factor", 1L)
  } else if (startsWith(arg, "--aoi-mode=")) {
    aoi_mode <- tolower(value("--aoi-mode="))
  } else if (startsWith(arg, "--square-draw-aoi=")) {
    square_draw_aoi <- parse_bool(value("--square-draw-aoi="), "--square-draw-aoi")
  } else if (startsWith(arg, "--nrb-threshold=")) {
    nrb_threshold <- suppressWarnings(as.numeric(value("--nrb-threshold=")))
  } else if (startsWith(arg, "--ctrees-units=")) {
    ctrees_units <- toupper(value("--ctrees-units="))
  } else if (identical(arg, "--dry-run")) {
    DRY_RUN <- TRUE
  } else if (startsWith(arg, "--")) {
    stopf("Unknown option: %s", arg)
  } else {
    WORKING_DIRS <- c(WORKING_DIRS, arg)
  }
}

if (is.na(SPINUP_YEARS)) stop("--spinup-years is required.", call. = FALSE)
if (!dir.exists(CTREES_DIR)) stopf("CTrees fNRB directory does not exist: %s", CTREES_DIR)
ctrees_dir <- normalizePath(CTREES_DIR, winslash = "/", mustWork = TRUE)
if (!file.exists(ADMIN_VECTOR) || dir.exists(ADMIN_VECTOR)) {
  stopf("Admin GeoPackage does not exist: %s", ADMIN_VECTOR)
}
ADMIN_VECTOR <- normalizePath(ADMIN_VECTOR, winslash = "/", mustWork = TRUE)
if (!resolution %in% c("1km", "100m")) stop("--resolution must be 1km or 100m.", call. = FALSE)
if (!aoi_mode %in% c("country", "full", "draw")) {
  stop("--aoi-mode must be country, full or draw.", call. = FALSE)
}
if (!is.finite(nrb_threshold) || nrb_threshold < 0) {
  stop("--nrb-threshold must be a non-negative number.", call. = FALSE)
}
if (!ctrees_units %in% c("CO2", "C")) stop("--ctrees-units must be CO2 or C.", call. = FALSE)
if (END_YEAR <= START_YEAR) stop("--end-year must be later than --start-year.", call. = FALSE)

find_parameters_file <- function(workdir) {
  root <- file.path(workdir, "LULCC", "DownloadedDatasets")
  files <- list.files(root, pattern = "^parameters.*\\.csv$", recursive = TRUE,
                      full.names = TRUE, ignore.case = TRUE)
  if (length(files) != 1L) {
    stopf("Expected exactly one parameters*.csv below %s; found %d.", root, length(files))
  }
  files[[1]]
}
read_run_metadata <- function(workdir) {
  workdir <- normalizePath(workdir, winslash = "/", mustWork = TRUE)
  path <- find_parameters_file(workdir)
  x <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (ncol(x) < 2L) stopf("Parameter table has fewer than two columns: %s", path)
  keys <- trimws(as.character(x[[1]]))
  vals <- trimws(as.character(x[[2]]))
  value <- function(key) {
    z <- vals[keys == key]
    z <- z[!is.na(z) & nzchar(z)]
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
    stopf("Could not classify scenario_ver '%s' as BAU or CCTS in %s.", scenario, path)
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
    model_start = int_value("start_year"),
    model_end = int_value("end_year"),
    mc_runs = int_value("monte_carlo_runs"),
    gee_scale = gee_scale,
    stringsAsFactors = FALSE
  )
}

WORKING_DIRS <- unique(as.character(WORKING_DIRS[nzchar(WORKING_DIRS)]))
if (length(WORKING_DIRS) != 4L) {
  stopf("This batch requires exactly four working folders; received %d.", length(WORKING_DIRS))
}
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
  stopf("Expected one BAU/CCTS x capped/uncapped folder; found: %s", paste(combos, collapse = ", "))
}
scenario_count_by_role <- vapply(split(run_metadata$scenario, run_metadata$role),
                                 function(x) length(unique(tolower(x))), integer(1))
if (any(scenario_count_by_role != 1L)) {
  stop("Capped and uncapped folders must use the same scenario_ver within BAU and CCTS.", call. = FALSE)
}
country_iso3 <- run_metadata$iso3[[1]]
common_parent <- unique(path_key(dirname(run_metadata$working_dir)))
if (length(common_parent) != 1L) stop("All four working folders must share one parent.", call. = FALSE)
working_parent <- dirname(run_metadata$working_dir[[1]])
postprocessing_root <- if (nzchar(POSTPROCESSING_ROOT)) POSTPROCESSING_ROOT else
  file.path(working_parent, "mofuss_postprocessing")
analysis_id <- paste(
  safe_id(country_iso3), run_metadata$model_start[[1]] + SPINUP_YEARS,
  run_metadata$model_end[[1]], paste0("mc", run_metadata$mc_runs[[1]]), sep = "_"
)
analysis_root <- normalizePath(file.path(postprocessing_root, analysis_id), winslash = "/", mustWork = FALSE)
validation_root <- file.path(analysis_root, "validation", "2_sim_nrb_vs_obs_nrb")

# --- 1c. Comparison period (fixed common window) ---------------------------
ctrees_file1 <- sprintf("ctrees_global_%d_AGC.tif", START_YEAR)
ctrees_file2 <- sprintf("ctrees_global_%d_AGC.tif", END_YEAR)

# MoFuSS file codes are derived from the simulation start year. For a 2000
# simulation this resolves to Growth11, Growth_less_harv20 and Harvest_tot11:20.
MOFUSS_START_INDEX <- START_YEAR - run_metadata$model_start[[1]] + 1L
MOFUSS_END_INDEX <- END_YEAR - run_metadata$model_start[[1]]
MOFUSS_HARVEST_INDEX <- MOFUSS_START_INDEX:MOFUSS_END_INDEX
if (MOFUSS_START_INDEX < 1L || MOFUSS_END_INDEX < MOFUSS_START_INDEX) {
  stop("The requested validation period is outside the simulation period.", call. = FALSE)
}

# --- 1f. CTrees units (IMPORTANT - verify before trusting absolute fNRB) ----
# CTrees rasters are named *_AGC = Above-Ground CARBON. Converting to biomass
# (dry matter) depends on their true units, and the band carries NO unit
# metadata. A sample over Zambia was ~10-240 (median ~120), which is plausible
# under either interpretation, so this cannot be resolved from the data alone:
#   "CO2" -> agb = value * (12/44) / 0.47   <- your original v3 setting
#   "C"   -> agb = value / 0.47
# Check the CTrees documentation and set this accordingly.
agc_to_agb   <- if (ctrees_units == "C") 1 / 0.47 else (12 / 44) / 0.47

run_one_validation <- function(mofuss_dir, run_label, out_dir) {
message("\n=== NRB validation: ", run_label, " ===")
message("Working folder: ", mofuss_dir)
if (!identical(path_key(dirname(out_dir)), path_key(validation_root)) ||
    !identical(basename(out_dir), run_label)) {
  stop("Refusing unsafe validation output path: ", out_dir, call. = FALSE)
}

# =============================================================================
# 2. HELPER FUNCTIONS
# =============================================================================

# Load the exact MoFuSS MC1 2010-2020 NRB and harvest ------------------------
load_mofuss_mc1 <- function(dir) {
  mc1_dir <- file.path(dir, "debugging_1")
  if (!dir.exists(mc1_dir)) stop("MoFuSS MC1 folder not found: ", mc1_dir)

  start_file <- file.path(mc1_dir, sprintf("Growth%02d.tif", MOFUSS_START_INDEX))
  end_file <- file.path(mc1_dir, sprintf("Growth_less_harv%02d.tif", MOFUSS_END_INDEX))
  harvest_files <- file.path(mc1_dir, sprintf("Harvest_tot%02d.tif", MOFUSS_HARVEST_INDEX))
  required <- c(start_file, end_file, harvest_files)
  missing <- required[!file.exists(required)]
  if (length(missing)) stop("Missing MoFuSS MC1 input(s):\n", paste(missing, collapse = "\n"))

  agb_start <- terra::rast(start_file)
  agb_end <- terra::rast(end_file)
  harvest_stack <- terra::rast(harvest_files)
  if (!terra::compareGeom(agb_start, agb_end, harvest_stack, stopOnError = FALSE))
    stop("MoFuSS MC1 endpoint and harvest rasters do not share one geometry.")

  signed_change <- agb_start - agb_end
  nrb <- terra::ifel(is.na(signed_change), NA,
                     terra::ifel(signed_change > 0, signed_change, 0))
  harvest <- terra::app(harvest_stack, fun = sum)
  harvest[harvest < 0] <- NA

  list(nrb = nrb, harvest = harvest,
       agb_start = agb_start, agb_end = agb_end)
}

# Parse a 4-digit year from a CTrees file name (for labels / output names) ---
# Falls back to the file name (without extension) if no year is present.
ctrees_label <- function(fname) {
  y <- regmatches(fname, regexpr("(19|20)\\d{2}", fname))
  if (length(y)) y else tools::file_path_sans_ext(basename(fname))
}

# Load one CTrees AGB map, crop + mask to country, convert AGC -> AGB --------
load_ctrees_agb <- function(fname, country_vect) {
  r <- terra::rast(file.path(ctrees_dir, fname))
  country_r_crs <- country_vect
  if (!terra::same.crs(country_r_crs, r))
    country_r_crs <- terra::project(country_r_crs, terra::crs(r))
  r <- terra::crop(r, country_r_crs)
  r <- terra::mask(r, country_r_crs)
  r <- r * agc_to_agb
  r[r < 0] <- NA               # -9999 nodata and any negatives
  r
}

sum_raster <- function(r) {
  value <- as.numeric(terra::global(r, "sum", na.rm = TRUE)[1, 1])
  if (!is.finite(value)) stop("Raster sum is not finite.")
  value
}

mask_region <- function(r, region) {
  if (is.null(region)) return(r)
  region_r_crs <- region
  if (!terra::same.crs(region_r_crs, r))
    region_r_crs <- terra::project(region_r_crs, terra::crs(r))
  terra::mask(terra::crop(r, region_r_crs), region_r_crs)
}

make_square_region <- function(region) {
  e <- terra::ext(region)
  width <- terra::xmax(e) - terra::xmin(e)
  height <- terra::ymax(e) - terra::ymin(e)
  side <- max(width, height)
  if (!is.finite(side) || side <= 0) stop("The drawn AOI has zero or invalid size.")

  centre_x <- (terra::xmin(e) + terra::xmax(e)) / 2
  centre_y <- (terra::ymin(e) + terra::ymax(e)) / 2
  half_side <- side / 2
  square_ext <- terra::ext(centre_x - half_side, centre_x + half_side,
                           centre_y - half_side, centre_y + half_side)
  terra::as.polygons(square_ext, crs = terra::crs(region))
}

# Interactive AOI selection (BLOCKING) --------------------------------------
# Opens a Leaflet map. Draw ONE rectangle, then click "Use this area".
# Returns a rectangular SpatVector reprojected into `target_crs`, or NULL if
# the user chose "Use whole overlap". The script pauses and resumes on click.
select_aoi_draw <- function(zoom_bbox, target_crs) {

  if (!interactive()) {
    message("Non-interactive session: skipping the draw map, using full overlap.")
    return(NULL)
  }

  ui <- fluidPage(
    tags$style(HTML("#map {height: calc(100vh - 70px) !important;}")),
    leafletOutput("map"),
    div(style = "padding:6px 10px;",
        if (isTRUE(square_draw_aoi))
          helpText("Draw any rectangle. Its shorter side will be expanded around the same centre to create a square analysis area."),
        textOutput("coords"),
        actionButton("use",   "Use this area", class = "btn-primary"),
        actionButton("whole", "Use whole overlap"))
  )

  server <- function(input, output, session) {
    rv <- reactiveValues(bb = NULL)

    output$map <- renderLeaflet({
      leaflet() |>
        addProviderTiles(providers$Esri.NatGeoWorldMap) |>
        fitBounds(zoom_bbox[["xmin"]], zoom_bbox[["ymin"]],
                  zoom_bbox[["xmax"]], zoom_bbox[["ymax"]]) |>
        addDrawToolbar(
          targetGroup      = "draw",
          rectangleOptions = drawRectangleOptions(),
          polylineOptions  = FALSE, polygonOptions      = FALSE,
          circleOptions    = FALSE, markerOptions       = FALSE,
          circleMarkerOptions = FALSE,
          editOptions      = editToolbarOptions())
    })

    observeEvent(input$map_draw_new_feature, {
      coords <- input$map_draw_new_feature$geometry$coordinates[[1]]
      xs <- vapply(coords, function(p) p[[1]], numeric(1))
      ys <- vapply(coords, function(p) p[[2]], numeric(1))
      rv$bb <- c(xmin = min(xs), ymin = min(ys),
                 xmax = max(xs), ymax = max(ys))
      output$coords <- renderText(
        sprintf("Box (lon/lat):  %.3f, %.3f  ->  %.3f, %.3f",
                rv$bb[["xmin"]], rv$bb[["ymin"]],
                rv$bb[["xmax"]], rv$bb[["ymax"]]))
    })

    observeEvent(input$use,   stopApp(rv$bb))   # returns the drawn bbox (or NULL)
    observeEvent(input$whole, stopApp(NULL))    # explicit "whole overlap"
    session$onSessionEnded(function() stopApp(isolate(rv$bb)))
  }

  bb <- shiny::runApp(shinyApp(ui, server))     # <-- BLOCKS until a button click
  if (is.null(bb)) return(NULL)

  # Reproject the lon/lat box into the MoFuSS CRS.
  poly <- terra::as.polygons(
    terra::ext(bb[["xmin"]], bb[["xmax"]], bb[["ymin"]], bb[["ymax"]]),
    crs = "EPSG:4326")
  poly_target <- terra::project(poly, target_crs)
  if (isTRUE(square_draw_aoi)) {
    poly_target <- make_square_region(poly_target)
    side_km <- (terra::xmax(poly_target) - terra::xmin(poly_target)) / 1000
    message(sprintf("Drawn AOI expanded to a %.1f x %.1f km square.", side_km, side_km))
  }
  poly_target
}


# =============================================================================
# 3. LOAD  -  MoFuSS (modelled) and CTrees (observed)
# =============================================================================

# --- MoFuSS MC1 NRB + harvest for the fixed 2010-2020 period ----------------
mofuss_mc1  <- load_mofuss_mc1(mofuss_dir)
nrb_mofuss  <- mofuss_mc1$nrb
harv_mofuss <- mofuss_mc1$harvest
target_crs  <- terra::crs(nrb_mofuss)          # World Mercator for this dataset

# --- Country polygon --------------------------------------------------------
ctry     <- terra::vect(ADMIN_VECTOR)
ctry_sel <- ctry[ctry$GID_0 == country_iso3, ]
if (nrow(ctry_sel) != 1L)
  stop("Expected one country polygon for ", country_iso3, "; found ", nrow(ctry_sel), ".")

# --- CTrees observed AGB, two maps ------------------------------------------
agb_y1 <- load_ctrees_agb(ctrees_file1, ctry_sel)
agb_y2 <- load_ctrees_agb(ctrees_file2, ctry_sel)

# years parsed from the file names, used for plot titles and output names
ctrees_year1 <- ctrees_label(ctrees_file1)
ctrees_year2 <- ctrees_label(ctrees_file2)


# =============================================================================
# 4. OBSERVED AGB LOSS (kept as a DENSITY until the final grid)
# =============================================================================
# loss density is Mg/ha (INTENSIVE), so it can be reprojected and resampled with
# bilinear WITHOUT breaking mass conservation. It is turned into a per-pixel
# total (Mg/pixel) only at the very end, on the MoFuSS grid (section 6), by
# multiplying by the true area of each MoFuSS cell.
loss_mgha      <- agb_y1 - agb_y2                        # Mg/ha  (+ = loss, - = gain)
loss_mgha_proj <- terra::project(loss_mgha, target_crs) # density in the MoFuSS CRS


# =============================================================================
# 5. AOI  -  pick the comparison area
# =============================================================================
# Country bounds in lon/lat, used only to zoom the draw map.
# NOTE: use the accessor functions xmin()/xmax()/ymin()/ymax() rather than
# `e$xmin` - `$` on a SpatExtent returns NULL on some terra versions, which
# would silently drop the names and trigger "subscript out of bounds" later.
ctry_ll   <- terra::project(ctry_sel, "EPSG:4326")
e_ll      <- terra::ext(ctry_ll)
zoom_bbox <- c(xmin = terra::xmin(e_ll), ymin = terra::ymin(e_ll),
               xmax = terra::xmax(e_ll), ymax = terra::ymax(e_ll))

aoi_region <- switch(
  aoi_mode,
  draw    = select_aoi_draw(zoom_bbox, target_crs),
  country = terra::project(ctry_sel, target_crs),
  full    = NULL,
  stop("aoi_mode must be 'draw', 'country' or 'full'")
)

# NULL means the full MoFuSS x CTrees overlap, either by configuration or
# because the user clicked "Use whole overlap" in draw mode.
aoi_ext <- if (is.null(aoi_region)) {
  terra::intersect(terra::ext(nrb_mofuss), terra::ext(loss_mgha_proj))
} else {
  terra::ext(aoi_region)
}

# --- Aggregate CTrees gross/net NRB on the native observed grid ------------
# This matches the validated observed app: endpoint difference only, common
# valid endpoint cells, geodesic hectares, and no stability threshold.
agb_y1_region <- mask_region(agb_y1, aoi_region)
agb_y2_region <- mask_region(agb_y2, aoi_region)
ctrees_change_mg <- (agb_y1_region - agb_y2_region) *
  terra::cellSize(agb_y1_region, unit = "ha")
ctrees_common_cells <- as.numeric(
  terra::global(!is.na(ctrees_change_mg), "sum", na.rm = TRUE)[1, 1]
)
if (!is.finite(ctrees_common_cells) || ctrees_common_cells == 0)
  stop("No common valid CTrees cells for 2010 and 2020 in the selected region.")

observed_gross_nrb <- sum_raster(
  terra::ifel(ctrees_change_mg > 0, ctrees_change_mg, 0)
)
observed_balance <- sum_raster(ctrees_change_mg) # positive = loss; negative = gain
observed_net_nrb <- max(0, observed_balance)


# =============================================================================
# 6. ALIGN  -  put everything on the MoFuSS grid, mass-consistently
# =============================================================================
nrb_mofuss_c <- if (is.null(aoi_region)) {
  terra::crop(nrb_mofuss, aoi_ext)
} else {
  mask_region(nrb_mofuss, aoi_region)
}
harv_mofuss_c <- if (is.null(aoi_region)) {
  terra::crop(harv_mofuss, aoi_ext)
} else {
  mask_region(harv_mofuss, aoi_region)
}

# fNRB totals use the complete selected-region MoFuSS MC1 footprint. Save them
# before optional aggregation/common-footprint masking used by pixel metrics.
modelled_nrb   <- sum_raster(nrb_mofuss_c)
mofuss_harvest <- sum_raster(harv_mofuss_c)
if (mofuss_harvest <= 0)
  stop("Selected-region MoFuSS harvest must be greater than zero.")

# Resample the observed loss DENSITY (Mg/ha) onto the MoFuSS grid (bilinear is
# correct for a density), THEN convert to Mg/pixel using the true area of each
# MoFuSS cell. cellSize returns geodesic hectares, so this is real ground area,
# not the latitude-distorted World-Mercator planar area.
loss_mgha_rs <- if (terra::same.crs(loss_mgha, nrb_mofuss_c)) {
  terra::resample(loss_mgha, nrb_mofuss_c, method = "bilinear")
} else {
  terra::project(loss_mgha, nrb_mofuss_c, method = "bilinear")
}
cell_ha       <- terra::cellSize(nrb_mofuss_c, unit = "ha")   # true ha per cell
nrb_ctrees_c  <- loss_mgha_rs * cell_ha                       # Mg / pixel (signed)

# Optional coarsening (e.g. 100 m -> 1 km). All three are now Mg/pixel
# (EXTENSIVE), so summing on aggregation conserves mass for all of them.
if (agg_factor > 1) {
  nrb_mofuss_c <- terra::aggregate(nrb_mofuss_c, fact = agg_factor, fun = "sum", na.rm = TRUE)
  harv_mofuss_c<- terra::aggregate(harv_mofuss_c,fact = agg_factor, fun = "sum", na.rm = TRUE)
  nrb_ctrees_c <- terra::aggregate(nrb_ctrees_c, fact = agg_factor, fun = "sum", na.rm = TRUE)
}

# --- Source-specific display/output layers ---------------------------------
# Do NOT clip CTrees maps to the MoFuSS valid-cell footprint. CTrees is one
# fixed observed dataset, so its mapped gross losses and gains must be the same
# for every MoFuSS configuration. Likewise, display each MoFuSS result over its
# own available footprint. The common footprint below is used only for the
# pixel-by-pixel comparison diagnostics.
nrb_ctrees_plot <- round(terra::ifel(nrb_ctrees_c >= nrb_threshold,
                                     nrb_ctrees_c, NA))
gains_ctrees_plot <- round(
  terra::ifel(nrb_ctrees_c <= -nrb_threshold, abs(nrb_ctrees_c), NA)
)
nrb_mofuss_plot <- round(terra::ifel(nrb_mofuss_c >= nrb_threshold,
                                     nrb_mofuss_c, NA))
harv_mofuss_plot <- round(harv_mofuss_c)

# --- Common valid footprint -------------------------------------------------
# Keep ONLY cells where observed change, MoFuSS endpoint NRB, and MoFuSS harvest
# are all defined for the pixel-comparison diagnostics. Aggregate fNRB totals
# above deliberately use each source's complete selected-region footprint.
both_valid <- terra::ifel(!is.na(nrb_mofuss_c) & !is.na(nrb_ctrees_c) &
                            !is.na(harv_mofuss_c), 1, NA)
nrb_mofuss_m  <- terra::mask(nrb_mofuss_c,  both_valid)
nrb_ctrees_m  <- terra::mask(nrb_ctrees_c,  both_valid)

# --- Comparison layers ------------------------------------------------------
# Common-footprint NRB layers are thresholded only for pixel diagnostics.
# Source-specific map/output layers were made above. The fNRB numerators and
# denominator are never thresholded.
nrb_mofuss_cmp <- round(terra::ifel(nrb_mofuss_m >= nrb_threshold,
                                    nrb_mofuss_m, NA))
nrb_ctrees_cmp <- round(terra::ifel(nrb_ctrees_m >= nrb_threshold,
                                    nrb_ctrees_m, NA))


# =============================================================================
# 7. METRICS
# =============================================================================

# --- 7a. Magnitude agreement, CO-DETECTED pixels only -----------------------
# correlation / RMSE / MAE are computed only where BOTH sides pass the NRB
# threshold, i.e. "when both flag a substantial loss, do the magnitudes track?"
# They deliberately ignore presence/absence disagreement (see 7b for that).
v_obs <- terra::values(nrb_ctrees_cmp)
v_mof <- terra::values(nrb_mofuss_cmp)
ok    <- is.finite(v_obs) & is.finite(v_mof)
n_ok  <- sum(ok)

correlation <- if (n_ok > 2) cor(v_obs[ok], v_mof[ok]) else NA_real_
rmse        <- if (n_ok > 0) sqrt(mean((v_obs[ok] - v_mof[ok])^2)) else NA_real_
mae         <- if (n_ok > 0) mean(abs(v_obs[ok] - v_mof[ok]))       else NA_real_

# --- 7b. Loss / no-loss AGREEMENT over the common footprint -----------------
# Classifies every common-footprint pixel as loss (>= threshold) or not, for
# observed and modelled, and cross-tabulates. This captures the disagreement
# the magnitude metrics miss - where one side sees a loss and the other doesn't.
vm  <- terra::values(nrb_mofuss_m)
vo  <- terra::values(nrb_ctrees_m)
dom <- is.finite(vm) & is.finite(vo)          # the common footprint
obs_loss <- vo[dom] >= nrb_threshold
mof_loss <- vm[dom] >= nrb_threshold

hits   <- sum( obs_loss &  mof_loss)          # both see a loss
misses <- sum( obs_loss & !mof_loss)          # observed loss, model missed it
falarm <- sum(!obs_loss &  mof_loss)          # model loss, not observed
corrng <- sum(!obs_loss & !mof_loss)          # both "no loss"
n_dom  <- hits + misses + falarm + corrng

agreement <- if (n_dom > 0) (hits + corrng) / n_dom else NA_real_
pod <- if ((hits + misses) > 0) hits / (hits + misses) else NA_real_  # detection rate
far <- if ((hits + falarm) > 0) falarm / (hits + falarm) else NA_real_ # false-alarm ratio
csi <- if ((hits + misses + falarm) > 0) hits / (hits + misses + falarm) else NA_real_ # threat score

# --- 7c. Gross/net totals + fNRB, one exact denominator ---------------------
# All three fNRBs use the complete, unthresholded MC1 Harvest_tot11:20 sum in
# the selected region. CTrees ratios are diagnostic because observed AGB change
# includes all drivers and can therefore exceed 100% of woodfuel harvest.
fnrb_ctrees_gross_pct <- 100 * observed_gross_nrb / mofuss_harvest
fnrb_ctrees_net_pct   <- 100 * observed_net_nrb   / mofuss_harvest
fnrb_mofuss_pct       <- 100 * modelled_nrb       / mofuss_harvest

fnrb_summary <- data.frame(
  Run = run_label,
  Working.Directory = mofuss_dir,
  Country = country_iso3,
  Start.Year = START_YEAR,
  End.Year = END_YEAR,
  AOI.Mode = aoi_mode,
  Monte.Carlo = 1L,
  `Demand = MoFuSS harvest (Mg)` = mofuss_harvest,
  `CTrees gross NRB (Mg)` = observed_gross_nrb,
  `CTrees net NRB (Mg)` = observed_net_nrb,
  `MoFuSS NRB (Mg)` = modelled_nrb,
  `CTrees gross fNRB (%)` = round(fnrb_ctrees_gross_pct),
  `CTrees net fNRB (%)` = round(fnrb_ctrees_net_pct),
  `MoFuSS fNRB (%)` = round(fnrb_mofuss_pct),
  check.names = FALSE
)

# Inputs and calculations have now passed. Only at this point replace the exact
# configuration output folder, preserving previous results if preflight fails.
if (dir.exists(out_dir)) {
  status <- unlink(out_dir, recursive = TRUE, force = TRUE)
  if (status != 0L || dir.exists(out_dir) || file.exists(out_dir)) {
    stop("Could not fully remove prior validation output folder: ", out_dir, call. = FALSE)
  }
}
if (!dir.create(out_dir, recursive = TRUE, showWarnings = FALSE) && !dir.exists(out_dir)) {
  stop("Could not create validation output folder: ", out_dir, call. = FALSE)
}

fnrb_csv_path <- file.path(
  out_dir,
  sprintf("fNRB_comparison_%s_%d_%d_MC1.csv", country_iso3, START_YEAR, END_YEAR)
)
utils::write.csv(fnrb_summary, fnrb_csv_path, row.names = FALSE)

agreement_summary <- data.frame(
  Run = run_label,
  Working.Directory = mofuss_dir,
  Country = country_iso3,
  Start.Year = START_YEAR,
  End.Year = END_YEAR,
  AOI.Mode = aoi_mode,
  Monte.Carlo = 1L,
  NRB.Threshold.Mg.Pixel = nrb_threshold,
  Co.detected.loss.pixels = n_ok,
  Pearson.r = correlation,
  RMSE.Mg.Pixel = rmse,
  MAE.Mg.Pixel = mae,
  Common.footprint.pixels = n_dom,
  Hits = hits,
  Misses = misses,
  False.alarms = falarm,
  Correct.negatives = corrng,
  Overall.agreement = agreement,
  POD = pod,
  FAR = far,
  CSI = csi,
  check.names = FALSE
)
agreement_csv_path <- file.path(
  out_dir,
  sprintf("NRB_pixel_agreement_%s_%d_%d_MC1.csv", country_iso3, START_YEAR, END_YEAR)
)
utils::write.csv(agreement_summary, agreement_csv_path, row.names = FALSE)

cat("\n================  NRB / fNRB comparison  (", country_iso3,
    " ", START_YEAR, "-", END_YEAR, ", MC1)",
    "  res =", resolution, " AOI =", aoi_mode, "\n")
cat("  -- magnitude, co-detected pixels only --\n")
cat(sprintf("  co-detected pixels : %d\n",      n_ok))
cat(sprintf("  correlation        : %.3f\n",    correlation))
cat(sprintf("  RMSE               : %.1f\n",     rmse))
cat(sprintf("  MAE                : %.1f\n",     mae))
cat("  -- loss / no-loss agreement, common footprint --\n")
cat(sprintf("  common pixels      : %d\n",       n_dom))
cat(sprintf("  hits / miss / f.a. : %d / %d / %d\n", hits, misses, falarm))
cat(sprintf("  overall agreement  : %.1f%%\n",   100 * agreement))
cat(sprintf("  POD / FAR / CSI    : %.2f / %.2f / %.2f\n", pod, far, csi))
cat("  -- unthresholded regional totals and fNRB --\n")
cat(sprintf("  CTrees common endpoint cells : %.0f\n", ctrees_common_cells))
cat(sprintf("  CTrees gross NRB (Mg)        : %.0f\n", observed_gross_nrb))
cat(sprintf("  CTrees signed balance (Mg)   : %.0f\n", observed_balance))
cat(sprintf("  CTrees net NRB (Mg)          : %.0f\n", observed_net_nrb))
cat(sprintf("  MoFuSS MC1 NRB (Mg)          : %.0f\n", modelled_nrb))
cat(sprintf("  Shared denominator / harvest : %.0f Mg\n", mofuss_harvest))
cat(sprintf("  CTrees gross fNRB            : %.0f%%\n", fnrb_ctrees_gross_pct))
cat(sprintf("  CTrees net fNRB              : %.0f%%\n", fnrb_ctrees_net_pct))
cat(sprintf("  MoFuSS MC1 fNRB              : %.0f%%\n\n", fnrb_mofuss_pct))

# 2 x 2 contingency table (printed as a labelled matrix)
# Column-major fill -> [MoFuSS loss/Obs loss], [MoFuSS no-loss/Obs loss],
#                      [MoFuSS loss/Obs no-loss], [MoFuSS no-loss/Obs no-loss]
conf_mat <- matrix(c(hits, misses, falarm, corrng), nrow = 2,
                   dimnames = list("MoFuSS"   = c("loss", "no-loss"),
                                   "Observed" = c("loss", "no-loss")))
cat("Loss / no-loss contingency (pixel counts):\n")
print(conf_mat)
cat("\n")


# =============================================================================
# 8. OUTPUTS  -  4-panel PNG + 4 GeoTIFFs
# =============================================================================

# --- 8a. Scatter (observed vs modelled), 1:1 line ---------------------------
scatter_path <- file.path(
  out_dir,
  sprintf("NRB_scatter_%s_%d_%d_MC1.png", country_iso3, START_YEAR, END_YEAR)
)
grDevices::png(scatter_path, width = 7, height = 7, units = "in", res = 300,
               type = if (capabilities("cairo")) "cairo" else getOption("bitmapType"))
plot(v_obs[ok], v_mof[ok],
     xlab = "Observed NRB (Mg/pixel)",
     ylab = "Modelled NRB (Mg/pixel)",
     main = sprintf("Observed vs MoFuSS MC1 NRB  (r = %.2f)", correlation))
abline(0, 1, col = "red")
grDevices::dev.off()

# --- 8b. 4-panel map --------------------------------------------------------
# The two NRB panels share ONE scale anchored to the fixed observed CTrees map.
# This makes the observed panel (including its palette) identical across MoFuSS
# configurations and preserves direct visual comparison with the model. Harvest
# is a different quantity (a gross flux, not a net loss), so it gets its own
# scale and its own colour ramp (green) to make that clear at a glance.
nrb_colors  <- colorRampPalette(c("white", "orange", "red"))(100)
harv_colors <- colorRampPalette(c("white", "yellowgreen", "darkgreen"))(100)

rng_nrb <- range(terra::minmax(nrb_ctrees_plot), na.rm = TRUE)

ctry_r <- terra::project(ctry_sel, nrb_ctrees_plot)   # country outline in map CRS

# Draw the boundary as lines rather than calling plot(..., add = TRUE). The
# latter can reset the raster panel's graphics transform for tall countries,
# making correctly computed annotation coordinates appear outside the panel.
add_country_outline <- function() {
  terra::lines(ctry_r, col = "black", lwd = 1)
}

period_text <- sprintf("%d-%d", START_YEAR, END_YEAR)
percent_text <- function(x) paste0(format(round(x), big.mark = ",", scientific = FALSE), "%")
add_metric_box <- function(lines, panel_lim, cex = 0.82, inset = 0.035) {
  # Build the annotation from the active raster panel coordinates. A base-R
  # legend can become wider than the plotting region for tall, narrow countries
  # (for example Malawi), which clips its left-hand text. Shrink only when
  # necessary and keep the complete rectangle inside par("usr").
  u <- unname(panel_lim[c("xmin", "xmax", "ymin", "ymax")])
  panel_w <- u[2] - u[1]
  panel_h <- u[4] - u[3]
  max_box_w <- panel_w * (1 - 2 * inset)

  cex_use <- cex
  repeat {
    char_w <- strwidth("M", cex = cex_use, units = "user")
    text_w <- max(strwidth(lines, cex = cex_use, units = "user"))
    box_w <- text_w + 1.4 * char_w
    if (box_w <= max_box_w || cex_use <= 0.50) break
    cex_use <- cex_use * 0.90
  }

  line_h <- strheight("Mg", cex = cex_use, units = "user")
  pad_x <- 0.7 * char_w
  pad_y <- 0.55 * line_h
  line_step <- 1.15 * line_h
  box_h <- 2 * pad_y + length(lines) * line_step

  xleft <- u[1] + inset * panel_w
  xright <- min(xleft + box_w, u[2] - inset * panel_w)
  ytop <- u[4] - inset * panel_h
  ybottom <- ytop - box_h

  rect(xleft, ybottom, xright, ytop,
       col = grDevices::adjustcolor("white", alpha.f = 0.90),
       border = "grey35", xpd = FALSE)
  ytext <- ytop - pad_y - (seq_along(lines) - 0.5) * line_step
  graphics::text(xleft + pad_x, ytext, labels = lines,
                 pos = 4, offset = 0, col = "black",
                 cex = cex_use, xpd = FALSE)
}

png_path <- file.path(
  out_dir,
  sprintf("NRB_comparison_%s_%d_%d_MC1.png", country_iso3, START_YEAR, END_YEAR)
)
png(png_path, width = 10, height = 10, units = "in", res = 300, type = "cairo")
op <- par(mfrow = c(2, 2))

pinfo <- plot(nrb_ctrees_plot, main = "Observed gross NRB",
              col = nrb_colors, range = rng_nrb)
add_country_outline()
add_metric_box(c(period_text,
                 paste("Regional gross fNRB:", percent_text(fnrb_ctrees_gross_pct))),
               pinfo$lim)

pinfo <- plot(nrb_mofuss_plot, main = "MoFuSS MC1 NRB",
              col = nrb_colors, range = rng_nrb)
add_country_outline()
add_metric_box(c(period_text,
                 paste("MoFuSS fNRB:", percent_text(fnrb_mofuss_pct))),
               pinfo$lim)

pinfo <- plot(gains_ctrees_plot, main = "Observed AGB gains")
add_country_outline()
add_metric_box(c(period_text,
                 paste("Regional net fNRB:", percent_text(fnrb_ctrees_net_pct))),
               pinfo$lim)

# harvest: its own auto-scale (no shared range) and its own palette
pinfo <- plot(harv_mofuss_plot, main = "MoFuSS MC1 harvest", col = harv_colors)
add_country_outline()
add_metric_box(c(period_text, "Shared demand denominator:",
                 paste0(formatC(mofuss_harvest / 1e6, format = "f", digits = 1), " million Mg")),
               pinfo$lim)

par(op)
dev.off()

# --- 8c. GeoTIFFs -----------------------------------------------------------
tif <- function(r, name)
  terra::writeRaster(r, file.path(out_dir, name), overwrite = TRUE,
                     wopt = list(gdal = "COMPRESS=LZW"))

tif(nrb_ctrees_plot,   sprintf("Observed_NRB_%s_%s.tif",       ctrees_year1, ctrees_year2))
tif(nrb_mofuss_plot,   sprintf("Modeled_MC1_NRB_%s_%s.tif",    ctrees_year1, ctrees_year2))
tif(gains_ctrees_plot, sprintf("Observed_AGB_Gains_%s_%s.tif", ctrees_year1, ctrees_year2))
tif(harv_mofuss_plot,  sprintf("Modeled_MC1_Harvest_%s_%s.tif", ctrees_year1, ctrees_year2))

cat("Wrote outputs to:", out_dir, "\n")
cat("  -", basename(png_path), "\n")
cat("  -", basename(scatter_path), "\n")
cat("  -", basename(fnrb_csv_path), "\n")
cat("  -", basename(agreement_csv_path), "\n")

invisible(normalizePath(out_dir, winslash = "/", mustWork = TRUE))
}

# =============================================================================
# BATCH EXECUTION
# =============================================================================
run_metadata$run_label <- paste(
  vapply(run_metadata$scenario, safe_id, character(1)),
  run_metadata$mode,
  sep = "_"
)
run_metadata$output_dir <- file.path(validation_root, run_metadata$run_label)
message("\nResolved four-run NRB validation batch:")
print(run_metadata[, c("scenario", "role", "mode", "working_dir", "output_dir")], row.names = FALSE)
failures <- character()
if (DRY_RUN) {
  message("DRY RUN complete; no validation outputs were changed.")
} else {
  for (i in seq_len(nrow(run_metadata))) {
    result <- tryCatch(
      run_one_validation(
        run_metadata$working_dir[[i]],
        run_metadata$run_label[[i]],
        run_metadata$output_dir[[i]]
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      failure <- sprintf("%s: %s", run_metadata$run_label[[i]], conditionMessage(result))
      failures <- c(failures, failure)
      message("FAILED: ", failure)
    }
  }
  if (length(failures)) {
    stop("One or more NRB validations failed:\n  ", paste(failures, collapse = "\n  "), call. = FALSE)
  }
  message("\nAll four NRB validations completed: ", validation_root)
}
