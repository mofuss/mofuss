# =============================================================================
#  MoFuSS validation against observed AGB dynamics (CTrees)
#  v4  -  polished / seamless rewrite of calibration_validation_v3.R
# =============================================================================
#
#  READ ME FIRST  ------------------------------------------------------------
#
#  WHAT IT DOES
#    Compares Non-Renewable Biomass (NRB) between:
#      * MoFuSS  - MODELLED woodfuel harvest / NRB   (this run: 1 km, Web Mercator)
#      * CTrees  - OBSERVED AGB change from third-party maps (2000 / 2010 / 2020)
#
#  NRB DEFINITION USED HERE
#      NRB = biomass LOST between year1 and year2 (pixels that lose biomass).
#      Pixels that GAIN biomass, or stay within the stability band defined by
#      `nrb_threshold`, are NOT counted as NRB.
#
#  WHY AGREEMENT IS ONLY PARTIAL
#      MoFuSS models ONLY woodfuel harvest. Observed change also includes land
#      clearing, fire, etc. Agreement is therefore expected to be closer in
#      areas where woodfuel is a dominant driver - which is exactly why you can
#      restrict the comparison to an Area Of Interest (AOI) drawn on a map.
#
#  WORKFLOW  (runs top-to-bottom, NO manual step in the middle)
#      1. CONFIG    - resolution, country, years/period, AOI mode, threshold
#      2. LOAD      - MoFuSS NRB + harvest ; CTrees AGB (two years)
#      3. OBSERVED  - observed AGB loss as a DENSITY (Mg/ha), reprojected
#      4. AOI       - draw a box on a Leaflet map (returns automatically)
#      5. ALIGN     - resample density to MoFuSS grid, then -> Mg/pixel;
#                     keep only the common (both-valid) footprint
#      6. METRICS   - magnitude (r/RMSE/MAE, co-detected) + loss/no-loss
#                     agreement (POD/FAR/CSI) + totals + fNRB
#      7. OUTPUTS   - 4-panel PNG + 4 GeoTIFFs written to <ctrees_dir>/temp
#
#  MASS CONSISTENCY (why the observed side stays a density until the end)
#      NRB in Mg/pixel is an EXTENSIVE quantity; reprojecting/resampling it with
#      interpolation does not conserve mass. So the observed loss is kept in
#      Mg/ha (intensive) through all reprojection/resampling and converted to
#      Mg/pixel only once it sits on the final MoFuSS grid, using the true
#      geodesic area of each cell. All totals/metrics use one common footprint.
#
#  THE OLD PAIN POINT (now fixed)
#      The AOI map used to require running a Shiny app BY HAND in the middle of
#      the script (there was a `stop("Run the shiny app")`). It is now a normal
#      blocking function: the map opens, you draw a rectangle, click a button,
#      and the script continues on its own. If you don't want to draw, set
#      `aoi_mode` to "country" or "full" in the CONFIG block and no map opens.
#
# =============================================================================


# =============================================================================
# 0. PACKAGES
# =============================================================================
# Trimmed to what is actually used. terra handles all raster + vector work,
# so the old sp / raster / rgdal / sf stack is no longer needed.
library(terra)          # rasters + vectors
library(shiny)          # blocking AOI map
library(leaflet)        # the map itself
library(leaflet.extras) # rectangle draw toolbar


# =============================================================================
# 1. CONFIG  -  the only block you normally need to edit
# =============================================================================

# --- 1a. Resolution switch --------------------------------------------------
# "1km"  -> use the 1 km MoFuSS output and 1 km CTrees maps (this dataset).
# "100m" -> use a genuine 100 m MoFuSS output + 100 m CTrees maps (if you have
#            them). `agg_factor` can then coarsen 100 m -> 1 km before comparing
#            (set to 10) or keep native 100 m (set to 1).
resolution <- "1km"        # "1km" or "100m"

res_cfg <- list(
  "1km" = list(
    mofuss_dir  = "C:/Users/aghil/Documents/MoFuSS_localhost/webmofuss_nv3_tests_ng",
    ctrees_dir  = "G:/Mi unidad/webpages/2026_MoFuSSGlobal_Datasets/fnrb_obs_data/1km_agco2_2000_2025",
    agg_factor  = 1        # 1 = no aggregation
  ),
  "100m" = list(
    mofuss_dir  = "D:/_zambia/zmb_bau_1km_subc",   # <- point to a real 100 m run
    ctrees_dir  = "C:/Users/aghil/Documents/MoFuSS_localhost/calib_valid_agb_FAO",
    agg_factor  = 1        # set to 10 to coarsen 100 m -> 1 km before comparing
  )
)[[resolution]]

mofuss_dir       <- res_cfg$mofuss_dir
ctrees_dir       <- res_cfg$ctrees_dir
agg_factor       <- res_cfg$agg_factor
mofuss_regionsdir <- "C:/Users/aghil/Documents/MoFuSS_localhost/admin_regions/regions_adm0"

# --- 1b. Country ------------------------------------------------------------
country_iso3 <- "KEN"      # matches GID_0 in mofuss_regions0.gpkg

# --- 1c. Comparison period --------------------------------------------------
# Name the two CTrees maps directly - their file names change between 1km /
# 100m and other versions, so this is more robust than rebuilding the name.
# The year is parsed out of each file name automatically (for plot titles and
# output file names), so you don't set it separately.
# MoFuSS periods available: 10_20, 20_30, 20_35, 20_40, 30_40.
# For a fair comparison the CTrees interval must match the MoFuSS period.
ctrees_file1  <- "ctrees_global_2010_AGC.tif"   # earlier year
ctrees_file2  <- "ctrees_global_2020_AGC.tif"   # later year
mofuss_period <- "10_20"   # -> nrb_10_20_mean.tif / harv_10_20_mean.tif

# --- 1d. AOI mode -----------------------------------------------------------
# "draw"    -> open a map, draw a rectangle, script continues automatically
# "country" -> use the whole selected country polygon (no map)
# "full"    -> use the full MoFuSS x CTrees overlap (no map)
aoi_mode <- "draw"

# --- 1e. NRB threshold ------------------------------------------------------
# Stability band, in Mg / pixel. Losses of at least this much count as NRB;
# gains and smaller changes are dropped (set to NA).
nrb_threshold <- 100

# --- 1f. CTrees units (IMPORTANT - verify before trusting absolute fNRB) ----
# CTrees rasters are named *_AGC = Above-Ground CARBON. Converting to biomass
# (dry matter) depends on their true units, and the band carries NO unit
# metadata. A sample over Zambia was ~10-240 (median ~120), which is plausible
# under either interpretation, so this cannot be resolved from the data alone:
#   "CO2" -> agb = value * (12/44) / 0.47   <- your original v3 setting
#   "C"   -> agb = value / 0.47
# Check the CTrees documentation and set this accordingly.
ctrees_units <- "CO2"      # "CO2" (original) or "C"
agc_to_agb   <- if (ctrees_units == "C") 1 / 0.47 else (12 / 44) / 0.47

# --- 1g. Output location ----------------------------------------------------
out_dir <- file.path(ctrees_dir, "temp")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# 2. HELPER FUNCTIONS
# =============================================================================

# Locate the single results folder --------------------------------------------
# The output folder always starts with "Out" (Out, OutBaU, ...) but the exact
# name varies per run. There is only ever one, so find it automatically.
find_out_dir <- function(dir) {
  outs <- list.dirs(dir, recursive = FALSE, full.names = FALSE)
  outs <- outs[startsWith(outs, "Out")]
  if (length(outs) != 1)
    stop(sprintf("Expected exactly one 'Out*' folder in %s, found %d%s",
                 dir, length(outs),
                 if (length(outs)) paste0(": ", paste(outs, collapse = ", ")) else ""))
  file.path(dir, outs)
}
mofuss_out_dir <- find_out_dir(mofuss_dir)   # resolved once

# Path to a MoFuSS webmofuss_results raster ---------------------------------
mofuss_file <- function(name) {
  file.path(mofuss_out_dir, "webmofuss_results", name)
}

# Load a MoFuSS raster (RAW) ------------------------------------------------
# Do NOT threshold here: the loss/no-loss agreement analysis and the common
# footprint need the full modelled domain, including no-loss pixels. The NRB
# threshold is applied later, only to the comparison layers (section 6).
load_mofuss <- function(name) {
  terra::rast(mofuss_file(name))
}

# Parse a 4-digit year from a CTrees file name (for labels / output names) ---
# Falls back to the file name (without extension) if no year is present.
ctrees_label <- function(fname) {
  y <- regmatches(fname, regexpr("(19|20)\\d{2}", fname))
  if (length(y)) y else tools::file_path_sans_ext(basename(fname))
}

# Load one CTrees AGB map, crop + mask to country, convert AGC -> AGB --------
load_ctrees_agb <- function(fname, country_vect) {
  r <- terra::rast(file.path(ctrees_dir, "in", fname))
  r <- terra::crop(r, country_vect)
  r <- terra::mask(r, country_vect)
  r <- r * agc_to_agb
  r[r < 0] <- NA               # -9999 nodata and any negatives
  r
}

# Interactive AOI selection (BLOCKING) --------------------------------------
# Opens a Leaflet map. Draw ONE rectangle, then click "Use this area".
# Returns a SpatExtent already reprojected into `target_crs`, or NULL if the
# user chose "Use whole overlap". The script pauses here and resumes on click.
select_aoi_draw <- function(zoom_bbox, target_crs) {

  if (!interactive()) {
    message("Non-interactive session: skipping the draw map, using full overlap.")
    return(NULL)
  }

  ui <- fluidPage(
    tags$style(HTML("#map {height: calc(100vh - 70px) !important;}")),
    leafletOutput("map"),
    div(style = "padding:6px 10px;",
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

  # reproject the lon/lat box into the MoFuSS CRS and return a SpatExtent
  poly <- terra::as.polygons(
    terra::ext(bb[["xmin"]], bb[["xmax"]], bb[["ymin"]], bb[["ymax"]]),
    crs = "EPSG:4326")
  terra::ext(terra::project(poly, target_crs))
}


# =============================================================================
# 3. LOAD  -  MoFuSS (modelled) and CTrees (observed)
# =============================================================================

# --- MoFuSS modelled NRB + harvest for the chosen period --------------------
nrb_mofuss  <- load_mofuss(sprintf("nrb_%s_mean.tif",  mofuss_period))
harv_mofuss <- load_mofuss(sprintf("harv_%s_mean.tif", mofuss_period))
target_crs  <- terra::crs(nrb_mofuss)          # Web Mercator for this dataset

# --- Country polygon --------------------------------------------------------
ctry     <- terra::vect(file.path(mofuss_regionsdir, "mofuss_regions0.gpkg"))
ctry_sel <- ctry[ctry$GID_0 == country_iso3, ]

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

aoi_ext <- switch(
  aoi_mode,
  draw    = select_aoi_draw(zoom_bbox, target_crs),
  country = terra::ext(terra::project(ctry_sel, target_crs)),
  full    = terra::ext(loss_mgha_proj),
  stop("aoi_mode must be 'draw', 'country' or 'full'")
)
# "draw" returns NULL if the user clicked "Use whole overlap"
if (is.null(aoi_ext)) aoi_ext <- terra::ext(loss_mgha_proj)


# =============================================================================
# 6. ALIGN  -  put everything on the MoFuSS grid, mass-consistently
# =============================================================================
nrb_mofuss_c  <- terra::crop(nrb_mofuss,  aoi_ext)
harv_mofuss_c <- terra::crop(harv_mofuss, aoi_ext)

# Resample the observed loss DENSITY (Mg/ha) onto the MoFuSS grid (bilinear is
# correct for a density), THEN convert to Mg/pixel using the true area of each
# MoFuSS cell. cellSize(transform = TRUE) returns geodesic hectares, so this is
# the real ground area, not the latitude-distorted Web-Mercator area.
loss_mgha_rs  <- terra::resample(loss_mgha_proj, nrb_mofuss_c)
cell_ha       <- terra::cellSize(nrb_mofuss_c, unit = "ha")   # true ha per cell
nrb_ctrees_c  <- loss_mgha_rs * cell_ha                       # Mg / pixel (signed)

# Optional coarsening (e.g. 100 m -> 1 km). All three are now Mg/pixel
# (EXTENSIVE), so summing on aggregation conserves mass for all of them.
if (agg_factor > 1) {
  nrb_mofuss_c <- terra::aggregate(nrb_mofuss_c, fact = agg_factor, fun = "sum", na.rm = TRUE)
  harv_mofuss_c<- terra::aggregate(harv_mofuss_c,fact = agg_factor, fun = "sum", na.rm = TRUE)
  nrb_ctrees_c <- terra::aggregate(nrb_ctrees_c, fact = agg_factor, fun = "sum", na.rm = TRUE)
}

# --- Common valid footprint -------------------------------------------------
# Keep ONLY cells where BOTH MoFuSS and observed have data, so every total and
# metric below is computed over exactly the same set of pixels. Build a 1/NA
# mask (NA where either side is missing) so plain mask() works on any terra
# version, regardless of the maskvalue/maskvalues argument name.
both_valid    <- terra::ifel(!is.na(nrb_mofuss_c) & !is.na(nrb_ctrees_c), 1, NA)
nrb_mofuss_m  <- terra::mask(nrb_mofuss_c,  both_valid)
nrb_ctrees_m  <- terra::mask(nrb_ctrees_c,  both_valid)
harv_mofuss_m <- terra::mask(harv_mofuss_c, both_valid)

# --- Comparison layers ------------------------------------------------------
# NRB layers ARE thresholded: sub-threshold changes and gains -> NA
# ("stable / not NRB"). Harvest is NOT thresholded - every modelled harvest
# pixel counts toward the fNRB denominator - so it keeps the full common
# footprint (only masked and rounded).
nrb_mofuss_cmp <- round(terra::classify(nrb_mofuss_m, cbind(-Inf, nrb_threshold, NA)))
nrb_ctrees_cmp <- round(terra::classify(nrb_ctrees_m, cbind(-Inf, nrb_threshold, NA)))
harv_cmp       <- round(harv_mofuss_m)

# observed AGB GAINS (negative side of observed NRB, made positive)
gains_ctrees_cmp <- nrb_ctrees_m |>
  terra::classify(rbind(c(-nrb_threshold, 0,   NA),   # small gains -> NA
                        c(0,             Inf,  NA))) |> # all losses  -> NA
  abs() |> round()


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

agreement <- (hits + corrng) / n_dom          # overall % of pixels that agree
pod <- if ((hits + misses) > 0) hits / (hits + misses) else NA_real_  # detection rate
far <- if ((hits + falarm) > 0) falarm / (hits + falarm) else NA_real_ # false-alarm ratio
csi <- if ((hits + misses + falarm) > 0) hits / (hits + misses + falarm) else NA_real_ # threat score

# --- 7c. Totals + fNRB, over the common footprint ---------------------------
sum_ras <- function(r) as.numeric(terra::global(r, "sum", na.rm = TRUE)[[1]])
observed_nrb   <- sum_ras(nrb_ctrees_cmp)   # thresholded NRB (losses >= threshold)
modelled_nrb   <- sum_ras(nrb_mofuss_cmp)   # thresholded NRB (losses >= threshold)
mofuss_harvest <- sum_ras(harv_mofuss_m)    # FULL harvest, NOT thresholded

# fNRB = NRB / harvest, all over the same common footprint. The denominator is
# the TOTAL modelled woodfuel harvest (every pixel counts, no threshold), so it
# is complete and fNRB is not inflated by clipping small-harvest pixels.
# NOTE: "fNRB observed" divides observed AGB loss (ALL drivers) by MoFuSS
# woodfuel harvest, so it is a diagnostic ratio and can exceed 1.
fNRB_obs    <- round(observed_nrb / mofuss_harvest, 2)
fNRB_mofuss <- round(modelled_nrb / mofuss_harvest, 2)

cat("\n================  NRB comparison  (", country_iso3, mofuss_period, ")",
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
cat("  -- totals --\n")
cat(sprintf("  observed NRB (Mg)  : %.0f\n",     observed_nrb))
cat(sprintf("  modelled NRB (Mg)  : %.0f\n",     modelled_nrb))
cat(sprintf("  MoFuSS harvest (Mg): %.0f\n",     mofuss_harvest))
cat(sprintf("  fNRB observed      : %.2f\n",     fNRB_obs))
cat(sprintf("  fNRB MoFuSS        : %.2f\n\n",   fNRB_mofuss))

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
plot(v_obs[ok], v_mof[ok],
     xlab = "Observed NRB (Mg/pixel)",
     ylab = "Modelled NRB (Mg/pixel)",
     main = sprintf("Observed vs Modelled NRB  (r = %.2f)", correlation))
abline(0, 1, col = "red")

# --- 8b. 4-panel map --------------------------------------------------------
# The two NRB panels share ONE scale so they are directly comparable. Harvest
# is a different quantity (a gross flux, not a net loss), so it gets its own
# scale and its own colour ramp (green) to make that clear at a glance.
nrb_colors  <- colorRampPalette(c("white", "orange", "red"))(100)
harv_colors <- colorRampPalette(c("white", "yellowgreen", "darkgreen"))(100)

rng_nrb <- range(c(terra::minmax(nrb_ctrees_cmp),
                   terra::minmax(nrb_mofuss_cmp)), na.rm = TRUE)

ctry_r <- terra::project(ctry_sel, nrb_ctrees_cmp)   # country outline in map CRS

png_path <- file.path(out_dir, sprintf("NRB_comparison_%s.png", country_iso3))
png(png_path, width = 10, height = 10, units = "in", res = 300, type = "cairo")
op <- par(mfrow = c(2, 2))

plot(nrb_ctrees_cmp, main = sprintf("Observed NRB (%s-%s)", ctrees_year1, ctrees_year2),
     col = nrb_colors, range = rng_nrb); plot(ctry_r, add = TRUE, border = "black", lwd = 1)

plot(nrb_mofuss_cmp, main = sprintf("Modelled NRB (%s)", mofuss_period),
     col = nrb_colors, range = rng_nrb); plot(ctry_r, add = TRUE, border = "black", lwd = 1)

plot(gains_ctrees_cmp, main = sprintf("Observed AGB gains (%s-%s)", ctrees_year1, ctrees_year2))
plot(ctry_r, add = TRUE, border = "black", lwd = 1)

# harvest: its own auto-scale (no shared range) and its own palette
plot(harv_cmp, main = sprintf("Modelled harvest (%s)", mofuss_period),
     col = harv_colors); plot(ctry_r, add = TRUE, border = "black", lwd = 1)

par(op)
dev.off()

# --- 8c. GeoTIFFs -----------------------------------------------------------
tif <- function(r, name)
  terra::writeRaster(r, file.path(out_dir, name), overwrite = TRUE,
                     wopt = list(gdal = "COMPRESS=LZW"))

tif(nrb_ctrees_cmp,   sprintf("Observed_NRB_%s_%s.tif",       ctrees_year1, ctrees_year2))
tif(nrb_mofuss_cmp,   sprintf("Modeled_NRB_%s_%s.tif",        ctrees_year1, ctrees_year2))
tif(gains_ctrees_cmp, sprintf("Observed_AGB_Gains_%s_%s.tif", ctrees_year1, ctrees_year2))
tif(harv_cmp,         sprintf("Modeled_Harvest_%s_%s.tif",    ctrees_year1, ctrees_year2))

cat("Wrote outputs to:", out_dir, "\n")
cat("  -", basename(png_path), "\n")

# =============================================================================
# END
# =============================================================================
