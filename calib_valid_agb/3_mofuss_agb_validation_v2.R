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

if (!requireNamespace("terra",   quietly = TRUE)) stop("Please install 'terra':   install.packages('terra')")
if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install 'ggplot2': install.packages('ggplot2')")
## All terra/ggplot2 calls below are explicitly namespaced. This prevents the
## `conflicted` package (or an attached raster/dplyr/glue/gdata package) from
## making function dispatch depend on the user's current RStudio session.

###############################################################################
## 1. CONFIGURATION
###############################################################################
INTERACTIVE <- FALSE          # TRUE = ask via pop-up/menus; FALSE = use paths below

COUNTRY      <- "Malawi"
CAPPED_DIR   <- "C:/Users/aghil/Documents/MoFuSS_localhost/x_mwi_nv3_tests_ng"   # capped   (_ng)
UNCAPPED_DIR <- "" #C:/Users/aghil/Documents/MoFuSS_localhost/x_mwi_nv3_tests_g"    # uncapped (_g); "" to skip

OBS_TYPE     <- "projected"  # "projected" (MgDM/ha, EPSG:3395) or "latlong" (MgCO2/ha, EPSG:4326)
OBS_PROJ_DIR <- "D:/agb3rdparties/ctrees_dic2025_agb_paras/1km_agco2_2000_2025/agb_projected_ha"
OBS_LL_DIR   <- "D:/agb3rdparties/ctrees_dic2025_agb_paras/1km_agco2_2000_2025"

OUT_BASE     <- "C:/Users/aghil/Documents/MoFuSS_localhost/calib_valid_agb_new_2000-2025_verra"

## Country boundary used to clip the observed maps (and to overlay in the maps).
## "" = auto-find at <CAPPED_DIR>/LULCC/TempVector/userarea1.gpkg
CLIP_OBS_TO_COUNTRY <- TRUE
ADMIN_VECTOR        <- ""

## Validation-domain water mask. "" auto-finds hydrolakes_pcs.tif under the
## capped working folder (then the uncapped folder as a fallback). Applying the
## mask remains safe after upstream rasters are corrected because it is idempotent.
EXCLUDE_HYDROLAKES <- TRUE
HYDROLAKES_RASTER  <- ""

BASE_YEAR <- 2000
END_YEAR  <- 2025
SIM_END_YEAR <- 2050       # optional extension used only in Figure 1b
NODATA    <- -9999

## LAT/LONG observed unit conversion  MgCO2/ha -> MgDM/ha  (empirical 0.581 ~ theoretical 0.580)
CARBON_FRACTION <- 0.47
CO2_TO_DM       <- (12/44) / CARBON_FRACTION

## MoFuSS convention: 01 = BASE_YEAR, 02 = BASE_YEAR+1, ..., 51 = BASE_YEAR+50.
sim_file_name <- function(year) sprintf("Growth_less_harv%02d.tif", as.integer(year - BASE_YEAR + 1L))
obs_proj_name <- function(year) sprintf("ctrees_%d_agb_MgDM_ha.tif", year)
obs_ll_name   <- function(year) sprintf("ctrees_global_%d_AGC.tif",  year)

COL_OBS <- "#222222"; COL_CAP <- "#1f77b4"; COL_UNC <- "#d62728"

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
  cat("=== MoFuSS AGB validation : interactive setup ===\n")
  cn <- readline(sprintf("Country name [%s]: ", COUNTRY)); if (nzchar(cn)) COUNTRY <- cn
  CAPPED_DIR   <- ask_dir("Select the CAPPED (_ng) MoFuSS working folder", CAPPED_DIR)
  uc <- utils::select.list(c("Yes","No"), preselect = "Yes", title = "Also include an UNCAPPED (_g) run?")
  UNCAPPED_DIR <- if (identical(uc, "Yes")) ask_dir("Select the UNCAPPED (_g) MoFuSS working folder", UNCAPPED_DIR) else ""
  ot <- utils::select.list(c("projected  (MgDM/ha, same grid as MoFuSS)","lat-long   (MgCO2/ha, global EPSG:4326)"),
                           preselect = "projected  (MgDM/ha, same grid as MoFuSS)", title = "Which observed AGB maps?")
  OBS_TYPE <- if (grepl("^lat", ot)) "latlong" else "projected"
  if (OBS_TYPE == "projected") OBS_PROJ_DIR <- ask_dir("Select the observed PROJECTED (MgDM/ha) folder", OBS_PROJ_DIR)
  if (OBS_TYPE == "latlong")   OBS_LL_DIR   <- ask_dir("Select the observed LAT/LONG (MgCO2/ha) folder", OBS_LL_DIR)
  OUT_BASE <- ask_dir("Select the OUTPUT folder (figures + csv)", OUT_BASE)
}

if (!OBS_TYPE %in% c("projected", "latlong")) stop("OBS_TYPE must be 'projected' or 'latlong'.")
if (END_YEAR <= BASE_YEAR) stop("END_YEAR must be later than BASE_YEAR.")
OBS_DIR  <- if (OBS_TYPE == "latlong") OBS_LL_DIR else OBS_PROJ_DIR
if (!dir.exists(OBS_DIR)) stop("Observed AGB folder not found: ", OBS_DIR)
obs_name <- if (OBS_TYPE == "latlong") obs_ll_name else obs_proj_name
OUT_DIR  <- file.path(OUT_BASE, paste0(tolower(gsub("[^A-Za-z0-9]", "_", COUNTRY)), "_R"))
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
admin_path <- if (nzchar(ADMIN_VECTOR)) ADMIN_VECTOR else file.path(CAPPED_DIR, "LULCC", "TempVector", "userarea1.gpkg")
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
  adm <- terra::vect(admin_path)
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
validate_sim_coverage(capMC, years, "Capped")
if (length(uncMC)) validate_sim_coverage(uncMC, years, "Uncapped")

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
utils::write.csv(traj, file.path(OUT_DIR, paste0(COUNTRY, "_national_trajectory_allMC.csv")), row.names = FALSE)

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
utils::write.csv(stats, file.path(OUT_DIR, paste0(COUNTRY, "_change_pixelwise_stats.csv")), row.names = FALSE)
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

utils::write.csv(nrb_all, file.path(OUT_DIR, paste0(COUNTRY, "_NRB_aggregates_allMC.csv")), row.names = FALSE)
utils::write.csv(nrb_summary, file.path(OUT_DIR, paste0(COUNTRY, "_NRB_aggregates_summary.csv")), row.names = FALSE)
utils::write.csv(nrb_pairwise, file.path(OUT_DIR, paste0(COUNTRY, "_NRB_aggregates_pairwise_allMC.csv")), row.names = FALSE)
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
  ggplot2::labs(title = sprintf("%s - aboveground biomass change relative to %d", COUNTRY, BASE_YEAR),
       subtitle = paste0(coverage_note, "; thin = MC runs, bold = MC means / observed"),
       x = "Year", y = sprintf("AGB change vs %d", BASE_YEAR)) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_text(size = 12),
        plot.subtitle = ggplot2::element_text(size = 9))
ggplot2::ggsave(file.path(OUT_DIR, paste0(COUNTRY, "_fig1_national_trajectory.png")), g1,
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
  ggplot2::labs(title = sprintf("%s - total aboveground biomass by configuration, %d-%d", COUNTRY, BASE_YEAR, sim_end),
       subtitle = sprintf("%s; observed ends %d (dotted); thin = MC runs, bold = means",
                          coverage_note, END_YEAR),
       x = "Year", y = "Total AGB (Mt dry matter)") +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_text(size = 12),
                 plot.subtitle = ggplot2::element_text(size = 9))
ggplot2::ggsave(file.path(OUT_DIR, paste0(COUNTRY, "_fig1b_total_AGB.png")), g1b,
                width = 9, height = 5.4, dpi = 150)

###############################################################################
## 8. FIGURE 2 : spatial maps of AGB change (debugging_1), each on its OWN footprint
###############################################################################
cat("Drawing Figure 2 (change maps) ...\n")
mk   <- function(vec) { r <- terra::rast(ref); terra::values(r) <- vec; r }
rObs <- mk(dObs); rCap <- mk(dCap); rUnc <- if (!is.null(dUnc)) mk(dUnc) else NULL

nObs   <- sum(is.finite(dObs))
lblObs <- "Observed (CTrees)"
lblCap <- sprintf("MoFuSS capped (%.0f%% of obs. area)",   100 * sum(is.finite(dCap)) / nObs)
lblUnc <- if (!is.null(dUnc)) sprintf("MoFuSS uncapped (%.0f%% of obs. area)", 100 * sum(is.finite(dUnc)) / nObs) else NULL

te   <- terra::ext(terra::trim(rObs))
rObs <- terra::crop(rObs, te)
rCap <- terra::crop(rCap, te)
if (!is.null(rUnc)) rUnc <- terra::crop(rUnc, te)
to_df <- function(r, nm) { d <- as.data.frame(r, xy = TRUE, na.rm = FALSE); names(d)[3] <- "value"; d$panel <- nm; d }
map_df <- to_df(rObs, lblObs); map_df <- rbind(map_df, to_df(rCap, lblCap))
if (!is.null(rUnc)) map_df <- rbind(map_df, to_df(rUnc, lblUnc))
map_df$panel <- factor(map_df$panel, levels = unique(map_df$panel)); np <- nlevels(map_df$panel)

map_values <- c(dObs[is.finite(dObs)], dCap[is.finite(dCap)])
if (!is.null(dUnc)) map_values <- c(map_values, dUnc[is.finite(dUnc)])
vmax <- as.numeric(stats::quantile(abs(map_values), 0.98, na.rm = TRUE))
if (!is.finite(vmax) || vmax <= 0) vmax <- 1
map_df$value <- pmax(pmin(map_df$value, vmax), -vmax)
divpal <- grDevices::colorRampPalette(c("#a50026","#d73027","#f46d43","#fdae61","#fee08b",
                                         "#ffffbf","#d9ef8b","#a6d96a","#66bd63","#1a9850","#006837"))(100)

adm_layer <- NULL
if (file.exists(admin_path)) {
  adm2 <- terra::vect(admin_path)
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

g2 <- ggplot2::ggplot(map_df, ggplot2::aes(x, y, fill = value)) +
  ggplot2::geom_raster() + adm_layer + ggplot2::facet_wrap(~ panel, nrow = 1) +
  ggplot2::scale_fill_gradientn(colours = divpal, limits = c(-vmax, vmax), na.value = "grey85", name = "AGB change\n(MgDM/ha)") +
  ggplot2::coord_equal(xlim = c(terra::xmin(te), terra::xmax(te)),
                       ylim = c(terra::ymin(te), terra::ymax(te)), expand = FALSE) +
  ggplot2::labs(title = sprintf("%s - aboveground biomass change (%d vs %d)", COUNTRY, BASE_YEAR, END_YEAR),
       subtitle = "each panel shows its own data coverage; grey = no data or HydroLAKES water", x = NULL, y = NULL) +
  ggplot2::guides(fill = ggplot2::guide_colourbar(barheight = grid::unit(3.2, "cm"), title.position = "top")) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "grey85", colour = NA), panel.spacing = grid::unit(3, "pt"),
        plot.title = ggplot2::element_text(face = "bold", size = 12), strip.text = ggplot2::element_text(size = 10),
        legend.title = ggplot2::element_text(size = 9))
ggplot2::ggsave(file.path(OUT_DIR, paste0(COUNTRY, "_fig2_change_maps.png")), g2,
       width = np * maph * asp + 1.7, height = maph + 1.1, dpi = 150)
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
                       COUNTRY, BASE_YEAR, END_YEAR),
       x = "Observed dAGB (MgDM/ha)", y = "Simulated dAGB (MgDM/ha)") +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 11))
ggplot2::ggsave(file.path(OUT_DIR, paste0(COUNTRY, "_fig3_pixel_scatter.png")), g3,
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
utils::write.csv(summ, file.path(OUT_DIR, paste0(COUNTRY, "_national_summary.csv")), row.names = FALSE)

cat("\n================  SUMMARY  (", COUNTRY, ", ", BASE_YEAR, "-", END_YEAR, ")  ================\n", sep = "")
print(summ, row.names = FALSE)
cat("\nPixel-level change agreement (debugging_1, pairwise):\n")
print(stats[, c("config","n","pearson_r","rmse_MgDMha","bias_MgDMha")], row.names = FALSE, digits = 3)
cat("\nGross/net NRB aggregates (full endpoint scopes; MC values summarized):\n")
print(nrb_summary, row.names = FALSE, digits = 5)
cat("\nOutputs written to:\n  ", OUT_DIR, "\n", sep = "")
cat("Done.\n")
###############################################################################
