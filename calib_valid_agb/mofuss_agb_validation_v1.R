###############################################################################
##  MoFuSS simulated  vs  CTrees observed  ABOVEGROUND BIOMASS (AGB) validation
##  -------------------------------------------------------------------------
##  Reproduces, in R, the 2000-2025 comparison previously done in Python:
##    * national AGB trajectories (observed vs capped vs uncapped regrowth)
##    * spatial maps of AGB change 2000-2025
##    * pixel-level agreement (r / RMSE / bias)
##
##  Extra features requested:
##    * interactive selection of the MoFuSS working folder(s) and observed maps
##    * observed maps can be the PROJECTED layers (MgDM/ha, same grid as MoFuSS)
##      or the LAT/LONG global layers (MgCO2/ha, EPSG:4326) - converted on the fly
##    * if a run has several Monte-Carlo folders (debugging_1 .. debugging_N),
##      ALL of their national trajectories are drawn on the trajectory figure.
##      The maps and the pixel scatter use debugging_1 only.
##
##  Requirements:  install.packages(c("terra","ggplot2"))
##  Author: prepared for the MoFuSS / CTrees validation work.
###############################################################################

## ------------------------------------------------------------------ packages
if (!requireNamespace("terra",   quietly = TRUE)) stop("Please install 'terra':   install.packages('terra')")
if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install 'ggplot2': install.packages('ggplot2')")
suppressPackageStartupMessages({ library(terra); library(ggplot2) })

###############################################################################
## 1. CONFIGURATION  --  edit these defaults, or leave INTERACTIVE = TRUE to be
##    prompted for the folders every time you run the script.
###############################################################################

INTERACTIVE <- FALSE          # TRUE = ask via pop-up/menus; FALSE = use paths below

## ---- defaults (used when INTERACTIVE = FALSE, and pre-filled in prompts) ----
COUNTRY      <- "Kenya"

## MoFuSS working folders (each contains debugging_1 .. debugging_N)
CAPPED_DIR   <- "D:/mofuss_amazon/nv3/ken_bau1_1km_nv3_ng"   # capped   (_ng)
UNCAPPED_DIR <- "D:/mofuss_amazon/nv3/ken_bau1_1km_nv3_g"    # uncapped (_g); set "" to skip

## Observed CTrees maps -------------------------------------------------------
##   OBS_TYPE = "projected" -> MgDM/ha, EPSG:3395  (…/agb_projected_ha)
##   OBS_TYPE = "latlong"   -> MgCO2/ha, EPSG:4326 (global ctrees_global_YYYY_AGC.tif)
OBS_TYPE     <- "latlong"
OBS_PROJ_DIR <- "D:/agb3rdparties/ctrees_dic2025_agb_paras/1km_agco2_2000_2025/agb_projected_ha"
OBS_LL_DIR   <- "D:/agb3rdparties/ctrees_dic2025_agb_paras/1km_agco2_2000_2025"

## Output folder (figures + csv). Created if it does not exist.
OUT_BASE     <- "C:/Users/aghil/Documents/MoFuSS_localhost/calib_valid_agb_new_2000-2025_v2"

## ---- analysis window & constants (rarely need changing) --------------------
BASE_YEAR <- 2000            # shared initial condition (MoFuSS is initialised with observed 2000)
END_YEAR  <- 2025            # last observed year to compare against
NODATA    <- -9999

## Unit conversion for the LAT/LONG observed layers (MgCO2/ha -> MgDM/ha).
## MgDM = MgCO2 * (12/44) / CARBON_FRACTION.  Empirically checked against the
## projected MgDM/ha layers over Kenya: factor = 0.581 (theoretical 0.580).
CARBON_FRACTION <- 0.47
CO2_TO_DM       <- (12/44) / CARBON_FRACTION      # = 0.5803

## File-name templates (change only if CTrees renames their files) -----------
sim_file_name  <- function(harv)  sprintf("Growth_less_harv%02d.tif", harv)     # harv = year - 2000
obs_proj_name  <- function(year)  sprintf("ctrees_%d_agb_MgDM_ha.tif", year)
obs_ll_name    <- function(year)  sprintf("ctrees_global_%d_AGC.tif",  year)

## Colours (match the report figures) ----------------------------------------
COL_OBS <- "#222222"; COL_CAP <- "#1f77b4"; COL_UNC <- "#d62728"

###############################################################################
## 2. INTERACTIVE SELECTION  (Windows: uses choose.dir / menus)
###############################################################################
ask_dir <- function(prompt, default) {
  if (!INTERACTIVE) return(default)
  if (.Platform$OS.type == "windows") {
    d <- utils::choose.dir(default = gsub("/", "\\\\", default), caption = prompt)
    if (is.na(d)) d <- default
  } else {
    cat("\n", prompt, "\n[Enter = ", default, "]: ", sep = ""); d <- readline()
    if (!nzchar(d)) d <- default
  }
  gsub("\\\\", "/", d)
}

if (INTERACTIVE) {
  cat("=== MoFuSS AGB validation : interactive setup ===\n")
  cn <- readline(sprintf("Country name [%s]: ", COUNTRY)); if (nzchar(cn)) COUNTRY <- cn

  CAPPED_DIR   <- ask_dir("Select the CAPPED (_ng) MoFuSS working folder",   CAPPED_DIR)
  uc <- utils::select.list(c("Yes","No"), preselect = "Yes",
                           title = "Also include an UNCAPPED (_g) run?")
  UNCAPPED_DIR <- if (identical(uc, "Yes")) ask_dir("Select the UNCAPPED (_g) MoFuSS working folder", UNCAPPED_DIR) else ""

  ot <- utils::select.list(c("projected  (MgDM/ha, same grid as MoFuSS)",
                             "lat-long   (MgCO2/ha, global EPSG:4326)"),
                           preselect = "projected  (MgDM/ha, same grid as MoFuSS)",
                           title = "Which observed AGB maps?")
  OBS_TYPE <- if (grepl("^lat", ot)) "latlong" else "projected"
  if (OBS_TYPE == "projected") OBS_PROJ_DIR <- ask_dir("Select the observed PROJECTED (MgDM/ha) folder", OBS_PROJ_DIR)
  if (OBS_TYPE == "latlong")   OBS_LL_DIR   <- ask_dir("Select the observed LAT/LONG (MgCO2/ha) folder", OBS_LL_DIR)

  OUT_BASE <- ask_dir("Select the OUTPUT folder (figures + csv)", OUT_BASE)
}

OBS_DIR  <- if (OBS_TYPE == "latlong") OBS_LL_DIR else OBS_PROJ_DIR
obs_name <- if (OBS_TYPE == "latlong") obs_ll_name else obs_proj_name
OUT_DIR  <- file.path(OUT_BASE, paste0(tolower(gsub("[^A-Za-z0-9]", "_", COUNTRY)), "_R"))
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

cat("\nCountry      :", COUNTRY,
    "\nCapped dir   :", CAPPED_DIR,
    "\nUncapped dir :", if (nzchar(UNCAPPED_DIR)) UNCAPPED_DIR else "(none)",
    "\nObserved     :", OBS_TYPE, "->", OBS_DIR,
    "\nOutput       :", OUT_DIR, "\n\n")

###############################################################################
## 3. HELPERS
###############################################################################

## pad a SpatExtent by a fraction on every side (avoids relying on extent arithmetic)
pad_ext <- function(e, f = 0.02) {
  dx <- (xmax(e) - xmin(e)) * f; dy <- (ymax(e) - ymin(e)) * f
  ext(xmin(e) - dx, xmax(e) + dx, ymin(e) - dy, ymax(e) + dy)
}

## list debugging_1 .. debugging_N inside a MoFuSS working folder, ordered
list_mc <- function(workdir) {
  d <- list.dirs(workdir, recursive = FALSE)
  d <- d[grepl("debugging_\\d+$", d)]
  d[order(as.integer(sub(".*debugging_", "", d)))]
}

## reference grid = capped / debugging_1 / harv01
ref_sim_file <- function() {
  f <- file.path(list_mc(CAPPED_DIR)[1], sim_file_name(1))
  if (!file.exists(f)) stop("Reference sim file not found: ", f)
  f
}

## align an observed raster onto the reference grid, return MgDM/ha SpatRaster
align_obs <- function(year, ref) {
  f <- file.path(OBS_DIR, obs_name(year))
  if (!file.exists(f)) { warning("Missing observed file: ", f); return(NULL) }
  r <- rast(f)
  NAflag(r) <- NODATA
  if (crs(r) == "" ) crs(r) <- crs(ref)
  same_crs <- terra::same.crs(r, ref)
  ## crop first (fast) then resample/project to the exact reference grid
  if (same_crs) {
    r <- crop(r, pad_ext(ext(ref), 0.02), snap = "out")
    a <- resample(r, ref, method = "bilinear")
  } else {
    box <- project(as.polygons(ext(ref), crs = crs(ref)), crs(r))
    r   <- crop(r, pad_ext(ext(box), 0.05), snap = "out")
    a   <- project(r, ref, method = "bilinear")
  }
  if (OBS_TYPE == "latlong") a <- a * CO2_TO_DM     # MgCO2/ha -> MgDM/ha
  a[a < 0] <- NA
  a
}

## read a MoFuSS sim raster (MgDM per CELL) as a plain numeric vector on ref grid
sim_vec <- function(mc_dir, year) {
  f <- file.path(mc_dir, sim_file_name(year - BASE_YEAR))
  if (!file.exists(f)) return(NULL)
  rr <- rast(f); NAflag(rr) <- NODATA
  ## if this raster is not exactly on the reference grid (e.g. the uncapped run
  ## was clipped slightly differently), resample it so every vector aligns cell-by-cell
  if (ncell(rr) != ncell(ref) ||
      !isTRUE(all.equal(as.vector(ext(rr)), as.vector(ext(ref)), tolerance = 1)))
    rr <- resample(rr, ref, method = "bilinear")
  v <- as.numeric(values(rr))
  v[v == NODATA | v < 0] <- NA
  v
}

###############################################################################
## 4. LOAD REFERENCE GRID, OBSERVED SERIES, BUILD COMMON MASK
###############################################################################
ref <- rast(ref_sim_file())
NAflag(ref) <- NODATA
cell_ha <- prod(res(ref)) / 1e4                       # pixel area in hectares
years   <- BASE_YEAR:END_YEAR
cat(sprintf("Reference grid: %d x %d cells | pixel = %.2f x %.2f m | cell = %.3f ha\n",
            ncol(ref), nrow(ref), res(ref)[1], res(ref)[2], cell_ha))

cat("Loading observed CTrees maps ...\n")
obs_ha <- list()                                      # per-ha vectors, by year
for (y in years) {
  a <- align_obs(y, ref)
  obs_ha[[as.character(y)]] <- if (is.null(a)) rep(NA_real_, ncell(ref)) else as.numeric(values(a))
}

capMC <- list_mc(CAPPED_DIR)
uncMC <- if (nzchar(UNCAPPED_DIR)) list_mc(UNCAPPED_DIR) else character(0)
cat(sprintf("Monte-Carlo folders: capped = %d, uncapped = %d\n", length(capMC), length(uncMC)))

## common valid mask: observed (all yrs) & capped d1 (all yrs) & uncapped d1 (all yrs)
cat("Building common valid mask ...\n")
mask <- rep(TRUE, ncell(ref))
for (y in years) mask <- mask & is.finite(obs_ha[[as.character(y)]])
for (y in (BASE_YEAR + 1):END_YEAR) {
  v <- sim_vec(capMC[1], y); mask <- mask & is.finite(v)
  if (length(uncMC)) { v <- sim_vec(uncMC[1], y); mask <- mask & is.finite(v) }
}
n_mask <- sum(mask)
cat(sprintf("Common valid cells: %d\n", n_mask))

obs2000 <- obs_ha[["2000"]]                           # shared baseline (per ha)
tot_obs2000 <- sum(obs2000[mask]) * cell_ha / 1e6     # Mt

###############################################################################
## 5. NATIONAL TRAJECTORIES  (observed + every Monte-Carlo run of each config)
###############################################################################
cat("Computing national trajectories (all Monte-Carlo runs) ...\n")

traj <- data.frame()                                  # long format

## observed
obs_tot <- sapply(years, function(y) sum(obs_ha[[as.character(y)]][mask]) * cell_ha / 1e6)
traj <- rbind(traj, data.frame(year = years, total_Mt = obs_tot,
                               series = "Observed", mc = 0L))

## simulated configs
add_config <- function(mcdirs, label) {
  out <- data.frame()
  for (i in seq_along(mcdirs)) {
    tot <- numeric(length(years)); names(tot) <- years
    tot[as.character(BASE_YEAR)] <- tot_obs2000            # year 2000 = shared IC
    for (y in (BASE_YEAR + 1):END_YEAR) {
      v <- sim_vec(mcdirs[i], y)
      tot[as.character(y)] <- if (is.null(v)) NA else sum(v[mask], na.rm = TRUE) / 1e6
    }
    out <- rbind(out, data.frame(year = years, total_Mt = as.numeric(tot),
                                 series = label, mc = i))
    cat(sprintf("   %s MC %d/%d\r", label, i, length(mcdirs)))
  }
  cat("\n"); out
}
traj <- rbind(traj, add_config(capMC, "Capped"))
if (length(uncMC)) traj <- rbind(traj, add_config(uncMC, "Uncapped"))

## change relative to 2000 (per series/mc)
traj$change_Mt <- ave(traj$total_Mt, interaction(traj$series, traj$mc, drop = TRUE),
                      FUN = function(x) x - x[1])

write.csv(traj, file.path(OUT_DIR, paste0(COUNTRY, "_national_trajectory_allMC.csv")), row.names = FALSE)

###############################################################################
## 6. PIXEL-LEVEL CHANGE 2000-2025  (debugging_1 only)  -> maps + scatter + stats
###############################################################################
cat("Computing pixel-level change (debugging_1) ...\n")
obs2025  <- obs_ha[["2025"]]
capd1_25 <- sim_vec(capMC[1], END_YEAR) / cell_ha                       # -> MgDM/ha
uncd1_25 <- if (length(uncMC)) sim_vec(uncMC[1], END_YEAR) / cell_ha else NULL

dObs <- obs2025  - obs2000
dCap <- capd1_25 - obs2000
dUnc <- if (!is.null(uncd1_25)) uncd1_25 - obs2000 else NULL

pix_stats <- function(sim_change, obs_change, m, label) {
  o <- obs_change[m]; s <- sim_change[m]; e <- s - o
  data.frame(config = label, n = sum(m),
             pearson_r = cor(o, s), slope = coef(lm(s ~ o))[2],
             rmse_MgDMha = sqrt(mean(e^2)), bias_MgDMha = mean(e),
             obs_mean_change = mean(o), sim_mean_change = mean(s), row.names = NULL)
}
stats <- pix_stats(dCap, dObs, mask, "capped")
if (!is.null(dUnc)) stats <- rbind(stats, pix_stats(dUnc, dObs, mask, "uncapped"))
write.csv(stats, file.path(OUT_DIR, paste0(COUNTRY, "_change_pixelwise_stats.csv")), row.names = FALSE)
print(stats, digits = 3)

###############################################################################
## 7. FIGURE 1 : national trajectories (all Monte-Carlo runs)
###############################################################################
cat("Drawing Figure 1 (trajectories) ...\n")
plot_df <- rbind(
  data.frame(traj[, c("year","series","mc")], value = traj$total_Mt,  metric = "Total AGB (Mt dry matter)"),
  data.frame(traj[, c("year","series","mc")], value = traj$change_Mt, metric = "Change in AGB vs 2000 (Mt)")
)
plot_df$metric <- factor(plot_df$metric, levels = c("Total AGB (Mt dry matter)", "Change in AGB vs 2000 (Mt)"))
sim_df <- subset(plot_df, series != "Observed")
obs_df <- subset(plot_df, series == "Observed")
mean_df <- aggregate(value ~ year + series + metric, data = sim_df, FUN = mean)   # per-config MC mean

g1 <- ggplot() +
  geom_line(data = sim_df, aes(year, value, group = interaction(series, mc), colour = series),
            linewidth = 0.35, alpha = 0.30) +
  geom_line(data = mean_df, aes(year, value, colour = series), linewidth = 1.2) +
  geom_line(data = obs_df,  aes(year, value, colour = series), linewidth = 1.3) +
  facet_wrap(~ metric, scales = "free_y") +
  scale_colour_manual(values = c(Observed = COL_OBS, Capped = COL_CAP, Uncapped = COL_UNC), name = NULL) +
  labs(title = paste0(COUNTRY, " - national AGB, ", BASE_YEAR, "-", END_YEAR,
                      "  (thin = individual Monte-Carlo runs, bold = mean/observed)"),
       x = "Year", y = NULL) +
  theme_bw(base_size = 12) + theme(legend.position = "bottom",
                                   plot.title = element_text(size = 11))
ggsave(file.path(OUT_DIR, paste0(COUNTRY, "_fig1_national_trajectory.png")),
       g1, width = 12, height = 5.2, dpi = 150)

###############################################################################
## 8. FIGURE 2 : spatial maps of AGB change 2000-2025 (debugging_1)
###############################################################################
## =============================================================================
## DROP-IN REPLACEMENT for Section 8 (FIGURE 2 : spatial change maps)
## Replace the whole "## 8. FIGURE 2 ..." block in mofuss_agb_validation_v1.R
## with the code below.
##
## What changed vs the previous version:
##   * colour bar is now labelled with its units  -> "AGB change (MgDM/ha)"
##   * the title states the comparison period      -> "(2000 vs 2025)"
##   * drawn with ggplot2 (consistent look with Figures 1 and 3)
##
## It uses the same variables already created earlier in the script:
##   ref, mask, dObs, dCap, dUnc, COUNTRY, BASE_YEAR, END_YEAR, OUT_DIR
## (no other changes needed).
## =============================================================================

###############################################################################
## 8. FIGURE 2 : spatial maps of AGB change BASE_YEAR-END_YEAR (debugging_1)
##    Colour bar is labelled with units (MgDM/ha); title states the period.
###############################################################################
cat("Drawing Figure 2 (change maps) ...\n")

mk <- function(vec) { r <- rast(ref); values(r) <- ifelse(mask, vec, NA); r }
rObs <- mk(dObs); rCap <- mk(dCap); rUnc <- if (!is.null(dUnc)) mk(dUnc) else NULL

## crop to the visible data region (trims blank rows/cols)
te   <- ext(trim(rObs))
rObs <- crop(rObs, te); rCap <- crop(rCap, te); if (!is.null(rUnc)) rUnc <- crop(rUnc, te)

to_df <- function(r, nm) { d <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
names(d)[3] <- "value"; d$panel <- nm; d }
map_df <- to_df(rObs, "Observed (CTrees)")
map_df <- rbind(map_df, to_df(rCap, "MoFuSS - capped regrowth"))
if (!is.null(rUnc)) map_df <- rbind(map_df, to_df(rUnc, "MoFuSS - uncapped regrowth"))
map_df$panel <- factor(map_df$panel, levels = unique(map_df$panel))
np <- nlevels(map_df$panel)

vmax <- as.numeric(quantile(abs(c(dObs[mask], dCap[mask])), 0.98, na.rm = TRUE))
map_df$value <- pmax(pmin(map_df$value, vmax), -vmax)          # saturate extremes at +/- vmax
divpal <- colorRampPalette(c("#a50026","#d73027","#f46d43","#fdae61","#fee08b",
                             "#ffffbf","#d9ef8b","#a6d96a","#66bd63","#1a9850","#006837"))(100)

## subnational admin boundaries: use ADMIN_VECTOR if set, else auto-find in the run folder
adm_path <- if (exists("ADMIN_VECTOR") && nzchar(ADMIN_VECTOR)) ADMIN_VECTOR else
  file.path(CAPPED_DIR, "LULCC", "TempVector", "userarea1.gpkg")
adm_layer <- NULL
if (file.exists(adm_path)) {
  adm <- terra::vect(adm_path)
  if (!terra::same.crs(adm, ref)) adm <- terra::project(adm, crs(ref))
  gdf <- as.data.frame(terra::geom(adm))          # columns: geom, part, x, y, hole
  adm_layer <- geom_polygon(data = gdf, aes(x = x, y = y, group = interaction(geom, part)),
                            inherit.aes = FALSE, fill = NA, colour = "grey20", linewidth = 0.18)
  cat("   admin boundaries overlaid from:", adm_path, "\n")
}

asp  <- as.numeric((xmax(te) - xmin(te)) / (ymax(te) - ymin(te)))   # data width / height
maph <- 4.4                                                          # map height (inches)

g2 <- ggplot(map_df, aes(x, y, fill = value)) +
  geom_raster() + adm_layer +
  facet_wrap(~ panel, nrow = 1) +
  scale_fill_gradientn(colours = divpal, limits = c(-vmax, vmax), na.value = "grey85",
                       name = "AGB change\n(MgDM/ha)") +
  coord_equal(xlim = c(xmin(te), xmax(te)), ylim = c(ymin(te), ymax(te)), expand = FALSE) +
  labs(title    = sprintf("%s - aboveground biomass change (%d vs %d)", COUNTRY, BASE_YEAR, END_YEAR),
       subtitle = "green = gain, red = loss, grey = no data", x = NULL, y = NULL) +
  guides(fill = guide_colourbar(barheight = grid::unit(3.2, "cm"), title.position = "top")) +
  theme_minimal(base_size = 12) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(),
        panel.background = element_rect(fill = "grey85", colour = NA),
        panel.spacing = grid::unit(3, "pt"),
        plot.title = element_text(face = "bold", size = 12), strip.text = element_text(size = 11),
        legend.title = element_text(size = 9))

ggsave(file.path(OUT_DIR, paste0(COUNTRY, "_fig2_change_maps.png")),
       g2, width = np * maph * asp + 1.7, height = maph + 1.1, dpi = 150)
cat("   Figure 2 saved.\n")
###############################################################################
## 9. FIGURE 3 : pixel-level simulated vs observed change (debugging_1)
###############################################################################
cat("Drawing Figure 3 (pixel scatter) ...\n")
sc_df <- data.frame(obs = dObs[mask], sim = dCap[mask], config = "MoFuSS - capped regrowth")
if (!is.null(dUnc))
  sc_df <- rbind(sc_df, data.frame(obs = dObs[mask], sim = dUnc[mask], config = "MoFuSS - uncapped regrowth"))
lim <- range(c(sc_df$obs, sc_df$sim), finite = TRUE)

g3 <- ggplot(sc_df, aes(obs, sim)) +
  geom_bin2d(bins = 120) +
  scale_fill_viridis_c(trans = "log10", name = "pixel count") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey25") +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 0.7, formula = y ~ x) +
  coord_equal(xlim = lim, ylim = lim) +
  facet_wrap(~ config) +
  labs(title = paste0(COUNTRY, " - pixel-level simulated vs observed AGB change ",
                      BASE_YEAR, "-", END_YEAR, " (debugging_1; dashed = 1:1)"),
       x = "Observed dAGB (MgDM/ha)", y = "Simulated dAGB (MgDM/ha)") +
  theme_bw(base_size = 12) + theme(plot.title = element_text(size = 11))
ggsave(file.path(OUT_DIR, paste0(COUNTRY, "_fig3_pixel_scatter.png")),
       g3, width = 11, height = 5.2, dpi = 150)

###############################################################################
## 10. SUMMARY TABLE (national totals 2000 / 2025 + net %) and console report
###############################################################################
net <- function(a, b) 100 * (b - a) / a
summ_row <- function(ser) {
  d <- traj[traj$series == ser, ]
  m <- aggregate(total_Mt ~ year, data = d, FUN = mean)      # MC mean
  y0 <- m$total_Mt[m$year == BASE_YEAR]; y1 <- m$total_Mt[m$year == END_YEAR]
  data.frame(series = ser, AGB_2000_Mt = round(y0, 1),
             AGB_2025_Mt = round(y1, 1), net_pct = round(net(y0, y1), 1))
}
summ <- summ_row("Observed")
summ <- rbind(summ, summ_row("Capped"))
if (length(uncMC)) summ <- rbind(summ, summ_row("Uncapped"))
write.csv(summ, file.path(OUT_DIR, paste0(COUNTRY, "_national_summary.csv")), row.names = FALSE)

cat("\n================  SUMMARY  (", COUNTRY, ", ", BASE_YEAR, "-", END_YEAR, ")  ================\n", sep = "")
print(summ, row.names = FALSE)
cat("\nPixel-level change agreement (debugging_1):\n")
print(stats[, c("config","pearson_r","rmse_MgDMha","bias_MgDMha")], row.names = FALSE, digits = 3)
cat("\nOutputs written to:\n  ", OUT_DIR, "\n", sep = "")
cat("  - ", COUNTRY, "_fig1_national_trajectory.png  (all Monte-Carlo runs)\n", sep = "")
cat("  - ", COUNTRY, "_fig2_change_maps.png           (debugging_1)\n", sep = "")
cat("  - ", COUNTRY, "_fig3_pixel_scatter.png         (debugging_1)\n", sep = "")
cat("  - ", COUNTRY, "_national_trajectory_allMC.csv, *_national_summary.csv, *_change_pixelwise_stats.csv\n", sep = "")
cat("Done.\n")
###############################################################################
