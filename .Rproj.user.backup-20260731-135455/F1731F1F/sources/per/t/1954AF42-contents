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

# MoFuSS
# Version 5
# Date: May 2026
# Description: Decompose Delta AGB 2050 (ICS - BaU) into AVOIDED LOSS and REGROWTH,
#              pairwise between BaU and any given ICS, per regrowth configuration.
#              Also rebuilds the BaU-vs-ICS comparison table by READING the existing
#              emissions outputs (harvest + enduse + totals) so numbers never drift
#              from the emissions pipeline (3post_emissions_bau-vs-ics_v7_mofuss.R).
#
#              Decomposition definition (deterministic, MC turned off; uses MC1 / run001):
#                gate         : BaU2050 < AGB2000              (BaU is the loss scenario)
#                avoided_loss : min(ICS2050, AGB2000) - BaU2050   on gated pixels, else 0
#                regrowth     : delta_agb - avoided_loss          (residual; sums exact)
#              Terms are SIGNED (not clamped) so avoided_loss + regrowth == delta_agb
#              exactly. Counts of negative pixels are reported, not hidden.
#
#              Capped vs uncapped are INDEPENDENT enveloped simulations: each config is
#              decomposed on its own footprint. No cross-config pixel subtraction.

# 2dolist ----
# 1.- Add all Monte Carlo runs (currently MC1 / run001 only, matching deterministic table)
# 2.- Calculate uncertainty across runs for the decomposition
# 3.- Optional: pixel-base trajectories on negative pixels

# Internal parameters ----
fixdir   <- 0          # WARNING: when 1, set the *_dir paths by hand below instead of dialogs
co2_factor <- 0.47 * (44/12)  # biomass -> C (0.47), then C -> CO2 (44/12). Match emissions script.
mc_run     <- 1L       # Monte Carlo realization to decompose (1 = run001 = MC1). Deterministic for now.
agb_year   <- 51L      # year code for AGB 2050 (51 = 2050, matches Growth_less_harvXX naming)
eps        <- 1e-6     # tolerance (Mg) for "near zero" when classifying gate/exceedance
make_plot  <- TRUE     # write a quick bar plot of the decomposition per config

# Relative paths inside each scenario folder (MoFuSS conventions) ----
rel_agb2050 <- file.path("debugging_%d", sprintf("Growth_less_harv%02d.tif", agb_year)) # %d = mc_run
rel_agb2000 <- file.path("LULCC", "TempRaster", "agb3_c.tif")  # carrying-capacity reference (year 2000)

# Hand-set paths (only used if fixdir == 1) ----
# A "config" is one triplet: BaU folder, ICS folder, Emissions output folder.
# To loop both regrowth configs, list more than one triplet.
if (fixdir == 1L) {
  configs_fixed <- list(
    list(label = "capped",
         bau   = "D:/ken_fafe/nv3/ken_bau1_1km_nv3_ng",
         ics   = "D:/ken_fafe/nv3/ken_ics2_1km_nv3_ng",
         emis  = "D:/ken_fafe/nv3/ken_emissions_ics2"),
    list(label = "uncapped",
         bau   = "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/ken_bau1_1km_nv3_g",
         ics   = "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/ken_ics2_1km_nv3_g",
         emis  = "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/ken_emissions_ics2")
  )
}

# Where to write the decomposition outputs (chosen via dialog when fixdir != 1) ----
# decomp_out_fixed <- "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/agb_decomposition"

# Load packages ----
required <- c("terra", "fs", "stringr", "dplyr", "readr", "tibble")
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)
suppressPackageStartupMessages({
  library(terra); library(fs); library(stringr); library(dplyr); library(readr); library(tibble)
})

## ======================== 1) Folder pickers ========================
# Same helper as the emissions script (RStudio dialog -> Windows tcltk -> console).
pick_dir <- function(caption = "Select a folder") {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::selectDirectory(caption), error = function(e) NULL)
    # A GUI picker WAS shown. Trust its result: empty/NULL == Cancel == stop.
    # Do NOT fall through to other methods (that caused idling on readline).
    if (is.null(p) || !nzchar(p)) return("")
    return(normalizePath(p, winslash = "/", mustWork = FALSE))
  }
  if (tolower(Sys.info()[["sysname"]]) == "windows" &&
      requireNamespace("tcltk", quietly = TRUE) && isTRUE(capabilities("tcltk"))) {
    p <- tcltk::tk_choose.dir(default = normalizePath("~"), caption = caption)
    # tk_choose.dir returns NA on Cancel. Trust it: NA/empty == stop.
    if (length(p) != 1 || is.na(p) || !nzchar(p)) return("")
    return(normalizePath(p, winslash = "/", mustWork = FALSE))
  }
  # No GUI picker available: console fallback only.
  p <- readline(paste0(caption, " (paste full path, or blank to stop): "))
  if (!nzchar(p)) return("")  # blank = stop adding configs
  if (!dir.exists(p)) stop("Folder does not exist: ", p)
  normalizePath(p, winslash = "/", mustWork = FALSE)
}

# Derive a config label from the ICS folder name: "_g" = uncapped, "_ng" = capped, plus icsN tag.
config_label <- function(ics_dir) {
  ics_tag <- stringr::str_extract(basename(ics_dir), "ics\\d+")
  cap_tag <- if (grepl("_ng(_|$)", basename(ics_dir)) || grepl("_ng$", basename(ics_dir))) "capped"
  else if (grepl("_g(_|$)",  basename(ics_dir)) || grepl("_g$",  basename(ics_dir))) "uncapped"
  else "config"
  paste(na.omit(c(cap_tag, ics_tag)), collapse = "_")
}

# Collect the TWO fixed configs with explicit, labelled prompts (no open-ended loop).
# Capped = _ng (carrying capacity = AGB2000). Uncapped = _g (biophysical capacity).
collect_configs <- function() {
  cat("\n>>> CAPPED configuration (_ng): select its BaU, ICS, and Emissions folders.\n")
  cap_bau  <- pick_dir("CAPPED (_ng): select BAU scenario folder")
  cap_ics  <- pick_dir("CAPPED (_ng): select ICS scenario folder")
  cap_emis <- pick_dir("CAPPED (_ng): select EMISSIONS output folder")
  
  cat("\n>>> UNCAPPED configuration (_g): select its BaU, ICS, and Emissions folders.\n")
  unc_bau  <- pick_dir("UNCAPPED (_g): select BAU scenario folder")
  unc_ics  <- pick_dir("UNCAPPED (_g): select ICS scenario folder")
  unc_emis <- pick_dir("UNCAPPED (_g): select EMISSIONS output folder")
  
  picked <- c(cap_bau, cap_ics, cap_emis, unc_bau, unc_ics, unc_emis)
  if (any(!nzchar(picked))) stop("All six folders are required (a picker was cancelled).")
  if (identical(cap_bau, cap_ics)) stop("CAPPED BAU and ICS must be different folders.")
  if (identical(unc_bau, unc_ics)) stop("UNCAPPED BAU and ICS must be different folders.")
  
  # Use the user's intent for the label (capped/uncapped) but keep the icsN tag auto-detected.
  cap_tag <- stringr::str_extract(basename(cap_ics), "ics\\d+")
  unc_tag <- stringr::str_extract(basename(unc_ics), "ics\\d+")
  cap_lab <- paste(na.omit(c("capped",   cap_tag)), collapse = "_")
  unc_lab <- paste(na.omit(c("uncapped", unc_tag)), collapse = "_")
  
  # Soft sanity warning if folder suffixes don't match the slot they were picked into.
  if (!grepl("_ng$|_ng_", basename(cap_ics)))
    warning("CAPPED ICS folder name '", basename(cap_ics), "' does not end in _ng - double-check the selection.")
  if (!grepl("_g$|_g_", basename(unc_ics)) || grepl("_ng$|_ng_", basename(unc_ics)))
    warning("UNCAPPED ICS folder name '", basename(unc_ics), "' does not look like _g - double-check the selection.")
  
  list(
    list(label = cap_lab, bau = cap_bau, ics = cap_ics, emis = cap_emis),
    list(label = unc_lab, bau = unc_bau, ics = unc_ics, emis = unc_emis)
  )
}

if (fixdir == 1L) {
  configs <- configs_fixed
  decomp_out <- if (exists("decomp_out_fixed")) decomp_out_fixed else dirname(configs[[1]]$emis)
} else {
  cat("Select folders for BOTH configurations: CAPPED (_ng) first, then UNCAPPED (_g).\n")
  configs <- collect_configs()
  decomp_out <- pick_dir("Select OUTPUT folder for the decomposition + comparison table")
  if (!nzchar(decomp_out)) stop("Output folder selection cancelled.")
}

dir.create(decomp_out, showWarnings = FALSE, recursive = TRUE)
cat("\nConfigs to process:", length(configs), "| Output:", decomp_out, "\n")

## ======================== 2) Helpers ========================

# Read the harvest (AGB-avoided) total tCO2e for the chosen MC run from per_run_sumco2.csv.
# This is the value the emissions pipeline already validated; we READ it (no recompute).
read_harvest_tco2e <- function(emis_dir, run_id = mc_run) {
  f <- file.path(emis_dir, "harvest", "per_run_sumco2.csv")
  if (!file.exists(f)) stop("Missing per_run_sumco2.csv in: ", file.path(emis_dir, "harvest"))
  pr <- readr::read_csv(f, show_col_types = FALSE)
  row <- dplyr::filter(pr, run_id == !!run_id)
  if (!nrow(row)) stop(sprintf("run_id %d not found in %s", run_id, f))
  as.numeric(row$sumco2_Mg[1])  # units are Mg CO2 == tCO2e
}

# Read the End-use total tCO2e (sum of per-fuel Delta) from summary_co2_<fy>-<ly>.csv.
# Robust to: (a) presence/absence of an unlabelled grand-total row, (b) comma thousands
# separators and trailing spaces. We sum the per-fuel Delta rows ourselves (authoritative).
read_enduse_tco2e <- function(emis_dir) {
  ed <- file.path(emis_dir, "enduse")
  f <- list.files(ed, pattern = "^summary_co2_.*\\.csv$", full.names = TRUE)
  if (!length(f)) stop("Missing summary_co2_*.csv in: ", ed)
  f <- f[1]
  raw <- readr::read_csv(f, show_col_types = FALSE, na = character())
  num <- function(x) as.numeric(gsub(",", "", trimws(as.character(x))))
  delta_rows <- raw[trimws(raw$scenario) == "Delta" & nzchar(trimws(raw$fuel)), , drop = FALSE]
  if (!nrow(delta_rows)) stop("No per-fuel Delta rows in: ", f)
  sum(num(delta_rows$total_tCO2e), na.rm = TRUE)
}

# Load the three AGB rasters for a config; align check; return masked-common values + a template.
load_agb <- function(cfg) {
  bau_f <- file.path(cfg$bau, sprintf(rel_agb2050, mc_run))
  ics_f <- file.path(cfg$ics, sprintf(rel_agb2050, mc_run))
  ref_f <- file.path(cfg$bau, rel_agb2000)   # AGB2000 lives in BaU folder; identical in ICS (verified)
  for (f in c(bau_f, ics_f, ref_f)) if (!file.exists(f)) stop("Missing raster: ", f)
  
  bau <- terra::rast(bau_f)
  ics <- terra::rast(ics_f)
  ref <- terra::rast(ref_f)
  
  if (!terra::compareGeom(bau, ics, stopOnError = FALSE))
    stop("BaU & ICS AGB2050 rasters not aligned for config '", cfg$label, "'.")
  if (!terra::compareGeom(bau, ref, stopOnError = FALSE))
    stop("AGB2000 (agb3_c) not aligned with AGB2050 for config '", cfg$label,
         "'. (Check it is the same grid; do not resample silently.)")
  
  list(bau = bau, ics = ics, ref = ref, bau_f = bau_f, ics_f = ics_f, ref_f = ref_f)
}

## ======================== 3) Per-config decomposition ========================
results <- tibble(
  config = character(), label = character(),
  bau_agb_2050_mg = numeric(), ics_agb_2050_mg = numeric(), delta_agb_2050_mg = numeric(),
  avoided_loss_mg = numeric(), regrowth_mg = numeric(),
  avoided_loss_tco2e = numeric(), regrowth_tco2e = numeric(),
  agb_avoided_tco2e = numeric(),      # = harvest MC1 from emissions pipeline (read)
  enduse_avoided_tco2e = numeric(),   # = end-use delta from emissions pipeline (read)
  total_avoided_tco2e = numeric(),    # = agb_avoided + enduse_avoided
  n_common = integer(), n_gated = integer(),
  n_neg_avoided = integer(), n_neg_regrowth = integer(),
  recon_ok = logical()
)

for (cfg in configs) {
  cat("\n==================== Config:", cfg$label, "====================\n")
  cat("  BaU :", cfg$bau, "\n  ICS :", cfg$ics, "\n  Emis:", cfg$emis, "\n")
  
  R <- load_agb(cfg)
  
  # Common valid footprint: all three finite (terra treats NoData as NA on read).
  valid <- !is.na(R$bau) & !is.na(R$ics) & !is.na(R$ref)
  
  bau_v <- terra::mask(R$bau, valid, maskvalue = FALSE)
  ics_v <- terra::mask(R$ics, valid, maskvalue = FALSE)
  ref_v <- terra::mask(R$ref, valid, maskvalue = FALSE)
  
  delta_agb <- ics_v - bau_v
  
  # gate: BaU2050 < AGB2000
  gate <- bau_v < (ref_v - eps)
  # ICS capped at AGB2000 for the avoided-loss credit
  ics_capped <- terra::ifel(ics_v > ref_v, ref_v, ics_v)
  # avoided_loss = (min(ICS, AGB2000) - BaU) on gated pixels, else 0
  avoided_loss <- terra::ifel(gate, ics_capped - bau_v, 0)
  # regrowth = delta_agb - avoided_loss   (residual; signed)
  regrowth <- delta_agb - avoided_loss
  
  # --- global sums (Mg) ---
  g <- function(r) as.numeric(terra::global(r, "sum", na.rm = TRUE)[1, 1])
  bau_sum   <- g(bau_v)
  ics_sum   <- g(ics_v)
  delta_sum <- g(delta_agb)
  al_sum    <- g(avoided_loss)
  rg_sum    <- g(regrowth)
  
  # --- diagnostics ---
  n_common <- as.integer(g(terra::ifel(valid, 1, NA)))
  n_gated  <- as.integer(g(terra::ifel(gate, 1, NA)))
  n_neg_al <- as.integer(g(terra::ifel(avoided_loss < -eps, 1, NA)))
  n_neg_rg <- as.integer(g(terra::ifel(regrowth     < -eps, 1, NA)))
  
  # --- table numbers READ from the emissions pipeline (no recompute / no drift) ---
  agb_avoided_tco2e    <- read_harvest_tco2e(cfg$emis, run_id = mc_run)
  enduse_avoided_tco2e <- read_enduse_tco2e(cfg$emis)
  total_avoided_tco2e  <- agb_avoided_tco2e + enduse_avoided_tco2e
  
  # --- reconciliation: avoided + regrowth == delta_agb, and delta*co2_factor == harvest ---
  recon_split   <- abs((al_sum + rg_sum) - delta_sum) < 1.0
  recon_harvest <- abs(delta_sum * co2_factor - agb_avoided_tco2e) < max(2.0, abs(agb_avoided_tco2e) * 1e-6)
  recon_ok <- recon_split && recon_harvest
  
  cat(sprintf("  Common cells: %s | Gated (BaU<AGB2000): %s\n",
              format(n_common, big.mark = ","), format(n_gated, big.mark = ",")))
  cat(sprintf("  Delta AGB    : %18s Mg  (%s tCO2e)\n",
              format(round(delta_sum), big.mark = ","), format(round(delta_sum * co2_factor), big.mark = ",")))
  cat(sprintf("  Avoided loss : %18s Mg  (%s tCO2e)\n",
              format(round(al_sum), big.mark = ","), format(round(al_sum * co2_factor), big.mark = ",")))
  cat(sprintf("  Regrowth     : %18s Mg  (%s tCO2e)\n",
              format(round(rg_sum), big.mark = ","), format(round(rg_sum * co2_factor), big.mark = ",")))
  cat(sprintf("  Neg pixels   : avoided=%s  regrowth=%s  (signed, unclamped)\n",
              format(n_neg_al, big.mark = ","), format(n_neg_rg, big.mark = ",")))
  cat(sprintf("  Reconcile split=%s  harvest=%s\n", recon_split, recon_harvest))
  if (!recon_ok) warning("Reconciliation failed for config '", cfg$label, "' - inspect before trusting outputs.")
  
  # --- write per-pixel rasters (avoided_loss, regrowth) in tCO2e and Mg ---
  cfg_out <- file.path(decomp_out, cfg$label)
  dir.create(cfg_out, showWarnings = FALSE, recursive = TRUE)
  
  terra::writeRaster(avoided_loss,             file.path(cfg_out, "avoided_loss_mg.tif"),    overwrite = TRUE)
  terra::writeRaster(regrowth,                 file.path(cfg_out, "regrowth_mg.tif"),         overwrite = TRUE)
  terra::writeRaster(avoided_loss * co2_factor, file.path(cfg_out, "avoided_loss_tco2e.tif"), overwrite = TRUE)
  terra::writeRaster(regrowth     * co2_factor, file.path(cfg_out, "regrowth_tco2e.tif"),     overwrite = TRUE)
  terra::writeRaster(delta_agb,                file.path(cfg_out, "delta_agb_mg.tif"),        overwrite = TRUE)
  
  # built-in raster check: avoided + regrowth must equal delta_agb pixel-wise
  chk <- terra::global(abs((avoided_loss + regrowth) - delta_agb), "max", na.rm = TRUE)[1, 1]
  cat(sprintf("  Raster identity max|.(avoided+regrowth) - delta| = %.6g Mg\n", chk))
  
  results <- bind_rows(results, tibble(
    config = cfg$label, label = cfg$label,
    bau_agb_2050_mg = bau_sum, ics_agb_2050_mg = ics_sum, delta_agb_2050_mg = delta_sum,
    avoided_loss_mg = al_sum, regrowth_mg = rg_sum,
    avoided_loss_tco2e = al_sum * co2_factor, regrowth_tco2e = rg_sum * co2_factor,
    agb_avoided_tco2e = agb_avoided_tco2e,
    enduse_avoided_tco2e = enduse_avoided_tco2e,
    total_avoided_tco2e = total_avoided_tco2e,
    n_common = n_common, n_gated = n_gated,
    n_neg_avoided = n_neg_al, n_neg_regrowth = n_neg_rg,
    recon_ok = recon_ok
  ))
}

## ======================== 4) Write summary CSV ========================
summary_csv <- file.path(decomp_out, "agb_decomposition_summary.csv")
readr::write_csv(results, summary_csv)
cat("\nSaved decomposition summary ->", summary_csv, "\n")

# Also write a "wide" comparison table mirroring the uploaded screenshot layout, one column per config.
wide <- results |>
  transmute(
    label,
    `BAU AGB 2050 (Mg)`        = round(bau_agb_2050_mg),
    `ICS AGB 2050 (Mg)`        = round(ics_agb_2050_mg),
    `Delta AGB 2050 (Mg)`      = round(delta_agb_2050_mg),
    `Avoided loss (Mg)`        = round(avoided_loss_mg),
    `Regrowth (Mg)`            = round(regrowth_mg),
    `Avoided loss (tCO2e)`     = round(avoided_loss_tco2e),
    `Regrowth (tCO2e)`         = round(regrowth_tco2e),
    `AGB-avoided MC1 (tCO2e)`  = round(agb_avoided_tco2e),
    `End-use delta (tCO2e)`    = round(enduse_avoided_tco2e),
    `Total avoided MC1 (tCO2e)`= round(total_avoided_tco2e)
  )
# transpose to metric-rows x config-cols (screenshot orientation)
wide_t <- as.data.frame(t(wide[setdiff(names(wide), "label")]))
colnames(wide_t) <- wide$label
wide_t <- tibble::rownames_to_column(wide_t, var = "Metric")
table_csv <- file.path(decomp_out, "comparison_table.csv")
readr::write_csv(wide_t, table_csv)
cat("Saved comparison table     ->", table_csv, "\n")

## ======================== 5) Quick plot ========================
if (isTRUE(make_plot)) {
  png_path <- file.path(decomp_out, "agb_decomposition_plot.png")
  cols <- c(`Avoided loss` = "#E1A100", Regrowth = "#1B9E77")  # orange / green, matching the concept figure
  mat <- rbind(`Avoided loss` = results$avoided_loss_tco2e,
               Regrowth       = results$regrowth_tco2e) / 1e6   # MtCO2e
  colnames(mat) <- results$label
  
  # Stacked bars assume non-negative parts. If any component is negative (signed split),
  # fall back to grouped (beside) bars so nothing is hidden or misrendered.
  beside <- any(mat < 0)
  grDevices::png(png_path, width = 1100, height = 700, res = 130)
  op <- par(mar = c(5, 5, 4, 8), xpd = NA)
  bp <- barplot(mat, beside = beside, col = cols,
                ylab = expression("Avoided emissions (Mt CO"[2]*"e)"),
                main = paste0("AGB 2050 decomposition: avoided loss vs regrowth",
                              if (beside) "  (grouped: a component is negative)" else ""),
                border = NA)
  if (!beside) {
    totals <- colSums(mat)
    text(bp, totals, labels = formatC(totals, format = "f", digits = 1),
         pos = 3, cex = 0.9, font = 2)
  }
  legend("topright", inset = c(-0.22, 0), fill = cols, legend = names(cols),
         bty = "n", border = NA, title = "Component")
  par(op)
  grDevices::dev.off()
  cat("Saved plot                 ->", png_path, "\n")
}

cat("\n[OK] AGB decomposition complete for", nrow(results), "config(s).\n")
print(as.data.frame(wide_t), row.names = FALSE)

