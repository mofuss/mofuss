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
# Date: Aug 2025
# Description: Calculate avoided GHG emissions from cookstove interventions, only works pairwise between BaU and any given ICS

# 2dolist ----
# 1.- Fix for regions and personalized AoI
# 2.- Add al monte carlo runs
# 3.- Calculate uncertainty
# 4.- Run enveloped scenarios
# 5.- To spatialize total harvest over population I need to re-spread in some way,
# perhaps dividing in V vs W harvest and respreading based on urban rural population for each year?
# Or justs reducing the demand map by the fraction actually harvested which shouldn't be never more than 10%, 
# moreover after calibration 
# Add pixel base trajectories to see what happens on negative pixels


# Internal parameters ----
bypass = 1

# ======================== EMISSIONS FROM HARVEST ========================
## ======================== 0) Packages ========================
required <- c("terra", "fs", "stringr", "dplyr", "readr")
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)
library(terra); library(fs); library(stringr); library(dplyr); library(readr)

## ======================== 1) Folder pickers ========================
# (Tiny helper; we keep it because dialogs differ by OS/RStudio)

# minimal helper to pick a directory (RStudio dialog → Windows tcltk → readline)
pick_dir <- function(caption = "Select a folder") {
  # 1) RStudio directory picker (any OS)
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::selectDirectory(caption), error = function(e) "")
    if (!is.null(p) && nzchar(p)) return(normalizePath(p, winslash="/", mustWork=FALSE))
  }
  # 2) Windows: tcltk chooser
  if (tolower(Sys.info()[["sysname"]]) == "windows" &&
      requireNamespace("tcltk", quietly = TRUE) && isTRUE(capabilities("tcltk"))) {
    p <- tcltk::tk_choose.dir(default = normalizePath("~"), caption = caption)
    if (!is.na(p) && nzchar(p)) return(normalizePath(p, winslash="/", mustWork=FALSE))
  }
  # 3) Fallback: console input
  p <- readline(paste0(caption, " (paste full path): "))
  if (!nzchar(p)) stop("Folder selection cancelled.")
  if (!dir.exists(p)) stop("Folder does not exist: ", p)
  normalizePath(p, winslash="/", mustWork=FALSE)
}

cat("Please select the folders explicitly (BAU → ICS → OUTPUT).\n")

if (bypass == 1L) {
  bau_dir <- "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/zmb_bau_1km_subc"
  ics_dir <- "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/zmb_ics2_1km_subc"
  output_dir <- "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/emissions"
  rTempdir <- "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/rTemp"
} else {
  bau_dir    <- pick_dir("Select BAU scenario folder")
  ics_dir    <- pick_dir("Select ICS scenario folder")
  output_dir <- pick_dir("Select OUTPUT folder (results will be written here)")
  rTempdir   <- pick_dir("Select your Rtemp folder")
  }
unlink(paste0(output_dir,"/"), recursive = TRUE)
Sys.sleep(5)

# sanity checks
if (identical(bau_dir, ics_dir)) stop("BAU and ICS must be different folders.")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# quick write test in OUTPUT
.testfile <- file.path(output_dir, ".write_test_ok.txt")
ok <- tryCatch({ writeLines("ok", .testfile); TRUE }, error = function(e) FALSE)
if (!ok) stop("Cannot write to the selected OUTPUT folder: ", output_dir)
unlink(.testfile, force = TRUE)

cat("\nBAU: ", bau_dir,
    "\nICS: ", ics_dir,
    "\nOUT: ", output_dir, "\n\n", sep = "")

## ======================== 2) Find debugging_n runs ========================
# (Make sure these are loaded once in your session)
suppressPackageStartupMessages({
  library(terra); library(fs); library(stringr); library(dplyr); library(readr)
})

# Find runs in each scenario
runs_bau <- dir_ls(bau_dir, type = "directory", recurse = TRUE, regexp = "debugging_\\d+$")
runs_ics <- dir_ls(ics_dir, type = "directory", recurse = TRUE, regexp = "debugging_\\d+$")

tb_bau <- tibble(run_dir_bau = runs_bau,
                 run_id = as.integer(str_extract(basename(runs_bau), "\\d+"))) |> arrange(run_id)
tb_ics <- tibble(run_dir_ics = runs_ics,
                 run_id = as.integer(str_extract(basename(runs_ics), "\\d+"))) |> arrange(run_id)

common_runs <- inner_join(tb_bau, tb_ics, by = "run_id")
if (nrow(common_runs) == 0) stop("No common debugging_n runs between BAU and ICS.")

cat("Common runs found:", nrow(common_runs), "\n")

# Build processing plan: for each common run, pick the LATEST common Growth_less_harvXX.tif
plan <- tibble(run_id = integer(), year_code = integer(), bau_file = character(), ics_file = character())

ptn <- "Growth_less_harv(\\d+)\\.tif$"  # case-insensitive pattern

for (k in seq_len(nrow(common_runs))) {
  dir_b <- common_runs$run_dir_bau[k]
  dir_i <- common_runs$run_dir_ics[k]
  
  files_b <- dir_ls(dir_b, type = "file", glob = "*.tif")
  files_i <- dir_ls(dir_i, type = "file", glob = "*.tif")
  
  files_b <- files_b[grepl(ptn, basename(files_b), ignore.case = TRUE)]
  files_i <- files_i[grepl(ptn, basename(files_i), ignore.case = TRUE)]
  
  yrs_b <- tibble(file = files_b,
                  year_code = as.integer(str_match(basename(files_b), regex(ptn, ignore_case = TRUE))[,2]))
  yrs_i <- tibble(file = files_i,
                  year_code = as.integer(str_match(basename(files_i), regex(ptn, ignore_case = TRUE))[,2]))
  
  if (nrow(yrs_b) == 0 || nrow(yrs_i) == 0) next
  common_years <- intersect(yrs_b$year_code, yrs_i$year_code)
  if (!length(common_years)) next
  
  y <- max(common_years)  # latest common year
  plan <- bind_rows(plan, tibble(
    run_id    = common_runs$run_id[k],
    year_code = y,
    bau_file  = yrs_b$file[yrs_b$year_code == y][1],
    ics_file  = yrs_i$file[yrs_i$year_code == y][1]
  ))
}

if (nrow(plan) == 0) stop("Plan is empty. Check file names match 'Growth_less_harvXX.tif' and there are overlapping years.")

write_csv(plan, file.path(output_dir, "plan_runs_and_files.csv"))
cat("Plan rows:", nrow(plan), "→", file.path(output_dir, "plan_runs_and_files.csv"), "\n")



## ======================== 3) Terra options (progress/temp) ========================
terraOptions(progress = 1)
dir.create(file.path(rTempdir), showWarnings = FALSE, recursive = TRUE)
terraOptions(tempdir = file.path(output_dir))

## ======================== 4) Loop: compute ΔAGB → ΔCO2, write per-run rasters ========================
# Factor: biomass → C (0.47), then C → CO2 (44/12)
co2_factor <- 0.47 * (44/12)

per_run <- tibble(run_id = integer(), year_code = integer(), sumco2_Mg = numeric())

# We'll compute mean/SE whenever we have at least 2 runs
min_runs_for_se <- 30

# Accumulators for mean/SE
S  <- NULL  # sum of delta_co2
S2 <- NULL  # sum of (delta_co2^2)
n_stream <- 0L

for (i in seq_len(nrow(plan))) {
  #i = 1
  rid <- plan$run_id[i]; y <- plan$year_code[i]
  
  bau_agb <- rast(plan$bau_file[i])
  ics_agb <- rast(plan$ics_file[i])
  
    # This checks CRS, rows/cols, extent, resolution, and origin.
  # Optional: assert once that grids match (never modify originals)
  if (i == 1L && !terra::compareGeom(bau_agb, ics_agb, stopOnError = FALSE)) {
    stop("BAU & ICS rasters are not perfectly aligned (CRS/grid mismatch). Refusing to resample.")
  }
  
  delta_agb <- ics_agb - bau_agb
  delta_co2 <- delta_agb * co2_factor
  
  # For the first processed run, also save the input rasters AGBs (sanity check)
  if (i == 1L) {
    writeRaster(bau_agb, file.path(output_dir, "bau_agb_mc1.tif"), overwrite = TRUE)
    writeRaster(ics_agb, file.path(output_dir, "ics_agb_mc1.tif"), overwrite = TRUE)
    writeRaster(delta_agb, file.path(output_dir, "delta_agb_mc1.tif"), overwrite = TRUE)
    writeRaster(delta_co2, file.path(output_dir, "delta_co2_mc1.tif"), overwrite = TRUE)
    
    # keep only cells < 0 (others -> NA)
    delta_agb_neg <- terra::ifel(delta_agb < 0, delta_agb, NA)
    
    # save
    writeRaster(delta_agb_neg,
                file.path(output_dir, "delta_agb_mc1_neg.tif"),
                overwrite = TRUE)
    
    # Optional tolerance for near-zero noise:
    eps <- 0   # set to 1e-9 (or 1e-6 in your units) if needed
    
    # 2) Counts (cells), disjoint bins
    n_neg  <- terra::global(delta_agb <  -eps, "sum", na.rm = TRUE)[[1]]
    n_ge0  <- terra::global(delta_agb >= -eps, "sum", na.rm = TRUE)[[1]]
    n_val  <- terra::global(!is.na(delta_agb), "sum", na.rm = TRUE)[[1]]
    
    # Sanity check: counts must add up
    stopifnot(n_val == n_neg + n_ge0)
    
    # 3) Sums of biomass change within each bin
    sum_neg <- terra::global(terra::ifel(delta_agb <  -eps, delta_agb, NA), "sum", na.rm = TRUE)[[1]]
    sum_ge0 <- terra::global(terra::ifel(delta_agb >= -eps, delta_agb, NA), "sum", na.rm = TRUE)[[1]]
    
    # Shares
    gross      <- abs(sum_neg) + sum_ge0
    neg_share  <- if (gross > 0) abs(sum_neg) / gross * 100 else NA_real_
    ge0_share  <- if (gross > 0) sum_ge0      / gross * 100 else NA_real_
    
    # Pretty print
    cat(
      sprintf("NEG (<0):   %s cells | sum = %g | share = %.4f\n",
              format(n_neg, big.mark=","), sum_neg, neg_share),
      sprintf("GE0 (>=0):  %s cells | sum = %g | share = %.4f\n",
              format(n_ge0, big.mark=","), sum_ge0, ge0_share),
      sprintf("VALID CELLS: %s\n", format(n_val, big.mark=",")),
      sep = ""
    )
    
    # Add pixel base trajectories to see what happens on negative pixels
    
  }
  
  # Global sum (Mg CO2) — assumes raster units are Mg biomass per pixel
  sumco2 <- global(delta_co2, "sum", na.rm = TRUE)[[1]]
  per_run <- bind_rows(per_run, tibble(run_id = rid, year_code = y, sumco2_Mg = sumco2))
  
  # Write a per-run ΔCO2 raster (handy to see outputs happening)
  out_delta <- file.path(output_dir, sprintf("delta_co2_run%03d_y%02d.tif", rid, y))
  writeRaster(delta_co2, out_delta, overwrite = TRUE)
  
  # # Accumulate for mean/SE
  # if (is.null(S)) {
  #   S  <- writeRaster(delta_co2,                    file.path(output_dir, "S_delta_co2.tif"),  overwrite = TRUE)
  #   S2 <- writeRaster(delta_co2 * delta_co2,        file.path(output_dir, "S2_delta_co2.tif"), overwrite = TRUE)
  # } else {
  #   S  <- writeRaster(S  + delta_co2,               file.path(output_dir, "S_delta_co2.tif"),  overwrite = TRUE)
  #   S2 <- writeRaster(S2 + (delta_co2 * delta_co2), file.path(output_dir, "S2_delta_co2.tif"), overwrite = TRUE)
  # }
  # n_stream <- n_stream + 1L
}

# Per-run and summary tables
write_csv(per_run, file.path(output_dir, "per_run_sumco2.csv"))

summary_tbl <- per_run |>
  summarise(
    runs           = n(),
    year_used_max  = max(year_code, na.rm = TRUE),
    sumco2_mean_Mg = mean(sumco2_Mg, na.rm = TRUE),
    sumco2_sd_Mg   = sd(sumco2_Mg,   na.rm = TRUE),
    sumco2_se_Mg   = sumco2_sd_Mg / sqrt(runs)
  )
write_csv(summary_tbl, file.path(output_dir, "summary_sumco2.csv"))

# Mean (always, if at least 1 run) and SE (if at least 2 runs)
if (!is.null(S) && n_stream >= 1) {
  mean_r <- S / n_stream
  writeRaster(mean_r, file.path(output_dir, "delta_co2_mean.tif"), overwrite = TRUE)
}

if (!is.null(S2) && n_stream >= min_runs_for_se) {
  var_r <- (S2 - (S * S) / n_stream) / (n_stream - 1)  # unbiased variance
  se_r  <- sqrt(var_r) / sqrt(n_stream)
  writeRaster(se_r, file.path(output_dir, "delta_co2_se.tif"), overwrite = TRUE)
}

cat("Done. Files in:", output_dir, "\n")


## ======================== 5) Write summaries ========================
write_csv(per_run, file.path(output_dir, "per_run_sumco2.csv"))

summary_tbl <- per_run |>
  summarise(
    runs           = n(),
    year_used_max  = max(year_code, na.rm = TRUE),
    sumco2_mean_Mg = mean(sumco2_Mg, na.rm = TRUE),
    sumco2_sd_Mg   = sd(sumco2_Mg,   na.rm = TRUE),
    sumco2_se_Mg   = sumco2_sd_Mg / sqrt(runs)
  )
write_csv(summary_tbl, file.path(output_dir, "summary_sumco2.csv"))

# ======================== EMISSIONS FROM END-USE: Harvest sums ========================
message("\n[End-Use] Summing all Harvest_totXX.tif per run (BAU & ICS)…")

# Helpers -------------------------------------------------------------
.sum_harvest_in_run <- function(run_dir, out_prefix, outdir, mc1_tag = FALSE) {
  # list all .tif files in the run folder (non-recursive is usually enough; use recurse=TRUE if needed)
  files <- fs::dir_ls(run_dir, type = "file", glob = "*.tif")
  # keep Harvest_totXX.tif (case-insensitive), XX = 01..99+
  ptn   <- "(?i)^Harvest_tot(\\d+)\\.tif$"
  files <- files[grepl(ptn, basename(files))]
  if (!length(files)) return(list(ok = FALSE, msg = paste0("No Harvest_totXX.tif in: ", run_dir)))
  
  # order by XX so logs look neat (not strictly required for the sum)
  ord <- stringr::str_match(basename(files), regex(ptn))[,2] |> as.integer()
  files <- files[order(ord)]
  
  # read first to establish geometry
  r0 <- terra::rast(files[1])
  # sanity: ensure all geometries match (CRS/grid/extent/resolution/origin)
  for (f in files[-1]) {
    if (!terra::compareGeom(r0, terra::rast(f), stopOnError = FALSE)) {
      stop("[End-Use] Geometry mismatch within run folder: ", run_dir,
           "\nProblematic file: ", f)
    }
  }
  
  # stack + sum (terra sums cell-wise, ignoring NA)
  # Use wrap in SpatRaster list to avoid unexpected memory spikes
  rlist <- lapply(files, terra::rast)
  rs    <- terra::rast(rlist)
  rsum  <- terra::app(rs, fun = sum, na.rm = TRUE)
  
  # write outputs
  out_tif <- file.path(outdir, paste0(out_prefix, "_harvest_sum.tif"))
  terra::writeRaster(rsum, out_tif, overwrite = TRUE)
  
  # global total (assumes per-pixel units are Mg biomass; change if different)
  gsum <- as.numeric(terra::global(rsum, "sum", na.rm = TRUE)[[1]])
  
  # if this is Monte Carlo #1, save a convenience copy
  if (mc1_tag) {
    mc1_tif <- file.path(outdir, paste0(out_prefix, "_harvest_sum_mc1.tif"))
    terra::writeRaster(rsum, mc1_tif, overwrite = TRUE)
    # also stash the list of source files for traceability
    readr::write_lines(files, file.path(outdir, paste0(out_prefix, "_harvest_sources_mc1.txt")))
  }
  
  list(ok = TRUE, out_tif = out_tif, global_sum = gsum, n_layers = length(files))
}

# Output subfolders ---------------------------------------------------
enduse_dir_bau <- file.path(output_dir, "enduse_bau")
enduse_dir_ics <- file.path(output_dir, "enduse_ics")
dir.create(enduse_dir_bau, showWarnings = FALSE, recursive = TRUE)
dir.create(enduse_dir_ics, showWarnings = FALSE, recursive = TRUE)

# Accumulators for CSVs
harv_tbl_bau <- tibble(run_id = integer(), n_layers = integer(), global_sum_Mg = numeric(), out_tif = character())
harv_tbl_ics <- tibble(run_id = integer(), n_layers = integer(), global_sum_Mg = numeric(), out_tif = character())

# Iterate common runs (derived above) --------------------------------
for (k in seq_len(nrow(common_runs))) {
  rid   <- common_runs$run_id[k]
  dir_b <- common_runs$run_dir_bau[k]
  dir_i <- common_runs$run_dir_ics[k]
  
  # BAU
  out_prefix_b <- sprintf("bau_run%03d", rid)
  res_b <- .sum_harvest_in_run(
    run_dir = dir_b,
    out_prefix = out_prefix_b,
    outdir = enduse_dir_bau,
    mc1_tag = (rid == 1L)
  )
  if (isTRUE(res_b$ok)) {
    harv_tbl_bau <- bind_rows(harv_tbl_bau,
                              tibble(run_id = rid,
                                     n_layers = res_b$n_layers,
                                     global_sum_Mg = res_b$global_sum,
                                     out_tif = res_b$out_tif))
  } else {
    message("[End-Use][BAU] ", res_b$msg)
  }
  
  # ICS
  out_prefix_i <- sprintf("ics_run%03d", rid)
  res_i <- .sum_harvest_in_run(
    run_dir = dir_i,
    out_prefix = out_prefix_i,
    outdir = enduse_dir_ics,
    mc1_tag = (rid == 1L)
  )
  if (isTRUE(res_i$ok)) {
    harv_tbl_ics <- bind_rows(harv_tbl_ics,
                              tibble(run_id = rid,
                                     n_layers = res_i$n_layers,
                                     global_sum_Mg = res_i$global_sum,
                                     out_tif = res_i$out_tif))
  } else {
    message("[End-Use][ICS] ", res_i$msg)
  }
}

# Write per-scenario summaries ---------------------------------------
readr::write_csv(harv_tbl_bau, file.path(output_dir, "enduse_bau_harvest_per_run.csv"))
readr::write_csv(harv_tbl_ics, file.path(output_dir, "enduse_ics_harvest_per_run.csv"))

# Optional quick stats
enduse_summary <- bind_rows(
  harv_tbl_bau |> mutate(scenario = "BAU"),
  harv_tbl_ics |> mutate(scenario = "ICS")
) |>
  group_by(scenario) |>
  summarise(
    runs          = n(),
    mean_Mg       = mean(global_sum_Mg, na.rm = TRUE),
    sd_Mg         = sd(global_sum_Mg,   na.rm = TRUE),
    se_Mg         = sd_Mg / sqrt(runs),
    .groups = "drop"
  )

readr::write_csv(enduse_summary, file.path(output_dir, "enduse_harvest_summary.csv"))

message("[End-Use] Done. Rasters in:\n  - ", enduse_dir_bau, "\n  - ", enduse_dir_ics,
        "\nTables:\n  - ", file.path(output_dir, "enduse_bau_harvest_per_run.csv"),
        "\n  - ", file.path(output_dir, "enduse_ics_harvest_per_run.csv"),
        "\n  - ", file.path(output_dir, "enduse_harvest_summary.csv"))


# Compare Harvest vs Demand to extract % of unmet demand



