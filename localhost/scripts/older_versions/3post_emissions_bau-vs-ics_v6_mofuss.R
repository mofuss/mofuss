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
fixdir = 1
bau_dir <- "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/zmb_bau_1km_subc"
ics_dir <- "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/zmb_ics3_1km_subc"
output_dir2 <- "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/emissions"
rTempdir <- "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/rTemp"
gid0       <- "ZMB"
efchratio  <- 6
impchfw <- 0 #turns on and off imp_charcoal and imp_fuelwood
# ---- Map window (Growth_less_harvXX.tif) ----
# Use NULL to auto-span available years; or set explicit bounds like 11 and 25/41
# frequent preset A:
harv_first <- 11; harv_last <- 25
# frequent preset B:
# harv_first <- 11; harv_last <- 41

output_dir <- paste0(output_dir2,"_",stringr::str_extract(ics_dir, "ics\\d+"))

# ======================== EMISSIONS FROM HARVEST ========================
## ======================== 0) Packages ========================
required <- c("terra", "fs", "stringr", "dplyr", "readr")
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)
library(terra); library(fs); library(stringr); library(dplyr); library(readr)

## ======================== 1) Temporal window ========================
# Keep only Growth_less_harvXX.tif whose XX is within [first,last]
.filter_harv_range <- function(files, ptn, first = NULL, last = NULL) {
  if (!length(files)) return(character())
  mm <- stringr::str_match(basename(files), stringr::regex(ptn, ignore_case = TRUE))
  yy <- suppressWarnings(as.integer(mm[,2]))
  keep <- !is.na(yy)
  if (!is.null(first)) keep <- keep & (yy >= first)
  if (!is.null(last))  keep <- keep & (yy <= last)
  files[keep]
}

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

if (fixdir != 1L) {
  bau_dir    <- pick_dir("Select BAU scenario folder")
  ics_dir    <- pick_dir("Select ICS scenario folder")
  output_dir <- pick_dir("Select OUTPUT folder (results will be written here)")
  rTempdir   <- pick_dir("Select your Rtemp folder")
  }
# Respect previous scenarios?
unlink(paste0(output_dir,"/"), recursive = TRUE)
Sys.sleep(5)

# sanity checks
if (identical(bau_dir, ics_dir)) stop("BAU and ICS must be different folders.")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output_dir,"/harvest"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output_dir,"/enduse"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output_dir,"/summary_mc1"), showWarnings = FALSE, recursive = TRUE)

# quick write test in OUTPUT
.testfile <- file.path(output_dir, ".write_test_ok.txt")
ok <- tryCatch({ writeLines("ok", .testfile); TRUE }, error = function(e) FALSE)
if (!ok) stop("Cannot write to the selected OUTPUT folder: ", output_dir)
unlink(.testfile, force = TRUE)

cat("\nBAU: ", bau_dir,
    "\nICS: ", ics_dir,
    "\nOUT: ", output_dir, "\n\n", sep = "")

## ======================== 2) Find debugging_n runs (strict) ========================
# Expect exact names like ".../debugging_01", ".../debugging_41" (lowercase)
.strict_run_rows <- function(root) {
  all_dirs <- fs::dir_ls(root, type = "directory", recurse = TRUE)
  m <- grepl("[/\\\\]debugging_\\d{2}$", all_dirs, perl = TRUE)      # exact "debugging_??"
  cand <- all_dirs[m]
  if (!length(cand)) return(tibble(run_dir = character(), run_id = integer()))
  # last two digits → integer id
  rid <- as.integer(sub(".*[/\\\\]debugging_(\\d{2})$", "\\1", cand, perl = TRUE))
  tibble(run_dir = cand, run_id = rid) |> dplyr::arrange(run_id)
}

tb_bau <- .strict_run_rows(bau_dir) |> dplyr::rename(run_dir_bau = run_dir)
tb_ics <- .strict_run_rows(ics_dir) |> dplyr::rename(run_dir_ics = run_dir)

common_runs <- dplyr::inner_join(tb_bau, tb_ics, by = "run_id") |> dplyr::arrange(run_id)

cat(sprintf("[runs] BAU=%d | ICS=%d | common=%d (expected 41 if complete)\n",
            nrow(tb_bau), nrow(tb_ics), nrow(common_runs)))
if (nrow(common_runs) == 0) stop("No matching 'debugging_??' runs were found in both scenarios.")

# Build processing plan: for each common run, pick the LAST common Growth_less_harvXX.tif
## ======================== Plan: pick LAST within [harv_first, harv_last] ========================
# Expect exact filenames "Growth_less_harvXX.tif" (case-sensitive), XX in 01..41
stopifnot(is.null(harv_first) || (is.numeric(harv_first) && harv_first >= 1 && harv_first <= 41))
stopifnot(is.null(harv_last)  || (is.numeric(harv_last)  && harv_last  >= 1 && harv_last  <= 41))

plan <- tibble(
  run_id    = integer(),
  first_req = integer(),
  last_req  = integer(),
  year_used = integer(),
  bau_file  = character(),
  ics_file  = character()
)

growth_window_log <- tibble(
  run_id      = integer(),
  avail_min   = integer(),
  avail_max   = integer(),
  first_req   = integer(),
  last_req    = integer(),
  used_min    = integer(),
  used_max    = integer(),
  chosen_year = integer(),
  note        = character()
)

GROWTH_PTN <- "^Growth_less_harv(\\d{2})\\.tif$"     # case-sensitive; exactly 2 digits

for (k in seq_len(nrow(common_runs))) {
  rid   <- common_runs$run_id[k]
  dir_b <- common_runs$run_dir_bau[k]
  dir_i <- common_runs$run_dir_ics[k]
  
  fb <- fs::dir_ls(dir_b, type = "file", glob = "*.tif")
  fi <- fs::dir_ls(dir_i, type = "file", glob = "*.tif")
  
  # keep only exact Growth_less_harvXX.tif (no ignore.case here)
  fb <- fb[grepl(GROWTH_PTN, basename(fb))]
  fi <- fi[grepl(GROWTH_PTN, basename(fi))]
  
  if (!length(fb) || !length(fi)) {
    growth_window_log <- dplyr::bind_rows(growth_window_log, tibble(
      run_id = rid, avail_min = NA, avail_max = NA,
      first_req = harv_first, last_req = harv_last,
      used_min = NA, used_max = NA, chosen_year = NA,
      note = "No Growth_less_harvXX.tif found in one/both runs"
    ))
    next
  }
  
  # extract XX as integers (01..41)
  xb <- as.integer(sub(GROWTH_PTN, "\\1", basename(fb)))
  xi <- as.integer(sub(GROWTH_PTN, "\\1", basename(fi)))
  
  common_xx <- sort(intersect(xb, xi))
  if (!length(common_xx)) {
    growth_window_log <- dplyr::bind_rows(growth_window_log, tibble(
      run_id = rid, avail_min = NA, avail_max = NA,
      first_req = harv_first, last_req = harv_last,
      used_min = NA, used_max = NA, chosen_year = NA,
      note = "No common XX between BAU and ICS"
    ))
    next
  }
  
  avail_min <- min(common_xx); avail_max <- max(common_xx)
  
  # apply strict window (no fallback; you want it strict)
  used_xx <- common_xx
  if (!is.null(harv_first)) used_xx <- used_xx[used_xx >= harv_first]
  if (!is.null(harv_last))  used_xx <- used_xx[used_xx <= harv_last]
  
  if (!length(used_xx)) {
    growth_window_log <- dplyr::bind_rows(growth_window_log, tibble(
      run_id = rid,
      avail_min = avail_min, avail_max = avail_max,
      first_req = harv_first, last_req = harv_last,
      used_min = NA, used_max = NA, chosen_year = NA,
      note = "Requested window has no overlap with available XX"
    ))
    next
  }
  
  used_min <- min(used_xx); used_max <- max(used_xx)
  y <- max(used_xx)  # choose last within window
  
  bau_file <- fb[xb == y][1]
  ics_file <- fi[xi == y][1]
  
  plan <- dplyr::bind_rows(plan, tibble(
    run_id    = rid,
    first_req = harv_first,
    last_req  = harv_last,
    year_used = y,
    bau_file  = bau_file,
    ics_file  = ics_file
  ))
  
  growth_window_log <- dplyr::bind_rows(growth_window_log, tibble(
    run_id = rid,
    avail_min = avail_min, avail_max = avail_max,
    first_req = harv_first, last_req = harv_last,
    used_min = used_min, used_max = used_max,
    chosen_year = y,
    note = "OK"
  ))
}

if (nrow(plan) == 0) {
  stop(sprintf(
    "Plan is empty with the strict matcher.\n- Runs detected: %d\n- Requested window: [%s, %s]\n- Check exact filenames 'Growth_less_harvXX.tif' and window overlap.",
    nrow(common_runs),
    ifelse(is.null(harv_first), "auto", harv_first),
    ifelse(is.null(harv_last),  "auto", harv_last)
  ))
}

readr::write_csv(plan, file.path(paste0(output_dir,"/harvest"), "plan_runs_and_files.csv"))
readr::write_csv(growth_window_log, file.path(paste0(output_dir,"/harvest"), "growth_window_log.csv"))

cat("Plan rows: ", nrow(plan), " (each = one run using last map within window)\n", sep = "")



## ======================== 3) Terra options (progress/temp) ========================
terraOptions(progress = 1)
terraOptions(tempdir = rTempdir)

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
    writeRaster(bau_agb, file.path(paste0(output_dir,"/harvest"), "bau_agb_mc1.tif"), overwrite = TRUE)
    writeRaster(ics_agb, file.path(paste0(output_dir,"/harvest"), "ics_agb_mc1.tif"), overwrite = TRUE)
    writeRaster(delta_agb, file.path(paste0(output_dir,"/harvest"), "delta_agb_mc1.tif"), overwrite = TRUE)
    writeRaster(delta_co2, file.path(paste0(output_dir,"/harvest"), "delta_co2_mc1.tif"), overwrite = TRUE)
    
    # keep only cells < 0 (others -> NA)
    delta_agb_neg <- terra::ifel(delta_agb < 0, delta_agb, NA)
    
    # save
    writeRaster(delta_agb_neg,
                file.path(paste0(output_dir,"/harvest"), "delta_agb_mc1_neg.tif"),
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
  out_delta <- file.path(paste0(output_dir,"/harvest"), sprintf("delta_co2_run%03d_y%02d.tif", rid, y))
  writeRaster(delta_co2, out_delta, overwrite = TRUE)
  
  # Accumulate for mean/SE
  if (is.null(S)) {
    S  <- writeRaster(delta_co2,                    file.path(paste0(output_dir,"/harvest"), "S_delta_co2.tif"),  overwrite = TRUE)
    S2 <- writeRaster(delta_co2 * delta_co2,        file.path(paste0(output_dir,"/harvest"), "S2_delta_co2.tif"), overwrite = TRUE)
  } else {
    S  <- writeRaster(S  + delta_co2,               file.path(paste0(output_dir,"/harvest"), "S_delta_co2.tif"),  overwrite = TRUE)
    S2 <- writeRaster(S2 + (delta_co2 * delta_co2), file.path(paste0(output_dir,"/harvest"), "S2_delta_co2.tif"), overwrite = TRUE)
  }
  n_stream <- n_stream + 1L
}

# Per-run and summary tables
write_csv(per_run, file.path(paste0(output_dir,"/harvest"), "per_run_sumco2.csv"))

summary_tbl <- per_run |>
  summarise(
    runs           = n(),
    year_used_max  = max(year_code, na.rm = TRUE),
    sumco2_mean_Mg = mean(sumco2_Mg, na.rm = TRUE),
    sumco2_sd_Mg   = sd(sumco2_Mg,   na.rm = TRUE),
    sumco2_se_Mg   = sumco2_sd_Mg / sqrt(runs)
  )
write_csv(summary_tbl, file.path(paste0(output_dir,"/harvest"), "summary_sumco2.csv"))

# Mean (always, if at least 30 run) and SE (if at least 30 runs)
if (!is.null(S) && n_stream >= 30) {
  mean_r <- S / n_stream
  writeRaster(mean_r, file.path(paste0(output_dir,"/harvest"), "delta_co2_mean.tif"), overwrite = TRUE)
}

if (!is.null(S2) && n_stream >= 30) {
  var_r <- (S2 - (S * S) / n_stream) / (n_stream - 1)  # unbiased variance
  se_r  <- sqrt(var_r) / sqrt(n_stream)
  writeRaster(se_r, file.path(paste0(output_dir,"/harvest"), "delta_co2_se.tif"), overwrite = TRUE)
}

cat("Done. Files in:", paste0(output_dir,"/harvest"), "\n")


## ======================== 5) Write summaries ========================
write_csv(per_run, file.path(paste0(output_dir,"/harvest"), "per_run_sumco2.csv"))

summary_tbl <- per_run |>
  summarise(
    runs           = n(),
    year_used_max  = max(year_code, na.rm = TRUE),
    sumco2_mean_Mg = mean(sumco2_Mg, na.rm = TRUE),
    sumco2_sd_Mg   = sd(sumco2_Mg,   na.rm = TRUE),
    sumco2_se_Mg   = sumco2_sd_Mg / sqrt(runs)
  )
write_csv(summary_tbl, file.path(paste0(output_dir,"/harvest"), "summary_sumco2.csv"))

# Sum harvest to comare 2 demand from the next section ----
message("\n[End-Use] Summing all Harvest_totXX.tif per run (BAU & ICS)…")

# Helpers -------------------------------------------------------------
.sum_harvest_in_run <- function(run_dir, out_prefix, outdir, mc1_tag = FALSE,
                                first = NULL, last = NULL) {
  files <- fs::dir_ls(run_dir, type = "file", glob = "*.tif")
  ptn   <- "(?i)^Harvest_tot(\\d+)\\.tif$"
  
  files <- files[grepl(ptn, basename(files))]
  files <- .filter_harv_range(files, "Harvest_tot(\\d+)\\.tif$", first, last)
  
  if (!length(files)) {
    return(list(ok = FALSE, msg = paste0("No Harvest_totXX.tif in range in: ", run_dir)))
  }
  
  # order by XX for neat logs
  ord <- as.integer(stringr::str_match(basename(files), stringr::regex("Harvest_tot(\\d+)\\.tif$", ignore_case = TRUE))[,2])
  o   <- order(ord)
  files <- files[o]
  ord   <- ord[o]
  
  r0 <- terra::rast(files[1])
  for (f in files[-1]) {
    if (!terra::compareGeom(r0, terra::rast(f), stopOnError = FALSE)) {
      stop("[End-Use] Geometry mismatch within run folder: ", run_dir,
           "\nProblematic file: ", f)
    }
  }
  
  rlist <- lapply(files, terra::rast)
  rs    <- terra::rast(rlist)
  rsum  <- terra::app(rs, fun = sum, na.rm = TRUE)
  
  out_tif <- file.path(outdir, paste0(out_prefix, "_harvest_sum.tif"))
  terra::writeRaster(rsum, out_tif, overwrite = TRUE)
  
  gsum <- as.numeric(terra::global(rsum, "sum", na.rm = TRUE)[[1]])
  
  # sources & log
  src_txt <- file.path(outdir, paste0(out_prefix, "_harvest_sources.txt"))
  readr::write_lines(basename(files), src_txt)
  
  if (mc1_tag) {
    # convenience copy + explicit sources list for MC1
    mc1_tif <- file.path(outdir, paste0(out_prefix, "_harvest_sum_mc1.tif"))
    # terra::writeRaster(rsum, mc1_tif, overwrite = TRUE)
    readr::write_lines(basename(files), file.path(outdir, paste0(out_prefix, "_harvest_sources_mc1.txt")))
  }
  
  list(
    ok = TRUE,
    out_tif = out_tif,
    global_sum = gsum,
    n_layers = length(files),
    first_used = min(ord),
    last_used  = max(ord),
    sources = basename(files)
  )
}


# Output subfolders ---------------------------------------------------
enduse_dir_bau <- file.path(paste0(output_dir,"/harvest"), "harvesttot_bau")
enduse_dir_ics <- file.path(paste0(output_dir,"/harvest"), "harvesttot_ics")
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
    run_dir   = dir_b,
    out_prefix= out_prefix_b,
    outdir    = enduse_dir_bau,
    mc1_tag   = (rid == 1L),
    first     = harv_first,
    last      = harv_last
  )
  if (isTRUE(res_b$ok)) {
    harv_tbl_bau <- bind_rows(
      harv_tbl_bau,
      tibble(run_id = rid,
             n_layers = res_b$n_layers,
             first_used = res_b$first_used,
             last_used  = res_b$last_used,
             global_sum_Mg = res_b$global_sum,
             out_tif = res_b$out_tif)
    )
  } else {
    message("[End-Use][BAU] ", res_b$msg)
  }
  
  # ICS
  out_prefix_i <- sprintf("ics_run%03d", rid)
  res_i <- .sum_harvest_in_run(
    run_dir   = dir_i,
    out_prefix= out_prefix_i,
    outdir    = enduse_dir_ics,
    mc1_tag   = (rid == 1L),
    first     = harv_first,
    last      = harv_last
  )
  if (isTRUE(res_i$ok)) {
    if (isTRUE(res_i$ok)) {
      harv_tbl_ics <- bind_rows(
        harv_tbl_ics,
        tibble(run_id = rid,
               n_layers = res_i$n_layers,
               first_used = res_i$first_used,
               last_used  = res_i$last_used,
               global_sum_Mg = res_i$global_sum,
               out_tif = res_i$out_tif)
      )
  } else {
    message("[End-Use][ICS] ", res_i$msg)
  }
}

# Write per-scenario summaries ---------------------------------------
readr::write_csv(harv_tbl_bau, file.path(paste0(output_dir,"/harvest"), "bau_harvest_per_run.csv"))
readr::write_csv(harv_tbl_ics, file.path(paste0(output_dir,"/harvest"), "ics_harvest_per_run.csv"))

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

readr::write_csv(enduse_summary, file.path(paste0(output_dir,"/harvest"), "harvest_summary.csv"))

message("[End-Use] Done. Rasters in:\n  - ", enduse_dir_bau, "\n  - ", enduse_dir_ics,
        "\nTables:\n  - ", file.path(paste0(output_dir,"/harvest"), "bau_harvest_per_run.csv"),
        "\n  - ", file.path(paste0(output_dir,"/harvest"), "ics_harvest_per_run.csv"),
        "\n  - ", file.path(paste0(output_dir,"/harvest"), "harvest_summary.csv"))

# ======================== EMISSIONS FROM END-USE ========================

# Compare Harvest vs Demand to extract % of unmet demand
# Actual harvest
# harvesttot_bau/bau_run001_harvest_sum.tif
# harvesttot_ics/ics_run001_harvest_sum.tif


suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(stringr)
  library(readr)
})

# ========================= 0) CONFIG =========================
# gid0       <- "ZMB"
# efchratio  <- 6
out_dir    <- file.path(output_dir, "enduse")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

bau_demand_dir <- file.path(bau_dir, "LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_out")
ics_demand_dir <- file.path(ics_dir, "LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_out")

# ========================= 1) EF TABLE ======================
efdb_path <- file.path(bau_dir, "LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/efdb_all NEW.csv")

efdb <- read_csv(efdb_path, show_col_types = FALSE) %>%
  mutate(fuel_std = str_to_title(trimws(fueltype))) %>%
  filter(GID_0 == gid0) %>%
  select(fuel_std, CO2, CH4, N2O)

# Map from group key -> EF table "fuel_std"
ef_name_map <- c(
  fuelwood = "Biomass",
  charcoal = "Charcoal",
  gas      = "Gas",
  kerosene = "Kerosene",
  electric = "Electricity",
  pellets  = "Pellets",
  ethanol  = "Ethanol",
  biogas   = "Biogas",
  coal     = "Coal",
  other    = "Other"
)

# ========================= 2) FUEL GROUPS ===================
# Merge imp_* with base for fuelwood/charcoal; others are single tags
# Example: switch between definitions depending on impchfw
if (impchfw == 1) {
  groups <- list(
    fuelwood = c("fuelwood", "imp_fuelwood"),
    charcoal = c("charcoal", "imp_charcoal"),
    gas      = "gas",
    kerosene = "kerosene",
    electric = "electric",
    pellets  = "pellets",
    ethanol  = "ethanol",
    biogas   = "biogas",
    coal     = "coal",
    other    = "other"
  )
} else {
  groups <- list(
    fuelwood = "fuelwood",
    charcoal = "charcoal",
    gas      = "gas",
    kerosene = "kerosene",
    electric = "electric",
    pellets  = "pellets",
    ethanol  = "ethanol",
    biogas   = "biogas",
    coal     = "coal",
    other    = "other"
  )
}

# ========================= 3) TEMPLATE ======================
# Pick a template grid from first demand file found (BAU first, then ICS)
cand <- list.files(bau_demand_dir, "^WorldPop_.*_\\d{4}_demand\\.tif$", full.names = TRUE)
if (!length(cand)) cand <- list.files(ics_demand_dir, "^WorldPop_.*_\\d{4}_demand\\.tif$", full.names = TRUE)
if (!length(cand)) stop("No demand files found in BAU/ICS folders.")
template <- rast(cand[1])
cat("Template set from:", cand[1], "\n")

# ========================= 4) MAIN LOOP =====================
for (g in names(groups)) {
  cat("\n=== Group:", g, "===\n")
  # g = "charcoal"
  # 4.1) Get EF (CH4 + N2O) for this group
  ef_key <- ef_name_map[[g]]
  row_g  <- efdb %>% filter(fuel_std == ef_key)
  if (nrow(row_g) == 0) {
    warning(sprintf("Missing EF (CH4+N2O) for '%s' in %s; skipping.", g, gid0))
    next
  }
  ef_val <- as.numeric(row_g$CH4 + row_g$N2O)
  cat("EF(CH4+N2O):", ef_val, "for", ef_key, "\n")

  # 4.2) Collect all BAU/ICS files for this group's tags
  tags <- groups[[g]]
  pat  <- paste0("^WorldPop_(", paste(tags, collapse = "|"), ")_\\d{4}_demand\\.tif$")

  bau_files <- list.files(bau_demand_dir, pattern = pat, full.names = TRUE)
  ics_files <- list.files(ics_demand_dir, pattern = pat, full.names = TRUE)

  if (!length(bau_files) || !length(ics_files)) {
    warning(sprintf("No files for '%s' in one of the scenarios; skipping.", g))
    next
  }
  cat("BAU files:", length(bau_files), " | ICS files:", length(ics_files), "\n")

  # 4.3) Extract available years in each scenario and intersect
  get_years <- function(v) as.integer(str_match(basename(v), "_(\\d{4})_demand\\.tif$")[,2])
  years_bau <- sort(unique(get_years(bau_files)))
  years_ics <- sort(unique(get_years(ics_files)))
  years     <- intersect(years_bau, years_ics)

  if (!length(years)) {
    warning(sprintf("No common years for '%s'; skipping.", g))
    next
  }
  cat("Common years:", paste(years, collapse = ", "), "\n")
  
  # 4.4) Sum BAU over tags per year, then sum across years (→ single layer)
  yearly_bau <- list()
  for (y in years) {
    files_y <- bau_files[grepl(paste0("_", y, "_demand\\.tif$"), bau_files)]
    if (!length(files_y)) next
    
    # Read all tag rasters for this year
    rs <- lapply(files_y, terra::rast)
    
    # STREAMING sum across tags for this year (NA-safe)
    year_sum <- NULL
    for (r in rs) {
      year_sum <- if (is.null(year_sum)) r else terra::app(terra::c(year_sum, r), fun = sum, na.rm = TRUE)
    }
    
    yearly_bau[[as.character(y)]] <- year_sum
  }
  
  # STREAMING sum across years (your working pattern)
  yearly_bau <- Filter(Negate(is.null), yearly_bau)
  if (!length(yearly_bau)) {
    warning(sprintf("No yearly BAU rasters built for '%s'; skipping.", g))
    next
  }
  bau_sum <- NULL
  for (r in yearly_bau) {
    bau_sum <- if (is.null(bau_sum)) r else (bau_sum + r)   # switch to app+c(...) if you want NA-safe across years too
  }
  cat("BAU sum built. Layers:", terra::nlyr(bau_sum), "\n")

  # 4.5) Sum ICS over tags per year, then sum across years (→ single layer)
  yearly_ics <- list()
  for (y in years) {
    files_y <- ics_files[grepl(paste0("_", y, "_demand\\.tif$"), ics_files)]
    if (!length(files_y)) next
    
    rs <- lapply(files_y, terra::rast)
    
    # STREAMING sum across tags for this year (NA-safe)
    year_sum <- NULL
    for (r in rs) {
      year_sum <- if (is.null(year_sum)) r else terra::app(terra::c(year_sum, r), fun = sum, na.rm = TRUE)
    }
    
    yearly_ics[[as.character(y)]] <- year_sum
  }
  
  # STREAMING sum across years (your working pattern)
  yearly_ics <- Filter(Negate(is.null), yearly_ics)
  if (!length(yearly_ics)) {
    warning(sprintf("No yearly ICS rasters built for '%s'; skipping.", g))
    next
  }
  ics_sum <- NULL
  for (r in yearly_ics) {
    ics_sum <- if (is.null(ics_sum)) r else (ics_sum + r)   # switch to app+c(...) if you want NA-safe across years too
  }
  cat("ICS sum built. Layers:", terra::nlyr(ics_sum), "\n")
  
  
  # Ensure output dir exists
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # Save BAU/ICS sums for this fuel group `g`
  bau_sum_path <- file.path(out_dir, sprintf("bau_sum_%s.tif", g))
  ics_sum_path <- file.path(out_dir, sprintf("ics_sum_%s.tif", g))
  
  terra::writeRaster(bau_sum, bau_sum_path, filetype = "GTiff", overwrite = TRUE)
  terra::writeRaster(ics_sum, ics_sum_path, filetype = "GTiff", overwrite = TRUE)
  
  cat("Saved BAU sum → ", bau_sum_path, "\n", sep = "")
  cat("Saved ICS sum → ", ics_sum_path, "\n", sep = "")
  
  # --- Totals (sum over non-NA pixels) ---
  bau_total <- as.numeric(terra::global(bau_sum, "sum", na.rm = TRUE)[1,1])
  ics_total <- as.numeric(terra::global(ics_sum, "sum", na.rm = TRUE)[1,1])
  
  # Year range for this group (computed after you’ve set `years`)
  y_start <- min(years)
  y_end   <- max(years)
  
  cat(sprintf("Group %s [%d–%d] — BAU total (sum of non-NA pixels): %s\n",
              g, y_start, y_end, format(bau_total, big.mark = ",")))
  cat(sprintf("Group %s [%d–%d] — ICS total (sum of non-NA pixels): %s\n",
              g, y_start, y_end, format(ics_total, big.mark = ",")))
  
# 4.6) Convert demand to emissions (tCO2e) using CH4+N2O; charcoal ÷ efchratio
bau_emis <- bau_sum * ef_val
ics_emis <- ics_sum * ef_val
if (g == "charcoal") {
  bau_emis <- bau_emis / efchratio
  ics_emis <- ics_emis / efchratio
  cat("Applied charcoal ratio 1/", efchratio, "\n", sep = "")
}

delta <- bau_emis - ics_emis   # single layer


  # 4.7) Delta = BAU − ICS (single layer)
  delta <- bau_emis - ics_emis

  # 4.8) Sanity check for solid fuels: ICS should not exceed BAU per pixel
  if (g %in% c("fuelwood", "charcoal")) {
    neg_count <- as.integer(global(ifel(delta < 0, 1, 0), "sum", na.rm = TRUE)[1,1])
    neg_min   <- as.numeric(global(delta, "min", na.rm = TRUE)[1,1])
    if (neg_count > 0) {
      warning(sprintf(
        "Found %d negative pixels for '%s' (min=%.6g). Likely misalignment or mismatched years.",
        neg_count, g, neg_min
      ))
    } else {
      cat("No negative pixels detected for solid fuel group.\n")
    }
  }

  # 4.9) Write result
  out_file <- file.path(out_dir, sprintf("delta_co2_%senduse.tif", g))
  writeRaster(delta, out_file, filetype = "GTiff", overwrite = TRUE)
  cat("Wrote:", out_file, "\n")
}

cat("\n✓ Done. Outputs in: ", out_dir, "\n", sep = "")

# ======================== SUMMARY ========================
# --- Sanity check for saved demand maps 2010-2050 with no harvesting adjustment ----
bau_charcoal <- rast(paste0(output_dir,"/enduse/bau_sum_charcoal.tif"))
bau_charcoal_total <- as.numeric(terra::global(bau_charcoal, "sum", na.rm = TRUE)[1,1])
ics_charcoal <- rast(paste0(output_dir,"/enduse/ics_sum_charcoal.tif"))
ics_charcoal_total <- as.numeric(terra::global(ics_charcoal, "sum", na.rm = TRUE)[1,1])
bau_charcoal_total
ics_charcoal_total
# I get totals of 790875789 for BAU and 763771314 for Proj1/2 so yours are 3-4% lower.
# Charcoal demand 2010-2050


# ---- Packages ----
if (!"terra" %in% rownames(installed.packages())) install.packages("terra", quiet = TRUE)
library(terra)

# ---- Inputs ----
# Set this before running:
# output_dir <- "C:/path/to/your/output"

enduse_dir  <- file.path(output_dir, "enduse")
harvest_dir <- file.path(output_dir, "harvest")
summary_dir <- file.path(output_dir, "summary_mc1")
if (!dir.exists(summary_dir)) dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)

# ---- 1) Load all end-use rasters starting with "delta_co2" and sum (NA -> 0) ----
enduse_files <- list.files(
  enduse_dir,
  pattern = "^delta_co2.*\\.tif$",
  full.names = TRUE,
  ignore.case = TRUE
)
if (!length(enduse_files)) {
  stop(sprintf("No rasters found in '%s' starting with 'delta_co2'.", enduse_dir))
}

enduse_stack <- rast(enduse_files)
# Sum across layers; na.rm=TRUE means NA are ignored (equivalent to 0 contribution)
delta_co2_enduse <- app(enduse_stack, fun = sum, na.rm = TRUE)
writeRaster(delta_co2_enduse, paste0(summary_dir,"/delta_co2_enduse.tif"), overwrite = TRUE)

# ---- 2) Load harvest delta_co2_mc1.tif, project & snap to enduse grid ----
harvest_src <- file.path(harvest_dir, "delta_co2_mc1.tif")
if (!file.exists(harvest_src)) {
  stop(sprintf("Harvest raster not found: %s", harvest_src))
}

delta_co2_harvest_raw <- rast(harvest_src)

# Project to exactly match delta_co2_enduse grid (CRS, res, extent, alignment)
# Passing the template raster 'y' makes the result snap to its grid.
# Bilinear is typical for continuous data like CO2 deltas.
delta_co2_harvest <- project(delta_co2_harvest_raw, delta_co2_enduse, method = "bilinear")

# Save the projected/snap-aligned harvest copy
delta_co2_harvest_path <- file.path(summary_dir, "delta_co2_harvest.tif")
writeRaster(delta_co2_harvest, delta_co2_harvest_path, overwrite = TRUE)

# ---- 3) Sum enduse + harvest with NA-safe addition and save final ----
# Use app(c(...), sum, na.rm=TRUE) to treat NA as 0 across layers
delta_co2_total <- app(c(delta_co2_enduse, delta_co2_harvest), fun = sum, na.rm = TRUE)

delta_co2_total_path <- file.path(summary_dir, "delta_co2.tif")
writeRaster(delta_co2_total, delta_co2_total_path, overwrite = TRUE)

cat("[OK]\n - Wrote projected harvest:", delta_co2_harvest_path,
    "\n - Wrote total delta:", delta_co2_total_path, "\n")
