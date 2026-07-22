#!/usr/bin/env Rscript
# ==============================================================================
# mechanics_verifications.R   (v2 - dual growth model)
# Verify MoFuSS per-pixel AGB dynamics. Auto-detects the growth mechanic:
#   "chapman-richards" (*_g):  grow(B)=CR(invCR(B)+1), CR=A*(1-exp(-k*age))^m
#                              A,k,m from LULCC/TempRaster/{A_c,k_c,m_c}.tif
#   "logistic"        (*_ng):  grow(B)=B + r*B*(1-B/K), K=agb3_c (AGB at t=0)
#                              r='rmax' from the growth table keyed by LULCt1_c.tif
#                              -> growth-only (no harvest) is a FLAT line at K
#   both:  B_{t+1} = grow(B_t) - Harvest_{t+1}   (forest, TOF mask==0)
#          TOF (mask==1): constant residue, held = observed.
# USAGE: edit CONFIG, then  Rscript mechanics_verifications.R  [optional working_dir]
# ==============================================================================
suppressPackageStartupMessages({ library(terra); library(data.table); library(ggplot2) })

cfg <- list(
  working_dir   = "C:/Users/aghil/Documents/MoFuSS_localhost/webmofuss_nv3_tests_ng",
  mc            = 1,
  growth_model  = "auto",   # "auto" | "chapman-richards" | "logistic"
  growth_table  = "LULCC/TempTables/growth_parameters_v3_modis.csv",
  lulc_cat_rast = "LULCC/TempRaster/LULCt1_c.tif",
  key_col       = "Key*",
  r_col         = "rmax",
  n_pixels_plot = 14, seed = 42
)
args <- commandArgs(trailingOnly = TRUE); if (length(args) >= 1) cfg$working_dir <- args[[1]]

wd  <- cfg$working_dir
dbg <- file.path(wd, sprintf("debugging_%d", cfg$mc))
trs <- file.path(wd, "LULCC", "TempRaster")
outv<- file.path(wd, "Out", "pixel-wise mechanics verification")
dir.create(outv, recursive = TRUE, showWarnings = FALSE)

tof  <- rast(file.path(trs, "TOFvsFOR_mask1.tif")); agb3 <- rast(file.path(trs, "agb3_c.tif"))
area_ha <- prod(res(agb3)) / 10000
gl_f <- list.files(dbg, pattern = "^Growth_less_harv[0-9]{2}\\.tif$", full.names = TRUE)
h_f  <- list.files(dbg, pattern = "^Harvest_tot[0-9]{2}\\.tif$", full.names = TRUE)
gl_f <- gl_f[order(as.integer(sub(".*harv([0-9]{2})\\.tif$", "\\1", basename(gl_f))))]
h_f  <- h_f [order(as.integer(sub(".*tot([0-9]{2})\\.tif$",  "\\1", basename(h_f))))]
N <- length(gl_f); Y0 <- 2000; Y1 <- 1999 + N
message(sprintf("years %d-%d (%d steps) | pixel area %.4f ha", Y0, Y1, N, area_ha))

tv <- values(tof)[, 1]; a3 <- values(agb3)[, 1]
GL <- as.matrix(rast(gl_f)); H <- as.matrix(rast(h_f))

CR    <- function(a, A, k, m) A * (1 - exp(-k * a))^m
invCR <- function(B, A, k, m) { r <- pmin(pmax(B / A, 0), 1 - 1e-9); -log(1 - r^(1 / m)) / k }

have_cr <- all(file.exists(file.path(trs, c("A_c.tif", "k_c.tif", "m_c.tif"))))
Av <- kv <- mv <- rep(NA_real_, length(a3))
if (have_cr) { Av <- values(rast(file.path(trs, "A_c.tif")))[, 1]
               kv <- values(rast(file.path(trs, "k_c.tif")))[, 1]
               mv <- values(rast(file.path(trs, "m_c.tif")))[, 1] }

rpix <- rep(NA_real_, length(a3))
tbl_ok <- file.exists(file.path(wd, cfg$growth_table)) && file.exists(file.path(wd, cfg$lulc_cat_rast))
if (tbl_ok) {
  gtb  <- fread(file.path(wd, cfg$growth_table))
  rlut <- setNames(as.numeric(gtb[[cfg$r_col]]), as.integer(gtb[[cfg$key_col]]))
  catv <- as.integer(values(rast(file.path(wd, cfg$lulc_cat_rast)))[, 1])
  ok   <- !is.na(catv) & as.character(catv) %in% names(rlut)
  rpix[ok] <- rlut[as.character(catv[ok])]
}

grow_cr  <- function(B, idx) CR(invCR(B, Av[idx], kv[idx], mv[idx]) + 1, Av[idx], kv[idx], mv[idx])
grow_log <- function(B, idx) { K <- a3[idx]; B + rpix[idx] * B * (1 - B / K) }

base <- is.finite(a3) & a3 > 0 & is.finite(tv)
for (t in seq_len(N)) base <- base & is.finite(GL[, t])
forest <- base & tv == 0
cr_param  <- have_cr & is.finite(Av) & is.finite(kv) & is.finite(mv) & Av > 0 & kv > 0 & mv > 0
log_param <- is.finite(rpix)

onestep_median <- function(grow_fun, vidx) {
  if (!length(vidx)) return(Inf)
  prev <- a3[vidx]; e <- numeric(N)
  for (t in seq_len(N)) { pred <- grow_fun(prev, vidx) - H[vidx, t]; e[t] <- median(abs(pred - GL[vidx, t])); prev <- GL[vidx, t] }
  median(e)
}

cand <- list()
if (have_cr) cand[["chapman-richards"]] <- list(grow = grow_cr,  vidx = which(forest & cr_param))
if (tbl_ok)  cand[["logistic"]]         <- list(grow = grow_log, vidx = which(forest & log_param))
if (cfg$growth_model != "auto") cand <- cand[cfg$growth_model]
stopifnot(length(cand) >= 1)
errs <- sapply(names(cand), function(nm) onestep_median(cand[[nm]]$grow, cand[[nm]]$vidx))
mech <- names(cand)[which.min(errs)]
grow <- cand[[mech]]$grow; vi <- cand[[mech]]$vidx
message("mechanic one-step median |err|: ", paste(sprintf("%s=%.4g", names(errs), errs), collapse = " | "), "  -> ", mech)

a3i <- a3[vi]; GLi <- GL[vi, , drop = FALSE]; Hi <- H[vi, , drop = FALSE]
onestep <- numeric(N); prev <- a3i; recon <- a3i
for (t in seq_len(N)) {
  onestep[t] <- median(abs((grow(prev, vi) - Hi[, t]) - GLi[, t]))
  recon <- grow(recon, vi) - Hi[, t]; prev <- GLi[, t]
}
cer <- abs(recon - GLi[, N])
tofm <- base & tv == 1
tof_sd_med <- if (any(tofm)) median(apply(GL[tofm, , drop = FALSE], 1, sd)) else NA_real_

rep <- data.table(metric = c("growth_model", "valid_forest_px", "tof_px",
                             "onestep_median_abs_err", "onestep_worst_year",
                             "forward_compounded_median_err", "forward_frac_within_1Mg",
                             "tof_median_sd_over_years"),
                  value = c(mech, length(vi), sum(tofm),
                            signif(median(onestep), 4), signif(max(onestep), 4),
                            signif(median(cer), 4), round(mean(cer < 1), 4), signif(tof_sd_med, 4)))
fwrite(rep, file.path(outv, sprintf("validation_report_%d_%d.csv", Y0, Y1)))
cat("\n============ MoFuSS mechanics validation ============\n"); print(rep, row.names = FALSE)
pass <- median(onestep) < 0.01 && (is.na(tof_sd_med) || tof_sd_med < 1e-6)
cat(sprintf("\nVERDICT: %s | mechanic=%s | one-step median err=%.2g | TOF sd=%.2g\n",
            if (pass) "PASS - reproduces intended mechanics" else "CHECK - discrepancies exceed tolerance",
            mech, median(onestep), tof_sd_med))

## stratified pixel-trajectory figure
set.seed(cfg$seed)
agb0_ha <- a3i / area_ha; hcum <- rowSums(Hi) / area_ha
pick <- function(mask, n) { w <- which(mask); if (!length(w)) return(integer(0)); sample(w, min(n, length(w))) }
selF <- c(pick(agb0_ha >= 2 & agb0_ha < 10, 3), pick(agb0_ha >= 10 & agb0_ha < 40, 3),
          pick(agb0_ha >= 80, 3), pick(hcum > quantile(hcum, 0.98), 3))
tof_pos <- which(tofm); selT <- if (length(tof_pos)) sample(tof_pos, min(2, length(tof_pos))) else integer(0)
ncw <- ncol(agb3)
grow1 <- function(B, cell) if (mech == "logistic") { K <- a3[cell]; B + rpix[cell] * B * (1 - B / K) } else CR(invCR(B, Av[cell], kv[cell], mv[cell]) + 1, Av[cell], kv[cell], mv[cell])

traj <- rbindlist(lapply(seq_along(c(selF, selT)), function(j) {
  isF <- j <= length(selF); vpos <- if (isF) selF[j] else selT[j - length(selF)]
  cell <- if (isF) vi[vpos] else vpos
  row <- ((cell - 1L) %/% ncw) + 1L; col <- ((cell - 1L) %% ncw) + 1L
  obs <- GL[cell, ] / area_ha; harv <- H[cell, ] / area_ha
  B <- a3[cell]; g <- a3[cell]; rec <- numeric(N); gro <- numeric(N)
  for (t in seq_len(N)) { if (isF) { B <- grow1(B, cell) - H[cell, t]; g <- grow1(g, cell) } else { B <- GL[cell, t]; g <- NA_real_ }; rec[t] <- B / area_ha; gro[t] <- g / area_ha }
  ktxt <- if (isF && mech == "logistic") sprintf("r=%.3f K=AGB0=%.1f", rpix[cell], a3[cell] / area_ha) else if (isF) sprintf("A=%.0f k=%.3f m=%.2f", Av[cell] / area_ha, kv[cell], mv[cell]) else "TOF residue"
  data.table(panel = sprintf("px(%d,%d) TOF=%d | %s\nAGB0=%.1f  max|err|=%.2g", row, col, as.integer(!isF), ktxt, obs[1], max(abs(rec - obs))),
             year = Y0:(Y0 + N - 1L), observed = obs, reconstructed = rec, growth_only = gro, harvest = harv)
}))
traj[, panel := factor(panel, levels = unique(panel))]
lines_long <- melt(traj, id.vars = c("panel", "year"), measure.vars = c("observed", "reconstructed", "growth_only"), variable.name = "series", value.name = "AGB")
lines_long[, series := factor(series, levels = c("observed", "reconstructed", "growth_only"), labels = c("observed", "reconstructed", "growth-only (no harvest)"))]
scol <- c("observed" = "#1f77b4", "reconstructed" = "#d62728", "growth-only (no harvest)" = "#2e7d32")
slty <- c("observed" = "solid",   "reconstructed" = "dashed", "growth-only (no harvest)" = "dotted")
pv <- ggplot() +
  geom_col(data = traj, aes(x = year, y = harvest, fill = "harvest"), alpha = 0.5, width = 0.6) +
  geom_line(data = lines_long, aes(x = year, y = AGB, colour = series, linetype = series), linewidth = 0.7, na.rm = TRUE) +
  geom_point(data = traj, aes(x = year, y = observed), colour = "#1f77b4", size = 0.8) +
  facet_wrap(~panel, scales = "free_y", ncol = 5) +
  scale_colour_manual(name = NULL, values = scol) + scale_linetype_manual(name = NULL, values = slty) +
  scale_fill_manual(name = NULL, values = c("harvest" = "#e07b39")) +
  labs(x = "year", y = "AGB (Mg/ha)",
       title = sprintf("MoFuSS pixel-trajectory validation - %s MC%d %d-%d  (mechanic: %s)", basename(wd), cfg$mc, Y0, Y1, mech),
       subtitle = "observed (blue) vs reconstructed grow-then-harvest (red dashed) ; growth-only no-harvest (green dotted) ; harvest (orange)") +
  theme_bw(base_size = 8) + theme(legend.position = "top", strip.background = element_blank(), strip.text = element_text(size = 6.5), plot.title = element_text(face = "bold"))
ggsave(file.path(outv, "pixel_trajectories_validation.png"), pv, width = 19, height = 2.7 * ceiling(nrow(traj) / N / 5), dpi = 140, limitsize = FALSE)
message(sprintf("DONE. Report + figure in %s | detected mechanic: %s", outv, mech))
