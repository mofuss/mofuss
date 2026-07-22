#!/usr/bin/env Rscript
# ==============================================================================
# growth_loss_gains.R
# ------------------------------------------------------------------------------
# Per-admin-unit AGB loss & gain curves from a MoFuSS working folder.
#
# For every consecutive-year pair in the annual AGB stack (Growth_less_harv),
# computes per pixel  dAGB = AGB(t+1) - AGB(t)  with x = AGB(t), pools ALL pairs
# into one sample per admin unit, and maps:
#     loss curve  H(B) = binned median of |dAGB| where dAGB < 0   (Mg/ha/yr)
#     gain curve  G(B) = binned median of  dAGB  where dAGB > 0   (Mg/ha/yr)
# vs AGB (Mg/ha). Detects the G=H crossing B*, classifies each unit, and writes
# a composite figure, per-unit figures, and CSVs.
#
# Pixel size is handled: AGB and rates are divided by pixel area in hectares.
#
# USAGE:
#   Edit the CONFIG block below, then:   Rscript growth_loss_gains.R
#   Or pass a working folder as an argument: Rscript growth_loss_gains.R "D:/path/to/cty_folder"
# ==============================================================================

suppressPackageStartupMessages({
  library(terra); library(sf); library(data.table); library(ggplot2)
})

## ----------------------------- CONFIG -----------------------------
cfg <- list(
  working_dir = "C:/Users/aghil/Documents/MoFuSS_localhost/webmofuss_nv3_tests_ng",
  mc          = 1,        # Monte Carlo realization -> debugging_<mc>
  bin_method  = "quantile", # "quantile" (adaptive; resolves low-AGB gains) or "fixed"
  bin_width   = 5,        # AGB bin width (Mg/ha), used only when bin_method = "fixed"
  max_bins    = 20,       # max quantile bins when bin_method = "quantile"
  gmin        = 30,       # min events in a bin to draw a loss/gain point
  admin_field = "NAME_1", # admin-unit name field in userarea1.gpkg
  id_field    = "ID"      # integer id field in userarea1.gpkg
)
## ------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) cfg$working_dir <- args[[1]]

GREEN <- "#2e7d32"; ORANGE <- "#e07b39"

wd     <- cfg$working_dir
dbg    <- file.path(wd, sprintf("debugging_%d", cfg$mc))
gpkg   <- file.path(wd, "LULCC", "TempVector", "userarea1.gpkg")
outdir <- file.path(wd, "Out", "agb_g_h_curves")
indiv  <- file.path(outdir, "individual")
dir.create(indiv, recursive = TRUE, showWarnings = FALSE)

## --- locate the annual AGB stack and infer the year range ---
glh_files <- list.files(dbg, pattern = "^Growth_less_harv[0-9]{2}\\.tif$", full.names = TRUE)
stopifnot(length(glh_files) >= 2)
idx <- as.integer(sub(".*Growth_less_harv([0-9]{2})\\.tif$", "\\1", basename(glh_files)))
o <- order(idx); glh_files <- glh_files[o]; idx <- idx[o]
Y0 <- 1999 + min(idx); Y1 <- 1999 + max(idx); npairs <- length(idx) - 1L
message(sprintf("Growth_less_harv: %d layers | years %d-%d | %d annual pairs",
                length(idx), Y0, Y1, npairs))

## --- reference grid, pixel area, country name ---
r0      <- rast(glh_files[1])
area_ha <- prod(res(r0)) / 10000
av      <- vect(gpkg)
country <- as.character(av[["NAME_0"]][1, 1])
message(sprintf("country: %s | admin units: %d | pixel area: %.4f ha",
                country, nrow(av), area_ha))

## --- rasterize admin units by integer ID onto the grid ---
zones   <- rasterize(av, r0, field = cfg$id_field)
zid     <- as.integer(round(values(zones)[, 1]))
names_lu <- setNames(as.character(av[[cfg$admin_field]][, 1]),
                     as.integer(round(av[[cfg$id_field]][, 1])))

## --- pool all consecutive-year pairs: agb(t) [Mg/ha], rate [Mg/ha/yr], uid ---
M <- as.matrix(rast(glh_files))   # ncell x nlyr, Mg per pixel
pair_dt <- function(t) {
  a <- M[, t]; b <- M[, t + 1L]
  ok <- is.finite(a) & is.finite(b) & !is.na(zid) & zid > 0
  data.table(agb = a[ok] / area_ha, rate = (b[ok] - a[ok]) / area_ha, uid = zid[ok])
}
DT <- rbindlist(lapply(seq_len(ncol(M) - 1L), pair_dt))
rm(M); gc()
message(sprintf("pooled obs=%d | frac_gain=%.3f frac_loss=%.3f",
                nrow(DT), mean(DT$rate > 0), mean(DT$rate < 0)))

## --- AGB bins computed PER UNIT. Quantile bins (default) adapt resolution to data
##     density, so the low-AGB zone (where gains and the loss peak concentrate) is
##     resolved instead of being lumped into one wide fixed bin (which hid the gains).
make_curve <- function(sub) {
  if (identical(cfg$bin_method, "fixed")) {
    p99 <- as.numeric(quantile(sub$agb, 0.99, names = FALSE))
    edges <- seq(0, p99, by = cfg$bin_width); if (tail(edges, 1) < p99) edges <- c(edges, p99)
  } else {
    nb <- max(8L, min(cfg$max_bins, as.integer(floor(nrow(sub) / 2000))))
    edges <- unique(as.numeric(quantile(sub$agb, seq(0, 1, length.out = nb + 1), names = FALSE)))
  }
  if (length(edges) < 3) return(NULL)
  b <- findInterval(sub$agb, edges, rightmost.closed = TRUE)
  sub <- copy(sub)[, bin := b]
  agg <- sub[, {
    los <- -rate[rate < 0]; gan <- rate[rate > 0]
    .(x = median(agb), n = .N, fg = mean(rate > 0),
      loss = if (length(los) >= cfg$gmin) median(los) else NA_real_,
      gain = if (length(gan) >= cfg$gmin) median(gan) else NA_real_)
  }, by = bin][n >= cfg$gmin][order(x)]
  agg
}

find_cross <- function(agg) {
  ok <- is.finite(agg$gain) & is.finite(agg$loss)
  x <- agg$x[ok]; dif <- (agg$gain - agg$loss)[ok]
  if (length(x) < 2) return(list(cls = "too-few", Bx = NA_real_))
  Bx <- NA_real_
  for (i in seq_len(length(x) - 1)) {
    if (dif[i] < 0 && dif[i + 1] > 0) { tt <- -dif[i] / (dif[i + 1] - dif[i]); Bx <- x[i] + tt * (x[i + 1] - x[i]); break }
    if (dif[i] > 0 && dif[i + 1] < 0) { tt <-  dif[i] / (dif[i] - dif[i + 1]); Bx <- x[i] + tt * (x[i + 1] - x[i]); break }
  }
  cls <- if (all(dif > 0)) "always-gain" else if (all(dif < 0)) "trap" else if (!is.na(Bx)) "crossing" else "mixed"
  list(cls = cls, Bx = Bx)
}

## --- build curves for national + each admin unit ---
units <- list()
add_unit <- function(key, name, sub, is_nat = FALSE) {
  if (nrow(sub) < cfg$gmin) { units[[key]] <<- list(name = name, n = nrow(sub), fg = NA_real_,
                                                    cur = NULL, cls = "too-few", Bx = NA_real_, is_nat = is_nat); return(invisible()) }
  cur <- make_curve(sub); cr <- find_cross(cur)
  units[[key]] <<- list(name = name, n = nrow(sub), fg = mean(sub$rate > 0),
                        cur = cur, cls = cr$cls, Bx = cr$Bx, is_nat = is_nat)
}
add_unit("NAT", sprintf("%s (national)", country), DT, is_nat = TRUE)
for (id in sort(unique(DT$uid))) add_unit(as.character(id), names_lu[[as.character(id)]], DT[uid == id])

## --- CSV outputs ---
summ <- rbindlist(lapply(names(units), function(k) {
  u <- units[[k]]; data.table(ukey = k, county = u$name, n_obs = u$n,
                              frac_gain = u$fg, class = u$cls, B_cross_Mgha = u$Bx)
}))
fwrite(summ, file.path(outdir, sprintf("county_summary_%d_%d.csv", Y0, Y1)))
bins <- rbindlist(lapply(names(units), function(k) {
  u <- units[[k]]; if (is.null(u$cur)) return(NULL)
  cbind(county = u$name, u$cur[, .(agb_mgha = x, n, frac_gain = fg, loss_med = loss, gain_med = gain)])
}))
fwrite(bins, file.path(outdir, sprintf("county_bins_%d_%d.csv", Y0, Y1)))
message("class tally: ", paste(names(table(summ$class[summ$ukey != "NAT"])),
                               table(summ$class[summ$ukey != "NAT"]), sep = "=", collapse = "  "))

## --- assemble a long table for plotting, ordered by class then B* ---
pri <- c(crossing = 0, `always-gain` = 1, trap = 2, mixed = 3, `too-few` = 4)
ord_keys <- names(units)[names(units) != "NAT"]
ord_keys <- ord_keys[order(sapply(ord_keys, function(k) pri[[units[[k]]$cls]]),
                           sapply(ord_keys, function(k) ifelse(is.na(units[[k]]$Bx), 1e9, units[[k]]$Bx)),
                           sapply(ord_keys, function(k) units[[k]]$name))]
ord <- c("NAT", ord_keys)
lab <- sapply(ord, function(k) {
  u <- units[[k]]; sprintf("%s  [%s]", u$name, if (!is.na(u$Bx)) sprintf("B*=%.0f", u$Bx) else u$cls)
})
names(lab) <- ord
long <- rbindlist(lapply(ord, function(k) {
  u <- units[[k]]; if (is.null(u$cur)) return(NULL)
  data.table(unit = factor(lab[[k]], levels = lab), x = u$cur$x, loss = u$cur$loss, gain = u$cur$gain)
}))
vlines <- rbindlist(lapply(ord, function(k) {
  u <- units[[k]]; if (is.na(u$Bx)) return(NULL)
  data.table(unit = factor(lab[[k]], levels = lab), Bx = u$Bx)
}))

# ensure the crossing table always has a Bx column (empty when no unit crosses),
# otherwise geom_vline(aes(xintercept = Bx)) errors on a 0-column data.table
if (!("Bx" %in% names(vlines))) vlines <- data.table(unit = factor(character(0), levels = lab), Bx = numeric(0))

## --- composite small-multiples ---
p <- ggplot(long, aes(x = x)) +
  geom_ribbon(data = long[is.finite(gain) & is.finite(loss) & gain > loss],
              aes(ymin = loss, ymax = gain), fill = GREEN, alpha = 0.18) +
  geom_line(aes(y = loss), colour = ORANGE, linewidth = 0.5, na.rm = TRUE) +
  geom_line(aes(y = gain), colour = GREEN,  linewidth = 0.5, na.rm = TRUE) +
  geom_vline(data = vlines, aes(xintercept = Bx), colour = "red", linetype = "dashed", linewidth = 0.4) +
  facet_wrap(~unit, scales = "free", ncol = 7) +
  labs(x = "AGB in year t  (Mg/ha)", y = "rate  (Mg/ha/yr)",
       title = sprintf("%s - per-unit AGB loss (orange) & gain (green) vs AGB | MoFuSS MC%d | %d-%d, %d pairs pooled",
                       country, cfg$mc, Y0, Y1, npairs),
       subtitle = sprintf("green shade = gain>loss ; red dashed = G=H crossing B* ; national gain share %.1f%%",
                          units$NAT$fg * 100)) +
  theme_bw(base_size = 7) + theme(strip.text = element_text(size = 6))
ggsave(file.path(outdir, sprintf("composite_counties_loss_gain_%d_%d.png", Y0, Y1)),
       p, width = 18, height = 2.0 * ceiling(length(ord) / 7), dpi = 145, limitsize = FALSE)

## --- per-unit individual figures ---
for (k in ord) {
  u <- units[[k]]; if (is.null(u$cur)) next
  cur <- u$cur
  pp <- ggplot(cur, aes(x = x)) +
    geom_ribbon(data = cur[is.finite(gain) & is.finite(loss) & gain > loss],
                aes(ymin = loss, ymax = gain), fill = GREEN, alpha = 0.18) +
    geom_line(aes(y = loss), colour = ORANGE, linewidth = 0.9, na.rm = TRUE) +
    geom_point(aes(y = loss), colour = ORANGE, size = 1.4, na.rm = TRUE) +
    geom_line(aes(y = gain), colour = GREEN,  linewidth = 0.9, na.rm = TRUE) +
    geom_point(aes(y = gain), colour = GREEN,  size = 1.4, na.rm = TRUE) +
    {if (!is.na(u$Bx)) geom_vline(xintercept = u$Bx, colour = "red", linetype = "dashed")} +
    labs(x = "AGB in year t  (Mg/ha)", y = "rate  (Mg/ha/yr)",
         title = sprintf("%s - MoFuSS MC%d", u$name, cfg$mc),
         subtitle = sprintf("%d-%d (%d pairs) | n=%d | gains=%.0f%% | %s%s",
                            Y0, Y1, npairs, u$n, u$fg * 100, u$cls,
                            if (!is.na(u$Bx)) sprintf(" | B*=%.1f Mg/ha", u$Bx) else "")) +
    theme_bw()
  safe <- gsub("[^A-Za-z0-9]+", "_", u$name)
  ggsave(file.path(indiv, sprintf("%s_loss_gain_%d_%d.png", safe, Y0, Y1)), pp,
         width = 6.6, height = 4.7, dpi = 140)
}
message(sprintf("DONE. Outputs in %s", outdir))
