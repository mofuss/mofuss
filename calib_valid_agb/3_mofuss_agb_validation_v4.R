#!/usr/bin/env Rscript

# Summarize the structural--stochastic consistency of observed CTrees AGB
# change with capped and uncapped BaU MoFuSS configurations.  Inputs are the
# fixed-support aggregates created by 2_prepare_agb_validation_v2.R.

suppressPackageStartupMessages({
  if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required.", call. = FALSE)
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.", call. = FALSE)
})

script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args, value = TRUE)
  if (length(hit)) normalizePath(sub("^--file=", "", hit[[1L]]), winslash = "/", mustWork = FALSE) else NA_character_
}

parse_args <- function(x) {
  out <- list()
  for (a in x) {
    if (!startsWith(a, "--")) next
    z <- sub("^--", "", a)
    if (!grepl("=", z, fixed = TRUE)) out[[z]] <- TRUE else {
      p <- regexpr("=", z, fixed = TRUE)
      out[[substr(z, 1L, p - 1L)]] <- substr(z, p + 1L, nchar(z))
    }
  }
  out
}

as_flag <- function(x, default = FALSE) {
  if (is.null(x)) return(default)
  tolower(as.character(x)) %in% c("1", "true", "yes", "y")
}

as_int <- function(x, default) {
  if (is.null(x) || !nzchar(x)) return(as.integer(default))
  z <- suppressWarnings(as.integer(x))
  if (is.na(z)) stop("Expected an integer, received: ", x, call. = FALSE)
  z
}

normalize_slash <- function(x) gsub("\\\\", "/", x)

safe_quantile <- function(x, p) {
  if (!any(is.finite(x))) return(NA_real_)
  unname(stats::quantile(x[is.finite(x)], p, names = FALSE, type = 7))
}

group_summary <- function(x, groups, metrics) {
  if (!nrow(x)) return(data.frame())
  key_groups <- groups[!grepl("_name$", groups)]
  key <- interaction(x[key_groups], drop = TRUE, lex.order = TRUE)
  idx <- split(seq_len(nrow(x)), key)
  rows <- lapply(idx, function(ii) {
    z <- x[ii, , drop = FALSE]
    out <- z[1L, groups, drop = FALSE]
    out$n_mc <- length(unique(z$mc_id[is.finite(z$mc_id)]))
    for (m in metrics) {
      out[[paste0(m, "_median")]] <- stats::median(z[[m]], na.rm = TRUE)
      out[[paste0(m, "_q025")]] <- safe_quantile(z[[m]], 0.025)
      out[[paste0(m, "_q975")]] <- safe_quantile(z[[m]], 0.975)
    }
    out
  })
  do.call(rbind, rows)
}

prefix_metrics <- function(x, prefix, groups) {
  names(x)[!names(x) %in% groups] <- paste0(prefix, names(x)[!names(x) %in% groups])
  x
}

write_csv_gz <- function(x, path) {
  con <- gzfile(path, open = "wt")
  on.exit(close(con), add = TRUE)
  utils::write.csv(x, con, row.names = FALSE, na = "")
}

copy_directory <- function(from, to) {
  if (!dir.exists(from)) stop("Copy source does not exist: ", from, call. = FALSE)
  if (dir.exists(to)) stop("Refusing to overwrite existing directory: ", to, call. = FALSE)
  if (!dir.create(to, recursive = TRUE, showWarnings = FALSE)) stop("Could not create copy destination: ", to, call. = FALSE)
  rel <- list.files(from, recursive = TRUE, all.files = TRUE, no.. = TRUE, include.dirs = TRUE)
  info <- file.info(file.path(from, rel))
  dirs <- rel[!is.na(info$isdir) & info$isdir]
  if (length(dirs)) for (d in dirs) dir.create(file.path(to, d), recursive = TRUE, showWarnings = FALSE)
  files <- rel[!is.na(info$isdir) & !info$isdir]
  if (length(files)) {
    ok <- file.copy(file.path(from, files), file.path(to, files), overwrite = FALSE, copy.mode = TRUE, copy.date = TRUE)
    if (!all(ok)) stop("One or more files failed to copy into: ", to, call. = FALSE)
  }
  expected <- sum(file.info(file.path(from, files))$size)
  actual <- sum(file.info(file.path(to, files))$size)
  if (!identical(expected, actual)) stop("Copied directory failed byte-size verification: ", to, call. = FALSE)
  invisible(TRUE)
}

guarded_remove_directory <- function(target, expected_parent, expected_leaf) {
  target <- normalize_slash(normalizePath(target, winslash = "/", mustWork = FALSE))
  expected_parent <- normalize_slash(normalizePath(expected_parent, winslash = "/", mustWork = FALSE))
  if (!identical(basename(target), expected_leaf) ||
      !identical(tolower(dirname(target)), tolower(expected_parent)) ||
      identical(tolower(target), tolower(expected_parent)) ||
      identical(dirname(expected_parent), expected_parent)) {
    stop("Refusing unexpected cleanup target: ", target, call. = FALSE)
  }
  if (file.exists(target) && !dir.exists(target)) stop("Cleanup target is not a directory: ", target, call. = FALSE)
  if (!dir.exists(target)) return(invisible(FALSE))
  resolved_target <- normalize_slash(normalizePath(target, winslash = "/", mustWork = TRUE))
  resolved_parent <- normalize_slash(normalizePath(expected_parent, winslash = "/", mustWork = TRUE))
  if (!identical(tolower(dirname(resolved_target)), tolower(resolved_parent)) ||
      !identical(basename(resolved_target), expected_leaf)) {
    stop("Resolved cleanup target escaped its guarded parent: ", resolved_target, call. = FALSE)
  }
  status <- unlink(resolved_target, recursive = TRUE, force = TRUE)
  if (!identical(status, 0L) || file.exists(target)) stop("Failed to remove previous output: ", target, call. = FALSE)
  cat("Removed previous output: ", target, "\n", sep = "")
  invisible(TRUE)
}

sign_label <- function(x, tol = 1e-10) ifelse(x > tol, "gain", ifelse(x < -tol, "loss", "stable"))

wasserstein_equal_n <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  if (!length(x)) return(NA_real_)
  mean(abs(sort(x) - sort(y)))
}

outside_fields <- function(obs, lower, upper) {
  inside <- is.finite(obs) & is.finite(lower) & is.finite(upper) & obs >= lower & obs <= upper
  signed <- ifelse(!is.finite(obs + lower + upper), NA_real_, ifelse(obs < lower, obs - lower, ifelse(obs > upper, obs - upper, 0)))
  width <- upper - lower
  normalized <- ifelse(is.finite(width) & width > 0, signed / width, ifelse(signed == 0, 0, NA_real_))
  list(inside = inside, signed_distance = signed, normalized_distance = normalized, width = width)
}

opts <- parse_args(commandArgs(trailingOnly = TRUE))
prepared_dir <- normalize_slash(opts[["prepared-dir"]])
if (!nzchar(prepared_dir)) stop("--prepared-dir is required.", call. = FALSE)
prepared_path <- normalize_slash(file.path(prepared_dir, "agb_validation_prepared_v2.rds"))
if (!file.exists(prepared_path)) stop("Prepared RDS not found: ", prepared_path, call. = FALSE)
bootstrap_reps <- as_int(opts[["bootstrap-reps"]], 1000L)
bootstrap_seed <- as_int(opts[["bootstrap-seed"]], 42L)
staging_root <- if (is.null(opts[["staging-root"]])) NA_character_ else normalize_slash(opts[["staging-root"]])
final_validation_dir <- if (is.null(opts[["final-validation-dir"]])) dirname(prepared_dir) else normalize_slash(opts[["final-validation-dir"]])
dry_run <- as_flag(opts[["dry-run"]], FALSE)
overwrite <- as_flag(opts[["overwrite"]], FALSE)
if (bootstrap_reps < 100L) stop("Use at least 100 bootstrap replicates.", call. = FALSE)

prepared <- readRDS(prepared_path)
required_names <- c("design", "periods", "units", "block_lookup", "trajectory", "period_all_mc", "block_all_mc")
if (!all(required_names %in% names(prepared))) stop("Prepared RDS does not satisfy the v2 schema.", call. = FALSE)
if (!identical(prepared$design$schema_version, "agb_validation_preparation_v2")) stop("Unexpected prepared-data schema: ", prepared$design$schema_version, call. = FALSE)

# Keep locale-dependent display strings out of grouping keys and generated
# filenames/tables. Unit IDs remain the stable analytical identifiers.
display_names <- iconv(enc2utf8(prepared$units$unit_name), from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
display_names[is.na(display_names) | !nzchar(display_names)] <- prepared$units$unit_id[is.na(display_names) | !nzchar(display_names)]
prepared$units$unit_name <- display_names
unit_name_lookup <- setNames(display_names, prepared$units$unit_id)
for (nm in c("trajectory", "period_all_mc", "block_all_mc", "coverage", "block_lookup")) {
  if (!is.null(prepared[[nm]]) && all(c("unit_id", "unit_name") %in% names(prepared[[nm]]))) {
    prepared[[nm]]$unit_name <- unname(unit_name_lookup[prepared[[nm]]$unit_id])
  }
}

primary_pid <- prepared$design$primary_period
primary_label <- gsub("_", "-", primary_pid, fixed = TRUE)
block_raster_path <- list.files(prepared_dir, pattern = paste0("^primary_", primary_pid, "_50km_block_id\\.tif$"), full.names = TRUE)
if (length(block_raster_path) != 1L) stop("Expected one primary-period block-ID raster in ", prepared_dir, call. = FALSE)

cat("AGB CONSISTENCY ENVELOPE V4\n")
cat("Prepared input: ", prepared_path, "\n", sep = "")
cat("Primary period: ", primary_pid, "\n", sep = "")
cat("MC realizations: ", prepared$design$expected_mc, " per structural configuration\n", sep = "")
cat("Bootstrap replicates: ", bootstrap_reps, "; seed: ", bootstrap_seed, "\n", sep = "")
cat("Interpretation: empirical AGB-change consistency; not fNRB attribution.\n")

if (dry_run) {
  invisible(terra::rast(block_raster_path))
  cat("DRY RUN PASSED: prepared schema and primary block raster are readable.\n")
  quit(save = "no", status = 0L)
}

final_dir <- normalize_slash(file.path(final_validation_dir, "3_agb_consistency_envelope_v4"))
if (is.na(staging_root) || !nzchar(staging_root)) staging_root <- dirname(prepared_dir)
if (!dir.exists(staging_root)) dir.create(staging_root, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(staging_root)) stop("Could not create staging root: ", staging_root, call. = FALSE)
stage_dir <- normalize_slash(file.path(staging_root, paste0(".3_agb_consistency_envelope_v4_stage_", Sys.getpid())))
completed_stage_dir <- normalize_slash(file.path(staging_root, "3_agb_consistency_envelope_v4"))
if (overwrite) {
  guarded_remove_directory(final_dir, dirname(final_dir), basename(final_dir))
  guarded_remove_directory(completed_stage_dir, staging_root, basename(completed_stage_dir))
  stale <- list.dirs(staging_root, recursive = FALSE, full.names = TRUE)
  stale <- stale[grepl("^\\.3_agb_consistency_envelope_v4_stage_[0-9]+$", basename(stale))]
  if (length(stale)) for (path in stale) guarded_remove_directory(path, staging_root, basename(path))
}
if (dir.exists(final_dir)) stop("Refusing to overwrite existing versioned output: ", final_dir, call. = FALSE)
if (dir.exists(stage_dir)) stop("Unexpected staging directory already exists: ", stage_dir, call. = FALSE)
if (dir.exists(completed_stage_dir)) stop("Refusing to overwrite completed temporary product: ", completed_stage_dir, call. = FALSE)
dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(stage_dir)) stop("Could not create staging directory: ", stage_dir, call. = FALSE)

period_metrics <- c("start_agb_Mg", "end_agb_Mg", "delta_agb_Mg", "delta_percent", "gross_loss_Mg", "gross_gain_Mg")
period_model <- prepared$period_all_mc[prepared$period_all_mc$source == "mofuss", , drop = FALSE]
period_obs <- prepared$period_all_mc[prepared$period_all_mc$source == "ctrees", , drop = FALSE]
period_groups <- c("period_id", "unit_id", "unit_name", "unit_level", "config")
period_model_summary <- group_summary(period_model, period_groups, period_metrics)

join_groups <- c("period_id", "unit_id", "unit_name", "unit_level")
cap <- period_model_summary[period_model_summary$config == "capped", , drop = FALSE]
unc <- period_model_summary[period_model_summary$config == "uncapped", , drop = FALSE]
cap$config <- NULL
unc$config <- NULL
cap <- prefix_metrics(cap, "capped_", join_groups)
unc <- prefix_metrics(unc, "uncapped_", join_groups)
obs_keep <- period_obs[c(join_groups, period_metrics, "cell_count", "support_area_ha")]
names(obs_keep)[names(obs_keep) %in% period_metrics] <- paste0("observed_", names(obs_keep)[names(obs_keep) %in% period_metrics])
envelope <- Reduce(function(a, b) merge(a, b, by = join_groups, all = TRUE, sort = FALSE), list(obs_keep, cap, unc))
envelope <- merge(envelope, prepared$periods[c("period_id", "start_year", "end_year", "role", "is_primary")], by = "period_id", all.x = TRUE, sort = FALSE)

envelope$envelope_lower_Mg <- pmin(envelope$capped_delta_agb_Mg_q025, envelope$uncapped_delta_agb_Mg_q025, na.rm = TRUE)
envelope$envelope_upper_Mg <- pmax(envelope$capped_delta_agb_Mg_q975, envelope$uncapped_delta_agb_Mg_q975, na.rm = TRUE)
dist_Mg <- outside_fields(envelope$observed_delta_agb_Mg, envelope$envelope_lower_Mg, envelope$envelope_upper_Mg)
envelope$inside_consistency_interval <- dist_Mg$inside
envelope$signed_outside_distance_Mg <- dist_Mg$signed_distance
envelope$normalized_outside_distance <- dist_Mg$normalized_distance
envelope$interval_width_Mg <- dist_Mg$width
envelope$interval_width_percent_observed_baseline <- ifelse(envelope$observed_start_agb_Mg != 0, 100 * dist_Mg$width / envelope$observed_start_agb_Mg, NA_real_)
envelope$observed_minus_capped_median_Mg <- envelope$observed_delta_agb_Mg - envelope$capped_delta_agb_Mg_median
envelope$observed_minus_uncapped_median_Mg <- envelope$observed_delta_agb_Mg - envelope$uncapped_delta_agb_Mg_median
envelope$observed_direction <- sign_label(envelope$observed_delta_agb_Mg)
envelope$capped_direction <- sign_label(envelope$capped_delta_agb_Mg_median)
envelope$uncapped_direction <- sign_label(envelope$uncapped_delta_agb_Mg_median)

envelope$envelope_lower_percent <- pmin(envelope$capped_delta_percent_q025, envelope$uncapped_delta_percent_q025, na.rm = TRUE)
envelope$envelope_upper_percent <- pmax(envelope$capped_delta_percent_q975, envelope$uncapped_delta_percent_q975, na.rm = TRUE)
dist_pct <- outside_fields(envelope$observed_delta_percent, envelope$envelope_lower_percent, envelope$envelope_upper_percent)
envelope$signed_outside_distance_percent_points <- dist_pct$signed_distance

# Normalize each trajectory to its own AGB at the primary-period start.  This
# prevents cross-configuration baseline differences from masquerading as
# temporal agreement or disagreement.
trajectory <- prepared$trajectory
trajectory$agb_Mg <- trajectory$end_agb_Mg
traj_obs <- trajectory[trajectory$source == "ctrees", , drop = FALSE]
traj_model <- trajectory[trajectory$source == "mofuss", , drop = FALSE]

normalize_groups <- function(x, groups, baseline_year) {
  key <- interaction(x[groups], drop = TRUE, lex.order = TRUE)
  idx <- split(seq_len(nrow(x)), key)
  rows <- lapply(idx, function(ii) {
    z <- x[ii, , drop = FALSE]
    b <- z$agb_Mg[z$year == baseline_year]
    if (length(b) != 1L || !is.finite(b) || b == 0) stop("Missing or invalid trajectory baseline for a group.", call. = FALSE)
    z$relative_agb_change_percent <- 100 * (z$agb_Mg - b) / b
    z
  })
  do.call(rbind, rows)
}

primary_start <- prepared$periods$start_year[prepared$periods$is_primary][[1L]]
traj_obs <- normalize_groups(traj_obs, c("unit_id"), primary_start)
traj_model <- normalize_groups(traj_model, c("unit_id", "config", "mc_id"), primary_start)
traj_summary <- group_summary(traj_model, c("year", "unit_id", "unit_name", "unit_level", "config"), c("agb_Mg", "relative_agb_change_percent"))
trajectory_summary <- rbind(
  data.frame(source = "ctrees", config = "observed", n_mc = NA_integer_, traj_obs[c("year", "unit_id", "unit_name", "unit_level")],
             agb_Mg_median = traj_obs$agb_Mg, agb_Mg_q025 = traj_obs$agb_Mg, agb_Mg_q975 = traj_obs$agb_Mg,
             relative_agb_change_percent_median = traj_obs$relative_agb_change_percent,
             relative_agb_change_percent_q025 = traj_obs$relative_agb_change_percent,
             relative_agb_change_percent_q975 = traj_obs$relative_agb_change_percent, stringsAsFactors = FALSE),
  data.frame(source = "mofuss", traj_summary, stringsAsFactors = FALSE)
)

block_metrics <- c("delta_agb_Mg", "delta_Mg_ha", "delta_percent")
block_model <- prepared$block_all_mc[prepared$block_all_mc$source == "mofuss", , drop = FALSE]
block_obs <- prepared$block_all_mc[prepared$block_all_mc$source == "ctrees", , drop = FALSE]
block_groups <- c("period_id", "block_id", "unit_id", "unit_name", "config")
block_model_summary <- group_summary(block_model, block_groups, block_metrics)
block_join <- c("period_id", "block_id", "unit_id", "unit_name")
bcap <- block_model_summary[block_model_summary$config == "capped", , drop = FALSE]
bunc <- block_model_summary[block_model_summary$config == "uncapped", , drop = FALSE]
bcap$config <- NULL
bunc$config <- NULL
bcap <- prefix_metrics(bcap, "capped_", block_join)
bunc <- prefix_metrics(bunc, "uncapped_", block_join)
bobs <- block_obs[c(block_join, "cell_count", "support_area_ha", block_metrics)]
names(bobs)[names(bobs) %in% block_metrics] <- paste0("observed_", names(bobs)[names(bobs) %in% block_metrics])
block_envelope <- Reduce(function(a, b) merge(a, b, by = block_join, all = TRUE, sort = FALSE), list(bobs, bcap, bunc))
block_envelope <- merge(block_envelope, prepared$block_lookup, by = c("block_id", "unit_id", "unit_name"), all.x = TRUE, sort = FALSE)
block_envelope <- merge(block_envelope, prepared$periods[c("period_id", "role", "is_primary")], by = "period_id", all.x = TRUE, sort = FALSE)
block_envelope$envelope_lower_Mg_ha <- pmin(block_envelope$capped_delta_Mg_ha_q025, block_envelope$uncapped_delta_Mg_ha_q025, na.rm = TRUE)
block_envelope$envelope_upper_Mg_ha <- pmax(block_envelope$capped_delta_Mg_ha_q975, block_envelope$uncapped_delta_Mg_ha_q975, na.rm = TRUE)
bdist <- outside_fields(block_envelope$observed_delta_Mg_ha, block_envelope$envelope_lower_Mg_ha, block_envelope$envelope_upper_Mg_ha)
block_envelope$inside_consistency_interval <- bdist$inside
block_envelope$signed_outside_distance_Mg_ha <- bdist$signed_distance
block_envelope$normalized_outside_distance <- bdist$normalized_distance
block_envelope$interval_width_Mg_ha <- bdist$width
block_envelope$observed_direction <- sign_label(block_envelope$observed_delta_Mg_ha)
block_envelope$capped_direction <- sign_label(block_envelope$capped_delta_Mg_ha_median)
block_envelope$uncapped_direction <- sign_label(block_envelope$uncapped_delta_Mg_ha_median)

diagnostic_one <- function(z, unit_id, unit_name, reference, pred) {
  obs <- z$observed_delta_Mg_ha
  ok <- is.finite(obs) & is.finite(pred)
  obs <- obs[ok]
  pred <- pred[ok]
  data.frame(
    period_id = z$period_id[[1L]], unit_id = unit_id, unit_name = unit_name,
    reference_series = reference, n_blocks = length(obs),
    spearman_rho = if (length(obs) >= 3L) suppressWarnings(stats::cor(obs, pred, method = "spearman")) else NA_real_,
    rmse_Mg_ha = if (length(obs)) sqrt(mean((pred - obs)^2)) else NA_real_,
    mae_Mg_ha = if (length(obs)) mean(abs(pred - obs)) else NA_real_,
    bias_Mg_ha = if (length(obs)) mean(pred - obs) else NA_real_,
    direction_agreement = if (length(obs)) mean(sign_label(pred) == sign_label(obs)) else NA_real_,
    wasserstein_Mg_ha = wasserstein_equal_n(obs, pred),
    consistency_interval_coverage = if (nrow(z)) mean(z$inside_consistency_interval, na.rm = TRUE) else NA_real_,
    median_interval_width_Mg_ha = stats::median(z$interval_width_Mg_ha, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

diagnostic_rows <- list()
for (pid in unique(block_envelope$period_id)) {
  z0 <- block_envelope[block_envelope$period_id == pid, , drop = FALSE]
  groups <- c("GOG", unique(z0$unit_id))
  for (uid in groups) {
    z <- if (uid == "GOG") z0 else z0[z0$unit_id == uid, , drop = FALSE]
    uname <- if (uid == "GOG") "Gulf of Guinea" else z$unit_name[[1L]]
    refs <- list(
      capped_median = z$capped_delta_Mg_ha_median,
      uncapped_median = z$uncapped_delta_Mg_ha_median,
      interval_midpoint = (z$envelope_lower_Mg_ha + z$envelope_upper_Mg_ha) / 2
    )
    for (nm in names(refs)) diagnostic_rows[[length(diagnostic_rows) + 1L]] <- diagnostic_one(z, uid, uname, nm, refs[[nm]])
  }
}
block_diagnostics <- do.call(rbind, diagnostic_rows)

set.seed(bootstrap_seed)
bootstrap_rows <- list()
for (pid in unique(block_envelope$period_id)) {
  z0 <- block_envelope[block_envelope$period_id == pid, , drop = FALSE]
  groups <- c("GOG", unique(z0$unit_id))
  for (uid in groups) {
    z <- if (uid == "GOG") z0 else z0[z0$unit_id == uid, , drop = FALSE]
    n <- nrow(z)
    vals <- if (n) replicate(bootstrap_reps, mean(sample(z$inside_consistency_interval, n, replace = TRUE), na.rm = TRUE)) else NA_real_
    bootstrap_rows[[length(bootstrap_rows) + 1L]] <- data.frame(
      period_id = pid, unit_id = uid, unit_name = if (uid == "GOG") "Gulf of Guinea" else z$unit_name[[1L]],
      n_blocks = n, coverage = if (n) mean(z$inside_consistency_interval, na.rm = TRUE) else NA_real_,
      bootstrap_q025 = safe_quantile(vals, 0.025), bootstrap_q975 = safe_quantile(vals, 0.975),
      bootstrap_reps = bootstrap_reps, bootstrap_seed = bootstrap_seed, stringsAsFactors = FALSE
    )
  }
}
block_coverage_bootstrap <- do.call(rbind, bootstrap_rows)

annual <- traj_obs[traj_obs$unit_id == "GOG", c("year", "agb_Mg")]
annual <- annual[order(annual$year), ]
annual$annual_delta_Mg <- c(NA_real_, diff(annual$agb_Mg))
annual$annual_delta_percent_previous <- c(NA_real_, 100 * diff(annual$agb_Mg) / head(annual$agb_Mg, -1L))
annual$flag_recent_comparability_unconfirmed <- !prepared$design$ctrees_recent_comparable & annual$year >= 2024L

theme_validation <- ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.position = "bottom")

# Figure 1: fixed-support regional trajectory, normalized at primary start.
gog_t <- trajectory_summary[trajectory_summary$unit_id == "GOG", ]
gog_obs <- gog_t[gog_t$source == "ctrees", ]
gog_mod <- gog_t[gog_t$source == "mofuss", ]
p1 <- ggplot2::ggplot() +
  ggplot2::geom_ribbon(data = gog_mod, ggplot2::aes(x = year, ymin = relative_agb_change_percent_q025, ymax = relative_agb_change_percent_q975, fill = config), alpha = 0.22) +
  ggplot2::geom_line(data = gog_mod, ggplot2::aes(x = year, y = relative_agb_change_percent_median, colour = config), linewidth = 0.9) +
  ggplot2::geom_line(data = gog_obs, ggplot2::aes(x = year, y = relative_agb_change_percent_median), colour = "black", linewidth = 1.05) +
  ggplot2::geom_vline(xintercept = c(primary_start, prepared$periods$end_year[prepared$periods$is_primary][[1L]]), linetype = "dotted", colour = "grey35") +
  ggplot2::scale_colour_manual(values = c(capped = "#B53A3A", uncapped = "#2878B5")) +
  ggplot2::scale_fill_manual(values = c(capped = "#D97979", uncapped = "#67A8D4")) +
  ggplot2::labs(x = NULL, y = paste0("AGB change from ", primary_start, " baseline (%)"), colour = "BaU configuration", fill = "BaU configuration",
                title = "Gulf of Guinea AGB trajectory on a fixed common support",
                subtitle = "Black: CTrees; lines/ribbons: MoFuSS MC median and 2.5-97.5% quantiles") + theme_validation
ggplot2::ggsave(file.path(stage_dir, paste0("figure1_regional_trajectory_primary_", primary_pid, ".png")), p1, width = 10.5, height = 6.3, dpi = 180)

# Figure 2: country-period forest plot in percentage-change units.
forest <- envelope[envelope$unit_level == "country", ]
forest$period_label <- paste0(gsub("_", "-", forest$period_id, fixed = TRUE), ifelse(forest$is_primary, " (primary)", ""))
forest$unit_name <- factor(forest$unit_name, levels = rev(unique(forest$unit_name)))
p2 <- ggplot2::ggplot(forest, ggplot2::aes(y = unit_name)) +
  ggplot2::geom_segment(ggplot2::aes(x = envelope_lower_percent, xend = envelope_upper_percent, yend = unit_name), linewidth = 1.6, colour = "grey55") +
  ggplot2::geom_point(ggplot2::aes(x = capped_delta_percent_median, colour = "Capped median"), shape = 17, size = 2.3) +
  ggplot2::geom_point(ggplot2::aes(x = uncapped_delta_percent_median, colour = "Uncapped median"), shape = 17, size = 2.3) +
  ggplot2::geom_point(ggplot2::aes(x = observed_delta_percent, fill = inside_consistency_interval), shape = 21, colour = "black", size = 2.8, stroke = 0.5) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey45") +
  ggplot2::facet_wrap(~period_label, scales = "free_x") +
  ggplot2::scale_colour_manual(values = c("Capped median" = "#B53A3A", "Uncapped median" = "#2878B5"), name = NULL) +
  ggplot2::scale_fill_manual(values = c(`TRUE` = "#3B9D5D", `FALSE` = "#F0A43A"), name = "Observed inside interval") +
  ggplot2::labs(x = "Endpoint AGB change (%)", y = NULL, title = "Country-scale observed change versus BaU consistency interval",
                subtitle = "Grey segment: min/max of capped and uncapped MC 2.5--97.5% quantiles") + theme_validation
ggplot2::ggsave(file.path(stage_dir, "figure2_country_period_forest.png"), p2, width = 13, height = 8.2, dpi = 180)

# Figure 3: primary-period 50-km block maps.  First three panels share a
# symmetric colour range; outside-interval distance has its own symmetric range.
primary_blocks <- block_envelope[block_envelope$period_id == primary_pid, ]
block_id_r <- terra::rast(block_raster_path)
ids_cell <- as.integer(round(as.numeric(terra::values(block_id_r, mat = FALSE))))
map_from_blocks <- function(values) {
  lookup <- rep(NA_real_, max(primary_blocks$block_id, na.rm = TRUE))
  lookup[primary_blocks$block_id] <- values
  out <- block_id_r
  mapped <- rep(NA_real_, length(ids_cell))
  ok <- is.finite(ids_cell) & ids_cell >= 1L & ids_cell <= length(lookup)
  mapped[ok] <- lookup[ids_cell[ok]]
  terra::values(out) <- mapped
  out
}
map_obs <- map_from_blocks(primary_blocks$observed_delta_Mg_ha)
map_cap <- map_from_blocks(primary_blocks$capped_delta_Mg_ha_median)
map_unc <- map_from_blocks(primary_blocks$uncapped_delta_Mg_ha_median)
map_dist <- map_from_blocks(primary_blocks$signed_outside_distance_Mg_ha)
map_stack <- c(map_obs, map_cap, map_unc, map_dist)
names(map_stack) <- c("CTrees", "Capped_BaU_median", "Uncapped_BaU_median", "Outside_interval_distance")
terra::writeRaster(map_stack, file.path(stage_dir, paste0("primary_", primary_pid, "_block50km_diagnostics.tif")), overwrite = FALSE, datatype = "FLT4S", NAflag = -9999)
common_lim <- safe_quantile(abs(c(terra::minmax(map_obs), terra::minmax(map_cap), terra::minmax(map_unc))), 0.99)
if (!is.finite(common_lim) || common_lim == 0) common_lim <- 1
dist_lim <- safe_quantile(abs(terra::minmax(map_dist)), 0.99)
if (!is.finite(dist_lim) || dist_lim == 0) dist_lim <- 1
cols <- grDevices::colorRampPalette(c("#6B1F1F", "#E69F9F", "#F7F7F7", "#9BC4E2", "#185A8D"))(101)
grDevices::png(file.path(stage_dir, paste0("figure3_primary_", primary_pid, "_block50km_maps.png")), width = 2600, height = 2100, res = 220)
graphics::par(mfrow = c(2, 2), mar = c(2.2, 2.2, 3.2, 4.5), oma = c(0, 0, 3.5, 0))
terra::plot(map_obs, col = cols, range = c(-common_lim, common_lim), main = "CTrees observed change", axes = FALSE, plg = list(title = "MgDM/ha"))
terra::plot(map_cap, col = cols, range = c(-common_lim, common_lim), main = "Capped BaU MC median", axes = FALSE, plg = list(title = "MgDM/ha"))
terra::plot(map_unc, col = cols, range = c(-common_lim, common_lim), main = "Uncapped BaU MC median", axes = FALSE, plg = list(title = "MgDM/ha"))
terra::plot(map_dist, col = cols, range = c(-dist_lim, dist_lim), main = "Observed distance outside interval", axes = FALSE, plg = list(title = "MgDM/ha"))
graphics::mtext(paste0("Primary period ", primary_label, "; fixed common endpoint support; 50-km blocks"), outer = TRUE, side = 3, line = 1.2, cex = 1.05)
grDevices::dev.off()

# Figure 4: distributional comparison of primary-period block changes.
ecdf_long <- rbind(
  data.frame(series = "CTrees", value = primary_blocks$observed_delta_Mg_ha),
  data.frame(series = "Capped BaU median", value = primary_blocks$capped_delta_Mg_ha_median),
  data.frame(series = "Uncapped BaU median", value = primary_blocks$uncapped_delta_Mg_ha_median)
)
p4 <- ggplot2::ggplot(ecdf_long, ggplot2::aes(x = value, colour = series)) +
  ggplot2::stat_ecdf(linewidth = 0.9, geom = "step", na.rm = TRUE) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey45") +
  ggplot2::scale_colour_manual(values = c("CTrees" = "black", "Capped BaU median" = "#B53A3A", "Uncapped BaU median" = "#2878B5")) +
  ggplot2::labs(x = "50-km block AGB change (MgDM/ha)", y = "Empirical cumulative probability", colour = NULL,
                title = paste0("Spatial distribution of AGB change, ", primary_label),
                subtitle = "Descriptive ECDFs on the same eligible block set") + theme_validation
ggplot2::ggsave(file.path(stage_dir, paste0("figure4_primary_", primary_pid, "_block50km_ecdf.png")), p4, width = 9.5, height = 6.2, dpi = 180)

# Figure 5: annual CTrees diagnostic, with recent unconfirmed years highlighted.
annual_plot <- annual[is.finite(annual$annual_delta_percent_previous), ]
p5 <- ggplot2::ggplot(annual_plot, ggplot2::aes(x = year, y = annual_delta_percent_previous, fill = flag_recent_comparability_unconfirmed)) +
  ggplot2::geom_col(width = 0.78) +
  ggplot2::geom_hline(yintercept = 0, colour = "grey25") +
  ggplot2::scale_fill_manual(values = c(`FALSE` = "#5F7F99", `TRUE` = "#E28E2C"), labels = c(`FALSE` = "2001-2023", `TRUE` = "2024-2025"), name = "CTrees period") +
  ggplot2::labs(x = NULL, y = "Annual AGB change from prior year (%)", title = "CTrees annual-change diagnostic on the fixed trajectory support",
                subtitle = "Large annual steps can dominate endpoint validation and require product-version confirmation") + theme_validation
ggplot2::ggsave(file.path(stage_dir, "figure5_ctrees_annual_change_diagnostic.png"), p5, width = 10.5, height = 5.8, dpi = 180)

write_csv_gz(trajectory, file.path(stage_dir, "trajectory_all_mc_normalized.csv.gz"))
utils::write.csv(trajectory_summary, file.path(stage_dir, "trajectory_summary.csv"), row.names = FALSE, na = "")
write_csv_gz(prepared$period_all_mc, file.path(stage_dir, "unit_period_all_mc.csv.gz"))
utils::write.csv(period_model_summary, file.path(stage_dir, "unit_period_model_quantiles.csv"), row.names = FALSE, na = "")
utils::write.csv(envelope, file.path(stage_dir, "unit_period_consistency_envelope.csv"), row.names = FALSE, na = "")
write_csv_gz(prepared$block_all_mc, file.path(stage_dir, "block50km_period_all_mc.csv.gz"))
utils::write.csv(block_envelope, file.path(stage_dir, "block50km_consistency_envelope.csv"), row.names = FALSE, na = "")
utils::write.csv(block_diagnostics, file.path(stage_dir, "block50km_diagnostics.csv"), row.names = FALSE, na = "")
utils::write.csv(block_coverage_bootstrap, file.path(stage_dir, "block50km_coverage_bootstrap.csv"), row.names = FALSE, na = "")
utils::write.csv(annual, file.path(stage_dir, "ctrees_annual_change_diagnostic.csv"), row.names = FALSE, na = "")
utils::write.csv(prepared$coverage, file.path(stage_dir, "comparison_support_coverage.csv"), row.names = FALSE, na = "")
utils::write.csv(prepared$input_manifest, file.path(stage_dir, "upstream_input_manifest.csv"), row.names = FALSE, na = "")

primary_region <- envelope[envelope$is_primary & envelope$unit_id == "GOG", , drop = FALSE]
primary_coverage <- block_coverage_bootstrap[block_coverage_bootstrap$period_id == primary_pid & block_coverage_bootstrap$unit_id == "GOG", , drop = FALSE]
fmt <- function(x, digits = 2) ifelse(is.finite(x), format(round(x, digits), nsmall = digits, big.mark = ",", scientific = FALSE), "NA")
report <- c(
  "# MoFuSS--CTrees AGB consistency validation",
  "",
  paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  "",
  "## Interpretation",
  "",
  "This validation tests whether CTrees all-driver AGB change is empirically consistent with the structural--stochastic range formed by capped and uncapped BaU MoFuSS runs. It does not estimate observed fNRB, attribute observed loss to woodfuel, or treat capped and uncapped configurations as a formal prediction interval.",
  "",
  "Model AGB is an extensive Mg-per-cell quantity and is summed directly. CTrees MgDM/ha density is converted to cell mass with geodesic cell area. Every comparison uses a fixed common support across CTrees and all 30 MC realizations of both BaU configurations.",
  "",
  "## Primary regional result",
  "",
  paste0("Configured primary period: **", primary_label, "**. CTrees recent-year comparability confirmed: **", prepared$design$ctrees_recent_comparable, "**."),
  paste0("Observed AGB change: **", fmt(primary_region$observed_delta_agb_Mg / 1e6), " MtDM** (", fmt(primary_region$observed_delta_percent), "%)."),
  paste0("Capped BaU median: **", fmt(primary_region$capped_delta_agb_Mg_median / 1e6), " MtDM**; uncapped BaU median: **", fmt(primary_region$uncapped_delta_agb_Mg_median / 1e6), " MtDM**."),
  paste0("Structural--stochastic consistency interval: **[", fmt(primary_region$envelope_lower_Mg / 1e6), ", ", fmt(primary_region$envelope_upper_Mg / 1e6), "] MtDM**; observed result inside: **", primary_region$inside_consistency_interval, "**."),
  paste0("Primary-period 50-km block coverage: **", fmt(100 * primary_coverage$coverage, 1), "%** (block-bootstrap 95% interval ", fmt(100 * primary_coverage$bootstrap_q025, 1), "--", fmt(100 * primary_coverage$bootstrap_q975, 1), "%; ", primary_coverage$n_blocks, " blocks)."),
  "",
  "## Temporal design",
  "",
  "The primary period remains 2010--2020 while the pronounced 2023--2024 CTrees step and recent product comparability are unconfirmed. Results for 2010--2025 and 2000--2025 are sensitivities; 2000--2010 and 2020--2025 are diagnostics. The configuration can promote 2010--2025 after comparability is confirmed without changing the analysis code.",
  "",
  "## Reading the interval",
  "",
  "For each unit and period, the lower endpoint is the smaller of the capped and uncapped MC 2.5% quantiles; the upper endpoint is the larger of their 97.5% quantiles. This deliberately combines structural and stochastic spread. High coverage is descriptive rather than proof of model validity, especially when the interval is wide. An observation outside the interval flags empirical inconsistency; its signed and interval-width-normalized distances quantify severity.",
  "",
  "## Spatial diagnostics",
  "",
  "Country and 50-km summaries use the same endpoint support within each period. One-kilometre pixels are aggregation inputs, not independent validation replicates. Block correlations, errors, direction agreement, Wasserstein distance, and bootstrap coverage are descriptive diagnostics and do not establish causal attribution.",
  "",
  "## Outputs",
  "",
  "Tables retain all MC realizations, model quantiles, interval endpoints, support coverage, block diagnostics, and provenance. Figures show the regional trajectory, country-period comparison, primary-period 50-km maps, block ECDFs, and CTrees annual-change diagnostic."
)
writeLines(report, file.path(stage_dir, "validation_report.md"))

analysis <- list(
  schema_version = "agb_consistency_envelope_v4", created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  design = prepared$design, periods = prepared$periods, trajectory_summary = trajectory_summary,
  period_model_summary = period_model_summary, unit_period_envelope = envelope,
  block_envelope = block_envelope, block_diagnostics = block_diagnostics,
  block_coverage_bootstrap = block_coverage_bootstrap, annual_observation_diagnostic = annual
)
saveRDS(analysis, file.path(stage_dir, "agb_consistency_analysis_v4.rds"), compress = "xz")

provenance <- data.frame(
  role = c("prepared_rds", "primary_block_id_raster", "analysis_script"),
  path = normalize_slash(c(prepared_path, block_raster_path, script_path())),
  md5 = unname(tools::md5sum(c(prepared_path, block_raster_path, script_path()))),
  stringsAsFactors = FALSE
)
utils::write.csv(provenance, file.path(stage_dir, "analysis_provenance.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(stage_dir, "sessionInfo.txt"))

if (!file.rename(stage_dir, completed_stage_dir)) stop("Could not promote staging output to completed temporary directory: ", completed_stage_dir, call. = FALSE)
if (!dir.exists(dirname(final_dir))) dir.create(dirname(final_dir), recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(dirname(final_dir))) stop("Could not create final validation root: ", dirname(final_dir), call. = FALSE)
copy_directory(completed_stage_dir, final_dir)
cat("Validation analysis complete.\n")
cat("Primary regional observation inside consistency interval: ", primary_region$inside_consistency_interval, "\n", sep = "")
cat("Retained completed temporary product: ", completed_stage_dir, "\n", sep = "")
cat("Output: ", final_dir, "\n", sep = "")
