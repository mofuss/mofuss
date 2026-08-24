#!/usr/bin/env Rscript
# ==============================================================================
# MoFuSS mechanics verification (v4 mechanics; multi-run/outreach edition)
#
# Audits every Monte Carlo realization and every annual raster in native model
# units (Mg dry matter per grid cell).  It independently verifies:
#   1. forest growth from the sampled MC parameter rasters;
#   2. forest and TOF post-harvest mass balance;
#   3. final standing-stock capping (expected-map versus realized harvest);
#   4. the earlier TOF annual-supply cap and its redistribution to forests;
#   5. state integrity (unexpected NULL cells, zero-supply TOFs, K overshoot);
#   6. raw demand -> assigned harvest -> realized harvest reconciliation; and
#   7. the grid-cell area/unit convention used by the current workflow.
#
# Usage:
#   Rscript 1_mechanics_verifications_v2.R [working_dir ...]
#
# If per-MC Expect_harv_tot / Non_harv_AGR diagnostics have not been saved in
# debugging_N, annual pixel-level capping is audited only for the MC represented
# by the shared Debugging folder (normally the last MC).  Full-run cumulative
# capping remains available for every MC from Temp/2_*_TOTNN.tif.
# ==============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(data.table)
  library(ggplot2)
})

cfg <- list(
  # Edit this vector to audit any number of completed MoFuSS working folders.
  # Command-line folders, when supplied, replace this vector.
  working_dirs = c(
    "D:/ken_1000m_bau1_2030_mc2_capped",
    "D:/ken_1000m_bau1_2030_mc2_uncapped",
    "D:/ken_1000m_ics3_2030_mc2_capped",
    "D:/ken_1000m_ics3_2030_mc2_uncapped"
  ),
  growth_model = "auto",       # auto | logistic | chapman-richards
  depleted_reset_Mg_cell = 2,  # EGO feedback stock after depleted forest
  float_tolerance_Mg_cell = 0.01,
  plot_seed = 42L,
  plot_cells_per_group = 3L
)

args <- commandArgs(trailingOnly = TRUE)
if (length(args)) cfg$working_dirs <- args[nzchar(args)]
cfg$working_dirs <- unique(as.character(cfg$working_dirs[nzchar(cfg$working_dirs)]))
if (!length(cfg$working_dirs)) {
  stop(
    "Set cfg$working_dirs or supply one or more working folders on the command line.",
    call. = FALSE
  )
}

run_verifier <- function(selected_working_dir, base_cfg) {
cfg <- base_cfg
cfg$working_dir <- selected_working_dir

stopf <- function(...) stop(sprintf(...), call. = FALSE)
assert_data_table <- function(x, label) {
  if (!data.table::is.data.table(x)) {
    stopf(
      "Internal verifier object '%s' lost its data.table class (class: %s)",
      label, paste(class(x), collapse = "/")
    )
  }
  invisible(TRUE)
}
wd <- normalizePath(cfg$working_dir, winslash = "/", mustWork = TRUE)

temp_dir <- file.path(wd, "Temp")
lulcc_raster_dir <- file.path(wd, "LULCC", "TempRaster")
lulcc_table_dir <- file.path(wd, "LULCC", "TempTables")
shared_debug_dir <- file.path(wd, "Debugging")
parameter_file <- file.path(lulcc_table_dir, "parameters_dinamica.csv")

# Validate the selected run before deleting anything.  This prevents a typo in
# working_dir from turning the cleanup below into an unrelated directory edit.
required_run_dirs <- c(temp_dir, lulcc_raster_dir, lulcc_table_dir)
missing_run_dirs <- required_run_dirs[!dir.exists(required_run_dirs)]
debug_dir_names <- basename(list.dirs(wd, recursive = FALSE, full.names = TRUE))
has_debug_dir <- any(grepl("^debugging_[0-9]+$", debug_dir_names))
if (length(missing_run_dirs) || !file.exists(parameter_file) || !has_debug_dir) {
  missing_markers <- c(
    missing_run_dirs,
    if (!file.exists(parameter_file)) parameter_file,
    if (!has_debug_dir) file.path(wd, "debugging_N")
  )
  stopf(
    "Selected folder does not look like a completed MoFuSS run; missing: %s",
    paste(missing_markers, collapse = ", ")
  )
}

# The verifier has exactly one product directory, always inside the selected
# MoFuSS run.  Keep the lexical paths for deletion so a link can never redirect
# recursive cleanup to another location.
path_key <- function(path) {
  key <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (.Platform$OS.type == "windows") tolower(key) else key
}
lexical_path_key <- function(path) {
  key <- sub("/+$", "", gsub("\\\\", "/", path))
  if (.Platform$OS.type == "windows") tolower(key) else key
}
same_path <- function(left, right) identical(path_key(left), path_key(right))
output_leaf <- "pixel-wise mechanics verification"
out_parent <- file.path(wd, "Out")
out_dir_lexical <- file.path(out_parent, output_leaf)
if (!same_path(dirname(out_parent), wd) ||
    !identical(tolower(basename(out_parent)), "out") ||
    !same_path(dirname(out_dir_lexical), out_parent) ||
    !identical(basename(out_dir_lexical), output_leaf)) {
  stopf("Refusing unsafe verifier output path: %s", out_dir_lexical)
}

for (candidate in c(out_parent, out_dir_lexical)) {
  link_target <- Sys.readlink(candidate)
  if (length(link_target) && !is.na(link_target) && nzchar(link_target)) {
    stopf("Refusing to use linked verifier output path: %s", candidate)
  }
  if (dir.exists(candidate)) {
    resolved_candidate <- normalizePath(candidate, winslash = "/", mustWork = TRUE)
    if (!identical(lexical_path_key(resolved_candidate), lexical_path_key(candidate))) {
      stopf("Refusing to use redirected verifier output path: %s", candidate)
    }
  }
}
if (file.exists(out_parent) && !dir.exists(out_parent)) {
  stopf("Verifier output parent exists but is not a directory: %s", out_parent)
}
if (file.exists(out_dir_lexical) && !dir.exists(out_dir_lexical)) {
  stopf("Verifier output path exists but is not a directory: %s", out_dir_lexical)
}
if (dir.exists(out_dir_lexical)) {
  unlink_status <- unlink(out_dir_lexical, recursive = TRUE, force = TRUE)
  if (unlink_status != 0L || file.exists(out_dir_lexical) || dir.exists(out_dir_lexical)) {
    stopf("Could not completely remove prior verifier products: %s", out_dir_lexical)
  }
}
if (!dir.create(out_dir_lexical, recursive = TRUE, showWarnings = FALSE) ||
    !dir.exists(out_dir_lexical)) {
  stopf("Could not create verifier output directory: %s", out_dir_lexical)
}
out_dir <- normalizePath(out_dir_lexical, winslash = "/", mustWork = TRUE)
if (!identical(lexical_path_key(out_dir), lexical_path_key(out_dir_lexical)) ||
    !same_path(dirname(out_dir), out_parent) ||
    !identical(basename(out_dir), output_leaf)) {
  stopf("Verifier output directory failed its post-creation safety check: %s", out_dir)
}
message("Reset verifier output directory: ", out_dir)

tol <- cfg$float_tolerance_Mg_cell
need_file <- function(path) {
  if (!file.exists(path)) stopf("Required file is missing: %s", path)
  path
}
.audit_template <- NULL
read_vec <- function(path) {
  x <- rast(need_file(path))
  if (!is.null(.audit_template) &&
      !isTRUE(compareGeom(.audit_template, x, stopOnError = FALSE, messages = FALSE))) {
    stopf("Raster geometry does not match the simulation grid: %s", path)
  }
  as.numeric(values(x, mat = FALSE))
}
sum0 <- function(x) sum(x[is.finite(x)])
sum_complete <- function(x) if (length(x) && all(is.finite(x))) sum(x) else NA_real_
mean_safe <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}
max_safe <- function(x, empty = NA_real_) {
  x <- x[is.finite(x)]
  if (length(x)) max(x) else empty
}
min_safe <- function(x, empty = NA_real_) {
  x <- x[is.finite(x)]
  if (length(x)) min(x) else empty
}
pct <- function(num, den) {
  n <- max(length(num), length(den))
  if (!n) return(numeric())
  num <- rep_len(num, n)
  den <- rep_len(den, n)
  out <- rep(NA_real_, n)
  ok <- is.finite(num) & is.finite(den) & den != 0
  out[ok] <- 100 * num[ok] / den[ok]
  out
}
as_num <- function(x) suppressWarnings(as.numeric(x))
value_or <- function(x, default = NA_real_) if (length(x) && is.finite(as_num(x[[1L]]))) as_num(x[[1L]]) else default

series_paths <- function(directory, prefix, suffixes) {
  p <- file.path(directory, paste0(prefix, suffixes, ".tif"))
  if (all(file.exists(p))) p else NULL
}

error_stats <- function(observed, predicted, domain, tolerance = tol) {
  domain <- !is.na(domain) & domain
  both <- domain & is.finite(observed) & is.finite(predicted)
  err <- abs(observed[both] - predicted[both])
  data.table(
    domain_cells = sum(domain),
    compared_cells = sum(both),
    missing_observed_cells = sum(domain & !is.finite(observed)),
    missing_predicted_cells = sum(domain & !is.finite(predicted)),
    mean_abs_error_Mg_cell = if (length(err)) mean(err) else NA_real_,
    max_abs_error_Mg_cell = if (length(err)) max(err) else NA_real_,
    cells_over_tolerance = if (length(err)) sum(err > tolerance) else NA_integer_
  )
}

# ---- Simulation metadata -----------------------------------------------------
parameter_file <- need_file(parameter_file)
parameters <- fread(parameter_file)
if (!all(c("Var", "ParCHR") %in% names(parameters))) {
  stopf("Unexpected schema in %s", parameter_file)
}
parameter_value <- function(key, default = NA_real_) {
  x <- parameters[Var == key, ParCHR]
  if (length(x)) value_or(x, default) else default
}

start_year <- as.integer(parameter_value("start_year"))
end_year <- as.integer(parameter_value("end_year"))
n_mc_declared <- as.integer(parameter_value("monte_carlo_runs"))
if (!is.finite(start_year) || !is.finite(end_year) || end_year < start_year) {
  stopf("Invalid start/end years in %s", parameter_file)
}
years <- seq.int(start_year, end_year)
suffixes <- sprintf("%02d", seq_along(years))

debug_dirs <- list.dirs(wd, recursive = FALSE, full.names = TRUE)
debug_ids <- suppressWarnings(as.integer(sub("^debugging_", "", basename(debug_dirs))))
mc_ids <- sort(unique(debug_ids[is.finite(debug_ids)]))
if (is.finite(n_mc_declared)) mc_ids <- base::intersect(seq_len(n_mc_declared), mc_ids)
if (!length(mc_ids)) stopf("No debugging_N directories were found under %s", wd)

# The iteration length is currently defined inside rnorm_v3.R.
iteration_weeks <- NA_real_
rnorm_run <- file.path(wd, "rnorm_v3.R")
if (file.exists(rnorm_run)) {
  rnorm_text <- readLines(rnorm_run, warn = FALSE)
  il_line <- grep("^[[:space:]]*IL[[:space:]]*=", rnorm_text, value = TRUE)
  if (length(il_line)) {
    iteration_weeks <- as_num(sub(".*IL[[:space:]]*=[[:space:]]*([0-9.]+).*", "\\1", il_line[[1L]]))
  }
}

period_starts <- seq.int(start_year, end_year, by = 10L)
period_ends <- pmin(period_starts + 9L, end_year)
period_labels <- sprintf("%d-%d", period_starts, period_ends)
reporting_period <- function(y) {
  period_labels[((as.integer(y) - start_year) %/% 10L) + 1L]
}

message(sprintf(
  "Auditing %s | years %d-%d | MC: %s",
  basename(wd), start_year, end_year, paste(mc_ids, collapse = ", ")
))

# ---- Geometry and native units ----------------------------------------------
template_file <- need_file(file.path(temp_dir, sprintf("2_IniSt%02d.tif", mc_ids[[1L]])))
template <- rast(template_file)
.audit_template <- template
resolution_xy <- res(template)
nominal_area_ha <- abs(prod(resolution_xy)) / 10000

area_mask_template <- rast(need_file(file.path(temp_dir, sprintf("2_TOFvsFOR%02d.tif", mc_ids[[1L]]))))
area_geodesic <- cellSize(area_mask_template, unit = "ha", mask = TRUE)
area_stats <- global(area_geodesic, c("min", "mean", "max"), na.rm = TRUE)[1, ]
geodesic_min_ha <- as.numeric(area_stats[["min"]])
geodesic_mean_ha <- as.numeric(area_stats[["mean"]])
geodesic_max_ha <- as.numeric(area_stats[["max"]])

geometry_and_units <- data.table(
  metric = c(
    "nrow", "ncol", "x_resolution_m", "y_resolution_m",
    "nominal_projected_area_ha_cell", "geodesic_area_min_ha_cell",
    "geodesic_area_mean_ha_cell", "geodesic_area_max_ha_cell",
    "nominal_minus_geodesic_mean_percent", "native_stock_flow_unit"
  ),
  value = c(
    nrow(template), ncol(template), resolution_xy[[1L]], resolution_xy[[2L]],
    nominal_area_ha, geodesic_min_ha, geodesic_mean_ha, geodesic_max_ha,
    pct(nominal_area_ha - geodesic_mean_ha, geodesic_mean_ha),
    "Mg dry matter per grid cell (flows are Mg/grid-cell/model iteration)"
  )
)
fwrite(geometry_and_units, file.path(out_dir, "geometry_and_units.csv"))

# ---- Demand: raw -> assigned -------------------------------------------------
demand_table_dir <- file.path(wd, "In", "DemandScenarios")
raw_demand_dir <- file.path(
  wd, "LULCC", "DownloadedDatasets", "SourceDataGlobal", "demand", "demand_temp"
)

csv_value_sum <- function(path) {
  if (!file.exists(path)) return(NA_real_)
  x <- fread(path)
  if ("Value" %in% names(x)) return(sum_complete(as_num(x[["Value"]])))
  numeric_columns <- names(x)[vapply(x, is.numeric, logical(1))]
  if (!length(numeric_columns)) return(NA_real_)
  sum_complete(as_num(x[[tail(numeric_columns, 1L)]]))
}

raw_demand_sum <- function(year, channel) {
  if (!dir.exists(raw_demand_dir)) return(NA_real_)
  p <- list.files(
    raw_demand_dir,
    pattern = sprintf("_%d_wftons_%s\\.tif$", year, channel),
    full.names = TRUE, recursive = TRUE, ignore.case = TRUE
  )
  if (length(p) != 1L) return(NA_real_)
  as.numeric(global(rast(p), "sum", na.rm = TRUE)[1, 1])
}

demand_by_year <- rbindlist(lapply(seq_along(years), function(j) {
  s <- suffixes[[j]]
  raw_w <- raw_demand_sum(years[[j]], "w")
  raw_v <- raw_demand_sum(years[[j]], "v")
  assigned_w <- csv_value_sum(file.path(demand_table_dir, paste0("fwuse_W", s, ".csv")))
  assigned_v <- csv_value_sum(file.path(demand_table_dir, paste0("fwuse_V", s, ".csv")))
  data.table(
    year = years[[j]], suffix = s, period = reporting_period(years[[j]]),
    raw_demand_W_Mg = raw_w, raw_demand_V_Mg = raw_v,
    raw_demand_total_Mg = raw_w + raw_v,
    raw_demand_complete = is.finite(raw_w) && is.finite(raw_v),
    assigned_W_Mg = assigned_w, assigned_V_Mg = assigned_v,
    assigned_expected_total_Mg = assigned_w + assigned_v,
    assigned_demand_complete = is.finite(assigned_w) && is.finite(assigned_v),
    preassignment_gap_Mg = raw_w + raw_v - assigned_w - assigned_v
  )
}))

# Shared dynamic-event maps are normally overwritten by the last MC.  They are
# still useful for determining whether a static-mask recurrence is sufficient.
dynamic_event_diagnostics <- rbindlist(lapply(
  c("Sim_gain", "Sim_loss", "Fw_def_tot"),
  function(prefix) {
    p <- series_paths(shared_debug_dir, prefix, suffixes)
    if (is.null(p)) return(data.table())
    data.table(
      source = "Debugging (shared; normally last MC)",
      event = prefix, year = years, suffix = suffixes,
      raster_sum = vapply(p, function(f) as.numeric(global(rast(f), "sum", na.rm = TRUE)[1, 1]), numeric(1))
    )
  }
), fill = TRUE)
if (nrow(dynamic_event_diagnostics)) {
  fwrite(dynamic_event_diagnostics, file.path(out_dir, "dynamic_event_diagnostics.csv"))
}
dynamic_event_total <- if (nrow(dynamic_event_diagnostics)) {
  sum(abs(dynamic_event_diagnostics$raster_sum), na.rm = TRUE)
} else NA_real_

# ---- Determine which MC owns shared annual expected-harvest maps ------------
expected_paths <- setNames(vector("list", length(mc_ids)), as.character(mc_ids))
expected_provenance <- vector("list", length(mc_ids))
shared_expected <- series_paths(shared_debug_dir, "Expect_harv_tot", suffixes)

for (ii in seq_along(mc_ids)) {
  mc <- mc_ids[[ii]]
  per_mc <- series_paths(file.path(wd, sprintf("debugging_%d", mc)), "Expect_harv_tot", suffixes)
  if (!is.null(per_mc)) {
    expected_paths[[as.character(mc)]] <- per_mc
    expected_provenance[[ii]] <- data.table(
      mc = mc, source = sprintf("debugging_%d", mc),
      annual_pixel_detail = TRUE, ownership_score_max_Mg_cell = 0,
      ownership_compared_cells = NA_real_, owner_inference = "not needed: per-MC series",
      note = "Per-MC expected-harvest rasters available"
    )
  }
}

shared_owner <- NA_integer_
shared_owner_scores <- setNames(rep(NA_real_, length(mc_ids)), as.character(mc_ids))
shared_owner_compared <- setNames(rep(0, length(mc_ids)), as.character(mc_ids))
shared_owner_inference <- if (is.null(shared_expected)) "shared series unavailable" else "not evaluated"
if (!is.null(shared_expected)) {
  for (mc in mc_ids) {
    dbg <- file.path(wd, sprintf("debugging_%d", mc))
    score <- 0
    compared <- 0
    for (j in seq_along(years)) {
      e <- read_vec(shared_expected[[j]])
      g <- read_vec(file.path(dbg, paste0("Growth", suffixes[[j]], ".tif")))
      h <- read_vec(file.path(dbg, paste0("Harvest_tot", suffixes[[j]], ".tif")))
      z <- is.finite(e) & is.finite(g) & is.finite(h)
      compared <- compared + sum(z)
      if (any(z)) {
        predicted_h <- numeric(sum(z))
        positive <- e[z] > 0 & g[z] > 0
        predicted_h[positive] <- pmin(e[z][positive], g[z][positive])
        score <- max(score, max(abs(h[z] - predicted_h)))
      }
    }
    shared_owner_scores[[as.character(mc)]] <- if (compared > 0) score else NA_real_
    shared_owner_compared[[as.character(mc)]] <- compared
  }
  matches <- names(shared_owner_scores)[
    is.finite(shared_owner_scores) & shared_owner_scores <= tol & shared_owner_compared > 0
  ]
  if (length(matches) == 1L) {
    shared_owner <- as.integer(matches)
    shared_owner_inference <- sprintf("unique match: MC%d", shared_owner)
  } else if (length(matches) > 1L) {
    shared_owner_inference <- sprintf(
      "ambiguous: %s all match within tolerance", paste0("MC", matches, collapse = ", ")
    )
  } else {
    shared_owner_inference <- "no MC matches the shared expected series within tolerance"
  }
}

for (ii in seq_along(mc_ids)) {
  mc <- mc_ids[[ii]]
  if (is.null(expected_paths[[as.character(mc)]]) && is.finite(shared_owner) && mc == shared_owner) {
    expected_paths[[as.character(mc)]] <- shared_expected
    expected_provenance[[ii]] <- data.table(
      mc = mc, source = "Debugging (shared; inferred owner)",
      annual_pixel_detail = TRUE,
      ownership_score_max_Mg_cell = shared_owner_scores[[as.character(mc)]],
      ownership_compared_cells = shared_owner_compared[[as.character(mc)]],
      owner_inference = shared_owner_inference,
      note = "Shared diagnostics are overwritten between MC runs"
    )
  } else if (is.null(expected_provenance[[ii]])) {
    expected_provenance[[ii]] <- data.table(
      mc = mc, source = NA_character_, annual_pixel_detail = FALSE,
      ownership_score_max_Mg_cell = shared_owner_scores[[as.character(mc)]],
      ownership_compared_cells = shared_owner_compared[[as.character(mc)]],
      owner_inference = shared_owner_inference,
      note = "Use aggregate annual demand and cumulative per-MC rasters; save Expect_harv_totNN.tif in debugging_MC for annual spatial detail"
    )
  }
}
expected_provenance <- rbindlist(expected_provenance, fill = TRUE)
fwrite(expected_provenance, file.path(out_dir, "expected_map_provenance.csv"))

# ---- Growth functions --------------------------------------------------------
cr_files <- file.path(lulcc_raster_dir, c("A_c.tif", "k_c.tif", "m_c.tif"))
have_cr <- all(file.exists(cr_files))
A_cr <- k_cr <- m_cr <- rep(NA_real_, ncell(template))
if (have_cr) {
  A_cr <- read_vec(cr_files[[1L]])
  k_cr <- read_vec(cr_files[[2L]])
  m_cr <- read_vec(cr_files[[3L]])
}

predict_growth <- function(model, feedback, tof, K, rmax) {
  out <- feedback
  forest <- is.finite(tof) & tof == 0
  if (model == "logistic") {
    out[forest] <- NA_real_
    zero_K <- forest & is.finite(K) & K <= 0
    valid <- forest & is.finite(feedback) & is.finite(K) & K > 0 & is.finite(rmax)
    out[zero_K] <- 0
    raw <- feedback[valid] + rmax[valid] * feedback[valid] *
      (1 - feedback[valid] / K[valid])
    # EGOML v6 enforces the capped-regrowth invariant after every logistic step.
    out[valid] <- pmin(K[valid], pmax(0, raw))
  } else {
    valid <- forest & is.finite(feedback) & is.finite(A_cr) & A_cr > 0 &
      is.finite(k_cr) & k_cr > 0 & is.finite(m_cr) & m_cr > 0
    out[forest] <- NA_real_
    ratio_raw <- feedback[valid] / A_cr[valid]
    # EGO substitutes 0.999999 only when B/A >= 1; values just below A retain
    # their exact ratio, while depleted/nonpositive stock maps to age zero.
    ratio <- ifelse(
      feedback[valid] <= 0,
      0,
      ifelse(ratio_raw >= 1, 0.999999, ratio_raw)
    )
    age <- -log1p(-ratio^(1 / m_cr[valid])) / k_cr[valid]
    # This reproduces the current EGO implementation: +1 year per iteration.
    target <- A_cr[valid] * (1 - exp(-k_cr[valid] * (age + 1)))^m_cr[valid]
    # Observed stock above fitted A is preserved rather than forced downward.
    out[valid] <- pmax(feedback[valid], target)
  }
  out
}

# Recreate the original simplified MC1 outreach figure without changing any of
# the full-resolution audit products.  The style, variables, sampling strata,
# units, colours, and line patterns match the earlier verifier: post-harvest AGB
# observed from the model, its independent reconstruction, a no-harvest growth
# trajectory, and realized harvest as bars.
write_mc1_outreach_plot <- function(model, initial, tof, K, rmax, dbg_dir) {
  n_steps <- length(years)
  n_cells <- length(initial)
  complete_observed <- is.finite(initial) & is.finite(tof)
  cumulative_harvest <- numeric(n_cells)

  # Stream the annual rasters so the old all-years eligibility rule is retained
  # without holding national raster stacks in memory.
  for (j in seq_along(suffixes)) {
    observed_j <- read_vec(file.path(
      dbg_dir, paste0("Growth_less_harv", suffixes[[j]], ".tif")
    ))
    harvest_j <- read_vec(file.path(
      dbg_dir, paste0("Harvest_tot", suffixes[[j]], ".tif")
    ))
    complete_observed <- complete_observed & is.finite(observed_j)
    finite_harvest <- is.finite(harvest_j)
    cumulative_harvest[finite_harvest] <-
      cumulative_harvest[finite_harvest] + harvest_j[finite_harvest]
  }

  parameter_complete <- if (model == "logistic") {
    is.finite(K) & (K <= 0 | is.finite(rmax))
  } else {
    is.finite(A_cr) & A_cr > 0 & is.finite(k_cr) & k_cr > 0 &
      is.finite(m_cr) & m_cr > 0
  }
  base <- complete_observed & is.finite(initial) & initial > 0
  forest_cells <- which(base & tof == 0 & parameter_complete)
  tof_cells <- which(base & tof == 1)

  set.seed(cfg$plot_seed)
  pick_positions <- function(mask, n) {
    available <- which(mask)
    if (!length(available)) return(integer())
    sample(available, min(n, length(available)))
  }

  forest_agb0_ha <- initial[forest_cells] / nominal_area_ha
  forest_harvest_ha <- cumulative_harvest[forest_cells] / nominal_area_ha
  harvest_cutoff <- if (any(is.finite(forest_harvest_ha))) {
    as.numeric(quantile(forest_harvest_ha, 0.98, na.rm = TRUE))
  } else Inf
  selected_forest <- forest_cells[c(
    pick_positions(forest_agb0_ha >= 2 & forest_agb0_ha < 10, 3L),
    pick_positions(forest_agb0_ha >= 10 & forest_agb0_ha < 40, 3L),
    pick_positions(forest_agb0_ha >= 80, 3L),
    pick_positions(forest_harvest_ha > harvest_cutoff, 3L)
  )]
  selected_tof <- if (length(tof_cells)) {
    sample(tof_cells, min(2L, length(tof_cells)))
  } else integer()
  selected_cells <- c(selected_forest, selected_tof)
  selected_is_forest <- c(
    rep(TRUE, length(selected_forest)),
    rep(FALSE, length(selected_tof))
  )
  if (!length(selected_cells)) {
    message("MC1 simplified outreach figure skipped: no eligible pixels.")
    return(invisible(NULL))
  }

  observed_matrix <- matrix(
    NA_real_, nrow = length(selected_cells), ncol = n_steps
  )
  harvest_matrix <- matrix(
    NA_real_, nrow = length(selected_cells), ncol = n_steps
  )
  for (j in seq_along(suffixes)) {
    observed_matrix[, j] <- read_vec(file.path(
      dbg_dir, paste0("Growth_less_harv", suffixes[[j]], ".tif")
    ))[selected_cells]
    harvest_matrix[, j] <- read_vec(file.path(
      dbg_dir, paste0("Harvest_tot", suffixes[[j]], ".tif")
    ))[selected_cells]
  }

  grow_one <- function(B, cell) {
    if (!is.finite(B)) return(NA_real_)
    if (model == "logistic") {
      if (!is.finite(K[[cell]])) return(NA_real_)
      if (K[[cell]] <= 0) return(0)
      if (!is.finite(rmax[[cell]])) return(NA_real_)
      raw <- B + rmax[[cell]] * B * (1 - B / K[[cell]])
      return(min(K[[cell]], max(0, raw)))
    }
    if (!is.finite(A_cr[[cell]]) || A_cr[[cell]] <= 0 ||
        !is.finite(k_cr[[cell]]) || k_cr[[cell]] <= 0 ||
        !is.finite(m_cr[[cell]]) || m_cr[[cell]] <= 0) {
      return(NA_real_)
    }
    ratio_raw <- B / A_cr[[cell]]
    ratio <- if (B <= 0) 0 else if (ratio_raw >= 1) 0.999999 else ratio_raw
    age <- -log1p(-ratio^(1 / m_cr[[cell]])) / k_cr[[cell]]
    target <- A_cr[[cell]] *
      (1 - exp(-k_cr[[cell]] * (age + 1)))^m_cr[[cell]]
    max(B, target)
  }

  n_columns <- ncol(template)
  trajectory <- rbindlist(lapply(seq_along(selected_cells), function(i) {
    cell <- selected_cells[[i]]
    is_forest <- selected_is_forest[[i]]
    observed <- observed_matrix[i, ] / nominal_area_ha
    harvest <- harvest_matrix[i, ] / nominal_area_ha
    reconstructed <- rep(NA_real_, n_steps)
    growth_only <- rep(NA_real_, n_steps)
    feedback <- initial[[cell]]
    no_harvest_state <- initial[[cell]]

    for (j in seq_len(n_steps)) {
      if (is_forest) {
        predicted_growth <- grow_one(feedback, cell)
        predicted_post <- predicted_growth
        if (is.finite(harvest_matrix[i, j])) {
          predicted_post <- predicted_growth - harvest_matrix[i, j]
        }
        reconstructed[[j]] <- predicted_post / nominal_area_ha
        feedback <- predicted_post
        if (is.finite(feedback) && feedback <= 0) {
          feedback <- cfg$depleted_reset_Mg_cell
        }
        no_harvest_state <- grow_one(no_harvest_state, cell)
        growth_only[[j]] <- no_harvest_state / nominal_area_ha
      } else {
        # TOF is nondegradable in EGOML v6; this is the same observed/replayed
        # identity used by the original outreach figure.
        reconstructed[[j]] <- observed[[j]]
      }
    }

    row <- ((cell - 1L) %/% n_columns) + 1L
    column <- ((cell - 1L) %% n_columns) + 1L
    parameter_text <- if (is_forest && model == "logistic") {
      sprintf(
        "r=%.3f K=AGB0=%.1f",
        rmax[[cell]], initial[[cell]] / nominal_area_ha
      )
    } else if (is_forest) {
      sprintf(
        "A=%.0f k=%.3f m=%.2f",
        A_cr[[cell]] / nominal_area_ha, k_cr[[cell]], m_cr[[cell]]
      )
    } else "TOF residue"
    finite_error <- abs(reconstructed - observed)
    max_error <- if (any(is.finite(finite_error))) {
      max(finite_error, na.rm = TRUE)
    } else NA_real_

    data.table(
      panel = sprintf(
        "px(%d,%d) TOF=%d | %s\nAGB0=%.1f  max|err|=%.2g",
        row, column, as.integer(!is_forest), parameter_text,
        observed[[1L]], max_error
      ),
      year = years,
      observed = observed,
      reconstructed = reconstructed,
      growth_only = growth_only,
      harvest = harvest
    )
  }))
  assert_data_table(trajectory, "MC1 simplified outreach trajectory")
  data.table::set(
    trajectory,
    j = "panel",
    value = factor(trajectory$panel, levels = unique(trajectory$panel))
  )
  line_data <- melt(
    trajectory,
    id.vars = c("panel", "year"),
    measure.vars = c("observed", "reconstructed", "growth_only"),
    variable.name = "series", value.name = "AGB"
  )
  series_labels <- c(
    observed = "observed",
    reconstructed = "reconstructed",
    growth_only = "growth-only (no harvest)"
  )
  data.table::set(
    line_data,
    j = "series",
    value = factor(
      series_labels[as.character(line_data$series)],
      levels = unname(series_labels)
    )
  )

  stock_colours <- c(
    "observed" = "#1f77b4",
    "reconstructed" = "#d62728",
    "growth-only (no harvest)" = "#2e7d32"
  )
  stock_linetypes <- c(
    "observed" = "solid",
    "reconstructed" = "dashed",
    "growth-only (no harvest)" = "dotted"
  )
  outreach_plot <- ggplot() +
    geom_col(
      data = trajectory,
      aes(x = year, y = harvest, fill = "harvest"),
      alpha = 0.5, width = 0.6
    ) +
    geom_line(
      data = line_data,
      aes(x = year, y = AGB, colour = series, linetype = series),
      linewidth = 0.7, na.rm = TRUE
    ) +
    geom_point(
      data = trajectory,
      aes(x = year, y = observed),
      colour = "#1f77b4", size = 0.8
    ) +
    facet_wrap(~panel, scales = "free_y", ncol = 5) +
    scale_colour_manual(name = NULL, values = stock_colours) +
    scale_linetype_manual(name = NULL, values = stock_linetypes) +
    scale_fill_manual(name = NULL, values = c("harvest" = "#e07b39")) +
    labs(
      x = "year", y = "AGB (Mg/ha)",
      title = sprintf(
        "MoFuSS pixel-trajectory validation - %s MC1 %d-%d  (mechanic: %s)",
        basename(wd), start_year, end_year, model
      ),
      subtitle = paste0(
        "observed (blue) vs reconstructed grow-then-harvest (red dashed) ; ",
        "growth-only no-harvest (green dotted) ; harvest (orange)"
      )
    ) +
    theme_bw(base_size = 8) +
    theme(
      legend.position = "top",
      strip.background = element_blank(),
      strip.text = element_text(size = 6.5),
      plot.title = element_text(face = "bold")
    )
  ggsave(
    file.path(out_dir, "pixel_trajectories_validation_MC1.png"),
    outreach_plot,
    width = 19,
    height = 2.7 * ceiling(nrow(trajectory) / n_steps / 5),
    dpi = 140,
    limitsize = FALSE
  )
  message(
    "MC1 simplified outreach figure: ",
    file.path(out_dir, "pixel_trajectories_validation_MC1.png")
  )
  invisible(trajectory)
}

detect_growth_model <- function(mc, initial, tof, K, rmax) {
  candidates <- character()
  logistic_available <-
    all(is.finite(c(K[is.finite(tof) & tof == 0], rmax[is.finite(tof) & tof == 0]))) |
    any(is.finite(K) & K > 0 & is.finite(rmax))
  if (logistic_available) candidates <- c(candidates, "logistic")
  if (have_cr) candidates <- c(candidates, "chapman-richards")
  if (cfg$growth_model != "auto") candidates <- base::intersect(candidates, cfg$growth_model)
  if (!length(candidates)) stopf("No growth-model candidate is available for MC%d", mc)

  scores <- setNames(rep(NA_real_, length(candidates)), candidates)
  for (model in candidates) {
    feedback <- initial
    yearly_mae <- numeric()
    for (j in seq_len(min(3L, length(years)))) {
      g <- read_vec(file.path(wd, sprintf("debugging_%d", mc), paste0("Growth", suffixes[[j]], ".tif")))
      s <- read_vec(file.path(wd, sprintf("debugging_%d", mc), paste0("Growth_less_harv", suffixes[[j]], ".tif")))
      pred <- predict_growth(model, feedback, tof, K, rmax)
      z <- tof == 0 & is.finite(g) & is.finite(pred)
      yearly_mae <- c(yearly_mae, if (any(z)) mean(abs(g[z] - pred[z])) else Inf)
      feedback <- s
      feedback[tof == 0 & is.finite(feedback) & feedback <= 0] <- cfg$depleted_reset_Mg_cell
    }
    scores[[model]] <- mean(yearly_mae)
  }
  list(model = names(which.min(scores)), scores = scores)
}

# ---- Per-MC audit ------------------------------------------------------------
mechanics_annual_all <- list()
state_annual_all <- list()
capping_annual_all <- list()
full_period_class_all <- list()
tof_stage_annual_all <- list()
trajectory_all <- list()
model_detection_all <- list()

for (mc in mc_ids) {
  message(sprintf("MC%d: loading sampled parameter rasters", mc))
  dbg_dir <- file.path(wd, sprintf("debugging_%d", mc))
  initial <- read_vec(file.path(temp_dir, sprintf("2_IniSt%02d.tif", mc)))
  K <- read_vec(file.path(temp_dir, sprintf("2_K%02d.tif", mc)))
  rmax <- read_vec(file.path(temp_dir, sprintf("2_rmax%02d.tif", mc)))
  tof <- read_vec(file.path(temp_dir, sprintf("2_TOFvsFOR%02d.tif", mc)))

  detected <- detect_growth_model(mc, initial, tof, K, rmax)
  model <- detected$model
  model_detection_all[[as.character(mc)]] <- data.table(
    mc = mc, selected_model = model,
    logistic_probe_MAE_Mg_cell = unname(detected$scores["logistic"]),
    chapman_richards_probe_MAE_Mg_cell = unname(detected$scores["chapman-richards"])
  )
  message(sprintf(
    "MC%d: selected %s (%s)", mc, model,
    paste(sprintf("%s MAE=%.5g", names(detected$scores), detected$scores), collapse = "; ")
  ))

  e_paths <- expected_paths[[as.character(mc)]]
  exact_expected <- !is.null(e_paths)

  # Per-MC TOF-stage diagnostics are preferred; shared maps belong only to the
  # inferred shared owner.
  stage_dir <- dbg_dir
  shortage_paths <- series_paths(stage_dir, "Non_harv_AGR", suffixes)
  preliminary_paths <- series_paths(stage_dir, "Proj_harv_Wtot", suffixes)
  allocated_paths <- series_paths(stage_dir, "harv_AGR", suffixes)
  redistributed_paths <- series_paths(stage_dir, "Ex_agr_harv", suffixes)
  if (is.null(shortage_paths) && is.finite(shared_owner) && mc == shared_owner) {
    stage_dir <- shared_debug_dir
    shortage_paths <- series_paths(stage_dir, "Non_harv_AGR", suffixes)
    preliminary_paths <- series_paths(stage_dir, "Proj_harv_Wtot", suffixes)
    allocated_paths <- series_paths(stage_dir, "harv_AGR", suffixes)
    redistributed_paths <- series_paths(stage_dir, "Ex_agr_harv", suffixes)
  }
  exact_tof_stage <- !is.null(shortage_paths) && !is.null(preliminary_paths) &&
    !is.null(allocated_paths) && !is.null(redistributed_paths)

  # Select informative cells before the annual loop.
  gl1 <- read_vec(file.path(dbg_dir, paste0("Growth_less_harv", suffixes[[1L]], ".tif")))
  g2 <- if (length(years) >= 2L) {
    read_vec(file.path(dbg_dir, paste0("Growth", suffixes[[2L]], ".tif")))
  } else rep(NA_real_, length(initial))
  disappeared_tof <- which(tof == 1 & is.finite(initial) & !is.finite(gl1))
  zero_supply_reset_tof <- which(tof == 1 & is.finite(K) & K == 0 &
                                   is.finite(g2) & abs(g2 - cfg$depleted_reset_Mg_cell) <= tol)

  cumulative_expected_path <- file.path(temp_dir, sprintf("2_EXP_CON_TOT%02d.tif", mc))
  cumulative_actual_path <- file.path(temp_dir, sprintf("2_CON_TOT%02d.tif", mc))
  cap_cumulative <- rep(0, length(initial))
  if (file.exists(cumulative_expected_path) && file.exists(cumulative_actual_path)) {
    cap_cumulative <- read_vec(cumulative_expected_path) - read_vec(cumulative_actual_path)
  }

  set.seed(cfg$plot_seed + mc)
  choose_n <- function(idx, n = cfg$plot_cells_per_group, score = NULL, top = FALSE) {
    idx <- unique(idx[is.finite(idx)])
    if (!length(idx)) return(integer())
    n <- min(as.integer(n), length(idx))
    if (top && !is.null(score)) return(idx[order(score[idx], decreasing = TRUE)[seq_len(n)]])
    sample(idx, n)
  }
  sel_group <- function(idx, label, score = NULL, top = FALSE) {
    chosen <- choose_n(idx, score = score, top = top)
    if (!length(chosen)) return(data.table(cell = integer(), group = character()))
    data.table(cell = chosen, group = rep(label, length(chosen)))
  }
  sel <- rbindlist(list(
    sel_group(which(tof == 0 & cap_cumulative > tol), "forest: highest cumulative stock cap", score = cap_cumulative, top = TRUE),
    sel_group(which(tof == 0 & is.finite(initial) & cap_cumulative <= tol), "forest: uncapped control"),
    sel_group(disappeared_tof, "TOF: became NULL"),
    sel_group(zero_supply_reset_tof, "TOF: K=0 reset to 2"),
    sel_group(which(tof == 1 & is.finite(initial) & is.finite(gl1) & K > 0), "TOF: valid control")
  ), fill = TRUE)
  sel <- unique(sel, by = "cell")
  assert_data_table(sel, sprintf("MC%d trajectory selection", mc))
  if (nrow(sel)) {
    xy <- xyFromCell(template, sel$cell)
    sel_cells <- sel$cell
    sel_tof <- as.integer(tof[sel_cells])
    capacity_label <- ifelse(
      sel_tof == 1L,
      "annual TOF K",
      if (model == "chapman-richards") "A" else "K"
    )
    capacity_value <- ifelse(
      sel_tof == 1L,
      K[sel_cells],
      if (model == "chapman-richards") A_cr[sel_cells] else K[sel_cells]
    )
    data.table::set(
      sel,
      j = c(
        "x", "y", "tof", "capacity_label", "capacity_Mg_cell",
        "rmax_per_iteration", "cr_k", "cr_m"
      ),
      value = list(
        xy[, 1], xy[, 2], sel_tof, capacity_label, capacity_value,
        rmax[sel_cells], k_cr[sel_cells], m_cr[sel_cells]
      )
    )
  }

  feedback <- initial
  mechanics_rows <- vector("list", length(years))
  state_rows <- vector("list", length(years))
  cap_rows <- vector("list", length(years))
  stage_rows <- vector("list", length(years))
  trajectory_rows <- vector("list", length(years))

  for (j in seq_along(years)) {
    sfx <- suffixes[[j]]
    year <- years[[j]]
    growth <- read_vec(file.path(dbg_dir, paste0("Growth", sfx, ".tif")))
    actual <- read_vec(file.path(dbg_dir, paste0("Harvest_tot", sfx, ".tif")))
    post <- read_vec(file.path(dbg_dir, paste0("Growth_less_harv", sfx, ".tif")))
    expected <- if (exact_expected) read_vec(e_paths[[j]]) else rep(NA_real_, length(growth))
    prediction <- predict_growth(model, feedback, tof, K, rmax)

    forest_domain <- is.finite(tof) & tof == 0
    forest_growth_stats <- error_stats(growth, prediction, forest_domain)
    # Separate missing selected-model inputs from mechanics failures.  A pixel
    # with finite stock but missing inputs is coverage-only when both the engine
    # and replay return NULL.  Once all selected-model inputs exist, however,
    # both growth values are required and any missing output is a true failure.
    forest_growth_required <- forest_domain & is.finite(feedback)
    forest_selected_model_parameter_missing <- if (model == "logistic") {
      forest_growth_required &
        (!is.finite(K) | (K > 0 & !is.finite(rmax)))
    } else {
      forest_growth_required &
        (!is.finite(A_cr) | A_cr <= 0 |
           !is.finite(k_cr) | k_cr <= 0 |
           !is.finite(m_cr) | m_cr <= 0)
    }
    forest_growth_input_covered <- forest_growth_required &
      !forest_selected_model_parameter_missing
    forest_growth_covered_missing_output <- forest_growth_input_covered &
      (!is.finite(growth) | !is.finite(prediction))

    forest_balance_domain <- is.finite(tof) & tof == 0
    forest_balance_prediction <- growth
    finite_actual <- is.finite(actual)
    forest_balance_prediction[finite_actual] <-
      growth[finite_actual] - actual[finite_actual]
    forest_balance_stats <- error_stats(
      post, forest_balance_prediction, forest_balance_domain
    )
    forest_balance_finite_pattern_mismatch <- forest_balance_domain &
      xor(is.finite(post), is.finite(forest_balance_prediction))
    # The engine falls back to the finite pre-harvest stock when no finite
    # forest harvest is applied, so finite growth must always yield finite post.
    forest_balance_required_missing <- forest_balance_domain &
      is.finite(growth) & !is.finite(post)

    # EGOML v6 preserves TOF stock after pruning: TOF is nondegradable, so its
    # post-harvest identity is simply post = growth.  Expected-versus-realized
    # harvest is checked independently by the Stage-B final-cap identity.
    tof_balance_prediction <- growth
    tof_balance_domain <- is.finite(tof) & tof == 1
    tof_balance_stats <- error_stats(post, tof_balance_prediction, tof_balance_domain)
    tof_balance_finite_pattern_mismatch <- tof_balance_domain &
      xor(is.finite(post), is.finite(tof_balance_prediction))
    # TOF post-stock is likewise required wherever its growth stock is finite.
    tof_balance_required_missing <- tof_balance_domain &
      is.finite(growth) & !is.finite(post)

    final_cap_stats <- data.table(
      domain_cells = NA_integer_, compared_cells = NA_integer_,
      missing_observed_cells = NA_integer_, missing_predicted_cells = NA_integer_,
      mean_abs_error_Mg_cell = NA_real_, max_abs_error_Mg_cell = NA_real_,
      cells_over_tolerance = NA_integer_
    )
    final_cap_active_missing_cells <- NA_integer_
    if (exact_expected) {
      cap_domain <- is.finite(tof)
      predicted_actual <- rep(0, length(growth))
      positive_supply_and_request <- is.finite(expected) & is.finite(growth) &
        expected > 0 & growth > 0
      predicted_actual[positive_supply_and_request] <- pmin(
        expected[positive_supply_and_request], growth[positive_supply_and_request]
      )
      final_cap_stats <- error_stats(actual, predicted_actual, cap_domain)
      cap_active <- cap_domain &
        ((is.finite(expected) & expected > tol) | (is.finite(actual) & actual > tol))
      # EGOML v6 defines NULL/nonpositive request or stock as zero realized harvest.
      final_cap_active_missing_cells <- sum(cap_active & !is.finite(actual))
    }

    forest_growth_increment <- growth - feedback
    mechanics_rows[[j]] <- data.table(
      mc = mc, year = year, suffix = sfx, period = reporting_period(year),
      growth_model = model, units = "MgDM per grid cell",
      forest_growth_compared_cells = forest_growth_stats$compared_cells,
      forest_growth_missing_cells = forest_growth_stats$missing_observed_cells,
      forest_growth_missing_prediction_cells = forest_growth_stats$missing_predicted_cells,
      forest_growth_finite_pattern_mismatch_cells = sum(
        forest_domain & xor(is.finite(growth), is.finite(prediction))
      ),
      forest_growth_required_cells = sum(forest_growth_required),
      forest_growth_input_covered_cells = sum(forest_growth_input_covered),
      forest_growth_covered_missing_output_cells = sum(
        forest_growth_covered_missing_output
      ),
      forest_selected_model_parameter_missing_cells = sum(
        forest_selected_model_parameter_missing
      ),
      forest_growth_mean_abs_error_Mg_cell = forest_growth_stats$mean_abs_error_Mg_cell,
      forest_growth_max_abs_error_Mg_cell = forest_growth_stats$max_abs_error_Mg_cell,
      forest_growth_cells_over_tolerance = forest_growth_stats$cells_over_tolerance,
      forest_regrowth_total_Mg = sum0(forest_growth_increment[tof == 0]),
      forest_balance_compared_cells = forest_balance_stats$compared_cells,
      forest_balance_missing_cells = forest_balance_stats$missing_observed_cells,
      forest_balance_missing_prediction_cells = forest_balance_stats$missing_predicted_cells,
      forest_balance_finite_pattern_mismatch_cells = sum(
        forest_balance_finite_pattern_mismatch
      ),
      forest_balance_required_missing_cells = sum(forest_balance_required_missing),
      forest_balance_max_abs_error_Mg_cell = forest_balance_stats$max_abs_error_Mg_cell,
      forest_balance_cells_over_tolerance = forest_balance_stats$cells_over_tolerance,
      tof_balance_equation = "post = growth (TOF nondegradable)",
      tof_balance_compared_cells = tof_balance_stats$compared_cells,
      tof_balance_missing_cells = tof_balance_stats$missing_observed_cells,
      tof_balance_missing_prediction_cells = tof_balance_stats$missing_predicted_cells,
      tof_balance_finite_pattern_mismatch_cells = sum(
        tof_balance_finite_pattern_mismatch
      ),
      tof_balance_required_missing_cells = sum(tof_balance_required_missing),
      tof_balance_max_abs_error_Mg_cell = tof_balance_stats$max_abs_error_Mg_cell,
      tof_balance_cells_over_tolerance = tof_balance_stats$cells_over_tolerance,
      tof_actual_harvest_Mg = sum0(actual[tof == 1]),
      tof_assigned_replenishment_Mg = if (exact_expected) sum0(expected[tof == 1]) else NA_real_,
      tof_K_minus_rmax_max_abs_difference_Mg_cell = max_safe(abs(K[tof == 1] - rmax[tof == 1]), 0),
      tof_actual_minus_supply_max_Mg_cell = max_safe(pmax(actual[tof == 1] - K[tof == 1], 0), 0),
      tof_expected_minus_supply_max_Mg_cell = if (exact_expected) max_safe(pmax(expected[tof == 1] - K[tof == 1], 0), 0) else NA_real_,
      final_cap_identity_missing_actual_cells = final_cap_stats$missing_observed_cells,
      final_cap_identity_missing_prediction_cells = final_cap_stats$missing_predicted_cells,
      final_cap_identity_active_missing_cells = final_cap_active_missing_cells,
      final_cap_identity_max_abs_error_Mg_cell = final_cap_stats$max_abs_error_Mg_cell,
      final_cap_identity_cells_over_tolerance = final_cap_stats$cells_over_tolerance
    )

    tof_initial_domain <- tof == 1 & is.finite(initial)
    # Effective K constrains only the capped logistic branch. Chapman-Richards
    # uses A/k/m; with finite prior stock, matching NULL growth is recorded as
    # missing-data coverage when any selected Chapman-Richards input is absent.
    forest_zero_K <- model == "logistic" & is.finite(tof) & tof == 0 & is.finite(K) & K <= 0
    tof_zero_K <- is.finite(tof) & tof == 1 & is.finite(K) & K <= 0
    overshoot <- model == "logistic" & is.finite(tof) & tof == 0 &
      is.finite(growth) & is.finite(K) & K > 0 & growth > K + tol
    state_rows[[j]] <- data.table(
      mc = mc, year = year, suffix = sfx, period = reporting_period(year),
      tof_mask_cells = sum(tof == 1, na.rm = TRUE),
      tof_initially_valid_cells = sum(tof_initial_domain),
      tof_post_stock_NULL_cells = sum(tof_initial_domain & !is.finite(post)),
      tof_new_NULL_cells = sum(tof == 1 & is.finite(feedback) & !is.finite(post)),
      tof_NULL_stock_Mg_at_start_of_step = sum0(feedback[tof == 1 & is.finite(feedback) & !is.finite(post)]),
      tof_zero_K_cells = sum(tof_zero_K),
      tof_zero_K_nonzero_growth_cells = sum(tof_zero_K & is.finite(growth) & abs(growth) > tol),
      tof_zero_K_reset_to_2_cells = sum(tof_zero_K & is.finite(growth) & abs(growth - cfg$depleted_reset_Mg_cell) <= tol),
      forest_zero_K_cells = sum(forest_zero_K),
      forest_zero_K_NULL_growth_cells = sum(forest_zero_K & !is.finite(growth)),
      forest_growth_above_K_cells = sum(overshoot),
      forest_growth_excess_above_K_Mg = sum0((growth - K)[overshoot]),
      forest_max_growth_to_K_ratio = max_safe((growth / K)[overshoot])
    )

    expected_total_map <- if (exact_expected) sum0(expected) else NA_real_
    expected_total_aggregate <- demand_by_year[["assigned_expected_total_Mg"]][[j]]
    if (exact_expected) expected_total <- expected_total_map else expected_total <- expected_total_aggregate
    actual_total <- sum0(actual)
    base_cap <- data.table(
      mc = mc, year = year, suffix = sfx, period = reporting_period(year),
      tof_class = "ALL", spatial_detail = exact_expected,
      expected_source = if (exact_expected) "annual expected raster" else "W+V demand-table aggregate proxy",
      expected_Mg = expected_total, actual_Mg = actual_total,
      capped_Mg = expected_total - actual_total,
      capped_percent = pct(expected_total - actual_total, expected_total),
      capped_pixel_years = if (exact_expected) sum(is.finite(expected) & is.finite(actual) & expected - actual > tol) else NA_integer_,
      max_cap_pixel_Mg = if (exact_expected) max_safe(expected - actual, 0) else NA_real_,
      expected_missing_model_cells = if (exact_expected) sum(is.finite(tof) & !is.finite(expected)) else NA_integer_,
      actual_missing_model_cells = sum(is.finite(tof) & !is.finite(actual)),
      expected_map_minus_demand_table_Mg = if (exact_expected) expected_total_map - expected_total_aggregate else NA_real_
    )
    if (exact_expected) {
      by_class <- rbindlist(lapply(0:1, function(zclass) {
        z <- tof == zclass
        es <- sum0(expected[z]); ac <- sum0(actual[z]); d <- expected - actual
        data.table(
          mc = mc, year = year, suffix = sfx, period = reporting_period(year),
          tof_class = sprintf("TOF=%d", zclass), spatial_detail = TRUE,
          expected_source = "annual expected raster",
          expected_Mg = es, actual_Mg = ac, capped_Mg = es - ac,
          capped_percent = pct(es - ac, es),
          capped_pixel_years = sum(z & is.finite(d) & d > tol),
          max_cap_pixel_Mg = max_safe(d[z], 0),
          expected_missing_model_cells = sum(z & !is.finite(expected)),
          actual_missing_model_cells = sum(z & !is.finite(actual)),
          expected_map_minus_demand_table_Mg = NA_real_
        )
      }))
      cap_rows[[j]] <- rbind(base_cap, by_class, fill = TRUE)
    } else cap_rows[[j]] <- base_cap

    if (exact_tof_stage) {
      preliminary <- read_vec(preliminary_paths[[j]])
      shortage <- read_vec(shortage_paths[[j]])
      allocated <- read_vec(allocated_paths[[j]])
      redistributed <- read_vec(redistributed_paths[[j]])
      z <- tof == 1
      p0 <- preliminary; p0[!is.finite(p0)] <- 0
      s0 <- shortage; s0[!is.finite(s0)] <- 0
      a0 <- allocated; a0[!is.finite(a0)] <- 0
      r0 <- redistributed; r0[!is.finite(r0)] <- 0
      supply0 <- K; supply0[!is.finite(supply0)] <- 0
      expected_shortage <- pmax(p0 - supply0, 0)
      expected_allocated <- pmin(p0, supply0)
      stage_rows[[j]] <- data.table(
        mc = mc, year = year, suffix = sfx, period = reporting_period(year),
        source = basename(stage_dir),
        preliminary_tof_request_Mg = sum0(p0[z]),
        tof_supply_capped_and_redirected_Mg = sum0(s0[z]),
        accepted_tof_allocation_Mg = sum0(a0[z]),
        redirected_to_forest_Mg = sum0(r0),
        tof_cap_percent_of_preliminary = pct(sum0(s0[z]), sum0(p0[z])),
        tof_capped_pixel_years = sum(z & s0 > tol),
        max_tof_cap_pixel_Mg = max_safe(s0[z], 0),
        shortage_identity_max_abs_error_Mg_cell = max_safe(abs(s0[z] - expected_shortage[z]), 0),
        allocation_identity_max_abs_error_Mg_cell = max_safe(abs(a0[z] - expected_allocated[z]), 0),
        redistribution_minus_shortage_Mg = sum0(r0) - sum0(s0[z])
      )
    }

    if (nrow(sel)) {
      trajectory_rows[[j]] <- data.table(
        mc = mc, year = year, cell = sel$cell, group = sel$group,
        growth_model = model,
        x = sel$x, y = sel$y, tof = sel$tof,
        capacity_label = sel$capacity_label,
        capacity_Mg_cell = sel$capacity_Mg_cell,
        rmax_per_iteration = sel$rmax_per_iteration,
        cr_k = sel$cr_k, cr_m = sel$cr_m,
        predicted_growth_Mg_cell = prediction[sel$cell],
        observed_growth_Mg_cell = growth[sel$cell],
        expected_harvest_Mg_cell = if (exact_expected) expected[sel$cell] else NA_real_,
        actual_harvest_Mg_cell = actual[sel$cell],
        post_harvest_stock_Mg_cell = post[sel$cell]
      )
    }

    feedback <- post
    # This is the intended forest-only feedback reset.  Applying it to TOFs is
    # audited above as a state-integrity defect.
    reset_forest <- tof == 0 & is.finite(feedback) & feedback <= 0
    feedback[reset_forest] <- cfg$depleted_reset_Mg_cell
  }

  mechanics_annual_all[[as.character(mc)]] <- rbindlist(mechanics_rows, fill = TRUE)
  state_annual_all[[as.character(mc)]] <- rbindlist(state_rows, fill = TRUE)
  capping_annual_all[[as.character(mc)]] <- rbindlist(cap_rows, fill = TRUE)
  if (exact_tof_stage) tof_stage_annual_all[[as.character(mc)]] <- rbindlist(stage_rows, fill = TRUE)
  if (nrow(sel)) trajectory_all[[as.character(mc)]] <- rbindlist(trajectory_rows, fill = TRUE)

  # Exact full-run class totals are retained separately for every MC.
  if (file.exists(cumulative_expected_path) && file.exists(cumulative_actual_path)) {
    cum_e <- read_vec(cumulative_expected_path)
    cum_a <- read_vec(cumulative_actual_path)
    full_period_class_all[[as.character(mc)]] <- rbindlist(lapply(c("ALL", "TOF=0", "TOF=1"), function(label) {
      z <- if (label == "ALL") is.finite(tof) else tof == as.integer(sub("TOF=", "", label))
      e <- sum0(cum_e[z]); a <- sum0(cum_a[z]); d <- cum_e - cum_a
      data.table(
        mc = mc, period = sprintf("%d-%d", start_year, end_year), tof_class = label,
        expected_Mg = e, actual_Mg = a, capped_Mg = e - a,
        capped_percent = pct(e - a, e),
        capped_pixels = sum(z & is.finite(d) & d > tol),
        max_cap_pixel_Mg = max_safe(d[z], 0),
        expected_missing_model_cells = sum(z & !is.finite(cum_e)),
        actual_missing_model_cells = sum(z & !is.finite(cum_a)),
        active_expected_with_missing_actual_cells = sum(z & is.finite(cum_e) & cum_e > tol & !is.finite(cum_a)),
        source = "Temp cumulative per-MC rasters"
      )
    }))
  }

  if (mc == 1L) {
    write_mc1_outreach_plot(
      model = model,
      initial = initial,
      tof = tof,
      K = K,
      rmax = rmax,
      dbg_dir = dbg_dir
    )
  }
}

mechanics_annual <- rbindlist(mechanics_annual_all, fill = TRUE)
state_integrity_annual <- rbindlist(state_annual_all, fill = TRUE)
harvest_capping_annual <- rbindlist(capping_annual_all, fill = TRUE)
full_period_capping_by_tof_class <- if (length(full_period_class_all)) {
  rbindlist(full_period_class_all, fill = TRUE)
} else data.table(
  mc = integer(), period = character(), tof_class = character(),
  expected_Mg = numeric(), actual_Mg = numeric(), capped_Mg = numeric(),
  capped_percent = numeric(), capped_pixels = integer(),
  max_cap_pixel_Mg = numeric(), expected_missing_model_cells = integer(),
  actual_missing_model_cells = integer(),
  active_expected_with_missing_actual_cells = integer(), source = character()
)
tof_preallocation_annual <- if (length(tof_stage_annual_all)) {
  rbindlist(tof_stage_annual_all, fill = TRUE)
} else data.table(
  mc = integer(), year = integer(), suffix = character(), period = character(),
  source = character(), preliminary_tof_request_Mg = numeric(),
  tof_supply_capped_and_redirected_Mg = numeric(),
  accepted_tof_allocation_Mg = numeric(), redirected_to_forest_Mg = numeric(),
  tof_cap_percent_of_preliminary = numeric(), tof_capped_pixel_years = integer(),
  max_tof_cap_pixel_Mg = numeric(),
  shortage_identity_max_abs_error_Mg_cell = numeric(),
  allocation_identity_max_abs_error_Mg_cell = numeric(),
  redistribution_minus_shortage_Mg = numeric()
)
model_detection <- rbindlist(model_detection_all, fill = TRUE)

fwrite(model_detection, file.path(out_dir, "growth_model_detection.csv"))
fwrite(mechanics_annual, file.path(out_dir, "mechanics_annual.csv"))
fwrite(state_integrity_annual, file.path(out_dir, "state_integrity_annual.csv"))
fwrite(harvest_capping_annual, file.path(out_dir, "harvest_capping_annual.csv"))
fwrite(full_period_capping_by_tof_class, file.path(out_dir, "full_period_capping_by_tof_class.csv"))
if (nrow(tof_preallocation_annual)) {
  fwrite(tof_preallocation_annual, file.path(out_dir, "tof_preallocation_capping_annual.csv"))
}

# ---- Period summaries --------------------------------------------------------
harvest_capping_periods <- harvest_capping_annual[, .(
  expected_Mg = sum_complete(expected_Mg),
  actual_Mg = sum_complete(actual_Mg),
  capped_Mg = sum_complete(capped_Mg),
  capped_percent = pct(sum_complete(capped_Mg), sum_complete(expected_Mg)),
  capped_pixel_years = if (all(is.na(capped_pixel_years))) NA_real_ else as.numeric(sum(capped_pixel_years, na.rm = TRUE)),
  max_cap_pixel_Mg = max_safe(max_cap_pixel_Mg),
  expected_missing_model_cell_years = if (all(is.na(expected_missing_model_cells))) NA_real_ else as.numeric(sum(expected_missing_model_cells, na.rm = TRUE)),
  actual_missing_model_cell_years = as.numeric(sum(actual_missing_model_cells, na.rm = TRUE)),
  annual_spatial_detail_complete = all(spatial_detail)
), by = .(mc, period, tof_class)]
fwrite(harvest_capping_periods, file.path(out_dir, "harvest_capping_reporting_periods.csv"))
fwrite(
  harvest_capping_periods[tof_class == "ALL"],
  file.path(out_dir, "harvest_capping_AOI_reporting_periods.csv")
)

tof_preallocation_periods <- data.table()
if (nrow(tof_preallocation_annual)) {
  tof_preallocation_periods <- tof_preallocation_annual[, .(
    preliminary_tof_request_Mg = sum(preliminary_tof_request_Mg),
    tof_supply_capped_and_redirected_Mg = sum(tof_supply_capped_and_redirected_Mg),
    accepted_tof_allocation_Mg = sum(accepted_tof_allocation_Mg),
    redirected_to_forest_Mg = sum(redirected_to_forest_Mg),
    tof_cap_percent_of_preliminary = pct(sum(tof_supply_capped_and_redirected_Mg), sum(preliminary_tof_request_Mg)),
    tof_capped_pixel_years = sum(tof_capped_pixel_years),
    max_tof_cap_pixel_Mg = max_safe(max_tof_cap_pixel_Mg),
    shortage_identity_max_abs_error_Mg_cell = max_safe(shortage_identity_max_abs_error_Mg_cell),
    allocation_identity_max_abs_error_Mg_cell = max_safe(allocation_identity_max_abs_error_Mg_cell),
    redistribution_minus_shortage_Mg = sum(redistribution_minus_shortage_Mg)
  ), by = .(mc, period)]
  fwrite(tof_preallocation_periods, file.path(out_dir, "tof_preallocation_capping_reporting_periods.csv"))
}

# ---- Raw demand / assignment / realization ----------------------------------
raw_demand_assignment_realization <- rbindlist(lapply(mc_ids, function(mc) {
  mc_i <- mc
  stage_b <- harvest_capping_annual[mc == mc_i & tof_class == "ALL", .(
    year,
    stage_B_expected_harvest_Mg = expected_Mg,
    stage_B_expected_source = expected_source,
    realized_harvest_Mg = actual_Mg
  )]
  ans <- merge(demand_by_year, stage_b, by = "year", all.x = TRUE)
  assert_data_table(ans, sprintf("MC%d annual reconciliation", mc))
  data.table::set(
    ans,
    j = c("mc", "final_stock_capped_Mg", "final_stock_capped_percent"),
    value = list(
      rep.int(mc, nrow(ans)),
      ans$stage_B_expected_harvest_Mg - ans$realized_harvest_Mg,
      pct(
        ans$stage_B_expected_harvest_Mg - ans$realized_harvest_Mg,
        ans$stage_B_expected_harvest_Mg
      )
    )
  )
  setcolorder(ans, c("mc", setdiff(names(ans), "mc")))
  ans
}))
fwrite(raw_demand_assignment_realization, file.path(out_dir, "raw_demand_assignment_realization.csv"))

annual_reconciliation <- raw_demand_assignment_realization[, {
  raw_total <- sum_complete(raw_demand_total_Mg)
  assigned_total <- sum_complete(assigned_expected_total_Mg)
  stage_b_expected_total <- sum_complete(stage_B_expected_harvest_Mg)
  realized_total <- sum_complete(realized_harvest_Mg)
  list(
    raw_demand_years_complete = all(raw_demand_complete),
    assigned_demand_years_complete = all(assigned_demand_complete),
    raw_demand_Mg = raw_total,
    annual_proxy_assigned_expected_Mg = assigned_total,
    annual_stage_B_expected_harvest_Mg = stage_b_expected_total,
    annual_stage_B_expected_source = if (
      length(stage_B_expected_source) &&
      all(!is.na(stage_B_expected_source) & stage_B_expected_source == "annual expected raster")
    ) "annual expected rasters" else "annual demand-table aggregate proxy",
    annual_realized_harvest_Mg = realized_total,
    preassignment_gap_Mg = if (all(is.finite(preassignment_gap_Mg))) sum(preassignment_gap_Mg) else NA_real_,
    preassignment_gap_percent = pct(
      if (all(is.finite(preassignment_gap_Mg))) sum(preassignment_gap_Mg) else NA_real_, raw_total
    ),
    annual_stage_B_final_stock_capped_Mg = sum_complete(final_stock_capped_Mg)
  )
}, by = mc]

authoritative_cumulative <- full_period_capping_by_tof_class[tof_class == "ALL", .(
  mc,
  cumulative_expected_Mg = expected_Mg,
  cumulative_realized_Mg = actual_Mg,
  cumulative_final_stock_capped_Mg = capped_Mg,
  cumulative_final_stock_capped_percent = capped_percent,
  cumulative_expected_missing_model_cells = expected_missing_model_cells,
  cumulative_actual_missing_model_cells = actual_missing_model_cells
)]
aggregate_reconciliation <- merge(annual_reconciliation, authoritative_cumulative, by = "mc", all = TRUE)
assert_data_table(aggregate_reconciliation, "aggregate reconciliation")
cumulative_spatial_complete <- with(
  aggregate_reconciliation,
  is.finite(cumulative_expected_Mg) & is.finite(cumulative_realized_Mg) &
    is.finite(cumulative_expected_missing_model_cells) &
    cumulative_expected_missing_model_cells == 0 &
    is.finite(cumulative_actual_missing_model_cells) &
    cumulative_actual_missing_model_cells == 0
)
data.table::set(
  aggregate_reconciliation,
  j = "cumulative_spatial_complete",
  value = cumulative_spatial_complete
)
data.table::set(
  aggregate_reconciliation,
  j = c(
    "authoritative_stage_B_source", "assigned_expected_Mg",
    "stage_B_expected_harvest_Mg", "realized_harvest_Mg",
    "final_stock_capped_Mg", "final_stock_capped_percent",
    "cumulative_minus_annual_expected_Mg",
    "cumulative_minus_assigned_demand_Mg",
    "cumulative_minus_annual_realized_Mg"
  ),
  value = list(
  data.table::fifelse(
    cumulative_spatial_complete,
    "Temp cumulative per-MC rasters",
    data.table::fifelse(
      is.finite(aggregate_reconciliation$cumulative_expected_missing_model_cells) |
        is.finite(aggregate_reconciliation$cumulative_actual_missing_model_cells),
      paste0(
        aggregate_reconciliation$annual_stage_B_expected_source,
        " (cumulative rasters incomplete; partial totals not used)"
      ),
      aggregate_reconciliation$annual_stage_B_expected_source
    )
  ),
  aggregate_reconciliation$annual_proxy_assigned_expected_Mg,
  data.table::fifelse(
    cumulative_spatial_complete,
    aggregate_reconciliation$cumulative_expected_Mg,
    aggregate_reconciliation$annual_stage_B_expected_harvest_Mg
  ),
  data.table::fifelse(
    cumulative_spatial_complete,
    aggregate_reconciliation$cumulative_realized_Mg,
    aggregate_reconciliation$annual_realized_harvest_Mg
  ),
  data.table::fifelse(
    cumulative_spatial_complete,
    aggregate_reconciliation$cumulative_final_stock_capped_Mg,
    aggregate_reconciliation$annual_stage_B_final_stock_capped_Mg
  ),
  data.table::fifelse(
    cumulative_spatial_complete,
    aggregate_reconciliation$cumulative_final_stock_capped_percent,
    pct(
      aggregate_reconciliation$annual_stage_B_final_stock_capped_Mg,
      aggregate_reconciliation$annual_stage_B_expected_harvest_Mg
    )
  ),
  data.table::fifelse(
    cumulative_spatial_complete,
    aggregate_reconciliation$cumulative_expected_Mg -
      aggregate_reconciliation$annual_stage_B_expected_harvest_Mg,
    NA_real_
  ),
  data.table::fifelse(
    cumulative_spatial_complete,
    aggregate_reconciliation$cumulative_expected_Mg -
      aggregate_reconciliation$annual_proxy_assigned_expected_Mg,
    NA_real_
  ),
  data.table::fifelse(
    cumulative_spatial_complete,
    aggregate_reconciliation$cumulative_realized_Mg -
      aggregate_reconciliation$annual_realized_harvest_Mg,
    NA_real_
  )
  )
)
data.table::set(
  aggregate_reconciliation,
  j = "stage_B_expected_minus_assigned_Mg",
  value = aggregate_reconciliation$stage_B_expected_harvest_Mg -
    aggregate_reconciliation$annual_proxy_assigned_expected_Mg
)
fwrite(aggregate_reconciliation, file.path(out_dir, "aggregate_reconciliation.csv"))

# ---- State and mechanics summaries / verdict --------------------------------
state_integrity_summary <- state_integrity_annual[, .(
  tof_mask_cells = max(tof_mask_cells),
  tof_initially_valid_cells = max(tof_initially_valid_cells),
  max_tof_post_stock_NULL_cells = max(tof_post_stock_NULL_cells),
  total_tof_new_NULL_pixel_years = sum(tof_new_NULL_cells),
  first_step_tof_stock_lost_to_NULL_Mg = tof_NULL_stock_Mg_at_start_of_step[[1L]],
  tof_zero_K_cells = max(tof_zero_K_cells),
  max_tof_zero_K_reset_to_2_cells = max(tof_zero_K_reset_to_2_cells),
  forest_zero_K_cells = max(forest_zero_K_cells),
  max_forest_zero_K_NULL_growth_cells = max(forest_zero_K_NULL_growth_cells),
  total_forest_growth_above_K_pixel_years = sum(forest_growth_above_K_cells),
  total_forest_growth_excess_above_K_Mg = sum(forest_growth_excess_above_K_Mg),
  max_forest_growth_to_K_ratio = max_safe(forest_max_growth_to_K_ratio)
), by = mc]
fwrite(state_integrity_summary, file.path(out_dir, "state_integrity_summary.csv"))

mechanics_summary <- mechanics_annual[, .(
  growth_model = unique(growth_model)[1L],
  max_forest_growth_error_Mg_cell = max_safe(forest_growth_max_abs_error_Mg_cell),
  forest_growth_missing_cells = sum(forest_growth_missing_cells, na.rm = TRUE),
  forest_growth_missing_prediction_cells = sum(forest_growth_missing_prediction_cells, na.rm = TRUE),
  forest_growth_finite_pattern_mismatch_cells = sum(
    forest_growth_finite_pattern_mismatch_cells, na.rm = TRUE
  ),
  max_forest_growth_required_cells = max(forest_growth_required_cells),
  max_forest_growth_input_covered_cells = max(forest_growth_input_covered_cells),
  max_forest_growth_covered_missing_output_cells = max(
    forest_growth_covered_missing_output_cells
  ),
  total_forest_growth_covered_missing_output_pixel_years = sum(
    forest_growth_covered_missing_output_cells, na.rm = TRUE
  ),
  max_forest_selected_model_parameter_missing_cells = max(
    forest_selected_model_parameter_missing_cells
  ),
  forest_growth_cells_over_tolerance = sum(forest_growth_cells_over_tolerance, na.rm = TRUE),
  max_forest_balance_error_Mg_cell = max_safe(forest_balance_max_abs_error_Mg_cell),
  forest_balance_missing_cells = sum(forest_balance_missing_cells, na.rm = TRUE),
  forest_balance_missing_prediction_cells = sum(forest_balance_missing_prediction_cells, na.rm = TRUE),
  forest_balance_finite_pattern_mismatch_cells = sum(
    forest_balance_finite_pattern_mismatch_cells, na.rm = TRUE
  ),
  max_forest_balance_required_missing_cells = max(forest_balance_required_missing_cells),
  total_forest_balance_required_missing_pixel_years = sum(
    forest_balance_required_missing_cells, na.rm = TRUE
  ),
  forest_balance_cells_over_tolerance = sum(forest_balance_cells_over_tolerance, na.rm = TRUE),
  max_tof_balance_error_Mg_cell = max_safe(tof_balance_max_abs_error_Mg_cell),
  tof_balance_missing_cells = sum(tof_balance_missing_cells, na.rm = TRUE),
  tof_balance_missing_prediction_cells = sum(tof_balance_missing_prediction_cells, na.rm = TRUE),
  tof_balance_finite_pattern_mismatch_cells = sum(
    tof_balance_finite_pattern_mismatch_cells, na.rm = TRUE
  ),
  max_tof_balance_required_missing_cells = max(tof_balance_required_missing_cells),
  total_tof_balance_required_missing_pixel_years = sum(
    tof_balance_required_missing_cells, na.rm = TRUE
  ),
  tof_balance_cells_over_tolerance = sum(tof_balance_cells_over_tolerance, na.rm = TRUE),
  max_final_cap_identity_error_Mg_cell = max_safe(final_cap_identity_max_abs_error_Mg_cell),
  final_cap_identity_missing_actual_cells = sum_complete(final_cap_identity_missing_actual_cells),
  final_cap_identity_missing_prediction_cells = sum_complete(final_cap_identity_missing_prediction_cells),
  final_cap_identity_active_missing_cells = sum_complete(final_cap_identity_active_missing_cells),
  final_cap_identity_cells_over_tolerance = sum_complete(final_cap_identity_cells_over_tolerance),
  max_tof_K_minus_rmax_difference_Mg_cell = max_safe(tof_K_minus_rmax_max_abs_difference_Mg_cell),
  max_tof_actual_above_supply_Mg_cell = max_safe(tof_actual_minus_supply_max_Mg_cell),
  max_tof_expected_above_supply_Mg_cell = max_safe(tof_expected_minus_supply_max_Mg_cell)
), by = mc]
mechanics_summary <- merge(mechanics_summary, state_integrity_summary, by = "mc", all = TRUE)
mechanics_summary <- merge(mechanics_summary, aggregate_reconciliation, by = "mc", all = TRUE)

stage_a_summary <- rbindlist(lapply(mc_ids, function(mc_i) {
  x <- tof_preallocation_annual[mc == mc_i]
  if (!nrow(x)) {
    return(data.table(
      mc = mc_i, stage_A_status = "NOT AUDITED",
      stage_A_max_shortage_identity_error_Mg_cell = NA_real_,
      stage_A_max_allocation_identity_error_Mg_cell = NA_real_,
      stage_A_max_redistribution_error_Mg = NA_real_
    ))
  }
  max_short <- max_safe(x$shortage_identity_max_abs_error_Mg_cell)
  max_alloc <- max_safe(x$allocation_identity_max_abs_error_Mg_cell)
  max_redist <- max_safe(abs(x$redistribution_minus_shortage_Mg))
  status <- if (any(c(max_short, max_alloc, max_redist) > tol, na.rm = TRUE)) "FAIL" else "PASS"
  data.table(
    mc = mc_i, stage_A_status = status,
    stage_A_max_shortage_identity_error_Mg_cell = max_short,
    stage_A_max_allocation_identity_error_Mg_cell = max_alloc,
    stage_A_max_redistribution_error_Mg = max_redist
  )
}))

stage_b_summary <- rbindlist(lapply(mc_ids, function(mc_i) {
  cumulative_row <- full_period_capping_by_tof_class[mc == mc_i & tof_class == "ALL"]
  have_cumulative <- nrow(cumulative_row) == 1L
  cumulative_expected_missing <- if (have_cumulative) {
    cumulative_row$expected_missing_model_cells[[1L]]
  } else NA_real_
  cumulative_actual_missing <- if (have_cumulative) {
    cumulative_row$actual_missing_model_cells[[1L]]
  } else NA_real_
  cumulative_status <- if (!have_cumulative) {
    "NOT AUDITED"
  } else if (isTRUE(cumulative_expected_missing > 0) ||
             isTRUE(cumulative_actual_missing > 0) ||
             isTRUE(cumulative_row$active_expected_with_missing_actual_cells > 0) ||
             isTRUE(cumulative_row$capped_Mg < -tol)) {
    "FAIL"
  } else "PASS"
  have_annual_spatial <- isTRUE(expected_provenance[mc == mc_i, annual_pixel_detail][[1L]])
  annual_rows <- harvest_capping_annual[mc == mc_i & tof_class == "ALL"]
  annual_expected_missing <- if (have_annual_spatial) {
    sum_complete(annual_rows$expected_missing_model_cells)
  } else NA_real_
  annual_actual_missing <- sum_complete(annual_rows$actual_missing_model_cells)
  m <- mechanics_summary[mc == mc_i]
  annual_status <- if (!have_annual_spatial) {
    "NOT AUDITED"
  } else if (any(c(
    annual_expected_missing > 0,
    annual_actual_missing > 0,
    m$max_final_cap_identity_error_Mg_cell > tol,
    m$final_cap_identity_cells_over_tolerance > 0,
    m$final_cap_identity_missing_actual_cells > 0
  ), na.rm = TRUE)) {
    "FAIL"
  } else "PASS"
  data.table(
    mc = mc_i,
    stage_B_full_run_aggregate_status = cumulative_status,
    stage_B_annual_spatial_status = annual_status,
    stage_B_cumulative_expected_missing_model_cells = cumulative_expected_missing,
    stage_B_cumulative_actual_missing_model_cells = cumulative_actual_missing,
    stage_B_annual_expected_missing_model_cell_years = annual_expected_missing,
    stage_B_annual_actual_missing_model_cell_years = annual_actual_missing
  )
}))

mechanics_summary <- merge(mechanics_summary, stage_a_summary, by = "mc", all = TRUE)
mechanics_summary <- merge(mechanics_summary, stage_b_summary, by = "mc", all = TRUE)
assert_data_table(mechanics_summary, "mechanics summary")
over_tolerance <- function(x) is.finite(x) & x > tol
over_zero <- function(x) is.finite(x) & x > 0
data.table::set(
  mechanics_summary,
  j = "critical_mechanics_failure",
  value = with(
    mechanics_summary,
    over_tolerance(max_forest_growth_error_Mg_cell) |
      over_zero(forest_growth_finite_pattern_mismatch_cells) |
      over_zero(total_forest_growth_covered_missing_output_pixel_years) |
      over_tolerance(max_forest_balance_error_Mg_cell) |
      over_zero(forest_balance_finite_pattern_mismatch_cells) |
      over_zero(total_forest_balance_required_missing_pixel_years) |
      over_tolerance(max_tof_balance_error_Mg_cell) |
      over_zero(tof_balance_finite_pattern_mismatch_cells) |
      over_zero(total_tof_balance_required_missing_pixel_years) |
      over_tolerance(max_tof_K_minus_rmax_difference_Mg_cell) |
      over_tolerance(max_tof_actual_above_supply_Mg_cell) |
      over_tolerance(max_tof_expected_above_supply_Mg_cell) |
      over_tolerance(max_final_cap_identity_error_Mg_cell) |
      over_zero(stage_B_annual_actual_missing_model_cell_years) |
      over_zero(final_cap_identity_active_missing_cells)
  )
)
data.table::set(
  mechanics_summary,
  j = "tof_state_failure",
  value = with(
    mechanics_summary,
    over_zero(max_tof_post_stock_NULL_cells) |
      over_zero(max_tof_zero_K_reset_to_2_cells)
  )
)
data.table::set(
  mechanics_summary,
  j = "mechanics_diagnostics_incomplete",
  value = with(
    mechanics_summary,
    stage_A_status == "NOT AUDITED" |
      stage_B_full_run_aggregate_status == "NOT AUDITED" |
      stage_B_annual_spatial_status == "NOT AUDITED"
  )
)
data.table::set(
  mechanics_summary,
  j = "demand_reconciliation_incomplete",
  value = with(
    mechanics_summary,
    is.na(raw_demand_years_complete) | !raw_demand_years_complete |
      is.na(assigned_demand_years_complete) | !assigned_demand_years_complete
  )
)
data.table::set(
  mechanics_summary,
  j = "audit_incomplete",
  value = mechanics_summary$mechanics_diagnostics_incomplete |
    mechanics_summary$demand_reconciliation_incomplete
)
data.table::set(
  mechanics_summary,
  j = "parameter_coverage_missing_data",
  value = over_zero(
    mechanics_summary$max_forest_selected_model_parameter_missing_cells
  )
)
data.table::set(
  mechanics_summary,
  j = "state_parameter_warning",
  value = with(
    mechanics_summary,
    over_zero(forest_zero_K_cells) |
      over_zero(total_forest_growth_above_K_pixel_years)
  )
)
data.table::set(
  mechanics_summary,
  j = "verdict",
  value = with(mechanics_summary, data.table::fifelse(
    critical_mechanics_failure | tof_state_failure |
      stage_A_status == "FAIL" | stage_B_full_run_aggregate_status == "FAIL" |
      stage_B_annual_spatial_status == "FAIL",
    "FAIL - inspect mechanics/state findings",
    data.table::fifelse(
      mechanics_diagnostics_incomplete,
      "INCOMPLETE - required mechanics diagnostics were unavailable",
      data.table::fifelse(
        demand_reconciliation_incomplete,
        data.table::fifelse(
          state_parameter_warning,
          "PASS MECHANICS WITH WARNINGS / INCOMPLETE DEMAND RECONCILIATION",
          "PASS MECHANICS / INCOMPLETE DEMAND RECONCILIATION"
        ),
        data.table::fifelse(state_parameter_warning, "PASS WITH STATE/PARAMETER WARNINGS", "PASS")
      )
    )
  ))
)
fwrite(mechanics_summary, file.path(out_dir, "mechanics_summary.csv"))

verification_coverage <- rbindlist(list(
  stage_a_summary[, .(mc, check = "Stage A: TOF supply cap and redistribution", status = stage_A_status)],
  stage_b_summary[, .(mc, check = "Stage B: full-run aggregate cumulative capping", status = stage_B_full_run_aggregate_status)],
  stage_b_summary[, .(mc, check = "Stage B: annual pixel-level capping", status = stage_B_annual_spatial_status)],
  mechanics_summary[, .(
    mc, check = "Raw-demand coverage",
    status = data.table::fifelse(!is.na(raw_demand_years_complete) & raw_demand_years_complete, "PASS", "NOT AUDITED")
  )],
  mechanics_summary[, .(
    mc, check = "Assigned-demand table coverage",
    status = data.table::fifelse(!is.na(assigned_demand_years_complete) & assigned_demand_years_complete, "PASS", "NOT AUDITED")
  )],
  mechanics_summary[, .(
    mc, check = "Selected growth-model input coverage",
    status = data.table::fifelse(
      parameter_coverage_missing_data,
      "MISSING DATA (not a mechanics failure)",
      "COMPLETE"
    )
  )]
))
fwrite(stage_a_summary, file.path(out_dir, "stage_A_verification_summary.csv"))
fwrite(stage_b_summary, file.path(out_dir, "stage_B_verification_summary.csv"))
fwrite(verification_coverage, file.path(out_dir, "verification_coverage.csv"))

# ---- Scaling/source-code audit ----------------------------------------------
is_1km <- all(abs(resolution_xy - 1000) < 1e-6)
is_mercator <- grepl("merc|3395", crs(template, proj = TRUE), ignore.case = TRUE)
expected_map_demand_error <- max_safe(abs(
  harvest_capping_annual[tof_class == "ALL" & spatial_detail == TRUE,
                         expected_map_minus_demand_table_Mg]
))
raw_demand_complete <- all(demand_by_year$raw_demand_complete)
assigned_demand_complete <- all(demand_by_year$assigned_demand_complete)
demand_complete <- raw_demand_complete && assigned_demand_complete
expected_map_coverage_complete <- all(expected_provenance$annual_pixel_detail)
core_unit_reconciled <- expected_map_coverage_complete &&
  is.finite(expected_map_demand_error) && expected_map_demand_error < 1
area_bias_percent <- pct(nominal_area_ha - geodesic_mean_ha, geodesic_mean_ha)
scaling_audit <- data.table(
  check = c(
    "native_core_units", "one_km_nominal_area", "table_K_and_TOF_scaling",
    "demand_total_mass", "AOI_nominal_vs_geodesic_area",
    "global_World_Mercator_area_consistency", "harvest_pixel_MC_upper_bound",
    "CTrees_time_step", "maps_animations_presentation_units"
  ),
  status = c(
    if (!expected_map_coverage_complete || !is.finite(expected_map_demand_error)) {
      "NOT FULLY AUDITED"
    } else if (core_unit_reconciled) "RECONCILED" else "CHECK",
    if (is_1km) "PASS" else "CHECK", "SOURCE-CODE INFERENCE",
    if (demand_complete && core_unit_reconciled) "PASS" else "CHECK",
    if (!is.finite(area_bias_percent)) "NOT AUDITED" else if (abs(area_bias_percent) < 1) "PASS (sub-percent)" else "CHECK",
    if (is_mercator) "STRUCTURAL WARNING" else "CHECK",
    "UNIT BUG", if (is.na(iteration_weeks)) "NOT AUDITED" else if (iteration_weeks == 48) "PASS FOR THIS RUN" else "CHECK",
    "PRESENTATION WARNING"
  ),
  evidence = c(
    sprintf("Per-MC annual expected-map coverage complete=%s; max expected-map versus demand-table |difference|=%s Mg", expected_map_coverage_complete, ifelse(is.finite(expected_map_demand_error), sprintf("%.6f", expected_map_demand_error), "not available")),
    sprintf("Grid is %.0f x %.0f m; nominal projected area %.3f ha/cell", resolution_xy[1], resolution_xy[2], nominal_area_ha),
    sprintf("Source audit: rnorm_v3.R uses resolution^2/10000 = %.3f ha/cell: forest K and annual TOF supply are multiplied by this factor; forest r is not. This row documents code behavior rather than independently re-deriving every input raster.", nominal_area_ha),
    sprintf("Raw-demand years complete=%s; assigned W/V demand years complete=%s; max expected-map versus demand-table difference=%s Mg", raw_demand_complete, assigned_demand_complete, ifelse(is.finite(expected_map_demand_error), sprintf("%.6f", expected_map_demand_error), "not available")),
    sprintf("Geodesic area within this AOI mask: %.3f-%.3f ha, mean %.3f ha; nominal mean bias %.3f%%", geodesic_min_ha, geodesic_max_ha, geodesic_mean_ha, area_bias_percent),
    "Table K/TOF use a fixed nominal cell area while harmonized AGB/CTrees A use geodesic source-cell area. EPSG:3395 is not equal-area, so the mismatch grows strongly with latitude.",
    "rnorm_v3.R converts LULC pixel frequencies to hectares and then uses those hectare totals as upper bounds for Harv.Pix, which is a pixel count. Keep pixel_count and area_ha separate.",
    sprintf("Current Chapman-Richards implementation advances age by +1 per iteration; iteration length detected as %s weeks", ifelse(is.na(iteration_weeks), "unknown", format(iteration_weeks))),
    "maps_animations7.R divides native pixel totals by a scalar nominal area and labels t/ha. For pixel-total reporting, do not divide; for density reporting, use a cell-area raster globally."
  )
)
fwrite(scaling_audit, file.path(out_dir, "scaling_audit.csv"))

# ---- Publication-quality diagnostic trajectory plots ------------------------
if (length(trajectory_all)) {
  for (mc_name in names(trajectory_all)) {
    tr <- trajectory_all[[mc_name]]
    if (!nrow(tr)) next
    assert_data_table(tr, sprintf("MC%s trajectory plot data", mc_name))
    data.table::set(
      tr,
      j = "parameter_text",
      value = with(tr, data.table::fcase(
        tof == 1L,
        sprintf("annual TOF K=%.1f Mg/cell", capacity_Mg_cell),
        growth_model == "chapman-richards" &
          (!is.finite(capacity_Mg_cell) | capacity_Mg_cell <= 0 |
             !is.finite(cr_k) | cr_k <= 0 | !is.finite(cr_m) | cr_m <= 0),
        "Chapman-Richards inputs missing (coverage only)",
        growth_model == "chapman-richards",
        sprintf("A=%.1f Mg/cell; k=%.4f; m=%.3f", capacity_Mg_cell, cr_k, cr_m),
        growth_model == "logistic" &
          (!is.finite(capacity_Mg_cell) |
             (capacity_Mg_cell > 0 & !is.finite(rmax_per_iteration))),
        "Logistic inputs missing (coverage only)",
        default = sprintf("K=%.1f Mg/cell; r=%.4f", capacity_Mg_cell, rmax_per_iteration)
      ))
    )
    data.table::set(
      tr,
      j = "panel",
      value = with(
        tr,
        sprintf("%s | cell %d | TOF=%d\n%s", group, cell, tof, parameter_text)
      )
    )
    data.table::set(
      tr,
      j = "panel",
      value = factor(tr$panel, levels = unique(tr$panel))
    )

    harvest_data <- melt(
      tr,
      id.vars = c("panel", "year"),
      measure.vars = c("expected_harvest_Mg_cell", "actual_harvest_Mg_cell"),
      variable.name = "harvest_series", value.name = "Mg_cell"
    )
    assert_data_table(harvest_data, sprintf("MC%s harvest plot data", mc_name))
    harvest_labels <- c(
      expected_harvest_Mg_cell = "assigned/expected harvest map",
      actual_harvest_Mg_cell = "realized harvest"
    )
    data.table::set(
      harvest_data,
      j = "harvest_series",
      value = factor(
        harvest_labels[as.character(harvest_data$harvest_series)],
        levels = unname(harvest_labels)
      )
    )
    harvest_data <- harvest_data[is.finite(Mg_cell)]

    line_ready <- function(column) {
      tr[, if (sum(is.finite(get(column))) >= 2L) .SD else NULL, by = panel]
    }
    post_line <- line_ready("post_harvest_stock_Mg_cell")
    observed_line <- line_ready("observed_growth_Mg_cell")
    predicted_line <- line_ready("predicted_growth_Mg_cell")

    harvest_fills <- c(
      "assigned/expected harvest map" = "#CC79A7",
      "realized harvest" = "#E69F00"
    )
    stock_colours <- c(
      "post-harvest stock" = "#009E73",
      "observed pre-harvest stock" = "#0072B2",
      "predicted pre-harvest stock" = "#D55E00"
    )
    stock_linetypes <- c(
      "post-harvest stock" = "solid",
      "observed pre-harvest stock" = "solid",
      "predicted pre-harvest stock" = "dotted"
    )

    p <- ggplot() +
      # Establish every facet and anchor its free y-scale at zero.  Harvest is
      # drawn first as two thin side-by-side flows, never connected as lines.
      geom_blank(data = tr, aes(year, 0)) +
      geom_col(
        data = harvest_data,
        aes(year, Mg_cell, fill = harvest_series),
        position = position_dodge2(width = 0.60, preserve = "single", padding = 0.25),
        width = 0.60, alpha = 0.55, colour = NA, na.rm = TRUE
      ) +
      # Explicit layer order matters: the prediction is last so exact matches
      # remain visible as red dots over the solid observed line.
      geom_line(
        data = post_line,
        aes(year, post_harvest_stock_Mg_cell,
            colour = "post-harvest stock", linetype = "post-harvest stock"),
        linewidth = 0.65, na.rm = TRUE
      ) +
      geom_line(
        data = observed_line,
        aes(year, observed_growth_Mg_cell,
            colour = "observed pre-harvest stock", linetype = "observed pre-harvest stock"),
        linewidth = 0.72, na.rm = TRUE
      ) +
      geom_line(
        data = predicted_line,
        aes(year, predicted_growth_Mg_cell,
            colour = "predicted pre-harvest stock", linetype = "predicted pre-harvest stock"),
        linewidth = 0.82, lineend = "round", na.rm = TRUE
      ) +
      facet_wrap(~panel, scales = "free_y", ncol = 2) +
      scale_fill_manual(
        values = harvest_fills, limits = names(harvest_fills),
        name = NULL, drop = FALSE
      ) +
      scale_colour_manual(
        values = stock_colours, limits = names(stock_colours),
        name = NULL, drop = FALSE
      ) +
      scale_linetype_manual(
        values = stock_linetypes, limits = names(stock_linetypes),
        name = NULL, drop = FALSE
      ) +
      labs(
        x = "Year", y = "Stock or harvest (MgDM per grid cell)",
        title = sprintf(
          "MoFuSS pixel mechanics - MC%s (%s)",
          mc_name, unique(tr$growth_model)[[1L]]
        ),
        subtitle = paste(
          "Thin bars: assigned/expected and realized harvest.",
          "Lines: post-harvest, observed pre-harvest, then dotted prediction on top.",
          "Each panel includes zero; gaps indicate NULL cells."
        )
      ) +
      theme_bw(base_size = 9) +
      theme(
        legend.position = "top", legend.text = element_text(size = 7.5),
        legend.box = "vertical",
        strip.background = element_blank(), strip.text = element_text(size = 7),
        plot.title = element_text(face = "bold")
      ) +
      guides(
        fill = guide_legend(order = 1, nrow = 1, byrow = TRUE),
        colour = guide_legend(order = 2, nrow = 1, byrow = TRUE),
        linetype = guide_legend(order = 2, nrow = 1, byrow = TRUE)
      )
    plot_height <- max(7.5, 2.4 * ceiling(uniqueN(tr$panel) / 2))
    ggsave(
      file.path(out_dir, sprintf("pixel_trajectories_MC%s.png", mc_name)),
      p, width = 11.7, height = plot_height, units = "in", dpi = 300,
      bg = "white", limitsize = FALSE
    )
  }
}

# ---- Evidence provenance -----------------------------------------------------
# Pass/fail is derived from raster replay.  Root log/debug files are recorded so
# stale or mismatched engine references cannot be mistaken for mechanics proof.
mechanics_evidence_files <- unlist(lapply(mc_ids, function(mc) {
  dbg <- file.path(wd, sprintf("debugging_%d", mc))
  unlist(lapply(
    c("Growth", "Harvest_tot", "Growth_less_harv"),
    function(prefix) file.path(dbg, paste0(prefix, suffixes, ".tif"))
  ), use.names = FALSE)
}), use.names = FALSE)
mechanics_evidence_info <- file.info(mechanics_evidence_files)
mechanics_mtimes <- mechanics_evidence_info$mtime[!is.na(mechanics_evidence_info$mtime)]
raster_mtime_first <- if (length(mechanics_mtimes)) min(mechanics_mtimes) else as.POSIXct(NA)
raster_mtime_last <- if (length(mechanics_mtimes)) max(mechanics_mtimes) else as.POSIXct(NA)
format_timestamp <- function(x) {
  if (length(x) && !is.na(x[[1L]])) {
    format(x[[1L]], "%Y-%m-%d %H:%M:%S %Z")
  } else NA_character_
}

auxiliary_provenance <- function(filename, inspect_engine_reference = FALSE) {
  path <- file.path(wd, filename)
  exists <- file.exists(path)
  mtime <- if (exists) file.info(path)$mtime[[1L]] else as.POSIXct(NA)
  older_than_rasters <- exists && !is.na(raster_mtime_first) &&
    !is.na(mtime) && mtime < raster_mtime_first
  engine_reference <- NA_character_
  version_mismatch <- FALSE
  if (exists && inspect_engine_reference) {
    text <- readLines(path, warn = FALSE)
    reference_lines <- trimws(grep("\\.egoml", text, value = TRUE, ignore.case = TRUE))
    if (length(reference_lines)) {
      engine_reference <- substr(paste(head(reference_lines, 3L), collapse = " | "), 1L, 1000L)
      version_mismatch <- any(grepl("v5\\.egoml", reference_lines, ignore.case = TRUE))
    }
  }
  flags <- c(
    if (older_than_rasters) "STALE",
    if (version_mismatch) "VERSION-MISMATCH"
  )
  status <- if (!exists) {
    "NOT PRESENT / NOT USED"
  } else if (length(flags)) {
    paste0(paste(flags, collapse = " + "), " / NOT USED")
  } else "INFORMATIONAL ONLY / NOT USED"
  detail <- if (!is.na(engine_reference) && nzchar(engine_reference)) {
    paste0("Engine reference: ", engine_reference)
  } else {
    "Excluded from mechanics and engine-version evidence; raster replay is authoritative."
  }
  data.table(
    source = filename, exists = exists,
    first_modified = format_timestamp(mtime), last_modified = format_timestamp(mtime),
    status = status, used_for_verdict = FALSE, detail = detail
  )
}

detected_models <- paste(sort(unique(model_detection$selected_model)), collapse = ", ")
verification_provenance <- rbindlist(list(
  data.table(
    source = "annual mechanics raster replay", exists = TRUE,
    first_modified = format_timestamp(raster_mtime_first),
    last_modified = format_timestamp(raster_mtime_last),
    status = "AUTHORITATIVE FOR THIS VERIFICATION", used_for_verdict = TRUE,
    detail = sprintf(
      "All model-domain pixels; MC=%s; years=%d-%d; raster-detected model(s)=%s",
      paste(mc_ids, collapse = ","), start_year, end_year, detected_models
    )
  ),
  auxiliary_provenance("log.txt", inspect_engine_reference = TRUE),
  auxiliary_provenance("debug.txt")
), fill = TRUE)
fwrite(verification_provenance, file.path(out_dir, "verification_provenance.csv"))

# ---- Human-readable report ---------------------------------------------------
overall_fail <- any(grepl("^FAIL", mechanics_summary$verdict), na.rm = TRUE)
overall_mechanics_incomplete <-
  any(is.na(mechanics_summary$mechanics_diagnostics_incomplete)) ||
  any(mechanics_summary$mechanics_diagnostics_incomplete, na.rm = TRUE)
overall_demand_incomplete <-
  any(is.na(mechanics_summary$demand_reconciliation_incomplete)) ||
  any(mechanics_summary$demand_reconciliation_incomplete, na.rm = TRUE)
overall_warning <- any(mechanics_summary$state_parameter_warning, na.rm = TRUE)
overall_parameter_missing_data <- any(
  mechanics_summary$parameter_coverage_missing_data, na.rm = TRUE
)
overall_verdict <- if (overall_fail) {
  "FAIL - localized state/mechanics defects were detected"
} else if (overall_mechanics_incomplete) {
  "INCOMPLETE - required mechanics diagnostics were unavailable"
} else if (overall_demand_incomplete && overall_warning) {
  "PASS MECHANICS WITH WARNINGS / INCOMPLETE DEMAND RECONCILIATION"
} else if (overall_demand_incomplete) {
  "PASS MECHANICS / INCOMPLETE DEMAND RECONCILIATION"
} else if (overall_warning) {
  "PASS WITH STATE/PARAMETER WARNINGS"
} else "PASS"
parameter_coverage_line <- if (overall_parameter_missing_data) {
  "PARAMETER COVERAGE: MISSING DATA PRESENT (reported separately; not a mechanics failure)"
} else {
  "PARAMETER COVERAGE: COMPLETE for all finite-stock model-domain pixels"
}

fmt_number <- function(x, digits = 3L) {
  if (length(x) && is.finite(x[[1L]])) {
    formatC(x[[1L]], format = "f", digits = digits, big.mark = ",")
  } else "not available"
}
fmt_percent <- function(x, digits = 3L) {
  if (length(x) && is.finite(x[[1L]])) {
    paste0(fmt_number(x, digits), "%")
  } else "not available"
}
format_mc_list <- function(ids) paste0("MC", ids, collapse = ", ")

missing_stage_a <- stage_a_summary[stage_A_status == "NOT AUDITED", mc]
missing_stage_b_full <- stage_b_summary[stage_B_full_run_aggregate_status == "NOT AUDITED", mc]
missing_stage_b_annual <- stage_b_summary[stage_B_annual_spatial_status == "NOT AUDITED", mc]
missing_raw_years <- demand_by_year[!raw_demand_complete, year]
missing_assigned_years <- demand_by_year[!assigned_demand_complete, year]

completeness_lines <- "Verification-input completeness:"
if (!length(c(missing_stage_a, missing_stage_b_full, missing_stage_b_annual))) {
  completeness_lines <- c(
    completeness_lines,
    sprintf(
      "  All per-MC mechanics diagnostics are complete for %s: Stage A, Stage B annual pixel maps, and Stage B cumulative rasters; no additional verifier diagnostics need to be saved.",
      format_mc_list(mc_ids)
    )
  )
} else {
  if (length(missing_stage_a) || length(missing_stage_b_annual)) {
    completeness_lines <- c(
      completeness_lines,
      sprintf("  Shared Debugging ownership inference: %s.", shared_owner_inference)
    )
  }
  if (length(missing_stage_a)) {
    completeness_lines <- c(completeness_lines, sprintf(
      "  %s: save Non_harv_AGRNN.tif, Proj_harv_WtotNN.tif, harv_AGRNN.tif, and Ex_agr_harvNN.tif in each matching debugging_MC directory for Stage A.",
      format_mc_list(missing_stage_a)
    ))
  }
  if (length(missing_stage_b_annual)) {
    completeness_lines <- c(completeness_lines, sprintf(
      "  %s: save Expect_harv_totNN.tif in each matching debugging_MC directory for annual Stage B pixel/class summaries.",
      format_mc_list(missing_stage_b_annual)
    ))
  }
  if (length(missing_stage_b_full)) {
    completeness_lines <- c(completeness_lines, sprintf(
      "  %s: retain Temp/2_EXP_CON_TOTNN.tif and Temp/2_CON_TOTNN.tif for full-run Stage B totals.",
      format_mc_list(missing_stage_b_full)
    ))
  }
}
if (!length(c(missing_raw_years, missing_assigned_years))) {
  completeness_lines <- c(
    completeness_lines,
    "  Raw-demand rasters and assigned W/V demand tables are complete for every reporting year."
  )
}
if (length(missing_raw_years)) {
  completeness_lines <- c(completeness_lines, sprintf(
    "  Raw-demand source rasters were unavailable or invalid for: %s.",
    paste(missing_raw_years, collapse = ", ")
  ))
}
if (length(missing_assigned_years)) {
  completeness_lines <- c(completeness_lines, sprintf(
    "  Assigned W/V demand tables were unavailable or invalid for: %s.",
    paste(missing_assigned_years, collapse = ", ")
  ))
}

scaling_conclusion_lines <- if (core_unit_reconciled) {
  c(
    sprintf(
      "  No factor-of-100 mismatch was detected: complete expected-harvest maps reconcile with assigned Mg demand to a maximum absolute difference of %s Mg.",
      fmt_number(expected_map_demand_error, 6L)
    ),
    "  Some scaling rows remain source-code inferences rather than independent end-to-end proofs."
  )
} else if (!expected_map_coverage_complete || !is.finite(expected_map_demand_error)) {
  "  The factor-of-100 reconciliation is NOT FULLY AUDITED because complete per-MC expected-harvest map coverage is unavailable."
} else {
  sprintf(
    "  CHECK the unit reconciliation: the maximum expected-map versus assigned-demand difference is %s Mg, above the 1 Mg audit threshold.",
    fmt_number(expected_map_demand_error, 6L)
  )
}
area_conclusion_line <- sprintf(
  "  Within this AOI, nominal cell area differs from mean geodesic cell area by %s. %s",
  fmt_percent(area_bias_percent, 3L),
  if (is_mercator) {
    "World Mercator is not equal-area, so this bias varies with latitude."
  } else {
    "Use the geodesic cell-area raster when reporting spatial densities."
  }
)

raster_provenance <- verification_provenance[source == "annual mechanics raster replay"][1L]
log_provenance <- verification_provenance[source == "log.txt"][1L]
debug_provenance <- verification_provenance[source == "debug.txt"][1L]
provenance_lines <- c(
  "Evidence provenance:",
  sprintf("  Raster replay: %s. %s.", raster_provenance$status, raster_provenance$detail),
  "  Every model-domain pixel contributes to the mechanics verdict; sampled cells are used only in the trajectory figures.",
  sprintf(
    "  log.txt: %s; modified=%s. %s",
    log_provenance$status,
    ifelse(is.na(log_provenance$first_modified), "not available", log_provenance$first_modified),
    log_provenance$detail
  ),
  sprintf(
    "  debug.txt: %s; modified=%s. %s",
    debug_provenance$status,
    ifelse(is.na(debug_provenance$first_modified), "not available", debug_provenance$first_modified),
    debug_provenance$detail
  )
)

report_lines <- c(
  "MoFuSS mechanics verification",
  "==============================",
  sprintf("Run: %s", wd),
  sprintf("Years: %d-%d; MC realizations: %s", start_year, end_year, paste(mc_ids, collapse = ", ")),
  sprintf("Native units: Mg dry matter per grid cell; nominal area %.3f ha/cell", nominal_area_ha),
  sprintf("Float32 tolerance: %.4f Mg/cell", tol),
  sprintf("OVERALL VERDICT: %s", overall_verdict),
  parameter_coverage_line,
  "",
  provenance_lines,
  "",
  "Interpretation of the two harvest constraints:",
  "  Stage A - TOF annual-supply constraint: Non_harv_AGR is the preliminary TOF request above annual TOF K. It is redistributed to forests and is not unmet demand.",
  "  Stage B - final standing-stock constraint: expected-harvest map minus realized Harvest_tot. This is genuinely unmet harvest caused by insufficient stock.",
  "  Raw demand, assigned W/V demand tables, and expected-harvest map totals are reported separately; raw-minus-assigned is not standing-stock capping.",
  sprintf("  Shared dynamic gain/loss/deforestation diagnostic total: %s (zero means the static recurrence is sufficient for this run).",
          ifelse(is.na(dynamic_event_total), "not available", format(dynamic_event_total, scientific = FALSE))),
  "",
  "Per-MC aggregate reconciliation:"
)
for (i in seq_len(nrow(aggregate_reconciliation))) {
  x <- aggregate_reconciliation[i]
  report_lines <- c(report_lines, sprintf(
    "  MC%d: raw demand %s Mg; assigned demand %s Mg; preassignment gap %s Mg (%s); Stage B expected %s Mg; realized %s Mg; final stock cap %s Mg (%s); Stage B expected minus assigned %s Mg; Stage B source=%s",
    x$mc, fmt_number(x$raw_demand_Mg), fmt_number(x$assigned_expected_Mg),
    fmt_number(x$preassignment_gap_Mg), fmt_percent(x$preassignment_gap_percent, 4L),
    fmt_number(x$stage_B_expected_harvest_Mg), fmt_number(x$realized_harvest_Mg),
    fmt_number(x$final_stock_capped_Mg), fmt_percent(x$final_stock_capped_percent, 4L),
    fmt_number(x$stage_B_expected_minus_assigned_Mg),
    x$authoritative_stage_B_source
  ))
}
report_lines <- c(report_lines, "", "Verification coverage:")
for (i in seq_len(nrow(mechanics_summary))) {
  x <- mechanics_summary[i]
  report_lines <- c(report_lines, sprintf(
    "  MC%d: raster-detected model=%s; Stage A=%s; Stage B full-run aggregate=%s; Stage B annual spatial=%s; raw demand=%s; assigned demand=%s; MC verdict=%s",
    x$mc, x$growth_model, x$stage_A_status, x$stage_B_full_run_aggregate_status,
    x$stage_B_annual_spatial_status,
    if (isTRUE(x$raw_demand_years_complete)) "PASS" else "NOT AUDITED",
    if (isTRUE(x$assigned_demand_years_complete)) "PASS" else "NOT AUDITED",
    x$verdict
  ))
}
report_lines <- c(report_lines, "", "Growth mechanics and selected-model input coverage:")
for (i in seq_len(nrow(mechanics_summary))) {
  x <- mechanics_summary[i]
  report_lines <- c(report_lines, sprintf(
    paste0(
      "  MC%d: maximum annual finite-stock forest cells=%.0f; ",
      "selected-model inputs available=%.0f; missing inputs=%.0f ",
      "(MISSING DATA, not a mechanics failure); covered cells with missing ",
      "growth output=%.0f; unexpected finite/NULL mismatches=%.0f; ",
      "max numerical growth error=%s Mg/cell."
    ),
    x$mc, x$max_forest_growth_required_cells,
    x$max_forest_growth_input_covered_cells,
    x$max_forest_selected_model_parameter_missing_cells,
    x$max_forest_growth_covered_missing_output_cells,
    x$forest_growth_finite_pattern_mismatch_cells,
    fmt_number(x$max_forest_growth_error_Mg_cell, 6L)
  ))
}
report_lines <- c(report_lines, "", "Post-harvest output completeness:")
for (i in seq_len(nrow(mechanics_summary))) {
  x <- mechanics_summary[i]
  report_lines <- c(report_lines, sprintf(
    paste0(
      "  MC%d: max finite-growth forest cells missing post-stock=%.0f; ",
      "max finite-growth TOF cells missing post-stock=%.0f; ",
      "unexpected forest/TOF post-stock finite-pattern mismatches=%.0f/%.0f; ",
      "annual expected-map missing model-cell-years=%s; ",
      "model-domain Harvest_tot missing pixel-years=%s; ",
      "cumulative expected/actual missing model cells=%s/%s."
    ),
    x$mc, x$max_forest_balance_required_missing_cells,
    x$max_tof_balance_required_missing_cells,
    x$forest_balance_finite_pattern_mismatch_cells,
    x$tof_balance_finite_pattern_mismatch_cells,
    fmt_number(x$stage_B_annual_expected_missing_model_cell_years, 0L),
    fmt_number(x$stage_B_annual_actual_missing_model_cell_years, 0L),
    fmt_number(x$stage_B_cumulative_expected_missing_model_cells, 0L),
    fmt_number(x$stage_B_cumulative_actual_missing_model_cells, 0L)
  ))
}
report_lines <- c(report_lines, "", "State-integrity findings:")
for (i in seq_len(nrow(state_integrity_summary))) {
  x <- state_integrity_summary[i]
  report_lines <- c(report_lines, sprintf(
    "  MC%d: max missing TOF post-stock cells=%d; first-step TOF stock lost=%.3f Mg; K=0 TOFs reset to 2=%d; finite forest K<=0 cells=%d; growth-above-K pixel-years=%.0f.",
    x$mc, x$max_tof_post_stock_NULL_cells, x$first_step_tof_stock_lost_to_NULL_Mg,
    x$max_tof_zero_K_reset_to_2_cells, x$forest_zero_K_cells,
    x$total_forest_growth_above_K_pixel_years
  ))
}
report_lines <- c(
  report_lines, "",
  completeness_lines,
  "",
  "Scaling conclusion:",
  scaling_conclusion_lines,
  area_conclusion_line,
  "",
  sprintf("Machine-readable tables and 300-dpi trajectory figures: %s", out_dir)
)
writeLines(report_lines, file.path(out_dir, "verification_report.txt"))

cat(paste(report_lines, collapse = "\n"), "\n")
message("DONE: ", out_dir)
invisible(out_dir)
}

run_failures <- character()
for (selected_working_dir in cfg$working_dirs) {
  message("\n=== MoFuSS mechanics verifier: ", selected_working_dir, " ===")
  failure <- tryCatch(
    {
      run_verifier(selected_working_dir, cfg)
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  if (!is.null(failure)) {
    run_failures <- c(
      run_failures,
      sprintf("%s: %s", selected_working_dir, failure)
    )
    message("FAILED: ", selected_working_dir, "\n  ", failure)
  }
}

if (length(run_failures)) {
  stop(
    paste(
      "One or more working folders failed verification:",
      paste0("  - ", run_failures, collapse = "\n"),
      sep = "\n"
    ),
    call. = FALSE
  )
}
