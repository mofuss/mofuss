#!/usr/bin/env Rscript

# Prepare CTrees--MoFuSS AGB validation data on fixed, explicitly documented
# comparison supports.  This stage does not interpret CTrees losses as fNRB.
# It creates reproducible regional, country, and 50-km block aggregates for
# the capped and uncapped BaU structural configurations across every MC run.

suppressPackageStartupMessages({
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required.", call. = FALSE)
  }
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
    if (!grepl("=", z, fixed = TRUE)) {
      out[[z]] <- TRUE
    } else {
      p <- regexpr("=", z, fixed = TRUE)
      key <- substr(z, 1L, p - 1L)
      val <- substr(z, p + 1L, nchar(z))
      if (identical(key, "workdir")) out[[key]] <- c(out[[key]], val) else out[[key]] <- val
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

as_num <- function(x, default) {
  if (is.null(x) || !nzchar(x)) return(as.numeric(default))
  z <- suppressWarnings(as.numeric(x))
  if (!is.finite(z)) stop("Expected a finite number, received: ", x, call. = FALSE)
  z
}

normalize_slash <- function(x) gsub("\\\\", "/", x)

parse_run_metadata <- function(run_dir) {
  nm <- basename(normalize_slash(run_dir))
  lc <- tolower(nm)
  mc_hit <- regmatches(lc, regexpr("mc[0-9]+", lc))
  spin_hit <- regmatches(lc, regexpr("spinup[0-9]+", lc))
  scenario <- if (grepl("(^|_)bau[0-9]*(_|$)", lc)) "bau" else if (grepl("(^|_)ics[0-9]*(_|$)", lc)) "ics" else NA_character_
  cap_status <- if (grepl("(^|_)uncapped(_|$)", lc)) "uncapped" else if (grepl("(^|_)capped(_|$)", lc)) "capped" else NA_character_
  data.frame(
    run_dir = normalize_slash(run_dir), run_name = nm, scenario = scenario,
    cap_status = cap_status,
    mc_expected = if (length(mc_hit) && nzchar(mc_hit)) as.integer(sub("mc", "", mc_hit)) else NA_integer_,
    spinup_years = if (length(spin_hit) && nzchar(spin_hit)) as.integer(sub("spinup", "", spin_hit)) else NA_integer_,
    stringsAsFactors = FALSE
  )
}

list_mc_dirs <- function(run_dir, expected_n) {
  dirs <- list.dirs(run_dir, recursive = FALSE, full.names = TRUE)
  dirs <- dirs[grepl("^debugging_[0-9]+$", basename(dirs))]
  ids <- suppressWarnings(as.integer(sub("^debugging_", "", basename(dirs))))
  ord <- order(ids)
  dirs <- normalize_slash(dirs[ord])
  ids <- ids[ord]
  if (!length(dirs)) stop("No debugging_N MC directories found under ", run_dir, call. = FALSE)
  if (!is.na(expected_n) && length(dirs) != expected_n) {
    stop("Expected ", expected_n, " MC directories under ", run_dir, "; found ", length(dirs), ".", call. = FALSE)
  }
  if (!identical(ids, seq_len(length(ids)))) stop("MC directory IDs are not consecutive from 1 under ", run_dir, call. = FALSE)
  data.frame(mc_id = ids, mc_dir = dirs, stringsAsFactors = FALSE)
}

same_geometry <- function(x, y) {
  terra::compareGeom(x, y, stopOnError = FALSE, crs = TRUE, ext = TRUE, rowcol = TRUE, res = TRUE)
}

read_model_values <- function(path, ref) {
  if (!file.exists(path)) stop("Missing model raster: ", path, call. = FALSE)
  r <- terra::rast(path)
  if (!same_geometry(r, ref)) stop("Model raster geometry differs from the reference: ", path, call. = FALSE)
  as.numeric(terra::values(r, mat = FALSE))
}

align_observation <- function(path, ref, obs_type) {
  if (!file.exists(path)) stop("Missing CTrees raster: ", path, call. = FALSE)
  r <- terra::rast(path)
  if (terra::nlyr(r) != 1L) stop("CTrees raster must have one band: ", path, call. = FALSE)
  if (!nzchar(terra::crs(r))) stop("CTrees raster has no CRS: ", path, call. = FALSE)
  if (terra::same.crs(r, ref)) {
    if (!same_geometry(r, ref)) {
      r <- terra::crop(r, terra::ext(ref), snap = "out")
      r <- terra::resample(r, ref, method = "bilinear")
    }
  } else {
    r <- terra::project(r, ref, method = "bilinear")
  }
  r
}

append_rows <- function(store, row) {
  store[[length(store) + 1L]] <- row
  store
}

safe_bind <- function(x) {
  if (!length(x)) return(data.frame())
  do.call(rbind, x)
}

weighted_metrics <- function(start, end) {
  delta <- end - start
  c(
    start_agb_Mg = sum(start), end_agb_Mg = sum(end), delta_agb_Mg = sum(delta),
    delta_percent = if (sum(start) != 0) 100 * sum(delta) / sum(start) else NA_real_,
    gross_loss_Mg = sum(pmax(-delta, 0)), gross_gain_Mg = sum(pmax(delta, 0))
  )
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

file_record <- function(path, role) {
  fi <- file.info(path)
  data.frame(
    role = role, path = normalize_slash(path), exists = file.exists(path),
    bytes = if (file.exists(path)) unname(fi$size) else NA_real_,
    modified_utc = if (file.exists(path)) format(fi$mtime, tz = "UTC", usetz = TRUE) else NA_character_,
    stringsAsFactors = FALSE
  )
}

opts <- parse_args(commandArgs(trailingOnly = TRUE))
workdirs <- unique(normalize_slash(opts$workdir))
if (length(workdirs) != 4L) stop("Provide exactly four --workdir arguments: BaU/ICS crossed with capped/uncapped.", call. = FALSE)

obs_type <- tolower(if (is.null(opts[["obs-type"]])) "projected" else opts[["obs-type"]])
if (!obs_type %in% c("projected", "latlong")) stop("--obs-type must be projected or latlong.", call. = FALSE)
obs_dir <- normalize_slash(opts[["obs-dir"]])
admin_path <- normalize_slash(opts[["admin"]])
post_dir <- normalize_slash(opts[["postprocessing-dir"]])
staging_root <- if (is.null(opts[["staging-root"]])) NA_character_ else normalize_slash(opts[["staging-root"]])
hydro_path <- normalize_slash(opts[["hydro"]])
obs_start <- as_int(opts[["obs-start"]], 2000L)
obs_end <- as_int(opts[["obs-end"]], 2025L)
model_start <- as_int(opts[["model-start"]], 2000L)
primary_start <- as_int(opts[["primary-start"]], 2010L)
primary_end <- as_int(opts[["primary-end"]], 2020L)
block_size_km <- as_num(opts[["block-size-km"]], 50)
min_block_cells <- as_int(opts[["min-block-cells"]], 100L)
carbon_fraction <- as_num(opts[["carbon-fraction"]], 0.47)
exclude_hydrolakes <- as_flag(opts[["exclude-hydrolakes"]], TRUE)
ctrees_recent_comparable <- as_flag(opts[["ctrees-recent-comparable"]], FALSE)
dry_run <- as_flag(opts[["dry-run"]], FALSE)
overwrite <- as_flag(opts[["overwrite"]], FALSE)

required <- c(obs_dir, admin_path, post_dir)
if (any(!nzchar(required))) stop("--obs-dir, --admin, and --postprocessing-dir are required.", call. = FALSE)
if (exclude_hydrolakes && !nzchar(hydro_path)) stop("--hydro is required when --exclude-hydrolakes=true.", call. = FALSE)
if (obs_end <= obs_start) stop("Observation end year must be later than start year.", call. = FALSE)
if (primary_start < obs_start || primary_end > obs_end || primary_end <= primary_start) stop("Primary period lies outside the observation period.", call. = FALSE)
if (carbon_fraction <= 0 || carbon_fraction >= 1) stop("Carbon fraction must lie between 0 and 1.", call. = FALSE)

meta <- safe_bind(lapply(workdirs, parse_run_metadata))
if (anyNA(meta$scenario) || anyNA(meta$cap_status)) stop("Could not parse scenario/cap status from all run-directory names.", call. = FALSE)
combos <- paste(meta$scenario, meta$cap_status, sep = ":")
expected_combos <- c("bau:capped", "bau:uncapped", "ics:capped", "ics:uncapped")
if (!setequal(combos, expected_combos) || anyDuplicated(combos)) stop("Run directories must represent exactly BaU/ICS crossed with capped/uncapped.", call. = FALSE)
if (length(unique(stats::na.omit(meta$mc_expected))) != 1L) stop("All run directories must declare the same MC count.", call. = FALSE)
expected_mc <- unique(stats::na.omit(meta$mc_expected))[[1L]]
if (expected_mc < 2L) stop("At least two MC runs are required for uncertainty intervals.", call. = FALSE)

bau <- meta[meta$scenario == "bau", , drop = FALSE]
bau <- bau[match(c("capped", "uncapped"), bau$cap_status), , drop = FALSE]
ics <- meta[meta$scenario == "ics", , drop = FALSE]
mc_sets <- setNames(lapply(seq_len(nrow(bau)), function(i) list_mc_dirs(bau$run_dir[[i]], bau$mc_expected[[i]])), bau$cap_status)

years <- seq.int(obs_start, obs_end)
periods <- unique(data.frame(
  period_id = c(sprintf("%d_%d", primary_start, primary_end), "2010_2025", "2000_2025", "2000_2010", "2020_2025"),
  start_year = c(primary_start, 2010L, 2000L, 2000L, 2020L),
  end_year = c(primary_end, 2025L, 2025L, 2010L, 2025L),
  stringsAsFactors = FALSE
))
periods <- periods[periods$start_year >= obs_start & periods$end_year <= obs_end & periods$end_year > periods$start_year, , drop = FALSE]
periods$is_primary <- periods$start_year == primary_start & periods$end_year == primary_end
periods$role <- ifelse(periods$is_primary, "primary", ifelse(periods$period_id == "2010_2025", "extended_recent_sensitivity", ifelse(periods$period_id == "2000_2025", "full_record_sensitivity", ifelse(periods$period_id == "2000_2010", "spinup_diagnostic", "late_period_diagnostic"))))
periods <- periods[order(!periods$is_primary, periods$start_year, periods$end_year), , drop = FALSE]

model_path <- function(mc_dir, year) normalize_slash(file.path(mc_dir, sprintf("Growth_less_harv%02d.tif", year - model_start + 1L)))
obs_path <- function(year) normalize_slash(file.path(obs_dir, sprintf("ctrees_%d_agb_MgDM_ha.tif", year)))

ref_path <- model_path(mc_sets$capped$mc_dir[[1L]], obs_start)
if (!file.exists(ref_path)) stop("Reference raster not found: ", ref_path, call. = FALSE)
if (!file.exists(admin_path)) stop("Administrative boundary file not found: ", admin_path, call. = FALSE)
if (!dir.exists(obs_dir)) stop("Observation directory not found: ", obs_dir, call. = FALSE)
if (exclude_hydrolakes && !file.exists(hydro_path)) stop("HydroLakes file not found: ", hydro_path, call. = FALSE)
missing_obs <- vapply(years, function(y) !file.exists(obs_path(y)), logical(1))
if (any(missing_obs)) stop("Missing CTrees rasters for years: ", paste(years[missing_obs], collapse = ", "), call. = FALSE)

cat("AGB VALIDATION PREPARATION V2\n")
cat("BaU capped:   ", bau$run_dir[bau$cap_status == "capped"], "\n", sep = "")
cat("BaU uncapped: ", bau$run_dir[bau$cap_status == "uncapped"], "\n", sep = "")
cat("ICS inputs excluded from empirical comparison by design: ", paste(ics$run_name, collapse = ", "), "\n", sep = "")
cat("CTrees: ", obs_dir, " (", obs_type, ")\n", sep = "")
cat("Observation years: ", obs_start, "--", obs_end, "; primary period: ", primary_start, "--", primary_end, "\n", sep = "")
cat("CTrees 2024--2025 comparability confirmed: ", ctrees_recent_comparable, "\n", sep = "")
cat("Output root: ", post_dir, "\n", sep = "")

if (dry_run) {
  for (cfg in names(mc_sets)) {
    for (d in mc_sets[[cfg]]$mc_dir) {
      for (y in years) if (!file.exists(model_path(d, y))) stop("Missing model raster: ", model_path(d, y), call. = FALSE)
    }
  }
  ref <- terra::rast(ref_path)
  invisible(align_observation(obs_path(obs_start), ref, obs_type))
  invisible(terra::vect(admin_path))
  if (exclude_hydrolakes) {
    if (tolower(tools::file_ext(hydro_path)) %in% c("tif", "tiff", "img", "vrt")) invisible(terra::rast(hydro_path)) else invisible(terra::vect(hydro_path))
  }
  cat("DRY RUN PASSED: all declared inputs and raster families were found; representative spatial reads succeeded.\n")
  quit(save = "no", status = 0L)
}

validation_root <- normalize_slash(file.path(post_dir, "validation"))
final_dir <- normalize_slash(file.path(validation_root, "2_agb_consistency_preparation_v2"))
if (is.na(staging_root) || !nzchar(staging_root)) staging_root <- validation_root
if (!dir.exists(staging_root)) dir.create(staging_root, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(staging_root)) stop("Could not create staging root: ", staging_root, call. = FALSE)
stage_dir <- normalize_slash(file.path(staging_root, paste0(".2_agb_consistency_preparation_v2_stage_", Sys.getpid())))
completed_stage_dir <- normalize_slash(file.path(staging_root, "2_agb_consistency_preparation_v2"))
if (overwrite) {
  guarded_remove_directory(final_dir, validation_root, basename(final_dir))
  guarded_remove_directory(completed_stage_dir, staging_root, basename(completed_stage_dir))
  stale <- list.dirs(staging_root, recursive = FALSE, full.names = TRUE)
  stale <- stale[grepl("^\\.2_agb_consistency_preparation_v2_stage_[0-9]+$", basename(stale))]
  if (length(stale)) for (path in stale) guarded_remove_directory(path, staging_root, basename(path))
}
if (dir.exists(final_dir)) stop("Refusing to overwrite existing versioned output: ", final_dir, call. = FALSE)
if (dir.exists(stage_dir)) stop("Unexpected staging directory already exists: ", stage_dir, call. = FALSE)
if (dir.exists(completed_stage_dir)) stop("Refusing to overwrite completed temporary product: ", completed_stage_dir, call. = FALSE)
dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(stage_dir)) stop("Could not create staging directory: ", stage_dir, call. = FALSE)

ref <- terra::rast(ref_path)
ncell_ref <- terra::ncell(ref)
ground_area_ha <- as.numeric(terra::values(terra::cellSize(ref, unit = "ha", mask = FALSE), mat = FALSE))

admin <- terra::vect(admin_path)
if (!terra::same.crs(admin, ref)) admin <- terra::project(admin, terra::crs(ref))
admin_fields <- names(admin)
gid_field <- if ("GID_0" %in% admin_fields) "GID_0" else if ("ISO3" %in% admin_fields) "ISO3" else stop("Administrative data need a GID_0 or ISO3 field.", call. = FALSE)
name_field <- if ("NAME_0" %in% admin_fields) "NAME_0" else gid_field
admin_df <- as.data.frame(admin)
all_country_ids <- sort(unique(as.character(admin_df[[gid_field]])))
all_country_ids <- all_country_ids[!is.na(all_country_ids) & nzchar(all_country_ids)]
if (!length(all_country_ids)) stop("No country identifiers found in administrative data.", call. = FALSE)
all_country_names <- vapply(all_country_ids, function(z) {
  hit <- which(as.character(admin_df[[gid_field]]) == z)
  as.character(admin_df[[name_field]][hit[[1L]]])
}, character(1))
country_index <- match(as.character(admin_df[[gid_field]]), all_country_ids)
admin$validation_country_code <- country_index
country_r <- terra::rasterize(admin, ref, field = "validation_country_code", background = NA, touches = FALSE)
country_cell <- as.integer(round(as.numeric(terra::values(country_r, mat = FALSE))))
reference_model_values <- as.numeric(terra::values(ref, mat = FALSE))
reference_model_domain <- is.finite(reference_model_values) & reference_model_values >= 0
present_country_codes <- sort(unique(country_cell[is.finite(country_cell) & reference_model_domain]))
if (!length(present_country_codes)) stop("Administrative countries do not overlap the model grid.", call. = FALSE)
country_ids <- all_country_ids[present_country_codes]
country_names <- all_country_names[present_country_codes]
country_cell <- match(country_cell, present_country_codes)
analysis_land <- is.finite(country_cell) & reference_model_domain

if (exclude_hydrolakes) {
  hydro_ext <- tolower(tools::file_ext(hydro_path))
  if (hydro_ext %in% c("tif", "tiff", "img", "vrt")) {
    hydro_r <- terra::rast(hydro_path)
    if (terra::nlyr(hydro_r) != 1L) stop("HydroLakes raster must have one band: ", hydro_path, call. = FALSE)
    if (!terra::same.crs(hydro_r, ref)) hydro_r <- terra::project(hydro_r, ref, method = "near") else if (!same_geometry(hydro_r, ref)) hydro_r <- terra::resample(hydro_r, ref, method = "near")
  } else {
    hydro <- terra::vect(hydro_path)
    if (!terra::same.crs(hydro, ref)) hydro <- terra::project(hydro, terra::crs(ref))
    hydro_r <- terra::rasterize(hydro, ref, field = 1, background = 0, touches = TRUE)
  }
  hydro_values <- as.numeric(terra::values(hydro_r, mat = FALSE))
  hydro_cell <- is.finite(hydro_values) & hydro_values > 0
  analysis_land <- analysis_land & !hydro_cell
}

units <- rbind(
  data.frame(unit_id = "GOG", unit_name = "Gulf of Guinea", unit_level = "region", country_code = NA_integer_, stringsAsFactors = FALSE),
  data.frame(unit_id = country_ids, unit_name = country_names, unit_level = "country", country_code = seq_along(country_ids), stringsAsFactors = FALSE)
)

unit_cells <- function(support, country_code) {
  if (is.na(country_code)) support else support & country_cell == country_code
}

make_country_index_set <- function(support) {
  lapply(seq_along(country_ids), function(code) which(support & country_cell == code))
}

obs_mass <- setNames(vector("list", length(years)), as.character(years))
obs_valid <- setNames(vector("list", length(years)), as.character(years))
cat("Reading and aligning CTrees annual AGB rasters...\n")
for (y in years) {
  r <- align_observation(obs_path(y), ref, obs_type)
  v <- as.numeric(terra::values(r, mat = FALSE))
  if (obs_type == "latlong") v <- v * (12 / 44) / carbon_fraction
  ok <- analysis_land & is.finite(v) & v >= 0 & is.finite(ground_area_ha) & ground_area_ha > 0
  obs_mass[[as.character(y)]] <- v * ground_area_ha
  obs_valid[[as.character(y)]] <- ok
}

cat("Constructing fixed all-year and endpoint comparison supports across all BaU MC realizations...\n")
trajectory_support <- analysis_land
for (y in years) trajectory_support <- trajectory_support & obs_valid[[as.character(y)]]
period_support <- setNames(lapply(seq_len(nrow(periods)), function(i) {
  obs_valid[[as.character(periods$start_year[[i]])]] & obs_valid[[as.character(periods$end_year[[i]])]] & analysis_land
}), periods$period_id)

for (cfg in names(mc_sets)) {
  cat("  validity scan: ", cfg, "\n", sep = "")
  for (j in seq_len(nrow(mc_sets[[cfg]]))) {
    d <- mc_sets[[cfg]]$mc_dir[[j]]
    for (y in years) {
      v <- read_model_values(model_path(d, y), ref)
      ok <- is.finite(v) & v >= 0
      trajectory_support <- trajectory_support & ok
      hits <- which(periods$start_year == y | periods$end_year == y)
      if (length(hits)) for (i in hits) period_support[[periods$period_id[[i]]]] <- period_support[[periods$period_id[[i]]]] & ok
    }
  }
}

if (!any(trajectory_support)) stop("The all-year common support is empty.", call. = FALSE)
if (any(vapply(period_support, function(x) !any(x), logical(1)))) stop("At least one endpoint common support is empty.", call. = FALSE)
trajectory_country_counts <- tabulate(country_cell[trajectory_support], nbins = length(country_ids))
if (any(trajectory_country_counts == 0L)) {
  stop("The all-year common support is empty for: ", paste(country_ids[trajectory_country_counts == 0L], collapse = ", "), call. = FALSE)
}

nr <- terra::nrow(ref)
nc <- terra::ncol(ref)
xstep <- abs(terra::xres(ref))
ystep <- abs(terra::yres(ref))
block_cols <- max(1L, as.integer(round(block_size_km * 1000 / xstep)))
block_rows <- max(1L, as.integer(round(block_size_km * 1000 / ystep)))
row_index <- rep(seq_len(nr), each = nc)
col_index <- rep.int(seq_len(nc), times = nr)
block_row <- (row_index - 1L) %/% block_rows + 1L
block_col <- (col_index - 1L) %/% block_cols + 1L
block_key_cell <- ifelse(analysis_land, country_cell * 100000000 + block_row * 10000 + block_col, NA_real_)
block_keys <- sort(unique(block_key_cell[is.finite(block_key_cell)]))
block_id_cell <- match(block_key_cell, block_keys)
first_cell <- match(block_keys, block_key_cell)
xy <- terra::xyFromCell(ref, first_cell)
block_lookup <- data.frame(
  block_id = seq_along(block_keys), block_key = format(block_keys, scientific = FALSE, trim = TRUE),
  unit_id = country_ids[country_cell[first_cell]], unit_name = country_names[country_cell[first_cell]],
  representative_x = xy[, 1L], representative_y = xy[, 2L],
  block_size_km = block_size_km, stringsAsFactors = FALSE
)
nblocks <- nrow(block_lookup)

eligible_blocks <- setNames(vector("list", nrow(periods)), periods$period_id)
period_block_cells <- setNames(vector("list", nrow(periods)), periods$period_id)
for (i in seq_len(nrow(periods))) {
  pid <- periods$period_id[[i]]
  counts <- tabulate(block_id_cell[period_support[[pid]]], nbins = nblocks)
  eligible_blocks[[pid]] <- counts >= min_block_cells
  period_block_cells[[pid]] <- which(
    period_support[[pid]] & is.finite(block_id_cell) & eligible_blocks[[pid]][block_id_cell]
  )
}

trajectory_index_set <- make_country_index_set(trajectory_support)
period_index_sets <- setNames(lapply(period_support, make_country_index_set), names(period_support))

aggregate_units <- function(start, end, country_indices, source, config, mc_id, period_id = NA_character_, year = NA_integer_) {
  country_metrics <- lapply(country_indices, function(cells) weighted_metrics(start[cells], end[cells]))
  country_metrics <- do.call(rbind, country_metrics)
  region_metrics <- colSums(country_metrics[, setdiff(colnames(country_metrics), "delta_percent"), drop = FALSE])
  region_metrics <- c(
    region_metrics[c("start_agb_Mg", "end_agb_Mg", "delta_agb_Mg")],
    delta_percent = if (region_metrics[["start_agb_Mg"]] != 0) 100 * region_metrics[["delta_agb_Mg"]] / region_metrics[["start_agb_Mg"]] else NA_real_,
    region_metrics[c("gross_loss_Mg", "gross_gain_Mg")]
  )
  all_metrics <- rbind(region_metrics, country_metrics)
  counts <- c(sum(lengths(country_indices)), lengths(country_indices))
  areas <- c(
    sum(vapply(country_indices, function(cells) sum(ground_area_ha[cells]), numeric(1))),
    vapply(country_indices, function(cells) sum(ground_area_ha[cells]), numeric(1))
  )
  rows <- vector("list", nrow(units))
  for (u in seq_len(nrow(units))) {
    m <- all_metrics[u, ]
    rows[[u]] <- data.frame(
      source = source, config = config, mc_id = mc_id, unit_id = units$unit_id[[u]],
      unit_name = units$unit_name[[u]], unit_level = units$unit_level[[u]], period_id = period_id,
      year = year, cell_count = counts[[u]], support_area_ha = areas[[u]],
      start_agb_Mg = unname(m[["start_agb_Mg"]]), end_agb_Mg = unname(m[["end_agb_Mg"]]),
      delta_agb_Mg = unname(m[["delta_agb_Mg"]]), delta_percent = unname(m[["delta_percent"]]),
      gross_loss_Mg = unname(m[["gross_loss_Mg"]]), gross_gain_Mg = unname(m[["gross_gain_Mg"]]),
      stringsAsFactors = FALSE
    )
  }
  safe_bind(rows)
}

aggregate_trajectory <- function(values, country_indices, source, config, mc_id, year) {
  country_agb <- vapply(country_indices, function(cells) sum(values[cells]), numeric(1))
  country_counts <- lengths(country_indices)
  country_areas <- vapply(country_indices, function(cells) sum(ground_area_ha[cells]), numeric(1))
  agb <- c(sum(country_agb), country_agb)
  counts <- c(sum(country_counts), country_counts)
  areas <- c(sum(country_areas), country_areas)
  data.frame(
    source = source, config = config, mc_id = mc_id, unit_id = units$unit_id,
    unit_name = units$unit_name, unit_level = units$unit_level, period_id = NA_character_,
    year = year, cell_count = counts, support_area_ha = areas,
    start_agb_Mg = 0, end_agb_Mg = agb, delta_agb_Mg = agb, delta_percent = NA_real_,
    gross_loss_Mg = 0, gross_gain_Mg = agb, stringsAsFactors = FALSE
  )
}

aggregate_blocks <- function(start, end, cells, eligible, source, config, mc_id, period_id) {
  ids <- block_id_cell[cells]
  sum_by_id <- function(values) {
    out <- numeric(nblocks)
    sums <- rowsum(values, group = ids, reorder = FALSE, na.rm = FALSE)
    out[as.integer(rownames(sums))] <- sums[, 1L]
    out
  }
  start_sum <- sum_by_id(start[cells])
  end_sum <- sum_by_id(end[cells])
  area_sum <- sum_by_id(ground_area_ha[cells])
  count_sum <- tabulate(ids, nbins = nblocks)
  present <- which(eligible & count_sum >= min_block_cells & area_sum > 0)
  delta <- end_sum[present] - start_sum[present]
  data.frame(
    source = source, config = config, mc_id = mc_id, period_id = period_id,
    block_id = present, unit_id = block_lookup$unit_id[present], unit_name = block_lookup$unit_name[present],
    cell_count = count_sum[present], support_area_ha = area_sum[present],
    start_agb_Mg = start_sum[present], end_agb_Mg = end_sum[present], delta_agb_Mg = delta,
    delta_Mg_ha = delta / area_sum[present],
    delta_percent = ifelse(start_sum[present] != 0, 100 * delta / start_sum[present], NA_real_),
    stringsAsFactors = FALSE
  )
}

trajectory_rows <- list()
period_rows <- list()
block_rows_out <- list()

for (y in years) {
  trajectory_rows <- append_rows(trajectory_rows, aggregate_trajectory(obs_mass[[as.character(y)]], trajectory_index_set, "ctrees", "observed", NA_integer_, y))
}
for (i in seq_len(nrow(periods))) {
  pid <- periods$period_id[[i]]
  sy <- as.character(periods$start_year[[i]])
  ey <- as.character(periods$end_year[[i]])
  period_rows <- append_rows(period_rows, aggregate_units(obs_mass[[sy]], obs_mass[[ey]], period_index_sets[[pid]], "ctrees", "observed", NA_integer_, period_id = pid))
  block_rows_out <- append_rows(block_rows_out, aggregate_blocks(obs_mass[[sy]], obs_mass[[ey]], period_block_cells[[pid]], eligible_blocks[[pid]], "ctrees", "observed", NA_integer_, pid))
}

endpoint_years <- sort(unique(c(periods$start_year, periods$end_year)))
for (cfg in names(mc_sets)) {
  cat("Aggregating ", cfg, " BaU MC runs...\n", sep = "")
  for (j in seq_len(nrow(mc_sets[[cfg]]))) {
    mc_id <- mc_sets[[cfg]]$mc_id[[j]]
    d <- mc_sets[[cfg]]$mc_dir[[j]]
    endpoint_cache <- list()
    for (y in years) {
      v <- read_model_values(model_path(d, y), ref)
      trajectory_rows <- append_rows(trajectory_rows, aggregate_trajectory(v, trajectory_index_set, "mofuss", cfg, mc_id, y))
      if (y %in% endpoint_years) endpoint_cache[[as.character(y)]] <- v
    }
    for (i in seq_len(nrow(periods))) {
      pid <- periods$period_id[[i]]
      start <- endpoint_cache[[as.character(periods$start_year[[i]])]]
      end <- endpoint_cache[[as.character(periods$end_year[[i]])]]
      period_rows <- append_rows(period_rows, aggregate_units(start, end, period_index_sets[[pid]], "mofuss", cfg, mc_id, period_id = pid))
      block_rows_out <- append_rows(block_rows_out, aggregate_blocks(start, end, period_block_cells[[pid]], eligible_blocks[[pid]], "mofuss", cfg, mc_id, pid))
    }
  }
}

trajectory <- safe_bind(trajectory_rows)
period_all_mc <- safe_bind(period_rows)
block_all_mc <- safe_bind(block_rows_out)

coverage_rows <- list()
coverage_rows <- append_rows(coverage_rows, data.frame(
  support_id = "trajectory_all_years", period_id = NA_character_, unit_id = units$unit_id,
  unit_name = units$unit_name, unit_level = units$unit_level,
  cell_count = vapply(units$country_code, function(cc) sum(unit_cells(trajectory_support, cc)), numeric(1)),
  support_area_ha = vapply(units$country_code, function(cc) sum(ground_area_ha[unit_cells(trajectory_support, cc)]), numeric(1)),
  stringsAsFactors = FALSE
))
for (i in seq_len(nrow(periods))) {
  pid <- periods$period_id[[i]]
  support <- period_support[[pid]]
  coverage_rows <- append_rows(coverage_rows, data.frame(
    support_id = paste0("endpoint_", pid), period_id = pid, unit_id = units$unit_id,
    unit_name = units$unit_name, unit_level = units$unit_level,
    cell_count = vapply(units$country_code, function(cc) sum(unit_cells(support, cc)), numeric(1)),
    support_area_ha = vapply(units$country_code, function(cc) sum(ground_area_ha[unit_cells(support, cc)]), numeric(1)),
    stringsAsFactors = FALSE
  ))
}
coverage <- safe_bind(coverage_rows)

primary_pid <- periods$period_id[periods$is_primary][[1L]]
primary_block_values <- rep(NA_real_, ncell_ref)
primary_eligible <- eligible_blocks[[primary_pid]]
keep_primary <- period_support[[primary_pid]] & is.finite(block_id_cell) & primary_eligible[block_id_cell]
primary_block_values[keep_primary] <- block_id_cell[keep_primary]
primary_block_r <- ref
terra::values(primary_block_r) <- primary_block_values
terra::writeRaster(primary_block_r, file.path(stage_dir, paste0("primary_", primary_pid, "_50km_block_id.tif")), overwrite = FALSE, datatype = "INT4S", NAflag = -9999)

input_manifest <- safe_bind(c(
  lapply(seq_len(nrow(meta)), function(i) file_record(meta$run_dir[[i]], paste0("run_dir_", meta$scenario[[i]], "_", meta$cap_status[[i]]))),
  lapply(years, function(y) file_record(obs_path(y), paste0("ctrees_", y))),
  list(file_record(admin_path, "administrative_boundaries")),
  if (exclude_hydrolakes) list(file_record(hydro_path, "hydrolakes")) else list(),
  list(file_record(script_path(), "preparation_script"))
))

design <- list(
  schema_version = "agb_validation_preparation_v2",
  created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  interpretation = "Empirical structural--stochastic consistency of CTrees all-driver AGB change with capped and uncapped BaU MoFuSS configurations; not fNRB attribution.",
  model_quantity = "Extensive dry aboveground biomass in Mg per model cell; summed directly without nominal-area rescaling.",
  observation_quantity = "CTrees dry aboveground biomass density in Mg/ha multiplied by geodesic model-cell area.",
  trajectory_support = "One fixed support valid for CTrees and all capped/uncapped BaU MC rasters in every observation year.",
  period_support = "One fixed endpoint support per period, valid for CTrees and all capped/uncapped BaU MC rasters at both endpoints.",
  block_definition = paste0(block_size_km, "-km projected blocks nested within countries; minimum ", min_block_cells, " supported 1-km cells."),
  primary_period = primary_pid,
  ctrees_recent_comparable = ctrees_recent_comparable,
  ctrees_recent_note = if (ctrees_recent_comparable) "Recent CTrees years were declared comparable by configuration." else "Recent CTrees temporal comparability is not yet confirmed; 2010--2020 remains the configured primary period.",
  hydrolakes_excluded = exclude_hydrolakes,
  carbon_fraction_metadata_only = carbon_fraction,
  expected_mc = expected_mc,
  excluded_scenarios = ics$run_name
)

prepared <- list(
  design = design, runs = meta, bau_runs = bau, periods = periods, units = units,
  block_lookup = block_lookup, coverage = coverage, trajectory = trajectory,
  period_all_mc = period_all_mc, block_all_mc = block_all_mc,
  spatial = list(crs = terra::crs(ref), extent = as.vector(terra::ext(ref)), resolution = terra::res(ref), nrow = nr, ncol = nc),
  input_manifest = input_manifest
)

saveRDS(prepared, file.path(stage_dir, "agb_validation_prepared_v2.rds"), compress = "xz")
write_csv_gz(trajectory, file.path(stage_dir, "trajectory_all_mc.csv.gz"))
write_csv_gz(period_all_mc, file.path(stage_dir, "unit_period_all_mc.csv.gz"))
write_csv_gz(block_all_mc, file.path(stage_dir, "block50km_period_all_mc.csv.gz"))
utils::write.csv(periods, file.path(stage_dir, "period_definitions.csv"), row.names = FALSE)
utils::write.csv(coverage, file.path(stage_dir, "comparison_support_coverage.csv"), row.names = FALSE)
utils::write.csv(block_lookup, file.path(stage_dir, "block50km_lookup.csv"), row.names = FALSE)
utils::write.csv(input_manifest, file.path(stage_dir, "input_manifest.csv"), row.names = FALSE)
writeLines(capture.output(str(design)), file.path(stage_dir, "validation_design.txt"))
writeLines(capture.output(sessionInfo()), file.path(stage_dir, "sessionInfo.txt"))

if (!file.rename(stage_dir, completed_stage_dir)) stop("Could not promote staging output to completed temporary directory: ", completed_stage_dir, call. = FALSE)
if (!dir.exists(validation_root)) dir.create(validation_root, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(validation_root)) stop("Could not create final validation root: ", validation_root, call. = FALSE)
copy_directory(completed_stage_dir, final_dir)

cat("Preparation complete.\n")
cat("All-year common support: ", sum(trajectory_support), " cells; ", round(sum(ground_area_ha[trajectory_support])), " geodesic ha.\n", sep = "")
cat("Retained completed temporary product: ", completed_stage_dir, "\n", sep = "")
cat("Prepared output: ", final_dir, "\n", sep = "")
