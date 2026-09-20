#!/usr/bin/env Rscript

# MoFuSS model-implied sourcing approximation, version 1
#
# Purpose
# -------
# Estimate origin-country -> harvest-country sourcing matrices from completed
# directional regional runs without modifying or rerunning Dinamica EGO.
#
# The method conserves realized model harvest. For each scenario, Monte Carlo
# realization and reporting period it:
#   1. sums annual realized harvest and the model's final W, V and total
#      expected-pressure rasters;
#   2. assigns realized harvest to W and V in proportion to those saved final
#      pressure rasters, retaining other model harvest as an explicit residual;
#   3. constructs period W origin weights from the installed country-specific
#      IDWs and their exact annual lookup-table demands;
#   4. reconstructs the two installed V components (regional importers and
#      domestic Somalia), then splits the pooled importer component by each
#      country's share of national V demand; and
#   5. aggregates the attributed harvest by ADM0 source country.
#
# This is an approximation, not observed trade. In particular, the origin IDW
# weights do not reproduce the model's changing annual biomass/Patcher masks.
# The richer rerun design will retain origin components inside the model.

suppressPackageStartupMessages({
  library(data.table)
  library(terra)
})

options(stringsAsFactors = FALSE, warn = 1)

.stopf <- function(...) stop(sprintf(...), call. = FALSE)
.msg <- function(...) cat(sprintf(...), "\n")

.defaults <- list(
  scenario_dirs = c(
    "E:/GLEA_1000m_bau1_2050_mc3_capped",
    "E:/GLEA_1000m_bau1_2050_mc3_uncapped",
    "E:/GLEA_1000m_ics3_2050_mc3_capped",
    "E:/GLEA_1000m_ics3_2050_mc3_uncapped"
  ),
  zones = paste0(
    "E:/_postprocessing_draft/GLEA_1000m_ics3_2050_mc3/pairs/",
    "ssa_adm0_glea_1000m_bau1_v2_vs_ics3_v2_2026_2050_mc3_capped/",
    "emissions/country/country_harvest_zones.tif"
  ),
  boundaries = paste0(
    "E:/_postprocessing_draft/GLEA_1000m_ics3_2050_mc3/",
    "agb_decomposition/country_boundaries.gpkg"
  ),
  output_dir = "E:/MoFuSS_Active/glea_sourcing_attribution_v1",
  periods = c("2020:2030", "2030:2040", "2040:2050", "2020:2050"),
  mc_runs = integer(),
  overwrite = FALSE
)

.parse_bool <- function(x) {
  y <- tolower(trimws(x))
  if (y %in% c("true", "t", "yes", "y", "1")) return(TRUE)
  if (y %in% c("false", "f", "no", "n", "0")) return(FALSE)
  .stopf("Invalid boolean value: %s", x)
}

.parse_cli <- function(args, defaults = .defaults) {
  cfg <- defaults
  scenario_dirs <- character()
  for (arg in args) {
    if (grepl("^--scenario-dir=", arg)) {
      scenario_dirs <- c(scenario_dirs, sub("^--scenario-dir=", "", arg))
    } else if (grepl("^--zones=", arg)) {
      cfg$zones <- sub("^--zones=", "", arg)
    } else if (grepl("^--boundaries=", arg)) {
      cfg$boundaries <- sub("^--boundaries=", "", arg)
    } else if (grepl("^--output-dir=", arg)) {
      cfg$output_dir <- sub("^--output-dir=", "", arg)
    } else if (grepl("^--periods=", arg)) {
      cfg$periods <- strsplit(sub("^--periods=", "", arg), ",", fixed = TRUE)[[1L]]
    } else if (grepl("^--mc-runs=", arg)) {
      value <- sub("^--mc-runs=", "", arg)
      if (grepl("^[0-9]+:[0-9]+$", value)) {
        endpoints <- as.integer(strsplit(value, ":", fixed = TRUE)[[1L]])
        cfg$mc_runs <- seq.int(endpoints[[1L]], endpoints[[2L]])
      } else {
        cfg$mc_runs <- as.integer(strsplit(value, ",", fixed = TRUE)[[1L]])
      }
    } else if (grepl("^--overwrite=", arg)) {
      cfg$overwrite <- .parse_bool(sub("^--overwrite=", "", arg))
    } else if (arg %in% c("--help", "-h")) {
      cat(
        "Usage: Rscript 1post_model_implied_sourcing_v1.R [options]\n",
        "  --scenario-dir=PATH   Repeat once per completed run\n",
        "  --zones=PATH          Validated country harvest-zone raster\n",
        "  --boundaries=PATH     Country boundary/crosswalk GPKG\n",
        "  --output-dir=PATH     Output directory\n",
        "  --periods=A:B,C:D     Inclusive reporting periods\n",
        "  --mc-runs=1:3         Optional Monte Carlo subset\n",
        "  --overwrite=true      Replace existing CSV outputs\n",
        sep = ""
      )
      quit(save = "no", status = 0L)
    } else {
      .stopf("Unknown argument: %s", arg)
    }
  }
  if (length(scenario_dirs)) cfg$scenario_dirs <- scenario_dirs
  cfg
}

.norm_existing <- function(path, label) {
  if (!file.exists(path)) .stopf("Missing %s: %s", label, path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.parse_periods <- function(spec) {
  rows <- lapply(spec, function(x) {
    bits <- strsplit(trimws(x), ":", fixed = TRUE)[[1L]]
    if (length(bits) != 2L || any(!grepl("^[0-9]{4}$", bits))) {
      .stopf("Invalid period '%s'; expected YYYY:YYYY", x)
    }
    start <- as.integer(bits[[1L]])
    end <- as.integer(bits[[2L]])
    if (start > end) .stopf("Period starts after it ends: %s", x)
    data.table(
      period = sprintf("%d-%d", start, end),
      period_start = start,
      period_end = end
    )
  })
  ans <- unique(rbindlist(rows))
  if (!nrow(ans)) .stopf("At least one reporting period is required")
  ans
}

.read_key_value <- function(path) {
  tab <- fread(path, showProgress = FALSE)
  if (!all(c("Var", "ParCHR") %in% names(tab))) {
    .stopf("Invalid parameter table: %s", path)
  }
  setNames(as.character(tab$ParCHR), tab$Var)
}

.scenario_metadata <- function(run_dir) {
  param_path <- file.path(
    run_dir, "LULCC", "TempTables", "parameters_dinamica.csv"
  )
  pars <- .read_key_value(.norm_existing(param_path, "Dinamica parameter table"))
  needed <- c("start_year", "end_year", "monte_carlo_runs", "uncapped_regrowth", "npa_ease")
  missing <- setdiff(needed, names(pars))
  if (length(missing)) {
    .stopf("Parameter table is missing: %s", paste(missing, collapse = ", "))
  }

  run_name <- basename(run_dir)
  lower_name <- tolower(run_name)
  if (grepl("bau1", lower_name, fixed = TRUE)) {
    role <- "bau"
    scenario <- "BaU1_v2"
    demand_basename <- "demand_bau1_v2.csv"
  } else if (grepl("ics3", lower_name, fixed = TRUE)) {
    role <- "ics"
    scenario <- "ICS3_v2"
    demand_basename <- "demand_ics3_v2.csv"
  } else {
    .stopf("Cannot infer BaU1/ICS3 scenario from folder name: %s", run_name)
  }

  list(
    run_dir = run_dir,
    run_name = run_name,
    role = role,
    scenario = scenario,
    start_year = as.integer(pars[["start_year"]]),
    end_year = as.integer(pars[["end_year"]]),
    monte_carlo_runs = as.integer(pars[["monte_carlo_runs"]]),
    regrowth = if (as.integer(pars[["uncapped_regrowth"]]) == 1L) "uncapped" else "capped",
    npa_ease = as.numeric(pars[["npa_ease"]]),
    demand_path = file.path(
      run_dir, "LULCC", "DownloadedDatasets", "SourceDataGlobal",
      "demand", "demand_in", demand_basename
    )
  )
}

.step_for_year <- function(year, start_year) year - start_year + 1L

.idw_step <- function(model_step) {
  if (model_step < 11L) return(1L)
  if (model_step < 21L) return(11L)
  if (model_step < 31L) return(21L)
  if (model_step < 41L) return(31L)
  if (model_step < 51L) return(41L)
  51L
}

.lookup_path <- function(run_dir, channel, step) {
  file.path(
    run_dir, "In", "DemandScenarios",
    sprintf("%s_origin_demand%02d.csv", channel, step)
  )
}

.component_path <- function(run_dir, channel, component_index, source_step) {
  file.path(
    run_dir, "In", sprintf("%s_origin_components", channel),
    sprintf(
      "IDW_C++_fw_%s%03d_%02d.tif",
      tolower(channel), component_index, source_step
    )
  )
}

.debug_path <- function(run_dir, mc_run, stem, step) {
  file.path(
    run_dir, sprintf("debugging_%d", mc_run),
    sprintf("%s%02d.tif", stem, step)
  )
}

.read_component_index <- function(run_dir, channel) {
  path <- file.path(
    run_dir, "In", "DemandScenarios",
    sprintf("%s_origin_component_index.csv", channel)
  )
  tab <- fread(.norm_existing(path, sprintf("%s component index", channel)))
  required <- c("ComponentIndex", "DemandISO3", "JobID", "DirectionRule", "AllowedSourceISO3")
  missing <- setdiff(required, names(tab))
  if (length(missing)) {
    .stopf("%s is missing: %s", path, paste(missing, collapse = ", "))
  }
  setorder(tab, ComponentIndex)
  tab
}

.read_exact_annual_demands <- function(meta, years, w_index, v_index) {
  w_rows <- vector("list", length(years))
  v_component_rows <- vector("list", length(years))
  for (k in seq_along(years)) {
    year <- years[[k]]
    step <- .step_for_year(year, meta$start_year)
    w <- fread(
      .norm_existing(.lookup_path(meta$run_dir, "W", step), "annual W lookup"),
      showProgress = FALSE
    )
    v <- fread(
      .norm_existing(.lookup_path(meta$run_dir, "V", step), "annual V lookup"),
      showProgress = FALSE
    )
    if (!all(c("Key", "Value") %in% names(w)) ||
        !all(c("Key", "Value") %in% names(v))) {
      .stopf("Invalid annual origin-demand lookup for %d in %s", year, meta$run_name)
    }
    if (!identical(as.integer(w$Key), as.integer(w_index$ComponentIndex)) ||
        !identical(as.integer(v$Key), as.integer(v_index$ComponentIndex))) {
      .stopf("Origin-demand lookup keys disagree with component indexes in %s", meta$run_name)
    }
    w_rows[[k]] <- data.table(
      year = year,
      channel = "W",
      component_index = as.integer(w$Key),
      origin_iso3 = as.character(w_index$DemandISO3),
      demand_tons = as.numeric(w$Value)
    )
    v_component_rows[[k]] <- data.table(
      year = year,
      channel = "V",
      component_index = as.integer(v$Key),
      component_origins = as.character(v_index$DemandISO3),
      demand_tons = as.numeric(v$Value)
    )
  }
  w_annual <- rbindlist(w_rows)
  v_components <- rbindlist(v_component_rows)

  demand <- fread(
    .norm_existing(meta$demand_path, "scenario demand table"),
    select = c("iso3", "area", "fuel", "year", "fuel_cons_tons"),
    showProgress = FALSE
  )
  region_iso3 <- sort(unique(unlist(strsplit(v_index$DemandISO3, ";", fixed = TRUE))))
  demand <- demand[
    iso3 %chin% region_iso3 & year %in% years & area %chin% c("urban", "rural")
  ]
  # V = urban fuelwood/imported fuelwood + urban/rural charcoal/imported charcoal.
  demand[, is_v :=
    (area == "urban" & fuel %chin% c("fuelwood", "imp_fuelwood")) |
      fuel %chin% c("charcoal", "imp_charcoal")]
  v_national <- demand[is_v == TRUE, .(
    table_demand_tons = sum(fuel_cons_tons, na.rm = TRUE)
  ), by = .(year, origin_iso3 = iso3)]

  split_rows <- vector("list", nrow(v_components))
  for (k in seq_len(nrow(v_components))) {
    row <- v_components[k]
    origins <- strsplit(row$component_origins, ";", fixed = TRUE)[[1L]]
    shares <- v_national[year == row$year & origin_iso3 %chin% origins]
    if (nrow(shares) != length(origins)) {
      .stopf(
        "Could not recover all national V demands for component %d, year %d",
        row$component_index, row$year
      )
    }
    total <- sum(shares$table_demand_tons)
    if (row$demand_tons > 0 && (!is.finite(total) || total <= 0)) {
      .stopf("Positive V component demand has no national-demand basis in %d", row$year)
    }
    shares[, `:=`(
      channel = "V",
      component_index = row$component_index,
      component_origins = row$component_origins,
      component_demand_tons = row$demand_tons,
      demand_tons = if (total > 0) row$demand_tons * table_demand_tons / total else 0,
      national_split_method = if (length(origins) > 1L) {
        "pooled_importer_component_split_by_national_V_demand_share"
      } else {
        "origin_specific_domestic_component"
      }
    )]
    split_rows[[k]] <- shares[, .(
      year, channel, component_index, component_origins, origin_iso3,
      component_demand_tons, table_demand_tons, demand_tons,
      national_split_method
    )]
  }
  v_annual <- rbindlist(split_rows)

  # Exact conservation of the installed component lookups is mandatory.
  check <- v_annual[, .(split_sum = sum(demand_tons)),
                    by = .(year, component_index, component_demand_tons)]
  if (any(abs(check$split_sum - check$component_demand_tons) >
          pmax(1e-6, abs(check$component_demand_tons) * 1e-12))) {
    .stopf("National V split does not conserve installed component demand")
  }

  list(w = w_annual, v = v_annual, v_components = v_components)
}

.sum_rasters <- function(paths, template = NULL) {
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    .stopf("Missing model raster(s), first: %s", missing[[1L]])
  }
  x <- rast(paths)
  if (!is.null(template) && !compareGeom(x[[1L]], template, stopOnError = FALSE)) {
    .stopf("Raster geometry mismatch: %s", paths[[1L]])
  }
  sum(x, na.rm = TRUE)
}

.finite_sum <- function(x) {
  value <- global(x, "sum", na.rm = TRUE)[[1L]]
  if (!is.finite(value)) 0 else as.numeric(value)
}

.normalised_component_pressure <- function(
    meta, channel, component_index, years, annual_component_demand,
    npa_multiplier, template) {
  source_steps <- vapply(
    .step_for_year(years, meta$start_year), .idw_step, integer(1L)
  )
  demand_by_snapshot <- data.table(
    year = years,
    source_step = source_steps
  )[annual_component_demand, on = "year"]
  demand_by_snapshot <- demand_by_snapshot[, .(
    demand_tons = sum(demand_tons, na.rm = TRUE)
  ), by = source_step]

  result <- NULL
  for (k in seq_len(nrow(demand_by_snapshot))) {
    source_step <- demand_by_snapshot$source_step[[k]]
    demand_tons <- demand_by_snapshot$demand_tons[[k]]
    if (!is.finite(demand_tons) || demand_tons <= 0) next
    path <- .norm_existing(
      .component_path(meta$run_dir, channel, component_index, source_step),
      sprintf("%s component IDW", channel)
    )
    raw <- rast(path)
    if (!compareGeom(raw, template, stopOnError = FALSE)) {
      .stopf("IDW geometry mismatch: %s", path)
    }
    adjusted <- raw * npa_multiplier
    denominator <- .finite_sum(adjusted)
    if (denominator <= 0) {
      .stopf(
        "%s component %d has demand but zero usable IDW pressure at step %d",
        channel, component_index, source_step
      )
    }
    contribution <- adjusted * (demand_tons / denominator)
    result <- if (is.null(result)) {
      contribution
    } else {
      ifel(is.na(result), 0, result) + ifel(is.na(contribution), 0, contribution)
    }
  }
  if (is.null(result)) template * 0 else result
}

.period_pressure <- function(meta, period, demands, w_index, v_index,
                             npa_multiplier, template) {
  years <- seq.int(period$period_start, period$period_end)

  w_layers <- vector("list", nrow(w_index))
  w_labels <- character(nrow(w_index))
  for (k in seq_len(nrow(w_index))) {
    idx <- w_index$ComponentIndex[[k]]
    origin <- w_index$DemandISO3[[k]]
    annual <- demands$w[component_index == idx & year %in% years,
                        .(year, demand_tons)]
    w_layers[[k]] <- .normalised_component_pressure(
      meta, "W", idx, years, annual, npa_multiplier, template
    )
    w_labels[[k]] <- sprintf("W_%s", origin)
  }
  w_pressure <- rast(w_layers)
  names(w_pressure) <- w_labels
  w_total <- app(w_pressure, sum, na.rm = TRUE)
  w_fractions <- ifel(w_total > 0, w_pressure / w_total, 0)
  names(w_fractions) <- w_labels

  v_layers <- vector("list", nrow(v_index))
  v_labels <- character(nrow(v_index))
  for (k in seq_len(nrow(v_index))) {
    idx <- v_index$ComponentIndex[[k]]
    annual <- demands$v_components[
      component_index == idx & year %in% years, .(year, demand_tons)
    ]
    v_layers[[k]] <- .normalised_component_pressure(
      meta, "V", idx, years, annual, npa_multiplier, template
    )
    v_labels[[k]] <- sprintf("VCOMP_%03d", idx)
  }
  v_pressure <- rast(v_layers)
  names(v_pressure) <- v_labels
  v_total <- app(v_pressure, sum, na.rm = TRUE)
  v_fractions <- ifel(v_total > 0, v_pressure / v_total, 0)
  names(v_fractions) <- v_labels

  list(w = w_fractions, v = v_fractions)
}

.period_model_maps <- function(meta, mc_run, period, template) {
  years <- seq.int(period$period_start, period$period_end)
  steps <- .step_for_year(years, meta$start_year)
  total_actual <- .sum_rasters(
    vapply(steps, function(s) .debug_path(meta$run_dir, mc_run, "Harvest_tot", s), character(1L)),
    template
  )
  total_expected <- .sum_rasters(
    vapply(steps, function(s) .debug_path(meta$run_dir, mc_run, "Expect_harv_tot", s), character(1L)),
    template
  )
  w_expected <- .sum_rasters(
    vapply(steps, function(s) .debug_path(meta$run_dir, mc_run, "harv_AGR", s), character(1L)),
    template
  )
  v_expected <- .sum_rasters(
    vapply(steps, function(s) .debug_path(meta$run_dir, mc_run, "Proj_harv_Vdef", s), character(1L)),
    template
  )

  # The realized-harvest limitation is applied to total expected pressure in the
  # model. Proportional channel attribution is therefore the neutral conserving
  # post-hoc rule.
  nonnegative_w <- ifel(w_expected > 0, w_expected, 0)
  nonnegative_v <- ifel(v_expected > 0, v_expected, 0)
  actual_w <- ifel(
    total_expected > 0,
    total_actual * nonnegative_w / total_expected,
    0
  )
  actual_v <- ifel(
    total_expected > 0,
    total_actual * nonnegative_v / total_expected,
    0
  )
  list(
    actual_total = total_actual,
    expected_total = total_expected,
    expected_w = w_expected,
    expected_v = v_expected,
    actual_w = actual_w,
    actual_v = actual_v
  )
}

.zonal_component_sums <- function(fractions, actual_channel, zones) {
  allocated <- fractions * actual_channel
  tab <- as.data.table(zonal(allocated, zones, fun = "sum", na.rm = TRUE))
  setnames(tab, 1L, "source_id")
  value_columns <- setdiff(names(tab), "source_id")
  for (column in value_columns) {
    set(tab, which(!is.finite(tab[[column]])), column, 0)
  }
  tab
}

.wide_to_long <- function(tab, prefix, channel) {
  columns <- grep(paste0("^", prefix), names(tab), value = TRUE)
  if (!length(columns)) .stopf("No %s allocation columns were produced", channel)
  melt(
    tab,
    id.vars = "source_id",
    measure.vars = columns,
    variable.name = "component_label",
    value.name = "actual_harvest_tons"
  )[, channel := channel]
}

.assert_close <- function(observed, expected, label, rel_tol = 1e-8, abs_tol = 1) {
  tolerance <- max(abs_tol, abs(expected) * rel_tol)
  if (!is.finite(observed) || !is.finite(expected) || abs(observed - expected) > tolerance) {
    .stopf(
      "%s does not reconcile: observed=%.12g expected=%.12g tolerance=%.12g",
      label, observed, expected, tolerance
    )
  }
}

.write_csv_safe <- function(x, path, overwrite) {
  if (file.exists(path) && !overwrite) {
    .stopf("Refusing to overwrite existing output: %s", path)
  }
  fwrite(x, path)
}

.summarise_mc <- function(matrix_by_mc, origin_by_mc) {
  matrix_summary <- matrix_by_mc[, .(
    actual_harvest_tons_mean = mean(actual_harvest_tons),
    actual_harvest_tons_sd = sd(actual_harvest_tons),
    actual_harvest_tons_se = sd(actual_harvest_tons) / sqrt(.N),
    source_share_pct_mean = mean(source_share_pct),
    source_share_pct_sd = sd(source_share_pct),
    mc_n = .N
  ), by = .(
    run_name, scenario, scenario_role, regrowth, period, period_start, period_end,
    channel, origin_iso3, origin_name, source_iso3, source_name,
    is_domestic, attribution_method
  )]

  origin_summary <- origin_by_mc[, .(
    expected_demand_tons = unique(expected_demand_tons)[[1L]],
    actual_harvest_tons_mean = mean(actual_harvest_tons),
    actual_harvest_tons_sd = sd(actual_harvest_tons),
    actual_harvest_tons_se = sd(actual_harvest_tons) / sqrt(.N),
    domestic_harvest_tons_mean = mean(domestic_harvest_tons),
    imported_harvest_tons_mean = mean(imported_harvest_tons),
    actual_as_pct_of_expected_mean = mean(actual_as_pct_of_expected),
    domestic_share_pct_mean = mean(domestic_share_pct),
    domestic_share_pct_sd = sd(domestic_share_pct),
    imported_share_pct_mean = mean(imported_share_pct),
    imported_share_pct_sd = sd(imported_share_pct),
    mc_n = .N
  ), by = .(
    run_name, scenario, scenario_role, regrowth, period, period_start, period_end,
    channel, origin_iso3, origin_name, attribution_method
  )]
  list(matrix = matrix_summary, origin = origin_summary)
}

cfg <- .parse_cli(commandArgs(trailingOnly = TRUE))
cfg$scenario_dirs <- vapply(
  cfg$scenario_dirs, .norm_existing, character(1L), label = "scenario directory"
)
cfg$zones <- .norm_existing(cfg$zones, "country harvest-zone raster")
cfg$boundaries <- .norm_existing(cfg$boundaries, "country boundaries")
periods <- .parse_periods(cfg$periods)

if (!dir.exists(cfg$output_dir)) {
  dir.create(cfg$output_dir, recursive = TRUE, showWarnings = FALSE)
}
if (!dir.exists(cfg$output_dir)) .stopf("Could not create output directory: %s", cfg$output_dir)
cfg$output_dir <- normalizePath(cfg$output_dir, winslash = "/", mustWork = TRUE)
temp_dir <- file.path(cfg$output_dir, "terra_tmp")
if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
terraOptions(tempdir = temp_dir, memfrac = 0.65, progress = 0)

zones <- rast(cfg$zones)
if (nlyr(zones) != 1L) .stopf("Country zone raster must have exactly one layer")
boundaries <- vect(cfg$boundaries)
required_boundary_fields <- c("ID", "GID_0", "NAME_0")
missing_boundary_fields <- setdiff(required_boundary_fields, names(boundaries))
if (length(missing_boundary_fields)) {
  .stopf("Country boundaries are missing: %s", paste(missing_boundary_fields, collapse = ", "))
}
country_crosswalk <- as.data.table(as.data.frame(boundaries))[, .(
  source_id = as.integer(ID),
  source_iso3 = as.character(GID_0),
  source_name = as.character(NAME_0)
)]
if (anyDuplicated(country_crosswalk$source_id) || anyDuplicated(country_crosswalk$source_iso3)) {
  .stopf("Country boundary IDs and ISO3 codes must be unique")
}
origin_crosswalk <- copy(country_crosswalk)
setnames(origin_crosswalk, c("source_id", "source_iso3", "source_name"),
         c("origin_id", "origin_iso3", "origin_name"))

all_matrix <- list()
all_qa <- list()
matrix_counter <- 0L
qa_counter <- 0L

for (run_dir in cfg$scenario_dirs) {
  meta <- .scenario_metadata(run_dir)
  .msg("=== %s (%s, %s) ===", meta$run_name, meta$scenario, meta$regrowth)
  if (any(periods$period_start < meta$start_year) ||
      any(periods$period_end > meta$end_year)) {
    .stopf("Requested periods fall outside %d-%d for %s",
           meta$start_year, meta$end_year, meta$run_name)
  }
  mc_runs <- if (length(cfg$mc_runs)) cfg$mc_runs else seq_len(meta$monte_carlo_runs)
  if (any(mc_runs < 1L | mc_runs > meta$monte_carlo_runs)) {
    .stopf("Invalid Monte Carlo selection for %s", meta$run_name)
  }

  w_index <- .read_component_index(run_dir, "W")
  v_index <- .read_component_index(run_dir, "V")
  region_iso3 <- sort(unique(c(
    w_index$DemandISO3,
    unlist(strsplit(v_index$DemandISO3, ";", fixed = TRUE))
  )))
  if (!setequal(region_iso3, country_crosswalk$source_iso3)) {
    .stopf("Component countries and source-zone countries disagree for %s", meta$run_name)
  }

  years_needed <- seq.int(min(periods$period_start), max(periods$period_end))
  demands <- .read_exact_annual_demands(meta, years_needed, w_index, v_index)

  template <- rast(.debug_path(
    run_dir, mc_runs[[1L]], "Harvest_tot",
    .step_for_year(years_needed[[1L]], meta$start_year)
  ))
  if (!compareGeom(template, zones, stopOnError = FALSE)) {
    .stopf("Country harvest zones do not match model grid for %s", meta$run_name)
  }
  npa <- rast(.norm_existing(
    file.path(run_dir, "LULCC", "TempRaster", "NPA_c.tif"),
    "NPA raster"
  ))
  if (!compareGeom(template, npa, stopOnError = FALSE)) {
    .stopf("NPA raster does not match model grid for %s", meta$run_name)
  }
  npa_multiplier <- ifel(!is.na(npa), meta$npa_ease / 100, 1)

  for (p in seq_len(nrow(periods))) {
    period <- periods[p]
    .msg("  Period %s: constructing origin-pressure fractions", period$period)
    fractions <- .period_pressure(
      meta, period, demands, w_index, v_index,
      npa_multiplier, template
    )

    period_years <- seq.int(period$period_start, period$period_end)
    expected_w_origin <- demands$w[year %in% period_years, .(
      expected_demand_tons = sum(demand_tons)
    ), by = .(channel, origin_iso3)]
    expected_v_origin <- demands$v[year %in% period_years, .(
      expected_demand_tons = sum(demand_tons),
      attribution_method = unique(national_split_method)[[1L]]
    ), by = .(channel, origin_iso3, component_index)]
    expected_origin <- rbindlist(list(
      expected_w_origin[, attribution_method := "origin_specific_W_IDW"],
      expected_v_origin[, .(channel, origin_iso3, expected_demand_tons, attribution_method)]
    ), use.names = TRUE)

    v_period_shares <- demands$v[year %in% period_years, .(
      origin_demand_tons = sum(demand_tons),
      component_demand_tons = sum(component_demand_tons),
      attribution_method = unique(national_split_method)[[1L]]
    ), by = .(component_index, origin_iso3)]
    v_period_shares[, origin_share := fifelse(
      component_demand_tons > 0,
      origin_demand_tons / component_demand_tons,
      0
    )]

    for (mc_run in mc_runs) {
      .msg("    MC%02d: aggregating model harvest and assigning sources", mc_run)
      model_maps <- .period_model_maps(meta, mc_run, period, template)
      w_zonal <- .zonal_component_sums(fractions$w, model_maps$actual_w, zones)
      v_zonal <- .zonal_component_sums(fractions$v, model_maps$actual_v, zones)

      w_long <- .wide_to_long(w_zonal, "W_", "W")
      w_long[, origin_iso3 := sub("^W_", "", as.character(component_label))]
      w_long[, attribution_method := "origin_specific_W_IDW"]

      v_component_long <- .wide_to_long(v_zonal, "VCOMP_", "V")
      v_component_long[, component_index := as.integer(sub(
        "^VCOMP_", "", as.character(component_label)
      ))]
      v_long <- merge(
        v_component_long[, .(source_id, component_index, component_harvest_tons = actual_harvest_tons)],
        v_period_shares,
        by = "component_index",
        allow.cartesian = TRUE
      )
      v_long[, actual_harvest_tons := component_harvest_tons * origin_share]
      v_long[, channel := "V"]

      matrix <- rbindlist(list(
        w_long[, .(
          source_id, channel, origin_iso3, actual_harvest_tons,
          attribution_method
        )],
        v_long[, .(
          source_id, channel, origin_iso3, actual_harvest_tons,
          attribution_method
        )]
      ), use.names = TRUE)
      matrix <- merge(matrix, country_crosswalk, by = "source_id", all.x = TRUE)
      matrix <- merge(matrix, origin_crosswalk[, .(origin_iso3, origin_name)],
                      by = "origin_iso3", all.x = TRUE)
      if (anyNA(matrix$source_iso3) || anyNA(matrix$origin_name)) {
        .stopf("Failed to attach country names to sourcing matrix")
      }
      matrix[, `:=`(
        run_name = meta$run_name,
        scenario = meta$scenario,
        scenario_role = meta$role,
        regrowth = meta$regrowth,
        mc_run = mc_run,
        period = period$period,
        period_start = period$period_start,
        period_end = period$period_end,
        is_domestic = origin_iso3 == source_iso3
      )]
      matrix[, origin_actual_harvest_tons := sum(actual_harvest_tons),
             by = .(channel, origin_iso3)]
      matrix[, source_share_pct := fifelse(
        origin_actual_harvest_tons > 0,
        100 * actual_harvest_tons / origin_actual_harvest_tons,
        NA_real_
      )]

      w_allocated <- sum(matrix[channel == "W", actual_harvest_tons])
      v_allocated <- sum(matrix[channel == "V", actual_harvest_tons])
      w_actual <- .finite_sum(model_maps$actual_w)
      v_actual <- .finite_sum(model_maps$actual_v)
      total_actual <- .finite_sum(model_maps$actual_total)
      total_expected <- .finite_sum(model_maps$expected_total)
      .assert_close(w_allocated, w_actual, "W source allocation")
      .assert_close(v_allocated, v_actual, "V source allocation")
      if (w_actual + v_actual > total_actual + max(1, total_actual * 1e-8)) {
        .stopf("Attributed W+V harvest exceeds total realized harvest")
      }

      matrix_counter <- matrix_counter + 1L
      all_matrix[[matrix_counter]] <- matrix[, .(
        run_name, scenario, scenario_role, regrowth, mc_run,
        period, period_start, period_end, channel,
        origin_iso3, origin_name, source_iso3, source_name,
        is_domestic, actual_harvest_tons, source_share_pct,
        attribution_method
      )]
      qa_counter <- qa_counter + 1L
      all_qa[[qa_counter]] <- data.table(
        run_name = meta$run_name,
        scenario = meta$scenario,
        scenario_role = meta$role,
        regrowth = meta$regrowth,
        mc_run = mc_run,
        period = period$period,
        period_start = period$period_start,
        period_end = period$period_end,
        total_expected_pressure_tons = total_expected,
        total_realized_harvest_tons = total_actual,
        attributed_W_harvest_tons = w_allocated,
        attributed_V_harvest_tons = v_allocated,
        other_model_harvest_residual_tons = total_actual - w_allocated - v_allocated,
        W_conservation_difference_tons = w_allocated - w_actual,
        V_conservation_difference_tons = v_allocated - v_actual
      )
      rm(model_maps, w_zonal, v_zonal, w_long, v_component_long, v_long, matrix)
      gc(verbose = FALSE)
    }
    rm(fractions)
    gc(verbose = FALSE)
  }
}

matrix_by_mc <- rbindlist(all_matrix, use.names = TRUE)
qa <- rbindlist(all_qa, use.names = TRUE)
if (!nrow(matrix_by_mc)) .stopf("No sourcing results were produced")

expected_lookup <- unique(rbindlist(lapply(cfg$scenario_dirs, function(run_dir) {
  meta <- .scenario_metadata(run_dir)
  w_index <- .read_component_index(run_dir, "W")
  v_index <- .read_component_index(run_dir, "V")
  years <- seq.int(min(periods$period_start), max(periods$period_end))
  d <- .read_exact_annual_demands(meta, years, w_index, v_index)
  rbindlist(lapply(seq_len(nrow(periods)), function(p) {
    py <- seq.int(periods$period_start[[p]], periods$period_end[[p]])
    rbindlist(list(
      d$w[year %in% py, .(expected_demand_tons = sum(demand_tons)),
          by = .(channel, origin_iso3)][, attribution_method := "origin_specific_W_IDW"],
      d$v[year %in% py, .(
        expected_demand_tons = sum(demand_tons),
        attribution_method = unique(national_split_method)[[1L]]
      ), by = .(channel, origin_iso3)]
    ))[, `:=`(
      run_name = meta$run_name,
      scenario = meta$scenario,
      scenario_role = meta$role,
      regrowth = meta$regrowth,
      period = periods$period[[p]],
      period_start = periods$period_start[[p]],
      period_end = periods$period_end[[p]]
    )]
  }))
})), use.names = TRUE)

origin_by_mc <- matrix_by_mc[, .(
  actual_harvest_tons = sum(actual_harvest_tons),
  domestic_harvest_tons = sum(actual_harvest_tons[is_domestic]),
  imported_harvest_tons = sum(actual_harvest_tons[!is_domestic])
), by = .(
  run_name, scenario, scenario_role, regrowth, mc_run,
  period, period_start, period_end, channel,
  origin_iso3, origin_name, attribution_method
)]
origin_by_mc <- merge(
  origin_by_mc,
  expected_lookup,
  by = c(
    "run_name", "scenario", "scenario_role", "regrowth", "period",
    "period_start", "period_end", "channel", "origin_iso3",
    "attribution_method"
  ),
  all.x = TRUE
)
if (anyNA(origin_by_mc$expected_demand_tons)) {
  .stopf("Failed to attach expected demand to one or more origin summaries")
}
origin_by_mc[, `:=`(
  unmet_demand_proxy_tons = pmax(expected_demand_tons - actual_harvest_tons, 0),
  actual_as_pct_of_expected = fifelse(
    expected_demand_tons > 0, 100 * actual_harvest_tons / expected_demand_tons, NA_real_
  ),
  domestic_share_pct = fifelse(
    actual_harvest_tons > 0, 100 * domestic_harvest_tons / actual_harvest_tons, NA_real_
  ),
  imported_share_pct = fifelse(
    actual_harvest_tons > 0, 100 * imported_harvest_tons / actual_harvest_tons, NA_real_
  )
)]

mc_summary <- .summarise_mc(matrix_by_mc, origin_by_mc)

matrix_combined_by_mc <- matrix_by_mc[, .(
  actual_harvest_tons = sum(actual_harvest_tons)
), by = .(
  run_name, scenario, scenario_role, regrowth, mc_run,
  period, period_start, period_end,
  origin_iso3, origin_name, source_iso3, source_name, is_domestic
)]
matrix_combined_by_mc[, `:=`(
  channel = "W+V",
  attribution_method = "combined_W_and_V_model_implied_approximation"
)]
matrix_combined_by_mc[, origin_actual_harvest_tons := sum(actual_harvest_tons),
                      by = .(run_name, scenario, regrowth, mc_run, period, origin_iso3)]
matrix_combined_by_mc[, source_share_pct := fifelse(
  origin_actual_harvest_tons > 0,
  100 * actual_harvest_tons / origin_actual_harvest_tons,
  NA_real_
)]

origin_combined_by_mc <- origin_by_mc[, .(
  expected_demand_tons = sum(expected_demand_tons),
  actual_harvest_tons = sum(actual_harvest_tons),
  domestic_harvest_tons = sum(domestic_harvest_tons),
  imported_harvest_tons = sum(imported_harvest_tons)
), by = .(
  run_name, scenario, scenario_role, regrowth, mc_run,
  period, period_start, period_end, origin_iso3, origin_name
)]
origin_combined_by_mc[, `:=`(
  channel = "W+V",
  attribution_method = "combined_W_and_V_model_implied_approximation",
  unmet_demand_proxy_tons = pmax(expected_demand_tons - actual_harvest_tons, 0),
  actual_as_pct_of_expected = fifelse(
    expected_demand_tons > 0, 100 * actual_harvest_tons / expected_demand_tons, NA_real_
  ),
  domestic_share_pct = fifelse(
    actual_harvest_tons > 0, 100 * domestic_harvest_tons / actual_harvest_tons, NA_real_
  ),
  imported_share_pct = fifelse(
    actual_harvest_tons > 0, 100 * imported_harvest_tons / actual_harvest_tons, NA_real_
  )
)]
combined_mc_summary <- .summarise_mc(matrix_combined_by_mc, origin_combined_by_mc)
kenya_summary <- rbindlist(list(
  mc_summary$origin[origin_iso3 == "KEN"],
  combined_mc_summary$origin[origin_iso3 == "KEN"]
), use.names = TRUE)

setcolorder(matrix_by_mc, c(
  "run_name", "scenario", "scenario_role", "regrowth", "mc_run",
  "period", "period_start", "period_end", "channel",
  "origin_iso3", "origin_name", "source_iso3", "source_name",
  "is_domestic", "actual_harvest_tons", "source_share_pct",
  "attribution_method"
))
setorder(matrix_by_mc, scenario_role, regrowth, period_start, mc_run,
         channel, origin_iso3, source_iso3)
setorder(origin_by_mc, scenario_role, regrowth, period_start, mc_run,
         channel, origin_iso3)
setorder(mc_summary$matrix, scenario_role, regrowth, period_start,
         channel, origin_iso3, source_iso3)
setorder(mc_summary$origin, scenario_role, regrowth, period_start,
         channel, origin_iso3)
setorder(matrix_combined_by_mc, scenario_role, regrowth, period_start, mc_run,
         origin_iso3, source_iso3)
setorder(origin_combined_by_mc, scenario_role, regrowth, period_start, mc_run,
         origin_iso3)
setorder(combined_mc_summary$matrix, scenario_role, regrowth, period_start,
         origin_iso3, source_iso3)
setorder(combined_mc_summary$origin, scenario_role, regrowth, period_start,
         origin_iso3)
setorder(qa, scenario_role, regrowth, period_start, mc_run)

methodology <- data.table(
  item = c(
    "method_name",
    "interpretation",
    "W_origin_attribution",
    "V_importer_attribution",
    "realized_harvest_attribution",
    "other_harvest_treatment",
    "major_limitation",
    "period_endpoint_rule",
    "source_zone_raster",
    "country_crosswalk",
    "generated_utc"
  ),
  value = c(
    "model_implied_sourcing_approximation_v1",
    "Model-implied allocation; not observed bilateral trade",
    "Country-specific W IDWs weighted by exact installed annual W demand",
    paste(
      "Pooled importer V IDW split by national V-demand shares and rescaled",
      "to the exact installed pooled demand"
    ),
    paste(
      "Period realized harvest allocated to W and V in proportion to the",
      "model's saved final expected-pressure rasters"
    ),
    "Retained as an explicit unattributed residual; never assigned to W or V origins",
    paste(
      "Period IDW weights do not reproduce changing annual biomass and Patcher",
      "eligibility masks"
    ),
    "Inclusive endpoints; adjacent decadal periods overlap at 2030 and 2040",
    cfg$zones,
    cfg$boundaries,
    format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
)

.write_csv_safe(matrix_by_mc, file.path(cfg$output_dir, "sourcing_matrix_by_mc.csv"), cfg$overwrite)
.write_csv_safe(mc_summary$matrix, file.path(cfg$output_dir, "sourcing_matrix_mc_summary.csv"), cfg$overwrite)
.write_csv_safe(origin_by_mc, file.path(cfg$output_dir, "origin_sourcing_summary_by_mc.csv"), cfg$overwrite)
.write_csv_safe(mc_summary$origin, file.path(cfg$output_dir, "origin_sourcing_summary_mc_summary.csv"), cfg$overwrite)
.write_csv_safe(matrix_combined_by_mc, file.path(cfg$output_dir, "sourcing_matrix_combined_by_mc.csv"), cfg$overwrite)
.write_csv_safe(combined_mc_summary$matrix, file.path(cfg$output_dir, "sourcing_matrix_combined_mc_summary.csv"), cfg$overwrite)
.write_csv_safe(origin_combined_by_mc, file.path(cfg$output_dir, "origin_sourcing_combined_by_mc.csv"), cfg$overwrite)
.write_csv_safe(combined_mc_summary$origin, file.path(cfg$output_dir, "origin_sourcing_combined_mc_summary.csv"), cfg$overwrite)
.write_csv_safe(kenya_summary, file.path(cfg$output_dir, "kenya_sourcing_summary.csv"), cfg$overwrite)
.write_csv_safe(qa, file.path(cfg$output_dir, "sourcing_conservation_qa.csv"), cfg$overwrite)
.write_csv_safe(methodology, file.path(cfg$output_dir, "sourcing_methodology.csv"), cfg$overwrite)

.msg("Wrote model-implied sourcing approximation to: %s", cfg$output_dir)
.msg("Rows: matrix by MC=%d; matrix MC summary=%d; origin summary=%d",
     nrow(matrix_by_mc), nrow(mc_summary$matrix), nrow(mc_summary$origin))
