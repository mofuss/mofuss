# Synthetic regression tests for origin-preserving regional W and V workflows.

suppressPackageStartupMessages(library(terra))

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
stopifnot(length(script_argument) == 1L)
test_script <- sub("^--file=", "", script_argument)
repository_root <- normalizePath(
  file.path(dirname(test_script), "..", "..", ".."),
  winslash = "/",
  mustWork = TRUE
)
scripts_root <- file.path(repository_root, "localhost", "scripts")

# The R handoff stages must agree on the same origin-preserving contract.
contract_files <- c(
  "3_demand4IDW_v12.R",
  "5_harmonizer_v8.R",
  "8_prepare_directional_IDW_inputs_v3.R",
  "9_install_directional_IDW_outputs_v4.R"
)
for (filename in contract_files) {
  path <- file.path(scripts_root, filename)
  stopifnot(file.exists(path))
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  stopifnot(grepl("runtime_normalize_by_origin_then_sum", text, fixed = TRUE))
}

demand_text <- paste(
  readLines(file.path(scripts_root, "3_demand4IDW_v12.R"), warn = FALSE),
  collapse = "\n"
)
stopifnot(
  grepl("W_[A-Z]{3}_ORIGIN", demand_text) ||
    grepl('paste0("W_", origin_iso3, "_ORIGIN")', demand_text, fixed = TRUE),
  grepl("origin_country_demand_regional_sources", demand_text, fixed = TRUE),
  grepl("Origin-country W jobs do not exactly conserve", demand_text, fixed = TRUE),
  grepl("origin_preserving_", demand_text, fixed = TRUE),
  grepl("runtime_normalize_by_origin_then_sum", demand_text, fixed = TRUE),
  grepl(".preserve_projected_stack_mass", demand_text, fixed = TRUE),
  grepl("projection_mass_audit_w.csv", demand_text, fixed = TRUE),
  grepl("projection_mass_audit_v.csv", demand_text, fixed = TRUE),
  !grepl("proj_factor_2010w", demand_text, fixed = TRUE),
  !grepl("proj_factor_2010v", demand_text, fixed = TRUE)
)

# Exercise the layer-specific projection correction without sourcing the full
# demand workflow (which intentionally has run-level side effects).
demand_path <- file.path(scripts_root, "3_demand4IDW_v12.R")
demand_expressions <- parse(demand_path)
mass_helper_index <- which(vapply(demand_expressions, function(expression) {
  is.call(expression) && identical(expression[[1L]], as.name("<-")) &&
    identical(expression[[2L]], as.name(".preserve_projected_stack_mass"))
}, logical(1)))
stopifnot(length(mass_helper_index) == 1L)
eval(demand_expressions[[mass_helper_index]], envir = .GlobalEnv)

mass_fixture <- tempfile("projection_mass_correction_")
dir.create(mass_fixture, recursive = TRUE)
on.exit(unlink(mass_fixture, recursive = TRUE, force = TRUE), add = TRUE)
source_1 <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2)
source_2 <- terra::rast(source_1)
terra::values(source_1) <- c(1, 2, 3, 4)
terra::values(source_2) <- c(2, 4, 6, 8)
source_paths <- file.path(mass_fixture, c("source_1.tif", "source_2.tif"))
terra::writeRaster(source_1, source_paths[[1L]], overwrite = TRUE)
terra::writeRaster(source_2, source_paths[[2L]], overwrite = TRUE)
biased_stack <- c(source_1 * 2, source_2 / 2)
mass_result <- .preserve_projected_stack_mass(
  biased_stack, source_paths, "Synthetic demand"
)
stopifnot(
  isTRUE(all.equal(
    as.numeric(terra::global(mass_result$raster, "sum", na.rm = TRUE)[, 1]),
    c(10, 20),
    tolerance = 1e-10
  )),
  isTRUE(all.equal(mass_result$audit$correction_factor, c(0.5, 2))),
  all(abs(mass_result$audit$residual_Mg) < 1e-10)
)

scenario_text <- paste(
  readLines(file.path(scripts_root, "6_scenarios_v4.R"), warn = FALSE),
  collapse = "\n"
)
stopifnot(
  grepl("fricc_ww_preborder", scenario_text, fixed = TRUE),
  grepl("borders_reclass", scenario_text, fixed = TRUE),
  grepl("fricc_ww <- overlay", scenario_text, fixed = TRUE)
)

egoml_path <- file.path(
  scripts_root,
  "10_dyn_Sc17_webmofuss_ctrees_g_v11.egoml"
)
egoml_text <- paste(readLines(egoml_path, warn = FALSE), collapse = "\n")
stopifnot(
  grepl("W_origin_demand.csv", egoml_text, fixed = TRUE),
  grepl("In/W_origin_components/IDW_C++_fw_w", egoml_text, fixed = TRUE),
  grepl("V_origin_demand.csv", egoml_text, fixed = TRUE),
  grepl("In/V_origin_components/IDW_C++_fw_v", egoml_text, fixed = TRUE),
  grepl("ForEach", egoml_text, fixed = TRUE)
)

# MuxMap emits the accumulator entering the current ForEach iteration. The
# feedback map is therefore the only output that contains the final origin.
# Downstream W consumers must use v368; v87 remains only the internal
# accumulator input for the next iteration.
count_fixed <- function(pattern, text) {
  hits <- gregexpr(pattern, text, fixed = TRUE)[[1L]]
  if (identical(hits[[1L]], -1L)) 0L else length(hits)
}
stopifnot(
  count_fixed('<inputport name="feedback" peerid="v368" />', egoml_text) == 1L,
  count_fixed('<inputport name="map" peerid="v368" />', egoml_text) == 3L,
  count_fixed('<inputport name="map" peerid="v87" />', egoml_text) == 1L,
  count_fixed('<inputport name="feedback" peerid="v94" />', egoml_text) == 1L,
  count_fixed('<inputport name="map" peerid="v371" />', egoml_text) == 1L
)

# V domestic components occupy disjoint country domains. Their accumulator must
# take the union of non-null cells; plain i1 + i2 would retain only successive
# domain intersections and can reduce the final V allocation to zero.
v_accumulator_start <- regexpr(
  'value="Add normalized V directional component"',
  egoml_text,
  fixed = TRUE
)[[1L]]
stopifnot(v_accumulator_start > 0L)
v_accumulator_tail <- substr(
  egoml_text,
  v_accumulator_start,
  nchar(egoml_text)
)
v_accumulator_end <- regexpr(
  "</containerfunctor>",
  v_accumulator_tail,
  fixed = TRUE
)[[1L]]
stopifnot(v_accumulator_end > 0L)
v_accumulator_text <- substr(
  v_accumulator_tail,
  1L,
  v_accumulator_end + nchar("</containerfunctor>") - 1L
)
stopifnot(
  grepl("if isNull(i1) then", v_accumulator_text, fixed = TRUE),
  grepl("else if isNull(i2) then", v_accumulator_text, fixed = TRUE),
  grepl("i1 + i2", v_accumulator_text, fixed = TRUE),
  !grepl(
    '<inputport name="expression">[&#x0A;    i1 + i2&#x0A;]</inputport>',
    v_accumulator_text,
    fixed = TRUE
  )
)
null_safe_union <- function(left, right) {
  ifelse(is.na(left), right, ifelse(is.na(right), left, left + right))
}
stopifnot(isTRUE(all.equal(
  null_safe_union(c(1, NA, 2, NA), c(NA, 3, 4, NA)),
  c(1, 3, 6, NA)
)))

deployment_text <- paste(
  readLines(file.path(scripts_root, "2_copy_files_v4.R"), warn = FALSE),
  collapse = "\n"
)
retained_models <- "10_dyn_Sc17_webmofuss_ctrees_g_v11.egoml"
v8_dependencies <- c(
  "rnorm_v8.R",
  "NRB_graphs_datasets_v8.R",
  "maps_animations_v8.R",
  "finalogs_v8.R",
  "bypassMC_v8.R",
  "bypass_maps_animations_v8.R",
  "LaTeX/generate_modern_report_v8.R"
)
stopifnot(
  all(file.exists(file.path(scripts_root, retained_models))),
  all(file.exists(file.path(scripts_root, v8_dependencies))),
  all(vapply(
    c(retained_models, v8_dependencies),
    grepl,
    logical(1),
    x = deployment_text,
    fixed = TRUE
  )),
  !grepl("v7_egoml", deployment_text, fixed = TRUE),
  !grepl("v7_r_dependencies", deployment_text, fixed = TRUE)
)

# Exercise the installer with two W origins, two V directional jobs, three
# decennial IDW periods, and twenty-one annual origin-demand lookup tables.
# The terminal period has exactly zero demand and all-zero HC outputs.
old_autorun <- Sys.getenv("MOFUSS_6F_NO_AUTORUN", unset = NA_character_)
Sys.setenv(MOFUSS_6F_NO_AUTORUN = "1")
on.exit({
  if (is.na(old_autorun)) {
    Sys.unsetenv("MOFUSS_6F_NO_AUTORUN")
  } else {
    Sys.setenv(MOFUSS_6F_NO_AUTORUN = old_autorun)
  }
}, add = TRUE)
source(file.path(scripts_root, "9_install_directional_IDW_outputs_v4.R"))

fixture <- tempfile("w_origin_preserving_")
dir.create(fixture, recursive = TRUE)
on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

in_root <- file.path(fixture, "In")
hc_root <- file.path(in_root, "DemandScenarios", "HC_jobs")
dir.create(hc_root, recursive = TRUE)

template <- terra::rast(
  nrows = 2,
  ncols = 3,
  xmin = 0,
  xmax = 3000,
  ymin = 0,
  ymax = 2000,
  crs = "EPSG:3857"
)
terra::values(template) <- 1
terra::writeRaster(template, file.path(in_root, "fricc_w.tif"), overwrite = TRUE)
terra::writeRaster(template, file.path(in_root, "fricc_v.tif"), overwrite = TRUE)

jobs <- data.frame(
  JobID = c("W_AAA_ORIGIN", "W_BBB_ORIGIN", "V_IMPORTERS", "V_DOMESTIC"),
  Channel = c("W", "W", "V", "V"),
  Status = "IDW_READY",
  PeriodStart = 1L,
  PeriodEnd = 21L,
  YearStart = 2000L,
  YearEnd = 2020L,
  SourceDomainMask = NA_character_,
  CombineOperation = c(
    "runtime_normalize_by_origin_then_sum",
    "runtime_normalize_by_origin_then_sum",
    "runtime_normalize_by_origin_then_sum",
    "runtime_normalize_by_origin_then_sum"
  ),
  OutputRole = c(
    "origin_preserving_W_pressure_component",
    "origin_preserving_W_pressure_component",
    "origin_preserving_V_pressure_component",
    "origin_preserving_V_pressure_component"
  ),
  DemandISO3 = c("AAA", "BBB", "AAA;CCC", "BBB"),
  AllowedSourceISO3 = c(
    "AAA;BBB", "AAA;BBB", "AAA;BBB;CCC", "BBB"
  ),
  DirectionRule = c(
    "origin_country_demand_regional_sources",
    "origin_country_demand_regional_sources",
    "importer_demand_regional_sources",
    "non_importer_demand_domestic_sources_only"
  ),
  DemandTable = NA_character_,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

mask_values <- list(
  W_AAA_ORIGIN = rep(1, 6),
  W_BBB_ORIGIN = rep(1, 6),
  V_IMPORTERS = c(1, 1, 1, 1, NA, NA),
  V_DOMESTIC = c(NA, NA, NA, NA, 1, 1)
)
annual_years <- 2000:2020
w_demand <- list(
  W_AAA_ORIGIN = c(seq(10, 29), 0),
  W_BBB_ORIGIN = c(seq(30, 49), 0)
)
v_demand <- list(
  V_IMPORTERS = c(seq(100, 119), 0),
  V_DOMESTIC = c(seq(50, 69), 0)
)

for (row_index in seq_len(nrow(jobs))) {
  job_id <- jobs$JobID[[row_index]]
  channel <- tolower(jobs$Channel[[row_index]])
  job_root <- file.path(hc_root, job_id)
  output_root <- file.path(hc_root, paste0("idw_", job_id))
  dir.create(job_root, recursive = TRUE)
  dir.create(output_root, recursive = TRUE)

  mask <- terra::setValues(terra::rast(template), mask_values[[job_id]])
  mask_path <- file.path(job_root, "source_domain_mask_c.tif")
  terra::writeRaster(mask, mask_path, overwrite = TRUE)
  jobs$SourceDomainMask[[row_index]] <- normalizePath(
    mask_path, winslash = "/", mustWork = TRUE
  )

  demand_path <- file.path(job_root, paste0("BaU_fwch_", channel, "_idw.csv"))
  if (channel == "w") {
    demand <- data.frame(ID = 1L, check.names = FALSE)
    for (year_index in seq_along(annual_years)) {
      demand[[paste0(annual_years[[year_index]], "_fw_w")]] <-
        w_demand[[job_id]][[year_index]]
    }
  } else {
    demand <- data.frame(ID = 1L, check.names = FALSE)
    for (year_index in seq_along(annual_years)) {
      demand[[paste0(annual_years[[year_index]], "_fw_v")]] <-
        v_demand[[job_id]][[year_index]]
    }
  }
  write.csv(demand, demand_path, row.names = FALSE, quote = FALSE)
  jobs$DemandTable[[row_index]] <- normalizePath(
    demand_path, winslash = "/", mustWork = TRUE
  )
}

write.csv(
  jobs,
  file.path(hc_root, "HC_job_manifest_idw_ready.csv"),
  row.names = FALSE,
  quote = TRUE,
  na = ""
)

for (year_index in seq_along(annual_years)) {
  w_lookup <- data.frame(
    Key = 1L,
    Value = w_demand$W_AAA_ORIGIN[[year_index]] +
      w_demand$W_BBB_ORIGIN[[year_index]]
  )
  write.csv(
    w_lookup,
    file.path(
      in_root,
      "DemandScenarios",
      sprintf("fwuse_W_ext_fwdef%02d.csv", year_index)
    ),
    row.names = FALSE,
    quote = TRUE
  )
  v_lookup <- data.frame(
    Key = 1L,
    Value = v_demand$V_IMPORTERS[[year_index]] +
      v_demand$V_DOMESTIC[[year_index]]
  )
  write.csv(
    v_lookup,
    file.path(
      in_root,
      "DemandScenarios",
      sprintf("fwuse_V_ext_fwdef%02d.csv", year_index)
    ),
    row.names = FALSE,
    quote = TRUE
  )
}

periods <- c(1L, 11L, 21L)
for (period in periods) {
  increment <- if (period == 1L) 0 else 10
  rasters <- if (period == 21L) {
    list(
      W_AAA_ORIGIN = rep(0, 6),
      W_BBB_ORIGIN = rep(0, 6),
      V_IMPORTERS = rep(0, 6),
      V_DOMESTIC = rep(0, 6)
    )
  } else {
    list(
      W_AAA_ORIGIN = c(1, 2, 3, 4, 5, 6) + increment,
      W_BBB_ORIGIN = c(6, 5, 4, 3, 2, 1) + increment,
      V_IMPORTERS = c(1, 2, 3, 4, NA, NA) + increment,
      V_DOMESTIC = c(NA, NA, NA, NA, 5, 6) + increment
    )
  }
  for (row_index in seq_len(nrow(jobs))) {
    job_id <- jobs$JobID[[row_index]]
    channel <- tolower(jobs$Channel[[row_index]])
    output <- terra::setValues(terra::rast(template), rasters[[job_id]])
    terra::writeRaster(
      output,
      file.path(
        hc_root,
        paste0("idw_", job_id),
        sprintf("IDW_C++_fw_%s%02d.tif", channel, period)
      ),
      overwrite = TRUE
    )
  }
}

# A zero output remains an error when demand is positive.
positive_demand_path <- file.path(
  hc_root, "idw_W_AAA_ORIGIN", "IDW_C++_fw_w11.tif"
)
positive_demand_original <- terra::values(
  terra::rast(positive_demand_path),
  mat = FALSE
)
terra::writeRaster(
  terra::setValues(terra::rast(template), rep(0, 6)),
  positive_demand_path,
  overwrite = TRUE
)
positive_demand_error <- tryCatch(
  {
    install_directional_idw_outputs(fixture, dry_run = TRUE)
    NULL
  },
  error = identity
)
stopifnot(
  inherits(positive_demand_error, "error"),
  grepl("positive demand", conditionMessage(positive_demand_error))
)
terra::writeRaster(
  terra::setValues(terra::rast(template), positive_demand_original),
  positive_demand_path,
  overwrite = TRUE
)

# A positive output remains an error when demand is exactly zero.
zero_demand_path <- file.path(
  hc_root, "idw_W_AAA_ORIGIN", "IDW_C++_fw_w21.tif"
)
terra::writeRaster(
  terra::setValues(terra::rast(template), 1:6),
  zero_demand_path,
  overwrite = TRUE
)
zero_demand_error <- tryCatch(
  {
    install_directional_idw_outputs(fixture, dry_run = TRUE)
    NULL
  },
  error = identity
)
stopifnot(
  inherits(zero_demand_error, "error"),
  grepl("must be an all-zero raster", conditionMessage(zero_demand_error))
)
terra::writeRaster(
  terra::setValues(terra::rast(template), rep(0, 6)),
  zero_demand_path,
  overwrite = TRUE
)

dry <- install_directional_idw_outputs(fixture, dry_run = TRUE)
stopifnot(
  nrow(dry$components) == 12L,
  nrow(dry$outputs) == 18L,
  nrow(dry$w_component_index) == 2L,
  nrow(dry$v_component_index) == 2L,
  identical(dry$w_component_index$DemandISO3, c("AAA", "BBB")),
  identical(dry$v_component_index$DemandISO3, c("AAA;CCC", "BBB")),
  identical(as.numeric(dry$w_demand_matrix[, 1L]), c(10, 30)),
  identical(as.numeric(dry$v_demand_matrix[, 1L]), c(100, 50)),
  !file.exists(file.path(in_root, "IDW_C++_fw_w01.tif"))
)

installed <- install_directional_idw_outputs(fixture)
stopifnot(
  nrow(installed$outputs) == 18L,
  nrow(installed$w_demand_audit) == 21L,
  nrow(installed$v_demand_audit) == 21L
)

for (period in periods) {
  increment <- if (period == 1L) 0 else 10
  component_1 <- terra::values(terra::rast(file.path(
    in_root,
    "W_origin_components",
    sprintf("IDW_C++_fw_w001_%02d.tif", period)
  )), mat = FALSE)
  component_2 <- terra::values(terra::rast(file.path(
    in_root,
    "W_origin_components",
    sprintf("IDW_C++_fw_w002_%02d.tif", period)
  )), mat = FALSE)
  combined_w <- terra::values(terra::rast(file.path(
    in_root, sprintf("IDW_C++_fw_w%02d.tif", period)
  )), mat = FALSE)
  combined_v <- terra::values(terra::rast(file.path(
    in_root, sprintf("IDW_C++_fw_v%02d.tif", period)
  )), mat = FALSE)
  v_component_1 <- terra::values(terra::rast(file.path(
    in_root,
    "V_origin_components",
    sprintf("IDW_C++_fw_v001_%02d.tif", period)
  )), mat = FALSE)
  v_component_2 <- terra::values(terra::rast(file.path(
    in_root,
    "V_origin_components",
    sprintf("IDW_C++_fw_v002_%02d.tif", period)
  )), mat = FALSE)
  if (period == 21L) {
    stopifnot(
      all(component_1 == 0),
      all(component_2 == 0),
      all(combined_w == 0),
      all(v_component_1 == 0),
      all(v_component_2 == 0),
      all(combined_v == 0)
    )
  } else {
    stopifnot(
      isTRUE(all.equal(component_1, as.numeric(1:6 + increment))),
      isTRUE(all.equal(component_2, as.numeric(6:1 + increment))),
      isTRUE(all.equal(combined_w, component_1 + component_2)),
      isTRUE(all.equal(
        v_component_1,
        as.numeric(c(1, 2, 3, 4, NA, NA) + increment)
      )),
      isTRUE(all.equal(
        v_component_2,
        as.numeric(c(NA, NA, NA, NA, 5, 6) + increment)
      )),
      isTRUE(all.equal(combined_v, as.numeric(1:6 + increment)))
    )
  }
}

lookup_2005 <- read.csv(
  file.path(in_root, "DemandScenarios", "W_origin_demand06.csv"),
  check.names = FALSE
)
component_index <- read.csv(
  file.path(in_root, "DemandScenarios", "W_origin_component_index.csv"),
  check.names = FALSE
)
v_lookup_2005 <- read.csv(
  file.path(in_root, "DemandScenarios", "V_origin_demand06.csv"),
  check.names = FALSE
)
v_component_index <- read.csv(
  file.path(in_root, "DemandScenarios", "V_origin_component_index.csv"),
  check.names = FALSE
)
audit <- read.csv(
  file.path(hc_root, "HC_IDW_install_manifest.csv"),
  check.names = FALSE
)
stopifnot(
  identical(lookup_2005$Key, 1:2),
  identical(as.numeric(lookup_2005$Value), c(15, 35)),
  identical(component_index$DemandISO3, c("AAA", "BBB")),
  identical(v_lookup_2005$Key, 1:2),
  identical(as.numeric(v_lookup_2005$Value), c(105, 55)),
  identical(v_component_index$DemandISO3, c("AAA;CCC", "BBB")),
  nrow(audit) == 18L,
  all(nzchar(audit$OutputSHA256)),
  all(file.exists(audit$TargetPath))
)

existing_error <- tryCatch(
  {
    install_directional_idw_outputs(fixture)
    NULL
  },
  error = identity
)
stopifnot(
  inherits(existing_error, "error"),
  grepl("Refusing to overwrite", conditionMessage(existing_error))
)

cat("WV_ORIGIN_PRESERVING_V4_OK\n")
