# Synthetic regression tests for the origin-preserving regional W workflow.

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
  "3_demand4IDW_v10.R",
  "5_harmonizer_v7.R",
  "6e_prepare_directional_IDW_inputs_v2.R",
  "6f_install_directional_IDW_outputs_v2.R"
)
for (filename in contract_files) {
  path <- file.path(scripts_root, filename)
  stopifnot(file.exists(path))
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  stopifnot(grepl("runtime_normalize_by_origin_then_sum", text, fixed = TRUE))
}

demand_text <- paste(
  readLines(file.path(scripts_root, "3_demand4IDW_v10.R"), warn = FALSE),
  collapse = "\n"
)
stopifnot(
  grepl("W_[A-Z]{3}_ORIGIN", demand_text) ||
    grepl('paste0("W_", origin_iso3, "_ORIGIN")', demand_text, fixed = TRUE),
  grepl("origin_country_demand_regional_sources", demand_text, fixed = TRUE),
  grepl("Origin-country W jobs do not exactly conserve", demand_text, fixed = TRUE)
)

scenario_text <- paste(
  readLines(file.path(scripts_root, "6a_scenarios_v4.R"), warn = FALSE),
  collapse = "\n"
)
stopifnot(
  grepl("fricc_ww_preborder", scenario_text, fixed = TRUE),
  grepl("borders_reclass", scenario_text, fixed = TRUE),
  grepl("fricc_ww <- overlay", scenario_text, fixed = TRUE)
)

egoml_path <- file.path(
  scripts_root,
  "7_dyn_Sc17_webmofuss_ctrees_g_v9.egoml"
)
egoml_text <- paste(readLines(egoml_path, warn = FALSE), collapse = "\n")
stopifnot(
  grepl("W_origin_demand.csv", egoml_text, fixed = TRUE),
  grepl("In/W_origin_components/IDW_C++_fw_w", egoml_text, fixed = TRUE),
  grepl("ForEach", egoml_text, fixed = TRUE)
)

deployment_text <- paste(
  readLines(file.path(scripts_root, "2_copy_files_v2.R"), warn = FALSE),
  collapse = "\n"
)
stopifnot(grepl(
  "7_dyn_Sc17_webmofuss_ctrees_g_v9.egoml",
  deployment_text,
  fixed = TRUE
))

# Exercise the installer with two W origins, two V directional jobs, two
# decennial IDW periods, and eleven annual origin-demand lookup tables.
old_autorun <- Sys.getenv("MOFUSS_6F_NO_AUTORUN", unset = NA_character_)
Sys.setenv(MOFUSS_6F_NO_AUTORUN = "1")
on.exit({
  if (is.na(old_autorun)) {
    Sys.unsetenv("MOFUSS_6F_NO_AUTORUN")
  } else {
    Sys.setenv(MOFUSS_6F_NO_AUTORUN = old_autorun)
  }
}, add = TRUE)
source(file.path(scripts_root, "6f_install_directional_IDW_outputs_v2.R"))

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
  PeriodEnd = 11L,
  YearStart = 2000L,
  YearEnd = 2010L,
  SourceDomainMask = NA_character_,
  CombineOperation = c(
    "runtime_normalize_by_origin_then_sum",
    "runtime_normalize_by_origin_then_sum",
    "pixelwise_sum_by_year",
    "pixelwise_sum_by_year"
  ),
  OutputRole = c(
    "origin_preserving_W_pressure_component",
    "origin_preserving_W_pressure_component",
    "directional_V_pressure_component",
    "directional_V_pressure_component"
  ),
  DemandISO3 = c("AAA", "BBB", "AAA", "BBB"),
  AllowedSourceISO3 = c("AAA;BBB", "AAA;BBB", "AAA;BBB", "BBB"),
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
annual_years <- 2000:2010
w_demand <- list(
  W_AAA_ORIGIN = seq(10, 20),
  W_BBB_ORIGIN = seq(30, 40)
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
    for (year in annual_years) demand[[paste0(year, "_fw_v")]] <- 1
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
  lookup <- data.frame(
    Key = 1L,
    Value = w_demand$W_AAA_ORIGIN[[year_index]] +
      w_demand$W_BBB_ORIGIN[[year_index]]
  )
  write.csv(
    lookup,
    file.path(
      in_root,
      "DemandScenarios",
      sprintf("fwuse_W_ext_fwdef%02d.csv", year_index)
    ),
    row.names = FALSE,
    quote = TRUE
  )
}

periods <- c(1L, 11L)
for (period in periods) {
  increment <- if (period == 1L) 0 else 10
  rasters <- list(
    W_AAA_ORIGIN = c(1, 2, 3, 4, 5, 6) + increment,
    W_BBB_ORIGIN = c(6, 5, 4, 3, 2, 1) + increment,
    V_IMPORTERS = c(1, 2, 3, 4, NA, NA) + increment,
    V_DOMESTIC = c(NA, NA, NA, NA, 5, 6) + increment
  )
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

dry <- install_directional_idw_outputs(fixture, dry_run = TRUE)
stopifnot(
  nrow(dry$components) == 8L,
  nrow(dry$outputs) == 8L,
  nrow(dry$w_component_index) == 2L,
  identical(dry$w_component_index$DemandISO3, c("AAA", "BBB")),
  identical(as.numeric(dry$w_demand_matrix[, 1L]), c(10, 30)),
  !file.exists(file.path(in_root, "IDW_C++_fw_w01.tif"))
)

installed <- install_directional_idw_outputs(fixture)
stopifnot(
  nrow(installed$outputs) == 8L,
  nrow(installed$demand_audit) == 11L
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
  stopifnot(
    isTRUE(all.equal(component_1, as.numeric(1:6 + increment))),
    isTRUE(all.equal(component_2, as.numeric(6:1 + increment))),
    isTRUE(all.equal(combined_w, component_1 + component_2)),
    isTRUE(all.equal(combined_v, as.numeric(1:6 + increment)))
  )
}

lookup_2005 <- read.csv(
  file.path(in_root, "DemandScenarios", "W_origin_demand06.csv"),
  check.names = FALSE
)
component_index <- read.csv(
  file.path(in_root, "DemandScenarios", "W_origin_component_index.csv"),
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
  nrow(audit) == 8L,
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

cat("W_ORIGIN_PRESERVING_V1_OK\n")
