# Synthetic regression tests for single-component IDW compatibility inputs.

suppressPackageStartupMessages(library(terra))

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
stopifnot(length(script_argument) == 1L)
test_script <- sub("^--file=", "", script_argument)
repository_root <- normalizePath(
  file.path(dirname(test_script), "..", "..", ".."),
  winslash = "/",
  mustWork = TRUE
)
installer_path <- file.path(
  repository_root,
  "localhost",
  "scripts",
  "9_install_directional_IDW_outputs_v4.R"
)

previous_no_autorun <- Sys.getenv("MOFUSS_6F_NO_AUTORUN", unset = NA_character_)
Sys.setenv(MOFUSS_6F_NO_AUTORUN = "1")
on.exit({
  if (is.na(previous_no_autorun)) {
    Sys.unsetenv("MOFUSS_6F_NO_AUTORUN")
  } else {
    Sys.setenv(MOFUSS_6F_NO_AUTORUN = previous_no_autorun)
  }
}, add = TRUE)
source(installer_path, local = .GlobalEnv)

write_parameters <- function(run_root, values) {
  path <- file.path(
    run_root, "LULCC", "DownloadedDatasets", "SourceDataGlobal",
    "parameters.csv"
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(
    data.frame(
      Var = names(values),
      ParCHR = unname(values),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    path,
    row.names = FALSE,
    quote = TRUE,
    na = ""
  )
  path
}

write_region_index <- function(run_root, iso3, regional_code) {
  path <- file.path(
    run_root, "LULCC", "DownloadedDatasets", "SourceDataGlobal",
    "demand", "demand_in", "mofuss_regions0.gpkg"
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  polygons <- lapply(seq_along(iso3), function(index) {
    x0 <- index - 1
    terra::vect(
      matrix(
        c(
          x0, 0,
          x0 + 0.8, 0,
          x0 + 0.8, 0.8,
          x0, 0.8,
          x0, 0
        ),
        ncol = 2,
        byrow = TRUE
      ),
      type = "polygons",
      crs = "EPSG:4326"
    )
  })
  regions <- do.call(rbind, polygons)
  regions$GID_0 <- iso3
  regions$mofuss_reg <- rep(regional_code, length(iso3))
  terra::writeVector(regions, path, overwrite = TRUE)
  path
}

fixture <- tempfile("single_component_idw_")
dir.create(file.path(fixture, "In", "DemandScenarios"), recursive = TRUE)
on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

invisible(write_parameters(
  fixture,
  c(
    byregion = "Regional",
    aoi_poly = "0",
    region2BprocessedReg = "SSA_adm0_single",
    region2BprocessedCtry_iso = "RWA",
    start_year = "2000",
    end_year = "2020"
  )
))
invisible(write_region_index(fixture, "AAA", "SSA_adm0_single"))

template <- terra::rast(
  nrows = 2,
  ncols = 3,
  xmin = 0,
  xmax = 3,
  ymin = 0,
  ymax = 2,
  crs = "EPSG:3857"
)
terra::values(template) <- 1
terra::writeRaster(template, file.path(fixture, "In", "fricc_w.tif"))
terra::writeRaster(template, file.path(fixture, "In", "fricc_v.tif"))

annual_periods <- 1:21
w_demand <- c(seq.int(10, 29), 0)
v_demand <- c(seq.int(50, 69), 0)
for (period in annual_periods) {
  write.csv(
    data.frame(Key = 1L, Value = w_demand[[period]]),
    file.path(
      fixture, "In", "DemandScenarios",
      sprintf("fwuse_W_ext_fwdef%02d.csv", period)
    ),
    row.names = FALSE
  )
  write.csv(
    data.frame(Key = 1L, Value = v_demand[[period]]),
    file.path(
      fixture, "In", "DemandScenarios",
      sprintf("fwuse_V_ext_fwdef%02d.csv", period)
    ),
    row.names = FALSE
  )
}

idw_periods <- c(1L, 11L, 21L)
for (period in idw_periods) {
  w_idw <- terra::rast(template)
  v_idw <- terra::rast(template)
  terra::values(w_idw) <- if (period == 21L) 0 else period + 1:6
  terra::values(v_idw) <- if (period == 21L) 0 else period + 11:16
  terra::writeRaster(
    w_idw,
    file.path(fixture, "In", sprintf("IDW_C++_fw_w%02d.tif", period))
  )
  terra::writeRaster(
    v_idw,
    file.path(fixture, "In", sprintf("IDW_C++_fw_v%02d.tif", period))
  )
}

dry_run <- install_directional_idw_outputs(fixture, dry_run = TRUE)
stopifnot(
  identical(dry_run$mode, "single_component"),
  identical(dry_run$scope$byregion, "Regional"),
  identical(dry_run$scope$country_iso3, "AAA"),
  nrow(dry_run$outputs) == 6L,
  !dir.exists(file.path(fixture, "In", "W_origin_components")),
  !dir.exists(file.path(fixture, "In", "V_origin_components"))
)

installed <- install_directional_idw_outputs(fixture)
stopifnot(
  identical(installed$mode, "single_component"),
  identical(installed$scope$country_iso3, "AAA"),
  nrow(installed$w_demand_audit) == 21L,
  nrow(installed$v_demand_audit) == 21L
)

for (channel in c("w", "v")) {
  for (period in idw_periods) {
    source_path <- file.path(
      fixture, "In", sprintf("IDW_C++_fw_%s%02d.tif", channel, period)
    )
    component_path <- file.path(
      fixture,
      "In",
      paste0(toupper(channel), "_origin_components"),
      sprintf("IDW_C++_fw_%s001_%02d.tif", channel, period)
    )
    stopifnot(
      file.exists(component_path),
      identical(.idw6f_sha256(source_path), .idw6f_sha256(component_path))
    )
  }
}

w_origin_06 <- read.csv(file.path(
  fixture, "In", "DemandScenarios", "W_origin_demand06.csv"
))
v_origin_21 <- read.csv(file.path(
  fixture, "In", "DemandScenarios", "V_origin_demand21.csv"
))
w_index <- read.csv(file.path(
  fixture, "In", "DemandScenarios", "W_origin_component_index.csv"
))
audit <- read.csv(file.path(
  fixture, "In", "DemandScenarios",
  "SINGLE_COMPONENT_IDW_install_manifest.csv"
))
stopifnot(
  identical(names(w_origin_06), c("Key", "Value")),
  identical(w_origin_06$Key, 1L),
  w_origin_06$Value == w_demand[[6L]],
  identical(v_origin_21$Key, 1L),
  v_origin_21$Value == 0,
  identical(w_index$DemandISO3, "AAA"),
  nrow(audit) == 6L,
  all(audit$DemandISO3 == "AAA")
)

overwrite_error <- tryCatch(
  {
    install_directional_idw_outputs(fixture)
    NULL
  },
  error = identity
)
stopifnot(
  inherits(overwrite_error, "error"),
  grepl("Refusing to overwrite", conditionMessage(overwrite_error), fixed = TRUE)
)

country_fixture <- tempfile("country_scope_")
dir.create(country_fixture)
on.exit(unlink(country_fixture, recursive = TRUE, force = TRUE), add = TRUE)
invisible(write_parameters(
  country_fixture,
  c(
    byregion = "Country",
    region2BprocessedCtry_iso = "xyz",
    start_year = "2000",
    end_year = "2000"
  )
))
country_scope <- .idw6f_resolve_single_country_scope(country_fixture)
stopifnot(
  identical(country_scope$byregion, "Country"),
  identical(country_scope$country_iso3, "XYZ")
)

regional_fixture <- tempfile("regional_multicountry_scope_")
dir.create(regional_fixture)
on.exit(unlink(regional_fixture, recursive = TRUE, force = TRUE), add = TRUE)
invisible(write_parameters(
  regional_fixture,
  c(
    byregion = "Regional",
    aoi_poly = "0",
    region2BprocessedReg = "SSA_adm0_pair",
    region2BprocessedCtry_iso = "RWA",
    start_year = "2000",
    end_year = "2000"
  )
))
invisible(write_region_index(
  regional_fixture,
  c("AAA", "BBB"),
  "SSA_adm0_pair"
))
regional_error <- tryCatch(
  {
    .idw6f_resolve_single_country_scope(regional_fixture)
    NULL
  },
  error = identity
)
stopifnot(
  inherits(regional_error, "error"),
  grepl(
    "directional HC manifest is required",
    conditionMessage(regional_error),
    fixed = TRUE
  )
)

message("Single-component IDW compatibility tests passed.")
