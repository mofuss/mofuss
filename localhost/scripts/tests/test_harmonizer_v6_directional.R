suppressPackageStartupMessages(library(terra))

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
stopifnot(length(script_argument) == 1L)
test_script <- sub("^--file=", "", script_argument)
repository_root <- normalizePath(
  file.path(dirname(test_script), "..", "..", ".."),
  winslash = "/",
  mustWork = TRUE
)
script_path <- file.path(
  repository_root,
  "localhost",
  "scripts",
  "5_harmonizer_v6.R"
)
expressions <- parse(file = script_path)

load_definition <- function(name) {
  matches <- which(vapply(expressions, function(expression) {
    is.call(expression) &&
      identical(expression[[1L]], as.name("<-")) &&
      identical(as.character(expression[[2L]]), name)
  }, logical(1)))
  stopifnot(length(matches) == 1L)
  eval(expressions[[matches]], envir = .GlobalEnv)
}

for (name in c(
  "align_raster_to_template",
  ".hc_manifest_flag",
  ".safe_hc_relative_path",
  ".location_ids_or_stop",
  ".remap_location_ids_to_template",
  ".source_domain_mask_on_template",
  ".write_harmonized_hc_raster",
  ".harmonize_directional_hc_jobs"
)) {
  load_definition(name)
}

fixture <- tempfile("harmonizer_v6_directional_")
dir.create(fixture, recursive = TRUE)
on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

hc_root <- file.path(fixture, "In", "DemandScenarios", "HC_jobs")
temporary_root <- file.path(fixture, "LULCC", "TempRaster", "HC_jobs")
temporary_manifest <- file.path(
  fixture,
  "LULCC",
  "TempTables",
  "HC_job_manifest_harmonized.csv"
)
dir.create(hc_root, recursive = TRUE)

template <- terra::rast(
  nrows = 4,
  ncols = 4,
  xmin = 0,
  xmax = 4,
  ymin = 0,
  ymax = 4,
  crs = "EPSG:3857"
)
terra::values(template) <- 1

source_polygons <- terra::vect(
  data.frame(
    GID_0 = c("AAA", "BBB"),
    geometry = c(
      "POLYGON ((0 0, 2 0, 2 4, 0 4, 0 0))",
      "POLYGON ((2 0, 4 0, 4 4, 2 4, 2 0))"
    ),
    stringsAsFactors = FALSE
  ),
  geom = "geometry",
  crs = "EPSG:3857"
)

jobs <- data.frame(
  JobID = c("W_TEST", "V_TEST_IMPORTERS", "V_TEST_DOMESTIC"),
  Channel = c("W", "V", "V"),
  AllowedSourceISO3 = c("AAA;BBB", "AAA;BBB", "AAA"),
  DemandRows = c(2L, 2L, 2L),
  DemandTable = c(
    "W_TEST/BaU_fwch_w.csv",
    "V_TEST_IMPORTERS/BaU_fwch_v.csv",
    "V_TEST_DOMESTIC/BaU_fwch_v.csv"
  ),
  LocationsRaster = c(
    "W_TEST/locs_raster_w.tif",
    "V_TEST_IMPORTERS/locs_raster_v.tif",
    "V_TEST_DOMESTIC/locs_raster_v.tif"
  ),
  SourceDomainMaskRaw = c(
    "W_TEST/source_domain_mask_raw.tif",
    "V_TEST_IMPORTERS/source_domain_mask_raw.tif",
    "V_TEST_DOMESTIC/source_domain_mask_raw.tif"
  ),
  RunOnHCCluster = TRUE,
  CombineOperation = c(
    "use_directly",
    "pixelwise_sum_by_year",
    "pixelwise_sum_by_year"
  ),
  Status = "ready",
  stringsAsFactors = FALSE
)

high_ids <- list(
  W_TEST = c(16777217L, 7L),
  V_TEST_IMPORTERS = c(18059274L, 9L),
  V_TEST_DOMESTIC = c(18059273L, 8L)
)

for (row_number in seq_len(nrow(jobs))) {
  job_id <- jobs$JobID[[row_number]]
  channel <- tolower(jobs$Channel[[row_number]])
  job_dir <- file.path(hc_root, job_id)
  dir.create(job_dir, recursive = TRUE)

  # Deliberately shift the raw one-unit grid relative to the template. A
  # nearest-neighbour raster resample is not guaranteed to preserve unique
  # categorical IDs in this situation; centre-to-cell transfer must do so.
  location <- terra::rast(
    nrows = 3,
    ncols = 3,
    xmin = 0.4,
    xmax = 3.4,
    ymin = 0.4,
    ymax = 3.4,
    crs = "EPSG:3857"
  )
  location_values <- rep(NA_integer_, terra::ncell(location))
  location_values[c(1L, 2L)] <- high_ids[[job_id]]
  terra::values(location) <- location_values
  terra::writeRaster(
    location,
    file.path(job_dir, paste0("locs_raster_", channel, ".tif")),
    datatype = "INT4S",
    overwrite = TRUE
  )

  source_mask <- location
  terra::values(source_mask) <- 1L
  terra::writeRaster(
    source_mask,
    file.path(job_dir, "source_domain_mask_raw.tif"),
    datatype = "INT1U",
    overwrite = TRUE
  )

  demand <- data.frame(ID = high_ids[[job_id]], value = c(1, 2))
  names(demand)[[2L]] <- paste0("2000_fw_", channel)
  write.csv(
    demand,
    file.path(job_dir, paste0("BaU_fwch_", channel, ".csv")),
    row.names = FALSE,
    quote = FALSE
  )
}

write.csv(
  jobs,
  file.path(hc_root, "HC_job_manifest.csv"),
  row.names = FALSE,
  quote = TRUE
)

stopifnot(isTRUE(.harmonize_directional_hc_jobs(
  hc_root = hc_root,
  template = template,
  source_polygons = source_polygons,
  temporary_root = temporary_root,
  temporary_manifest = temporary_manifest
)))

harmonized_manifest <- read.csv(
  file.path(hc_root, "HC_job_manifest_harmonized.csv"),
  stringsAsFactors = FALSE
)
stopifnot(
  nrow(harmonized_manifest) == 3L,
  all(harmonized_manifest$HarmonizationStatus == "ready"),
  all(harmonized_manifest$PreservedAllDemandIDs),
  all(harmonized_manifest$HarmonizedLocationIDs == 2L),
  all(harmonized_manifest$MaxLocationShiftMapUnits > 0),
  identical(
    harmonized_manifest$SourceMaskMethod,
    c(
      "analysis_mask_all_selected_countries",
      "analysis_mask_all_selected_countries",
      "allowed_iso3_polygons_on_analysis_grid"
    )
  ),
  file.exists(temporary_manifest)
)

for (row_number in seq_len(nrow(jobs))) {
  job_id <- jobs$JobID[[row_number]]
  channel <- tolower(jobs$Channel[[row_number]])
  location <- terra::rast(file.path(
    hc_root,
    job_id,
    paste0("locs_c_", channel, ".tif")
  ))
  source_mask <- terra::rast(file.path(
    hc_root,
    job_id,
    "source_domain_mask_c.tif"
  ))
  raw_location <- terra::rast(file.path(
    hc_root,
    job_id,
    paste0("locs_raster_", channel, ".tif")
  ))
  raw_cells <- which(!is.na(terra::values(raw_location, mat = FALSE)))
  mapped_cells <- terra::cellFromXY(
    template,
    terra::xyFromCell(raw_location, raw_cells)
  )
  location_values <- terra::values(location, mat = FALSE)
  source_values <- terra::values(source_mask, mat = FALSE)
  expected_source_cells <- if (
    jobs$AllowedSourceISO3[[row_number]] == "AAA"
  ) 8L else 16L
  stopifnot(
    all(terra::datatype(location) == "INT4S"),
    setequal(
      as.numeric(terra::values(location, mat = FALSE, na.rm = TRUE)),
      as.numeric(high_ids[[job_id]])
    ),
    identical(
      as.numeric(location_values[mapped_cells]),
      as.numeric(high_ids[[job_id]])
    ),
    all(terra::datatype(source_mask) == "INT1U"),
    sum(!is.na(source_values)) == expected_source_cells
  )
}

cat("HARMONIZER_V6_DIRECTIONAL_OK\n")
