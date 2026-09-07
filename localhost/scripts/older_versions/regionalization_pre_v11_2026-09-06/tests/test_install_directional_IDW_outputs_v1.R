suppressPackageStartupMessages(library(terra))

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
stopifnot(length(script_argument) == 1L)
test_script <- sub("^--file=", "", script_argument)
repository_root <- normalizePath(
  file.path(dirname(test_script), "..", "..", ".."),
  winslash = "/",
  mustWork = TRUE
)

old_autorun <- Sys.getenv("MOFUSS_6F_NO_AUTORUN", unset = NA_character_)
Sys.setenv(MOFUSS_6F_NO_AUTORUN = "1")
on.exit({
  if (is.na(old_autorun)) {
    Sys.unsetenv("MOFUSS_6F_NO_AUTORUN")
  } else {
    Sys.setenv(MOFUSS_6F_NO_AUTORUN = old_autorun)
  }
}, add = TRUE)
source(file.path(
  repository_root,
  "localhost",
  "scripts",
  "6f_install_directional_IDW_outputs_v1.R"
))

fixture <- tempfile("install_directional_idw_")
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
terra::values(template) <- rep(1, 6)
terra::writeRaster(template, file.path(in_root, "fricc_w.tif"), overwrite = TRUE)
terra::writeRaster(template, file.path(in_root, "fricc_v.tif"), overwrite = TRUE)

jobs <- data.frame(
  JobID = c("W_TEST", "V_IMPORTERS", "V_DOMESTIC"),
  Channel = c("W", "V", "V"),
  Status = "IDW_READY",
  PeriodStart = 1,
  PeriodEnd = 11,
  YearStart = 2000,
  YearEnd = 2010,
  SourceDomainMask = NA_character_,
  CombineOperation = c(
    "use_directly", "pixelwise_sum_by_year", "pixelwise_sum_by_year"
  ),
  OutputRole = c(
    "regional_W_pressure",
    "directional_V_pressure_component",
    "directional_V_pressure_component"
  ),
  stringsAsFactors = FALSE
)

mask_values <- list(
  W_TEST = rep(1, 6),
  V_IMPORTERS = c(1, 1, 1, 1, NA, NA),
  V_DOMESTIC = c(NA, NA, NA, NA, 1, 1)
)
for (job_id in jobs$JobID) {
  job_root <- file.path(hc_root, job_id)
  output_root <- file.path(hc_root, paste0("idw_", job_id))
  dir.create(job_root, recursive = TRUE)
  dir.create(output_root, recursive = TRUE)
  mask <- terra::setValues(terra::rast(template), mask_values[[job_id]])
  mask_path <- file.path(job_root, "source_domain_mask_c.tif")
  terra::writeRaster(mask, mask_path, overwrite = TRUE)
  jobs$SourceDomainMask[jobs$JobID == job_id] <- normalizePath(
    mask_path, winslash = "/", mustWork = TRUE
  )
}
write.csv(
  jobs,
  file.path(hc_root, "HC_job_manifest_idw_ready.csv"),
  row.names = FALSE,
  quote = TRUE,
  na = ""
)

periods <- c(1L, 11L)
for (period in periods) {
  increment <- if (period == 1L) 0 else 10
  w <- terra::setValues(terra::rast(template), 1:6 + increment)
  v_importers <- terra::setValues(
    terra::rast(template), c(1:4 + increment, NA, NA)
  )
  v_domestic <- terra::setValues(
    terra::rast(template), c(NA, NA, NA, NA, 5:6 + increment)
  )
  terra::writeRaster(
    w,
    file.path(hc_root, "idw_W_TEST", sprintf("IDW_C++_fw_w%02d.tif", period)),
    overwrite = TRUE
  )
  terra::writeRaster(
    v_importers,
    file.path(hc_root, "idw_V_IMPORTERS", sprintf("IDW_C++_fw_v%02d.tif", period)),
    overwrite = TRUE
  )
  terra::writeRaster(
    v_domestic,
    file.path(hc_root, "idw_V_DOMESTIC", sprintf("IDW_C++_fw_v%02d.tif", period)),
    overwrite = TRUE
  )
}

dry <- install_directional_idw_outputs(fixture, dry_run = TRUE)
stopifnot(
  nrow(dry$components) == 6L,
  nrow(dry$outputs) == 4L,
  !any(file.exists(file.path(
    in_root,
    c("IDW_C++_fw_w01.tif", "IDW_C++_fw_v01.tif")
  )))
)

installed <- install_directional_idw_outputs(fixture)
stopifnot(nrow(installed$outputs) == 4L)
for (period in periods) {
  increment <- if (period == 1L) 0 else 10
  w_values <- terra::values(terra::rast(file.path(
    in_root, sprintf("IDW_C++_fw_w%02d.tif", period)
  )), mat = FALSE)
  v_values <- terra::values(terra::rast(file.path(
    in_root, sprintf("IDW_C++_fw_v%02d.tif", period)
  )), mat = FALSE)
  stopifnot(
    isTRUE(all.equal(w_values, as.numeric(1:6 + increment))),
    isTRUE(all.equal(v_values, as.numeric(1:6 + increment)))
  )
}
audit <- read.csv(
  file.path(hc_root, "HC_IDW_install_manifest.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
stopifnot(
  nrow(audit) == 4L,
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

cat("6f directional IDW installation tests passed.\n")
