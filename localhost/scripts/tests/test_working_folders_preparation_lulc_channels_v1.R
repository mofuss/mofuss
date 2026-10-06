# Small end-to-end fixture for YES-driven MODIS and Woodman folder preparation.
# The fake raster payloads test copying and routing, not raster calculations.

scratch <- tempfile("woodman_prep_channels_", tmpdir = tempdir())
dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
stopifnot(dir.exists(scratch))
seed <- file.path(scratch, "_1000m_")
global <- file.path(seed, "LULCC", "DownloadedDatasets", "SourceDataGlobal")
dir.create(file.path(global, "InRaster"), recursive = TRUE)
writeLines("static MODIS fixture", file.path(global, "InRaster",
                                          "modis_lc_type1_pcs.tif"))

parameters <- read.csv(
  "localhost/scripts/working_folders_prep/parameters.csv",
  colClasses = "character", check.names = FALSE, na.strings = NULL
)
stopifnot(!any(c("LULCt3map_dataset", "woodman_series_dir") %in%
                 parameters$Var))
parameters$ParCHR[parameters$Var == "end_year"] <- "2001"
parameters$ParCHR[parameters$Var == "start_year"] <- "2000"
parameters$ParCHR[parameters$Var == "region2BprocessedReg"] <- "SSA_adm0_MDG"
parameters$ParCHR[parameters$Var %in% c("LULCt1map", "LULCt3map")] <- "YES"
parameter_path <- file.path(scratch, "parameters.csv")
write.csv(parameters, parameter_path, row.names = FALSE, quote = FALSE,
          na = "")
woodman_rasters <- c(
  "woodman_zone_pcs.tif",
  sprintf("woodman_luc_%d_pcs.tif", 2000:2001),
  sprintf("pre%d_v1_woodman_luc_pcs.tif", 2000:2001),
  sprintf("woodman_tof_%d_pcs.tif", 2000:2001)
)
woodman_tables <- c("growth_parameters_v3_woodman.csv",
                    "woodman_key_crosswalk.csv")
for (name in woodman_rasters[-1L]) {
  writeLines(paste("fixture", name), file.path(global, "InRaster", name))
}

rscript <- file.path(R.home("bin"),
                     if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
helper <- normalizePath(
  "localhost/scripts/working_folders_prep/working_folders_preparation_localhost.R",
  winslash = "/", mustWork = TRUE
)
run_helper <- function(output_dir, dry_run = FALSE) {
  arguments <- c(
    shQuote(helper),
    paste0("--parameters=", shQuote(parameter_path)),
    paste0("--template=", shQuote(seed)),
    paste0("--output-dir=", shQuote(output_dir)),
    if (dry_run) "--dry-run" else "--yes"
  )
  result <- suppressWarnings(system2(rscript, arguments, stdout = TRUE,
                                     stderr = TRUE))
  status <- attr(result, "status")
  list(output = result, status = if (is.null(status)) 0L else status)
}

# Projected rasters are still required, even though tables arrive later.
missing <- run_helper(scratch, dry_run = TRUE)
stopifnot(missing$status != 0L,
          any(grepl("missing a projected Woodman raster", missing$output)))
zone_path <- file.path(global, "InRaster", woodman_rasters[[1L]])
file.create(zone_path)
empty <- run_helper(scratch, dry_run = TRUE)
stopifnot(empty$status != 0L,
          any(grepl("empty Woodman raster", empty$output)))
writeLines("fixture woodman zone", zone_path)

# Missing seed InTables must not block any of the four variants. If tables
# happen to be present, the ordinary seed copy should still carry them along.
for (table_state in c("absent", "present")) {
  output_dir <- file.path(scratch, table_state)
  dir.create(output_dir)
  if (table_state == "present") {
    dir.create(file.path(global, "InTables"))
    for (name in woodman_tables) {
      writeLines(paste("fixture", name), file.path(global, "InTables", name))
    }
  }
  result <- run_helper(output_dir)
  if (result$status != 0L) stop(paste(result$output, collapse = "\n"))

  folders <- file.path(output_dir, c(
    "MDG_1000m_bau1_2001_mc3_capped",
    "MDG_1000m_bau1_2001_mc3_uncapped",
    "MDG_1000m_ics3_2001_mc3_capped",
    "MDG_1000m_ics3_2001_mc3_uncapped"
  ))
  for (folder in folders) {
    copied_global <- file.path(folder, "LULCC", "DownloadedDatasets",
                               "SourceDataGlobal")
    stopifnot(dir.exists(folder),
              file.exists(file.path(copied_global, "InRaster",
                                    "modis_lc_type1_pcs.tif")),
              all(file.exists(file.path(copied_global, "InRaster",
                                        woodman_rasters))))
    copied_tables <- file.path(copied_global, "InTables", woodman_tables)
    if (table_state == "present") {
      stopifnot(all(file.exists(copied_tables)))
    } else {
      stopifnot(!dir.exists(file.path(copied_global, "InTables")))
    }
    copied_parameters <- read.csv(file.path(copied_global, "parameters.csv"),
                                  colClasses = "character", check.names = FALSE,
                                  na.strings = NULL)
    parameter <- function(name) copied_parameters$ParCHR[
      copied_parameters$Var == name
    ]
    stopifnot(identical(parameter("LULCt1map"), "YES"),
              identical(parameter("LULCt3map"), "YES"),
              !any(c("LULCt3map_dataset", "woodman_series_dir") %in%
                     copied_parameters$Var))
  }
}
stopifnot(!file.exists(file.path(global, "parameters.csv")))
cat("Four-folder MODIS + Woodman fixture passed: seed tables optional, projected rasters required.\n")
