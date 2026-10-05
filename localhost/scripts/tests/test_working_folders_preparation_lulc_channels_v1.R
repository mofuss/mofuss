# Small end-to-end fixture for YES-driven MODIS and Woodman folder preparation.
# The fake raster payloads test copying and routing, not raster calculations.

scratch <- file.path(
  "E:/MoFuSS_Active", "woodman_prep_channels_fixture_2026-10-05",
  paste0("case_", Sys.getpid(), "_", format(Sys.time(), "%H%M%S"))
)
dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
stopifnot(dir.exists(scratch))
seed <- file.path(scratch, "_1000m_")
global <- file.path(seed, "LULCC", "DownloadedDatasets", "SourceDataGlobal")
dir.create(file.path(global, "InRaster"), recursive = TRUE)
dir.create(file.path(global, "InTables"), recursive = TRUE)
writeLines("static MODIS fixture", file.path(global, "InRaster",
                                          "modis_lc_type1_pcs.tif"))

parameters <- read.csv(
  "localhost/scripts/working_folders_prep/parameters.csv",
  colClasses = "character", check.names = FALSE, na.strings = NULL
)
stopifnot(!any(c("LULCt3map_dataset", "woodman_series_dir") %in%
                 parameters$Var))
parameters$ParCHR[parameters$Var == "end_year"] <- "2001"
parameter_path <- file.path(scratch, "parameters.csv")
write.csv(parameters, parameter_path, row.names = FALSE, quote = FALSE,
          na = "")
woodman_files <- c(
  "woodman_zone_pcs.tif",
  sprintf("woodman_luc_%d_pcs.tif", 2000:2001),
  sprintf("pre%d_v1_woodman_luc_pcs.tif", 2000:2001),
  sprintf("woodman_tof_%d_pcs.tif", 2000:2001),
  "growth_parameters_v3_woodman.csv", "woodman_key_crosswalk.csv"
)
for (name in woodman_files[1:7]) {
  writeLines(paste("fixture", name), file.path(global, "InRaster", name))
}
for (name in woodman_files[8:9]) {
  writeLines(paste("fixture", name), file.path(global, "InTables", name))
}

rscript <- file.path(R.home("bin"), "Rscript.exe")
helper <- normalizePath(
  "localhost/scripts/working_folders_prep/working_folders_preparation_localhost.R",
  winslash = "/", mustWork = TRUE
)
arguments <- c(
  shQuote(helper),
  paste0("--parameters=", shQuote(parameter_path)),
  paste0("--template=", shQuote(seed)),
  paste0("--output-dir=", shQuote(scratch)),
  "--yes"
)
result <- suppressWarnings(system2(rscript, arguments, stdout = TRUE,
                                   stderr = TRUE))
status <- attr(result, "status")
if (!is.null(status) && status != 0L) stop(paste(result, collapse = "\n"))

folders <- file.path(scratch, c(
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
            all(file.exists(c(
              file.path(copied_global, "InRaster", woodman_files[1:7]),
              file.path(copied_global, "InTables", woodman_files[8:9])
            ))))
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
cat("Four-folder MODIS + Woodman preparation fixture passed.\n")
