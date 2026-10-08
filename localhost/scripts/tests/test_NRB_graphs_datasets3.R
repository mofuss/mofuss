# Integration test for NRB_graphs_datasets3.R.
# Reads completed Kenya CSVs and writes all products only inside a temporary fixture.

stopifnot(.Platform$OS.type == "windows")

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
test_args <- commandArgs(trailingOnly = TRUE)
script_name <- if (length(test_args)) test_args[[1L]] else "NRB_graphs_datasets3.R"
script <- file.path(repo_root, "localhost", "scripts", script_name)
source_root <- "D:/ken_1km_bau1_2030_v3_ng"
stopifnot(file.exists(script), dir.exists(source_root))

fixture <- tempfile("nrb_graphs_datasets3_", tmpdir = Sys.getenv("MOFUSS_TEST_SCRATCH", tempdir()))
dir.create(file.path(fixture, "LULCC", "TempTables"), recursive = TRUE)
dir.create(
  file.path(fixture, "LULCC", "DownloadedDatasets", "SourceDataGlobal"),
  recursive = TRUE
)
dir.create(file.path(fixture, "Temp"), recursive = TRUE)
dir.create(file.path(fixture, "Out"), recursive = TRUE)
# V8 fixtures contain read-only junctions to canonical raster directories;
# retain that fixture rather than recursively cleaning across a junction.
on.exit({
  if (!identical(script_name, "NRB_graphs_datasets_v8.R"))
    unlink(fixture, recursive = TRUE, force = TRUE)
}, add = TRUE)

copy_one <- function(relative) {
  destination <- file.path(fixture, relative)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  stopifnot(file.copy(file.path(source_root, relative), destination))
}
copy_one("LULCC/TempTables/Country.csv")
copy_one("LULCC/DownloadedDatasets/SourceDataGlobal/parameters.csv")

tables <- list.files(
  file.path(source_root, "Temp"),
  pattern = "^(2_(AGBtx|NRB|CON_TOT|CON_NRB)[0-9]{2}|3_(NRB|CON_TOT|CON_NRB))\\.csv$",
  full.names = TRUE
)
stopifnot(length(tables) == 123L)
stopifnot(all(file.copy(tables, file.path(fixture, "Temp"))))

if (identical(script_name, "NRB_graphs_datasets_v8.R")) {
  # The current reader also derives post-spin-up period totals from the same
  # attributed raster ledger as maps, so a CSV-only fixture is insufficient.
  source(file.path(repo_root, "localhost/scripts/helpers/woodfuel_nrb_attribution.R"))
  source_context <- mofuss_nrb_context(source_root, expected_steps = 31L, mc = 1L)
  write.csv(data.frame(lulc_version = source_context$luc_mode),
            file.path(fixture, "Temp/mc_batch_ready.csv"), row.names = FALSE)
  if (!is.null(source_context$model_path)) stopifnot(file.copy(source_context$model_path, fixture))
  for (j in seq_len(30L)) {
    stopifnot(Sys.junction(file.path(source_root, paste0("debugging_", j)),
                          file.path(fixture, paste0("debugging_", j))))
  }
}

args <- c(
  script,
  "MC=30", "IT=2000", "K_MC=1", "TOF_MC=1", "Ini_st_MC=1",
  "Ini_st.factor.percentage=100", "COVER_MAP=1", "rmax_MC=1",
  "DEF_FW=0", "IL=48", "STdyn=30", "AGBmap=1", "SumTables=0",
  "OSType=64", "BaUvsICS='BaU'", "cutoff_yrs=10"
)
old <- setwd(fixture)
on.exit(setwd(old), add = TRUE)
output <- system2(
  file.path(R.home("bin"), "Rscript.exe"),
  args,
  stdout = TRUE,
  stderr = TRUE
)
status <- attr(output, "status")
if (is.null(status)) status <- 0L
if (status != 0L) stop(paste(tail(output, 80L), collapse = "\n"))

expected <- file.path(
  fixture,
  "Out",
  c("AGB_NRB_fNRB.tif", "Boxplots.tif", "AGB_NRB_fNRB_+10.tif", "Boxplots_+10.tif")
)
stopifnot(all(file.exists(expected)), all(file.info(expected)$size > 0))
cat("NRB_GRAPHS_DATASETS_FIXTURE_OK:", script_name, "\n")
