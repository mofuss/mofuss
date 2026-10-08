# Deterministic end-to-end reporting regression. Creates only small synthetic
# rasters, vectors and figures beneath the explicit temporary workspace.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Usage: Rscript test_reporting_woodfuel_attribution.R <scratch_parent>")
repo <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
scratch_parent <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
if (identical(tolower(scratch_parent), tolower(repo)) ||
    startsWith(tolower(scratch_parent), paste0(tolower(repo), "/"))) {
  stop("Reporting fixtures must be outside the source repository.")
}
fixture <- tempfile("reporting_woodfuel_", tmpdir = scratch_parent)
dir.create(fixture)
for (dir in c("debugging_1", "Temp", "Out", "LULCC/TempTables", "LULCC/TempRaster",
              "LULCC/TempVector", "LULCC/DownloadedDatasets/SourceDataGlobal/InVector")) {
  dir.create(file.path(fixture, dir), recursive = TRUE, showWarnings = FALSE)
}
suppressPackageStartupMessages({
  library(raster)
  library(sf)
  library(data.table)
  library(foreach)
  library(tidyverse)
})
source(file.path(repo, "localhost/scripts/helpers/woodfuel_nrb_attribution.R"))
maps_script <- file.path(repo, "localhost/scripts/maps_animations_v8.R")
maps_lines <- readLines(maps_script, warn = FALSE)
block_start <- grep("^summarise_mc_uncertainty <- function", maps_lines)
block_end <- grep("^} # if [(]fNRB_partition_tables == 1[)]", maps_lines)
stopifnot(length(block_start) == 1L, length(block_end) == 1L)

writeLines(c('<model>',
             paste0('<property key="mofuss.nrb.attribution.contract" value="', MOFUSS_NRB_CONTRACT, '"/>'),
             '</model>'), file.path(fixture, "10_dyn_fixture.egoml"))
template <- raster(nrows = 2L, ncols = 3L, xmn = 0, xmx = 3000,
                   ymn = 0, ymx = 2000, crs = "EPSG:3395")
write_map <- function(x, filename) {
  layer <- setValues(template, x)
  writeRaster(layer, file.path(fixture, filename), overwrite = TRUE)
}
stock <- c(100, 100, 100, 0, NA, 100)
balance <- c(0, 0, 0, 0, NA, 0)
annual <- matrix(0, 21L, 4L, dimnames = list(NULL, c("AGBtx", "NRB", "CON_TOT", "CON_NRB")))
for (step in seq_len(21L)) {
  # Cells 1 and 6 lose stock to direct LUC; cell 2 has harvest plus LUC.
  # Cell 3 recovers more than it harvests. Cell 4 has zero harvest and
  # cell 5 is wholly outside model support. Cell 6 regrows after clearing.
  if (step == 12L) stock[c(1L, 6L)] <- c(20, 5)
  if (step == 15L) stock[2L] <- 40
  growth <- c(0, 0, 3, 0, NA, if (step >= 13L) 1 else 0)
  harvest <- c(0, 2, 2, 0, NA, if (step == 11L) 5 else 0)
  before <- stock + growth
  after <- before - harvest
  balance <- balance + harvest - growth
  for (stem in c("Growth", "Growth_less_harv", "Harvest_tot", "Woodfuel_balance")) {
    value <- switch(stem, Growth = before, Growth_less_harv = after,
                    Harvest_tot = harvest, Woodfuel_balance = balance)
    write_map(value, sprintf("debugging_1/%s%02d.tif", stem, step))
  }
  annual[step, ] <- c(sum(after, na.rm = TRUE), sum(pmax(0, harvest - growth), na.rm = TRUE),
                     sum(harvest, na.rm = TRUE), sum(harvest[pmax(0, harvest - growth) > 0], na.rm = TRUE))
  stock <- after
}
zones <- setValues(template, seq_len(ncell(template)))
for (stem in c("admin_c", "admin_c1", "admin_c2", "ecoregions_c")) {
  writeRaster(zones, file.path(fixture, "LULCC/TempRaster", paste0(stem, ".tif")), overwrite = TRUE)
}
polys <- st_as_sf(rasterToPolygons(zones))
polys$layer <- NULL
polys$ID <- seq_len(nrow(polys))
for (col in c("NAME_0", "NAME_1", "NAME_2", "Subregion", "mofuss_reg", "GID_0", "GID_1", "GID_2")) {
  polys[[col]] <- paste0(col, polys$ID)
}
for (name in c("userarea", "userarea1", "userarea2")) {
  st_write(polys, file.path(fixture, "LULCC/TempVector", paste0(name, ".gpkg")), quiet = TRUE)
}
polys$ECO_ID <- polys$ID
polys$ECO_NAME <- paste0("Eco", polys$ID)
polys$NNH_NAME <- "Fixture"
st_write(polys, file.path(fixture, "LULCC/DownloadedDatasets/SourceDataGlobal/InVector/ecoregions.gpkg"), quiet = TRUE)

country_parameters <- data.frame(
  Var = c("start_year", "end_year", "monte_carlo_runs", "LUCmap_v", "aoi_poly",
          "ext_analysis_ID", "ext_analysis_NAME", "ext_analysis_ID_1", "ext_analysis_NAME_1",
          "ext_analysis_ID_2", "ext_analysis_NAME_2", "ecoregions_ID", "ecoregions_NAME"),
  ParCHR = c("2000", "2020", "1", "3", "0", "ID", "NAME_0", "ID", "NAME_1", "ID", "NAME_2", "ECO_ID", "ECO_NAME"))
write.csv(country_parameters[country_parameters$Var != "LUCmap_v", ], file.path(fixture, "LULCC/DownloadedDatasets/SourceDataGlobal/parameters.csv"), row.names = FALSE)
write.csv(data.frame(Key. = 1, Country = "Global"), file.path(fixture, "LULCC/TempTables/Country.csv"), row.names = FALSE)
write.csv(data.frame(lulc_version = 3L), file.path(fixture, "Temp/mc_batch_ready.csv"), row.names = FALSE)
oldwd <- setwd(fixture)
on.exit(setwd(oldwd), add = TRUE)
MC <- 1L; IT <- 2000L; end_year <- 2020L; STdyn <- 20L; luc_mode <- 3L
aoi_poly <- 0L; mcthreshold <- 30L; uncertainty_digits <- 2L; fNRB_partition_tables <- 1L
nrb_contexts <- list(mofuss_nrb_context(fixture, luc_mode = 3L, expected_steps = 21L))

# Run the actual production block through CSV and GeoPackage publication.
eval(parse(text = maps_lines[block_start:block_end]), envir = .GlobalEnv)
expected <- c(0, 22, 0, 0, NA, 0)
for (level in c("adm0", "adm1", "adm2", "ecoregions")) {
  path <- file.path(fixture, "Out/webmofuss_results", paste0("summary_", level, "_frcompl.csv"))
  tab <- read.csv(path)
  id <- if (level == "ecoregions") "ECO_ID" else "ID"
  tab <- tab[order(tab[[id]]), ]
  stopifnot(isTRUE(all.equal(tab$NRB_2010_2020_mean, expected)),
            is.na(tab$fNRB_2010_2020_mean[4L]), is.na(tab$fNRB_2010_2020_mean[5L]),
            tab$fNRB_2010_2020_mean[2L] == 100,
            all(is.na(tab$NRB_2010_2020_sd)))
  vector <- st_read(file.path(fixture, "Out/webmofuss_results", paste0("mofuss_", level, "_fr.gpkg")), quiet = TRUE)
  stopifnot(is.na(vector$fNRB_2010_2020_mean[vector[[id]] == 4L]))
}
short <- mofuss_period_nrb(nrb_contexts[[1L]], 11L, 12L)
stopifnot(getValues(short$nrb)[1L] == 0, getValues(short$nrb)[6L] == 5)
full <- mofuss_period_nrb(nrb_contexts[[1L]], 11L, 21L)
stopifnot(getValues(full$nrb)[6L] == 0)
periods2050 <- mofuss_reporting_periods(2000L, 2050L)
stopifnot(periods2050$last_step[periods2050$period_key == "2010_2050"] == 51L,
          periods2050$last_step[periods2050$period_key == "2010_2020"] == 20L,
          periods2050$first_step[periods2050$period_key == "2040_2050"] == 41L)

# MC1 figures exercise corrected annual CSVs, period boxplots, and shape retention.
for (name in colnames(annual)) {
  writeLines(c("Key*, Value,", sprintf("%d, %.12f,", seq_len(21L), annual[, name])),
             file.path(fixture, "Temp", paste0("2_", name, "01.csv")))
}
for (name in c("NRB", "CON_TOT", "CON_NRB")) {
  value <- if (name == "NRB") 42 else sum(annual[, name])
  writeLines(c("Key*, Value", paste0("1, ", value)), file.path(fixture, "Temp", paste0("3_", name, ".csv")))
}
graph_args <- c(shQuote(file.path(repo, "localhost/scripts/NRB_graphs_datasets_v8.R")),
                "MC=1", "IT=2000", "K_MC=1", "TOF_MC=1", "Ini_st_MC=1",
                "Ini_st.factor.percentage=100", "COVER_MAP=1", "rmax_MC=1",
                "DEF_FW=0", "IL=48", "STdyn=20", "AGBmap=1", "SumTables=0",
                "OSType=64", "BaUvsICS='BaU'", "cutoff_yrs=10")
Sys.setenv(MOFUSS_PLOT_DPI = "72")
rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
output <- system2(rscript, graph_args, stdout = TRUE, stderr = TRUE)
writeLines(output, file.path(fixture, "graphs.log"))
status <- attr(output, "status")
if (!is.null(status) && status != 0L) stop(paste(tail(output, 40L), collapse = "\n"))
stopifnot(all(file.exists(file.path(fixture, "Out", c("AGB_NRB_fNRB.tif", "AGB_NRB_fNRB_+10.tif", "Boxplots.tif", "Boxplots_+10.tif")))))

# PDF publication consumes certified tables and preserves their explicit dates.
# Keep the build under the fixture for checking the actual emitted table text.
pdflatex <- Sys.getenv("MOFUSS_PDFLATEX", "C:/Program Files/MiKTeX/miktex/bin/x64/pdflatex.exe")
if (file.exists(pdflatex)) {
  dir.create(file.path(fixture, "LaTeX"))
  write.csv(data.frame(Parameter = c("StartUp year", "Simulation Length (SL)", "Number of MC realizations", "Spatial resolution", "Type of scenario"),
                       Value = c("2000", "20", "1", "1000 m", "BaU")),
            file.path(fixture, "LULCC/TempTables/InputPara.csv"), row.names = FALSE)
  source(file.path(repo, "localhost/scripts/LaTeX/generate_modern_report_v8.R"))
  report <- generate_modern_report(fixture, output_dir = "Out", pdflatex = pdflatex,
                                   scenario_ver = "BaU1", keep_build = TRUE)
  stopifnot(file.exists(report), file.info(report)$size > 0)
  table_text <- paste(readLines(file.path(fixture, "LaTeX/build_modern/_nrb_table.tex")), collapse = "\n")
  stopifnot(grepl("Woodfuel-only NRB", table_text, fixed = TRUE),
            grepl("2010\\textendash{}2020", table_text, fixed = TRUE))
  metadata_path <- file.path(fixture, "Out/webmofuss_results/nrb_attribution_metadata.csv")
  stopifnot(file.rename(metadata_path, paste0(metadata_path, ".saved")))
  # The actual run parameter table need not contain LUCmap_v; batch provenance
  # must still identify dynamic cover and reject uncertified historical tables.
  write.csv(country_parameters[country_parameters$Var != "LUCmap_v", ],
            file.path(fixture, "LULCC/DownloadedDatasets/SourceDataGlobal/parameters.csv"), row.names = FALSE)
  failure <- tryCatch({
    generate_modern_report(fixture, output_dir = "Out", pdflatex = pdflatex,
                           scenario_ver = "BaU1", keep_build = TRUE)
    NULL
  }, error = identity)
  stopifnot(inherits(failure, "error"), grepl("Missing NRB attribution metadata", conditionMessage(failure)))
  stopifnot(file.rename(paste0(metadata_path, ".saved"), metadata_path))
}

cat("REPORTING_WOODFUEL_ATTRIBUTION_OK\nFixture: ", fixture, "\n", sep = "")
