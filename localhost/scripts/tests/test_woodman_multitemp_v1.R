# A small end-to-end raster fixture for the annual Woodman harmonizer.
suppressPackageStartupMessages(library(terra))

fixture <- tempfile(
  pattern = "harmonizer_test_",
  tmpdir = "E:/MoFuSS_Active/woodman_multitemp_2026-09-25"
)
countrydir <- fixture
country_name <- "Test"
global_data <- file.path(
  fixture, "LULCC", "DownloadedDatasets", "SourceDataGlobal"
)
source_data <- file.path(
  fixture, "LULCC", "DownloadedDatasets", "SourceDataTest"
)
series_dir <- "woodman_test_2000_2001"
for (path in c(
  file.path(global_data, "InRaster", series_dir),
  file.path(source_data, "InTables"),
  file.path(fixture, "LULCC", "TempTables"),
  file.path(fixture, "LULCC", "TempRaster")
)) dir.create(path, recursive = TRUE, showWarnings = FALSE)

grid <- rast(nrows = 3, ncols = 3, xmin = 0, xmax = 3,
             ymin = 0, ymax = 3, crs = "EPSG:4326")
with_values <- function(values) {
  result <- grid
  values(result) <- values
  result
}
luc2000 <- c(44, 22, 11, 45, 33, 22, 66, 77, NA)
luc2001 <- c(22, 44, 11, 44, 33, 44, 66, 77, NA)
writeRaster(with_values(luc2000), file.path(
  global_data, "InRaster", series_dir, "woodman_luc_2000_gcs.tif"
))
writeRaster(with_values(luc2001), file.path(
  global_data, "InRaster", series_dir, "woodman_luc_2001_gcs.tif"
))
writeRaster(with_values(rep(11100L, 9)), file.path(
  global_data, "InRaster", "woodman_zone_pcs.tif"
))

codes <- c(11L, 22L, 33L, 44L, 45L, 55L, 66L, 77L)
keys <- data.frame(
  IDorig = 11100L + codes, Key = seq_along(codes),
  TOF = as.integer(codes %in% c(11L, 66L, 77L)),
  luc_code = codes
)
write.csv(keys, file.path(source_data, "InTables", "woodman_key_crosswalk.csv"),
          row.names = FALSE)
growth <- data.frame(
  `Key*` = 1:9,
  LULC = c(paste0("Class_", codes), "Urban_Forced"),
  TOF = c(keys$TOF, 1L), check.names = FALSE
)
write.csv(growth, file.path(fixture, "LULCC", "TempTables",
                            "growth_parameters1.csv"), row.names = FALSE)
base <- c(4, 2, 1, 5, 3, 9, 7, 8, NA)
writeRaster(with_values(base), file.path(
  fixture, "LULCC", "TempRaster", "LULCt1_c.tif"
))

country_parameters <- data.frame(
  Var = c("LULCt1map_dataset", "woodman_series_dir", "start_year", "end_year"),
  ParCHR = c("woodman", series_dir, "2000", "2001")
)
userarea_r <- grid
align_raster_to_template <- function(x, template, method, mask_output = TRUE) {
  stopifnot(compareGeom(x, template, stopOnError = FALSE))
  x
}
source("localhost/scripts/5b_harmonizer_woodman_multitemp_v1.R")

result <- file.path(fixture, "LULCC", "TempRaster")
read_cells <- function(name) as.vector(values(rast(file.path(result, name))))
stopifnot(
  isTRUE(all.equal(read_cells("LULCt1_c_2000.tif"), as.numeric(base))),
  isTRUE(all.equal(read_cells("LULCt1_c_2001.tif"),
                   as.numeric(c(2, 4, 1, 4, 3, 9, 7, 8, NA)))),
  isTRUE(all.equal(read_cells("TOFvsFOR_mask1_2001.tif"),
                   as.numeric(c(0, 0, 1, 0, 0, 1, 1, 1, NA)))),
  isTRUE(all.equal(read_cells("WoodmanTransition_2001.tif"),
                   as.numeric(c(1, 2, 0, 0, 0, 0, 0, 0, NA))))
)
cat("Woodman annual harmonizer raster fixture passed.\n")
