# Small country-grid fixture for Woodman in LUC3 alongside MODIS in LUC1.
# Generated rasters live outside the source repository.
suppressPackageStartupMessages(library(terra))

test_root <- "E:/MoFuSS_Active/woodman_luc3_country_2026-10-03"
dir.create(test_root, recursive = TRUE, showWarnings = FALSE)
fixture <- tempfile(pattern = "woodman_luc3_fixture_", tmpdir = test_root)
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
  file.path(global_data, "InRaster"),
  file.path(global_data, "InRaster_GCS", series_dir),
  file.path(source_data, "InTables"),
  file.path(fixture, "LULCC", "TempTables"),
  file.path(fixture, "LULCC", "TempRaster")
)) dir.create(path, recursive = TRUE, showWarnings = FALSE)

grid <- rast(nrows = 3, ncols = 3, xmin = 0, xmax = 3,
             ymin = 0, ymax = 3, crs = "EPSG:4326")
with_values <- function(x) {
  result <- grid
  values(result) <- x
  result
}

# Cell 1 clears forest and gains TOF (clearing has priority), cell 2 gains
# forest, cell 5 gains TOF without forest change, cell 6 remains forced urban,
# cell 7 loses TOF without forest change, and source code 0 at cell 9 is NoData.
luc2000 <- c(44, 22, 11, 45, 33, 22, 66, 77, NA)
luc2001 <- c(11, 44, 11, 44, 11, 44, 33, 77, 0)
annual_luc <- list(`2000` = luc2000, `2001` = luc2001)
for (year in names(annual_luc)) {
  writeRaster(with_values(annual_luc[[year]]), file.path(
    global_data, "InRaster", sprintf("woodman_luc_%s_pcs.tif", year)
  ))
  # A complete but invalid GCS series proves that 5b chooses the PCS maps.
  writeRaster(with_values(rep(0, 9)), file.path(
    global_data, "InRaster_GCS", series_dir,
    sprintf("woodman_luc_%s_gcs.tif", year)
  ))
}
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
                            "growth_parameters3.csv"), row.names = FALSE)
base <- c(4, 2, 1, 5, 3, 9, 7, 8, NA)
output_dir <- file.path(fixture, "LULCC", "TempRaster")
writeRaster(with_values(base), file.path(output_dir, "LULCt3_c.tif"))

# The concurrent MODIS channel must retain its static inputs untouched.
luc1_path <- file.path(output_dir, "LULCt1_c.tif")
tof1_path <- file.path(output_dir, "TOFvsFOR_mask1.tif")
writeRaster(with_values(rep(2L, 9)), luc1_path)
writeRaster(with_values(rep(0L, 9)), tof1_path)
luc1_hash_before <- unname(tools::md5sum(c(luc1_path, tof1_path)))

country_parameters <- data.frame(
  Var = c("LULCt1map", "LULCt1map_dataset", "LULCt3map",
          "LULCt3map_dataset", "LULCt3map_name", "LULCt3map_yr",
          "woodman_series_dir", "start_year", "end_year"),
  ParCHR = c("YES", "modis", "YES", "woodman", "woodman_luc_pcs.tif",
             "2000", series_dir, "2000", "2001")
)
userarea_r <- grid
align_raster_to_template <- function(x, template, method, mask_output = TRUE) {
  stopifnot(compareGeom(x, template, stopOnError = FALSE))
  x
}
source("localhost/scripts/5b_harmonizer_woodman_multitemp_v1.R")

read_cells <- function(name) {
  as.vector(values(rast(file.path(output_dir, name))))
}
expected_tof_2000 <- as.numeric(c(0, 0, 1, 0, 0, 1, 1, 1, NA))
expected_tof_2001 <- as.numeric(c(1, 0, 1, 0, 1, 1, 0, 1, NA))
stopifnot(
  isTRUE(all.equal(read_cells("LULCt3_c_2000.tif"), as.numeric(base))),
  isTRUE(all.equal(read_cells("LULCt3_c_2001.tif"),
                   as.numeric(c(1, 4, 1, 4, 1, 9, 3, 8, NA)))),
  isTRUE(all.equal(read_cells("TOFvsFOR_mask3_2000.tif"), expected_tof_2000)),
  isTRUE(all.equal(read_cells("TOFvsFOR_mask3_2001.tif"), expected_tof_2001)),
  isTRUE(all.equal(read_cells("WoodmanTransition_2000.tif"),
                   as.numeric(c(0, 0, 0, 0, 0, 0, 0, 0, NA)))),
  isTRUE(all.equal(read_cells("WoodmanTransition_2001.tif"),
                   as.numeric(c(1, 2, 0, 0, 3, 0, 4, 0, NA)))),
  identical(unname(tools::md5sum(c(luc1_path, tof1_path))),
            luc1_hash_before),
  !file.exists(file.path(output_dir, "LULCt1_c_2001.tif")),
  !file.exists(file.path(output_dir, "TOFvsFOR_mask1_2001.tif"))
)
cat("Woodman LUC3 annual raster fixture passed.\n")
