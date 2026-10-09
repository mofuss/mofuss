# Small country-grid fixture for Woodman in LUC3 alongside MODIS in LUC1.
# Generated rasters live outside the source repository.
suppressPackageStartupMessages(library(terra))

test_root <- tempdir()
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
for (path in c(
  file.path(global_data, "InRaster"),
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
luc2002 <- c(11, 22, 44, 11, 44, 22, 33, NA, 77)
annual_luc <- list(`2000` = luc2000, `2001` = luc2001,
                   `2002` = luc2002, `2003` = luc2002)
for (year in names(annual_luc)) {
  writeRaster(with_values(annual_luc[[year]]), file.path(
    global_data, "InRaster", sprintf("woodman_luc_%s_pcs.tif", year)
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
  Var = c("LULCt1map", "LULCt3map", "LULCt3map_name", "LULCt3map_yr",
          "start_year", "end_year"),
  ParCHR = c("YES", "YES", "woodman_luc_pcs.tif",
             "2000", "2000", "2003")
)
userarea_r <- grid
align_raster_to_template <- function(x, template, method, mask_output = TRUE) {
  stopifnot(compareGeom(x, template, stopOnError = FALSE))
  x
}
options_before <- terraOptions(print = FALSE)
option_names <- c("tempdir", "memfrac", "memmin", "memmax", "todisk")
temporary_before <- list.files(options_before$tempdir, pattern = "^woodman_annual_")
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
  isTRUE(all.equal(read_cells("LULCt3_transition_2000.tif"),
                   as.numeric(c(0, 0, 0, 0, 0, 0, 0, 0, NA)))),
  isTRUE(all.equal(read_cells("LULCt3_transition_2001.tif"),
                   as.numeric(c(1, 2, 0, 0, 3, 0, 4, 0, NA)))),
  isTRUE(all.equal(read_cells("LULCt3_c_2002.tif"),
                   as.numeric(c(1, 2, 4, 1, 4, 9, 3, NA, NA)))),
  isTRUE(all.equal(read_cells("LULCt3_transition_2002.tif"),
                   as.numeric(c(0, 1, 2, 1, 2, 0, 0, NA, NA)))),
  isTRUE(all.equal(read_cells("LULCt3_transition_2003.tif"),
                   as.numeric(c(0, 0, 0, 0, 0, 0, 0, NA, NA)))),
  identical(unname(tools::md5sum(c(luc1_path, tof1_path))),
            luc1_hash_before),
  isTRUE(all.equal(read_cells("LULCt1_c_2001.tif"), rep(2, 9))),
  isTRUE(all.equal(read_cells("TOFvsFOR_mask1_2001.tif"), rep(0, 9))),
  isTRUE(all.equal(read_cells("LULCt1_transition_2001.tif"), rep(0, 9)))
)
stopifnot(
  identical(terraOptions(print = FALSE)[option_names], options_before[option_names]),
  identical(list.files(options_before$tempdir, pattern = "^woodman_annual_"),
            temporary_before)
)

# An unknown annual key must fail cleanly after earlier years completed.
bad_luc <- luc2002
bad_luc[[1L]] <- 99
writeRaster(with_values(bad_luc), file.path(
  global_data, "InRaster", "woodman_luc_2002_pcs.tif"
), overwrite = TRUE)
failure <- tryCatch(
  source("localhost/scripts/5b_harmonizer_woodman_multitemp_v1.R"),
  error = identity
)
stopifnot(
  inherits(failure, "error"),
  grepl("2002 has 1 Woodman cells without growth-parameter keys", conditionMessage(failure)),
  identical(terraOptions(print = FALSE)[option_names], options_before[option_names]),
  identical(list.files(options_before$tempdir, pattern = "^woodman_annual_"),
            temporary_before)
)

# Additive preparation must continue to refuse existing annual products.
outputs <- list.files(output_dir, pattern = "_20[0-9][0-9]\\.tif$", full.names = TRUE)
hashes <- unname(tools::md5sum(outputs))
woodman_no_overwrite <- TRUE
failure <- tryCatch(
  source("localhost/scripts/5b_harmonizer_woodman_multitemp_v1.R"),
  error = identity
)
stopifnot(inherits(failure, "error"),
          identical(unname(tools::md5sum(outputs)), hashes),
          identical(terraOptions(print = FALSE)[option_names], options_before[option_names]),
          identical(list.files(options_before$tempdir, pattern = "^woodman_annual_"),
                    temporary_before))
unlink(fixture, recursive = TRUE)
cat("Woodman LUC3 annual raster fixture passed.\n")
