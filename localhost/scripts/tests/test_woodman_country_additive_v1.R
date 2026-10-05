# Small additive country-prep fixture. All runtime files live on E:.
suppressPackageStartupMessages(library(terra))
source("localhost/scripts/5c_prepare_woodman_country_additive_v1.R")

test_root <- "E:/MoFuSS_Active/woodman_country_additive_fixture_2026-10-04"
dir.create(test_root, recursive = TRUE, showWarnings = FALSE)
fixture <- tempfile(pattern = "country_", tmpdir = test_root)
target <- file.path(fixture, "target")
prepared <- file.path(fixture, "prepared")
scratch <- file.path(fixture, "scratch")
for (root in c(target, prepared)) {
  for (folder in c(
    "In", "LULCC/TempRaster", "LULCC/TempTables",
    "LULCC/TempVector", "LULCC/TempVector_GCS",
    "LULCC/DownloadedDatasets/SourceDataGlobal/InRaster",
    "LULCC/DownloadedDatasets/SourceDataGlobal/InTables"
  )) {
    dir.create(file.path(root, folder), recursive = TRUE, showWarnings = FALSE)
  }
}

grid <- rast(
  nrows = 3, ncols = 3, xmin = 0, xmax = 3000,
  ymin = 0, ymax = 3000, crs = "EPSG:3395"
)
with_values <- function(x) {
  result <- grid
  values(result) <- x
  result
}
put_raster <- function(root, relative, x) {
  writeRaster(with_values(x), file.path(root, relative))
}
global_raster <- function(name) {
  file.path("LULCC/DownloadedDatasets/SourceDataGlobal/InRaster", name)
}
global_table <- function(name) {
  file.path("LULCC/DownloadedDatasets/SourceDataGlobal/InTables", name)
}

modis <- c(1, 2, 3, 4, 5, 9, 6, 7, NA)
for (root in c(target, prepared)) {
  put_raster(root, global_raster("modis_lc_type1_pcs.tif"), modis)
  put_raster(root, global_raster("DTEM_pcs_masked.tif"), 1:9)
  put_raster(root, global_raster("datamask_pcs.tif"), rep(1, 9))
  for (name in c(
    "IDW_C++_fw_v01.tif", "IDW_C++_fw_w01.tif",
    "fricc_v.tif", "fricc_w.tif"
  )) {
    put_raster(root, file.path("In", name), rep(1, 9))
  }
}
put_raster(prepared, "LULCC/TempRaster/mask_c.tif",
           c(rep(1, 8), NA))
put_raster(prepared, "LULCC/TempRaster/LULCt1_c.tif", modis)
put_raster(prepared, "LULCC/TempRaster/TOFvsFOR_mask1.tif",
           ifelse(is.na(modis), NA, 0L))
writeLines("prepared vector", file.path(
  prepared, "LULCC/TempVector/boundary.txt"
))
writeLines("prepared GCS vector", file.path(
  prepared, "LULCC/TempVector_GCS/boundary.txt"
))
codes <- c(11L, 22L, 33L, 44L, 45L, 55L, 66L, 77L)
keys <- data.frame(
  IDorig = 11100L + codes, Key = seq_along(codes),
  TOF = as.integer(codes %in% c(11L, 66L, 77L)),
  luc_code = codes
)
growth <- data.frame(
  "Key*" = seq_along(codes),
  LULC = c("Zone_Urban", paste0("Zone_Class", codes[-1L])),
  rmax = ifelse(keys$TOF == 1L, 0, 0.1),
  rmaxSD = 0,
  K = c(2, rep(1, 7)),
  KSD = c(1, rep(0.5, 7)),
  TOF = keys$TOF,
  check.names = FALSE
)
write.csv(keys, file.path(target, global_table(
  "woodman_key_crosswalk.csv"
)), row.names = FALSE)
write.csv(growth, file.path(target, global_table(
  "growth_parameters_v3_woodman.csv"
)), row.names = FALSE)
put_raster(target, global_raster("pre2000_v1_woodman_luc_pcs.tif"),
           c(4, 2, 1, 5, 3, 6, 7, 8, NA))
put_raster(target, global_raster("woodman_zone_pcs.tif"),
           rep(11100L, 9))
luc2000 <- c(44, 22, 11, 45, 33, 22, 66, 77, NA)
luc2001 <- c(11, 44, 11, 44, 11, 44, 33, 77, 0)
put_raster(target, global_raster("woodman_luc_2000_pcs.tif"), luc2000)
put_raster(target, global_raster("woodman_luc_2001_pcs.tif"), luc2001)

modis_growth <- data.frame(
  "Key*" = 1:8, LULC = paste0("MODIS_", 1:8),
  rmax = 0.1, rmaxSD = 0, K = 1, KSD = 0.5, TOF = 0,
  check.names = FALSE
)
write.csv(modis_growth, file.path(prepared, global_table(
  "growth_parameters_v3_modis.csv"
)), row.names = FALSE)
growth1 <- rbind(
  modis_growth,
  data.frame(
    "Key*" = 9, LULC = "Urban_Forced", rmax = 0, rmaxSD = 0,
    K = 2, KSD = 1, TOF = 1, check.names = FALSE
  )
)
write.csv(growth1, file.path(
  prepared, "LULCC/TempTables/growth_parameters1.csv"
), row.names = FALSE)
write.csv(
  data.frame(Key = 1:9, x = growth1$TOF),
  file.path(prepared, "LULCC/TempTables/TOFvsFOR_Categories1.csv"),
  row.names = FALSE
)

base_parameters <- data.frame(
  Var = c(
    "LULCt1map", "LULCt3map", "LULCt3map_name", "LULCt3map_yr",
    "start_year", "end_year", "monte_carlo_runs",
    "uncapped_regrowth", "npa_ease"
  ),
  ParCHR = c(
    "YES", "NO", "dw_pcs.tif", "2015",
    "2000", "2001", "3", "0", "1"
  )
)
write.csv(base_parameters, file.path(
  prepared, "LULCC/DownloadedDatasets/SourceDataGlobal/parameters.csv"
), row.names = FALSE)
target_parameters <- base_parameters
target_parameters$ParCHR[
  target_parameters$Var == "LULCt3map"
] <- "YES"
target_parameters$ParCHR[
  target_parameters$Var == "LULCt3map_name"
] <- "woodman_luc_pcs.tif"
target_parameters$ParCHR[
  target_parameters$Var == "LULCt3map_yr"
] <- "2000"
write.csv(target_parameters, file.path(
  target, "LULCC/DownloadedDatasets/SourceDataGlobal/parameters.csv"
), row.names = FALSE)

idw_path <- file.path(target, "In/IDW_C++_fw_v01.tif")
idw_before <- unname(tools::md5sum(idw_path))
scripts_dir <- file.path(getwd(), "localhost", "scripts")
dry_run <- prepare_woodman_country_additive(
  target, prepared, scratch, scripts_dir = scripts_dir
)
stopifnot(
  length(dry_run$static_to_copy) >= 1L,
  !any(file.exists(dry_run$new_outputs)),
  !dir.exists(scratch)
)
applied <- prepare_woodman_country_additive(
  target, prepared, scratch, apply = TRUE, scripts_dir = scripts_dir
)
read_cells <- function(name) {
  as.vector(values(rast(file.path(target, "LULCC/TempRaster", name))))
}
growth3 <- read.csv(file.path(
  target, "LULCC/TempTables/growth_parameters3.csv"
), check.names = FALSE)
forced <- growth3[growth3$LULC == "Urban_Forced", ]
stopifnot(
  nrow(forced) == 1L,
  forced[["Key*"]][[1L]] == 9L,
  forced$K[[1L]] == 2,
  forced$KSD[[1L]] == 1,
  isTRUE(all.equal(
    read_cells("LULCt3_c.tif"),
    as.numeric(c(4, 2, 1, 5, 3, 9, 7, 8, NA))
  )),
  isTRUE(all.equal(
    read_cells("LULCt3_transition_2001.tif"),
    as.numeric(c(1, 2, 0, 0, 3, 0, 4, 0, NA))
  )),
  identical(idw_before, unname(tools::md5sum(idw_path))),
  identical(
    unname(tools::md5sum(file.path(
      target, "LULCC/TempTables/growth_parameters3.csv"
    ))),
    unname(tools::md5sum(file.path(
      target, global_table("growth_parameters3.csv")
    )))
  ),
  file.exists(file.path(target, "LULCC/TempTables/parameters_dinamica.csv")),
  file.exists(file.path(target, "LULCC/TempVector/boundary.txt")),
  file.exists(file.path(target, "LULCC/TempVector_GCS/boundary.txt")),
  file.exists(applied$manifest_path),
  nrow(read.csv(applied$manifest_path)) ==
    length(applied$static_added) + length(applied$outputs)
)
collision <- tryCatch(
  prepare_woodman_country_additive(
    target, prepared, scratch, scripts_dir = scripts_dir
  ),
  error = function(error_condition) conditionMessage(error_condition)
)
stopifnot(is.character(collision), grepl("Refusing to overwrite", collision))
cat("Additive Woodman country preparation fixture passed.\n")
