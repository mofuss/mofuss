# Lightweight tests for the directional regional IDW handoff without executing
# the full demand-spatialization workflow.

suppressPackageStartupMessages({
  library(dplyr)
  library(terra)
})

repository_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
script <- file.path(
  repository_root,
  "localhost",
  "scripts",
  "3_demand4IDW_v9.R"
)
expressions <- parse(file = script)

wanted_helpers <- c(
  ".validate_location_id_raster",
  ".single_metadata_value",
  ".manifest_path",
  ".copy_file_or_stop",
  ".write_source_domain_mask",
  ".write_directional_idw_bundle"
)
for (expression in expressions) {
  if (
    is.call(expression) && identical(expression[[1L]], as.name("<-")) &&
      is.name(expression[[2L]]) &&
      as.character(expression[[2L]]) %in% wanted_helpers
  ) {
    eval(expression, envir = .GlobalEnv)
  }
}
stopifnot(all(vapply(
  wanted_helpers,
  exists,
  logical(1),
  envir = .GlobalEnv,
  inherits = FALSE
)))

directional_expression <- NULL
for (expression in expressions) {
  if (is.call(expression) && identical(expression[[1L]], as.name("if"))) {
    condition_text <- paste(deparse(expression[[2L]]), collapse = " ")
    if (grepl('identical(byregion, "Regional")', condition_text, fixed = TRUE)) {
      directional_expression <- expression
      break
    }
  }
}
stopifnot(!is.null(directional_expression))

test_dir <- tempfile("demand4idw_v9_directionality_")
dir.create(file.path(test_dir, "to_idw"), recursive = TRUE)
on.exit(unlink(test_dir, recursive = TRUE, force = TRUE), add = TRUE)
old_working_directory <- setwd(test_dir)
on.exit(setwd(old_working_directory), add = TRUE)

template <- terra::rast(
  ncols = 4,
  nrows = 2,
  xmin = 0,
  xmax = 4,
  ymin = 0,
  ymax = 2,
  crs = "EPSG:3857"
)
terra::values(template) <- 1

adm0_reg <- terra::vect(
  c(
    "POLYGON ((0 0,0.9 0,0.9 2,0 2,0 0))",
    "POLYGON ((2.2 0,4 0,4 2,2.2 2,2.2 0))"
  ),
  crs = terra::crs(template)
)
adm0_reg$GID_0 <- c("AAA", "BBB")
adm0_reg$NAME_0 <- c("Importer", "Domestic")
adm0_reg$mofuss_reg <- "SSA_adm0_GLEA"
adm0_reg$Subregion <- "Test region"
adm0_reg$RunCode <- "GLEA"
adm0_reg$CandidateID <- "M67_GME_V2"
adm0_reg$CandidateRegionID <- "M67_TEST"
adm0_reg$ImporterV <- c(1L, 0L)
adm0_reg$EvidenceConfidence <- "test"
adm0_reg$Status <- "test"

wf_v_st <- template
wf_v_db4idw <- data.frame(
  ID = c(11L, 22L, 33L),
  x = c(0.5, 2.5, 1.5),
  y = c(0.5, 0.5, 0.5),
  `2000_fw_v` = c(10, 20, 5),
  `2001_fw_v` = c(12, 22, 6),
  centroids = TRUE,
  check.names = FALSE
)
target_colsv <- match(c("2000_fw_v", "2001_fw_v"), names(wf_v_db4idw))

wf_w_db4idw <- data.frame(
  ID = c(31L, 32L),
  x = c(0.5, 2.5),
  y = c(1.5, 1.5),
  `2000_fw_w` = c(30, 40),
  `2001_fw_w` = c(32, 42),
  centroids = TRUE,
  check.names = FALSE
)
target_colsw <- match(c("2000_fw_w", "2001_fw_w"), names(wf_w_db4idw))

terra::writeRaster(
  template,
  "to_idw/locs_raster_w.tif",
  datatype = "INT4S",
  overwrite = TRUE
)
write.csv(
  wf_w_db4idw[, c("ID", "2000_fw_w", "2001_fw_w")],
  "to_idw/BaU_fwch_w.csv",
  row.names = FALSE
)

byregion <- "Regional"
aoi_poly <- "0"
mofuss_region <- "SSA_adm0_GLEA"
scenario_ver <- "BaU1_v2"
directional_hc_jobs_created <- FALSE
hc_jobs_dir <- file.path("to_idw", "HC_jobs")

eval(directional_expression, envir = .GlobalEnv)

manifest <- read.csv(
  "to_idw/HC_jobs/HC_job_manifest.csv",
  check.names = FALSE
)
stopifnot(
  directional_hc_jobs_created,
  nrow(manifest) == 3L,
  identical(
    manifest$JobID,
    c("W_GLEA", "V_GLEA_IMPORTERS", "V_BBB_DOMESTIC")
  ),
  all(manifest$RunOnHCCluster),
  sum(manifest$DemandRows[manifest$Channel == "V"]) == 3L
)

importer_demand <- read.csv(
  "to_idw/HC_jobs/V_GLEA_IMPORTERS/BaU_fwch_v.csv",
  check.names = FALSE
)
domestic_demand <- read.csv(
  "to_idw/HC_jobs/V_BBB_DOMESTIC/BaU_fwch_v.csv",
  check.names = FALSE
)
stopifnot(
  identical(importer_demand$ID, c(11L, 33L)),
  domestic_demand$ID == 22L,
  sum(importer_demand$`2000_fw_v`) +
    sum(domestic_demand$`2000_fw_v`) == 35
)

importer_mask <- terra::rast(
  "to_idw/HC_jobs/V_GLEA_IMPORTERS/source_domain_mask_raw.tif"
)
domestic_mask <- terra::rast(
  "to_idw/HC_jobs/V_BBB_DOMESTIC/source_domain_mask_raw.tif"
)
stopifnot(
  sum(!is.na(terra::values(importer_mask))) == 6L,
  sum(!is.na(terra::values(domestic_mask))) == 4L
)

# Regression guard for the original regional failure: FLT4S cannot represent
# every integer above 2^24, while INT4S must preserve these IDs exactly.
high_ids <- c(16777215L, 16777216L, 16777217L, 18059274L)
high_id_xyz <- data.frame(
  x = c(0.5, 1.5, 0.5, 1.5),
  y = c(1.5, 1.5, 0.5, 0.5),
  ID = high_ids
)
high_id_raster <- terra::rast(
  high_id_xyz,
  type = "xyz",
  crs = terra::crs(template),
  digits = 0
)
high_id_file <- file.path(test_dir, "high_ids_int4s.tif")
terra::writeRaster(
  high_id_raster,
  high_id_file,
  datatype = "INT4S",
  overwrite = TRUE
)
.validate_location_id_raster(
  high_id_file,
  high_ids,
  "High-ID regression raster"
)
stopifnot(identical(
  as.numeric(terra::values(terra::rast(high_id_file), na.rm = TRUE)),
  as.numeric(high_ids)
))

cat("DEMAND4IDW_V9_DIRECTIONALITY_OK\n")
