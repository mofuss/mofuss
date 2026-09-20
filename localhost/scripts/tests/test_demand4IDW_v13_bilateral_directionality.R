# Lightweight tests for domestic W and direct, tiered bilateral V permissions.
# The full demand-spatialization workflow and IDW executable are not run.

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(terra)
})

repository_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
script <- file.path(
  repository_root,
  "localhost",
  "scripts",
  "3_demand4IDW_v13.R"
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
    if (grepl('identical(effective_byregion, "Regional")',
              condition_text, fixed = TRUE)) {
      directional_expression <- expression
      break
    }
  }
}
stopifnot(!is.null(directional_expression))

test_dir <- tempfile("demand4idw_v13_bilateral_")
dir.create(file.path(test_dir, "to_idw"), recursive = TRUE)
on.exit(unlink(test_dir, recursive = TRUE, force = TRUE), add = TRUE)
old_working_directory <- setwd(test_dir)
on.exit(setwd(old_working_directory), add = TRUE)

template <- terra::rast(
  ncols = 6,
  nrows = 2,
  xmin = 0,
  xmax = 6,
  ymin = 0,
  ymax = 2,
  crs = "EPSG:3857"
)
terra::values(template) <- 1

adm0_reg <- terra::vect(
  c(
    "POLYGON ((0 0,2 0,2 2,0 2,0 0))",
    "POLYGON ((2 0,4 0,4 2,2 2,2 0))",
    "POLYGON ((4 0,6 0,6 2,4 2,4 0))"
  ),
  crs = terra::crs(template)
)
adm0_reg$GID_0 <- c("AAA", "BBB", "CCC")
adm0_reg$NAME_0 <- c("Alpha", "Beta", "Gamma")
adm0_reg$mofuss_reg <- "SSA_adm0_TEST"
adm0_reg$Subregion <- "Test bilateral component"
adm0_reg$RunCode <- "TEST"
adm0_reg$CandidateID <- "M85_B30_V1"
adm0_reg$CandidateRegionID <- "M85_TEST"
adm0_reg$ImporterV <- c(1L, 1L, 0L)
adm0_reg$EvidenceConfidence <- c("tier_A_inbound", "tier_B_inbound", "domestic_only")
adm0_reg$Status <- "model_ready_bilateral_permissions_v1"
adm0_reg$PermissionSetID <- "B30_V1"
adm0_reg$RegionBasis <- "minimum_evidence_connected_component"
adm0_reg$ComponentSize <- 3L
adm0_reg$AcceptedEdgeCount <- 3L
# Direct permissions: BBB>AAA (A), CCC>AAA (C), and AAA>BBB (B).
adm0_reg$VSrcA <- c("BBB", "", "")
adm0_reg$VSrcB <- c("", "AAA", "")
adm0_reg$VSrcC <- c("CCC", "", "")

wf_v_st <- template
wf_w_st <- template
wf_v_db4idw <- data.frame(
  ID = c(11L, 22L, 33L),
  x = c(0.5, 2.5, 4.5),
  y = c(0.5, 0.5, 0.5),
  `2000_fw_v` = c(10, 20, 30),
  `2001_fw_v` = c(12, 22, 32),
  centroids = TRUE,
  check.names = FALSE
)
target_colsv <- match(c("2000_fw_v", "2001_fw_v"), names(wf_v_db4idw))

wf_w_db4idw <- data.frame(
  ID = c(31L, 32L, 33L),
  x = c(0.5, 2.5, 4.5),
  y = c(1.5, 1.5, 1.5),
  `2000_fw_w` = c(30, 40, 50),
  `2001_fw_w` = c(32, 42, 52),
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

effective_byregion <- "Regional"
aoi_poly <- "0"
mofuss_region <- "SSA_adm0_TEST"
scenario_ver <- "BaU1_v2"
directional_hc_jobs_created <- FALSE
hc_jobs_dir <- file.path("to_idw", "HC_jobs")
country_parameters <- data.frame(
  Var = "v_permission_tier_policy",
  ParCHR = "ABC",
  stringsAsFactors = FALSE
)

eval(directional_expression, envir = .GlobalEnv)

manifest <- read.csv(
  "to_idw/HC_jobs/HC_job_manifest.csv",
  check.names = FALSE
)
stopifnot(
  directional_hc_jobs_created,
  nrow(manifest) == 6L,
  identical(
    manifest$JobID,
    c(
      "W_AAA_ORIGIN", "W_BBB_ORIGIN", "W_CCC_ORIGIN",
      "V_AAA_ORIGIN", "V_BBB_ORIGIN", "V_CCC_ORIGIN"
    )
  ),
  all(manifest$RunOnHCCluster),
  all(manifest$AllowedSourceISO3[manifest$Channel == "W"] ==
      c("AAA", "BBB", "CCC")),
  identical(
    manifest$AllowedSourceISO3[manifest$Channel == "V"],
    c("AAA;BBB;CCC", "AAA;BBB", "CCC")
  ),
  all(manifest$VPermissionTierPolicy[manifest$Channel == "V"] == "ABC"),
  sum(manifest$DemandRows[manifest$Channel == "W"]) == 3L,
  sum(manifest$DemandRows[manifest$Channel == "V"]) == 3L
)

# BBB may use AAA directly, but must not inherit AAA's supplier CCC.
stopifnot(manifest$AllowedSourceISO3[manifest$JobID == "V_BBB_ORIGIN"] ==
          "AAA;BBB")

count_mask_cells <- function(job_id) {
  mask <- terra::rast(file.path(
    "to_idw", "HC_jobs", job_id, "source_domain_mask_raw.tif"
  ))
  sum(!is.na(terra::values(mask)))
}
stopifnot(
  count_mask_cells("W_AAA_ORIGIN") == 4L,
  count_mask_cells("W_BBB_ORIGIN") == 4L,
  count_mask_cells("W_CCC_ORIGIN") == 4L,
  count_mask_cells("V_AAA_ORIGIN") == 12L,
  count_mask_cells("V_BBB_ORIGIN") == 8L,
  count_mask_cells("V_CCC_ORIGIN") == 4L
)

# Tier policy A retains BBB>AAA but excludes the B and C permissions.
country_parameters$ParCHR <- "A"
directional_hc_jobs_created <- FALSE
eval(directional_expression, envir = .GlobalEnv)
manifest_a <- read.csv(
  "to_idw/HC_jobs/HC_job_manifest.csv",
  check.names = FALSE
)
stopifnot(
  directional_hc_jobs_created,
  identical(
    manifest_a$AllowedSourceISO3[manifest_a$Channel == "V"],
    c("AAA;BBB", "BBB", "CCC")
  ),
  all(manifest_a$VPermissionTierPolicy[manifest_a$Channel == "V"] == "A")
)

# Regression guard: INT4S must preserve location IDs above 2^24 exactly.
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

cat("DEMAND4IDW_V13_BILATERAL_DIRECTIONALITY_OK\n")
