# Country sourcing from compact records produced by Dinamica v12/v13.
# Run AFTER the four simulations finish. This does not run Dinamica or IDW.
# Edit the configuration below for another region or MC count.

SOURCING_RUNS <- c(
  "E:/ECSA_1000m_bau1_2050_mc3_capped",
  "E:/ECSA_1000m_bau1_2050_mc3_uncapped",
  "E:/ECSA_1000m_ics3_2050_mc3_capped",
  "E:/ECSA_1000m_ics3_2050_mc3_uncapped"
)
SOURCING_OUTPUT <- "E:/_postprocessing_draft/ECSA_1000m_ics3_2050_mc3/runtime_sourcing"
SOURCING_PERIODS <- c("2020:2030", "2030:2040", "2040:2050", "2020:2050")
SOURCING_OVERWRITE <- FALSE
SOURCING_SOURCE <- "C:/Users/UNAM/Documents/mofuss/localhost/scripts/postprocessing_sourcing/2post_runtime_sourcing_v1.R"
# Fail rather than publish physical shares if a signed legacy adjustment exists.
SOURCING_SIGNED_POLICY <- "error"

run_runtime_sourcing_pipeline <- function() {
  stopifnot(length(SOURCING_RUNS) > 0L, file.exists(SOURCING_SOURCE))
  source(SOURCING_SOURCE, local = environment())
  frozen_path <- function(run, relative) {
    frozen <- file.path(run, "Sourcing/metadata/input_snapshot", relative)
    if (file.exists(frozen)) return(frozen)
    current <- file.path(run, relative)
    if (!file.exists(current)) stop("Missing sourcing input: ", current, call. = FALSE)
    warning("No frozen snapshot; using current input: ", current, call. = FALSE)
    current
  }
  zones <- vapply(SOURCING_RUNS, frozen_path, character(1), relative = "LULCC/TempRaster/admin_c.tif")
  vectors <- vapply(SOURCING_RUNS, frozen_path, character(1), relative = "LULCC/TempVector/userarea.gpkg")
  .rs_require()
  base_crosswalk <- .rs_crosswalk(vectors[[1L]])
  for (i in seq_along(SOURCING_RUNS)) {
    if (!identical(.rs_crosswalk(vectors[[i]]), base_crosswalk)) {
      stop("Country ID/ISO3 crosswalk differs across the requested runs.", call. = FALSE)
    }
    if (!identical(unname(tools::md5sum(zones[[i]])), unname(tools::md5sum(zones[[1L]])))) {
      stop("Country zone rasters differ across the requested runs.", call. = FALSE)
    }
  }
  args <- c(paste0("--run-dir=", SOURCING_RUNS), paste0("--zones=", zones[[1L]]),
            paste0("--crosswalk=", vectors[[1L]]), paste0("--output-dir=", SOURCING_OUTPUT),
            paste0("--periods=", paste(SOURCING_PERIODS, collapse = ",")),
            paste0("--signed-policy=", SOURCING_SIGNED_POLICY),
            paste0("--overwrite=", if (SOURCING_OVERWRITE) "YES" else "NO"))
  rs_main(args)
}

if (!identical(Sys.getenv("MOFUSS_RUNTIME_SOURCING_NO_AUTORUN"), "1")) {
  run_runtime_sourcing_pipeline()
}
