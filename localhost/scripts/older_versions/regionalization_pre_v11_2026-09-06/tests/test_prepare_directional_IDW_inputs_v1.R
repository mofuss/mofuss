suppressPackageStartupMessages({
  library(terra)
  library(data.table)
})

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
stopifnot(length(script_argument) == 1L)
test_script <- sub("^--file=", "", script_argument)
repository_root <- normalizePath(
  file.path(dirname(test_script), "..", "..", ".."),
  winslash = "/",
  mustWork = TRUE
)

old_autorun <- Sys.getenv("MOFUSS_6E_NO_AUTORUN", unset = NA_character_)
Sys.setenv(MOFUSS_6E_NO_AUTORUN = "1")
on.exit({
  if (is.na(old_autorun)) {
    Sys.unsetenv("MOFUSS_6E_NO_AUTORUN")
  } else {
    Sys.setenv(MOFUSS_6E_NO_AUTORUN = old_autorun)
  }
}, add = TRUE)
source(file.path(
  repository_root,
  "localhost",
  "scripts",
  "6e_prepare_directional_IDW_inputs_v1.R"
))

fixture <- tempfile("prepare_directional_idw_")
dir.create(fixture, recursive = TRUE)
on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

# Script 6e is sourced for every run by the main driver. A valid run without a
# directional HC_jobs directory must therefore be a successful no-op, while an
# HC_jobs directory missing its harmonized manifest remains an error.
non_directional_run <- file.path(fixture, "non_directional_run")
dir.create(file.path(non_directional_run, "In"), recursive = TRUE)
invisible(file.create(file.path(
  non_directional_run,
  "In",
  c("fricc_w.tif", "fricc_v.tif")
)))
had_countrydir <- exists("countrydir", envir = .GlobalEnv, inherits = FALSE)
old_countrydir <- get0("countrydir", envir = .GlobalEnv, inherits = FALSE)
assign("countrydir", non_directional_run, envir = .GlobalEnv)
resolved_run <- .idw_resolve_run_root()
if (had_countrydir) {
  assign("countrydir", old_countrydir, envir = .GlobalEnv)
} else {
  rm("countrydir", envir = .GlobalEnv)
}
stopifnot(identical(
  resolved_run,
  normalizePath(non_directional_run, winslash = "/", mustWork = TRUE)
))
no_jobs <- prepare_directional_idw_inputs(non_directional_run)
stopifnot(is.data.frame(no_jobs), nrow(no_jobs) == 0L)

incomplete_directional_run <- file.path(fixture, "incomplete_directional_run")
dir.create(
  file.path(incomplete_directional_run, "In", "DemandScenarios", "HC_jobs"),
  recursive = TRUE
)
incomplete_error <- tryCatch(
  {
    prepare_directional_idw_inputs(incomplete_directional_run)
    NULL
  },
  error = identity
)
stopifnot(
  inherits(incomplete_error, "error"),
  grepl("Harmonized HC-job manifest does not exist", conditionMessage(incomplete_error))
)

template <- terra::rast(
  nrows = 3,
  ncols = 3,
  xmin = 0,
  xmax = 3000,
  ymin = 0,
  ymax = 3000,
  crs = "EPSG:3857"
)

# No-data inside the permitted domain receives the declared neutral fallback;
# explicit barriers and values outside the domain are not changed into sources.
friction_values <- c(NA, 1, 999999, rep(2, 6))
allowed_values <- c(TRUE, TRUE, TRUE, rep(FALSE, 6))
repaired <- .idw_fill_friction_gaps(
  friction_values,
  allowed_values,
  fallback_value = 0.4,
  barrier_value = 999999
)
stopifnot(
  repaired$gap_count == 1L,
  repaired$values[[1L]] == 0.4,
  repaired$values[[3L]] == 999999,
  all(is.na(repaired$values[4:9]))
)

# ID 10 begins outside the source domain and snaps onto the already occupied
# cell containing ID 20. It must be merged there instead of being moved farther
# merely to retain a separate raster ID.
locations <- list(cells = c(1L, 2L), ids = c(10L, 20L))
mapping <- .idw_collapse_location_mapping(
  locations,
  allowed_values = c(FALSE, TRUE, rep(FALSE, 7)),
  template = template,
  max_radius_cells = 4L
)
stopifnot(
  identical(mapping$target_cells, c(2L, 2L)),
  identical(mapping$survivor_ids, c(20L, 20L)),
  identical(mapping$survivor_indices, 2L),
  mapping$adjustments$Action[[1L]] == "merged_into_survivor"
)

input_demand <- file.path(fixture, "BaU_fwch_w.csv")
output_demand <- file.path(fixture, "BaU_fwch_w_idw.csv")
data.table::fwrite(data.table::data.table(
  ID = c(10L, 20L),
  `2000_fw_w` = c(3, 5),
  `2001_fw_w` = c(4, 6)
), input_demand)
collapsed <- .idw_write_collapsed_demand(
  input_demand,
  output_demand,
  location_ids = locations$ids,
  survivor_ids = mapping$survivor_ids,
  channel = "w"
)
installed <- data.table::fread(output_demand)
stopifnot(
  collapsed$rows == 1L,
  collapsed$merged_ids == 1L,
  collapsed$max_annual_delta == 0,
  installed$ID[[1L]] == 20L,
  installed$`2000_fw_w`[[1L]] == 8,
  installed$`2001_fw_w`[[1L]] == 10
)

cat("6e directional IDW preparation tests passed.\n")
