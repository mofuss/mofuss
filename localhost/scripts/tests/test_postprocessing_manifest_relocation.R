# Synthetic portability test for relocated mc_bypass_manifest.csv consumers.
# All fixtures and destructive cleanup remain inside R's temporary directory.

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
scripts_dir <- file.path(repo_root, "localhost", "scripts")
post_dir <- file.path(scripts_dir, "postprocessing_emissions")
stage_files <- c(
  stage1 = file.path(post_dir, "1post_raster_fr_generator_diskmemory_v9.R"),
  stage2 = file.path(post_dir, "2post_emissions_bau-vs-ics_v13.R"),
  stage3 = file.path(post_dir, "3post_agb_decomposition_v5.R")
)
stopifnot(all(file.exists(stage_files)))

fixture <- tempfile("mofuss_manifest_relocation_")
actual_parent <- file.path(fixture, "current_computer")
old_parent <- file.path(fixture, "retired_computer")
bau_dir <- file.path(actual_parent, "ken_1000m_bau1_2050_mc30_capped")
ics_dir <- file.path(actual_parent, "ken_1000m_ics3_2050_mc30_capped")
dir.create(file.path(bau_dir, "Temp"), recursive = TRUE)
dir.create(file.path(ics_dir, "Temp"), recursive = TRUE)
on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

manifest_path <- file.path(ics_dir, "Temp", "mc_bypass_manifest.csv")
manifest <- data.frame(
  status = "complete",
  mode = "reuse_BAU_MC_tables",
  current_scenario_dir = file.path(old_parent, basename(ics_dir)),
  current_scenario_ver = "ICS3_v2",
  bau_source_dir = file.path(old_parent, basename(bau_dir)),
  bau_scenario_ver = "BaU1_v2",
  geography = "KEN",
  start_year = 2000L,
  end_year = 2050L,
  monte_carlo_runs = 30L,
  uncapped_regrowth = 0L,
  patcher_bypassed = TRUE,
  patcher_rng_paired = FALSE,
  stringsAsFactors = FALSE
)
write.csv(manifest, manifest_path, row.names = FALSE)

had_config_flag <- exists(
  "MOFUSS_CONFIG_ONLY", envir = .GlobalEnv, inherits = FALSE
)
old_config_flag <- if (had_config_flag) {
  get("MOFUSS_CONFIG_ONLY", envir = .GlobalEnv, inherits = FALSE)
} else {
  NULL
}
assign("MOFUSS_CONFIG_ONLY", TRUE, envir = .GlobalEnv)
on.exit({
  if (had_config_flag) {
    assign("MOFUSS_CONFIG_ONLY", old_config_flag, envir = .GlobalEnv)
  } else if (exists("MOFUSS_CONFIG_ONLY", envir = .GlobalEnv, inherits = FALSE)) {
    rm("MOFUSS_CONFIG_ONLY", envir = .GlobalEnv)
  }
}, add = TRUE)

load_stage <- function(path) {
  env <- new.env(parent = globalenv())
  env$MOFUSS_CONFIG_ONLY <- TRUE
  sys.source(path, envir = env)
  env
}
stage1 <- load_stage(stage_files[["stage1"]])
stage2 <- load_stage(stage_files[["stage2"]])
stage3 <- load_stage(stage_files[["stage3"]])

bau_v13 <- list(
  scenario = "BaU1_v2", iso3 = "KEN", model_start_year = 2000L,
  model_end_year = 2050L, mc_runs = 30L, uncapped_regrowth = 0L
)
ics_v13 <- list(
  scenario = "ICS3_v2", iso3 = "KEN", model_start_year = 2000L,
  model_end_year = 2050L, mc_runs = 30L, uncapped_regrowth = 0L
)
bau_v5 <- list(
  scenario_ver = "BaU1_v2", country_iso = "KEN",
  simulation_start_year = 2000L, simulation_end_year = 2050L,
  monte_carlo_runs = 30L, uncapped_regrowth = 0L
)
ics_v5 <- list(
  scenario_ver = "ICS3_v2", country_iso = "KEN",
  simulation_start_year = 2000L, simulation_end_year = 2050L,
  monte_carlo_runs = 30L, uncapped_regrowth = 0L
)
cfg_v5 <- list(label = "ken_test", bau_dir = bau_dir, ics_dir = ics_dir)

check_all_consumers <- function() {
  one <- stage1$read_pairing_provenance(ics_dir, "ICS3_v2")
  two <- stage2$.v13_read_bypass_provenance(
    bau_dir, ics_dir, bau_v13, ics_v13
  )
  three <- stage3$read_pairing_provenance(
    cfg_v5, bau_v5, ics_v5, "strict"
  )
  stopifnot(
    identical(one$mc_bau_source_dir, normalizePath(bau_dir, winslash = "/")),
    identical(two$current_scenario_dir, normalizePath(ics_dir, winslash = "/")),
    identical(two$bau_source_dir, normalizePath(bau_dir, winslash = "/")),
    isTRUE(two$metadata_validated),
    isTRUE(three$comparison_validated)
  )
}

# Historical absolute paths from another computer must relocate safely.
check_all_consumers()

# New v8 relative references must resolve without needing relocation fallback.
manifest$current_scenario_rel <- ".."
manifest$bau_source_rel <- file.path("..", "..", basename(bau_dir))
write.csv(manifest, manifest_path, row.names = FALSE)
check_all_consumers()

# A stale path with a different folder identity must remain a hard failure.
manifest$current_scenario_rel <- NULL
manifest$bau_source_rel <- NULL
manifest$current_scenario_dir <- file.path(old_parent, "wrong_scenario_folder")
write.csv(manifest, manifest_path, row.names = FALSE)
expect_rejection <- function(expression) {
  error <- tryCatch({
    force(expression)
    NULL
  }, error = identity)
  stopifnot(
    inherits(error, "error"),
    grepl("differently named", conditionMessage(error), fixed = TRUE)
  )
}
expect_rejection(stage1$read_pairing_provenance(ics_dir, "ICS3_v2"))
expect_rejection(stage2$.v13_read_bypass_provenance(
  bau_dir, ics_dir, bau_v13, ics_v13
))
expect_rejection(stage3$read_pairing_provenance(
  cfg_v5, bau_v5, ics_v5, "strict"
))

# An existing directory with the same leaf name is ambiguous and must not be
# silently substituted for the selected run.
ambiguous_parent <- file.path(fixture, "other_existing_computer")
ambiguous_ics <- file.path(ambiguous_parent, basename(ics_dir))
dir.create(ambiguous_ics, recursive = TRUE)
manifest$current_scenario_dir <- ambiguous_ics
write.csv(manifest, manifest_path, row.names = FALSE)
expect_rejection(stage1$read_pairing_provenance(ics_dir, "ICS3_v2"))
expect_rejection(stage2$.v13_read_bypass_provenance(
  bau_dir, ics_dir, bau_v13, ics_v13
))
expect_rejection(stage3$read_pairing_provenance(
  cfg_v5, bau_v5, ics_v5, "strict"
))

cat("POSTPROCESSING_MANIFEST_RELOCATION_TEST_OK\n")
