# Regression for the original-AGB reporting footprint in Stages 2 and 3.
# Synthetic rasters and tables are written only to R's temporary directory.

check_biomass_support <- function() {
  post <- file.path(getwd(), "localhost", "scripts", "postprocessing_emissions")
  old_flag <- get0("MOFUSS_CONFIG_ONLY", .GlobalEnv, inherits = FALSE)
  had_flag <- exists("MOFUSS_CONFIG_ONLY", .GlobalEnv, inherits = FALSE)
  assign("MOFUSS_CONFIG_ONLY", TRUE, .GlobalEnv)
  on.exit({
    if (had_flag) assign("MOFUSS_CONFIG_ONLY", old_flag, .GlobalEnv) else
      rm("MOFUSS_CONFIG_ONLY", envir = .GlobalEnv)
  }, add = TRUE)
  load_stage <- function(name) {
    e <- new.env(parent = globalenv())
    e$MOFUSS_CONFIG_ONLY <- TRUE
    sys.source(file.path(post, name), envir = e)
    e
  }
  s2 <- load_stage("2post_emissions_bau-vs-ics_v14.R")
  s3 <- load_stage("3post_agb_decomposition_v6.R")
  scratch <- tempfile("mofuss_biomass_support_")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  template <- terra::rast(nrows = 2, ncols = 4, xmin = 0, xmax = 4000,
                          ymin = 0, ymax = 2000, crs = "EPSG:3395")
  raster <- function(x) terra::setValues(template, x)
  r <- list(
    # Cells 1 and 5 acquire finite stocks despite a missing original reference.
    # Cell 2 is an original numeric zero; cell 4 loses growth-parameter coverage.
    ref_bau = raster(c(NA, 0, 100, 50, NA, NA, 80, 40)),
    bau_baseline = raster(c(10, 0, 90, 40, 10, NA, 60, 20)),
    ics_baseline = raster(c(10, 0, 90, 40, 10, NA, 62, 20)),
    bau_end = raster(c(20, 5, 70, NA, 15, NA, 55, 10)),
    ics_end = raster(c(50, 8, 90, NA, 5, NA, 60, 5))
  )
  r$ref_ics <- r$ref_bau
  v <- function(x) as.numeric(terra::values(x))
  result <- s2$.v14_biomass_period(r$ref_bau, r$bau_baseline, r$ics_baseline,
                                  r$bau_end, r$ics_end)
  expected <- c(NA, 3, 20, NA, NA, NA, 3, -5)
  stopifnot(isTRUE(all.equal(v(result$delta), expected)),
            result$n_raw_pair == 6, result$n_excluded == 2,
            s2$.v9_global_sum(result$excluded_delta) == 20,
            isTRUE(all.equal(v(result$excluded_delta), c(30, NA, NA, NA, -10, NA, NA, NA))))
  support <- s3$v6_biomass_support(r)
  stopifnot(identical(v(support$pair_valid), v(result$common_support)))
  paths <- lapply(names(r), function(nm) {
    path <- file.path(scratch, paste0(nm, ".tif"))
    terra::writeRaster(r[[nm]], path, overwrite = TRUE)
    path
  })
  names(paths) <- names(r)
  ref_md5 <- unname(tools::md5sum(paths$ref_bau))
  policy <- "finite_initial_agb_reference_v1"
  factor <- s2$.V9_CO2_FACTOR
  scope <- data.frame(country_id = 1:2, country_iso = c("AAA", "BBB"),
                      country_name = c("A", "B"), analysis_area_kind = "Regional",
                      analysis_area_id = "TEST", analysis_area_name = "Test")
  incidence <- data.frame(country_id = 1:2,
                          harvest_avoided_tCO2e = c(23, -2) * factor,
                          enduse_avoided_tCO2e = c(100, 200),
                          total_avoided_tCO2e = c(23, -2) * factor + c(100, 200))
  pairing <- list(pairing_policy = "strict", patcher_bypassed = TRUE,
                  patcher_rng_paired = FALSE, comparison_validated = TRUE,
                  full_stochastic_pairing_validated = TRUE,
                  pairing_design = "paired_mc_inputs_patcher_bypassed",
                  independent_patcher_rng_included = FALSE,
                  uncertainty_status = "paired_mc_inputs_validated_patcher_skipped")
  meta <- list(label = "synthetic", safe_label = "synthetic", regrowth_mode = "capped",
               raster_paths = paths, reference_md5 = ref_md5,
               pairing = pairing, mc_table_pairing_validated = TRUE,
               full_horizon = FALSE, baseline_year = 2025L, baseline_code = 26L,
               baseline_source = "Growth_less_harv", baseline_timing = "end_of_previous_year",
               end_code = 51L, bau_params = list(simulation_start_year = 2000L),
               ics_params = list(scenario_ver = "ICS"),
               harvest = list(tco2e = 21 * factor),
               enduse = list(tco2e = 300, max_abs_residual_tco2e = 0,
                             per_fuel = data.frame(fuel = "example", delta = 300)),
               country_partition = list(scope = scope, incidence = incidence,
                                        zones = raster(rep(1:2, each = 4))))
  out <- s3$process_config(meta, 1L, list(start = 2026L, end = 2050L), factor, 1e-6)
  stopifnot(isTRUE(all.equal(v(out$rasters$delta_mg), expected)),
            out$row$period_avoided_loss_mg == 18, out$row$period_regrowth_mg == 3,
            out$row$raw_reference_excluded_delta_mg == 20,
            out$row$raw_reference_excluded_positive_mg == 30,
            out$row$raw_reference_excluded_negative_mg == -10,
            out$row$n_reference_excluded_pair_cells == 2,
            out$row$reference_excluded_delta_mg == 0,
            out$row$all_invariants_ok, all(out$country_rows$all_invariants_ok),
            out$row$enduse_avoided_tco2e == 300,
            out$row$total_avoided_tco2e == 21 * factor + 300,
            all(out$country_rows$biomass_support_policy == policy),
            all(out$country_rows$agb_reference_md5 == ref_md5),
            out$row$biomass_support_policy == policy, out$row$agb_reference_md5 == ref_md5)
  # Excluded raw benefits may change sign or magnitude without changing reported biomass.
  alternate <- r
  alternate$ics_end <- raster(c(-500, 8, 90, NA, 500, NA, 60, 5))
  alt <- s2$.v14_biomass_period(alternate$ref_bau, alternate$bau_baseline,
                               alternate$ics_baseline, alternate$bau_end, alternate$ics_end)
  stopifnot(identical(v(alt$delta), v(result$delta)))
  expect_error <- function(expr, pattern) {
    err <- tryCatch(force(expr), error = identity)
    stopifnot(inherits(err, "error"), grepl(pattern, conditionMessage(err)))
  }
  manifest <- data.frame(biomass_support_policy = policy, initial_agb_md5 = ref_md5)
  stopifnot(s3$validate_stage2_biomass_support(manifest, ref_md5, "fixture"))
  expect_error(s3$validate_stage2_biomass_support(manifest["initial_agb_md5"], ref_md5, "fixture"), "rerun Stage 2")
  wrong <- manifest; wrong$biomass_support_policy <- "unmasked"
  expect_error(s3$validate_stage2_biomass_support(wrong, ref_md5, "fixture"), "policy")
  wrong <- manifest; wrong$initial_agb_md5 <- strrep("0", 32)
  expect_error(s3$validate_stage2_biomass_support(wrong, ref_md5, "fixture"), "MD5")
  # The actual manifest reader invokes the guard even for a post-spin-up period.
  utils::write.csv(manifest["initial_agb_md5"], file.path(scratch, "run_manifest.csv"), row.names = FALSE)
  expect_error(s3$read_stage2_run_manifest(list(emissions_dir = scratch),
                                         list(start = 2026L, end = 2050L), 1L,
                                         list(full_horizon = FALSE), ref_md5, pairing), "rerun Stage 2")
  shifted <- r$ref_bau; terra::ext(shifted) <- terra::ext(1000, 5000, 0, 2000)
  expect_error(s2$.v14_biomass_period(shifted, r$bau_baseline, r$ics_baseline,
                                     r$bau_end, r$ics_end), "geometry")
  cat("Fixed biomass support passed: zero retention, NoData exclusion, signed diagnostics,\n",
      "Stage-2/3 and country reconciliation, unchanged end use, and provenance rejection.\n", sep = "")
}

check_biomass_support()
