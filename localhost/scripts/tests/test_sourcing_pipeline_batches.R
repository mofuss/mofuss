# Run from the repository root. Both real analyses run on tiny synthetic data.
# The copied repository, inputs and outputs live only in R temporary storage.
verify_sourcing_pipeline <- function() {
  repo <- normalizePath(getwd(), winslash="/", mustWork=TRUE)
  fixture <- new.env(parent=globalenv())
  fixture$MOFUSS_KEEP_SOURCING_TEST_FIXTURE <- TRUE
  sys.source(file.path(repo, "localhost/scripts/tests/test_runtime_sourcing_v1.R"), fixture)
  root <- tempfile("sourcing batches & spaces ")
  dir.create(root)
  on.exit({
    unlink(fixture$scratch, recursive=TRUE)
    unlink(root, recursive=TRUE)
  }, add=TRUE)
  post <- "localhost/scripts/postprocessing_sourcing"
  copied_post <- file.path(root, "repo with spaces", post)
  dir.create(copied_post, recursive=TRUE)
  scripts <- c("0post_runtime_sourcing_pipeline_v1.R",
               "1post_model_implied_sourcing_v1.R", "2post_runtime_sourcing_v1.R")
  stopifnot(all(file.copy(file.path(repo, post, scripts), copied_post)))
  folders <- c("AAA_bau1_capped", "AAA_bau1_uncapped", "AAA_ics3_capped", "AAA_ics3_uncapped")
  roots <- c(root, file.path(root, "another drive & folder"))
  for (parent in roots) for (folder in folders) {
    run <- file.path(parent, folder)
    dir.create(run, recursive=TRUE)
    files <- list.files(fixture$scratch, recursive=TRUE, full.names=FALSE)
    files <- files[!startsWith(files, "output/")]
    for (name in files) {
      dest <- file.path(run, name)
      dir.create(dirname(dest), recursive=TRUE, showWarnings=FALSE)
      stopifnot(file.copy(file.path(fixture$scratch, name), dest))
    }
    data.table::fwrite(data.table::data.table(
      Var=c("start_year", "end_year", "monte_carlo_runs", "uncapped_regrowth", "npa_ease"),
      ParCHR=c(2020, 2020, 1, as.integer(grepl("uncapped", folder)), 100)
    ), file.path(run, "LULCC/TempTables/parameters_dinamica.csv"))
    dir.create(file.path(run, "LULCC/TempRaster"), showWarnings=FALSE)
    stopifnot(file.copy(fixture$zonepath, file.path(run, "LULCC/TempRaster/admin_c.tif")))
    npa <- terra::setValues(fixture$template, NA_real_)
    terra::writeRaster(npa, file.path(run, "LULCC/TempRaster/npa_c.tif"))
    dir.create(file.path(run, "Sourcing/metadata"), showWarnings=FALSE)
    data.table::fwrite(fixture$cw, file.path(run, "Sourcing/metadata/country_crosswalk.csv"))
    for (channel in c("W", "V")) {
      index <- data.table::copy(fixture$indices[[channel]])
      index[, `:=`(JobID=paste0(channel, ComponentIndex), DirectionRule="fixture")]
      relative <- file.path("In/DemandScenarios", paste0(channel, "_origin_component_index.csv"))
      data.table::fwrite(index, file.path(run, relative))
      frozen <- file.path(run, "Sourcing/metadata/input_snapshot", relative)
      if (file.exists(frozen)) stopifnot(file.copy(file.path(run, relative), frozen, overwrite=TRUE))
      demand <- if (channel=="W") c(100,100) else c(40,20)
      data.table::fwrite(data.table::data.table(Key=1:2, Value=demand),
        file.path(run, "In/DemandScenarios", paste0(channel, "_origin_demand01.csv")))
      component_dir <- file.path(run, "In", paste0(channel, "_origin_components"))
      dir.create(component_dir)
      for (i in 1:2) stopifnot(file.copy(
        file.path(run, "Sourcing/static", sprintf("%s_base%03d_01.tif", channel, i)),
        file.path(component_dir, sprintf("IDW_C++_fw_%s%03d_01.tif", tolower(channel), i))
      ))
    }
    demand_dir <- file.path(run, "LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in")
    dir.create(demand_dir, recursive=TRUE)
    role <- if (grepl("bau1", folder)) "bau1" else "ics3"
    data.table::fwrite(data.table::data.table(iso3=c("AAA","BBB"), area="urban",
      fuel="fuelwood", year=2020, fuel_cons_tons=c(40,20)),
      file.path(demand_dir, paste0("demand_", role, "_v2.csv")))
  }
  env <- new.env(parent=globalenv()); env$MOFUSS_CONFIG_ONLY <- TRUE
  sys.source(file.path(copied_post, scripts[[1L]]), env)
  env$SOURCING_BATCHES <- list(
    First=list(enabled=TRUE, root="", analysis_folder="first analysis", folders=folders),
    Disabled=list(enabled=FALSE),
    Second=list(enabled=TRUE, root=roots[[2L]], analysis_folder="second analysis", folders=folders)
  )
  env$SOURCING_PERIODS <- "2020:2020"
  env$SOURCING_MC_RUNS <- "1"
  env$SOURCING_TEMP_DIR <- file.path(root, "scratch with spaces")
  plan <- env$.sp_plan(env$.sourcing_pipeline_file)
  stopifnot(identical(names(plan$batches), c("First","Second")),
    identical(plan$disabled, "Disabled"),
    identical(unname(plan$batches$First$root), normalizePath(root, winslash="/")))
  outputs <- unlist(lapply(plan$batches, `[[`, "outputs"), use.names=FALSE)

  # A missing input in the later batch must be found before the first writes.
  missing <- file.path(roots[[2L]], folders[[1L]], "debugging_1/Expect_harv_tot01.tif")
  stopifnot(file.rename(missing, paste0(missing, ".held")))
  failure <- tryCatch(env$run_runtime_sourcing_pipeline(), error=identity)
  stopifnot(inherits(failure, "error"), !any(dir.exists(outputs)))
  stopifnot(file.rename(paste0(missing, ".held"), missing))

  env$run_runtime_sourcing_pipeline("--check")
  stopifnot(!any(dir.exists(outputs)))
  temp_vars <- c("TMPDIR", "TMP", "TEMP", "MOFUSS_SOURCING_TEMP_DIR")
  previous_temp <- Sys.getenv(temp_vars, unset=NA_character_)
  env$run_runtime_sourcing_pipeline()
  stopifnot(identical(previous_temp, Sys.getenv(temp_vars, unset=NA_character_)))
  for (batch in plan$batches) {
    approximate <- data.table::fread(file.path(batch$outputs[[1L]], "sourcing_conservation_qa.csv"))
    recorded <- data.table::fread(file.path(batch$outputs[[2L]], "runtime_sourcing_qa.csv"))
    stopifnot(nrow(approximate)==4L, nrow(recorded)==4L,
      all(abs(approximate$W_conservation_difference_tons)<1e-8),
      all(abs(approximate$V_conservation_difference_tons)<1e-8),
      all(abs(recorded$origin_source_reconciliation_residual_tonnes)<1e-8))
  }
  saved <- lapply(outputs, function(dir) tools::md5sum(list.files(dir, full.names=TRUE)))
  failure <- tryCatch(env$run_runtime_sourcing_pipeline(), error=identity)
  stopifnot(inherits(failure, "error"), identical(saved,
    lapply(outputs, function(dir) tools::md5sum(list.files(dir, full.names=TRUE)))))
  env$SOURCING_BATCHES$Second$root <- ""
  failure <- tryCatch(env$.sp_plan(env$.sourcing_pipeline_file), error=identity)
  stopifnot(inherits(failure, "error"), grepl("reuse a scenario", conditionMessage(failure)))
  cat("BATCH PIPELINE PASSED: two enabled batches, both analyses, disabled placeholders,\n",
      "relocated scripts, paths with spaces/ampersands, no-write preflight, overwrite protection,\n",
      "distinct outputs, preserved accounting and restored temporary-directory settings.\n", sep="")
}
verify_sourcing_pipeline()
