# Stage 5 accounting regression: current catalog, legacy regional partitions,
# singleton addition, coherent uncertainty, and rejection of duplicate inputs.
# Run from the repository root. All fixture output stays in disposable scratch.

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
stage5 <- file.path(repo_root, "localhost/scripts/postprocessing_emissions",
                    "5post_manuscript_outputsGLOBAL_v1.R")
expressions <- parse(stage5)
scratch_parent <- Sys.getenv(
  "MOFUSS_TEST_SCRATCH", "E:/MoFuSS_Active/stage5_global_incremental_fix"
)
dir.create(scratch_parent, recursive = TRUE, showWarnings = FALSE)
fixture <- tempfile("accounting_", tmpdir = scratch_parent)
analysis_parent <- file.path(fixture, "analyses")
dir.create(analysis_parent, recursive = TRUE)

write_analysis <- function(id, totals) {
  rows <- do.call(rbind, lapply(names(totals), function(iso) {
    do.call(rbind, lapply(c("capped", "uncapped"), function(configuration) {
      total <- totals[[iso]] * 25e6 * if (configuration == "capped") 1 else 2
      data.frame(
        country_iso = iso, country_name = iso, analysis_area_id = id,
        analysis_area_name = id, regrowth_mode = configuration,
        run_id = seq_along(total), period_start_year = 2026L,
        period_end_year = 2050L,
        period_avoided_loss_tco2e = total / 2,
        period_regrowth_tco2e = total / 4,
        agb_avoided_stage2_tco2e = total * 3 / 4,
        enduse_avoided_tco2e = total / 4,
        total_avoided_tco2e = total, all_invariants_ok = TRUE
      )
    }))
  }))
  path <- file.path(analysis_parent, id, "agb_decomposition",
                    "agb_decomposition_by_country_per_run_fixture.csv")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(rows, path, row.names = FALSE)
  path
}

# Execute the real CLI calculations through validation, stopping before output
# publication/figures. Real-data end-to-end checks cover the rendering path.
calculate <- function(extra_args = character()) {
  env <- new.env(parent = globalenv())
  args <- c(
    paste0("--analysis-parent=", analysis_parent),
    paste0("--output-dir=", fixture, "/global/manuscript_outputs"),
    paste0("--temp-dir=", fixture, "/scratch"),
    "--mode=partial", "--min-runs=3", "--global-resamples=1000"
  )
  for (arg in extra_args) {
    prefix <- sub("=.*", "=", arg)
    args <- c(args[!startsWith(args, prefix)], arg)
  }
  env$commandArgs <- function(trailingOnly = FALSE) {
    if (trailingOnly) args else paste0("--file=", stage5)
  }
  for (expression in expressions) {
    assignment <- is.call(expression) && identical(expression[[1L]], as.name("<-"))
    if (assignment && identical(expression[[2L]], as.name("prepare_output_dir"))) break
    if (assignment && identical(expression[[2L]], as.name("source_mode"))) {
      env$source_mode <- FALSE
    } else {
      eval(expression, envir = env)
    }
  }
  env
}
expect_failure <- function(args, pattern) {
  result <- tryCatch(calculate(args), error = identity)
  stopifnot(inherits(result, "error"), grepl(pattern, conditionMessage(result)))
}
check_global <- function(result, capped_total) {
  for (configuration in c("capped", "uncapped")) {
    target <- capped_total * if (configuration == "capped") 1 else 2
    rows <- result$global_draws[result$global_draws$configuration == configuration, ]
    stopifnot(all(abs(rows$total - target) < 1e-10))
  }
  from_subregions <- aggregate(
    total ~ configuration + run_id, result$subregion_draws, sum
  )
  joined <- merge(from_subregions, result$global_draws,
                  by = c("configuration", "run_id"), suffixes = c("_sum", "_global"))
  stopifnot(all(abs(joined$total_sum - joined$total_global) < 1e-10))
}

# Perfectly anticorrelated countries span current canonical regions but share
# one original simulation. Their combined uncertainty must remain exactly zero.
legacy_path <- write_analysis("legacy", list(KEN = c(10, 30, 50), SDN = c(50, 30, 10)))
before <- calculate()
check_global(before, 60)
stopifnot(nrow(before$source_countries) == 2L,
          before$manifest$partition_status == "legacy_partition")

singleton_path <- write_analysis("gabon", list(GAB = c(9, 9, 9)))
after <- calculate()
check_global(after, 69)
stopifnot(nrow(after$manifest) == 2L, nrow(after$source_countries) == 3L,
          all(c("KEN", "SDN", "GAB") %in% after$country_summary$country_iso),
          !anyNA(after$country_draws$subregion_number),
          all(after$coverage$coverage_status[after$coverage$GID_0 %in%
                c("KEN", "SDN", "GAB")] == "included"))
gab <- after$manifest[after$manifest$analysis_id == "GABON", ]
stopifnot(gab$partition_status == "final")
check_global(calculate("--mc-combination=aligned"), 69)
stopifnot(identical(after$global_draws, calculate()$global_draws))
expect_failure("--mode=strict", "not matching one complete canonical region")

# An explicit catalog must override the default; full coverage still must not
# bypass strict mode's requirement for complete canonical partitions.
catalog <- read.csv(file.path(repo_root, "admin_regions/regionalization_M85_B30_V1.csv"))
catalog <- catalog[catalog$GID_0 %in% c("KEN", "SDN", "GAB"), ]
catalog_path <- file.path(fixture, "explicit_catalog.csv")
write.csv(catalog, catalog_path, row.names = FALSE)
custom <- calculate(paste0("--regionalization-file=", catalog_path))
stopifnot(custom$coverage_complete, nrow(custom$coverage) == 3L)
expect_failure(c("--mode=strict", paste0("--regionalization-file=", catalog_path)),
               "not matching one complete canonical region")

# Reject repeated rows and overlapping analysis roots rather than double count.
singleton <- read.csv(singleton_path)
write.csv(rbind(singleton, singleton[1L, ]), singleton_path, row.names = FALSE)
expect_failure(character(), "repeats country/configuration/run rows")
write.csv(singleton, singleton_path, row.names = FALSE)
write_analysis("gabon_duplicate", list(GAB = c(9, 9, 9)))
expect_failure(character(), "overlap country ISO codes")
cat("PASS: Stage 5 incremental accounting, singletons, coherent draws, and input guards\n")
cat("FIXTURE=", fixture, "\n", sep = "")
