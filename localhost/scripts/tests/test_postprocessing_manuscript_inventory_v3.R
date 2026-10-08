# Exercise the real Stage 4 inventory block without running rendering or
# touching any country/region output. Run from the repository root.

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
stage4 <- file.path(repo_root, "localhost/scripts/postprocessing_emissions",
                    "4post_manuscript_outputs_v3.R")
expressions <- parse(stage4)
is_assignment <- function(expression, name) {
  is.call(expression) && identical(expression[[1L]], as.name("<-")) &&
    identical(expression[[2L]], as.name(name))
}
assignment_index <- function(name) {
  index <- which(vapply(expressions, is_assignment, logical(1), name = name))
  stopifnot(length(index) == 1L)
  index
}
inventory_start <- assignment_index("expected_files")
inventory_end <- which(vapply(expressions, function(expression) {
  is.call(expression) && identical(expression[[1L]], as.name("if")) &&
    any(grepl("Final package inventory differs", deparse(expression), fixed = TRUE))
}, logical(1)))
stopifnot(length(inventory_end) == 1L, inventory_end > inventory_start)
inventory_expressions <- expressions[inventory_start:inventory_end]

scratch <- Sys.getenv(
  "MOFUSS_TEST_SCRATCH",
  "E:/MoFuSS_Active/emissions_stage4_inventory_2026-10-07"
)
dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
scratch <- normalizePath(scratch, winslash = "/", mustWork = TRUE)
if (identical(tolower(scratch), tolower(repo_root)) ||
    startsWith(tolower(paste0(scratch, "/")), tolower(paste0(repo_root, "/")))) {
  stop("Inventory fixture must be outside the source repository.", call. = FALSE)
}
fixture <- tempfile("stage4_inventory_", tmpdir = scratch)
dir.create(fixture)

# Independent physical fixture inventory: includes the support-policy file in
# both branches. These are intentionally empty placeholders: only filenames
# are relevant to the final package gate under test.
fixture_inventory <- function(ensemble) {
  scope <- if (ensemble) "mc_all" else "mc_1"
  country_stat <- if (ensemble) "mc_all" else "mc1"
  files <- c(
    "biomass_support_policy.csv",
    "figures/mc_1/figure_tst_2026-2050_emissions_maps.png",
    "tables/table_tst_2026-2050_mc_1.csv",
    "tables/table_tst_2026-2050_mc_1.png",
    "tables/table_tst_2026-2050_by_country_mc_1.csv",
    "tables/table_tst_2026-2050_by_country_capped_mc_1.csv",
    "tables/table_tst_2026-2050_by_country_uncapped_mc_1.csv",
    sprintf("figures/%s/figure_tst_2026-2050_by_country_contributions_%s.png",
            scope, country_stat),
    "spatial/country_scope.csv",
    "spatial/country_boundaries.gpkg"
  )
  for (configuration in c("capped", "uncapped")) {
    for (component in c("harvest", "enduse", "total")) {
      files <- c(files, sprintf("rasters/mc_1/tst_2026-2050_%s_%s_mc1_tco2e.tif",
                                configuration, component))
      if (ensemble) {
        files <- c(files, sprintf("rasters/mc_all/tst_2026-2050_%s_%s_%s_tco2e.tif",
                                  configuration, component, c("mean", "sd")))
      }
    }
  }
  if (ensemble) {
    files <- c(files,
      "figures/mc_all/figure_tst_2026-2050_emissions_maps_wuncer.png",
      "tables/table_tst_2026-2050_mc_all.csv",
      "tables/table_tst_2026-2050_mc_all.png",
      "tables/table_tst_2026-2050_by_country_mc_all.csv")
  }
  files
}

evaluate_inventory <- function(output_dir, ensemble) {
  env <- new.env(parent = baseenv())
  env$output_dir <- output_dir
  env$uncertainty_adequate <- ensemble
  env$region_slug <- "tst"
  env$period_tag <- "2026-2050"
  env$setNames <- stats::setNames
  env$stopf <- function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)
  # Use the production naming expressions and raster helper, rather than
  # sourcing Stage 4 and executing its upstream computations or output writes.
  for (name in c(
    "CONFIGURATION_ORDER", "COMPONENT_ORDER", "figure_path",
    "mc_all_figure_path", "table_mc1_path", "table_mc_all_path",
    "table_mc1_png_path", "table_mc_all_png_path", "country_tidy_mc1_path",
    "country_tidy_mc_all_path", "country_compact_csv_paths",
    "country_figure_scope", "country_figure_statistic", "country_figure_path",
    "country_scope_output_path", "country_boundaries_output_path", "raster_path"
  )) {
    eval(expressions[[assignment_index(name)]], envir = env)
  }
  for (expression in inventory_expressions) eval(expression, envir = env)
  env
}

missing_policy <- "biomass_support_policy.csv"
missing_raster <- "rasters/mc_1/tst_2026-2050_capped_harvest_mc1_tco2e.tif"
unexpected_file <- "tables/unexpected_inventory_probe.csv"
checks <- 0L
for (ensemble in c(FALSE, TRUE)) {
  branch <- if (ensemble) "ensemble" else "mc1_only"
  expected <- fixture_inventory(ensemble)
  stopifnot(length(expected) == if (ensemble) 32L else 16L,
            !anyDuplicated(expected))
  for (case in c("valid", "missing_policy", "missing_raster", "unexpected_file")) {
    output_dir <- file.path(fixture, branch, case)
    files <- switch(case,
      valid = expected,
      missing_policy = setdiff(expected, missing_policy),
      missing_raster = setdiff(expected, missing_raster),
      unexpected_file = c(expected, unexpected_file))
    paths <- file.path(output_dir, files)
    for (directory in unique(dirname(paths))) {
      dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    }
    stopifnot(all(file.create(paths)))
    result <- tryCatch(evaluate_inventory(output_dir, ensemble), error = identity)
    if (case == "valid") {
      if (inherits(result, "error")) stop(result)
      stopifnot(setequal(gsub("\\\\", "/", result$expected_files), expected),
                length(result$actual_files) == length(expected))
    } else {
      offending_path <- switch(case,
        missing_policy = missing_policy,
        missing_raster = missing_raster,
        unexpected_file = unexpected_file)
      stopifnot(inherits(result, "error"))
      message <- conditionMessage(result)
      stopifnot(grepl("Final package inventory differs", message, fixed = TRUE),
                grepl(offending_path, message, fixed = TRUE),
                grepl(paste0("expected ", length(expected), " files"), message, fixed = TRUE),
                grepl(if (case == "unexpected_file") "Missing: none" else "Unexpected: none",
                      message, fixed = TRUE))
    }
    checks <- checks + 1L
    cat(sprintf("PASS: %s / %s\n", branch, case))
  }
}
stopifnot(checks == 8L)
cat("PASS: Stage 4 v3 inventories (16 MC1-only and 32 ensemble files), required support policy, missing raster and unexpected-file diagnostics.\n")
cat("FIXTURE=", fixture, "\n", sep = "")
