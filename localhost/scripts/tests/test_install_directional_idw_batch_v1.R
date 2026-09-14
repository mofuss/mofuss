# Synthetic regression tests for the directional IDW batch entry point.

suppressPackageStartupMessages(library(terra))

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
stopifnot(length(script_argument) == 1L)
test_script <- sub("^--file=", "", script_argument)
repository_root <- normalizePath(
  file.path(dirname(test_script), "..", "..", ".."),
  winslash = "/",
  mustWork = TRUE
)
installer_path <- file.path(
  repository_root,
  "localhost",
  "scripts",
  "9_install_directional_IDW_outputs_v4.R"
)

previous_no_autorun <- Sys.getenv("MOFUSS_6F_NO_AUTORUN", unset = NA_character_)
Sys.setenv(MOFUSS_6F_NO_AUTORUN = "1")
on.exit({
  if (is.na(previous_no_autorun)) {
    Sys.unsetenv("MOFUSS_6F_NO_AUTORUN")
  } else {
    Sys.setenv(MOFUSS_6F_NO_AUTORUN = previous_no_autorun)
  }
}, add = TRUE)
source(installer_path, local = .GlobalEnv)

fixture <- tempfile("directional_idw_batch_")
dir.create(fixture, recursive = TRUE)
on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

folder_names <- paste0("scenario_", seq_len(4L))
for (folder in folder_names) {
  dir.create(
    file.path(fixture, folder, "In", "DemandScenarios"),
    recursive = TRUE
  )
}

PIPELINE_BATCHES <- list(
  synthetic = list(
    enabled = TRUE,
    root = fixture,
    analysis_folder = "synthetic_analysis",
    folders = folder_names
  ),
  placeholder = list(
    enabled = FALSE,
    root = file.path(fixture, "does_not_exist"),
    analysis_folder = "future_analysis",
    folders = paste0("future_", seq_len(4L))
  )
)
PIPELINE_DRY_RUN <- FALSE
PIPELINE_OUTPUT_PREFIX <- "idw_"

resolved <- .idw6f_pipeline_resolve_batches()
stopifnot(
  identical(names(resolved$enabled), "synthetic"),
  identical(resolved$disabled, "placeholder"),
  length(resolved$enabled$synthetic$run_roots) == 4L,
  all(dir.exists(resolved$enabled$synthetic$run_roots))
)

plan_output <- capture.output(plan <- .idw6f_pipeline_main("--check"))
stopifnot(
  identical(names(plan$batches), "synthetic"),
  !plan$dry_run,
  any(grepl("CHECK COMPLETE", plan_output, fixed = TRUE))
)

calls <- list()
install_directional_idw_outputs <- function(
    run_root,
    output_prefix = "idw_",
    dry_run = FALSE) {
  calls[[length(calls) + 1L]] <<- list(
    run_root = run_root,
    output_prefix = output_prefix,
    dry_run = dry_run
  )
  list(run_root = run_root)
}
dry_output <- capture.output(dry_result <- .idw6f_pipeline_main("--dry-run"))
stopifnot(
  length(calls) == 4L,
  identical(
    vapply(calls, `[[`, character(1), "run_root"),
    resolved$enabled$synthetic$run_roots
  ),
  all(vapply(calls, `[[`, logical(1), "dry_run")),
  all(vapply(calls, `[[`, character(1), "output_prefix") == "idw_"),
  isTRUE(dry_result$config$dry_run),
  any(grepl("DRY RUN COMPLETE: 1 batch(es), 4 run(s)", dry_output, fixed = TRUE))
)

duplicate_batches <- list(
  first = PIPELINE_BATCHES$synthetic,
  second = PIPELINE_BATCHES$synthetic
)
duplicate_error <- tryCatch(
  {
    .idw6f_pipeline_resolve_batches(duplicate_batches)
    NULL
  },
  error = identity
)
stopifnot(
  inherits(duplicate_error, "error"),
  grepl("may not reuse", conditionMessage(duplicate_error), fixed = TRUE)
)

cat("Directional IDW batch tests passed.\n")
