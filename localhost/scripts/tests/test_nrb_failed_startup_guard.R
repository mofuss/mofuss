# Read-only postprocessing guards for failed R startup followed by stale inputs.
# Fixtures remain outside the source repository and never launch a simulation.
repo <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
scratch <- Sys.getenv("MOFUSS_TEST_SCRATCH",
  "E:/MoFuSS_Active/MDG_emissions_provenance_2026-10-09/postprocessing_guard")
stopifnot(grepl("^[A-Za-z]:[/\\\\]MoFuSS_Active[/\\\\].+", scratch))
dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
fixture <- tempfile("failed_startup_", tmpdir = scratch)
dir.create(fixture)
fixture <- normalizePath(fixture, winslash = "/", mustWork = TRUE)
source(file.path(repo, "localhost/scripts/helpers/woodfuel_nrb_attribution.R"))
timestamp <- as.POSIXct("2026-10-09 12:00:00", tz = "UTC")
new_case <- function(name) {
  path <- file.path(fixture, name)
  dir.create(path)
  path
}
write_log <- function(root, script, lines, offset = 0) {
  path <- file.path(root, paste0(script, "_v8.Rout"))
  writeLines(lines, path)
  stopifnot(Sys.setFileTime(path, timestamp + offset))
  invisible(path)
}
fails <- function(expr, patterns) {
  failure <- tryCatch(force(expr), error = identity)
  stopifnot(inherits(failure, "error"))
  message <- conditionMessage(failure)
  for (pattern in patterns) stopifnot(grepl(pattern, message, fixed = TRUE))
  invisible(message)
}

# Older installations and synthetic scientific fixtures may have no R logs.
legacy <- new_case("legacy_no_logs")
stopifnot(identical(.mofuss_nrb_validate_startup(legacy), character()))

# Matching annual LUC=3/freeze2050 metadata does not make stale MC inputs valid.
# Check the actual public NRB entry point, including debugging_N path handling.
stale <- new_case("stale_dynamic_ics")
dir.create(file.path(stale, "Temp"))
dir.create(file.path(stale, "debugging_1"))
write.csv(data.frame(status = "ready", lulc_version = 3L,
                     woodman_luc_freeze_year = 2050L),
          file.path(stale, "Temp/mc_batch_ready.csv"), row.names = FALSE)
writeLines('{"luc":3}', file.path(stale, "windows_performance_preparation.json"))
write.csv(data.frame(Key = 1:4, Value = c(3, 2050, 1, 2000)),
          file.path(stale, "debugging_1/woodman_luc_execution.csv"), row.names = FALSE)
bypass_error <- "ERROR: Multiple bau_mc_source.txt files found; retain only one."
write_log(stale, "bypassMC", c(
  "> tryCatch(main(), error = function(e) {",
  '+   message("ERROR: ", conditionMessage(e))', "+ })", bypass_error))
fails(mofuss_nrb_context(stale, expected_steps = 1L),
      c("Failed latest Monte Carlo startup", stale, bypass_error, "bypassMC_v8.Rout"))
fails(mofuss_nrb_context(file.path(stale, "debugging_1"), expected_steps = 1L),
      c("Failed latest Monte Carlo startup", stale, bypass_error))

# Prompted source, quoted strings and warnings must not be mistaken for errors.
success <- new_case("success_after_older_failure")
write_log(success, "rnorm", c("Error in old_attempt(): failed", "Execution halted"), -60)
latest <- write_log(success, "bypassMC", c(
  "> tryCatch(main(), error = function(e) {",
  '+   message("ERROR: ", conditionMessage(e))',
  '+   stop("Error in example")', "+ })",
  '[1] "Error text in a quoted diagnostic"',
  "Warning: an informational warning",
  "[OK] BAU Monte Carlo tables installed in CCTS Temp with verified hashes."))
stopifnot(identical(.mofuss_nrb_validate_startup(success), latest))

# R's unhandled errors and its terminal failure footer must also be rejected.
for (line in c("Error in read.csv(path) : missing input", "Error: invalid parameter",
               "Error en read.csv(path): no existe", "Execution halted")) {
  native_error <- new_case(paste0("r_error_", length(list.files(fixture))))
  write_log(native_error, "rnorm", c("> publish_current_mc_batch()", line))
  fails(.mofuss_nrb_validate_startup(native_error), c(native_error, line, "rnorm_v8.Rout"))
}

# A timestamp tie does not permit one branch's success to hide the other's error.
tied <- new_case("timestamp_tie")
write_log(tied, "rnorm", "[OK] Current MC batch ready")
write_log(tied, "bypassMC", bypass_error)
fails(.mofuss_nrb_validate_startup(tied), bypass_error)

# A corrected rerun replaces the failed .Rout; no failure state is cached.
write_log(tied, "bypassMC", "[OK] BAU Monte Carlo tables installed", 60)
stopifnot(length(.mofuss_nrb_validate_startup(tied)) == 1L)
cat("PASS: failed startup rejection, matching stale dynamic metadata, root/debugging entry points, source-echo exclusions, legacy logs, latest-log selection and rerun recovery.\n")
cat("FIXTURE=", fixture, "\n", sep = "")
