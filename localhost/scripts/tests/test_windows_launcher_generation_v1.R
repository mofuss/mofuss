# Bounded deployment and execution checks for Windows launchers. This never
# invokes Dinamica or reads/writes a production run. The executable below is a
# tiny local probe that records arguments, working directory, and temporary
# paths, then returns a requested exit code.

scripts <- normalizePath("localhost/scripts", winslash = "/", mustWork = TRUE)
source(file.path(scripts, "tools", "windows_launcher_v1.R"))

scratch <- tempfile("windows_launcher_test_", tmpdir = tempdir())
stopifnot(dir.create(scratch, recursive = TRUE))
scratch <- normalizePath(scratch, winslash = "/", mustWork = TRUE)
cat("Windows launcher test scratch: ", scratch, "\n", sep = "")

read_bytes <- function(path) {
  readBin(path, "raw", n = file.info(path)$size)
}
read_text <- function(path) rawToChar(read_bytes(path))
write_bytes <- function(bytes, path) writeBin(bytes, path)

model_names <- c(
  v13 = "10_dyn_Sc17_webmofuss_ctrees_g_v13.egoml",
  v14 = "10_dyn_Sc17_webmofuss_ctrees_g_v14.egoml"
)
source_paths <- file.path(scripts, model_names)
source_hashes <- tools::md5sum(source_paths)

parameters <- function(scenario = "BaU1_v2", luc = 1L) {
  data.frame(
    Var = c("scenario_ver", "LULCt1map", "LULCt2map", "LULCt3map"),
    ParCHR = c(scenario, if (luc == 1L) "YES" else "NO",
               if (luc == 2L) "YES" else "NO",
               if (luc == 3L) "YES" else "NO"),
    stringsAsFactors = FALSE
  )
}

# Locate the actual owning functor independently of the implementation. Byte
# equality after masking only these three constants proves all other graph bytes
# (including scientific expressions and formatting) have been retained.
constant_block <- function(text, id) {
  blocks <- gregexpr("(?s)<functor\\b[^>]*>.*?</functor>", text, perl = TRUE)
  values <- regmatches(text, blocks)[[1L]]
  selected <- which(grepl(paste0(' id="', id, '"'), values, fixed = TRUE))
  stopifnot(length(selected) == 1L)
  values[[selected]]
}
constant_value <- function(text, id) {
  block <- constant_block(text, id)
  match <- regexec('<inputport name="constant">([^<]*)</inputport>', block)
  values <- regmatches(block, match)[[1L]]
  stopifnot(length(values) == 2L)
  values[[2L]]
}
mask_constants <- function(text) {
  for (id in c("v256", "v302", "v261")) {
    block <- constant_block(text, id)
    old <- paste0('<inputport name="constant">', constant_value(text, id),
                  '</inputport>')
    changed <- sub(old, paste0('<inputport name="constant">MASK_', id,
                               '</inputport>'), block, fixed = TRUE)
    text <- sub(block, changed, text, fixed = TRUE)
  }
  text
}

engine <- file.path(scratch, "engine tools !", "Dinamica probe.exe")
dir.create(dirname(engine))
# Existing-file validation is exercised even on platforms where the execution
# probe is unavailable. The genuine Windows probe replaces these inert bytes.
write_bytes(charToRaw("bounded executable fixture"), engine)
temporary_root <- file.path(scratch, "separate temp !")

new_fixture <- function(name) {
  folder <- file.path(scratch, name)
  stopifnot(dir.create(folder))
  stopifnot(all(file.copy(source_paths, file.path(folder, model_names))))
  for (relative in c("parameters.csv", "Out/science_result.csv",
                     "Temp/mc_batch_ready.csv", "Sourcing/retained.csv")) {
    path <- file.path(folder, relative)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(paste("Preserve exactly:", relative), path, useBytes = TRUE)
  }
  folder
}
tree_hashes <- function(folder) {
  paths <- sort(list.files(folder, recursive = TRUE, full.names = TRUE,
                           all.files = TRUE, no.. = TRUE))
  tools::md5sum(paths[!dir.exists(paths)])
}

fixtures <- list()
for (luc in c(1L, 3L)) {
  for (scenario in c("BaU1_v2", "ICS3_v2")) {
    folder <- new_fixture(paste("country with spaces !", luc, scenario))
    before <- tree_hashes(folder)
    input <- parameters(scenario, luc)
    configured <- mofuss_windows_run_configuration(input)
    selected <- unname(model_names[["v14"]])
    rerun <- startsWith(scenario, "BaU")
    stopifnot(identical(configured$model, selected),
              as.integer(configured$luc) == luc,
              identical(configured$mc_reruns, rerun))
    result <- mofuss_write_windows_launcher(
      folder, input, engine = engine, processors = 2L,
      temp_root = temporary_root
    )
    launcher <- file.path(folder, "RUN_MoFuSS.cmd")
    stopifnot(file.exists(launcher), identical(result$model, selected),
              as.integer(result$luc) == luc,
              identical(result$mc_reruns, rerun))
    installed <- read_text(file.path(folder, selected))
    original <- read_text(file.path(scripts, selected))
    stopifnot(identical(constant_value(installed, "v256"),
                        if (rerun) ".yes" else ".no"),
              identical(constant_value(installed, "v302"), as.character(luc)),
              identical(constant_value(installed, "v261"), if (rerun) '"BaU"' else '"ICS"'),
              identical(mask_constants(installed), mask_constants(original)))
    preserved <- names(before)[names(before) != file.path(folder, selected)]
    stopifnot(identical(before[preserved], tools::md5sum(preserved)))
    command <- read_text(launcher)
    stopifnot(grepl(selected, command, fixed = TRUE),
              grepl(paste0("echo Configured LUC: ", luc), command, fixed = TRUE),
              grepl(paste0("echo Monte Carlo rerun: ", if (rerun) "Yes" else "No"), command, fixed = TRUE),
              grepl("-processors 2", command, fixed = TRUE),
              grepl("-log-level 4", command, fixed = TRUE),
              !grepl("-disable-native-expressions", command, fixed = TRUE),
              !grepl("-seed", command, fixed = TRUE))
    # Same inputs must produce exactly the same installed graph and launcher.
    first <- tree_hashes(folder)
    mofuss_write_windows_launcher(folder, input, engine = engine,
                                  processors = 2L, temp_root = temporary_root)
    stopifnot(identical(first, tree_hashes(folder)))
    fixtures[[paste(luc, scenario, sep = "_")]] <- folder
  }
}

# LUC2 works through v13. When multiple channels are enabled, precedence is
# Woodman (3), MODIS (1), then Copernicus (2).
copernicus <- parameters("ccts3_v2", 2L)
choice <- mofuss_windows_run_configuration(copernicus)
stopifnot(identical(choice$model, unname(model_names[["v13"]])),
          as.integer(choice$luc) == 2L, identical(choice$mc_reruns, FALSE))
both <- parameters()
both$ParCHR[both$Var == "LULCt3map"] <- "YES"
stopifnot(as.integer(mofuss_windows_run_configuration(both)$luc) == 3L)
both$ParCHR[both$Var == "LULCt3map"] <- "NO"
both$ParCHR[both$Var == "LULCt2map"] <- "YES"
stopifnot(as.integer(mofuss_windows_run_configuration(both)$luc) == 1L)

# Explicit experiment overrides take precedence without changing preparation
# parameters, including a folder where both LUC channels have been prepared.
both$ParCHR[both$Var == "LULCt3map"] <- "YES"
both_before <- both
for (luc in c(1L, 3L)) {
  choice <- mofuss_windows_run_configuration(both, luc = luc)
  stopifnot(identical(choice$luc, luc),
            identical(choice$model, unname(model_names[["v14"]])),
            identical(both, both_before))
}
override_folder <- new_fixture("explicit fixed cover ICS !")
override_input <- both
override_input$ParCHR[override_input$Var == "scenario_ver"] <- "ICS3_v2"
override_before <- override_input
override_hashes <- tree_hashes(override_folder)
paired_bau <- file.path(scratch, "matching BAU with spaces & parentheses (1) !")
override_result <- mofuss_write_windows_launcher(
  override_folder, override_input, engine = engine, processors = 2L,
  temp_root = temporary_root, luc = 1L, paired_bau = paired_bau)
override_model <- file.path(override_folder, model_names[["v14"]])
override_launcher <- file.path(override_folder, "RUN_MoFuSS.cmd")
stopifnot(identical(override_input, override_before),
          identical(override_result$luc, 1L), identical(override_result$mc_reruns, FALSE),
          identical(override_result$paired_bau, paired_bau),
          identical(constant_value(read_text(override_model), "v302"), "1"),
          identical(constant_value(read_text(override_model), "v256"), ".no"),
          identical(constant_value(read_text(override_model), "v261"), '"ICS"'),
          grepl("echo Configured LUC: 1 - fixed MODIS cover", read_text(override_launcher), fixed = TRUE),
          grepl("echo Monte Carlo rerun: No", read_text(override_launcher), fixed = TRUE),
          grepl(.mofuss_windows_path(paired_bau, "fixture"), read_text(override_launcher), fixed = TRUE))
preserved <- names(override_hashes)[names(override_hashes) != override_model]
stopifnot(identical(override_hashes[preserved], tools::md5sum(preserved)),
          identical(mask_constants(read_text(override_model)),
                    mask_constants(read_text(file.path(scripts, model_names[["v14"]])))))

regenerated <- fixtures[["3_ICS3_v2"]]
sentinels <- file.path(regenerated, c("parameters.csv", "Out/science_result.csv",
                                     "Temp/mc_batch_ready.csv",
                                     "Sourcing/retained.csv"))
sentinel_hashes <- tools::md5sum(sentinels)
mofuss_write_windows_launcher(regenerated, copernicus, engine = engine,
                              processors = 2L, temp_root = temporary_root)
stopifnot(identical(constant_value(read_text(file.path(regenerated,
                           model_names[["v13"]])), "v302"), "2"),
          grepl(model_names[["v13"]],
                read_text(file.path(regenerated, "RUN_MoFuSS.cmd")), fixed = TRUE),
          !grepl(model_names[["v14"]],
                 read_text(file.path(regenerated, "RUN_MoFuSS.cmd")), fixed = TRUE),
          identical(sentinel_hashes, tools::md5sum(sentinels)))

expect_no_mutation <- function(input, ...) {
  before <- tree_hashes(regenerated)
  failed <- try(mofuss_write_windows_launcher(
    regenerated, input, engine = engine, temp_root = temporary_root, ...
  ), silent = TRUE)
  stopifnot(inherits(failed, "try-error"),
            identical(before, tree_hashes(regenerated)))
}
expect_no_mutation(parameters("unsupported_v2"))
duplicate <- rbind(parameters(), parameters()[1L, , drop = FALSE])
expect_no_mutation(duplicate)
missing <- parameters()[-1L, , drop = FALSE]
expect_no_mutation(missing)
none <- parameters()
none$ParCHR[grepl("^LULCt", none$Var)] <- "NO"
expect_no_mutation(none)
expect_no_mutation(parameters(), processors = 0L)
for (invalid_luc in list(0L, 2L, 4L, 1.5, "1", NA_real_, c(1L, 3L))) {
  expect_no_mutation(parameters(), luc = invalid_luc)
}
expect_no_mutation(parameters(), luc = 1L, paired_bau = paired_bau)
expect_no_mutation(parameters("ICS3_v2"), luc = 1L, paired_bau = "relative/BAU")
expect_no_mutation(parameters("ICS3_v2"), luc = 1L, paired_bau = "E:/bad\nBAU")

# The freeze control applies to Woodman only, preserves old tables through the
# default, and must agree with the CSV actually consumed by the model.
stopifnot(identical(mofuss_windows_run_configuration(parameters())$woodman_luc_freeze_year,
                    2050L))
with_freeze <- function(year, luc = 3L) {
  rbind(parameters("BaU1_v2", luc),
        data.frame(Var = "woodman_luc_freeze_year", ParCHR = as.character(year)))
}
for (invalid_year in c("1999", "2051", "2026.5", "", "NA", "2026bad")) {
  expect_no_mutation(with_freeze(invalid_year))
}
expect_no_mutation(rbind(with_freeze(2026), tail(with_freeze(2026), 1L)))
expect_no_mutation(with_freeze(2026)) # explicit freeze cannot lack runtime input
for (invalid_seed in list(-1, 1.5, NA_real_, Inf, "123", c(1L, 2L), 2147483648)) {
  expect_no_mutation(parameters(), seed = invalid_seed)
}
for (invalid_filename in c("../bad.cmd", "bad.exe", "folder/bad.cmd", "bad&name.cmd")) {
  expect_no_mutation(parameters(), filename = invalid_filename)
}
freeze_folder <- new_fixture("Woodman frozen 2026 !")
freeze_runtime <- file.path(freeze_folder, "LULCC/TempTables/parameters_dinamica.csv")
dir.create(dirname(freeze_runtime), recursive = TRUE)
write.csv(data.frame(Var = "woodman_luc_freeze_year", ParCHR = 2026L),
          freeze_runtime, row.names = FALSE)
freeze_result <- mofuss_write_windows_launcher(
  freeze_folder, with_freeze(2026), engine = engine, processors = 2L,
  temp_root = temporary_root, seed = 20261009L, filename = "RUN_MDG_optimized.cmd")
freeze_command <- read_text(freeze_result$path)
stopifnot(identical(freeze_result$woodman_luc_freeze_year, 2026L),
          identical(freeze_result$seed, 20261009L),
          identical(basename(freeze_result$path), "RUN_MDG_optimized.cmd"),
          !file.exists(file.path(freeze_folder, "RUN_MoFuSS.cmd")),
          grepl("Woodman LUC freeze year: 2026", freeze_command, fixed = TRUE),
          grepl('set "MOFUSS_SEED=20261009"', freeze_command, fixed = TRUE),
          identical(mask_constants(read_text(freeze_result$model_path)),
                    mask_constants(read_text(file.path(scripts, model_names[["v14"]])))))
before_mismatch <- tree_hashes(freeze_folder)
mismatch <- try(mofuss_write_windows_launcher(
  freeze_folder, with_freeze(2050), engine = engine, temp_root = temporary_root), silent = TRUE)
stopifnot(inherits(mismatch, "try-error"), identical(before_mismatch, tree_hashes(freeze_folder)))
# The same nondefault parameter is explicitly inactive when selecting MODIS.
inactive <- mofuss_write_windows_launcher(
  freeze_folder, with_freeze(2026, 1L), engine = engine, temp_root = temporary_root)
stopifnot(grepl("freeze year is inactive", read_text(inactive$path), fixed = TRUE))
cat("WINDOWS_LAUNCHER_GENERATION_OK\n")

# Verify the actual workflow wiring without executing any preprocessing step.
# The two entry points must install the launcher as their last script, after
# directional IDW input preparation.
for (entry_point in c("000_main_localhost_v1.R", "0_main.R")) {
  expressions <- parse(file.path(scripts, entry_point))
  assignments <- Filter(function(expression) {
    is.call(expression) && identical(expression[[1L]], as.name("<-")) &&
      identical(expression[[2L]], as.name("scripts"))
  }, as.list(expressions))
  stopifnot(length(assignments) == 1L)
  steps <- as.list(assignments[[1L]][[3L]])[-1L]
  stopifnot(identical(tail(steps, 2L), list(
    "8_prepare_directional_IDW_inputs_v3.R", "10_prepare_windows_launcher_v1.R"
  )))
}

if (.Platform$OS.type == "windows") {
  test_workflow_step <- function() {
    variable_names <- c("MOFUSS_DINAMICA_CONSOLE", "MOFUSS_WINDOWS_TEMP_ROOT")
    previous <- Sys.getenv(variable_names, unset = NA_character_)
    on.exit({
      Sys.unsetenv(variable_names)
      present <- !is.na(previous)
      if (any(present)) do.call(Sys.setenv, as.list(previous[present]))
    }, add = TRUE)
    Sys.setenv(MOFUSS_DINAMICA_CONSOLE = engine,
               MOFUSS_WINDOWS_TEMP_ROOT = temporary_root)
    folder <- new_fixture("workflow step with spaces !")
    before <- tree_hashes(folder)
    context <- new.env(parent = baseenv())
    context$countrydir <- folder
    context$country_parameters <- parameters("ICS3_v2", 3L)
    context$runtime_scripts_dir <- scripts
    # The wrapper itself loads its helper into a separate base-only module.
    # Nothing from this test's helper environment may mask a missing dependency.
    sys.source(file.path(scripts, "10_prepare_windows_launcher_v1.R"),
               envir = context)
    selected <- file.path(folder, model_names[["v14"]])
    installed <- read_text(selected)
    stopifnot(identical(constant_value(installed, "v256"), ".no"),
              identical(constant_value(installed, "v302"), "3"),
              identical(constant_value(installed, "v261"), '"ICS"'),
              identical(mask_constants(installed),
                        mask_constants(read_text(file.path(scripts, model_names[["v14"]])))),
              file.exists(file.path(folder, "RUN_MoFuSS.cmd")))
    preserved <- names(before)[names(before) != selected]
    stopifnot(identical(before[preserved], tools::md5sum(preserved)),
              identical(context$windows_dinamica_console, engine),
              identical(context$windows_dinamica_temp_root, temporary_root))
  }
  test_workflow_step()
}
cat("WINDOWS_LAUNCHER_WORKFLOW_WIRING_OK\n")

if (.Platform$OS.type == "windows") {
  # .NET Framework is part of supported Windows installations. Compile a tiny
  # console probe locally; do not use the production engine during this test.
  compiler <- file.path(Sys.getenv("WINDIR"), "Microsoft.NET", "Framework64",
                         "v4.0.30319", "csc.exe")
  if (!file.exists(compiler)) {
    compiler <- file.path(Sys.getenv("WINDIR"), "Microsoft.NET", "Framework",
                           "v4.0.30319", "csc.exe")
  }
  stopifnot(file.exists(compiler))
  probe_source <- file.path(scratch, "launcher_probe.cs")
  writeLines(c(
    "using System;",
    "using System.IO;",
    "public class LauncherProbe {",
    "  public static int Main(string[] args) {",
    "    string[] record = new string[5 + args.Length];",
    "    record[0] = Environment.CurrentDirectory;",
    "    record[1] = Environment.GetEnvironmentVariable(\"TEMP\") ?? \"\";",
    "    record[2] = Environment.GetEnvironmentVariable(\"TMP\") ?? \"\";",
    "    record[3] = Environment.GetEnvironmentVariable(\"TMPDIR\") ?? \"\";",
    "    record[4] = args.Length.ToString();",
    "    Array.Copy(args, 0, record, 5, args.Length);",
    "    File.WriteAllLines(\"launcher_probe.txt\", record);",
    "    int code;",
    "    return Int32.TryParse(Environment.GetEnvironmentVariable(\"MOFUSS_LAUNCHER_TEST_EXIT\"), out code) ? code : 0;",
    "  }",
    "}"
  ), probe_source, useBytes = TRUE)
  windows_engine <- normalizePath(engine, winslash = "\\", mustWork = TRUE)
  windows_source <- normalizePath(probe_source, winslash = "\\", mustWork = TRUE)
  unlink(engine)
  compilation <- suppressWarnings(system2(compiler,
    c("/nologo", "/target:exe", paste0("/out:", shQuote(windows_engine)),
       shQuote(windows_source)), stdout = TRUE, stderr = TRUE))
  if (!file.exists(engine)) stop(paste(compilation, collapse = "\n"))

  folder <- fixtures[["1_BaU1_v2"]]
  launcher <- file.path(folder, "RUN_MoFuSS.cmd")
  input_path <- file.path(scratch, "pause_input.txt")
  writeLines(rep("", 10L), input_path, useBytes = TRUE)
  previous_exit <- Sys.getenv("MOFUSS_LAUNCHER_TEST_EXIT", unset = NA_character_)
  execute_probe <- function(code) {
    Sys.setenv(MOFUSS_LAUNCHER_TEST_EXIT = as.character(code))
    log <- file.path(scratch, paste0("launch_", code, ".log"))
    status <- suppressWarnings(system2(Sys.getenv("COMSPEC"),
      c("/d", "/c", shQuote(normalizePath(launcher, winslash = "\\"))),
      stdin = input_path, stdout = log, stderr = log))
    if (is.null(status)) status <- 0L
    list(status = as.integer(status), log = log)
  }
  first <- execute_probe(0L)
  if (first$status != 0L) stop(paste(readLines(first$log), collapse = "\n"))
  record_path <- file.path(folder, "launcher_probe.txt")
  record <- readLines(record_path, warn = FALSE)
  normalize <- function(path) normalizePath(path, winslash = "/", mustWork = TRUE)
  stopifnot(length(record) == 10L,
            identical(normalize(record[[1L]]), normalize(folder)),
            dir.exists(record[[2L]]), identical(record[[2L]], record[[3L]]),
            identical(record[[2L]], record[[4L]]),
            startsWith(normalize(record[[2L]]), paste0(normalize(temporary_root), "/")),
            identical(record[5:10], c("5", "-processors", "2", "-log-level", "4",
                                        unname(model_names[["v14"]]))))
  first_temp <- record[[2L]]
  second <- execute_probe(23L)
  stopifnot(second$status == 23L)
  record <- readLines(record_path, warn = FALSE)
  stopifnot(!identical(first_temp, record[[2L]]))

  # Runtime missing-engine failure must remain visible to a double-click user,
  # propagate a nonzero exit, and never start the probe.
  stopifnot(file.rename(engine, paste0(engine, ".saved")))
  unlink(record_path)
  missing_engine <- execute_probe(0L)
  stopifnot(missing_engine$status != 0L, !file.exists(record_path))
  stopifnot(file.rename(paste0(engine, ".saved"), engine))
  # Execute the explicit fixed-cover ICS launcher as well. Its quoted pairing
  # notice must survive spaces, ampersands, parentheses and exclamation marks.
  launcher <- override_launcher
  paired_probe <- execute_probe(0L)
  paired_record <- readLines(file.path(override_folder, "launcher_probe.txt"), warn = FALSE)
  paired_log <- paste(readLines(paired_probe$log, warn = FALSE), collapse = "\n")
  stopifnot(paired_probe$status == 0L,
            identical(normalize(paired_record[[1L]]), normalize(override_folder)),
            identical(paired_record[[10L]], unname(model_names[["v14"]])),
            grepl("Configured LUC: 1 - fixed MODIS cover", paired_log, fixed = TRUE),
            grepl("Monte Carlo rerun: No", paired_log, fixed = TRUE),
            grepl(chartr("/", "\\", paired_bau), paired_log, fixed = TRUE))
  if (is.na(previous_exit)) Sys.unsetenv("MOFUSS_LAUNCHER_TEST_EXIT") else
    Sys.setenv(MOFUSS_LAUNCHER_TEST_EXIT = previous_exit)
  cat("WINDOWS_LAUNCHER_EXECUTION_OK\n")
} else {
  cat("Windows execution probe skipped on this operating system.\n")
}

stopifnot(identical(source_hashes, tools::md5sum(source_paths)))
cat("WINDOWS_LAUNCHER_TESTS_OK\n")
