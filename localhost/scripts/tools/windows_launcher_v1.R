# SPDX-License-Identifier: Apache-2.0
# Windows launcher generation only; sourcing this file never starts a model.

mofuss_windows_run_configuration <- function(parameters) {
  if (!is.data.frame(parameters) ||
      !all(c("Var", "ParCHR") %in% names(parameters))) {
    stop("Launcher preparation requires a parameters table with Var and ParCHR.")
  }
  parameter <- function(name, default = NULL) {
    selected <- which(trimws(as.character(parameters$Var)) == name)
    if (!length(selected) && !is.null(default)) return(default)
    if (length(selected) != 1L) stop("Expected one launcher parameter: ", name)
    value <- trimws(as.character(parameters$ParCHR[selected]))
    if (is.na(value) || !nzchar(value)) stop("Empty launcher parameter: ", name)
    value
  }
  enabled <- vapply(1:3, function(channel) {
    name <- paste0("LULCt", channel, "map")
    value <- toupper(parameter(name, "NO"))
    if (!value %in% c("YES", "NO")) stop(name, " must be YES or NO.")
    value == "YES"
  }, logical(1))
  # Match the current Woodman default when both channels are prepared.
  # Copernicus-only preparations use the existing static v13 LUC2 route.
  luc <- c(3L, 1L, 2L)[enabled[c(3L, 1L, 2L)]][1L]
  if (is.na(luc)) stop("Enable at least one LULCt1map/LULCt2map/LULCt3map channel.")
  scenario <- parameter("scenario_ver")
  role <- if (grepl("^bau", scenario, ignore.case = TRUE)) "BAU" else
    if (grepl("^(ics|ccts)", scenario, ignore.case = TRUE)) "ICS" else
      stop("Cannot choose the MC rerun setting for scenario_ver: ", scenario)
  list(model = sprintf("10_dyn_Sc17_webmofuss_ctrees_g_v%d.egoml",
                       if (luc == 3L) 14L else 13L),
       luc = luc, mc_reruns = role == "BAU", role = role, scenario = scenario)
}

.mofuss_windows_constant <- function(text, type, id, value) {
  # Change one literal port in the copied graph, preserving every other byte.
  # Refuse unfamiliar/ambiguous structures instead of replacing by position.
  pattern <- paste0('(?s)<functor name="', type, '">(?:(?!</functor>).)*',
                    '<outputport name="object" id="', id, '"\\s*/>',
                    '(?:(?!</functor>).)*</functor>')
  matches <- gregexpr(pattern, text, perl = TRUE)
  nodes <- regmatches(text, matches)[[1L]]
  if (length(nodes) != 1L) stop("Expected one ", type, " model setting: ", id)
  port <- '<inputport name="constant">[^<]*</inputport>'
  ports <- gregexpr(port, nodes, perl = TRUE)
  if (length(regmatches(nodes, ports)[[1L]]) != 1L) {
    stop("Expected one literal constant in model setting: ", id)
  }
  replacement <- sub(port, paste0('<inputport name="constant">', value,
                                  '</inputport>'), nodes, perl = TRUE)
  regmatches(text, matches) <- list(replacement)
  text
}

.mofuss_windows_path <- function(path, label) {
  if (length(path) != 1L || is.na(path) || !nzchar(path) ||
      grepl('["\r\n]', path) || !grepl("^[A-Za-z]:[/\\\\]", path)) {
    stop(label, " must be an absolute Windows drive path without quotes/newlines.")
  }
  # Delayed expansion is disabled; percent signs embedded in batch source
  # need doubling. Ampersands and parentheses are protected by quoted SET.
  gsub("%", "%%", chartr("/", "\\", path), fixed = TRUE)
}

mofuss_write_windows_launcher <- function(
    destination, parameters,
    engine = "C:/Program Files/Dinamica EGO/DinamicaConsole.exe",
    processors = 2L, temp_root = "E:/MoFuSS_Active/windows_runs") {
  configuration <- mofuss_windows_run_configuration(parameters)
  if (length(processors) != 1L || is.na(processors) ||
      !is.numeric(processors) || !is.finite(processors) ||
      processors < 1 || processors != floor(processors) || processors > 1024) {
    stop("Windows launcher processors must be a positive whole number.")
  }
  engine_line <- .mofuss_windows_path(engine, "DinamicaConsole path")
  temp_line <- .mofuss_windows_path(temp_root, "Temporary root")
  if (!grepl("[.]exe$", engine, ignore.case = TRUE)) {
    stop("DinamicaConsole path must name an .exe file.")
  }
  destination <- normalizePath(destination, winslash = "/", mustWork = TRUE)
  if (!dir.exists(destination)) stop("Run destination must be a folder.")
  model_path <- file.path(destination, configuration$model)
  if (!file.exists(model_path) || dir.exists(model_path)) {
    stop("Selected model is missing; complete step 2 first: ", model_path)
  }
  original <- readBin(model_path, "raw", n = file.info(model_path)$size)
  model_text <- rawToChar(original)
  model_text <- .mofuss_windows_constant(
    model_text, "Bool", "v256", if (configuration$mc_reruns) ".yes" else ".no")
  model_text <- .mofuss_windows_constant(model_text, "Int", "v302",
                                        as.character(configuration$luc))

  notice <- if (configuration$mc_reruns)
    "echo BAU: this run generates a new Monte Carlo batch." else
    c("echo ICS: start only after the matching BAU has generated its NEW complete MC batch.",
      "echo The model reuses BAU draws; Monte Carlo rerun is set to No.")
  lines <- c(
    "@echo off", "setlocal EnableExtensions DisableDelayedExpansion",
    "rem Generated by MoFuSS preprocessing. Run once after installing IDW outputs.",
    'pushd "%~dp0"', "if errorlevel 1 exit /b 1",
    paste0('set "MOFUSS_ENGINE=', engine_line, '"'),
    paste0('set "MOFUSS_MODEL=', configuration$model, '"'),
    paste0('set "MOFUSS_TEMP_ROOT=', temp_line, '"'),
    'if not exist "%MOFUSS_ENGINE%" goto engine_error',
    'if not exist "%MOFUSS_MODEL%" goto model_error',
    notice,
    sprintf("echo Dinamica processors: %d. Use at most four simultaneous runs on this machine.",
            as.integer(processors)),
    "echo Launch this folder once; preparation may take several minutes.",
    ":choose_temp",
    'set "TEMP=%MOFUSS_TEMP_ROOT%\\run_%RANDOM%_%RANDOM%"',
    'if exist "%TEMP%" goto choose_temp',
    'mkdir "%TEMP%"', "if errorlevel 1 goto temp_error",
    'set "TMP=%TEMP%"', 'set "TMPDIR=%TEMP%"',
    sprintf('"%%MOFUSS_ENGINE%%" -processors %d -log-level 4 "%%MOFUSS_MODEL%%"',
            as.integer(processors)),
    'set "MOFUSS_EXIT=%ERRORLEVEL%"', "echo.",
    "echo Dinamica finished with exit code %MOFUSS_EXIT%.",
    "goto finish",
    ":engine_error", "echo DinamicaConsole was not found:",
    'echo "%MOFUSS_ENGINE%"', 'set "MOFUSS_EXIT=1"', "goto finish",
    ":model_error", "echo The selected model was not found:",
    'echo "%MOFUSS_MODEL%"', 'set "MOFUSS_EXIT=1"', "goto finish",
    ":temp_error", "echo Could not create temporary storage:",
    'echo "%TEMP%"', 'set "MOFUSS_EXIT=1"',
    ":finish", "popd", "pause", "endlocal & exit /b %MOFUSS_EXIT%"
  )
  # Write explicit CRLF once, regardless of the R connection's platform.
  launcher <- charToRaw(paste0(paste(lines, collapse = "\r\n"), "\r\n"))
  model_bytes <- charToRaw(model_text)
  path <- file.path(destination, "RUN_MoFuSS.cmd")
  if (!identical(original, model_bytes)) writeBin(model_bytes, model_path)
  writeBin(launcher, path)
  if (!identical(readBin(model_path, "raw", n = file.info(model_path)$size), model_bytes) ||
      !identical(readBin(path, "raw", n = file.info(path)$size), launcher)) {
    stop("Windows launcher/model readback failed in: ", destination)
  }
  message("Created ", path, " -> ", configuration$model,
          "; LUC=", configuration$luc, "; MC rerun=",
          if (configuration$mc_reruns) "Yes" else "No",
          ". Complete IDW output installation before running.")
  invisible(c(list(path = path, model_path = model_path), configuration))
}
