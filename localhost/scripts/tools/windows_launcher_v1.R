# SPDX-License-Identifier: Apache-2.0
# Windows launcher generation only; sourcing this file never starts a model.

mofuss_windows_run_configuration <- function(parameters, luc = NULL) {
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
  # Both supported fixed MODIS and annual Woodman runs use v14 so their NRB
  # accounting has the same woodfuel attribution contract. Copernicus-only
  # preparations retain the legacy static v13 LUC2 route.
  if (is.null(luc)) {
    enabled <- vapply(1:3, function(channel) {
      name <- paste0("LULCt", channel, "map")
      value <- toupper(parameter(name, "NO"))
      if (!value %in% c("YES", "NO")) stop(name, " must be YES or NO.")
      value == "YES"
    }, logical(1))
    luc <- c(3L, 1L, 2L)[enabled[c(3L, 1L, 2L)]][1L]
    if (is.na(luc)) stop("Enable at least one LULCt1map/LULCt2map/LULCt3map channel.")
  } else {
    # An explicit experiment selector is authoritative even when preparation
    # produced both channels. The caller verifies the selected input files.
    if (length(luc) != 1L || !is.numeric(luc) || is.na(luc) ||
        !is.finite(luc) || !luc %in% c(1L, 3L)) {
      stop("Explicit Windows LUC override must be numeric 1 or 3.")
    }
    luc <- as.integer(luc)
  }
  scenario <- parameter("scenario_ver")
  role <- if (grepl("^bau", scenario, ignore.case = TRUE)) "BAU" else
    if (grepl("^(ics|ccts)", scenario, ignore.case = TRUE)) "ICS" else
      stop("Cannot choose the MC rerun setting for scenario_ver: ", scenario)
  freeze_text <- parameter("woodman_luc_freeze_year", "2050")
  freeze_year <- suppressWarnings(as.numeric(freeze_text))
  if (!grepl("^[0-9]+$", freeze_text) || !is.finite(freeze_year) ||
      freeze_year < 2000 || freeze_year > 2050) {
    stop("woodman_luc_freeze_year must be an integer from 2000 through 2050.")
  }
  list(model = sprintf("10_dyn_Sc17_webmofuss_ctrees_g_v%d.egoml",
                       if (luc %in% c(1L, 3L)) 14L else 13L),
       luc = luc, mc_reruns = role == "BAU", role = role, scenario = scenario,
       woodman_luc_freeze_year = as.integer(freeze_year))
}

mofuss_configure_model_luc <- function(model_path, luc) {
  if (length(luc) != 1L || is.na(luc) || !luc %in% 1:3) {
    stop("Model LUC selection must be 1, 2 or 3.")
  }
  if (!file.exists(model_path) || dir.exists(model_path)) {
    stop("Selected model is missing: ", model_path)
  }
  original <- readBin(model_path, "raw", n = file.info(model_path)$size)
  text <- rawToChar(original)
  if (grepl("_v14[.]egoml$", model_path)) {
    if (!luc %in% c(1L, 3L)) stop("v14 supports MODIS LUC1 and Woodman LUC3 only.")
    if (!grepl(paste0('key="mofuss.nrb.attribution.contract" value="',
                     'woodfuel_attributed_signed_balance_v1"'), text, fixed = TRUE)) {
      stop("Selected v14 model lacks the woodfuel NRB attribution contract. ",
           "Prepare a fresh run from the current repository model.")
    }
  }
  configured <- charToRaw(.mofuss_windows_constant(text, "Int", "v302", as.character(luc)))
  if (!identical(original, configured)) writeBin(configured, model_path)
  if (!identical(readBin(model_path, "raw", n = file.info(model_path)$size), configured)) {
    stop("Model LUC setting readback failed: ", model_path)
  }
  invisible(model_path)
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
    processors = 2L, temp_root = "E:/MoFuSS_Active/windows_runs",
    luc = NULL, paired_bau = NULL, seed = NULL, filename = "RUN_MoFuSS.cmd") {
  configuration <- mofuss_windows_run_configuration(parameters, luc = luc)
  if (length(filename) != 1L || is.na(filename) ||
      !grepl("^[A-Za-z0-9_-]+[.]cmd$", filename, ignore.case = TRUE)) {
    stop("Launcher filename must be a plain .cmd filename containing letters, numbers, '_' or '-'.")
  }
  seed_notice <- character()
  if (!is.null(seed)) {
    if (length(seed) != 1L || !is.numeric(seed) || is.na(seed) ||
        !is.finite(seed) || seed < 0 || seed > .Machine$integer.max ||
        seed != floor(seed)) {
      stop("Monte Carlo seed must be a whole number from 0 through 2147483647.")
    }
    seed <- as.integer(seed)
    seed_notice <- c(sprintf('set "MOFUSS_SEED=%d"', seed),
                     'echo R Monte Carlo seed: %MOFUSS_SEED%.')
  }
  if (length(processors) != 1L || is.na(processors) ||
      !is.numeric(processors) || !is.finite(processors) ||
      processors < 1 || processors != floor(processors) || processors > 1024) {
    stop("Windows launcher processors must be a positive whole number.")
  }
  engine_line <- .mofuss_windows_path(engine, "DinamicaConsole path")
  temp_line <- .mofuss_windows_path(temp_root, "Temporary root")
  paired_notice <- character()
  if (!is.null(paired_bau)) {
    if (configuration$role != "ICS") stop("paired_bau is valid only for an ICS/CCTS launcher.")
    paired_line <- .mofuss_windows_path(paired_bau, "Paired BAU folder")
    paired_notice <- c(paste0('set "MOFUSS_PAIRED_BAU=', paired_line, '"'),
                       'echo Matching BAU folder: "%MOFUSS_PAIRED_BAU%"')
  }
  if (!grepl("[.]exe$", engine, ignore.case = TRUE)) {
    stop("DinamicaConsole path must name an .exe file.")
  }
  destination <- normalizePath(destination, winslash = "/", mustWork = TRUE)
  if (!dir.exists(destination)) stop("Run destination must be a folder.")
  # The model reads the prepared runtime table, not the launcher's echo line.
  # Refuse a stale/nonexistent table for an explicit frozen-Woodman experiment.
  runtime_path <- file.path(destination, "LULCC/TempTables/parameters_dinamica.csv")
  if (configuration$luc == 3L && file.exists(runtime_path)) {
    runtime <- utils::read.csv(runtime_path, check.names = FALSE,
                               stringsAsFactors = FALSE, colClasses = "character")
    if (!all(c("Var", "ParCHR") %in% names(runtime))) {
      stop("Prepared Dinamica parameters must contain Var and ParCHR: ", runtime_path)
    }
    selected <- which(trimws(runtime$Var) == "woodman_luc_freeze_year")
    if (length(selected) > 1L) stop("Duplicate prepared woodman_luc_freeze_year.")
    runtime_text <- if (length(selected)) trimws(runtime$ParCHR[selected]) else "2050"
    if (is.na(runtime_text) || !identical(runtime_text,
                                        as.character(configuration$woodman_luc_freeze_year))) {
      stop("Prepared woodman_luc_freeze_year does not match parameters.csv. ",
           "Run parameter export before launcher preparation: ", runtime_path)
    }
  } else if (configuration$luc == 3L && configuration$woodman_luc_freeze_year != 2050L) {
    stop("Frozen Woodman requires a prepared parameters_dinamica.csv: ", runtime_path)
  }
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
  model_text <- .mofuss_windows_constant(
    model_text, "String", "v261", if (configuration$role == "BAU") '"BaU"' else '"ICS"')
  if (configuration$luc %in% c(1L, 3L) &&
      !grepl(paste0('key="mofuss.nrb.attribution.contract" value="',
                   'woodfuel_attributed_signed_balance_v1"'), model_text, fixed = TRUE)) {
    stop("Selected v14 model lacks the woodfuel NRB attribution contract. ",
         "Prepare a fresh run from the current repository model.")
  }
  if (configuration$luc == 3L && configuration$woodman_luc_freeze_year != 2050L &&
      !grepl('key="mofuss.woodman.freeze.contract" value="woodman_freeze_year_v1"',
             model_text, fixed = TRUE)) {
    stop("Selected v14 model does not implement the Woodman freeze-year parameter. ",
         "Install the current repository model before preparing this launcher.")
  }

  notice <- if (configuration$mc_reruns)
    "echo BAU: this run generates a new Monte Carlo batch." else
    c("echo ICS: start only after the matching BAU has generated its NEW complete MC batch.",
      "echo The model reuses BAU draws; Monte Carlo rerun is set to No.")
  freeze_notice <- if (configuration$luc == 3L) c(
    sprintf("echo Woodman LUC freeze year: %d.", configuration$woodman_luc_freeze_year),
    "echo Annual cover changes apply through that year; its cover is retained afterward."
  ) else "echo Woodman LUC freeze year is inactive for this LUC selection."
  lines <- c(
    "@echo off", "setlocal EnableExtensions DisableDelayedExpansion",
    "rem Generated by MoFuSS preprocessing. Run once after installing IDW outputs.",
    'pushd "%~dp0"', "if errorlevel 1 exit /b 1",
    paste0('set "MOFUSS_ENGINE=', engine_line, '"'),
    paste0('set "MOFUSS_MODEL=', configuration$model, '"'),
    paste0('set "MOFUSS_TEMP_ROOT=', temp_line, '"'),
    seed_notice,
    'if not exist "%MOFUSS_ENGINE%" goto engine_error',
    'if not exist "%MOFUSS_MODEL%" goto model_error',
    sprintf("echo Configured LUC: %d - %s", configuration$luc,
            switch(as.character(configuration$luc), "1" = "fixed MODIS cover",
                   "3" = "annual Woodman cover", "2" = "legacy Copernicus cover")),
    freeze_notice,
    paste0("echo Monte Carlo rerun: ", if (configuration$mc_reruns) "Yes" else "No"),
    paired_notice,
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
  path <- file.path(destination, filename)
  if (!identical(original, model_bytes)) writeBin(model_bytes, model_path)
  writeBin(launcher, path)
  if (!identical(readBin(model_path, "raw", n = file.info(model_path)$size), model_bytes) ||
      !identical(readBin(path, "raw", n = file.info(path)$size), launcher)) {
    stop("Windows launcher/model readback failed in: ", destination)
  }
  message("Created ", path, " -> ", configuration$model,
          "; LUC=", configuration$luc,
          "; Woodman freeze year=", configuration$woodman_luc_freeze_year,
          if (configuration$luc == 3L) "" else " (inactive)", "; MC rerun=",
          if (configuration$mc_reruns) "Yes" else "No",
          ". Complete IDW output installation before running.")
  invisible(c(list(path = path, model_path = model_path, paired_bau = paired_bau,
                   seed = seed), configuration))
}
