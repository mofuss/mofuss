# SPDX-License-Identifier: Apache-2.0
# Run 000_main_localhost_v1.R sequentially for prepared working folders.
# Edit your computer's paths below, then source this file in RStudio.
# A working folder can also be used instead of its parent location.
# The main workflow clears generated outputs in each selected folder.

# BEGIN USER INPUTS ----------------------------------------------------------
# Paths are selected automatically for the computer running this script.
# Demand CSV paths are derived from repo below.
batch_computers <- list(
  Windows = list(          # WindowsNRBV1
    root = "D:/",
    repo = "C:/Users/aghil/Documents/mofuss", #"C:/Users/UNAM/Documents/mofuss" "C:/Users/aghil/Documents/mofuss"
    admin_regions = "D:/admin_regions",
    log_root = "D:/MoFuSS_Active"  #"E:/MoFuSS_Active"
  ),
  Linux = list(            # LinuxC3
    root = "/home/mofuss/Documents",
    repo = "/home/mofuss/Documents/mofuss",
    admin_regions = "/home/mofuss/Documents/admin_regions",
    log_root = "/home/mofuss/MoFuSS_Active"
  )
)
batch_apply <- TRUE      # FALSE previews; TRUE runs them sequentially.
batch_telegram_msgs <- TRUE # One result per folder using the repository .env.
# END USER INPUTS ------------------------------------------------------------

batch_paths <- batch_computers[[Sys.info()[["sysname"]]]]
if (is.null(batch_paths)) {
  stop("No batch paths configured for this operating system: ",
       Sys.info()[["sysname"]])
}
batch_root <- batch_paths$root

# Only read Telegram settings; never execute .env contents or change batch paths.
batch_telegram_notifier <- function(scripts_dir, repo, enabled = TRUE) {
  disabled <- function(text) invisible(NA)
  if (!enabled) return(disabled)
  keys <- c("MOFUSS_TELEGRAM_BOT_TOKEN", "MOFUSS_TELEGRAM_CHAT_ID")
  credentials <- Sys.getenv(keys, unset = NA_character_)
  explicit <- Sys.getenv("MOFUSS_TELEGRAM_ENV_FILE", unset = "")
  candidates <- if (nzchar(explicit)) path.expand(explicit) else
    c(file.path(scripts_dir, ".env"), file.path(repo, ".env"))
  for (path in unique(candidates)) {
    if (all(!is.na(credentials))) break
    if (!file.exists(path) || dir.exists(path)) next
    lines <- tryCatch(suppressWarnings(readLines(path, warn = FALSE,
                                                encoding = "UTF-8")),
                      error = function(e) character())
    found <- setNames(rep(NA_character_, length(keys)), keys)
    for (line in lines) {
      line <- trimws(sub("^\ufeff", "", line))
      line <- sub("^export[[:space:]]+", "", line)
      separator <- regexpr("=", line, fixed = TRUE)[[1L]]
      if (separator < 1L) next
      key <- trimws(substr(line, 1L, separator - 1L))
      if (!key %in% keys) next
      value <- trimws(substring(line, separator + 1L))
      quote <- substr(value, 1L, 1L)
      if (quote %in% c("\"", "'")) {
        end <- regexpr(quote, substring(value, 2L), fixed = TRUE)[[1L]]
        if (end < 1L) next
        trailing <- trimws(substring(value, end + 2L))
        if (nzchar(trailing) && !startsWith(trailing, "#")) next
        value <- substr(value, 2L, end)
      } else {
        value <- trimws(sub("[[:space:]]+#.*$", "", value))
      }
      found[[key]] <- value
    }
    missing <- is.na(credentials)
    credentials[missing] <- found[missing]
  }
  if (anyNA(credentials) || any(!nzchar(credentials))) {
    message("Batch Telegram notifications disabled: credentials were not found ",
            "in the repository .env or process environment.")
    return(disabled)
  }
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    message("Batch Telegram notifications disabled: install httr and jsonlite.")
    return(disabled)
  }
  function(text) {
    sent <- tryCatch(suppressWarnings({
      response <- httr::POST(
        paste0("https://api.telegram.org/bot", credentials[[keys[[1L]]]],
               "/sendMessage"),
        body = list(chat_id = credentials[[keys[[2L]]]],
                    text = substr(enc2utf8(text), 1L, 4096L)),
        encode = "form", httr::timeout(8)
      )
      !httr::http_error(response) &&
        isTRUE(httr::content(response, as = "parsed", encoding = "UTF-8")$ok)
    }), error = function(e) FALSE)
    if (!sent) {
      # HTTP exceptions can contain the secret bot URL; never print them.
      message("Telegram notification failed; the batch result is still saved locally.")
    }
    invisible(sent)
  }
}

batch_failure_reason <- function(status, log_path) {
  memory_pattern <- paste(
    "cannot allocate", "unable to allocate", "vector memory (exhausted|limit)",
    "memory exhausted", "out[- ]of[- ]memory", "bad_alloc",
    "memory allocation (failed|failure)", "not enough memory", "insufficient memory",
    sep = "|"
  )
  # Stream potentially large logs instead of loading them into the parent R session.
  memory_error <- tryCatch(suppressWarnings({
    connection <- file(log_path, open = "rt")
    on.exit(close(connection), add = TRUE)
    found <- FALSE
    repeat {
      lines <- readLines(connection, n = 500L, warn = FALSE)
      if (!length(lines)) break
      if (any(grepl(memory_pattern, lines, ignore.case = TRUE))) {
        found <- TRUE
        break
      }
    }
    found
  }), error = function(e) FALSE)
  if (memory_error) return("R ran out of memory or could not allocate memory.")
  if (status == 137L) {
    return(paste("R was killed (SIGKILL / exit 137), possibly by the operating",
                 "system after running out of memory."))
  }
  "R preprocessing failed; see the folder log for details."
}

run_mofuss_preprocessing_batch <- function(
    root,
    apply = FALSE,
    repo = batch_paths$repo,
    admin_regions = batch_paths$admin_regions,
    bau_csv = file.path(repo, "localhost/scripts/working_folders_prep/demand_bau1_v2.csv"),
    ics_csv = file.path(repo, "localhost/scripts/working_folders_prep/demand_ics3_v2.csv"),
    log_dir = file.path(
      batch_paths$log_root, "batch_main_localhost_v1",
      paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", Sys.getpid())
    ),
    continue_on_error = FALSE,
    telegram_msgs = TRUE) {
  stopifnot(is.logical(apply), length(apply) == 1L, !is.na(apply))
  stopifnot(is.logical(continue_on_error), length(continue_on_error) == 1L,
            !is.na(continue_on_error))
  stopifnot(is.logical(telegram_msgs), length(telegram_msgs) == 1L,
            !is.na(telegram_msgs))

  existing_dir <- function(path, label) {
    if (length(path) != 1L || is.na(path) || !dir.exists(path)) {
      stop(label, " does not exist: ", path)
    }
    normalizePath(path, winslash = "/", mustWork = TRUE)
  }
  existing_csv <- function(path, label) {
    if (length(path) != 1L || is.na(path)) stop("Invalid ", label, " path.")
    if (!file.exists(path) && file.exists(paste0(path, ".csv"))) {
      path <- paste0(path, ".csv")
    }
    if (!file.exists(path) || dir.exists(path) ||
        !grepl("\\.csv$", path, ignore.case = TRUE)) {
      stop(label, " CSV does not exist: ", path)
    }
    normalizePath(path, winslash = "/", mustWork = TRUE)
  }
  parameters_for <- function(folder) {
    base <- file.path(folder, "LULCC", "DownloadedDatasets")
    if (!dir.exists(base)) return(character())
    list.files(base, pattern = "^parameters\\.csv$", recursive = TRUE,
               full.names = TRUE, ignore.case = TRUE)
  }
  configuration_for <- function(path) {
    header <- readLines(path, n = 1L, warn = FALSE)
    delimiter <- if (grepl(";", header, fixed = TRUE)) ";" else ","
    parameters <- read.csv(path, sep = delimiter, colClasses = "character",
                           check.names = FALSE, na.strings = character())
    if (!all(c("Var", "ParCHR") %in% names(parameters))) {
      stop("Missing Var or ParCHR column in: ", path)
    }
    value <- unique(trimws(parameters$ParCHR[parameters$Var == "scenario_ver"]))
    value <- value[!is.na(value) & nzchar(value)]
    if (length(value) != 1L ||
        !value %in% c("BaU1_v2", "BaU2_v2", "BaU3_v2",
                      "ICS1_v2", "ICS2_v2", "ICS3_v2")) {
      stop("Missing or unsupported scenario_ver in: ", path)
    }
    option <- function(name, default) {
      selected <- parameters$ParCHR[
        !is.na(parameters$Var) & parameters$Var == name
      ]
      if (!length(selected)) return(default)
      if (length(selected) != 1L) stop("Duplicate ", name, " in: ", path)
      if (is.na(selected[[1L]]) || !nzchar(trimws(selected[[1L]]))) {
        return(default)
      }
      tolower(trimws(selected[[1L]]))
    }
    channels <- c(
      if (option("LULCt1map", "no") == "yes") "LUC1 MODIS",
      if (option("LULCt2map", "no") == "yes") "LUC2 Copernicus",
      if (option("LULCt3map", "no") == "yes") "LUC3 Woodman"
    )
    if (!length(channels)) stop("No LULC channel is enabled in: ", path)
    c(scenario = value,
      model = if ("LUC3 Woodman" %in% channels) "v14 selectable" else "v13 static",
      channels = paste(channels, collapse = " + "))
  }

  root <- existing_dir(root, "Working-folder location")
  repo <- existing_dir(repo, "MoFuSS repository")
  admin_regions <- existing_dir(admin_regions, "Admin-regions directory")
  bau_csv <- existing_csv(bau_csv, "BaU demand")
  ics_csv <- existing_csv(ics_csv, "ICS demand")
  scripts_dir <- file.path(repo, "localhost", "scripts")
  main_script <- file.path(scripts_dir, "000_main_localhost_v1.R")
  if (!file.exists(main_script)) stop("Main script does not exist: ", main_script)

  # Search the supplied directory and its immediate children only. If the
  # supplied directory is itself a working folder, use it alone.
  root_parameters <- parameters_for(root)
  folders <- if (length(root_parameters)) root else
    list.dirs(root, recursive = FALSE, full.names = TRUE)
  folders <- folders[vapply(folders, function(folder) {
    length(parameters_for(folder)) > 0L
  }, logical(1))]
  folders <- sort(unique(normalizePath(folders, winslash = "/", mustWork = TRUE)))
  if (!length(folders)) stop("No MoFuSS working folders found in: ", root)

  parameter_paths <- lapply(folders, parameters_for)
  ambiguous <- lengths(parameter_paths) != 1L
  if (any(ambiguous)) {
    stop("Expected exactly one parameters.csv in: ",
         paste(folders[ambiguous], collapse = ", "))
  }
  configurations <- lapply(parameter_paths, function(paths) {
    configuration_for(paths[[1L]])
  })
  if (any(file.exists(file.path(folders, ".env")))) {
    stop("A working folder has a .env file that could override batch paths: ",
         paste(folders[file.exists(file.path(folders, ".env"))],
               collapse = ", "))
  }
  plan <- data.frame(
    folder = folders,
    scenario = vapply(configurations, `[[`, character(1), "scenario"),
    model = vapply(configurations, `[[`, character(1), "model"),
                     stringsAsFactors = FALSE)
  print(plan, row.names = FALSE, right = FALSE)
  if (!apply) {
    cat("Preview only. Call again with apply = TRUE to run in this order.\n")
    return(invisible(plan))
  }

  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows")
    "Rscript.exe" else "Rscript")
  if (!file.exists(rscript)) stop("Rscript executable does not exist: ", rscript)
  if (!dir.exists(log_dir) && !dir.create(log_dir, recursive = TRUE)) {
    stop("Could not create batch log directory: ", log_dir)
  }
  log_dir <- normalizePath(log_dir, winslash = "/", mustWork = TRUE)
  notify <- batch_telegram_notifier(scripts_dir, repo, enabled = telegram_msgs)

  variables <- c(
    MOFUSS_GITHUB_DIR = repo,
    MOFUSS_SCRIPTS_DIR = scripts_dir,
    MOFUSS_ADMIN_DIR = admin_regions,
    MOFUSS_DEMAND_BAU_CSV = bau_csv,
    MOFUSS_DEMAND_ICS_CSV = ics_csv,
    MOFUSS_TELEGRAM_MSGS = "0"
  )
  variable_names <- c(names(variables), "MOFUSS_COUNTRY_DIR")
  previous <- Sys.getenv(variable_names, unset = NA_character_)
  old_wd <- getwd()
  on.exit({
    Sys.unsetenv(variable_names)
    present <- !is.na(previous)
    if (any(present)) do.call(Sys.setenv, as.list(previous[present]))
    setwd(old_wd)
  }, add = TRUE)
  do.call(Sys.setenv, as.list(variables))

  plan$status <- NA_integer_
  plan$log <- NA_character_
  plan$telegram_sent <- NA
  for (i in seq_len(nrow(plan))) {
    folder <- plan$folder[[i]]
    log_path <- file.path(log_dir, paste0(basename(folder), ".log"))
    cat(sprintf("[%d/%d] Running %s (%s)\n", i, nrow(plan),
                basename(folder), plan$scenario[[i]]))
    Sys.setenv(MOFUSS_COUNTRY_DIR = folder)
    setwd(folder)
    started <- Sys.time()
    status <- suppressWarnings(system2(
      rscript, args = c("--vanilla", shQuote(main_script)),
      stdout = log_path, stderr = log_path, wait = TRUE
    ))
    plan$status[[i]] <- as.integer(status)
    plan$log[[i]] <- log_path
    cat("  Exit status: ", status, " | Log: ", log_path, "\n", sep = "")
    elapsed <- round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1L)
    result <- if (status == 0L) "MoFuSS folder completed successfully" else
      "MoFuSS folder failed"
    details <- if (status == 0L) "All preprocessing scripts finished successfully." else
      batch_failure_reason(status, log_path)
    # The parent sends the result, even when an OOM kill prevents child R cleanup.
    # If the parent session is also killed, it cannot deliver a notification.
    plan$telegram_sent[[i]] <- notify(paste(
      result,
      paste0("Folder: ", basename(folder), " [", i, "/", nrow(plan), "]"),
      paste0("Scenario: ", plan$scenario[[i]]),
      paste0("Computer: ", Sys.info()[["nodename"]]),
      paste0("Elapsed: ", elapsed, " minutes | Exit status: ", status),
      details, paste0("Log: ", log_path), sep = "\n"
    ))
    write.csv(plan, file.path(log_dir, "batch_results.csv"), row.names = FALSE)
    if (status != 0L && !continue_on_error) {
      stop("Preprocessing failed in ", folder, ". See ", log_path,
           ". Remaining folders were not started.")
    }
  }
  cat("Batch finished. Results: ",
      file.path(log_dir, "batch_results.csv"), "\n", sep = "")
  invisible(plan)
}

run_mofuss_preprocessing_batch(batch_root, apply = batch_apply,
                              telegram_msgs = batch_telegram_msgs)
