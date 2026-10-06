# Exercise folder results and killed child processes without Telegram traffic.
run_tests <- function() {
  script <- "localhost/scripts/working_folders_prep/batch_000_main_localhost_v1.R"
  if (!file.exists(script)) script <- sub("^localhost/", "", script)
  helpers <- new.env(parent = globalenv())
  expressions <- parse(script)
  for (expression in expressions[-length(expressions)]) eval(expression, helpers)

  scratch <- tempfile("batch telegram ")
  dir.create(scratch)
  repo <- file.path(scratch, "repo with spaces")
  scripts <- file.path(repo, "localhost", "scripts")
  admin <- file.path(scratch, "admin")
  root <- file.path(scratch, "folders")
  for (path in c(scripts, admin, root)) dir.create(path, recursive = TRUE)
  keys <- c("MOFUSS_TELEGRAM_BOT_TOKEN", "MOFUSS_TELEGRAM_CHAT_ID")
  env_names <- c(keys, "MOFUSS_TELEGRAM_ENV_FILE", "MOFUSS_COUNTRY_DIR",
                 "MOFUSS_SCRIPTS_DIR", "MOFUSS_TELEGRAM_MSGS")
  original <- Sys.getenv(env_names, unset = NA_character_)
  old_wd <- getwd()
  Sys.unsetenv(env_names)
  on.exit({
    setwd(old_wd)
    Sys.unsetenv(env_names)
    present <- !is.na(original)
    if (any(present)) do.call(Sys.setenv, as.list(original[present]))
    unlink(scratch, recursive = TRUE)
  }, add = TRUE)

  # Fake only HTTP; batch jobs below use the real Rscript and system2.
  transport <- new.env()
  transport$requests <- list()
  transport$fail <- FALSE
  transport$accepted <- TRUE
  namespace <- asNamespace("httr")
  original_post <- get("POST", envir = namespace)
  replace_post <- function(value) {
    unlockBinding("POST", namespace)
    assign("POST", value, envir = namespace)
    lockBinding("POST", namespace)
  }
  replace_post(function(url, ..., body, encode) {
    transport$requests[[length(transport$requests) + 1L]] <-
      list(url = url, body = body, encode = encode)
    if (transport$fail) stop("HTTP error at ", url)
    structure(list(
      url = url, status_code = 200L,
      headers = list("Content-Type" = "application/json"),
      content = charToRaw(if (transport$accepted) '{"ok":true}' else '{"ok":false}')
    ), class = "response")
  })
  on.exit(replace_post(original_post), add = TRUE)

  env_file <- file.path(scripts, ".env")
  writeLines(c(
    'MOFUSS_TELEGRAM_BOT_TOKEN="fake-token" # local bot',
    "export MOFUSS_TELEGRAM_CHAT_ID='123'",
    'MOFUSS_COUNTRY_DIR="should not change"',
    'UNRELATED=$(touch should_never_exist)'
  ), env_file)
  notify <- helpers$batch_telegram_notifier(scripts, repo)
  stopifnot(is.na(Sys.getenv("MOFUSS_COUNTRY_DIR", unset = NA_character_)))
  stopifnot(notify("fixture message"))
  request <- transport$requests[[1L]]
  stopifnot(identical(request$url, "https://api.telegram.org/botfake-token/sendMessage"),
            identical(request$body$chat_id, "123"),
            identical(request$body$text, "fixture message"),
            identical(request$encode, "form"),
            !file.exists("should_never_exist"))
  Sys.setenv(MOFUSS_TELEGRAM_BOT_TOKEN = "process-token")
  notify <- helpers$batch_telegram_notifier(scripts, repo)
  stopifnot(notify("process override"),
            grepl("botprocess-token/", tail(transport$requests, 1L)[[1L]]$url))
  Sys.unsetenv(keys)
  other_env <- file.path(scratch, "explicit.env")
  writeLines(c("MOFUSS_TELEGRAM_BOT_TOKEN=other-token",
               "MOFUSS_TELEGRAM_CHAT_ID=456"), other_env)
  Sys.setenv(MOFUSS_TELEGRAM_ENV_FILE = other_env)
  notify <- helpers$batch_telegram_notifier(scripts, repo)
  stopifnot(notify("explicit file"),
            identical(tail(transport$requests, 1L)[[1L]]$body$chat_id, "456"))
  Sys.unsetenv("MOFUSS_TELEGRAM_ENV_FILE")
  file.rename(env_file, file.path(repo, ".env"))
  notify <- helpers$batch_telegram_notifier(scripts, repo)
  stopifnot(notify("repository fallback"),
            identical(tail(transport$requests, 1L)[[1L]]$body$chat_id, "123"))
  file.rename(file.path(repo, ".env"), env_file)

  transport$fail <- TRUE
  output <- capture.output(sent <- notify("network failure"), type = "message")
  stopifnot(identical(sent, FALSE),
            any(grepl("Telegram notification failed", output)),
            !any(grepl("fake-token", output)))
  transport$fail <- FALSE
  transport$accepted <- FALSE
  suppressMessages(stopifnot(identical(notify("API rejection"), FALSE)))
  transport$accepted <- TRUE
  stopifnot(notify(strrep("a", 5000L)),
            nchar(tail(transport$requests, 1L)[[1L]]$body$text) == 4096L)

  main <- file.path(scripts, "000_main_localhost_v1.R")
  writeLines(c(
    'stopifnot(Sys.getenv("MOFUSS_TELEGRAM_MSGS") == "0")',
    'kind <- basename(Sys.getenv("MOFUSS_COUNTRY_DIR"))',
    'if (grepl("oom", kind)) stop("cannot allocate vector of size 4.0 Gb")',
    'if (grepl("error", kind)) stop("missing input file")',
    'if (grepl("killed", kind)) tools::pskill(Sys.getpid(), signal = 9L)',
    'cat("Fixture finished successfully.\\n")'
  ), main)
  folders <- c("01_success", "02_oom", "03_success", "04_error")
  if (.Platform$OS.type == "unix") folders <- c(folders, "05_killed")
  for (folder in folders) {
    base <- file.path(root, folder, "LULCC", "DownloadedDatasets")
    dir.create(base, recursive = TRUE)
    write.csv(data.frame(Var = c("scenario_ver", "LULCt1map"),
                         ParCHR = c("BaU1_v2", "yes")),
              file.path(base, "parameters.csv"), row.names = FALSE)
  }
  bau <- file.path(scratch, "bau.csv")
  ics <- file.path(scratch, "ics.csv")
  writeLines("fixture", bau)
  writeLines("fixture", ics)
  arguments <- list(root = root, repo = repo, admin_regions = admin,
                    bau_csv = bau, ics_csv = ics, log_dir = file.path(scratch, "logs"))
  run_batch <- function(...) {
    do.call(helpers$run_mofuss_preprocessing_batch, c(arguments, list(...)))
  }

  transport$requests <- list()
  preview <- run_batch()
  stopifnot(nrow(preview) == length(folders), length(transport$requests) == 0L,
            !dir.exists(arguments$log_dir))
  plan <- run_batch(apply = TRUE, continue_on_error = TRUE)
  messages <- vapply(transport$requests, function(x) x$body$text, character(1))
  stopifnot(length(messages) == length(folders), all(plan$telegram_sent),
            identical(plan$status[1:4], c(0L, 1L, 0L, 1L)),
            grepl("completed successfully", messages[[1L]]),
            grepl("02_oom", messages[[2L]]),
            grepl("could not allocate memory", messages[[2L]]),
            grepl("completed successfully", messages[[3L]]),
            grepl("failed", messages[[4L]]),
            !grepl("ran out of memory", messages[[4L]]),
            identical(getwd(), old_wd),
            is.na(Sys.getenv("MOFUSS_TELEGRAM_MSGS", unset = NA_character_)))
  if (.Platform$OS.type == "unix") {
    stopifnot(plan$status[[5L]] == 137L,
              grepl("SIGKILL / exit 137", messages[[5L]]),
              grepl("possibly", messages[[5L]]))
  }
  saved <- read.csv(file.path(arguments$log_dir, "batch_results.csv"))
  stopifnot(identical(saved$status, plan$status), all(saved$telegram_sent))

  transport$requests <- list()
  stopped <- tryCatch(run_batch(apply = TRUE), error = identity)
  stopifnot(inherits(stopped, "error"), length(transport$requests) == 2L,
            grepl("could not allocate memory", transport$requests[[2L]]$body$text),
            identical(getwd(), old_wd))
  saved <- read.csv(file.path(arguments$log_dir, "batch_results.csv"))
  stopifnot(identical(saved$status[1:2], c(0L, 1L)), all(is.na(saved$status[-(1:2)])))

  transport$requests <- list()
  transport$fail <- TRUE
  suppressMessages(plan <- run_batch(apply = TRUE, continue_on_error = TRUE))
  stopifnot(all(!plan$telegram_sent), plan$status[[1L]] == 0L,
            plan$status[[2L]] == 1L, nrow(plan) == length(folders))
  transport$fail <- FALSE

  transport$requests <- list()
  plan <- run_batch(apply = TRUE, continue_on_error = TRUE, telegram_msgs = FALSE)
  stopifnot(length(transport$requests) == 0L, all(is.na(plan$telegram_sent)))
  unlink(env_file)
  suppressMessages(plan <- run_batch(apply = TRUE, continue_on_error = TRUE))
  stopifnot(length(transport$requests) == 0L, all(is.na(plan$telegram_sent)))
  cat("Batch preprocessing Telegram tests passed.\n")
}

run_tests()
