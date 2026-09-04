# MoFuSS run-log collector
# Version: 2.0
# Date: August 2026

rm(list = ls(all.names = TRUE))

# This utility intentionally uses base R only. The original 2016 script loaded
# numerous retired spatial packages even though it only copied and summarized
# text log files.

args <- commandArgs(trailingOnly = TRUE)
BaUvsICS <- "BaU"
scenario_arg <- grep("^BaUvsICS=", args, value = TRUE)
if (length(scenario_arg) > 0L) {
  BaUvsICS <- sub("^BaUvsICS=", "", scenario_arg[[length(scenario_arg)]])
  BaUvsICS <- sub("^[\\\"']", "", BaUvsICS)
  BaUvsICS <- sub("[\\\"']$", "", BaUvsICS)
}

logs_dir <- "Logs"
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

collect_log_files <- function(directory) {
  if (!dir.exists(directory)) {
    return(character())
  }
  list.files(
    directory,
    pattern = "(\\.Rout$|^(debug|log)\\.txt$)",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  )
}

root_logs <- collect_log_files(".")
lulcc_logs <- collect_log_files("LULCC")

# The current finalogs Rout is still open while this script runs and therefore
# cannot be archived reliably. It is excluded rather than copying a partial log.
root_logs <- root_logs[!grepl(
  "^finalogs(_v[0-9]+)?\\.Rout$",
  basename(root_logs),
  ignore.case = TRUE
)]

sources <- c(root_logs, lulcc_logs)
prefixes <- c(
  rep("", length(root_logs)),
  rep("LULCC_", length(lulcc_logs))
)
destinations <- file.path(logs_dir, paste0(prefixes, basename(sources)))

copied <- logical(length(sources))
if (length(sources) > 0L) {
  copied <- file.copy(
    from = sources,
    to = destinations,
    overwrite = TRUE,
    copy.mode = TRUE,
    copy.date = TRUE
  )
}

log_manifest <- data.frame(
  scenario = rep(BaUvsICS, length(sources)),
  source = sources,
  destination = destinations,
  copied = copied,
  stringsAsFactors = FALSE
)
utils::write.csv(
  log_manifest,
  file.path(logs_dir, "log_collection_manifest.csv"),
  row.names = FALSE
)

extract_proc_time <- function(path) {
  lines <- readLines(path, warn = FALSE)
  marker <- grep("^>\\s*proc\\.time\\(\\)", lines)
  if (length(marker) == 0L) {
    return(FALSE)
  }

  after <- lines[seq.int(
    min(marker[[length(marker)]] + 1L, length(lines)),
    min(marker[[length(marker)]] + 5L, length(lines))
  )]
  header_index <- grep("\\buser\\b.*\\bsystem\\b.*\\belapsed\\b", after)
  if (length(header_index) == 0L) {
    return(FALSE)
  }

  header_index <- header_index[[1L]]
  value_candidates <- after[seq.int(
    min(header_index + 1L, length(after)),
    length(after)
  )]
  value_candidates <- value_candidates[nzchar(trimws(value_candidates))]
  if (length(value_candidates) == 0L) {
    return(FALSE)
  }

  timing <- tryCatch(
    utils::read.table(
      text = paste(after[[header_index]], value_candidates[[1L]], sep = "\n"),
      header = TRUE,
      check.names = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(timing) || nrow(timing) != 1L) {
    return(FALSE)
  }

  timing_file <- file.path(
    logs_dir,
    paste0(tools::file_path_sans_ext(basename(path)), "_PT.csv")
  )
  utils::write.csv(timing, timing_file, row.names = FALSE)
  TRUE
}

rout_files <- destinations[copied & grepl("\\.Rout$", destinations, ignore.case = TRUE)]
timing_extracted <- if (length(rout_files) > 0L) {
  vapply(rout_files, extract_proc_time, logical(1))
} else {
  logical()
}

summary_table <- data.frame(
  scenario = BaUvsICS,
  files_discovered = length(sources),
  files_copied = sum(copied),
  timing_tables_written = sum(timing_extracted),
  stringsAsFactors = FALSE
)
utils::write.csv(
  summary_table,
  file.path(logs_dir, "finalogs_summary.csv"),
  row.names = FALSE
)

# Preserve the historical archive behavior when both legacy image products are
# present. Failure to find a zip executable is reported but is not a model error.
archive_requested <-
  file.exists("Out/jpg/Map_AGB.jpg") &&
  file.exists("Out/png/Boxplots.png")

if (archive_requested) {
  bundled_zip <- "LULCC/Wizard_imgs/zip.exe"
  zip_executable <- if (file.exists(bundled_zip)) {
    bundled_zip
  } else {
    unname(Sys.which("zip"))
  }

  if (nzchar(zip_executable)) {
    archive_members <- list.files(
      logs_dir,
      full.names = TRUE,
      recursive = FALSE
    )
    archive_members <- archive_members[
      basename(archive_members) != "all_logs.zip"
    ]
    if (length(archive_members) > 0L) {
      tryCatch(
        utils::zip(
          zipfile = file.path(logs_dir, "all_logs.zip"),
          files = archive_members,
          flags = "-j",
          zip = zip_executable
        ),
        error = function(e) warning(
          "Log files were collected, but the optional zip archive failed: ",
          conditionMessage(e)
        )
      )
    }
  } else {
    warning(
      "Log files were collected, but no zip executable was available; ",
      "all_logs.zip was not created."
    )
  }
}

message(
  sprintf(
    "finalogs_v2 complete: copied %d of %d discovered log file(s); %d timing table(s) written.",
    sum(copied),
    length(sources),
    sum(timing_extracted)
  )
)
