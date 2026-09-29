# SPDX-License-Identifier: Apache-2.0
#
# Back up expensive MoFuSS IDW results under WORKING_FOLDERS_ROOT.
# The backups are working-folder-relative overlays: copy the
# contents of `working_folder_overlay` into the matching working folder.
#
# BEGIN USER INPUTS ----------------------------------------------------------
# Edit these settings, save, and click Source in RStudio.
# Parent containing the scenario folders. Use forward slashes on both systems.
# Examples: "E:/" on Windows or "/home/mofuss/Documents" on Linux.
WORKING_FOLDERS_ROOT <- if (.Platform$OS.type == "windows") {
  "E:/"
} else {
  path.expand("~/Documents")
}

# NULL writes to <WORKING_FOLDERS_ROOT>/IDW_backups. Alternatively provide an
# absolute destination, e.g. "F:/IDW_backups" or "/mnt/backup/IDW_backups".
BACKUP_ROOT <- NULL

# RunCode matching is exact and case-insensitive. Use character() to exclude
# nothing.
EXCLUDE_RUN_CODES <- c("GLEA")

# Default backup directory below WORKING_FOLDERS_ROOT. Existing
# snapshots are never overwritten. An unchanged latest snapshot is skipped.
BACKUP_DIRECTORY_NAME <- "IDW_backups"

# END USER INPUTS ------------------------------------------------------------

# Optional command-line use:
#   Rscript batch_idw_bkup.R --dry-run
#   Rscript batch_idw_bkup.R --yes
#   Rscript batch_idw_bkup.R --exclude=GOG,GLEA
#
# Optional command-line flags:
#   --exclude=GOG,GLEA   Override EXCLUDE_RUN_CODES (use --exclude= for none).
#   --root="E:/"         Override WORKING_FOLDERS_ROOT.
#                        Linux: --root="/home/mofuss/Documents"
#   --backup-root="E:/IDW_backups"
#                        Override the backup destination.
#   --dry-run            Inspect and print the plan without copying anything.
#   --yes                Skip the final confirmation prompt.
#   --force              Create a new snapshot even if the latest is unchanged.

options(stringsAsFactors = FALSE)

WORKING_FOLDER_PATTERN <- paste0(
  "^(.+)_([0-9]+m)_((?:bau|ics)[0-9]+)_([0-9]{4})_mc",
  "([0-9]+)_(capped|uncapped)$"
)
TOP_IDW_PATTERN <- "^IDW_C\\+\\+_fw_[wv][0-9]{2}\\.tif$"
HC_METADATA_PATTERN <- paste0(
  "^(?:HC_job_manifest.*\\.csv|HC_IDW_install_manifest\\.csv|",
  "[WV]_origin_demand_manifest\\.csv|country_direction_rules\\.csv|",
  "README_IDW_.*\\.txt|.*\\.(?:log|out|err))$"
)
DEMAND_RECOVERY_PATTERN <- paste0(
  "^(?:[WV]_origin_demand[0-9]{2}\\.csv|",
  "[WV]_origin_component_index\\.csv|",
  "SINGLE_COMPONENT_IDW_install_manifest\\.csv|",
  "README_SINGLE_COMPONENT_IDW_INSTALL\\.txt)$"
)

stopf <- function(fmt, ...) {
  stop(sprintf(fmt, ...), call. = FALSE)
}

normalize_loose <- function(path) {
  normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
}

normalize_existing <- function(path, label) {
  normalized <- normalize_loose(path)
  if (!dir.exists(normalized) && !file.exists(normalized)) {
    stopf("%s does not exist: %s", label, normalized)
  }
  normalizePath(normalized, winslash = "/", mustWork = TRUE)
}

is_absolute_path <- function(path) {
  grepl("^(?:[A-Za-z]:[/\\\\]|[/\\\\]{2}|/)", path, perl = TRUE)
}

absolute_from <- function(path, base) {
  if (length(path) != 1L || is.na(path) || !nzchar(trimws(path))) {
    stopf("Folder path must be one non-blank value.")
  }
  path <- path.expand(path)
  if (is_absolute_path(path)) normalize_loose(path) else normalize_loose(file.path(base, path))
}

get_script_path <- function() {
  full_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", full_args, value = TRUE)
  if (length(file_arg)) {
    return(normalize_existing(sub("^--file=", "", file_arg[[1L]]), "Script"))
  }

  frame_files <- vapply(
    sys.frames(),
    function(frame) {
      value <- frame$ofile
      if (is.null(value) || !length(value)) NA_character_ else as.character(value[[1L]])
    },
    character(1L)
  )
  frame_files <- frame_files[!is.na(frame_files) & nzchar(frame_files)]
  if (length(frame_files)) {
    return(normalize_existing(tail(frame_files, 1L), "Script"))
  }

  if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    editor_path <- rstudioapi::getSourceEditorContext()$path
    if (length(editor_path) && nzchar(editor_path)) {
      return(normalize_existing(editor_path, "Script"))
    }
  }

  fallback <- file.path(getwd(), "batch_idw_bkup.R")
  if (file.exists(fallback)) {
    return(normalize_existing(fallback, "Script"))
  }
  stopf(
    paste0(
      "Could not determine this script's location. Source the saved file from ",
      "RStudio or run it with Rscript."
    )
  )
}

parse_options <- function(args) {
  result <- list(
    exclude = NULL,
    root = NULL,
    backup_root = NULL,
    dry_run = FALSE,
    yes = FALSE,
    force = FALSE
  )
  for (arg in args) {
    if (identical(arg, "--dry-run")) {
      result$dry_run <- TRUE
    } else if (identical(arg, "--yes")) {
      result$yes <- TRUE
    } else if (identical(arg, "--force")) {
      result$force <- TRUE
    } else if (startsWith(arg, "--exclude=")) {
      result$exclude <- sub("^--exclude=", "", arg)
    } else if (startsWith(arg, "--root=")) {
      result$root <- sub("^--root=", "", arg)
    } else if (startsWith(arg, "--backup-root=")) {
      result$backup_root <- sub("^--backup-root=", "", arg)
    } else {
      stopf("Unknown command-line option: %s", arg)
    }
  }
  result
}

normalize_exclusions <- function(values) {
  if (is.null(values) || !length(values)) return(character())
  values <- trimws(as.character(values))
  values <- unique(values[nzchar(values)])
  if (!length(values)) return(character())
  invalid <- values[!grepl(
    "^[A-Za-z0-9]+(?:[-_][A-Za-z0-9]+)*$",
    values,
    perl = TRUE
  )]
  if (length(invalid)) {
    stopf(
      "Invalid EXCLUDE_RUN_CODES value(s): %s",
      paste(invalid, collapse = ", ")
    )
  }
  values
}

parse_working_folder <- function(path) {
  name <- basename(path)
  match <- regexec(
    WORKING_FOLDER_PATTERN,
    name,
    ignore.case = TRUE,
    perl = TRUE
  )
  fields <- regmatches(name, match)[[1L]]
  if (length(fields) != 7L) return(NULL)
  if (!grepl(
    "^[A-Za-z0-9]+(?:[-_][A-Za-z0-9]+)*$",
    fields[[2L]],
    perl = TRUE
  )) return(NULL)
  list(
    path = normalize_existing(path, "Working folder"),
    name = name,
    run_code = fields[[2L]],
    resolution = fields[[3L]],
    scenario = tolower(fields[[4L]]),
    end_year = as.integer(fields[[5L]]),
    monte_carlo_runs = as.integer(fields[[6L]]),
    cap = tolower(fields[[7L]])
  )
}

discover_working_folders <- function(root) {
  children <- list.dirs(root, full.names = TRUE, recursive = FALSE)
  parsed <- lapply(children, parse_working_folder)
  parsed <- Filter(Negate(is.null), parsed)
  parsed[order(tolower(vapply(parsed, `[[`, character(1L), "name")))]
}

path_key <- function(path) {
  value <- normalize_loose(path)
  if (.Platform$OS.type == "windows") tolower(value) else value
}

path_is_within <- function(path, parent, allow_equal = FALSE) {
  path <- path_key(path)
  parent <- sub("/+$", "", path_key(parent))
  identical(path, parent) && allow_equal || startsWith(path, paste0(parent, "/"))
}

relative_to <- function(path, root) {
  path <- normalize_existing(path, "Backup source file")
  root <- sub("/+$", "", normalize_existing(root, "Working folder"))
  if (!path_is_within(path, root)) {
    stopf("Refusing source outside its working folder: %s", path)
  }
  substring(path, nchar(root) + 2L)
}

list_regular_files <- function(path, recursive = TRUE) {
  if (!dir.exists(path)) return(character())
  paths <- list.files(
    path,
    full.names = TRUE,
    recursive = recursive,
    all.files = TRUE,
    no.. = TRUE,
    include.dirs = FALSE
  )
  if (!length(paths)) return(character())
  info <- file.info(paths)
  paths[!is.na(info$isdir) & !info$isdir]
}

format_utc <- function(value) {
  format(value, format = "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

collect_idw_payload <- function(folder) {
  run_root <- folder$path
  in_root <- file.path(run_root, "In")
  hc_root <- file.path(in_root, "DemandScenarios", "HC_jobs")
  demand_root <- file.path(in_root, "DemandScenarios")
  records <- list()
  seen <- character()

  add_files <- function(paths, category) {
    if (!length(paths)) return(invisible(NULL))
    existing <- paths[file.exists(paths)]
    if (!length(existing)) return(invisible(NULL))
    info <- file.info(existing)
    existing <- existing[!is.na(info$isdir) & !info$isdir]
    for (path in existing) {
      normalized <- normalize_existing(path, "Backup source file")
      relative <- gsub("\\\\", "/", relative_to(normalized, run_root))
      key <- if (.Platform$OS.type == "windows") tolower(relative) else relative
      if (key %in% seen) next
      seen <<- c(seen, key)
      records[[length(records) + 1L]] <<- data.frame(
        SourcePath = normalized,
        RelativePath = relative,
        Category = category,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
    invisible(NULL)
  }

  if (dir.exists(in_root)) {
    immediate_in <- list_regular_files(in_root, recursive = FALSE)
    add_files(
      immediate_in[grepl(TOP_IDW_PATTERN, basename(immediate_in), ignore.case = TRUE)],
      "top_level_idw"
    )
  }

  if (dir.exists(hc_root)) {
    hc_dirs <- list.dirs(hc_root, full.names = TRUE, recursive = FALSE)
    raw_dirs <- hc_dirs[startsWith(tolower(basename(hc_dirs)), "idw_")]
    for (raw_dir in raw_dirs) {
      add_files(list_regular_files(raw_dir, recursive = TRUE), "directional_raw_output")
    }
  }

  for (component_dir in c("W_origin_components", "V_origin_components")) {
    add_files(
      list_regular_files(file.path(in_root, component_dir), recursive = TRUE),
      "installed_component"
    )
  }

  core_count <- length(records)
  if (!core_count) {
    return(data.frame(
      SourcePath = character(),
      RelativePath = character(),
      Category = character(),
      Bytes = numeric(),
      SourceModifiedUTC = character(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  if (dir.exists(demand_root)) {
    demand_files <- list_regular_files(demand_root, recursive = FALSE)
    add_files(
      demand_files[grepl(
        DEMAND_RECOVERY_PATTERN,
        basename(demand_files),
        ignore.case = TRUE,
        perl = TRUE
      )],
      "installed_demand_and_index"
    )
  }
  if (dir.exists(hc_root)) {
    hc_files <- list_regular_files(hc_root, recursive = FALSE)
    add_files(
      hc_files[grepl(
        HC_METADATA_PATTERN,
        basename(hc_files),
        ignore.case = TRUE,
        perl = TRUE
      )],
      "idw_metadata"
    )
  }

  result <- do.call(rbind, records)
  info <- file.info(result$SourcePath)
  if (any(is.na(info$size)) || any(info$isdir)) {
    stopf("Could not inspect every IDW source file in %s.", run_root)
  }
  result$Bytes <- as.numeric(info$size)
  result$SourceModifiedUTC <- format_utc(info$mtime)
  result <- result[order(tolower(result$RelativePath)), , drop = FALSE]
  rownames(result) <- NULL
  result
}

extract_suffixes <- function(paths, pattern) {
  selected <- grepl(pattern, paths, ignore.case = TRUE, perl = TRUE)
  if (!any(selected)) return(character())
  sort(unique(sub(pattern, "\\1", paths[selected], ignore.case = TRUE, perl = TRUE)))
}

analyze_directional_outputs <- function(folder, files) {
  manifest_path <- file.path(
    folder$path,
    "In", "DemandScenarios", "HC_jobs", "HC_job_manifest_idw_ready.csv"
  )
  result <- list(
    manifest_present = file.exists(manifest_path),
    expected = 0L,
    present = 0L,
    complete = FALSE,
    missing = character(),
    warning = ""
  )
  if (!result$manifest_present) return(result)

  tryCatch({
    manifest <- utils::read.csv(
      manifest_path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      na.strings = c("", "NA")
    )
    required <- c("JobID", "Channel", "Status", "PeriodStart", "PeriodEnd")
    missing_columns <- setdiff(required, names(manifest))
    if (length(missing_columns)) {
      stopf("missing columns: %s", paste(missing_columns, collapse = ", "))
    }
    ready <- toupper(trimws(as.character(manifest$Status))) == "IDW_READY"
    manifest <- manifest[!is.na(ready) & ready, , drop = FALSE]
    if (!nrow(manifest)) stopf("contains no IDW_READY jobs")
    job_ids <- trimws(as.character(manifest$JobID))
    channels <- tolower(trimws(as.character(manifest$Channel)))
    if (any(!grepl("^[A-Za-z0-9_]+$", job_ids)) ||
        any(!channels %in% c("w", "v"))) {
      stopf("contains an unsafe JobID or invalid Channel")
    }
    starts <- unique(suppressWarnings(as.integer(manifest$PeriodStart)))
    ends <- unique(suppressWarnings(as.integer(manifest$PeriodEnd)))
    if (length(starts) != 1L || length(ends) != 1L ||
        is.na(starts) || is.na(ends) || starts < 1L || ends < starts ||
        (ends - starts) %% 10L != 0L || ends > 99L) {
      stopf("contains an invalid or inconsistent period range")
    }
    periods <- seq.int(starts, ends, by = 10L)
    expected <- unlist(lapply(seq_len(nrow(manifest)), function(index) {
      file.path(
        "In", "DemandScenarios", "HC_jobs",
        paste0("idw_", job_ids[[index]]),
        sprintf("IDW_C++_fw_%s%02d.tif", channels[[index]], periods)
      )
    }), use.names = FALSE)
    expected <- gsub("\\\\", "/", expected)
    present_keys <- tolower(files$RelativePath)
    is_present <- tolower(expected) %in% present_keys
    result$expected <- length(expected)
    result$present <- sum(is_present)
    result$complete <- all(is_present)
    result$missing <- expected[!is_present]
    result
  }, error = function(error) {
    result$warning <- conditionMessage(error)
    result
  })
}

analyze_installed_outputs <- function(files) {
  paths <- tolower(files$RelativePath)
  top_w <- extract_suffixes(paths, "^in/idw_c\\+\\+_fw_w([0-9]{2})\\.tif$")
  top_v <- extract_suffixes(paths, "^in/idw_c\\+\\+_fw_v([0-9]{2})\\.tif$")
  component_patterns <- c(
    W = "^in/w_origin_components/idw_c\\+\\+_fw_w[0-9]{3}_([0-9]{2})\\.tif$",
    V = "^in/v_origin_components/idw_c\\+\\+_fw_v[0-9]{3}_([0-9]{2})\\.tif$"
  )
  component_paths <- lapply(component_patterns, function(pattern) {
    paths[grepl(pattern, paths, ignore.case = TRUE, perl = TRUE)]
  })
  component_suffixes <- Map(function(channel_paths, pattern) {
    if (!length(channel_paths)) return(character())
    sub(pattern, "\\1", channel_paths, ignore.case = TRUE, perl = TRUE)
  }, component_paths, component_patterns)
  demand_w <- extract_suffixes(
    paths,
    "^in/demandscenarios/w_origin_demand([0-9]{2})\\.csv$"
  )
  demand_v <- extract_suffixes(
    paths,
    "^in/demandscenarios/v_origin_demand([0-9]{2})\\.csv$"
  )
  index_relatives <- c(
    "in/demandscenarios/w_origin_component_index.csv",
    "in/demandscenarios/v_origin_component_index.csv"
  )
  has_indexes <- all(index_relatives %in% paths)
  index_counts <- c(W = 0L, V = 0L)
  if (has_indexes) {
    for (index in seq_along(index_relatives)) {
      source_index <- which(paths == index_relatives[[index]])
      index_counts[[index]] <- tryCatch(
        nrow(utils::read.csv(
          files$SourcePath[[source_index[[1L]]]],
          stringsAsFactors = FALSE,
          check.names = FALSE
        )),
        error = function(error) 0L
      )
    }
  }
  has_marker <- any(c(
    "in/demandscenarios/hc_jobs/hc_idw_install_manifest.csv",
    "in/demandscenarios/single_component_idw_install_manifest.csv"
  ) %in% paths)
  demand_sequence <- length(demand_w) > 0L && identical(demand_w, demand_v) &&
    identical(as.integer(demand_w), seq_len(length(demand_w)))
  complete_component_grid <- function(channel, top_suffixes) {
    suffixes <- component_suffixes[[channel]]
    expected_components <- index_counts[[channel]]
    if (!length(suffixes) || !length(top_suffixes) || expected_components < 1L) {
      return(FALSE)
    }
    counts <- table(factor(suffixes, levels = top_suffixes))
    !any(is.na(counts)) && all(as.integer(counts) == expected_components) &&
      length(suffixes) == expected_components * length(top_suffixes)
  }
  ready <- length(top_w) > 0L && identical(top_w, top_v) &&
    complete_component_grid("W", top_w) &&
    complete_component_grid("V", top_v) &&
    demand_sequence && has_indexes && has_marker
  list(
    ready_for_egoml = ready,
    top_w = length(top_w),
    top_v = length(top_v),
    component_w_periods = length(unique(component_suffixes$W)),
    component_v_periods = length(unique(component_suffixes$V)),
    component_w_files = length(component_paths$W),
    component_v_files = length(component_paths$V),
    component_w_index_rows = index_counts[["W"]],
    component_v_index_rows = index_counts[["V"]],
    annual_demand_periods = if (identical(demand_w, demand_v)) length(demand_w) else 0L,
    has_indexes = has_indexes,
    has_install_marker = has_marker
  )
}

classify_recovery <- function(folder, files) {
  directional <- analyze_directional_outputs(folder, files)
  installed <- analyze_installed_outputs(files)
  categories <- table(files$Category)
  raw_count <- if ("directional_raw_output" %in% names(categories)) {
    unname(categories[["directional_raw_output"]])
  } else 0L
  top_count <- if ("top_level_idw" %in% names(categories)) {
    unname(categories[["top_level_idw"]])
  } else 0L
  if (installed$ready_for_egoml) {
    level <- "READY_FOR_EGOML"
  } else if (directional$complete) {
    level <- "RAW_DIRECTIONAL_COMPLETE_FOR_STEP_9"
  } else if (raw_count > 0L) {
    level <- "RAW_DIRECTIONAL_PARTIAL"
  } else if (top_count > 0L && installed$top_w == installed$top_v &&
             installed$top_w > 0L) {
    level <- "STANDARD_IDW_OUTPUTS"
  } else {
    level <- "PARTIAL_IDW_OUTPUTS"
  }
  list(level = level, directional = directional, installed = installed)
}

human_size <- function(bytes) {
  units <- c("B", "KB", "MB", "GB", "TB")
  value <- as.numeric(bytes)
  index <- 1L
  while (is.finite(value) && value >= 1024 && index < length(units)) {
    value <- value / 1024
    index <- index + 1L
  }
  sprintf(if (index == 1L) "%.0f %s" else "%.2f %s", value, units[[index]])
}

find_latest_snapshot <- function(run_backup_root) {
  if (!dir.exists(run_backup_root)) return(NULL)
  pointer <- file.path(run_backup_root, "LATEST.txt")
  candidates <- character()
  if (file.exists(pointer)) {
    value <- trimws(readLines(pointer, warn = FALSE, n = 1L))
    if (length(value) && grepl("^[A-Za-z0-9_.-]+$", value)) {
      candidates <- file.path(run_backup_root, value)
    }
  }
  candidates <- c(
    candidates,
    rev(sort(list.dirs(run_backup_root, full.names = TRUE, recursive = FALSE)))
  )
  candidates <- unique(candidates)
  valid <- candidates[
    dir.exists(candidates) & file.exists(file.path(candidates, ".complete")) &
      file.exists(file.path(candidates, "backup_manifest.csv"))
  ]
  if (length(valid)) normalize_existing(valid[[1L]], "Latest backup snapshot") else NULL
}

snapshot_is_unchanged <- function(files, snapshot) {
  if (is.null(snapshot)) return(FALSE)
  previous <- tryCatch(
    utils::read.csv(
      file.path(snapshot, "backup_manifest.csv"),
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character"
    ),
    error = function(error) NULL
  )
  required <- c("RelativePath", "Bytes", "SourceModifiedUTC", "MD5")
  if (is.null(previous) || !all(required %in% names(previous))) return(FALSE)
  previous <- previous[order(tolower(previous$RelativePath)), , drop = FALSE]
  current <- files[order(tolower(files$RelativePath)), , drop = FALSE]
  identical(tolower(previous$RelativePath), tolower(current$RelativePath)) &&
    identical(as.numeric(previous$Bytes), as.numeric(current$Bytes)) &&
    identical(previous$SourceModifiedUTC, current$SourceModifiedUTC)
}

build_plan <- function(folders, backup_root, exclusions, force = FALSE) {
  excluded <- list()
  no_idw <- list()
  unchanged <- list()
  eligible <- list()
  exclusion_keys <- tolower(exclusions)

  for (folder in folders) {
    if (tolower(folder$run_code) %in% exclusion_keys) {
      excluded[[length(excluded) + 1L]] <- folder
      next
    }
    files <- collect_idw_payload(folder)
    if (!nrow(files)) {
      no_idw[[length(no_idw) + 1L]] <- folder
      next
    }
    recovery <- classify_recovery(folder, files)
    run_backup_root <- file.path(backup_root, folder$name)
    latest <- find_latest_snapshot(run_backup_root)
    item <- list(
      folder = folder,
      files = files,
      recovery = recovery,
      bytes = sum(files$Bytes),
      run_backup_root = normalize_loose(run_backup_root),
      latest = latest
    )
    if (!force && snapshot_is_unchanged(files, latest)) {
      unchanged[[length(unchanged) + 1L]] <- item
    } else {
      eligible[[length(eligible) + 1L]] <- item
    }
  }
  list(
    excluded = excluded,
    no_idw = no_idw,
    unchanged = unchanged,
    eligible = eligible
  )
}

folder_names <- function(items) {
  if (!length(items)) return(character())
  vapply(items, function(item) {
    if (!is.null(item$folder)) item$folder$name else item$name
  }, character(1L))
}

print_plan <- function(plan, root, backup_root, exclusions, dry_run, force) {
  cat("MoFuSS IDW backup plan\n")
  cat("  Working-folder directory: ", root, "\n", sep = "")
  cat("  Backup directory:         ", backup_root, "\n", sep = "")
  cat("  Mode:                     ", if (dry_run) "DRY RUN" else "BACKUP", "\n", sep = "")
  cat("  MD5 verification:         required\n")
  cat("  Force unchanged snapshots:", if (force) " yes" else " no", "\n", sep = "")
  cat(
    "  Excluded RunCodes:        ",
    if (length(exclusions)) paste(exclusions, collapse = ", ") else "(none)",
    "\n",
    sep = ""
  )

  if (length(plan$eligible)) {
    cat("\nNew snapshots to create:\n")
    for (item in plan$eligible) {
      directional <- item$recovery$directional
      cat(
        "  - ", item$folder$name,
        " | ", item$recovery$level,
        " | ", nrow(item$files), " files",
        " | ", human_size(item$bytes),
        "\n",
        sep = ""
      )
      if (directional$expected > 0L && !directional$complete) {
        cat(
          "      directional outputs present: ", directional$present,
          "/", directional$expected, "\n", sep = ""
        )
      }
      if (nzchar(directional$warning)) {
        cat("      manifest warning: ", directional$warning, "\n", sep = "")
      }
    }
    cat(
      "  Total new data: ",
      human_size(sum(vapply(plan$eligible, `[[`, numeric(1L), "bytes"))),
      "\n",
      sep = ""
    )
  } else {
    cat("\nNew snapshots to create: (none)\n")
  }

  if (length(plan$unchanged)) {
    cat("\nUnchanged since latest verified snapshot; skipped:\n")
    for (item in plan$unchanged) {
      cat("  - ", item$folder$name, "\n", sep = "")
    }
  }
  if (length(plan$no_idw)) {
    cat("\nNo IDW output files found; skipped:\n")
    for (name in folder_names(plan$no_idw)) cat("  - ", name, "\n", sep = "")
  }
  if (length(plan$excluded)) {
    cat("\nExcluded before inspecting folder contents:\n")
    for (name in folder_names(plan$excluded)) cat("  - ", name, "\n", sep = "")
  }
  cat(
    paste0(
      "\nEach snapshot contains `working_folder_overlay`; its paths begin with ",
      "In/ and can be copied back into the exact matching working folder.\n",
      "Existing snapshots and working folders are never changed by this script.\n"
    )
  )
}

confirm_plan <- function() {
  answer <- trimws(tolower(readline(
    "Create and checksum the planned IDW backup snapshots? [y/N]: "
  )))
  answer %in% c("y", "yes")
}

safe_remove_staging <- function(path, backup_root) {
  if (!dir.exists(path)) return(invisible(TRUE))
  normalized <- normalize_existing(path, "Staging directory")
  if (!path_is_within(normalized, backup_root) ||
      !grepl("^\\.idw_tmp_[0-9]+$", basename(normalized))) {
    stopf("Refusing unexpected staging cleanup target: %s", normalized)
  }
  unlink(normalized, recursive = TRUE, force = TRUE)
  if (dir.exists(normalized)) stopf("Could not remove failed staging directory: %s", normalized)
  invisible(TRUE)
}

md5_file <- function(path) {
  value <- unname(as.character(tools::md5sum(path)))
  if (length(value) != 1L || is.na(value) || !grepl("^[0-9a-f]{32}$", value)) {
    stopf("Could not calculate MD5: %s", path)
  }
  value
}

write_restore_instructions <- function(path, item, snapshot_name) {
  level <- item$recovery$level
  next_step <- switch(
    level,
    READY_FOR_EGOML = paste0(
      "This snapshot contains the installed component layer. After a verified ",
      "restore into the unchanged matching run, you may proceed directly to EGOML."
    ),
    RAW_DIRECTIONAL_COMPLETE_FOR_STEP_9 = paste0(
      "All directional outputs expected by the IDW-ready manifest were present. ",
      "After restore, run 9_install_directional_IDW_outputs_v4.R, then EGOML."
    ),
    RAW_DIRECTIONAL_PARTIAL = paste0(
      "The directional output set was incomplete when backed up. Restore preserves ",
      "the completed pieces, but step 9 must not be run until missing jobs finish."
    ),
    STANDARD_IDW_OUTPUTS = paste0(
      "Paired top-level W/V IDW outputs were present. Restore them, then use the ",
      "same installation step used by this run before EGOML."
    ),
    "The snapshot contains only a partial IDW output set; do not treat it as run-ready."
  )
  directional <- item$recovery$directional
  lines <- c(
    paste0("MoFuSS IDW backup: ", item$folder$name),
    paste0("Snapshot: ", snapshot_name),
    paste0("Recovery level: ", level),
    "",
    "This is an IDW recovery overlay, not a complete MoFuSS working folder.",
    next_step,
    "",
    "Manual restore:",
    "1. Stop R, Dinamica EGO and every IDW process that uses the target folder.",
    paste0("2. Use only the exact matching working folder: ", item$folder$name),
    "3. Copy the CONTENTS of working_folder_overlay into that folder.",
    "4. Preserve the relative paths and approve replacement of the corresponding IDW files.",
    "5. Verify restored files against backup_manifest.csv before continuing.",
    "",
    paste0("Backed-up files: ", nrow(item$files)),
    paste0("Backed-up bytes: ", format(item$bytes, scientific = FALSE, trim = TRUE)),
    paste0(
      "Directional outputs present/expected: ",
      directional$present, "/", directional$expected
    ),
    "",
    "Important:",
    "- Do not restore into another scenario, RunCode, resolution or time horizon.",
    "- The overlay intentionally excludes ordinary MoFuSS inputs and simulation outputs.",
    "- Installer audit CSVs can contain historical absolute paths. Direct EGOML recovery uses the restored installed files, not those audit paths.",
    "- If the working-folder name changed after IDW preparation, rebase/regenerate absolute paths in HC_job_manifest_idw_ready.csv before rerunning step 9.",
    "- CostDistance_IDW -t/-e settings are not embedded in GeoTIFFs; logs are backed up when they are stored inside an idw_* directory or beside the HC manifests."
  )
  writeLines(lines, path, useBytes = TRUE)
}

copy_snapshot <- function(item, backup_root, batch_stamp) {
  run_backup_root <- item$run_backup_root
  dir.create(run_backup_root, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(run_backup_root)) {
    stopf("Could not create run backup directory: %s", run_backup_root)
  }
  run_backup_root <- normalize_existing(run_backup_root, "Run backup directory")
  if (!path_is_within(run_backup_root, backup_root)) {
    stopf("Refusing backup destination outside backup root: %s", run_backup_root)
  }

  snapshot_name <- batch_stamp
  final_dir <- file.path(run_backup_root, snapshot_name)
  if (dir.exists(final_dir) || file.exists(final_dir)) {
    snapshot_name <- paste0(batch_stamp, "_p", Sys.getpid())
    final_dir <- file.path(run_backup_root, snapshot_name)
  }
  if (dir.exists(final_dir) || file.exists(final_dir)) {
    stopf("Backup snapshot already exists: %s", final_dir)
  }
  stage_dir <- file.path(run_backup_root, paste0(".idw_tmp_", Sys.getpid()))
  if (dir.exists(stage_dir) || file.exists(stage_dir)) {
    stopf("Staging path already exists: %s", stage_dir)
  }
  if (!dir.create(stage_dir, recursive = FALSE)) {
    stopf("Could not create staging directory: %s", stage_dir)
  }
  completed <- FALSE
  on.exit({
    if (!completed && dir.exists(stage_dir)) {
      safe_remove_staging(stage_dir, backup_root)
    }
  }, add = TRUE)

  overlay_root <- file.path(stage_dir, "working_folder_overlay")
  if (!dir.create(overlay_root, recursive = TRUE)) {
    stopf("Could not create snapshot overlay: %s", overlay_root)
  }

  manifest_rows <- vector("list", nrow(item$files))
  for (index in seq_len(nrow(item$files))) {
    source <- item$files$SourcePath[[index]]
    relative <- item$files$RelativePath[[index]]
    destination <- file.path(overlay_root, relative)
    destination_parent <- dirname(destination)
    dir.create(destination_parent, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(destination_parent)) {
      stopf("Could not create backup subdirectory: %s", destination_parent)
    }

    before <- file.info(source)
    if (is.na(before$size) || before$isdir) {
      stopf("Source disappeared before backup: %s", source)
    }
    source_md5 <- md5_file(source)
    copied <- file.copy(
      from = source,
      to = destination,
      overwrite = FALSE,
      copy.mode = TRUE,
      copy.date = TRUE
    )
    if (!isTRUE(copied)) stopf("Could not copy: %s", source)
    after <- file.info(source)
    if (is.na(after$size) || before$size != after$size ||
        as.numeric(before$mtime) != as.numeric(after$mtime)) {
      stopf("Source changed while it was being backed up: %s", source)
    }
    destination_info <- file.info(destination)
    if (is.na(destination_info$size) || destination_info$size != before$size) {
      stopf("Backup size mismatch: %s", destination)
    }
    destination_md5 <- md5_file(destination)
    if (!identical(source_md5, destination_md5)) {
      stopf("Backup checksum mismatch: %s", destination)
    }

    manifest_rows[[index]] <- data.frame(
      RelativePath = relative,
      Category = item$files$Category[[index]],
      Bytes = as.numeric(before$size),
      SourceModifiedUTC = format_utc(before$mtime),
      MD5 = source_md5,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (index == 1L || index == nrow(item$files) || index %% 10L == 0L) {
      cat(
        "    verified ", index, "/", nrow(item$files),
        " files: ", relative, "\n", sep = ""
      )
    }
  }

  manifest <- do.call(rbind, manifest_rows)
  utils::write.csv(
    manifest,
    file.path(stage_dir, "backup_manifest.csv"),
    row.names = FALSE,
    quote = TRUE,
    na = ""
  )
  info <- data.frame(
    Field = c(
      "FormatVersion", "SourceWorkingFolder", "WorkingFolderName", "RunCode",
      "Resolution", "Scenario", "EndYear", "MonteCarloRuns", "Cap",
      "RecoveryLevel", "FileCount", "Bytes", "CreatedUTC"
    ),
    Value = c(
      "1", item$folder$path, item$folder$name, item$folder$run_code,
      item$folder$resolution, item$folder$scenario, item$folder$end_year,
      item$folder$monte_carlo_runs, item$folder$cap, item$recovery$level,
      nrow(item$files), format(item$bytes, scientific = FALSE, trim = TRUE),
      format(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  utils::write.csv(
    info,
    file.path(stage_dir, "backup_info.csv"),
    row.names = FALSE,
    quote = TRUE,
    na = ""
  )
  write_restore_instructions(
    file.path(stage_dir, "RESTORE_INSTRUCTIONS.txt"),
    item,
    snapshot_name
  )
  writeLines(
    c(
      paste0("Snapshot=", snapshot_name),
      paste0("WorkingFolder=", item$folder$name),
      paste0("FileCount=", nrow(item$files)),
      paste0("Bytes=", format(item$bytes, scientific = FALSE, trim = TRUE))
    ),
    file.path(stage_dir, ".complete"),
    useBytes = TRUE
  )

  if (!file.rename(stage_dir, final_dir)) {
    stopf("Could not finalize backup snapshot: %s", final_dir)
  }
  completed <- TRUE
  final_dir <- normalize_existing(final_dir, "Completed snapshot")

  pointer_temp <- file.path(
    run_backup_root,
    paste0(".LATEST_", Sys.getpid(), ".tmp")
  )
  writeLines(snapshot_name, pointer_temp, useBytes = TRUE)
  pointer <- file.path(run_backup_root, "LATEST.txt")
  pointer_ok <- file.copy(pointer_temp, pointer, overwrite = TRUE)
  unlink(pointer_temp, force = TRUE)
  if (!isTRUE(pointer_ok)) {
    warning("Snapshot is complete, but LATEST.txt could not be updated: ", pointer)
  }
  final_dir
}

write_batch_manifest <- function(results, backup_root, batch_stamp) {
  if (!length(results)) return(NULL)
  table <- do.call(rbind, lapply(results, function(result) {
    data.frame(
      WorkingFolder = result$item$folder$name,
      RunCode = result$item$folder$run_code,
      RecoveryLevel = result$item$recovery$level,
      FileCount = nrow(result$item$files),
      Bytes = result$item$bytes,
      Snapshot = result$path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }))
  path <- file.path(backup_root, paste0("backup_batch_", batch_stamp, ".csv"))
  if (file.exists(path)) {
    path <- file.path(
      backup_root,
      paste0("backup_batch_", batch_stamp, "_p", Sys.getpid(), ".csv")
    )
  }
  if (file.exists(path)) stopf("Batch manifest already exists: %s", path)
  utils::write.csv(table, path, row.names = FALSE, quote = TRUE, na = "")
  normalize_existing(path, "Batch manifest")
}

main <- function() {
  opts <- parse_options(commandArgs(trailingOnly = TRUE))
  script_path <- get_script_path()
  script_root <- dirname(script_path)
  root <- if (is.null(opts$root)) {
    normalize_existing(absolute_from(WORKING_FOLDERS_ROOT, script_root), "Working-folder directory")
  } else {
    normalize_existing(absolute_from(opts$root, script_root), "Working-folder directory")
  }
  exclusions <- if (is.null(opts$exclude)) {
    normalize_exclusions(EXCLUDE_RUN_CODES)
  } else if (!nzchar(opts$exclude)) {
    character()
  } else {
    normalize_exclusions(strsplit(opts$exclude, ",", fixed = TRUE)[[1L]])
  }
  backup_root <- if (is.null(opts$backup_root)) {
    absolute_from(if (is.null(BACKUP_ROOT)) BACKUP_DIRECTORY_NAME else BACKUP_ROOT, root)
  } else {
    absolute_from(opts$backup_root, root)
  }
  if (identical(path_key(backup_root), path_key(root))) {
    stopf("Backup directory cannot be the working-folder discovery directory itself.")
  }

  folders <- discover_working_folders(root)
  if (!length(folders)) {
    stopf("No MoFuSS working folders matched under WORKING_FOLDERS_ROOT: %s", root)
  }
  inside_folder <- vapply(
    folders,
    function(folder) path_is_within(backup_root, folder$path, allow_equal = TRUE),
    logical(1L)
  )
  if (any(inside_folder)) {
    stopf(
      "Backup directory must not be inside a working folder: %s",
      backup_root
    )
  }

  plan <- build_plan(
    folders = folders,
    backup_root = backup_root,
    exclusions = exclusions,
    force = opts$force
  )
  print_plan(plan, root, backup_root, exclusions, opts$dry_run, opts$force)
  if (opts$dry_run) {
    cat("\nDry run complete. Nothing was copied.\n")
    return(invisible(plan))
  }
  if (!length(plan$eligible)) {
    cat("\nNothing new to back up.\n")
    return(invisible(plan))
  }
  if (!opts$yes && !interactive()) {
    stopf("Review with --dry-run, then pass --yes to create backups in a non-interactive session.")
  }
  if (!opts$yes && !confirm_plan()) {
    cat("\nCancelled. Nothing was copied.\n")
    return(invisible(plan))
  }

  dir.create(backup_root, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(backup_root)) stopf("Could not create backup directory: %s", backup_root)
  backup_root <- normalize_existing(backup_root, "Backup directory")
  batch_stamp <- format(Sys.time(), format = "%Y%m%dT%H%M%SZ", tz = "UTC")
  results <- list()
  for (index in seq_along(plan$eligible)) {
    item <- plan$eligible[[index]]
    cat(
      "\n[", index, "/", length(plan$eligible), "] Backing up ",
      item$folder$name, "...\n", sep = ""
    )
    path <- copy_snapshot(item, backup_root, batch_stamp)
    results[[length(results) + 1L]] <- list(item = item, path = path)
    cat("  Complete: ", path, "\n", sep = "")
  }
  batch_manifest <- write_batch_manifest(results, backup_root, batch_stamp)

  cat("\nIDW backup completed successfully.\n")
  cat("  Snapshots created: ", length(results), "\n", sep = "")
  cat("  Batch manifest:    ", batch_manifest, "\n", sep = "")
  cat(
    "  Total verified:    ",
    human_size(sum(vapply(plan$eligible, `[[`, numeric(1L), "bytes"))),
    "\n",
    sep = ""
  )
  invisible(results)
}

if (!identical(Sys.getenv("MOFUSS_IDW_BACKUP_NO_AUTORUN"), "1")) main()
