# SPDX-License-Identifier: Apache-2.0
#
# Adjust the Monte Carlo run count for every PRE-RUN MoFuSS working folder
# stored beside this script.
# BEGIN USER INPUTS ----------------------------------------------------------
#
# Edit this one value, save the script, and click Source in RStudio:
NEW_MONTE_CARLO_RUNS <- 3L
# RunCodes listed here are never inspected, renamed, or edited. Matching is
# case-insensitive and exact. Use character() to exclude nothing.
EXCLUDE_RUN_CODES <- c("GOG", "GLEA")
# END USER INPUTS ------------------------------------------------------------
#
# The script will show a complete plan and ask for confirmation before it:
#   1. renames each eligible folder's _mcN_ segment;
#   2. changes monte_carlo_runs in parameters.csv;
#   3. changes monte_carlo_runs in parameters_dinamica.csv; and
#   4. updates an optional pre-run bau_mc_source.txt sibling-folder reference.
#
# Folders with evidence that Dinamica has started are reported and skipped.
# Existing destination folders are never overwritten or merged.
#
# Optional command-line use:
#   Rscript batch_adj_mc_runs.R --runs=3 --dry-run
#   Rscript batch_adj_mc_runs.R --runs=3 --yes
#
# Optional command-line flags:
#   --runs=3       Override NEW_MONTE_CARLO_RUNS.
#   --exclude=GOG,GLEA
#                  Override EXCLUDE_RUN_CODES (use --exclude= for none).
#   --root="E:/"   Override sibling-folder discovery (mainly for testing).
#   --dry-run      Validate and print the plan without changing anything.
#   --yes          Skip the final confirmation prompt.

options(stringsAsFactors = FALSE)

WORKING_FOLDER_PATTERN <- paste0(
  "^(.+_[0-9]+m_(?:bau|ics)[0-9]+_[0-9]{4})_mc",
  "([0-9]+)_(capped|uncapped)$"
)

PARAMETERS_RELATIVE <- file.path(
  "LULCC", "DownloadedDatasets", "SourceDataGlobal", "parameters.csv"
)
PARAMETERS_DINAMICA_RELATIVE <- file.path(
  "LULCC", "TempTables", "parameters_dinamica.csv"
)
BAU_LINK_RELATIVES <- c(
  "bau_mc_source.txt",
  file.path("LULCC", "TempTables", "bau_mc_source.txt")
)

stopf <- function(fmt, ...) {
  stop(sprintf(fmt, ...), call. = FALSE)
}

normalize_existing <- function(path, label) {
  if (!length(path) || is.na(path) || !nzchar(trimws(path))) {
    stopf("%s was not provided.", label)
  }
  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!file.exists(normalized) && !dir.exists(normalized)) {
    stopf("%s does not exist: %s", label, normalized)
  }
  normalizePath(normalized, winslash = "/", mustWork = TRUE)
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

  fallback <- file.path(getwd(), "batch_adj_mc_runs.R")
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
    runs = NULL,
    exclude = NULL,
    root = NULL,
    dry_run = FALSE,
    yes = FALSE
  )
  for (arg in args) {
    if (identical(arg, "--dry-run")) {
      result$dry_run <- TRUE
    } else if (identical(arg, "--yes")) {
      result$yes <- TRUE
    } else if (startsWith(arg, "--runs=")) {
      result$runs <- sub("^--runs=", "", arg)
    } else if (startsWith(arg, "--exclude=")) {
      result$exclude <- sub("^--exclude=", "", arg)
    } else if (startsWith(arg, "--root=")) {
      result$root <- sub("^--root=", "", arg)
    } else {
      stopf("Unknown command-line option: %s", arg)
    }
  }
  result
}

positive_integer <- function(value, label) {
  text <- trimws(as.character(value))
  if (length(text) != 1L || is.na(text) || !grepl("^[0-9]+$", text)) {
    stopf("%s must be one positive integer; found '%s'.", label, text)
  }
  parsed <- suppressWarnings(as.integer(text))
  if (is.na(parsed) || parsed < 1L) {
    stopf("%s must be one positive integer; found '%s'.", label, text)
  }
  parsed
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
  if (length(fields) != 4L) return(NULL)
  group_match <- regexec(
    "^(.+_[0-9]+m)_(?:bau|ics)[0-9]+_([0-9]{4})_mc[0-9]+_(?:capped|uncapped)$",
    name,
    ignore.case = TRUE,
    perl = TRUE
  )
  group_fields <- regmatches(name, group_match)[[1L]]
  if (length(group_fields) != 3L) return(NULL)
  run_code <- sub(
    "_[0-9]+m$", "", group_fields[[2L]], ignore.case = TRUE, perl = TRUE
  )
  list(
    source = normalizePath(path, winslash = "/", mustWork = TRUE),
    old_name = name,
    name_prefix = fields[[2L]],
    old_mc = positive_integer(fields[[3L]], paste0("MC segment in ", name)),
    mode = fields[[4L]],
    run_code = run_code,
    group_key = paste(tolower(group_fields[[2L]]), group_fields[[3L]], sep = "|")
  )
}

discover_working_folders <- function(root) {
  children <- list.dirs(root, recursive = FALSE, full.names = TRUE)
  parsed <- lapply(children, parse_working_folder)
  parsed <- Filter(Negate(is.null), parsed)
  if (!length(parsed)) {
    stopf(
      paste0(
        "No MoFuSS working folders matching ",
        "<RunCode>_<resolution>_<scenario>_<year>_mc<N>_<mode> were found in %s."
      ),
      root
    )
  }
  parsed[order(tolower(vapply(parsed, `[[`, character(1L), "old_name")))]
}

detect_run_artifacts <- function(folder) {
  exact <- c(
    file.path(folder, "Temp", "mc_batch_ready.csv"),
    file.path(folder, "debug.txt"),
    file.path(folder, "log.txt")
  )
  artifacts <- exact[file.exists(exact)]

  root_files <- list.files(folder, recursive = FALSE, full.names = TRUE)
  root_info <- file.info(root_files)
  rout <- root_files[
    root_info$isdir %in% FALSE & grepl("\\.Rout$", basename(root_files), ignore.case = TRUE)
  ]
  artifacts <- c(artifacts, rout)

  root_dirs <- list.dirs(folder, recursive = FALSE, full.names = TRUE)
  debugging <- root_dirs[
    grepl("^debugging_[0-9]+$", basename(root_dirs), ignore.case = TRUE)
  ]
  unique(c(artifacts, debugging))
}

read_raw_file <- function(path) {
  size <- file.info(path)$size
  if (is.na(size)) stopf("Could not determine file size: %s", path)
  readBin(path, what = "raw", n = size)
}

line_layout <- function(path, original_raw = read_raw_file(path)) {
  bytes <- as.integer(original_raw)
  has_crlf <- length(bytes) >= 2L && any(
    bytes[-length(bytes)] == 13L & bytes[-1L] == 10L
  )
  final_eol <- length(bytes) > 0L && tail(bytes, 1L) == 10L
  list(
    lines = readLines(path, warn = FALSE, encoding = "UTF-8"),
    eol = if (has_crlf) "\r\n" else "\n",
    final_eol = final_eol
  )
}

render_lines <- function(lines, eol, final_eol) {
  text <- paste(lines, collapse = eol)
  if (final_eol) text <- paste0(text, eol)
  charToRaw(enc2utf8(text))
}

without_utf8_bom <- function(contents) {
  bom <- as.raw(c(0xEF, 0xBB, 0xBF))
  if (length(contents) >= 3L && identical(contents[1L:3L], bom)) {
    contents[-(1L:3L)]
  } else {
    contents
  }
}

read_mc_parameter <- function(path) {
  if (!file.exists(path)) stopf("Required parameter table is missing: %s", path)
  first <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
  separator <- if (length(first) && grepl(";", first, fixed = TRUE)) ";" else ","
  table <- tryCatch(
    utils::read.csv(
      path,
      sep = separator,
      check.names = FALSE,
      colClasses = "character",
      na.strings = NULL,
      blank.lines.skip = TRUE,
      fileEncoding = "UTF-8-BOM"
    ),
    error = function(error) {
      stopf("Could not read %s: %s", path, conditionMessage(error))
    }
  )
  names(table) <- sub("^\ufeff", "", names(table))
  if (!all(c("Var", "ParCHR") %in% names(table))) {
    stopf("Parameter table lacks Var/ParCHR columns: %s", path)
  }
  rows <- which(trimws(as.character(table$Var)) == "monte_carlo_runs")
  if (length(rows) != 1L) {
    stopf("Expected exactly one monte_carlo_runs row in: %s", path)
  }
  value <- positive_integer(
    table$ParCHR[[rows]],
    paste0("monte_carlo_runs in ", path)
  )
  list(value = value, separator = separator)
}

prepare_parameter_update <- function(path, expected_old, new_mc) {
  parsed <- read_mc_parameter(path)
  if (parsed$value != expected_old) {
    stopf(
      paste0(
        "Folder/table MC mismatch. Folder says mc%d but table says %d:\n  %s"
      ),
      expected_old,
      parsed$value,
      path
    )
  }

  original <- read_raw_file(path)
  layout <- line_layout(path, original)
  candidates <- which(grepl(
    "^[[:space:]]*\"?monte_carlo_runs\"?[[:space:]]*[,;]",
    layout$lines
  ))
  if (length(candidates) != 1L) {
    stopf("Could not isolate one monte_carlo_runs CSV record in: %s", path)
  }
  row <- candidates[[1L]]
  delimiter <- regexpr("[,;]", layout$lines[[row]])[[1L]]
  if (delimiter < 1L) stopf("Could not find the CSV delimiter in: %s", path)
  layout$lines[[row]] <- paste0(
    substr(layout$lines[[row]], 1L, delimiter),
    new_mc
  )
  updated <- render_lines(layout$lines, layout$eol, layout$final_eol)

  # Verify the prepared bytes before any real file is changed.
  preview <- tryCatch(
    utils::read.csv(
      text = rawToChar(without_utf8_bom(updated)),
      sep = parsed$separator,
      check.names = FALSE,
      colClasses = "character",
      na.strings = NULL,
      blank.lines.skip = TRUE
    ),
    error = function(error) {
      stopf("Prepared CSV failed validation for %s: %s", path, conditionMessage(error))
    }
  )
  names(preview) <- sub("^\ufeff", "", names(preview))
  preview_rows <- which(trimws(as.character(preview$Var)) == "monte_carlo_runs")
  if (length(preview_rows) != 1L ||
      positive_integer(preview$ParCHR[[preview_rows]], "Prepared MC value") != new_mc) {
    stopf("Prepared CSV has the wrong Monte Carlo value: %s", path)
  }

  list(
    relative = NULL,
    original_raw = original,
    updated_raw = updated,
    separator = parsed$separator
  )
}

prepare_link_update <- function(path, mappings, relative) {
  original <- read_raw_file(path)
  layout <- line_layout(path, original)
  active <- trimws(layout$lines)
  active <- active[nzchar(active) & !startsWith(active, "#")]
  if (length(active) != 1L) {
    stopf("%s must contain exactly one non-comment BAU path.", path)
  }

  updated_lines <- layout$lines
  for (index in seq_len(nrow(mappings))) {
    updated_lines <- gsub(
      mappings$old_name[[index]],
      mappings$new_name[[index]],
      updated_lines,
      fixed = TRUE
    )
  }
  if (identical(updated_lines, layout$lines)) return(NULL)
  list(
    relative = relative,
    original_raw = original,
    updated_raw = render_lines(updated_lines, layout$eol, layout$final_eol)
  )
}

make_plan <- function(root, new_mc, exclude_run_codes = character()) {
  folders <- discover_working_folders(root)
  excluded <- list()
  skipped <- list()
  not_ready <- list()
  blocked_groups <- list()
  eligible <- list()

  for (folder in folders) {
    if (tolower(folder$run_code) %in% tolower(exclude_run_codes)) {
      excluded[[length(excluded) + 1L]] <- folder
      next
    }
    artifacts <- detect_run_artifacts(folder$source)
    if (length(artifacts)) {
      folder$artifacts <- artifacts
      skipped[[length(skipped) + 1L]] <- folder
      next
    }

    folder$new_name <- paste0(
      folder$name_prefix, "_mc", new_mc, "_", folder$mode
    )
    folder$destination <- file.path(root, folder$new_name)
    folder$changed <- !identical(folder$old_name, folder$new_name)

    parameter_paths <- c(
      file.path(folder$source, PARAMETERS_RELATIVE),
      file.path(folder$source, PARAMETERS_DINAMICA_RELATIVE)
    )
    missing <- parameter_paths[!file.exists(parameter_paths)]
    if (length(missing)) {
      folder$missing <- missing
      not_ready[[length(not_ready) + 1L]] <- folder
      next
    }

    # Validate even when this folder already has the target mcN name.
    existing_values <- vapply(
      parameter_paths,
      function(path) read_mc_parameter(path)$value,
      integer(1L)
    )
    if (any(existing_values != folder$old_mc)) {
      stopf(
        paste0(
          "Folder/table MC mismatch in %s. Folder says mc%d; tables say %s."
        ),
        folder$source,
        folder$old_mc,
        paste(existing_values, collapse = ", ")
      )
    }

    folder$table_updates <- list()
    if (folder$changed) {
      for (index in seq_along(parameter_paths)) {
        update <- prepare_parameter_update(
          parameter_paths[[index]], folder$old_mc, new_mc
        )
        update$relative <- c(
          PARAMETERS_RELATIVE,
          PARAMETERS_DINAMICA_RELATIVE
        )[[index]]
        folder$table_updates[[index]] <- update
      }
    }
    eligible[[length(eligible) + 1L]] <- folder
  }

  # Do not split a related region/resolution/year set. If one sibling has
  # started Dinamica or is not prepared, leave every sibling in that set alone.
  ineligible_groups <- unique(c(
    vapply(skipped, function(folder) folder$group_key, character(1L)),
    vapply(not_ready, function(folder) folder$group_key, character(1L))
  ))
  if (length(eligible) && length(ineligible_groups)) {
    blocked_index <- which(vapply(
      eligible,
      function(folder) folder$group_key %in% ineligible_groups,
      logical(1L)
    ))
    if (length(blocked_index)) {
      blocked_groups <- eligible[blocked_index]
      eligible <- eligible[-blocked_index]
    }
  }

  changed <- Filter(function(folder) isTRUE(folder$changed), eligible)
  if (length(changed)) {
    destinations <- vapply(changed, `[[`, character(1L), "destination")
    if (anyDuplicated(tolower(normalizePath(
      destinations, winslash = "/", mustWork = FALSE
    )))) {
      stopf("Two or more working folders would map to the same destination name.")
    }
    existing <- destinations[file.exists(destinations) | dir.exists(destinations)]
    if (length(existing)) {
      stopf(
        paste0(
          "Refusing to overwrite or merge with existing destination folder(s):\n  %s"
        ),
        paste(existing, collapse = "\n  ")
      )
    }
  }

  mappings <- if (length(changed)) {
    data.frame(
      old_name = vapply(changed, `[[`, character(1L), "old_name"),
      new_name = vapply(changed, `[[`, character(1L), "new_name"),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(old_name = character(), new_name = character())
  }

  links <- list()
  if (nrow(mappings)) {
    for (owner_index in seq_along(eligible)) {
      owner <- eligible[[owner_index]]
      for (relative in BAU_LINK_RELATIVES) {
        link_path <- file.path(owner$source, relative)
        if (!file.exists(link_path)) next
        update <- prepare_link_update(link_path, mappings, relative)
        if (is.null(update)) next
        update$owner_index <- owner_index
        links[[length(links) + 1L]] <- update
      }
    }
  }

  list(
    root = root,
    new_mc = new_mc,
    exclude_run_codes = exclude_run_codes,
    eligible = eligible,
    excluded = excluded,
    skipped = skipped,
    not_ready = not_ready,
    blocked_groups = blocked_groups,
    mappings = mappings,
    links = links
  )
}

print_plan <- function(plan) {
  cat("\nMoFuSS Monte Carlo adjustment plan\n")
  cat("  Working-folder root: ", plan$root, "\n", sep = "")
  cat("  New Monte Carlo runs: ", plan$new_mc, "\n", sep = "")
  cat(
    "  Excluded RunCodes: ",
    if (length(plan$exclude_run_codes)) {
      paste(plan$exclude_run_codes, collapse = ", ")
    } else {
      "(none)"
    },
    "\n",
    sep = ""
  )

  changed <- Filter(function(folder) isTRUE(folder$changed), plan$eligible)
  unchanged <- Filter(function(folder) !isTRUE(folder$changed), plan$eligible)

  cat("\nEligible folder changes:\n")
  if (!length(changed)) {
    cat("  (none)\n")
  } else {
    for (folder in changed) {
      cat("  ", folder$old_name, "\n", sep = "")
      cat("    -> ", folder$new_name, "\n", sep = "")
      cat(
        "    parameters.csv: ", folder$old_mc, " -> ", plan$new_mc, "\n",
        sep = ""
      )
      cat(
        "    parameters_dinamica.csv: ", folder$old_mc, " -> ",
        plan$new_mc, "\n", sep = ""
      )
    }
  }

  if (length(plan$links)) {
    cat("\nPre-run BAU link files to update:\n")
    for (link in plan$links) {
      owner <- plan$eligible[[link$owner_index]]
      cat("  ", file.path(owner$old_name, link$relative), "\n", sep = "")
    }
  }

  if (length(plan$excluded)) {
    cat("\nExcluded by RunCode (not inspected or changed):\n")
    cat(
      paste0(
        "  ", vapply(plan$excluded, `[[`, character(1L), "old_name"),
        collapse = "\n"
      ),
      "\n"
    )
  }

  if (length(unchanged)) {
    cat("\nAlready at the requested count (validated; no change):\n")
    cat(
      paste0(
        "  ", vapply(unchanged, `[[`, character(1L), "old_name"),
        collapse = "\n"
      ),
      "\n"
    )
  }

  if (length(plan$skipped)) {
    cat("\nSkipped because Dinamica has already started:\n")
    for (folder in plan$skipped) {
      cat("  ", folder$old_name, "\n", sep = "")
      cat(
        "    evidence: ",
        summarize_artifacts(folder$artifacts),
        "\n",
        sep = ""
      )
    }
  }

  if (length(plan$not_ready)) {
    cat("\nSkipped because preparation is incomplete:\n")
    for (folder in plan$not_ready) {
      cat("  ", folder$old_name, "\n", sep = "")
      cat(
        "    missing: ",
        paste(vapply(folder$missing, basename, character(1L)), collapse = ", "),
        "\n",
        sep = ""
      )
    }
    cat("  Run normal MoFuSS preparation through parameters_dinamica.csv first.\n")
  }


  if (length(plan$blocked_groups)) {
    cat("\nSkipped to keep each BAU/ICS region group consistent:\n")
    for (folder in plan$blocked_groups) {
      cat("  ", folder$old_name, "\n", sep = "")
      cat("    a related sibling is run-started or not fully prepared\n")
    }
  }

  cat("\nNo existing destination folder will be overwritten or merged.\n")
}

confirm_plan <- function() {
  answer <- trimws(tolower(readline(
    "Rename the eligible folders and update their files? [y/N]: "
  )))
  answer %in% c("y", "yes")
}

summarize_artifacts <- function(paths, limit = 6L) {
  labels <- unique(basename(paths))
  shown <- head(labels, limit)
  suffix <- if (length(labels) > limit) {
    sprintf(" (+%d more)", length(labels) - limit)
  } else {
    ""
  }
  paste0(paste(shown, collapse = ", "), suffix)
}

write_raw_file <- function(path, contents) {
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(contents, connection)
  invisible(path)
}

current_root <- function(folder) {
  if (isTRUE(folder$renamed)) folder$destination else folder$source
}

verify_applied_plan <- function(plan) {
  for (folder in plan$eligible) {
    root <- if (isTRUE(folder$changed)) folder$destination else folder$source
    if (!dir.exists(root)) stopf("Expected renamed folder is missing: %s", root)
    values <- vapply(
      c(
        file.path(root, PARAMETERS_RELATIVE),
        file.path(root, PARAMETERS_DINAMICA_RELATIVE)
      ),
      function(path) read_mc_parameter(path)$value,
      integer(1L)
    )
    if (any(values != plan$new_mc)) {
      stopf("Post-write parameter verification failed in: %s", root)
    }
  }

  if (nrow(plan$mappings) && length(plan$links)) {
    for (link in plan$links) {
      owner <- plan$eligible[[link$owner_index]]
      owner_root <- if (isTRUE(owner$changed)) owner$destination else owner$source
      path <- file.path(owner_root, link$relative)
      text <- paste(readLines(path, warn = FALSE), collapse = "\n")
      stale <- plan$mappings$old_name[
        vapply(plan$mappings$old_name, grepl, logical(1L), x = text, fixed = TRUE)
      ]
      if (length(stale)) {
        stopf("Stale BAU folder reference remains in: %s", path)
      }
    }
  }
  invisible(TRUE)
}

apply_plan <- function(plan) {
  changed_indices <- which(vapply(
    plan$eligible,
    function(folder) isTRUE(folder$changed),
    logical(1L)
  ))
  renamed_indices <- integer()
  written_tables <- list()
  written_links <- list()

  result <- tryCatch({
    # Rename all directories first. All target collisions were rejected during
    # preflight, so this does not merge folder contents.
    for (index in changed_indices) {
      folder <- plan$eligible[[index]]
      if (!file.rename(folder$source, folder$destination)) {
        stopf("Could not rename folder:\n  %s\n  -> %s", folder$source, folder$destination)
      }
      plan$eligible[[index]]$renamed <- TRUE
      renamed_indices <- c(renamed_indices, index)
    }

    for (index in changed_indices) {
      folder <- plan$eligible[[index]]
      for (update in folder$table_updates) {
        path <- file.path(folder$destination, update$relative)
        written_tables[[length(written_tables) + 1L]] <- list(
          owner_index = index,
          relative = update$relative,
          original_raw = update$original_raw
        )
        write_raw_file(path, update$updated_raw)
      }
    }

    for (link in plan$links) {
      owner <- plan$eligible[[link$owner_index]]
      owner_root <- if (isTRUE(owner$changed)) owner$destination else owner$source
      path <- file.path(owner_root, link$relative)
      written_links[[length(written_links) + 1L]] <- link
      write_raw_file(path, link$updated_raw)
    }

    verify_applied_plan(plan)
    TRUE
  }, error = function(error) {
    rollback_errors <- character()

    # Restore changed link files while their owning folders still have their
    # current names.
    for (link in rev(written_links)) {
      owner <- plan$eligible[[link$owner_index]]
      owner_root <- if (link$owner_index %in% renamed_indices) {
        owner$destination
      } else {
        owner$source
      }
      path <- file.path(owner_root, link$relative)
      restored <- tryCatch({
        write_raw_file(path, link$original_raw)
        TRUE
      }, error = function(inner) {
        rollback_errors <<- c(rollback_errors, conditionMessage(inner))
        FALSE
      })
      invisible(restored)
    }

    for (update in rev(written_tables)) {
      owner <- plan$eligible[[update$owner_index]]
      owner_root <- if (update$owner_index %in% renamed_indices) {
        owner$destination
      } else {
        owner$source
      }
      path <- file.path(owner_root, update$relative)
      restored <- tryCatch({
        write_raw_file(path, update$original_raw)
        TRUE
      }, error = function(inner) {
        rollback_errors <<- c(rollback_errors, conditionMessage(inner))
        FALSE
      })
      invisible(restored)
    }

    for (index in rev(renamed_indices)) {
      folder <- plan$eligible[[index]]
      if (!file.rename(folder$destination, folder$source)) {
        rollback_errors <- c(
          rollback_errors,
          sprintf("Could not restore folder name: %s", folder$destination)
        )
      }
    }

    suffix <- if (length(rollback_errors)) {
      paste0(
        "\nRollback also reported:\n  ",
        paste(unique(rollback_errors), collapse = "\n  ")
      )
    } else {
      "\nAll changes made by this attempt were rolled back."
    }
    stopf("Batch adjustment failed: %s%s", conditionMessage(error), suffix)
  })
  invisible(result)
}

main <- function() {
  opts <- parse_options(commandArgs(trailingOnly = TRUE))
  script_path <- get_script_path()
  script_dir <- dirname(script_path)
  root <- if (is.null(opts$root)) {
    script_dir
  } else {
    normalize_existing(opts$root, "Working-folder root")
  }
  if (!dir.exists(root)) stopf("Working-folder root is not a directory: %s", root)

  requested <- if (is.null(opts$runs)) NEW_MONTE_CARLO_RUNS else opts$runs
  new_mc <- positive_integer(requested, "NEW_MONTE_CARLO_RUNS")
  excluded <- if (is.null(opts$exclude)) {
    normalize_exclusions(EXCLUDE_RUN_CODES)
  } else if (!nzchar(opts$exclude)) {
    character()
  } else {
    normalize_exclusions(strsplit(opts$exclude, ",", fixed = TRUE)[[1L]])
  }
  plan <- make_plan(root, new_mc, excluded)
  print_plan(plan)

  if (!length(plan$eligible)) {
    cat("\nNo pre-run working folders are eligible. No changes were made.\n")
    return(invisible(plan))
  }

  changed <- Filter(function(folder) isTRUE(folder$changed), plan$eligible)
  if (!length(changed) && !length(plan$links)) {
    cat("\nEverything eligible is already consistent. No changes are needed.\n")
    return(invisible(plan))
  }
  if (opts$dry_run) {
    cat("\nDry run complete. No folders or files were changed.\n")
    return(invisible(plan))
  }
  if (!opts$yes && !confirm_plan()) {
    cat("\nCancelled. No folders or files were changed.\n")
    return(invisible(NULL))
  }

  apply_plan(plan)
  cat("\nBatch adjustment completed and verified.\n")
  invisible(plan)
}

tryCatch(main(), error = function(error) {
  stop(paste0("\nERROR: ", conditionMessage(error)), call. = FALSE)
})
