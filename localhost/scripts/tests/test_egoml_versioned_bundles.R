repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
scripts_dir <- file.path(repo_root, "localhost", "scripts")

bundles <- list(
  v7 = list(
    egoml = "7_dyn_Sc17_webmofuss_ctrees_g_v7.egoml",
    direct = c(
      "bypass_maps_animations_v7.R", "bypassMC_v7.R", "finalogs_v7.R",
      "maps_animations_v7.R", "NRB_graphs_datasets_v7.R", "rnorm_v7.R"
    ),
    map_script = "maps_animations_v7.R",
    report = "LaTeX/generate_modern_report_v7.R"
  ),
  v8 = list(
    egoml = "7_dyn_Sc17_webmofuss_ctrees_g_v8.egoml",
    direct = c(
      "bypass_maps_animations_v8.R", "bypassMC_v8.R", "finalogs_v8.R",
      "maps_animations_v8.R", "NRB_graphs_datasets_v8.R", "rnorm_v8.R"
    ),
    map_script = "maps_animations_v8.R",
    report = "LaTeX/generate_modern_report_v8.R"
  )
)

extract_r_tokens <- function(path) {
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  hits <- regmatches(text, gregexpr("[A-Za-z0-9_./\\\\-]+[.]R\\b", text))[[1L]]
  sort(unique(basename(hits[nzchar(hits)])))
}

all_dependency_paths <- character()
for (version in names(bundles)) {
  bundle <- bundles[[version]]
  egoml_path <- file.path(scripts_dir, bundle$egoml)
  stopifnot(file.exists(egoml_path))

  observed <- extract_r_tokens(egoml_path)
  expected <- sort(bundle$direct)
  if (!identical(observed, expected)) {
    stop(
      toupper(version), " EGOML dependency mismatch. Expected: ",
      paste(expected, collapse = ", "), "; observed: ",
      paste(observed, collapse = ", ")
    )
  }

  direct_paths <- file.path(scripts_dir, bundle$direct)
  report_path <- file.path(scripts_dir, bundle$report)
  missing <- c(direct_paths, report_path)[!file.exists(c(direct_paths, report_path))]
  if (length(missing)) stop("Missing bundle dependency: ", missing[[1L]])

  map_text <- paste(
    readLines(file.path(scripts_dir, bundle$map_script), warn = FALSE),
    collapse = "\n"
  )
  expected_report <- basename(bundle$report)
  if (!grepl(expected_report, map_text, fixed = TRUE)) {
    stop(bundle$map_script, " does not source ", expected_report)
  }

  invisible(lapply(c(direct_paths, report_path), parse))
  all_dependency_paths <- c(all_dependency_paths, direct_paths, report_path)
}

if (length(intersect(bundles$v7$direct, bundles$v8$direct)) != 0L) {
  stop("V7 and V8 unexpectedly share a direct R dependency filename.")
}

copy_script <- file.path(scripts_dir, "2_copy_files_v1.R")
invisible(parse(copy_script))
copy_text <- paste(readLines(copy_script, warn = FALSE), collapse = "\n")
required_deployment_names <- c(
  bundles$v7$egoml, bundles$v8$egoml,
  bundles$v7$direct, bundles$v8$direct,
  bundles$v7$report, bundles$v8$report
)
missing_from_copy_script <- required_deployment_names[
  !vapply(required_deployment_names, grepl, logical(1), x = copy_text, fixed = TRUE)
]
if (length(missing_from_copy_script)) {
  stop(
    "2_copy_files_v1.R omits: ",
    paste(missing_from_copy_script, collapse = ", ")
  )
}

cat("EGOML_V7_V8_VERSIONED_BUNDLES_OK\n")
