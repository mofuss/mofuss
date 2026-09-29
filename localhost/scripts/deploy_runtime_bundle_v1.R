# Runtime-code deployment only. Safe to use on an existing working folder:
# no scenario parameters, inputs, MC draws, or generated results are removed.
# BEGIN USER INPUTS ----------------------------------------------------------
# Pass scripts_dir and destination to mofuss_copy_runtime_bundle().
# This helper has no standalone editable settings block.
# END USER INPUTS ------------------------------------------------------------

mofuss_runtime_bundle_files <- function() {
  c(
    "10_dyn_Sc17_webmofuss_ctrees_g_v13.egoml",
    "10_dyn_Sc17_webmofuss_ctrees_g_v13_linux.egoml",
    "rnorm_v8.R", "NRB_graphs_datasets_v8.R", "maps_animations_v8.R",
    "finalogs_v8.R", "bypassMC_v8.R", "bypass_maps_animations_v8.R",
    "run_linux.sh", "run_linux.py", "mofuss_r_linux.sh", "mofuss_linux_env.sh",
    "prepare_linux_inputs.py", "9_install_directional_IDW_outputs_v4.R",
    "README_LINUX.md", "LaTeX/generate_modern_report_v8.R"
  )
}

mofuss_validate_runtime_bundle <- function(scripts_dir) {
  files <- mofuss_runtime_bundle_files()
  sources <- file.path(scripts_dir, files)
  missing <- files[!file.exists(sources) | dir.exists(sources)]
  if (length(missing)) {
    stop("Incomplete Windows/Linux runtime bundle: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  invisible(sources)
}

mofuss_copy_runtime_bundle <- function(scripts_dir, destination) {
  sources <- mofuss_validate_runtime_bundle(scripts_dir)
  files <- mofuss_runtime_bundle_files()
  scripts_dir <- normalizePath(scripts_dir, winslash = "/", mustWork = TRUE)
  destination <- normalizePath(destination, winslash = "/", mustWork = TRUE)
  if (identical(scripts_dir, destination)) stop("Source and destination must differ.")
  targets <- file.path(destination, files)
  for (directory in unique(dirname(targets))) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }
  copied <- file.copy(sources, targets, overwrite = TRUE, copy.mode = TRUE)
  if (!all(copied)) stop("Could not copy: ", paste(files[!copied], collapse = ", "))
  if (!identical(unname(tools::md5sum(sources)), unname(tools::md5sum(targets)))) {
    stop("Runtime bundle verification failed after copying.")
  }
  shell_files <- targets[grepl("[.]sh$", targets)]
  if (.Platform$OS.type != "windows") {
    Sys.chmod(shell_files, mode = "0755")
    if (any(file.access(shell_files, mode = 1L) != 0L)) stop("Cannot make Linux launchers executable.")
  }
  message("Copied and verified ", length(files), " runtime files in ", destination)
  invisible(targets)
}
