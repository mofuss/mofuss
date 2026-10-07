# SPDX-License-Identifier: Apache-2.0
# MoFuSS -- version 1, October 2026.
# Execution: sourced as the final step of the localhost preprocessing workflow.
# Purpose: create a double-click Windows launcher for each prepared run folder.
# Inputs: countrydir, country_parameters, and the canonical scripts directory.
# Outputs: RUN_MoFuSS.cmd and the copied model's LUC/MC wizard settings.
# Side effects: writes those code/configuration files; never starts simulations.

# BEGIN USER INPUTS ----------------------------------------------------------
# These are the same engine and processor settings used for the MDG runs.
# Set process environment variables before preprocessing to override paths.
windows_dinamica_console <- Sys.getenv(
  "MOFUSS_DINAMICA_CONSOLE", "C:/Program Files/Dinamica EGO/DinamicaConsole.exe")
windows_dinamica_processors <- 2L
windows_dinamica_temp_root <- Sys.getenv(
  "MOFUSS_WINDOWS_TEMP_ROOT", "E:/MoFuSS_Active/windows_runs")
# END USER INPUTS ------------------------------------------------------------

# 2dolist ----
# None.
# Internal parameters ----
if (.Platform$OS.type == "windows") {
  launcher_scripts_dir <- if (exists("runtime_scripts_dir", inherits = TRUE)) {
    runtime_scripts_dir
  } else if (exists("scriptsmofuss", inherits = TRUE)) {
    scriptsmofuss
  } else {
    stop("Source the preprocessing main script to set the canonical scripts directory.")
  }
  launcher_helpers <- new.env(parent = baseenv())
  sys.source(file.path(launcher_scripts_dir, "tools", "windows_launcher_v1.R"),
             envir = launcher_helpers)
  launcher_helpers$mofuss_write_windows_launcher(
    countrydir, country_parameters, engine = windows_dinamica_console,
    processors = windows_dinamica_processors, temp_root = windows_dinamica_temp_root)
} else {
  message("Windows launcher generation skipped on this operating system.")
}
