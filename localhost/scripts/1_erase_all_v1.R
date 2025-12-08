# Copyright 2025 Stockholm Environment Institute ----
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# MoFuSS
# Version 4
# Date: Jan 2025

# 2dolist ----
# Select webmofuss == 1 automatically

# Internal parameters ----
# # Select MoFuSS platform:
# webmofuss = 1 # "1" is  web-MoFuSS running in our Ubuntu server, "0" is localhost (Windows or Linux)
# source(paste0(scriptsmofuss,"00_webmofuss.R"))

# Load libraries ----

# Detect OS
os <- Sys.info()["sysname"]

# Set working directory
setwd(countrydir)

# ------------------------------------------------------------
# Define base paths
# ------------------------------------------------------------
base_path <- file.path("LULCC", "DownloadedDatasets", paste0("SourceData", country_name))

# Use /demand100m/ if it exists, otherwise /demand/
demand_base <- if (dir.exists(file.path(base_path, "demand100m"))) {
  file.path(base_path, "demand100m")
} else {
  file.path(base_path, "demand")
}

# ------------------------------------------------------------
# Define directories to remove
# ------------------------------------------------------------
directories_to_remove <- c(
  "Debugging", "debug*", "norm*", 
  "HTML_animation_OutBaU", "HTML_animation_OutICS", 
  "Logs", "OutBaU", "OutICS", "Summary_Report", 
  "Temp", "In", "ffmpeg32", "ffmpeg64", "LaTeX", "rTemp", "demand_atlas",
  "LULCC/InVector", "LULCC/Out_lulcc", "LULCC/SourceData", 
  "LULCC/TempRaster", "LULCC/TempTables", "LULCC/TempVector", 
  "LULCC/TempVector_GCS", "LULCC/Wizard_imgs",
  "LULCC/lucdynamics_luc1", "LULCC/lucdynamics_luc2", "LULCC/lucdynamics_luc3",
  file.path(demand_base, "demand_out"),
  file.path(demand_base, "demand_temp"),
  file.path(demand_base, "pop_maps_byregion"),
  file.path(demand_base, "pop_out"),
  file.path(demand_base, "pop_temp"),
  file.path(demand_base, "to_idw"),
  file.path(base_path, "InTables"),
  file.path(base_path, "InVector"),
  file.path(base_path, "InVector_GCS")
)

# Expand wildcards and keep only existing paths
expanded_dirs <- unique(unlist(lapply(directories_to_remove, Sys.glob)))
existing_dirs <- expanded_dirs[file.exists(expanded_dirs)]

# Show what will be deleted
cat("Deleting the following directories:\n")
cat(paste(existing_dirs, collapse = "\n"), "\n")

# ------------------------------------------------------------
# Define file patterns to remove
# ------------------------------------------------------------
file_patterns_to_remove <- c(
  "*.Rout", "*.txt", "*.log", "*.aux", "*.lof",
  "*.lot", "*.out", "*.toc", "*.R", "*.egoml"
)

latex_patterns_to_remove <- c(
  "LaTeX//*.pdf", "LaTeX//*.mp4", "LaTeX//*.csv", 
  "LaTeX//SimLength.txt", "LaTeX//MCruns.txt"
)

# detect demand folder level for LULCC patterns too
lulcc_demand_path <- demand_base  # same auto-detected path

lulcc_patterns_to_remove <- c(
  "LULCC//*.Rout", "LULCC//*.csv", "LULCC//*.egoml", 
  "LULCC//*.bat", "LULCC//*.sh",
  file.path(lulcc_demand_path, "demand_in", "*.xlsx"),
  file.path(lulcc_demand_path, "demand_in", "*.csv"),
  file.path(lulcc_demand_path, "demand_in", "*.ods")
)

# ------------------------------------------------------------
# Remove directories and files
# ------------------------------------------------------------
lapply(existing_dirs, unlink, recursive = TRUE, force = TRUE)

lapply(file_patterns_to_remove, unlink, force = TRUE)
lapply(latex_patterns_to_remove, unlink, force = TRUE)
lapply(lulcc_patterns_to_remove, unlink, force = TRUE)

cat("✅ Cleanup completed successfully.\n")

# End of script ----


# End of script ----