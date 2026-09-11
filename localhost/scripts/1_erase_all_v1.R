# SPDX-License-Identifier: Apache-2.0
#
# Copyright 2025-2027 Universidad Nacional Autónoma de México
# and Stockholm Environment Institute
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# https://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# MoFuSS ----
# Script: 1_erase_all_v1.R
# Version: 1
# Date: Apr 2026
# Execution: Source from RStudio; Dinamica EGO does not invoke this script directly.
#
# Purpose: Remove outputs and temporary artifacts from a previously prepared
# country workspace so the main workflow can rebuild them.
# Inputs: The inherited countrydir and its expected MoFuSS directory structure.
# Outputs: A cleaned workspace; no analytical products are created.
# Side effects: Destructively deletes configured output directories and file patterns.

# 2dolist ----

# Internal parameters ----

# Load libraries ----

# Detect OS
os <- Sys.info()["sysname"]

# Set working directory
setwd(countrydir)

# Define base paths----
base_path <- file.path("LULCC", "DownloadedDatasets", paste0("SourceData", country_name))

# Use /demand100m/ if it exists, otherwise /demand/
demand_base <- if (dir.exists(file.path(base_path, "demand100m"))) {
  file.path(base_path, "demand100m")
} else {
  file.path(base_path, "demand")
}
demand_input_dir <- file.path(demand_base, "demand_in")

.clean_demand_input_preserving_wp <- function(path) {
  if (!dir.exists(path)) {
    return(invisible(character()))
  }

  entries <- list.files(
    path,
    all.files = TRUE,
    full.names = TRUE,
    no.. = TRUE
  )
  entries_to_remove <- entries[!startsWith(basename(entries), "wp_")]

  if (length(entries_to_remove) > 0L) {
    cat(
      "Deleting demand-input entries (preserving names beginning with wp_):\n",
      paste(entries_to_remove, collapse = "\n"),
      "\n",
      sep = ""
    )
    invisible(lapply(
      entries_to_remove,
      unlink,
      recursive = TRUE,
      force = TRUE
    ))
  }

  failed_removals <- entries_to_remove[file.exists(entries_to_remove)]
  if (length(failed_removals) > 0L) {
    stop(
      "Could not remove demand-input entry or entries: ",
      paste(failed_removals, collapse = ", ")
    )
  }

  invisible(entries_to_remove)
}

# Define directories to remove ----
directories_to_remove <- c(
  "HTML_animation_OutBaU", "HTML_animation_OutICS", "OutBaU", "OutICS", "HTML_animation_Out",
  "Debugging", "debug*", "norm*", 
  "HTML_animation", "Logs", "Out", "Summary_Report",
  "Temp", "In", "ffmpeg32", "ffmpeg64", "LaTeX", "rTemp", "demand_atlas",
  "LULCC/InVector", "LULCC/Out_lulcc", "LULCC/SourceData", 
  "LULCC/TempRaster", "LULCC/TempTables", "LULCC/TempVector", 
  "LULCC/TempVector_GCS", "LULCC/Wizard_imgs",
  "LULCC/lucdynamics_luc1", "LULCC/lucdynamics_luc2", "LULCC/lucdynamics_luc3",
  "agb_g_h_curves",
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

## Define file patterns to remove ----
file_patterns_to_remove <- c(
  "*.Rout", "*.txt", "*.log", "*.aux", "*.lof",
  "*.lot", "*.out", "*.toc", "*.R", "*.egoml"
)

latex_patterns_to_remove <- c(
  "LaTeX//*.pdf", "LaTeX//*.mp4", "LaTeX//*.csv", 
  "LaTeX//SimLength.txt", "LaTeX//MCruns.txt"
)

lulcc_patterns_to_remove <- c(
  "LULCC//*.Rout", "LULCC//*.csv", "LULCC//*.egoml", 
  "LULCC//*.bat", "LULCC//*.sh"
)

# Remove directories and files ----
lapply(existing_dirs, unlink, recursive = TRUE, force = TRUE)

lapply(file_patterns_to_remove, unlink, force = TRUE)
lapply(latex_patterns_to_remove, unlink, force = TRUE)
lapply(lulcc_patterns_to_remove, unlink, force = TRUE)
.clean_demand_input_preserving_wp(demand_input_dir)

cat("✅ Cleanup completed successfully.\n")

# End of script ----
