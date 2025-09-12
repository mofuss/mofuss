# Copyright 2025 Stockholm Environment Institute ----

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
# webmofuss = 1 # "1" is  web-MoFuSS running in our Ubuntu server, "0" is localcal host (Windows or Linux)
# source(paste0(scriptsmofuss,"00_webmofuss.R"))

# Load libraries ----

# Detect OS
os <- Sys.info()["sysname"]

# Set working directory
setwd(countrydir)

# Define directories and patterns to clean ####
directories_to_remove <- c(
  "Debugging", "debug*", "norm*", 
  "HTML_animation_OutBaU", "HTML_animation_OutICS", 
  "Logs", "OutBaU", "OutICS", "Summary_Report", 
  "Temp", "In", "ffmpeg32", "ffmpeg64", "LaTeX", "rTemp", "demand_atlas",
  "LULCC/InVector", "LULCC/Out_lulcc", "LULCC/SourceData", 
  "LULCC/TempRaster", "LULCC/TempTables", "LULCC/TempVector", 
  "LULCC/TempVector_GCS", "LULCC/Wizard_imgs",
  "LULCC/lucdynamics_luc1", "LULCC/lucdynamics_luc2", "LULCC/lucdynamics_luc3"
)

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

# Remove directories
lapply(directories_to_remove, unlink, recursive = TRUE, force = TRUE)

# Remove files matching patterns
lapply(file_patterns_to_remove, unlink, force = TRUE)
lapply(latex_patterns_to_remove, unlink, force = TRUE)
lapply(lulcc_patterns_to_remove, unlink, force = TRUE)

# End of script ----