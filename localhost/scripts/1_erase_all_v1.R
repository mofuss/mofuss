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

# List all files and directories inside the folder
contents <- list.files(rTempdir, full.names = TRUE, recursive = TRUE)
# Delete the contents but keep the folder
unlink(contents, recursive = TRUE, force = TRUE)

# Detect OS
os <- Sys.info()["sysname"]

# Set working directory
setwd(countrydir)

# Define directories and patterns to clean ####
directories_to_remove <- c(
  "Debugging", "debug*", "norm*", 
  "HTML_animation_OutBaU", "HTML_animation_OutICS", 
  "Logs", "OutBaU", "OutICS", "Summary_Report", 
  "Temp", "In", "ffmpeg32", "ffmpeg64", "LaTeX",
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