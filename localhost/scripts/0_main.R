# MoFuSS
# Version 1
# Date: Jan 2025

# 2dolist ----

# Internal parameters ----
runGADM = 0

# Load libraries ----
library(rprojroot)
library(rstudioapi)

# Determine the script directory
if (interactive() && rstudioapi::isAvailable()) {
  # If in RStudio
  script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  # If running as an Rscript
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_dir <- dirname(normalizePath(sub(file_arg, "", args[grep(file_arg, args)])))
  
  # Fall back to current directory if not found
  if (length(script_dir) == 0) {
    script_dir <- getwd()
  }
}

scriptsmofuss <- script_dir

# Sourcing files
source(file.path(script_dir, "00_webmofuss.R"))
source(file.path(scriptsmofuss, "0_set_directories_and_region_v3.R"))
if (runGADM == 1) {
  source(file.path(scriptsmofuss, "preprocessing4globaldatasets/1apre_GADM_admin_wp_v5.R"))
}
source(file.path(scriptsmofuss, "1_erase_all_v1.R"))
source(file.path(scriptsmofuss, "2_copy_files_v1.R"))
source(paste0(scriptsmofuss,"/1_erase_all_v1.R"))
source(paste0(scriptsmofuss,"/2_copy_files_v1.R"))
# source(paste0(scriptsmofuss,"/3_demand4IDW_v3.R"))
# source(paste0(scriptsmofuss,"/4_produce_growth_and_stock_csv.R"))
# source(paste0(scriptsmofuss,"/5_harmonizer_v1.R"))
# source(paste0(scriptsmofuss,"/6_scenarios.R"))

