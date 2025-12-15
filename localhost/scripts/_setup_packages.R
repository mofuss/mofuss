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
# Version 1
# Date: Dec 2025
# Purpose: Install + load a common package superset used across legacy scripts.

options(repos = c(CRAN = "https://cloud.r-project.org"))

is_windows <- tolower(Sys.info()[["sysname"]]) == "windows"
if (is_windows) {
  options(pkgType = "binary")
  options(install.packages.check.source = "no")
}

# ---- Superset of packages found in legacy headers (deduplicated) ----
PKGS <- unique(c(
  pkgs <- unique(c(
    # tidy / data
    "tidyverse","dplyr","readr","readxl","tibble","purrr","stringr","tidyr",
    "data.table","plyr","glue","knitr","rmarkdown","tools","fs",
    
    # spatial / rasters / mapping
    "terra","raster","rasterVis","sf","sp","stars","fasterize","tmap","classInt",
    "rmapshaper","mapview","maptools",   # maptools is retired; see note below
    
    # stats / misc
    "msm","fBasics","spam","fields","igraph","foreach","tictoc",
    
    # UI / dialogs
    "svDialogs","tcltk","htmltools",
    
    # IO / images
    "png","jpeg","tiff",
    
    # utilities you had
    "animation","inline","caTools","bitops","gdata",
    
    # APIs
    "osmdata", "telegram.bot", "rstudioapi",
    
    "openxlsx", "readODS"
))))

# These are retired/legacy and often break installs; only enable if you *must*.
LEGACY_RETIRED <- c("rgdal","rgeos","maptools")

install_and_load <- function(pkgs, include_retired = FALSE) {
  pkgs <- unique(pkgs)
  if (!include_retired) pkgs <- setdiff(pkgs, LEGACY_RETIRED)
  
  installed <- rownames(installed.packages())
  to_install <- setdiff(pkgs, installed)
  
  message("=== Package bootstrap ===")
  message("Packages requested: ", length(pkgs))
  if (length(to_install) > 0) {
    message("Installing missing packages:\n  - ", paste(to_install, collapse = ", "))
    install.packages(to_install, dependencies = TRUE)
  } else {
    message("All requested packages are already installed.")
  }
  
  failed <- character(0)
  for (p in pkgs) {
    ok <- suppressWarnings(suppressMessages(require(p, character.only = TRUE)))
    if (!ok) failed <- c(failed, p)
  }
  
  if (length(failed) == 0) {
    message("✅ All packages loaded successfully.")
  } else {
    message("⚠️ Failed to load:\n  - ", paste(failed, collapse = ", "))
    if (any(failed %in% LEGACY_RETIRED)) {
      message("Note: rgdal/rgeos/maptools are retired. Prefer sf/terra where possible.")
    }
  }
  
  invisible(list(loaded = setdiff(pkgs, failed), failed = failed))
}

# Run on source()
install_and_load(PKGS, include_retired = TRUE)




