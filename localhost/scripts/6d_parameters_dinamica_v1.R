# Copyright 2027 Stockholm Environment Institute ----

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
# Date: Jul 2026

# 2dolist ----

# Internal parameters ----

# Load libraries ----
library(conflicted)

library(dplyr)
library(readr)

# Detect OS
os <- Sys.info()["sysname"]

setwd(countrydir)
getwd()

# Read parameters table ----
if (webmofuss == 1) {
  # Read parameters table in webmofuss
  country_parameters <- read_csv(parameters_file_path)
} else if(webmofuss == 0) {
  # Read parameters table (recognizing the delimiter)
  detect_delimiter <- function(file_path) {
    # Read the first line of the file
    first_line <- readLines(file_path, n = 1)
    # Check if the first line contains ',' or ';'
    if (grepl(";", first_line)) {
      return(";")
    } else {
      return(",")
    }
  }
  # Detect the delimiter
  delimiter <- detect_delimiter(parameters_file_path)
  # Read the CSV file with the detected delimiter
  country_parameters <- read_delim(parameters_file_path, delim = delimiter)
  print(tibble::as_tibble(country_parameters), n=100)
}

# country_parameters %>%
#   dplyr::filter(Var == "proj_gcs") %>%
#   pull(ParCHR) -> proj_gcs #GCSproj

country_parameters %>%
  dplyr::filter(Var == "start_year") %>%
  pull(ParCHR) %>%
  as.integer(.) -> start_year

country_parameters %>%
  dplyr::filter(Var == "end_year") %>%
  pull(ParCHR) %>%
  as.integer(.) -> end_year

country_parameters %>%
  dplyr::filter(Var == "monte_carlo_runs") %>%
  pull(ParCHR) %>%
  as.integer(.) -> monte_carlo_runs

country_parameters %>%
  dplyr::filter(Var == "uncapped_regrowth") %>%
  pull(ParCHR) %>%
  as.integer(.) -> uncapped_regrowth

country_parameters %>%
  dplyr::filter(Var == "npa_ease") %>%
  pull(ParCHR) %>%
  as.integer(.) -> npa_ease

# country_parameters %>%
#   dplyr::filter(Var == "instant_demand") %>%
#   pull(ParCHR) %>%
#   as.integer(.) -> instant_demand

# Save parameters table for Dinamica EGO ----
country_parameters_din <- data.frame(
  "Var" = c(
    "start_year",
    "end_year",
    "monte_carlo_runs",
    "uncapped_regrowth",
    "npa_ease"
    # "instant_demand"
  ),
  "ParCHR" = as.integer(c(
    start_year,
    end_year,
    monte_carlo_runs,
    uncapped_regrowth,
    npa_ease
    # instant_demand
  )),
  check.names = FALSE
)

write.csv(
  country_parameters_din,
  file = "LULCC/TempTables/parameters_dinamica.csv",
  row.names = FALSE,
  quote = FALSE
)

