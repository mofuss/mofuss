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
# Version 3
# Date: Mar 2024

# 2dolist ----

# Internal parameters ----

# Load libraries ----
library(conflicted)

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(countrycode)
library(purrr)


# Detect OS
os <- Sys.info()["sysname"]

setwd(countrydir)
getwd()

# Read parameters table ----
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

country_parameters %>%
  dplyr::filter(Var == "start_year") %>%
  pull(ParCHR) %>%
  as.integer(.) -> start_year

country_parameters %>%
  dplyr::filter(Var == "end_year") %>%
  pull(ParCHR) %>%
  as.integer(.) -> end_year

country_parameters %>%
  dplyr::filter(Var == "efchratio") %>%
  pull(ParCHR) %>%
  as.integer(.) -> efchratio

country_parameters %>%
  dplyr::filter(Var == "scenario_ver") %>%
  pull(ParCHR) -> scenario_ver

# anchor table
# Biomass-improvement pathway
# Clean cooking expands mostly through improved fuelwood and improved charcoal, especially where full fuel switching is slower.
anchors_user1 <- tibble(
  iso3 = rep("KEN", 66),
  area = rep(c("rural", "urban"), each = 33),
  year = rep(rep(c(2000, 2025, 2050), each = 11), 2),
  fuel = rep(c(
    "fuelwood", "charcoal", "imp_fuelwood", "imp_charcoal",
    "gas", "kerosene", "electric", "pellets",
    "ethanol", "biogas", "coal"
  ), 6),
  share_user = c(
    
    # RURAL 2000
    0.9209, 0.0549, 0, 0, 0.0030, 0.0202, 0.0010, 0, 0, 0, 0,
    
    # RURAL 2025
    0.8663, 0.0673, 0, 0, 0.0561, 0.0082, 0.0020, 0, 0, 0, 0,
    
    # RURAL 2050
    0.10, 0.05, 0.45, 0.22, 0.05, 0.00, 0.03, 0.02, 0.01, 0.07, 0.00,
    
    # URBAN 2000
    0.0923, 0.2515, 0, 0, 0.0464, 0.5995, 0.01031, 0, 0, 0, 0,
    
    # URBAN 2025
    0.1578, 0.1610, 0, 0, 0.5448, 0.1205, 0.0160, 0, 0, 0, 0,
    
    # URBAN 2050
    0.03, 0.10, 0.25, 0.35, 0.12, 0.00, 0.08, 0.03, 0.01, 0.02, 0.01
  )
)

# Modern-energy pathway
# Transition is driven mainly by gas and electricity, with traditional and improved biomass both shrinking strongly by 2050.
anchors_user2 <- tibble(
  iso3 = rep("KEN", 66),
  area = rep(c("rural", "urban"), each = 33),
  year = rep(rep(c(2000, 2025, 2050), each = 11), 2),
  fuel = rep(c(
    "fuelwood", "charcoal", "imp_fuelwood", "imp_charcoal",
    "gas", "kerosene", "electric", "pellets",
    "ethanol", "biogas", "coal"
  ), 6),
  share_user = c(
    
    # RURAL 2000
    0.9209, 0.0549, 0, 0, 0.0030, 0.0202, 0.0010, 0, 0, 0, 0,
    
    # RURAL 2025
    0.8663, 0.0673, 0, 0, 0.0561, 0.0082, 0.0020, 0, 0, 0, 0,
    
    # RURAL 2050
    0.02, 0.01, 0.03, 0.02, 0.56, 0.00, 0.28, 0.02, 0.02, 0.04, 0.00,
    
    # URBAN 2000
    0.0923, 0.2515, 0, 0, 0.0464, 0.5995, 0.01031, 0, 0, 0, 0,
    
    # URBAN 2025
    0.1578, 0.1610, 0, 0, 0.5448, 0.1205, 0.0160, 0, 0, 0, 0,
    
    # URBAN 2050
    0.00, 0.01, 0.01, 0.01, 0.62, 0.00, 0.32, 0.01, 0.01, 0.00, 0.01
  )
)

# Mixed pathway
# A more realistic compromise: improved biomass remains important, while gas and electricity also grow a lot.
anchors_user3 <- tibble(
  iso3 = rep("KEN", 66),
  area = rep(c("rural", "urban"), each = 33),
  year = rep(rep(c(2000, 2025, 2050), each = 11), 2),
  fuel = rep(c(
    "fuelwood", "charcoal", "imp_fuelwood", "imp_charcoal",
    "gas", "kerosene", "electric", "pellets",
    "ethanol", "biogas", "coal"
  ), 6),
  share_user = c(
    
    # RURAL 2000
    0.9209, 0.0549, 0, 0, 0.0030, 0.0202, 0.0010, 0, 0, 0, 0,
    
    # RURAL 2025
    0.8663, 0.0673, 0, 0, 0.0561, 0.0082, 0.0020, 0, 0, 0, 0,
    
    # RURAL 2050
    0.08, 0.03, 0.25, 0.12, 0.22, 0.01, 0.16, 0.03, 0.03, 0.07, 0.00,
    
    # URBAN 2000
    0.0923, 0.2515, 0, 0, 0.0464, 0.5995, 0.01031, 0, 0, 0, 0,
    
    # URBAN 2025
    0.1578, 0.1610, 0, 0, 0.5448, 0.1205, 0.0160, 0, 0, 0, 0,
    
    # URBAN 2050
    0.02, 0.04, 0.12, 0.16, 0.34, 0.00, 0.25, 0.02, 0.02, 0.02, 0.01
  )
)

anchors_user_fixed1 <- anchors_user1 %>%
  group_by(area, year) %>%
  mutate(
    total = sum(share_user),
    share_user = share_user / total
  ) %>%
  mutate(
    share_user = round(share_user, 4)
  ) %>%
  mutate(
    diff = 1 - sum(share_user),
    share_user = if_else(row_number() == 1, share_user + diff, share_user)
  ) %>%
  ungroup() %>%
  select(-total, -diff)

anchors_user_fixed2 <- anchors_user2 %>%
  group_by(area, year) %>%
  mutate(
    total = sum(share_user),
    share_user = share_user / total
  ) %>%
  mutate(
    share_user = round(share_user, 4)
  ) %>%
  mutate(
    diff = 1 - sum(share_user),
    share_user = if_else(row_number() == 1, share_user + diff, share_user)
  ) %>%
  ungroup() %>%
  select(-total, -diff)

anchors_user_fixed3 <- anchors_user3 %>%
  group_by(area, year) %>%
  mutate(
    total = sum(share_user),
    share_user = share_user / total
  ) %>%
  mutate(
    share_user = round(share_user, 4)
  ) %>%
  mutate(
    diff = 1 - sum(share_user),
    share_user = if_else(row_number() == 1, share_user + diff, share_user)
  ) %>%
  ungroup() %>%
  select(-total, -diff)


if (scenario_ver == "BaU_v2") {
  
  cat("\033[32mBaU scenario selected: no anchor points will be written.\033[0m\n")
  
} else if (scenario_ver == "ICS1_v2") {
  
  write_csv(
    anchors_user_fixed1,
    file.path(countrydir, "LULCC/DownloadedDatasets/SourceDataGlobal/anchor_points1.csv")
  )
  
} else if (scenario_ver == "ICS2_v2") {
  
  write_csv(
    anchors_user_fixed2,
    file.path(countrydir, "LULCC/DownloadedDatasets/SourceDataGlobal/anchor_points2.csv")
  )
  
} else if (scenario_ver == "ICS3_v2") {
  
  write_csv(
    anchors_user_fixed3,
    file.path(countrydir, "LULCC/DownloadedDatasets/SourceDataGlobal/anchor_points3.csv")
  )
  
} else {
  
  stop(sprintf("Invalid scenario_ver: %s", scenario_ver))
  
}

