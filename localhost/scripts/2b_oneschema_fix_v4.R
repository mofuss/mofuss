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

target_fuels <- c(
  "fuelwood", "charcoal", "imp_fuelwood", "imp_charcoal",
  "gas", "kerosene", "electric", "pellets",
  "ethanol", "biogas", "coal" #, "other"
)

# Build BAU table directly from WHO + demand parameters ----
build_mofuss_demand <- function(popWHO) {
  
  # 1) WHO table: keep only urban/rural, map fuels, keep people in thousands
  who_std <- popWHO %>%
    transmute(
      iso3    = as.character(iso3),
      country = as.character(country),
      area    = tolower(as.character(area)),
      year    = as.integer(year),
      fuel0   = tolower(as.character(fuel)),
      people  = as.numeric(pop * 1)
    ) %>%
    mutate(
      fuel = case_when(
        fuel0 == "biomass"        ~ "fuelwood",
        fuel0 == "charcoal"       ~ "charcoal",
        fuel0 == "gas"            ~ "gas",
        fuel0 == "kerosene"       ~ "kerosene",
        fuel0 == "electricity"    ~ "electric",
        fuel0 == "coal"           ~ "coal",
        fuel0 %in% c("total polluting", "total clean") ~ NA_character_,
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(
      area %in% c("urban", "rural"),
      year >= start_year,
      year <= end_year,
      !is.na(fuel)
    ) %>%
    distinct(iso3, country, area, year, fuel, people)
  
  dup_who <- who_std %>%
    count(iso3, area, year, fuel) %>%
    dplyr::filter(n > 1)
  
  # 2) WHO backbone expanded to all MoFuSS fuels
  who_keys <- who_std %>%
    distinct(iso3, country, area, year)
  
  who_full <- who_keys %>%
    tidyr::crossing(fuel = target_fuels) %>%
    left_join(
      who_std %>% dplyr::select(iso3, country, area, year, fuel, people),
      by = c("iso3", "country", "area", "year", "fuel"),
      relationship = "one-to-one"
    ) %>%
    mutate(
      people = ifelse(is.na(people), 0, people)
    ) %>%
    dplyr::select(iso3, country, area, year, fuel, people)
  
  # 3) Add region and fuel_tons_calc only
  dem_params_reg <- dem_params %>%
    dplyr::filter(!is.na(region)) %>%
    dplyr::select(fuel, region, pc_fuel)
  
  dem_params_all <- dem_params %>%
    dplyr::filter(is.na(region)) %>%
    dplyr::select(fuel, pc_fuel_default = pc_fuel)
  
  bau_no_overall <- who_full %>%
    mutate(
      region = case_when(
        iso3 %in% c("AGO","BDI","BEN","BFA","BWA","CAF","CIV","CMR","COD","COG","COM","CPV",
                    "DJI","ERI","ETH","GAB","GHA","GIN","GMB","GNB","GNQ","KEN","LBR","LSO",
                    "MDG","MLI","MOZ","MRT","MUS","MWI","NAM","NER","NGA","RWA","SDN","SEN",
                    "SLE","SOM","SSD","STP","SWZ","SYC","TCD","TGO","TZA","UGA","ZAF","ZMB","ZWE") ~ "ssa",
        iso3 %in% c("ARG","BOL","BRA","CHL","COL","CRI","CUB","DOM","ECU","SLV","GTM","HND",
                    "HTI","JAM","MEX","NIC","PAN","PER","PRY","URY","VEN","BLZ","GUY","SUR") ~ "latam",
        iso3 %in% c("CHN","MNG","PRK","KOR","JPN","TWN") ~ "east_asia",
        iso3 %in% c("AFG","BGD","BTN","IND","LKA","MDV","NPL","PAK") ~ "south_asia",
        TRUE ~ "other"
      )
    ) %>%
    left_join(
      dem_params_reg,
      by = c("fuel", "region"),
      relationship = "many-to-one"
    ) %>%
    left_join(
      dem_params_all,
      by = "fuel",
      relationship = "many-to-one"
    ) %>%
    mutate(
      pc_fuel = dplyr::coalesce(pc_fuel, pc_fuel_default),
      fuel_tons_calc = case_when(
        is.na(pc_fuel) ~ NA_real_,
        fuel == "charcoal" ~ people * 1000 * pc_fuel * efchratio,
        TRUE ~ people * 1000 * pc_fuel
      )
    ) %>%
    dplyr::select(-pc_fuel_default) %>%
    relocate(region, .after = country) %>%
    dplyr::select(
      iso3, country, region, area, fuel, year,
      people, pc_fuel, fuel_tons_calc
    ) %>%
    arrange(iso3, year, area, fuel)
  
  dup_final <- bau_no_overall %>%
    count(iso3, area, year, fuel) %>%
    dplyr::filter(n > 1)
  
  list(
    data = bau_no_overall,
    dup_who = dup_who,
    dup_final = dup_final
  )
}

popWHO <- read_excel(
  paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/A_LMIC_Estimates_2050_popmedian_original.xlsx")
)

# Demand parameters
read_flexible <- function(path) {
  read_delim(path, delim = NULL, show_col_types = FALSE)
}

dem_params <- read_flexible(
  paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand_parameters.csv")
)

bau_res <- build_mofuss_demand(popWHO)

bau_mofuss_or <- bau_res$data

bau_mofuss_or %>%
  dplyr::filter(iso3 %in% c("KEN"),
                year >= 2050,
                area == "rural") %>%
  print(., n=11)

unique(bau_mofuss_or$area)

# Interpolation ----
# ## 1) REMOVE "overall" ----
# bau_no_overall <- bau_mofuss_or %>%
#   dplyr::filter(area != "overall")

## 2) DEFAULT SHARE TABLE (REFERENCE ONLY)----
share_table_default <- bau_no_overall %>%
  group_by(iso3, country, region, area, year) %>%
  mutate(
    total_people_area_year = sum(people, na.rm = TRUE),
    share_default = if_else(total_people_area_year > 0, people / total_people_area_year, 0)
  ) %>%
  ungroup() %>%
  dplyr::select(iso3, country, region, area, year, fuel, people, share_default)

# Optional export for user reference
write_csv(
  share_table_default,
  paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/demand_default_shares_reference.csv")
)

## 3) USER ANCHOR TABLE ----
# User must provide ALL fuels for each anchor year
if (scenario_ver == "BaU_v2") {
  
  cat("\033[32mBaU scenario selected: no anchor points will be read.\033[0m\n")
  
} else if (scenario_ver == "ICS1_v2") {
  
  anchors_user <- read_flexible(
    paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/anchor_points1.csv")
  )
  
} else if (scenario_ver == "ICS2_v2") {
  
  anchors_user <- read_flexible(
    paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/anchor_points2.csv")
  )
  
} else if (scenario_ver == "ICS3_v2") {
  
  anchors_user <- read_flexible(
    paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/anchor_points3.csv")
  )
  
} else {
  
  stop(sprintf("Invalid scenario_ver: %s", scenario_ver))
  
}

if (scenario_ver %in% c("ICS1_v2", "ICS2_v2", "ICS3_v2")) {
  
  ## 4) STANDARDIZE USER VALUES TO 0-1 ----
  #    If entered as percentages (0-100), convert to 0-1
  anchors_user <- anchors_user %>%
    mutate(
      share_user = case_when(
        max(share_user, na.rm = TRUE) > 1 ~ share_user / 100,
        TRUE ~ share_user
      )
    )
  
  ## 5) VALIDATE USER ANCHOR TABLE ----
  # 5a) Allowed years
  bad_years <- anchors_user %>%
    dplyr::filter(year < start_year | year > end_year)
  
  if (nrow(bad_years) > 0) {
    stop("All anchor years must be between 2000 and 2050.")
  }
  
  # 5b) At least 2 and at most 51 anchor years per iso3-area
  anchor_year_count <- anchors_user %>%
    distinct(iso3, area, year) %>%
    count(iso3, area, name = "n_anchor_years")
  
  if (any(anchor_year_count$n_anchor_years < 2)) {
    stop("Each iso3-area must have at least 2 anchor years.")
  }
  
  if (any(anchor_year_count$n_anchor_years > 51)) {
    stop("Each iso3-area can have at most 51 anchor years.")
  }
  
  # 5c) Expected fuel set comes from BAU
  expected_fuels <- bau_no_overall %>%
    distinct(fuel) %>%
    arrange(fuel) %>%
    pull(fuel)
  
  n_expected_fuels <- length(expected_fuels)
  
  # 5d) Every anchor year must include all fuels exactly once
  fuel_check <- anchors_user %>%
    group_by(iso3, area, year) %>%
    summarise(
      n_rows = n(),
      n_fuels = n_distinct(fuel),
      missing_fuels = paste(setdiff(expected_fuels, unique(fuel)), collapse = ", "),
      extra_fuels = paste(setdiff(unique(fuel), expected_fuels), collapse = ", "),
      .groups = "drop"
    )
  
  bad_fuel_check <- fuel_check %>%
    dplyr::filter(
      n_rows != n_expected_fuels |
        n_fuels != n_expected_fuels |
        missing_fuels != "" |
        extra_fuels != ""
    )
  
  if (nrow(bad_fuel_check) > 0) {
    print(bad_fuel_check)
    stop("Each iso3-area-year in anchors_user must contain all fuels exactly once.")
  }
  
  # 5e) Shares must be between 0 and 1
  bad_shares <- anchors_user %>%
    dplyr::filter(share_user < 0 | share_user > 1)
  
  if (nrow(bad_shares) > 0) {
    stop("All share_user values must be between 0 and 1 (or 0 and 100 before standardization).")
  }
  
  # 5f) Shares must sum to 1 per iso3-area-year
  sum_check <- anchors_user %>%
    group_by(iso3, area, year) %>%
    summarise(share_sum = sum(share_user, na.rm = TRUE), .groups = "drop")
  
  bad_sums <- sum_check %>%
    dplyr::filter(abs(share_sum - 1) > 1e-8)
  
  if (nrow(bad_sums) > 0) {
    print(bad_sums)
    stop("For each iso3-area-year, anchor shares must sum to 1 (or 100 before standardization).")
  }
  
  ## 6) INTERPOLATE USER SHARES ACROSS 2000-2050 ----
  #    USING ONLY USER ANCHORS
  anchor_meta <- bau_no_overall %>%
    distinct(iso3, country, region, area)
  
  share_table_ics <- anchors_user %>%
    left_join(anchor_meta, by = c("iso3", "area")) %>%
    group_by(iso3, country, region, area, fuel) %>%
    group_modify(~{
      anchors_this <- .x %>%
        arrange(year)
      tibble(
        year = start_year:end_year,
        share_ics = approx(
          x = anchors_this$year,
          y = anchors_this$share_user,
          xout = start_year:end_year,
          method = "linear",
          rule = 2
        )$y
      )
    }) %>%
    ungroup()
  
  ## 7) CHECK INTERPOLATED SHARES SUM TO 1----
  ics_sum_check <- share_table_ics %>%
    group_by(iso3, area, year) %>%
    summarise(share_sum = sum(share_ics, na.rm = TRUE), .groups = "drop")
  
  bad_ics_sums <- ics_sum_check %>%
    dplyr::filter(abs(share_sum - 1) > 1e-8)
  
  if (nrow(bad_ics_sums) > 0) {
    print(bad_ics_sums)
    stop("Interpolated ICS shares do not sum to 1 for some iso3-area-year groups.")
  }
  
  # Optional output table
  share_table_ics_out <- share_table_ics %>%
    arrange(iso3, area, year, fuel)
  
  # Optional save
  write_csv(
    share_table_ics_out,
    paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/demand_ics_shares.csv")
  )
  
  ## 8) BUILD BAU SHARES + TOTAL PEOPLE FOR ALL COUNTRIES----
  bau_share_table <- bau_no_overall %>%
    group_by(iso3, country, region, area, year) %>%
    mutate(
      total_people = sum(people, na.rm = TRUE),
      share_bau = if_else(total_people > 0, people / total_people, 0)
    ) %>%
    ungroup() %>%
    dplyr::select(iso3, country, region, area, fuel, year, total_people, share_bau)
  
  ## 9) APPLY ICS ONLY WHERE AVAILABLE----
  #    OTHERWISE KEEP BAU SHARES
  share_table_final <- bau_share_table %>%
    left_join(
      share_table_ics_out %>%
        dplyr::select(iso3, area, year, fuel, share_ics),
      by = c("iso3", "area", "year", "fuel")
    ) %>%
    mutate(
      share_final = coalesce(share_ics, share_bau)
    )
  
  # Optional check: shares should sum to 1 everywhere
  share_check <- share_table_final %>%
    group_by(iso3, area, year) %>%
    summarise(share_sum = sum(share_final, na.rm = TRUE), .groups = "drop")
  
  # print(share_check %>% filter(abs(share_sum - 1) > 1e-8))
  
  ## 10) REBUILD NON-overall TABLE----
  ics_no_overall <- bau_no_overall %>%
    dplyr::select(-people, -fuel_tons_calc) %>%
    left_join(
      share_table_final %>%
        dplyr::select(iso3, area, year, fuel, total_people, share_final),
      by = c("iso3", "area", "year", "fuel")
    ) %>%
    mutate(
      people = total_people * share_final
    )
  
  ## 11) RECALCULATE fuel_tons_calc ----
  #    USE THE SAME case_when() AS IN bau_mofuss_or
  ics_no_overall <- ics_no_overall %>%
    mutate(
      fuel_tons_calc = case_when(
        is.na(pc_fuel) ~ NA_real_,
        fuel == "charcoal" ~ people * 1000 * pc_fuel * efchratio,
        TRUE ~ people * 1000 * pc_fuel
      )
    ) %>%
    dplyr::select(
      iso3, country, region, area, fuel, year,
      people, pc_fuel, fuel_tons_calc
    )

  ## 12) OPTIONAL CHECKS----
  
  # Check one non-anchor country, should match BAU up to floating precision
  # Example:
  bau_no_overall %>%
    dplyr::filter(iso3 == "KEN", area == "urban", year == end_year) %>%
    arrange(fuel)
  
  ics_no_overall %>%
    dplyr::filter(iso3 == "KEN", area == "urban", year == end_year) %>%
    arrange(fuel)
  
  # Compare BAU vs ICS for non-anchor countries
  ics_targets <- share_table_ics_out %>%
    distinct(iso3, area)
  
  bau_check <- bau_no_overall %>%
    anti_join(ics_targets, by = c("iso3", "area")) %>%
    arrange(iso3, area, year, fuel) %>%
    dplyr::select(iso3, area, year, fuel, people_bau = people, fuel_tons_calc_bau = fuel_tons_calc)
  
  ics_check <- ics_no_overall %>%
    anti_join(ics_targets, by = c("iso3", "area")) %>%
    arrange(iso3, area, year, fuel) %>%
    dplyr::select(iso3, area, year, fuel, people_ics = people, fuel_tons_calc_ics = fuel_tons_calc)
  
  diff_check <- bau_check %>%
    left_join(ics_check, by = c("iso3", "area", "year", "fuel")) %>%
    mutate(
      diff_people = people_ics - people_bau,
      diff_fuel_tons_calc = fuel_tons_calc_ics - fuel_tons_calc_bau
    )
  
  diff_check %>%
    summarise(
      max_abs_diff_people = max(abs(diff_people), na.rm = TRUE),
      max_abs_diff_fuel_tons_calc = max(abs(diff_fuel_tons_calc), na.rm = TRUE)
    )
  
}

# Rebuilding ----
## 1) REBUILD overall FOR BAU ----
bau_overall <- bau_no_overall %>%
  group_by(iso3, country, region, fuel, year) %>%
  summarise(
    area = "overall",
    people = sum(people, na.rm = TRUE),
    fuel_tons_calc = sum(fuel_tons_calc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pc_fuel = if_else(
      people > 0,
      fuel_tons_calc / (people * 1000),
      NA_real_
    )
  ) %>%
  dplyr::select(
    iso3, country, region, area, fuel, year,
    people, pc_fuel, fuel_tons_calc
  )

bau_mofuss_or_rebuilt <- bind_rows(
  bau_no_overall %>%
    dplyr::select(iso3, country, region, area, fuel, year, people, pc_fuel, fuel_tons_calc),
  bau_overall
) %>%
  arrange(iso3, year, area, fuel)

if (scenario_ver %in% c("ICS1_v2", "ICS2_v2", "ICS3_v2")) {
  
  # 2) REBUILD overall FOR ICS
  ics_overall <- ics_no_overall %>%
    group_by(iso3, country, region, fuel, year) %>%
    summarise(
      area = "overall",
      people = sum(people, na.rm = TRUE),
      fuel_tons_calc = sum(fuel_tons_calc, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      pc_fuel = if_else(
        people > 0,
        fuel_tons_calc / (people * 1000),
        NA_real_
      )
    ) %>%
    dplyr::select(
      iso3, country, region, area, fuel, year,
      people, pc_fuel, fuel_tons_calc
    )
  
  ics_mofuss_or <- bind_rows(
    ics_no_overall %>%
      dplyr::select(iso3, country, region, area, fuel, year, people, pc_fuel, fuel_tons_calc),
    ics_overall
  ) %>%
    arrange(iso3, year, area, fuel)
  
}

bau_mofuss_or_rebuilt %>%
  dplyr::filter(area == "overall", iso3 == "KEN", year == 2030) %>%
  arrange(fuel)

if (scenario_ver %in% c("ICS1_v2", "ICS2_v2", "ICS3_v2")) {
  
  ics_mofuss_or %>%
    dplyr::filter(area == "overall", iso3 == "KEN", year == 2030) %>%
    arrange(fuel)
}

bau_mofuss_or_rebuilt %>%
  count(area)
if (scenario_ver %in% c("ICS1_v2", "ICS2_v2", "ICS3_v2")) {
  ics_mofuss_or %>%
    count(area)
}

write_csv(
  bau_mofuss_or_rebuilt,
  paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/demand_bau_v2.csv")
)

if (scenario_ver == "BaU_v2") {
  
  cat("\033[32mBaU scenario selected: no demand table for ICS scenarios will be written.\033[0m\n")
  
} else if (scenario_ver == "ICS1_v2") {
  
  write_csv(
    ics_mofuss_or,
    paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/demand_ics1_v2.csv")
  )
  
} else if (scenario_ver == "ICS2_v2") {
  
  write_csv(
    ics_mofuss_or,
    paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/demand_ics2_v2.csv")
  )
  
} else if (scenario_ver == "ICS3_v2") {
  
  write_csv(
    ics_mofuss_or,
    paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/demand_ics3_v2.csv")
  )
  
} else {
  
  stop(sprintf("Invalid scenario_ver: %s", scenario_ver))
  
}

