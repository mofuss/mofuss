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

target_fuels <- c(
  "fuelwood", "charcoal", "imp_fuelwood", "imp_charcoal",
  "gas", "kerosene", "electric", "pellets",
  "ethanol", "biogas", "coal", "other"
)

build_mofuss_demand <- function(dem, popWHO) {
  
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
    filter(
      area %in% c("urban", "rural"),
      year >= 2000,
      year <= 2050,
      !is.na(fuel)
    ) %>%
    distinct(iso3, country, area, year, fuel, people)
  
  # optional check
  dup_who <- who_std %>%
    count(iso3, area, year, fuel) %>%
    filter(n > 1)
  
  # 2) WHO backbone expanded to all MoFuSS fuels
  who_keys <- who_std %>%
    distinct(iso3, country, area, year)
  
  who_full <- who_keys %>%
    crossing(fuel = target_fuels) %>%
    left_join(
      who_std %>% select(iso3, country, area, year, fuel, people),
      by = c("iso3", "country", "area", "year", "fuel"),
      relationship = "one-to-one"
    ) %>%
    mutate(
      people = ifelse(is.na(people), 0, people)
    ) %>%
    select(iso3, country, area, year, fuel, people)
  
  # 3) Demand table: keep only urban/rural, map fuels, aggregate if needed
  dem_std <- dem %>%
    transmute(
      iso3      = as.character(iso3),
      country   = as.character(country),
      area      = tolower(as.character(area)),
      year      = as.integer(year),
      fuel0     = tolower(as.character(fuel)),
      fuel_tons = as.numeric(fuel_tons3)
    ) %>%
    mutate(
      fuel = case_when(
        fuel0 == "biomass"     ~ "fuelwood",
        fuel0 == "charcoal"    ~ "charcoal",
        fuel0 == "gas"         ~ "gas",
        fuel0 == "kerosene"    ~ "kerosene",
        fuel0 == "electricity" ~ "electric",
        fuel0 == "coal"        ~ "coal",
        TRUE ~ fuel0
      )
    ) %>%
    filter(
      area %in% c("urban", "rural"),
      year >= 2000,
      year <= 2050
    ) %>%
    group_by(iso3, country, area, year, fuel) %>%
    summarise(
      fuel_tons = sum(fuel_tons, na.rm = TRUE),
      .groups = "drop"
    )
  
  # optional check
  dup_dem <- dem_std %>%
    count(iso3, area, year, fuel) %>%
    filter(n > 1)
  
  # 4) Final table without overall
  mofuss <- who_full %>%
    left_join(
      dem_std %>% select(iso3, area, year, fuel, fuel_tons),
      by = c("iso3", "area", "year", "fuel"),
      relationship = "one-to-one"
    ) %>%
    mutate(
      pc_fuel   = NA_real_,
      fuel_tons = ifelse(is.na(fuel_tons), 0, fuel_tons)
    ) %>%
    select(
      iso3, country, area, fuel, year,
      people, pc_fuel, fuel_tons
    ) %>%
    arrange(iso3, year, area, fuel)
  
  # 5) Add overall
  overall_rows <- mofuss %>%
    group_by(iso3, country, fuel, year) %>%
    summarise(
      area      = "overall",
      people    = sum(people, na.rm = TRUE),
      fuel_tons = sum(fuel_tons, na.rm = TRUE),
      pc_fuel   = NA_real_,
      .groups = "drop"
    ) %>%
    select(iso3, country, area, fuel, year, people, pc_fuel, fuel_tons)
  
  mofuss_o <- bind_rows(mofuss, overall_rows) %>%
    arrange(iso3, year, area, fuel)
  
  # 6) Add region
  mofuss_or <- mofuss_o %>%
    mutate(
      region = countrycode(iso3, "iso3c", "continent"),
      region = case_when(
        region == "Africa"   ~ "ssa",
        region == "Americas" ~ "latam",
        region == "Asia"     ~ "asia",
        region == "Oceania"  ~ "oceania",
        region == "Europe"   ~ "europe",
        TRUE ~ NA_character_
      )
    ) %>%
    relocate(region, .after = country) %>%
    select(
      iso3, country, region, area, fuel, year,
      people, pc_fuel, fuel_tons
    )
  
  # final duplicate check
  dup_final <- mofuss_or %>%
    count(iso3, area, year, fuel) %>%
    filter(n > 1)
  
  list(
    data = mofuss_or,
    dup_who = dup_who,
    dup_dem = dup_dem,
    dup_final = dup_final
  )
}

bau_dem <- read_csv(
  paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/cons_fuels_years_v2_original.csv")
)

ics_dem <- read_csv(
  paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/cons_fuels_years_proj_v2_original.csv")
)

popWHO <- read_excel(
  paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/A_LMIC_Estimates_2050_popmedian_original.xlsx")
)

bau_res <- build_mofuss_demand(bau_dem, popWHO)
ics_res <- build_mofuss_demand(ics_dem, popWHO)

bau_mofuss_or <- bau_res$data
ics_mofuss_or <- ics_res$data

bau_res$dup_who
bau_res$dup_dem
bau_res$dup_final

ics_res$dup_who
ics_res$dup_dem
ics_res$dup_final

unique(bau_mofuss_or$region)
unique(ics_mofuss_or$region)

sum(is.na(bau_mofuss_or$people))
sum(is.na(bau_mofuss_or$pc_fuel))
sum(is.na(bau_mofuss_or$fuel_tons))

sum(is.na(ics_mofuss_or$people))
sum(is.na(ics_mofuss_or$pc_fuel))
sum(is.na(ics_mofuss_or$fuel_tons))

write_csv(
  bau_mofuss_or,
  paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/cons_fuels_years_bau_mofuss.csv")
)

write_csv(
  ics_mofuss_or,
  paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/cons_fuels_years_ics_mofuss.csv")
)
