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
  dplyr::filter(Var == "end_year") %>%
  pull(ParCHR) %>%
  as.integer(.) -> end_year

country_parameters %>%
  dplyr::filter(Var == "efchratio") %>%
  pull(ParCHR) %>%
  as.integer(.) -> efchratio

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
    dplyr::filter(
      area %in% c("urban", "rural"),
      year >= start_year,
      year <= end_year,
      !is.na(fuel)
    ) %>%
    distinct(iso3, country, area, year, fuel, people)
  
  # optional check
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
    dplyr::filter(
      area %in% c("urban", "rural"),
      year >= start_year,
      year <= end_year
    ) %>%
    group_by(iso3, country, area, year, fuel) %>%
    summarise(
      fuel_tons = sum(fuel_tons, na.rm = TRUE),
      .groups = "drop"
    )
  
  # optional check
  dup_dem <- dem_std %>%
    count(iso3, area, year, fuel) %>%
    dplyr::filter(n > 1)
  
  # 4) Final table without overall
  mofuss <- who_full %>%
    left_join(
      dem_std %>% dplyr::select(iso3, area, year, fuel, fuel_tons),
      by = c("iso3", "area", "year", "fuel"),
      relationship = "one-to-one"
    ) %>%
    mutate(
      fuel_tons = ifelse(is.na(fuel_tons), 0, fuel_tons)
    ) %>%
    dplyr::select(
      iso3, country, area, fuel, year,
      people, fuel_tons
    ) %>%
    arrange(iso3, year, area, fuel)
  
  # 5) Add overall
  overall_rows <- mofuss %>%
    group_by(iso3, country, fuel, year) %>%
    summarise(
      area      = "overall",
      people    = sum(people, na.rm = TRUE),
      fuel_tons = sum(fuel_tons, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::select(iso3, country, area, fuel, year, people, fuel_tons)
  
  mofuss_o <- bind_rows(mofuss, overall_rows) %>%
    arrange(iso3, year, area, fuel)
  
  # 6) Add region and fuel_cons_calc
  dem_params_reg <- dem_params %>%
    dplyr::filter(!is.na(region)) %>%
    dplyr::select(fuel, region, pc_fuel)
  
  dem_params_all <- dem_params %>%
    dplyr::filter(is.na(region)) %>%
    dplyr::select(fuel, pc_fuel_default = pc_fuel)
  
  mofuss_or <- mofuss_o %>%
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
      people, pc_fuel, fuel_tons, fuel_tons_calc
    )
  
  # final duplicate check
  dup_final <- mofuss_or %>%
    count(iso3, area, year, fuel) %>%
    dplyr::filter(n > 1)
  
  list(
    data = mofuss_or,
    dup_who = dup_who,
    dup_dem = dup_dem,
    dup_final = dup_final
  )
}

bau_dem <- read_csv(
  paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/demand_bau_v2_original.csv")
)

ics_dem <- read_csv(
  paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/demand_ics_v2_original.csv")
)

popWHO <- read_excel(
  paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/A_LMIC_Estimates_2050_popmedian_original.xlsx")
)

# Demand parameters
dem_params <- read_excel(
  paste0(countrydir, "/LULCC/DownloadedDatasets//SourceDataGlobal/demand_parameters.xlsx"),
  sheet = "demand_parameters",
  skip = 0
) |> 
  mutate(
    fuel = as.character(fuel),
    region = as.character(region),
    pc_fuel = as.numeric(pc_fuel),
    pc_fuel_units = as.character(pc_fuel_units)
  ) 
dem_params

bau_res <- build_mofuss_demand(bau_dem, popWHO)
ics_res <- build_mofuss_demand(ics_dem, popWHO)

bau_mofuss_or <- bau_res$data
ics_mofuss_or <- ics_res$data
bau_mofuss_or
bau_mofuss_or %>%
  dplyr::filter(iso3 %in% c("KEN", "MWI", "ZMB"))

test <- bau_mofuss_or %>%
  dplyr::filter(
    iso3 == "KEN",
    area == "urban",
    fuel == "charcoal",
    year == 2020
  )
test
people_val <- test %>% pull(people)

# Test: people * charcaol yield * pc_cpns
people_val*1000*0.16*6


# Ref value: 5546396
5546396 / (3081*1000)


# ics_mofuss_or

# bau_res$dup_who
# bau_res$dup_dem
# bau_res$dup_final
# 
# ics_res$dup_who
# ics_res$dup_dem
# ics_res$dup_final
# 
# unique(bau_mofuss_or$region)
# unique(ics_mofuss_or$region)
# 
# sum(is.na(bau_mofuss_or$people))
# sum(is.na(bau_mofuss_or$pc_fuel))
# sum(is.na(bau_mofuss_or$fuel_tons))
# 
# sum(is.na(ics_mofuss_or$people))
# sum(is.na(ics_mofuss_or$pc_fuel))
# sum(is.na(ics_mofuss_or$fuel_tons))

# write_csv(
#   bau_mofuss_or,
#   paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/demand_bau_v2.csv")
# )
# 
# write_csv(
#   ics_mofuss_or,
#   paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/demand_ics_v2.csv")
# )

