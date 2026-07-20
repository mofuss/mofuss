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
# Version 5 -- runs ALL three ICS scenarios in one shot when scenario_ver is
# any of ICS1_v2 / ICS2_v2 / ICS3_v2 (writes demand_ics1_v2.csv,
# demand_ics2_v2.csv, demand_ics3_v2.csv). BaU behaviour unchanged.
# Date: July 2026

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

# country_parameters %>%
#   dplyr::filter(Var == "end_year") %>%
#   pull(ParCHR) %>%
#   as.integer(.) -> end_year
end_year <- 2050

country_parameters %>%
  dplyr::filter(Var == "efchratio") %>%
  pull(ParCHR) %>%
  as.integer(.) -> efchratio

country_parameters %>%
  dplyr::filter(Var == "scenario_ver") %>%
  pull(ParCHR) -> scenario_ver
# scenario_ver <- "BaU1_v2"
# scenario_ver <- "ICS1_v2"

country_parameters %>%
  dplyr::filter(Var == "demand_col") %>%
  pull(ParCHR) -> demand_col

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
      num_fuel_users_thousands  = as.numeric(pop * 1)
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
    distinct(iso3, country, area, year, fuel, num_fuel_users_thousands)
  
  dup_who <- who_std %>%
    count(iso3, area, year, fuel) %>%
    dplyr::filter(n > 1)
  
  # 2) WHO backbone expanded to all MoFuSS fuels
  who_keys <- who_std %>%
    distinct(iso3, country, area, year)
  
  who_full <- who_keys %>%
    tidyr::crossing(fuel = target_fuels) %>%
    left_join(
      who_std %>% dplyr::select(iso3, country, area, year, fuel, num_fuel_users_thousands),
      by = c("iso3", "country", "area", "year", "fuel"),
      relationship = "one-to-one"
    ) %>%
    mutate(
      num_fuel_users_thousands = ifelse(is.na(num_fuel_users_thousands), 0, num_fuel_users_thousands)
    ) %>%
    dplyr::select(iso3, country, area, year, fuel, num_fuel_users_thousands)
  
  # 3) Add region and demand_col only
  dem_params_reg <- dem_params %>%
    dplyr::filter(!is.na(region)) %>%
    dplyr::select(fuel, region, per_capita_fuel_cons)
  
  dem_params_all <- dem_params %>%
    dplyr::filter(is.na(region)) %>%
    dplyr::select(fuel, per_capita_fuel_cons_default = per_capita_fuel_cons)
  
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
      per_capita_fuel_cons = dplyr::coalesce(per_capita_fuel_cons, per_capita_fuel_cons_default),
      !!demand_col := case_when(
        is.na(per_capita_fuel_cons) ~ NA_real_,
        fuel == "charcoal" ~ num_fuel_users_thousands * 1000 * per_capita_fuel_cons * efchratio,
        TRUE ~ num_fuel_users_thousands * 1000 * per_capita_fuel_cons
      )
    ) %>%
    dplyr::select(-per_capita_fuel_cons_default) %>%
    relocate(region, .after = country) %>%
    dplyr::select(
      iso3, country, region, area, fuel, year,
      num_fuel_users_thousands, per_capita_fuel_cons, all_of(demand_col)
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
  paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/demand_parameters.csv")
) #If demand_parameters_user.csv exists....

bau_res <- build_mofuss_demand(popWHO)

bau_mofuss_or <- bau_res$data

bau_mofuss_or %>%
  dplyr::filter(iso3 %in% c("KEN"),
                year >= 2050,
                area == "rural") %>%
  print(., n=11)

unique(bau_mofuss_or$area)

# Interpolation ----
## 1) REMOVE "overall" ----
bau_no_overall <- bau_mofuss_or %>%
  dplyr::filter(area != "overall")

## 2) DEFAULT SHARE TABLE (REFERENCE ONLY)----
share_table_default <- bau_no_overall %>%
  group_by(iso3, country, region, area, year) %>%
  mutate(
    total_people_area_year = sum(num_fuel_users_thousands, na.rm = TRUE),
    share_default = if_else(total_people_area_year > 0, num_fuel_users_thousands / total_people_area_year, 0)
  ) %>%
  ungroup() %>%
  dplyr::select(iso3, country, region, area, year, fuel, num_fuel_users_thousands, share_default)

# Optional export for user reference
write_csv(
  share_table_default,
  paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/demand_default_shares_reference.csv")
)

# ---------------------------------------------------------------------------
# Multi-ICS pipeline ----
# ---------------------------------------------------------------------------
# When scenario_ver is any of ICS1_v2 / ICS2_v2 / ICS3_v2, run the full ICS
# pipeline for all three anchor files (anchor_points1.csv, anchor_points2.csv,
# anchor_points3.csv) and produce demand_ics1_v2.csv, demand_ics2_v2.csv,
# demand_ics3_v2.csv. The BaU branch is unchanged.

ics_scenarios <- c("ICS1_v2", "ICS2_v2", "ICS3_v2")
bau_scenarios <- c("BaU1_v2", "BaU2_v2", "BaU3_v2")

# Keep the observed/baseline history unchanged. The 2025 anchor is the
# transition point, so the first year in which an ICS can differ from BaU is
# 2026. This avoids back-casting the 2025 ICS anchor into 2001-2024.
historical_bau_end_year <- 2025L

if (historical_bau_end_year < start_year || historical_bau_end_year >= end_year) {
  stop(sprintf(
    "historical_bau_end_year (%d) must be within the simulation horizon [%d, %d).",
    historical_bau_end_year, start_year, end_year
  ))
}

if (!scenario_ver %in% c(ics_scenarios, bau_scenarios)) {
  stop(sprintf("Invalid scenario_ver: %s", scenario_ver))
}

if (scenario_ver %in% bau_scenarios) {
  cat("\033[32mA BaU scenario was selected: no anchor points will be read.\033[0m\n")
}

# Function: take an anchor scenario number (1/2/3) and return the
# ics_no_overall tibble that the trailing rebuild needs.
run_ics_pipeline <- function(anchor_num) {
  
  cat(sprintf("\n\033[36m=== Running ICS pipeline for anchor_points%d.csv ===\033[0m\n",
              anchor_num))
  
  anchors_user <- read_flexible(
    paste0(
      countrydir,
      "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/anchor_points",
      anchor_num,
      ".csv"
    )
  )
  
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
  
  ## 6) BUILD THE USER-ANCHOR TRAJECTORY ACROSS 2000-2050 ----
  # The complete trajectory is retained for diagnostics, but section 9 keeps
  # exact BaU shares through 2025 and applies this trajectory only from 2026.
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
  
  # Intermediate user-anchor trajectory
  share_table_ics_out <- share_table_ics %>%
    arrange(iso3, area, year, fuel)
  
  ## 8) BUILD BAU SHARES + TOTAL PEOPLE FOR ALL COUNTRIES----
  bau_share_table <- bau_no_overall %>%
    group_by(iso3, country, region, area, year) %>%
    mutate(
      total_people = sum(num_fuel_users_thousands, na.rm = TRUE),
      share_bau = if_else(total_people > 0, num_fuel_users_thousands / total_people, 0)
    ) %>%
    ungroup() %>%
    dplyr::select(iso3, country, region, area, fuel, year, total_people, share_bau)
  
  ## 9) KEEP BAU THROUGH 2025; APPLY ICS FROM 2026 ----
  share_table_final <- bau_share_table %>%
    left_join(
      share_table_ics_out %>%
        dplyr::select(iso3, area, year, fuel, share_ics),
      by = c("iso3", "area", "year", "fuel")
    ) %>%
    mutate(
      share_final = if_else(
        year <= historical_bau_end_year,
        share_bau,
        coalesce(share_ics, share_bau)
      )
    )

  # Hard regression guard: the applied fuel shares must be exactly BaU for
  # every historical row, not merely close after rounding.
  max_abs_historical_share_diff <- share_table_final %>%
    dplyr::filter(year <= historical_bau_end_year) %>%
    summarise(value = max(abs(share_final - share_bau), na.rm = TRUE)) %>%
    pull(value)

  if (!is.finite(max_abs_historical_share_diff) || max_abs_historical_share_diff != 0) {
    stop(sprintf(
      "ICS%d historical-share freeze failed (max absolute difference: %s).",
      anchor_num, format(max_abs_historical_share_diff, scientific = TRUE)
    ))
  }

  cat(sprintf(
    "\033[32mICS%d historical shares match BaU exactly through %d.\033[0m\n",
    anchor_num, historical_bau_end_year
  ))

  # Save the shares actually applied to the demand table. This deliberately
  # reports BaU through 2025 and the selected ICS trajectory from 2026.
  share_table_ics_applied_out <- share_table_final %>%
    transmute(
      iso3, country, region, area, fuel, year,
      share_ics = share_final
    ) %>%
    arrange(iso3, area, year, fuel)

  write_csv(
    share_table_ics_applied_out,
    paste0(
      countrydir,
      "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/demand_ics",
      anchor_num,
      "_shares.csv"
    )
  )
  
  # Optional check: shares should sum to 1 everywhere
  share_check <- share_table_final %>%
    group_by(iso3, area, year) %>%
    summarise(share_sum = sum(share_final, na.rm = TRUE), .groups = "drop")
  
  # print(share_check %>% filter(abs(share_sum - 1) > 1e-8))
  
  ## 10) REBUILD NON-overall TABLE----
  ics_no_overall <- bau_no_overall %>%
    dplyr::select(-num_fuel_users_thousands, -all_of(demand_col)) %>%
    left_join(
      share_table_final %>%
        dplyr::select(iso3, area, year, fuel, total_people, share_final),
      by = c("iso3", "area", "year", "fuel")
    ) %>%
    mutate(
      num_fuel_users_thousands = total_people * share_final
    )
  
  ## 11) RECALCULATE all_of(demand_col) ----
  #    USE THE SAME case_when() AS IN bau_mofuss_or
  ics_no_overall <- ics_no_overall %>%
    mutate(
      !!demand_col := case_when(
        is.na(per_capita_fuel_cons) ~ NA_real_,
        fuel == "charcoal" ~ num_fuel_users_thousands * 1000 * per_capita_fuel_cons * efchratio,
        TRUE ~ num_fuel_users_thousands * 1000 * per_capita_fuel_cons
      )
    ) %>%
    dplyr::select(
      iso3, country, region, area, fuel, year,
      num_fuel_users_thousands, per_capita_fuel_cons, all_of(demand_col)
    )

  # Copy the BaU records themselves for the frozen period. Besides preserving
  # shares, this guarantees byte-for-byte-equivalent numeric values in all
  # historical demand fields and prevents floating-point reconstruction noise.
  ics_no_overall <- bind_rows(
    bau_no_overall %>%
      dplyr::filter(year <= historical_bau_end_year) %>%
      dplyr::select(
        iso3, country, region, area, fuel, year,
        num_fuel_users_thousands, per_capita_fuel_cons, all_of(demand_col)
      ),
    ics_no_overall %>%
      dplyr::filter(year > historical_bau_end_year)
  ) %>%
    arrange(iso3, year, area, fuel)
  
  ## 12) BAU vs ICS COMPARISON ----
  # Compare the ICS demand to BaU across ALL countries (the previous version
  # used an anti-join against the anchor table, which is now empty because
  # anchor_points covers every country).
  #
  # The diff is reported on two fields:
  #   - num_fuel_users_thousands:         how the scenario reallocates population across fuels
  #                     (per (iso3, area, year, fuel)). Note: summed across
  #                     fuels per (iso3, area, year) this should be ~0
  #                     because total population per area/year is unchanged.
  #   - all_of(demand_col): the downstream demand quantity. The totals across
  #                     fuels DO change because each fuel has its own per_capita_fuel_cons
  #                     factor -- this is the real demand signal.
  
  cat(sprintf("BAU vs ICS%d snapshot for KEN urban %d:\n",
              anchor_num, end_year))
  bau_no_overall %>%
    dplyr::filter(iso3 == "KEN", area == "urban", year == end_year) %>%
    arrange(fuel) %>%
    print()
  
  ics_no_overall %>%
    dplyr::filter(iso3 == "KEN", area == "urban", year == end_year) %>%
    arrange(fuel) %>%
    print()
  
  bau_for_diff <- bau_no_overall %>%
    dplyr::select(iso3, country, region, area, year, fuel,
                  people_bau = num_fuel_users_thousands,
                  fuel_tons_calc_bau = all_of(demand_col))
  
  ics_for_diff <- ics_no_overall %>%
    dplyr::select(iso3, area, year, fuel,
                  people_ics = num_fuel_users_thousands,
                  fuel_tons_calc_ics = all_of(demand_col))
  
  diff_table <- bau_for_diff %>%
    left_join(ics_for_diff, by = c("iso3", "area", "year", "fuel")) %>%
    mutate(
      diff_people        = people_ics - people_bau,
      diff_fuel_tons     = fuel_tons_calc_ics - fuel_tons_calc_bau,
      pct_diff_people    = if_else(
        is.finite(people_bau) & people_bau != 0,
        100 * diff_people / people_bau,
        NA_real_
      ),
      pct_diff_fuel_tons = if_else(
        is.finite(fuel_tons_calc_bau) & fuel_tons_calc_bau != 0,
        100 * diff_fuel_tons / fuel_tons_calc_bau,
        NA_real_
      )
    ) %>%
    arrange(iso3, area, year, fuel)
  
  # Persist the full per-row diff so the user can inspect any country/year
  diff_path <- paste0(
    countrydir,
    "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/demand_bau_vs_ics",
    anchor_num,
    "_diff.csv"
  )
  write_csv(diff_table, diff_path)
  cat(sprintf("\033[32mWrote BaU-vs-ICS%d diff: %s\033[0m\n",
              anchor_num, diff_path))
  
  # Compact summary printed to console
  #   - max absolute diff (safe; uses [-Inf, Inf] guard so an all-NA group
  #     reports NA instead of -Inf)
  safe_max_abs <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) NA_real_ else max(abs(x))
  }
  
  diff_summary <- diff_table %>%
    summarise(
      max_abs_diff_people    = safe_max_abs(diff_people),
      max_abs_diff_fuel_tons = safe_max_abs(diff_fuel_tons),
      max_abs_pct_people     = safe_max_abs(pct_diff_people),
      max_abs_pct_fuel_tons  = safe_max_abs(pct_diff_fuel_tons)
    )
  cat(sprintf("\nICS%d vs BaU -- overall diff summary (all countries, all years, all fuels):\n",
              anchor_num))
  print(diff_summary)
  
  # End-year totals per country/area: how much does ICS shift total demand?
  end_year_totals <- diff_table %>%
    dplyr::filter(year == end_year) %>%
    group_by(iso3, area) %>%
    summarise(
      bau_fuel_tons = sum(fuel_tons_calc_bau, na.rm = TRUE),
      ics_fuel_tons = sum(fuel_tons_calc_ics, na.rm = TRUE),
      diff_tons     = ics_fuel_tons - bau_fuel_tons,
      pct_diff      = if_else(
        bau_fuel_tons != 0,
        100 * diff_tons / bau_fuel_tons,
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    arrange(desc(abs(pct_diff)))
  
  cat(sprintf("\nICS%d vs BaU -- top 10 (iso3, area) by |%% diff| in total %d fuel_tons:\n",
              anchor_num, end_year))
  print(head(end_year_totals, 10))
  
  # Sanity check: per-fuel `num_fuel_users_thousands` reallocation should sum to ~0 across
  # fuels within each (iso3, area, year).
  reallocation_check <- diff_table %>%
    group_by(iso3, area, year) %>%
    summarise(people_diff_sum = sum(diff_people, na.rm = TRUE), .groups = "drop") %>%
    summarise(max_abs_reallocation_sum = safe_max_abs(people_diff_sum))
  cat(sprintf("ICS%d people-reallocation sanity (max |row-sum of diff_people| per area/year, should be ~0):\n",
              anchor_num))
  print(reallocation_check)
  
  ics_no_overall
}

# Run the ICS pipeline for all three scenarios when an ICS scenario was selected
ics_outputs <- list()  # named list: "ICS1_v2" -> ics_no_overall tibble, ...

if (scenario_ver %in% ics_scenarios) {
  for (n in 1:3) {
    key <- sprintf("ICS%d_v2", n)
    ics_outputs[[key]] <- run_ics_pipeline(n)
  }
}

# Rebuilding ----
## 1) REBUILD overall FOR BAU ----
bau_overall <- bau_no_overall %>%
  group_by(iso3, country, region, fuel, year) %>%
  summarise(
    area = "overall",
    num_fuel_users_thousands = sum(num_fuel_users_thousands, na.rm = TRUE),
    !!demand_col := sum(.data[[demand_col]], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    per_capita_fuel_cons = if_else(
      num_fuel_users_thousands > 0,
      .data[[demand_col]] / (num_fuel_users_thousands * 1000),
      NA_real_
    )
  ) %>%
  dplyr::select(
    iso3, country, region, area, fuel, year,
    num_fuel_users_thousands, per_capita_fuel_cons, all_of(demand_col)
  )

bau_mofuss_or_rebuilt <- bind_rows(
  bau_no_overall %>%
    dplyr::select(iso3, country, region, area, fuel, year, num_fuel_users_thousands, per_capita_fuel_cons, all_of(demand_col)),
  bau_overall
) %>%
  arrange(iso3, year, area, fuel)

# Helper: rebuild the "overall" rows and bind with non-overall for one ICS scenario
rebuild_ics_with_overall <- function(ics_no_overall) {
  ics_overall <- ics_no_overall %>%
    group_by(iso3, country, region, fuel, year) %>%
    summarise(
      area = "overall",
      num_fuel_users_thousands = sum(num_fuel_users_thousands, na.rm = TRUE),
      !!demand_col := sum(.data[[demand_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      per_capita_fuel_cons = if_else(
        num_fuel_users_thousands > 0,
        .data[[demand_col]] / (num_fuel_users_thousands * 1000),
        NA_real_
      )
    ) %>%
    dplyr::select(
      iso3, country, region, area, fuel, year,
      num_fuel_users_thousands, per_capita_fuel_cons, all_of(demand_col)
    )
  
  bind_rows(
    ics_no_overall %>%
      dplyr::select(
        iso3, country, region, area, fuel, year,
        num_fuel_users_thousands, per_capita_fuel_cons, all_of(demand_col)
      ),
    ics_overall
  ) %>%
    arrange(iso3, year, area, fuel)
}

# Build the full ICS tables (with overall) for each scenario
ics_full_outputs <- list()
if (scenario_ver %in% ics_scenarios) {
  for (key in names(ics_outputs)) {
    ics_full_outputs[[key]] <- rebuild_ics_with_overall(ics_outputs[[key]])

    # Freeze the complete published table (including "overall" rows) to the
    # exact BaU records through 2025.
    ics_full_outputs[[key]] <- bind_rows(
      bau_mofuss_or_rebuilt %>%
        dplyr::filter(year <= historical_bau_end_year),
      ics_full_outputs[[key]] %>%
        dplyr::filter(year > historical_bau_end_year)
    ) %>%
      arrange(iso3, year, area, fuel)

    historical_bau <- bau_mofuss_or_rebuilt %>%
      dplyr::filter(year <= historical_bau_end_year)
    historical_ics <- ics_full_outputs[[key]] %>%
      dplyr::filter(year <= historical_bau_end_year)

    if (!identical(historical_ics, historical_bau)) {
      stop(sprintf(
        "%s output does not exactly match BaU through %d.",
        key, historical_bau_end_year
      ))
    }

    cat(sprintf(
      "\033[32m%s output matches BaU exactly through %d.\033[0m\n",
      key, historical_bau_end_year
    ))
  }
}

bau_mofuss_or_rebuilt %>%
  dplyr::filter(area == "overall", iso3 == "KEN", year == 2030) %>%
  arrange(fuel) %>%
  print()

if (scenario_ver %in% ics_scenarios) {
  for (key in names(ics_full_outputs)) {
    cat(sprintf("\n%s -- KEN overall 2030:\n", key))
    ics_full_outputs[[key]] %>%
      dplyr::filter(area == "overall", iso3 == "KEN", year == 2030) %>%
      arrange(fuel) %>%
      print()
  }
}

bau_mofuss_or_rebuilt %>%
  count(area) %>%
  print()
if (scenario_ver %in% ics_scenarios) {
  for (key in names(ics_full_outputs)) {
    cat(sprintf("\n%s area counts:\n", key))
    ics_full_outputs[[key]] %>%
      count(area) %>%
      print()
  }
}

# write_csv(
#   bau_mofuss_or_rebuilt,
#   paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/demand_bau_v2.csv")
# ) # if demand_bau_v2_user.csv exists...

# ---------------------------------------------------------------------------
# Write outputs ----
# ---------------------------------------------------------------------------

if (scenario_ver %in% bau_scenarios) {
  
  cat("\033[32mBaU scenario selected: no demand table for ICS scenarios will be written.\033[0m\n")
  
  write_csv(
    bau_mofuss_or_rebuilt,
    paste0(
      countrydir,
      "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/demand_",
      tolower(scenario_ver),
      ".csv"
    )
  )
  
} else if (scenario_ver %in% ics_scenarios) {
  
  # Write ALL three demand_icsN_v2.csv files regardless of which specific
  # ICS scenario was selected in scenario_ver.
  for (key in names(ics_full_outputs)) {
    out_path <- paste0(
      countrydir,
      "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/demand_",
      tolower(key),
      ".csv"
    )
    write_csv(ics_full_outputs[[key]], out_path)
    cat(sprintf("\033[32mWrote: %s\033[0m\n", out_path))
  }
  
} else {
  
  stop(sprintf("Invalid scenario_ver: %s", scenario_ver))
  
}
