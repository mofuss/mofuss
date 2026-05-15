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
# Version 3 -- adapted to derive 2000 / 2025 shares from demand_bau1_v2.csv
# Date: May 2026

# Load libraries ----
library(conflicted)

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(countrycode)
library(purrr)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

# Detect OS
os <- Sys.info()["sysname"]

setwd(countrydir)
getwd()

# Read parameters table ----
detect_delimiter <- function(file_path) {
  first_line <- readLines(file_path, n = 1)
  if (grepl(";", first_line)) {
    return(";")
  } else {
    return(",")
  }
}
delimiter <- detect_delimiter(parameters_file_path)
country_parameters <- read_delim(parameters_file_path, delim = delimiter)
print(tibble::as_tibble(country_parameters), n = 100)

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

# Pull the ISO3 from country_parameters; fall back to "KEN" if not present
iso3_code <- country_parameters %>%
  dplyr::filter(Var %in% c("iso3", "ISO3", "iso", "country_iso3")) %>%
  pull(ParCHR) %>%
  { if (length(.) == 0) "KEN" else .[1] }

cat(sprintf("\033[36mCountry ISO3: %s\033[0m\n", iso3_code))

# ---------------------------------------------------------------------------
# 1. Read demand_bau1_v2.csv and derive observed shares for 2000 and 2025 ----
# ---------------------------------------------------------------------------

# Canonical fuel ordering used by the model
fuel_order <- c(
  "fuelwood", "charcoal", "imp_fuelwood", "imp_charcoal",
  "gas", "kerosene", "electric", "pellets",
  "ethanol", "biogas", "coal"
)

demand_in_dir <- file.path(
  countrydir,
  "LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in"
)

demand_bau_path <- file.path(demand_in_dir, "demand_bau1_v2.csv")

if (!file.exists(demand_bau_path)) {
  stop(sprintf("Cannot find demand_bau1_v2.csv at: %s", demand_bau_path))
}

demand_bau <- read_csv(demand_bau_path, show_col_types = FALSE)

# Filter to the current country, rural + urban only, and the anchor years.
# Multiply the people column by 1000 (per user instruction) so the values
# represent actual people rather than thousands of people. Note: for share
# calculations this multiplier cancels out, but it is applied here for
# transparency and so any downstream user inspection sees real headcounts.
demand_country <- demand_bau %>%
  dplyr::filter(
    iso3 == iso3_code,
    area %in% c("rural", "urban"),
    year %in% c(2000, 2025, 2050)
  ) %>%
  mutate(people = people * 1000) %>%
  dplyr::select(iso3, area, year, fuel, people)

# Helper: compute area/year shares from the people column
compute_shares <- function(df) {
  df %>%
    group_by(area, year) %>%
    mutate(
      total_people = sum(people, na.rm = TRUE),
      share_user = if_else(total_people > 0, people / total_people, 0)
    ) %>%
    ungroup() %>%
    dplyr::select(iso3, area, year, fuel, share_user)
}

shares_observed <- compute_shares(demand_country)

# Sanity-check print: confirm shares sum to 1 within each area/year
shares_observed %>%
  group_by(area, year) %>%
  summarise(sum_share = sum(share_user), .groups = "drop") %>%
  print()

# Split out 2000 / 2025 (data-driven) vs 2050 (BaU, used only for comparison)
shares_2000_2025 <- shares_observed %>% dplyr::filter(year %in% c(2000, 2025))
shares_2050_bau  <- shares_observed %>% dplyr::filter(year == 2050)

# ---------------------------------------------------------------------------
# 2. User-defined 2050 targets for the three ICS scenarios ----
# ---------------------------------------------------------------------------
# These are the only values the user is allowed to tweak. The fuel ordering
# below must match `fuel_order` above.

make_2050_tibble <- function(rural_vec, urban_vec) {
  tibble(
    iso3 = iso3_code,
    area = rep(c("rural", "urban"), each = length(fuel_order)),
    year = 2050L,
    fuel = rep(fuel_order, 2),
    share_user = c(rural_vec, urban_vec)
  )
}

# Scenario 1: Biomass-improvement pathway
# Clean cooking expands mostly through improved fuelwood and improved charcoal.
anchors_2050_s1 <- make_2050_tibble(
  rural_vec = c(0.10, 0.05, 0.45, 0.22, 0.05, 0.00, 0.03, 0.02, 0.01, 0.07, 0.00),
  urban_vec = c(0.03, 0.10, 0.25, 0.35, 0.12, 0.00, 0.08, 0.03, 0.01, 0.02, 0.01)
)

# Scenario 2: Modern-energy pathway
# Transition driven mainly by gas and electricity.
anchors_2050_s2 <- make_2050_tibble(
  rural_vec = c(0.02, 0.01, 0.03, 0.02, 0.56, 0.00, 0.28, 0.02, 0.02, 0.04, 0.00),
  urban_vec = c(0.00, 0.01, 0.01, 0.01, 0.62, 0.00, 0.32, 0.01, 0.01, 0.00, 0.01)
)

# Scenario 3: Mixed pathway
# Improved biomass still important; gas and electricity grow strongly.
anchors_2050_s3 <- make_2050_tibble(
  rural_vec = c(0.08, 0.03, 0.25, 0.12, 0.22, 0.01, 0.16, 0.03, 0.03, 0.07, 0.00),
  urban_vec = c(0.02, 0.04, 0.12, 0.16, 0.34, 0.00, 0.25, 0.02, 0.02, 0.02, 0.01)
)

# ---------------------------------------------------------------------------
# 3. Assemble the full anchor tables (2000 + 2025 from data, 2050 from user) ----
# ---------------------------------------------------------------------------

build_anchor_table <- function(shares_2000_2025, anchors_2050) {
  bind_rows(shares_2000_2025, anchors_2050) %>%
    # Enforce the canonical fuel order and year order
    mutate(
      fuel = factor(fuel, levels = fuel_order),
      area = factor(area, levels = c("rural", "urban"))
    ) %>%
    arrange(area, year, fuel) %>%
    mutate(
      fuel = as.character(fuel),
      area = as.character(area)
    )
}

anchors_user1 <- build_anchor_table(shares_2000_2025, anchors_2050_s1)
anchors_user2 <- build_anchor_table(shares_2000_2025, anchors_2050_s2)
anchors_user3 <- build_anchor_table(shares_2000_2025, anchors_2050_s3)

# ---------------------------------------------------------------------------
# 4. Normalize + round each anchor table (same logic as the original script) ----
# ---------------------------------------------------------------------------

fix_anchors <- function(df) {
  df %>%
    group_by(area, year) %>%
    mutate(
      total = sum(share_user),
      share_user = if_else(total > 0, share_user / total, 0)
    ) %>%
    mutate(
      share_user = round(share_user, 4)
    ) %>%
    mutate(
      diff = 1 - sum(share_user),
      share_user = if_else(row_number() == 1, share_user + diff, share_user)
    ) %>%
    ungroup() %>%
    dplyr::select(-total, -diff)
}

anchors_user_fixed1 <- fix_anchors(anchors_user1)
anchors_user_fixed2 <- fix_anchors(anchors_user2)
anchors_user_fixed3 <- fix_anchors(anchors_user3)

# ---------------------------------------------------------------------------
# 5. Write all three anchor_points CSVs ----
# ---------------------------------------------------------------------------
# Per the user's request, all three files are written regardless of
# scenario_ver, so the three ICS scenarios are always available side by side.

dir.create(demand_in_dir, recursive = TRUE, showWarnings = FALSE)

out_path1 <- file.path(demand_in_dir, "anchor_points1.csv")
out_path2 <- file.path(demand_in_dir, "anchor_points2.csv")
out_path3 <- file.path(demand_in_dir, "anchor_points3.csv")

write_csv(anchors_user_fixed1, out_path1)
write_csv(anchors_user_fixed2, out_path2)
write_csv(anchors_user_fixed3, out_path3)

cat(sprintf("\033[32mWrote: %s\033[0m\n", out_path1))
cat(sprintf("\033[32mWrote: %s\033[0m\n", out_path2))
cat(sprintf("\033[32mWrote: %s\033[0m\n", out_path3))

# ---------------------------------------------------------------------------
# 6. Verification: also write the 2050 shares as observed in demand_bau1_v2.csv ----
# ---------------------------------------------------------------------------
# This lets the user compare the BaU 2050 shares against the ICS 2050 anchors
# to confirm the dynamic-share machinery is working as expected. The 2000 and
# 2025 rows are included as well, so the file is a complete "observed shares"
# snapshot.

verification_table <- shares_observed %>%
  mutate(
    fuel = factor(fuel, levels = fuel_order),
    area = factor(area, levels = c("rural", "urban"))
  ) %>%
  arrange(area, year, fuel) %>%
  mutate(
    fuel = as.character(fuel),
    area = as.character(area),
    share_user = round(share_user, 4)
  )

verification_path <- file.path(demand_in_dir, "anchor_points_bau_observed.csv")
write_csv(verification_table, verification_path)
cat(sprintf("\033[32mWrote verification file: %s\033[0m\n", verification_path))

# Console diff: highlight how 2050 BaU shares compare to each ICS scenario
print_2050_comparison <- function(scenario_name, anchors_fixed) {
  cat(sprintf("\n\033[36m--- 2050 comparison: BaU observed vs %s ---\033[0m\n", scenario_name))
  comp <- shares_2050_bau %>%
    dplyr::select(area, fuel, bau_2050 = share_user) %>%
    full_join(
      anchors_fixed %>%
        dplyr::filter(year == 2050) %>%
        dplyr::select(area, fuel, ics_2050 = share_user),
      by = c("area", "fuel")
    ) %>%
    mutate(
      bau_2050  = round(bau_2050, 4),
      ics_2050  = round(ics_2050, 4),
      delta     = round(ics_2050 - bau_2050, 4),
      fuel      = factor(fuel, levels = fuel_order),
      area      = factor(area, levels = c("rural", "urban"))
    ) %>%
    arrange(area, fuel)
  print(comp, n = 100)
}

print_2050_comparison("ICS1 (biomass-improvement)", anchors_user_fixed1)
print_2050_comparison("ICS2 (modern-energy)",      anchors_user_fixed2)
print_2050_comparison("ICS3 (mixed)",              anchors_user_fixed3)

cat("\n\033[32mDone. Three ICS anchor_points files written, plus a BaU-observed verification file.\033[0m\n")

