# SPDX-License-Identifier: Apache-2.0
#
# Copyright 2025-2027 Universidad Nacional Autónoma de México
# and Stockholm Environment Institute
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# https://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# MoFuSS ----
# Script: 2a_ics_constructor_v7.R
# Version: 7
# Date: August 2026
# Execution: Source from RStudio; Dinamica EGO does not invoke this script directly.
#
# Purpose: Build multi-country 2000/2025/2050 anchor tables for the three ICS
# pathways, including the BAU-bounded ICS3 wood-fuel phase-out trajectory.
# Inputs: demand_bau1_v2.csv, parameters.csv and the inherited country/demand paths.
# Outputs: ICS anchor-point, verification and comparison CSV files in demand_in.
# Side effects: Changes working directory and overwrites the generated anchor tables.
# Method: 2000/2025 shares are country-specific; 2050 uses configured targets or
# observed BAU shares. ICS3 equals BAU through 2025 and reallocates displaced
# wood-fuel shares to gas, electricity, pellets, ethanol and biogas thereafter.

# 2dolist ----

# Internal parameters ----

# REMEMBER TO FIRST RUN "2b_oneschema_fix_v4.R" for the the BaU1 scenario: 
# scenario_ver <- "BaU1_v2"

# ===========================================================================
# SWITCH: set TRUE to force 2050 = BaU observed shares (validation mode) ----
# ===========================================================================
# When TRUE, the three anchor_points files are built using the 2050 shares
# observed in demand_bau1_v2.csv (NOT the user-defined ICS targets). In that
# mode all three output files will be identical to each other AND identical
# to anchor_points_bau_observed.csv -- a clean integrity check that the
# pipeline is wired up correctly.
# When FALSE, 2050 uses the user-defined ICS targets defined further below.
use_bau_2050 <- FALSE

# ===========================================================================
# CONVERGENCE FACTORS --------------------------------------------------------
# ===========================================================================
# Controls how far each country moves from its 2025 share toward each
# scenario's 2050 target. Per (country, area, fuel):
#
#     share_2050  =  (1 - c) * share_2025  +  c * target_2050
#
# c = 1.00 -> every country ends at the uniform target (most aggressive)
# c = 0.50 -> every country moves halfway from its 2025 mix toward the target
# c = 0.00 -> every country stays at its 2025 mix (no transition)
#
# Countries already close to the target barely move; countries far from it
# move more, in proportion to their gap. Ignored when use_bau_2050 = TRUE.
#
# Per-scenario values reflect each pathway's realism:
#   - Biomass-improvement: medium c. Stove swaps are deployable but pulling
#     already-clean countries toward biomass is undesirable, so keep moderate.
#   - Modern-energy: lower c. Hardest in rural areas (LPG distribution, grid
#     expansion), so the transition is incomplete by 2050.
#   - Mixed: medium c, blends both transition speeds. For ICS3, c controls
#     the allocation weights used to distribute displaced wood-fuel shares
#     among the clean replacement fuels. The annual wood-fuel reduction itself
#     is controlled by `ics3_phaseout_exponent` below; kerosene and coal remain
#     on their same-year BaU paths.
convergence_s1 <- 0.50   # Biomass-improvement
convergence_s2 <- 0.90   # Modern-energy
convergence_s3 <- 0.70   # Wood phase-out with mixed clean replacements

# ICS3 annual phase-out path. For year y after 2025:
#
#   reduction_fraction(y) = ((y - 2025) / (2050 - 2025)) ^ exponent
#   ICS3_wood_share(y)     = BAU_wood_share(y) * (1 - reduction_fraction(y))
#
# exponent = 1 gives a linear increase in ambition relative to the same-year
# BaU: 4% below BaU in 2026, 20% below in 2030, and 100% below (zero) in 2050.
# Values below 1 act faster in early years; values above 1 act more slowly.
ics3_phaseout_start_year <- 2025L
ics3_phaseout_end_year <- 2050L
ics3_phaseout_exponent <- 1.0

# Country printed in the on-screen comparison (full table is always written
# to anchor_points_comparison.csv regardless)
sample_country <- "KEN"

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

# ---------------------------------------------------------------------------
# Read parameters table ----
# ---------------------------------------------------------------------------
detect_delimiter <- function(file_path) {
  first_line <- readLines(file_path, n = 1)
  if (grepl(";", first_line)) ";" else ","
}
delimiter <- detect_delimiter(parameters_file_path)
country_parameters <- read_delim(parameters_file_path, delim = delimiter)
print(tibble::as_tibble(country_parameters), n = 100)

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
# scenario_ver <- "ICS1_v2"

country_parameters %>%
  dplyr::filter(Var == "demand_col") %>%
  pull(ParCHR) -> demand_col

# ---------------------------------------------------------------------------
# Constants ----
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

# ---------------------------------------------------------------------------
# 1. Read demand_bau1_v2.csv and derive observed shares for every country ----
# ---------------------------------------------------------------------------

demand_bau <- read_csv(demand_bau_path, show_col_types = FALSE)

# Filter to rural + urban for the complete 2000-2050 BaU trajectory.
# Multiply the people column by 1000 (per user instruction) so values
# represent actual people rather than thousands of people. For share
# calculations this multiplier cancels out, but it is applied here for
# transparency and so any downstream inspection sees real headcounts.
demand_all <- demand_bau %>%
  dplyr::filter(
    area %in% c("rural", "urban"),
    year %in% 2000:2050
  ) %>%
  mutate(num_fuel_users_thousands = num_fuel_users_thousands * 1000) %>%
  dplyr::select(iso3, area, year, fuel, num_fuel_users_thousands)

# Make sure every (iso3, area, year, fuel) combination exists so countries
# with missing rows still appear with share 0 rather than being dropped.
countries_in_data <- sort(unique(demand_all$iso3))

grid_full <- tidyr::expand_grid(
  iso3 = countries_in_data,
  area = c("rural", "urban"),
  year = 2000:2050,
  fuel = fuel_order
)

demand_all <- grid_full %>%
  left_join(demand_all, by = c("iso3", "area", "year", "fuel")) %>%
  mutate(num_fuel_users_thousands = tidyr::replace_na(num_fuel_users_thousands, 0))

# Compute annual BaU shares per (iso3, area, year).
shares_bau_annual <- demand_all %>%
  group_by(iso3, area, year) %>%
  mutate(
    total_num_fuel_users_thousands = sum(num_fuel_users_thousands, na.rm = TRUE),
    share_user   = if_else(total_num_fuel_users_thousands > 0, num_fuel_users_thousands / total_num_fuel_users_thousands, 0)
  ) %>%
  ungroup() %>%
  dplyr::select(iso3, area, year, fuel, share_user)

empty_bau_groups <- shares_bau_annual %>%
  group_by(iso3, area, year) %>%
  summarise(share_sum = sum(share_user), .groups = "drop") %>%
  dplyr::filter(share_sum <= 0)

if (nrow(empty_bau_groups) > 0) {
  print(head(empty_bau_groups, 20))
  stop("BaU contains one or more country-area-years with no fuel users.")
}

# Retain the legacy 2000/2025/2050 subset for scenarios 1 and 2, validation
# mode, and the endpoint comparison table.
shares_observed <- shares_bau_annual %>%
  dplyr::filter(year %in% c(2000, 2025, 2050))

cat(sprintf(
  "\033[36mDerived annual BaU shares for %d countries (%d rural+urban x year x fuel rows).\033[0m\n",
  length(countries_in_data),
  nrow(shares_bau_annual)
))

# Split out 2000 / 2025 (always data-driven) vs 2050 (BaU; used for validation)
shares_2000_2025 <- shares_observed %>% dplyr::filter(year %in% c(2000, 2025))
shares_2050_bau  <- shares_observed %>% dplyr::filter(year == 2050)

# ---------------------------------------------------------------------------
# 2. User-defined 2050 targets for the three ICS scenarios ----
# ---------------------------------------------------------------------------
# These vectors define the *direction* each scenario points toward. Each
# country's actual 2050 share is a convex combination of its 2025 share and
# this target, weighted by the per-scenario convergence factor (see top of
# script). When convergence == 1 the target is applied uniformly to every
# country.

# Pull each country's 2025 shares (the starting point for convergence)
shares_2025 <- shares_observed %>%
  dplyr::filter(year == 2025) %>%
  dplyr::select(iso3, area, fuel, share_2025 = share_user)

make_2050_for_all_countries <- function(rural_vec, urban_vec,
                                        convergence = 1) {
  # Build the per-(area, fuel) target tibble
  target_tbl <- tibble(
    area   = rep(c("rural", "urban"), each = length(fuel_order)),
    fuel   = rep(fuel_order, 2),
    target = c(rural_vec, urban_vec)
  )

  # Cross with countries, then blend with each country's 2025 share
  tidyr::expand_grid(iso3 = countries_in_data, target_tbl) %>%
    left_join(shares_2025, by = c("iso3", "area", "fuel")) %>%
    mutate(
      # If 2025 data is missing for a (iso3, area, fuel), treat it as 0
      share_2025 = tidyr::replace_na(share_2025, 0),
      share_user = (1 - convergence) * share_2025 + convergence * target,
      year       = 2050L
    ) %>%
    dplyr::select(iso3, area, year, fuel, share_user)
}

# Force selected fuels to zero while preserving the scenario's country- and
# area-specific transition logic for the remaining fuels. The phase-out share
# is reallocated only to `recipient_fuels`, in proportion to their shares in
# the unconstrained 2050 anchor. `protected_fuels` retain their unconstrained
# shares exactly and therefore receive none of the displaced wood share.
force_exact_phaseout <- function(anchor_2050, phaseout_fuels,
                                 recipient_fuels, protected_fuels) {
  expected_fuels <- sort(c(phaseout_fuels, recipient_fuels, protected_fuels))

  if (!identical(sort(fuel_order), expected_fuels)) {
    stop("Phase-out, recipient and protected fuel sets must partition fuel_order exactly.")
  }

  group_stats <- anchor_2050 %>%
    group_by(iso3, area, year) %>%
    summarise(
      recipient_total = sum(share_user[fuel %in% recipient_fuels]),
      protected_total = sum(share_user[fuel %in% protected_fuels]),
      .groups = "drop"
    )

  bad_groups <- group_stats %>%
    dplyr::filter(
      recipient_total <= 0 |
        protected_total < 0 |
        protected_total >= 1
    )

  if (nrow(bad_groups) > 0) {
    print(head(bad_groups, 20))
    stop("Cannot reallocate the phase-out share for one or more country-area groups.")
  }

  anchor_2050 %>%
    left_join(group_stats, by = c("iso3", "area", "year")) %>%
    mutate(
      share_user = case_when(
        fuel %in% phaseout_fuels ~ 0,
        fuel %in% recipient_fuels ~
          share_user / recipient_total * (1 - protected_total),
        fuel %in% protected_fuels ~ share_user,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::select(iso3, area, year, fuel, share_user)
}

# Build an explicit annual ICS3 trajectory from the same-year BaU shares.
# Wood-fuel shares are multiplied by a declining residual factor and can
# therefore never exceed BaU. The share removed from wood fuels is added only
# to the clean recipient fuels. Recipient weights come from the original
# country/area-specific mixed ICS3 endpoint, preserving the intended clean
# replacement mix. Protected fuels remain exactly on their annual BaU paths.
build_bau_bounded_phaseout <- function(
  bau_annual,
  replacement_anchor,
  phaseout_fuels,
  recipient_fuels,
  protected_fuels,
  start_year,
  end_year,
  exponent = 1
) {
  expected_fuels <- sort(c(phaseout_fuels, recipient_fuels, protected_fuels))
  if (!identical(sort(fuel_order), expected_fuels)) {
    stop("Phase-out, recipient and protected fuel sets must partition fuel_order exactly.")
  }
  if (length(start_year) != 1L || length(end_year) != 1L ||
      is.na(start_year) || is.na(end_year) || end_year <= start_year) {
    stop("ICS3 phase-out start/end years are invalid.")
  }
  if (length(exponent) != 1L || is.na(exponent) ||
      !is.finite(exponent) || exponent <= 0) {
    stop("ics3_phaseout_exponent must be one finite number greater than zero.")
  }

  expected_years <- seq.int(start_year + 1L, end_year)
  available_years <- sort(unique(bau_annual$year))
  missing_years <- setdiff(expected_years, available_years)
  if (length(missing_years) > 0L) {
    stop(sprintf(
      "BaU annual shares are missing ICS3 phase-out year(s): %s",
      paste(missing_years, collapse = ", ")
    ))
  }

  recipient_weights <- replacement_anchor %>%
    dplyr::filter(fuel %in% recipient_fuels) %>%
    group_by(iso3, area) %>%
    mutate(
      recipient_total = sum(share_user),
      recipient_weight = if_else(
        recipient_total > 0,
        share_user / recipient_total,
        NA_real_
      )
    ) %>%
    ungroup() %>%
    dplyr::select(iso3, area, fuel, recipient_weight)

  bad_weights <- recipient_weights %>%
    group_by(iso3, area) %>%
    summarise(
      n = dplyr::n(),
      weight_sum = sum(recipient_weight),
      any_invalid = any(!is.finite(recipient_weight) | recipient_weight < 0),
      .groups = "drop"
    ) %>%
    dplyr::filter(
      n != length(recipient_fuels) |
        any_invalid |
        abs(weight_sum - 1) > 1e-10
    )

  if (nrow(bad_weights) > 0) {
    print(head(bad_weights, 20))
    stop("Cannot derive valid country-area clean-fuel replacement weights for ICS3.")
  }

  bau_annual %>%
    dplyr::filter(year %in% expected_years) %>%
    rename(bau_share = share_user) %>%
    left_join(recipient_weights, by = c("iso3", "area", "fuel")) %>%
    group_by(iso3, area, year) %>%
    mutate(
      phaseout_progress =
        ((year - start_year) / (end_year - start_year)) ^ exponent,
      displaced_wood_share = sum(
        if_else(fuel %in% phaseout_fuels, bau_share * phaseout_progress, 0)
      ),
      share_user = case_when(
        fuel %in% phaseout_fuels ~
          bau_share * (1 - phaseout_progress),
        fuel %in% recipient_fuels ~
          bau_share + displaced_wood_share * recipient_weight,
        fuel %in% protected_fuels ~ bau_share,
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    dplyr::select(iso3, area, year, fuel, share_user)
}

# Scenario 1: Biomass-improvement pathway
# Cleaner biomass cookstoves displace traditional ones. The bulk of the target
# is imp_fuelwood + imp_charcoal; gas/electric have modest roles; kerosene
# and coal are zero (phased out); biogas plays a small rural role.
# Fuel order: fuelwood, charcoal, imp_fuelwood, imp_charcoal, gas, kerosene,
#             electric, pellets, ethanol, biogas, coal
anchors_2050_s1 <- make_2050_for_all_countries(
  rural_vec   = c(0.05, 0.02, 0.45, 0.25, 0.10, 0.00, 0.04, 0.02, 0.01, 0.06, 0.00),
  urban_vec   = c(0.02, 0.05, 0.20, 0.25, 0.30, 0.00, 0.12, 0.03, 0.01, 0.02, 0.00),
  convergence = convergence_s1
)

# Scenario 2: Modern-energy pathway
# LPG (gas) and electric dominate. Improved biomass plays only a small
# transitional role. Urban targets are more aggressive (existing infra).
anchors_2050_s2 <- make_2050_for_all_countries(
  rural_vec   = c(0.05, 0.02, 0.10, 0.05, 0.45, 0.00, 0.25, 0.03, 0.02, 0.03, 0.00),
  urban_vec   = c(0.01, 0.02, 0.02, 0.02, 0.55, 0.00, 0.34, 0.02, 0.01, 0.01, 0.00),
  convergence = convergence_s2
)

# Scenario 3: Exact wood-fuel phase-out with mixed clean replacements
# Start with the original mixed-pathway anchor so the relative mix remains
# country-specific and distinguishes rural from urban areas. Then set
# fuelwood, charcoal, imp_fuelwood and imp_charcoal to exactly zero. Their
# combined share is distributed only across gas, electric, pellets, ethanol
# and biogas, proportional to each country's unconstrained ICS3 mix.
# Kerosene and coal retain their prior ICS3 shares but receive no reallocation.
anchors_2050_s3_unconstrained <- make_2050_for_all_countries(
  rural_vec   = c(0.05, 0.02, 0.25, 0.13, 0.28, 0.00, 0.15, 0.03, 0.02, 0.07, 0.00),
  urban_vec   = c(0.02, 0.03, 0.10, 0.13, 0.42, 0.00, 0.25, 0.03, 0.01, 0.01, 0.00),
  convergence = convergence_s3
)

phaseout_fuels_s3 <- c(
  "fuelwood", "charcoal", "imp_fuelwood", "imp_charcoal"
)

recipient_fuels_s3 <- c(
  "gas", "electric", "pellets", "ethanol", "biogas"
)

protected_fuels_s3 <- c("kerosene", "coal")

anchors_2050_s3 <- force_exact_phaseout(
  anchors_2050_s3_unconstrained,
  phaseout_fuels = phaseout_fuels_s3,
  recipient_fuels = recipient_fuels_s3,
  protected_fuels = protected_fuels_s3
)

anchors_annual_s3 <- build_bau_bounded_phaseout(
  bau_annual = shares_bau_annual,
  replacement_anchor = anchors_2050_s3,
  phaseout_fuels = phaseout_fuels_s3,
  recipient_fuels = recipient_fuels_s3,
  protected_fuels = protected_fuels_s3,
  start_year = ics3_phaseout_start_year,
  end_year = ics3_phaseout_end_year,
  exponent = ics3_phaseout_exponent
)

# ---------------------------------------------------------------------------
# 3. Pick the 2050 source based on the switch ----
# ---------------------------------------------------------------------------

if (isTRUE(use_bau_2050)) {
  cat(paste0(
    "\033[33m[VALIDATION MODE] use_bau_2050 = TRUE: ",
    "all three ICS scenarios will use BaU observed 2050 shares.\n",
    "The three anchor_points files should end up IDENTICAL to ",
    "anchor_points_bau_observed.csv. ",
    "(convergence factors are ignored in this mode.)\033[0m\n"
  ))
  anchors_2050_used_s1 <- shares_2050_bau
  anchors_2050_used_s2 <- shares_2050_bau
  anchors_future_used_s3 <- shares_2050_bau
} else {
  cat(sprintf(
    "\033[36muse_bau_2050 = FALSE: applying ICS 2050 targets with convergence c1=%.2f, c2=%.2f, c3=%.2f.\033[0m\n",
    convergence_s1, convergence_s2, convergence_s3
  ))
  anchors_2050_used_s1 <- anchors_2050_s1
  anchors_2050_used_s2 <- anchors_2050_s2
  anchors_future_used_s3 <- anchors_annual_s3
}

# ---------------------------------------------------------------------------
# 4. Assemble + normalize each anchor table ----
# ---------------------------------------------------------------------------

build_anchor_table <- function(shares_2000_2025, anchors_2050) {
  bind_rows(shares_2000_2025, anchors_2050) %>%
    mutate(
      fuel = factor(fuel, levels = fuel_order),
      area = factor(area, levels = c("rural", "urban"))
    ) %>%
    arrange(iso3, area, year, fuel) %>%
    mutate(
      fuel = as.character(fuel),
      area = as.character(area)
    )
}

# Same logic as the original script, now grouped by iso3 as well so the
# rounding correction is applied per country.
#
# The diff from rounding is added to the row with the LARGEST share rather
# than the first row. The original "row 1" rule fails for groups where the
# first fuel (fuelwood) has share 0 -- the diff (typically +/- 0.0001) would
# push fuelwood to -0.0001, producing an invalid negative share. Targeting
# the largest share is safe because (a) it's guaranteed > 0 so a small
# negative diff cannot drive it below 0, and (b) it's typically well below 1
# so a small positive diff cannot push it above 1. A final clamp + assertion
# guards against any pathological case slipping through.
fix_anchors <- function(df, digits = 4L) {
  if (length(digits) != 1L || is.na(digits) ||
      digits < 0 || digits > 15 || digits != as.integer(digits)) {
    stop("Anchor rounding digits must be one integer between 0 and 15.")
  }

  df %>%
    group_by(iso3, area, year) %>%
    mutate(
      total      = sum(share_user, na.rm = TRUE),
      share_user = if_else(total > 0, share_user / total, 0)
    ) %>%
    mutate(
      share_user = round(share_user, digits)
    ) %>%
    mutate(
      diff       = 1 - sum(share_user),
      # Apply diff to the row with the largest share in the group
      share_user = if_else(
        row_number() == which.max(share_user),
        share_user + diff,
        share_user
      )
    ) %>%
    ungroup() %>%
    dplyr::select(-total, -diff)
}

# Validator: every share must lie in [0, 1] and every (iso3, area, year)
# group must sum to 1 (with a small tolerance for floating-point error).
check_shares <- function(df, label) {
  bad_range <- df %>% dplyr::filter(share_user < 0 | share_user > 1)
  bad_sum <- df %>%
    group_by(iso3, area, year) %>%
    summarise(s = sum(share_user), .groups = "drop") %>%
    dplyr::filter(abs(s - 1) > 1e-3)

  if (nrow(bad_range) > 0) {
    cat(sprintf(
      "\033[31m[FAIL] %s: %d rows outside [0, 1]. Sample:\033[0m\n",
      label, nrow(bad_range)
    ))
    print(head(bad_range, 10))
    stop(sprintf("share_user out of bounds in %s", label))
  }
  if (nrow(bad_sum) > 0) {
    cat(sprintf(
      "\033[31m[FAIL] %s: %d groups not summing to 1. Sample:\033[0m\n",
      label, nrow(bad_sum)
    ))
    print(head(bad_sum, 10))
    stop(sprintf("group sums != 1 in %s", label))
  }
  cat(sprintf(
    "\033[32m[OK] %s: all %d rows in [0,1], all %d groups sum to 1.\033[0m\n",
    label, nrow(df),
    nrow(dplyr::distinct(df, iso3, area, year))
  ))
}

# Scenario-specific endpoint validator. This is separate from check_shares()
# because a valid set of shares could still fail the exact phase-out policy.
check_exact_phaseout <- function(df, label, phaseout_fuels, endpoint_year = 2050L) {
  bad_endpoint <- df %>%
    dplyr::filter(
      year == endpoint_year,
      fuel %in% phaseout_fuels,
      abs(share_user) > 1e-12
    )

  if (nrow(bad_endpoint) > 0) {
    print(head(bad_endpoint, 20))
    stop(sprintf("%s does not reach the exact %d phase-out endpoint.",
                 label, endpoint_year))
  }

  cat(sprintf(
    "\033[32m[OK] %s: %s are exactly zero in %d.\033[0m\n",
    label, paste(phaseout_fuels, collapse = ", "), endpoint_year
  ))
}

# Scenario-3 policy validator. This checks the property that the old 2025-to-
# 2050 straight-line interpolation did not guarantee: every annual wood-fuel
# share must follow the configured reduction from its same-year BaU value.
check_bau_bounded_phaseout <- function(
  df,
  bau_annual,
  label,
  phaseout_fuels,
  recipient_fuels,
  protected_fuels,
  replacement_anchor,
  start_year,
  end_year,
  exponent
) {
  years <- seq.int(start_year + 1L, end_year)
  expected_rows <- bau_annual %>%
    dplyr::filter(
      year %in% years,
      fuel %in% c(phaseout_fuels, recipient_fuels, protected_fuels)
    ) %>%
    rename(bau_share = share_user)

  recipient_weights <- replacement_anchor %>%
    dplyr::filter(fuel %in% recipient_fuels) %>%
    group_by(iso3, area) %>%
    mutate(recipient_weight = share_user / sum(share_user)) %>%
    ungroup() %>%
    dplyr::select(iso3, area, fuel, recipient_weight)

  actual_rows <- df %>%
    dplyr::filter(
      year %in% years,
      fuel %in% c(phaseout_fuels, recipient_fuels, protected_fuels)
    ) %>%
    dplyr::select(iso3, area, year, fuel, share_user)

  checked <- expected_rows %>%
    left_join(actual_rows, by = c("iso3", "area", "year", "fuel")) %>%
    left_join(recipient_weights, by = c("iso3", "area", "fuel")) %>%
    group_by(iso3, area, year) %>%
    mutate(
      progress = ((year - start_year) / (end_year - start_year)) ^ exponent,
      displaced_wood_share = sum(
        if_else(fuel %in% phaseout_fuels, bau_share * progress, 0)
      ),
      expected_share = case_when(
        fuel %in% phaseout_fuels ~ bau_share * (1 - progress),
        fuel %in% recipient_fuels ~
          bau_share + displaced_wood_share * recipient_weight,
        fuel %in% protected_fuels ~ bau_share,
        TRUE ~ NA_real_
      ),
      difference = share_user - expected_share
    ) %>%
    ungroup()

  missing <- checked %>% dplyr::filter(is.na(share_user))
  if (nrow(missing) > 0) {
    print(head(missing, 20))
    stop(sprintf("%s is missing annual policy rows.", label))
  }

  bad_formula <- checked %>%
    dplyr::filter(abs(difference) > 1e-10)
  if (nrow(bad_formula) > 0) {
    print(head(bad_formula, 20))
    stop(sprintf("%s does not follow the configured annual BaU-relative path.", label))
  }

  bad_ceiling <- checked %>%
    dplyr::filter(
      fuel %in% phaseout_fuels,
      share_user > bau_share + 1e-12
    )
  if (nrow(bad_ceiling) > 0) {
    print(head(bad_ceiling, 20))
    stop(sprintf("%s has wood-fuel shares above same-year BaU.", label))
  }

  not_strictly_lower <- checked %>%
    dplyr::filter(
      fuel %in% phaseout_fuels,
      bau_share > 1e-7,
      share_user >= bau_share - 1e-12
    )
  if (nrow(not_strictly_lower) > 0) {
    print(head(not_strictly_lower, 20))
    stop(sprintf("%s has material BaU wood-fuel shares that were not reduced.", label))
  }

  cat(sprintf(
    paste0(
      "\033[32m[OK] %s: every annual wood-fuel share from %d-%d is ",
      "BaU-bounded and follows the configured phase-out path; protected ",
      "fuels remain on BaU.\033[0m\n"
    ),
    label, start_year + 1L, end_year
  ))
}

anchors_user_fixed1 <- fix_anchors(build_anchor_table(shares_2000_2025, anchors_2050_used_s1))
anchors_user_fixed2 <- fix_anchors(build_anchor_table(shares_2000_2025, anchors_2050_used_s2))
anchors_user_fixed3 <- fix_anchors(
  build_anchor_table(shares_2000_2025, anchors_future_used_s3),
  digits = if (isTRUE(use_bau_2050)) 4L else 12L
)

cat("\n\033[36m--- Share-range validation (must be in [0, 1] and sum to 1) ---\033[0m\n")
check_shares(anchors_user_fixed1, "anchor_points1")
check_shares(anchors_user_fixed2, "anchor_points2")
check_shares(anchors_user_fixed3, "anchor_points3")

if (!isTRUE(use_bau_2050)) {
  check_exact_phaseout(
    anchors_user_fixed3,
    "anchor_points3",
    phaseout_fuels_s3,
    endpoint_year = 2050L
  )
  check_bau_bounded_phaseout(
    anchors_user_fixed3,
    shares_bau_annual,
    "anchor_points3",
    phaseout_fuels_s3,
    recipient_fuels_s3,
    protected_fuels_s3,
    anchors_2050_s3,
    start_year = ics3_phaseout_start_year,
    end_year = ics3_phaseout_end_year,
    exponent = ics3_phaseout_exponent
  )
}

# ---------------------------------------------------------------------------
# 5. Write the three anchor_points CSVs ----
# ---------------------------------------------------------------------------

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
# 6. Write the BaU-observed shares (always, for comparison) ----
# ---------------------------------------------------------------------------

# Apply the same normalize + round + diff-correction as the ICS tables so
# that in validation mode (use_bau_2050 = TRUE) the three anchor_points
# files are byte-identical to this one.
verification_table <- shares_observed %>%
  mutate(
    fuel = factor(fuel, levels = fuel_order),
    area = factor(area, levels = c("rural", "urban"))
  ) %>%
  arrange(iso3, area, year, fuel) %>%
  mutate(
    fuel = as.character(fuel),
    area = as.character(area)
  ) %>%
  fix_anchors()

verification_path <- file.path(demand_in_dir, "anchor_points_bau_observed.csv")
write_csv(verification_table, verification_path)
cat(sprintf("\033[32mWrote verification file: %s\033[0m\n", verification_path))
check_shares(verification_table, "anchor_points_bau_observed")

# ---------------------------------------------------------------------------
# 7. Full multi-country comparison file ----
# ---------------------------------------------------------------------------
# Wide format: one row per (iso3, area, fuel) with 2000/2025/2050 BaU
# shares and the ICS 2050 shares for each scenario side by side.

bau_wide <- shares_observed %>%
  mutate(year_lbl = paste0("bau_", year)) %>%
  dplyr::select(iso3, area, fuel, year_lbl, share_user) %>%
  pivot_wider(names_from = year_lbl, values_from = share_user)

ics_2050_wide <- bind_rows(
  anchors_user_fixed1 %>% dplyr::filter(year == 2050) %>%
    dplyr::select(iso3, area, fuel, ics1_2050 = share_user),
  anchors_user_fixed2 %>% dplyr::filter(year == 2050) %>%
    dplyr::select(iso3, area, fuel, ics2_2050 = share_user),
  anchors_user_fixed3 %>% dplyr::filter(year == 2050) %>%
    dplyr::select(iso3, area, fuel, ics3_2050 = share_user)
) %>%
  group_by(iso3, area, fuel) %>%
  summarise(
    ics1_2050 = max(ics1_2050, na.rm = TRUE),
    ics2_2050 = max(ics2_2050, na.rm = TRUE),
    ics3_2050 = max(ics3_2050, na.rm = TRUE),
    .groups   = "drop"
  )

comparison_full <- bau_wide %>%
  full_join(ics_2050_wide, by = c("iso3", "area", "fuel")) %>%
  mutate(
    fuel = factor(fuel, levels = fuel_order),
    area = factor(area, levels = c("rural", "urban"))
  ) %>%
  arrange(iso3, area, fuel) %>%
  mutate(
    fuel = as.character(fuel),
    area = as.character(area),
    dplyr::across(where(is.numeric), ~ round(.x, 4))
  )

comparison_path <- file.path(demand_in_dir, "anchor_points_comparison.csv")
write_csv(comparison_full, comparison_path)
cat(sprintf("\033[32mWrote full comparison file: %s\033[0m\n", comparison_path))

# ---------------------------------------------------------------------------
# 8. Console preview for the sample country only ----
# ---------------------------------------------------------------------------

cat(sprintf(
  "\n\033[36m--- 2050 comparison for sample country %s ---\033[0m\n",
  sample_country
))

preview <- comparison_full %>%
  dplyr::filter(iso3 == sample_country) %>%
  dplyr::select(area, fuel, bau_2050, ics1_2050, ics2_2050, ics3_2050) %>%
  mutate(
    fuel = factor(fuel, levels = fuel_order),
    area = factor(area, levels = c("rural", "urban"))
  ) %>%
  arrange(area, fuel)

if (nrow(preview) == 0) {
  cat(sprintf(
    "\033[33m(sample country '%s' not found in the data)\033[0m\n",
    sample_country
  ))
} else {
  print(preview, n = 100)
}

# ---------------------------------------------------------------------------
# 9. Integrity check when use_bau_2050 = TRUE ----
# ---------------------------------------------------------------------------

if (isTRUE(use_bau_2050)) {
  identical_check <- isTRUE(all.equal(anchors_user_fixed1, anchors_user_fixed2)) &&
    isTRUE(all.equal(anchors_user_fixed2, anchors_user_fixed3))
  matches_bau <- isTRUE(all.equal(anchors_user_fixed1, verification_table))

  if (identical_check) {
    cat("\033[32m[OK] All three anchor_points files are identical to each other.\033[0m\n")
  } else {
    cat("\033[31m[WARN] anchor_points files differ from each other -- something is off.\033[0m\n")
  }
  if (matches_bau) {
    cat("\033[32m[OK] anchor_points{1,2,3} match anchor_points_bau_observed.csv exactly.\033[0m\n")
  } else {
    cat("\033[31m[WARN] anchor_points files do NOT match anchor_points_bau_observed.csv -- something is off.\033[0m\n")
  }
}

cat("\n\033[32mDone.\033[0m\n")
