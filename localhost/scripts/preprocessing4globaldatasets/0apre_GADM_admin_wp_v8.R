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
# Version 5
# Date: Apr 2026
#
# This script loads GADM admin 0/1/2 levels and selects countries with complete
# info in both the WHO and HRSL population maps. For Nepal use 3.
#
# This version supports four SSA regionalizations from a single script:
#   - Default (UNFCCC)        : subregionsSSA_v4.csv
#   - FAO Miombo-Mopane       : subregionsSSA_v5FAO.csv
#   - TNC Congo Basin         : subregionsSSA_v5TNC_congo.csv
#   - TNC KAZA                : subregionsSSA_v5TNC_kaza.csv
#
# To add a new SSA scenario later
# Three steps in this order:
# 1. Create the new CSV (subregionsSSA_v5TNC_cerrado.csv or whatever).
# 2. Add one line to ssa_scenarios: cerrado = "subregionsSSA_v5TNC_cerrado.csv".
# 3. If the new CSV introduces any brand-new Subregion labels, add them to ssa_region_map with unique suffixes.
# 
# ============================================================================
# Internal parameters
# ============================================================================
run_ms <- "Yes"  # Run ms_simplify?

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Pick ONE SSA scenario. The corresponding CSV is auto-selected below.
ssa_scenario <- "default"   # one of: "default", "fao", "tnc_congo", "tnc_kaza"
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

ssa_scenarios <- list(
  default   = "subregionsSSA_v4.csv",
  fao       = "subregionsSSA_v5FAO.csv",
  tnc_congo = "subregionsSSA_v5TNC_congo.csv",
  tnc_kaza  = "subregionsSSA_v5TNC_kaza.csv"
)
stopifnot(ssa_scenario %in% names(ssa_scenarios))
subregionsSSA_v <- ssa_scenarios[[ssa_scenario]]
message("SSA scenario: ", ssa_scenario, "  ->  ", subregionsSSA_v)

subregionsLATAM_v    <- "subregionsLATAM_v3.csv"
subregionsASIA_v     <- "subregionsASIA_v5.csv"
subregionsOCEANIA_v  <- "subregionsOCEANIA.csv"
subregionsNorAfri_v  <- "subregionsNorAfri_v3.csv"

# ----------------------------------------------------------------------------
# Region maps (single source of truth per major region)
# ----------------------------------------------------------------------------
# SSA: covers labels across all four scenarios. New scenarios add rows here
# only if they introduce a brand-new Subregion label.
ssa_region_map <- tibble::tribble(
  ~Subregion,                ~suffix,
  # --- Labels common to multiple scenarios ---
  "Eastern Africa",          "eastern",
  "Southern Africa",         "southern",
  "Western Africa",          "western",
  "Madagascar",              "madagascar",
  "São Tomé and Príncipe",   "stp",
  "Comoros",                 "comoros",
  "Mauritius",               "mauritius",
  "Central Africa",          "central",
  "Northcentral Africa",     "northcentral",
  "Kenya",                   "kenya",
  "Uganda",                  "uganda",
  "Mali",                    "mali",
  "Chad",                    "chad",
  "Niger",                   "niger",
  "Cabo Verde",              "caboverde",
  "Westcentral Africa",      "westcentral",
  "Westsouthern Africa",     "westsouthern",
  "Mauritania",              "mauritania",
  "Benin",                   "benin",
  "Burkina Faso",            "burkinafaso",
  "Côte d'Ivoire",           "cdivoire",
  "Ghana",                   "ghana",
  "Senegambia",              "senegambia",
  "Togo",                    "togo",
  # --- Default (v4) only ---
  "Angola",                  "angola",
  "Malambique",              "malambique",
  "Tanzania",                "tanzania",
  "Zambia",                  "zambia",
  "Zimbabwe",                "zimbabwe",
  # --- FAO ---
  "Miombo Mopane",           "miombo_mopane",
  # --- TNC Congo ---
  "Congo Basin",             "congo_basin",
  # --- TNC KAZA ---
  "KAZA",                    "kaza"
)

latam_region_map <- tibble::tribble(
  ~Subregion,           ~suffix,
  "Southern LATAM",     "southern",
  "Central America",    "CA",
  "Espanhola",          "espanhola",
  "Jamaica",            "jamaica",
  "Western LATAM",      "western",
  "Brazil",             "brazil",
  "Mexico",             "mexico",
  "Bolivia",            "bolivia",
  "Colombia",           "colombia",
  "Guyana",             "guyana",
  "El Salvador",        "salvador"
)
# LATAM CSV has rows tagged "erasefromregions" (Dominica, Grenada, Saint Lucia,
# Saint Vincent and the Grenadines, Suriname). v7 silently ignored them; we
# drop them explicitly.
latam_drop_labels <- c("erasefromregions")

asia_region_map <- tibble::tribble(
  ~Subregion,     ~suffix,
  "Central Asia", "central",
  "India",        "india",
  "SEAsia",       "seasia",
  "China",        "china",
  "Mongolia",     "mongolia",
  "Middle East",  "middleeast",
  "Sri Lanka",    "srilanka",
  "Indonesia",    "indonesia",
  "Malaysia",     "malaysia",
  "Philippines",  "philippines",
  "Timor-Leste",  "timorleste",
  "Bangladesh",   "bangladesh",
  "Bhutan",       "bhutan",
  "Nepal",        "nepal",
  "Myanmar",      "myanmar",
  "Pakistan",     "pakistan"
)

# v7 only builds Papua New Guinea, even though the CSV lists 10 countries.
# We replicate that exactly by mapping only PNG and dropping the rest.
oceania_region_map <- tibble::tribble(
  ~Subregion,         ~suffix,
  "Papua New Guinea", "papuanewguinea"
)
oceania_drop_labels <- c("Cook Islands","Fiji","Micronesia","Kiribati",
                         "Marshall Islands","Niue","Solomon Islands",
                         "Tonga","Tuvalu")

# NorAfri uses the suffix "west" — kept as-is for downstream compatibility,
# even though it's the only NorAfri region and contains all 3 N.African countries.
norafr_region_map <- tibble::tribble(
  ~Subregion,        ~suffix,
  "Northern Africa", "west"
)

# ============================================================================
# Load packages
# ============================================================================
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(base::intersect)
conflicts_prefer(base::setdiff)
conflicts_prefer(base::union)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::rename)

library(lwgeom)
library(magrittr)
library(sf)
library(tidyverse)
library(readxl)
library(readr)
library(rmapshaper)
library(svDialogs)

setwd(countrydir)
getwd()

# ============================================================================
# Read parameters table (UNCHANGED from v7)
# ============================================================================
if (webmofuss == 1) {
  country_parameters <- read_csv(parameters_file_path)
} else if (webmofuss == 0) {
  detect_delimiter <- function(file_path) {
    first_line <- readLines(file_path, n = 1)
    if (grepl(";", first_line)) ";" else ","
  }
  delimiter <- detect_delimiter(parameters_file_path)
  country_parameters <- read_delim(parameters_file_path, delim = delimiter)
  print(tibble::as_tibble(country_parameters), n = 100)
}

epsg_gcs <- country_parameters %>%
  dplyr::filter(Var == "epsg_gcs") %>% pull(ParCHR) %>% as.integer()
epsg_pcs <- country_parameters %>%
  dplyr::filter(Var == "epsg_pcs") %>% pull(ParCHR) %>% as.integer()
proj_authority <- country_parameters %>%
  dplyr::filter(Var == "proj_authority") %>% pull(ParCHR)

if (!exists("demanddir")) {
  choose_directory1 <- function(caption = "Choose the directory where demand_in files are") {
    if (.Platform$OS.type == "unix") {
      setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
    } else {
      setwd(choose.dir("/home/mofuss/Documents", caption = caption))
    }
  }
  choose_directory1()
  demanddir <- getwd()
}

if (!exists("admindir")) {
  choose_directory1 <- function(caption = "Choose the directory where admin_regions files are") {
    if (.Platform$OS.type == "unix") {
      setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
    } else {
      setwd(choose.dir("/home/mofuss/Documents", caption = caption))
    }
  }
  choose_directory1()
  admindir <- getwd()
}

setwd(admindir)

# Clean previous outputs
for (d in c("regions_adm0","regions_adm1","regions_adm2",
            "regions_adm0_p","regions_adm1_p","regions_adm2_p",
            "InVector","ecoregions","ecoregions_p")) {
  unlink(d, recursive = TRUE, force = TRUE)
  if (!dir.exists(d)) dir.create(d)
}

# ============================================================================
# GADM read + harmonize ADM0/ADM1/ADM2 (UNCHANGED)
# ============================================================================
recodedisputed <- function(adm_lyr){
  adm_lyr %>%
    mutate(GID_0 = recode(GID_0,
                          "Z01" = "IND", "Z02" = "CHN", "Z03" = "CHN",
                          "Z04" = "IND", "Z05" = "IND", "Z06" = "PAK",
                          "Z07" = "IND", "Z08" = "CHN", "Z09" = "IND"))
}

sf::sf_use_s2(FALSE)

# --- ADM0 ---
gadm_adm0_sel <- st_read("gadm_410-levels.gpkg", layer = "ADM_0") %>%
  dplyr::rename(NAME_0 = "COUNTRY") %>%
  dplyr::select(GID_0, NAME_0) %>%
  recodedisputed() %>%
  dplyr::filter(!is.na(GID_0), GID_0 != "NA")

gadm_adm0_sel_db <- gadm_adm0_sel %>% st_drop_geometry()
adm0_countries   <- unique(gadm_adm0_sel$GID_0)

# --- ADM1 ---
gadm_adm1_sel <- st_read("gadm_410-levels.gpkg", layer = "ADM_1") %>%
  dplyr::rename(NAME_0 = "COUNTRY") %>%
  dplyr::select(GID_0, NAME_0, GID_1, NAME_1) %>%
  recodedisputed() %>%
  dplyr::filter(!is.na(GID_0), GID_0 != "NA", GID_0 %in% adm0_countries)
gadm_adm1_sel$NAME_1 <- gsub("[,/.()+]", "", gadm_adm1_sel$NAME_1)

missing_adm1 <- setdiff(adm0_countries, unique(gadm_adm1_sel$GID_0))
adm0_as_adm1 <- gadm_adm0_sel %>%
  dplyr::filter(GID_0 %in% missing_adm1) %>%
  dplyr::mutate(GID_1 = paste0(GID_0, ".0_1"), NAME_1 = NAME_0) %>%
  dplyr::select(GID_0, NAME_0, GID_1, NAME_1)
gadm_adm1_sel <- bind_rows(gadm_adm1_sel, adm0_as_adm1) %>%
  dplyr::filter(GID_0 %in% adm0_countries)
gadm_adm1_sel_db <- gadm_adm1_sel %>% st_drop_geometry()

# --- ADM2 ---
gadm_adm2_sel <- st_read("gadm_410-levels.gpkg", layer = "ADM_2") %>%
  dplyr::rename(NAME_0 = "COUNTRY") %>%
  dplyr::select(GID_0, NAME_0, GID_1, NAME_1, GID_2, NAME_2) %>%
  recodedisputed() %>%
  dplyr::filter(!is.na(GID_0), GID_0 != "NA", GID_0 %in% adm0_countries)
gadm_adm2_sel$NAME_1 <- gsub("[,/.()+]", "", gadm_adm2_sel$NAME_1)
gadm_adm2_sel$NAME_2 <- gsub("[,/.()+]", "", gadm_adm2_sel$NAME_2)

missing_adm2 <- setdiff(adm0_countries, unique(gadm_adm2_sel$GID_0))
adm1_as_adm2 <- gadm_adm1_sel %>%
  dplyr::filter(GID_0 %in% missing_adm2) %>%
  dplyr::mutate(GID_2 = paste0(GID_1, ".0_2"), NAME_2 = NAME_1) %>%
  dplyr::select(GID_0, NAME_0, GID_1, NAME_1, GID_2, NAME_2)
gadm_adm2_sel <- bind_rows(gadm_adm2_sel, adm1_as_adm2) %>%
  dplyr::filter(GID_0 %in% adm0_countries)

still_missing_adm2 <- setdiff(adm0_countries, unique(gadm_adm2_sel$GID_0))
adm0_as_adm2 <- gadm_adm0_sel %>%
  dplyr::filter(GID_0 %in% still_missing_adm2) %>%
  dplyr::mutate(GID_1 = paste0(GID_0, ".0_1"), NAME_1 = NAME_0,
                GID_2 = paste0(GID_0, ".0_1.0_2"), NAME_2 = NAME_0) %>%
  dplyr::select(GID_0, NAME_0, GID_1, NAME_1, GID_2, NAME_2)
gadm_adm2_sel <- bind_rows(gadm_adm2_sel, adm0_as_adm2) %>%
  dplyr::filter(GID_0 %in% adm0_countries)
gadm_adm2_sel_db <- gadm_adm2_sel %>% st_drop_geometry()

# ============================================================================
# WHO regions and per-major-region adm0 layers (UNCHANGED from v7)
# ============================================================================
setwd(githubdir)
whodb <- read_excel("demand_tables/A_LMIC_Estimates_2050_popmedian_original.xlsx")
whodb_sel_u <- whodb %>% dplyr::select(iso3, country, region) %>% unique()
regions_u   <- unique(whodb_sel_u$region)

region1        <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[1])
region2        <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[2])
regionNorAfr   <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[3]) %>%
  filter(!iso3 %in% c("AZE","GEO","IRQ","JOR","SYR","TUR","YEM","ARM","SDN"))
regionEastEuro <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[3]) %>%
  filter(!iso3 %in% c("EGY","DZA","MAR","TUN","YEM","SDN"))
regionSSA      <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[4])
regionLATAM    <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[5]) %>% filter(iso3 != "BLZ")
region6        <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[6]) %>% filter(iso3 != "PRK")
region7        <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[7])

R1_adm0          <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% region1$iso3)
R2_adm0          <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% region2$iso3)
NorAfr_adm0      <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% regionNorAfr$iso3)
NorEastEuro_adm0 <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% regionEastEuro$iso3)
SSA_adm0         <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% regionSSA$iso3)
LATAM_adm0       <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% regionLATAM$iso3)
R6_adm0          <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% region6$iso3)
OCEANIA_adm0     <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% region7$iso3)

ASIA_adm0 <- rbind(R1_adm0, R6_adm0, NorEastEuro_adm0) %>%
  ms_dissolve(field = "GID_0", copy_fields = c("NAME_0")) %>%
  dplyr::select(GID_0, NAME_0)

setwd(admindir)

# ============================================================================
# Generic helper: build, write and project per-subregion layers
# ============================================================================
# Args:
#   prefix         : "SSA", "LATAM", "ASIA", "OCEANIA", "NorAfr"
#   subregions_df  : CSV table with cols Subregion, GID_0, NAME_0
#   adm0_sf        : the major-region adm0 sf (e.g. SSA_adm0, LATAM_adm0)
#   adm1_sf, adm2_sf : GADM adm1 / adm2 sf (filtered by GID_0 per region)
#   region_map     : tibble(Subregion, suffix)  -- one row per built region
#   pcs            : projected EPSG (epsg_pcs)
#   proj_auth      : "EPSG" etc.
#   drop_labels    : character vector of Subregion labels to drop before build
#                    (default character(0)).  Use for LATAM "erasefromregions"
#                    and OCEANIA's non-PNG countries.
#
# Returns: invisibly a named list of the per-region adm0 sf objects.
build_and_write_regions <- function(prefix, subregions_df, adm0_sf,
                                    adm1_sf, adm2_sf,
                                    region_map, pcs, proj_auth,
                                    drop_labels = character(0)) {
  
  # Drop rows the caller doesn't want built
  if (length(drop_labels) > 0) {
    subregions_df <- subregions_df %>% filter(!Subregion %in% drop_labels)
  }
  
  # Sanity: every remaining label MUST be in region_map
  csv_labels <- unique(subregions_df$Subregion)
  unknown    <- setdiff(csv_labels, region_map$Subregion)
  if (length(unknown) > 0) {
    stop(sprintf("[%s] Subregion labels missing from region_map: %s\n  Add them to the map or to drop_labels.",
                 prefix, paste(unknown, collapse = ", ")))
  }
  
  # Join CSV to major-region adm0 by GID_0 (replicates the right_join pattern)
  adm0_sub <- adm0_sf %>%
    right_join(subregions_df, by = "GID_0") %>%
    dplyr::select(-NAME_0.y) %>%
    dplyr::rename(NAME_0 = NAME_0.x) %>%
    filter(!is.na(NAME_0))
  
  message(sprintf("[%s] %d countries across %d subregions",
                  prefix, nrow(adm0_sub), length(csv_labels)))
  
  out <- list()
  for (lab in intersect(region_map$Subregion, unique(adm0_sub$Subregion))) {
    suf       <- region_map$suffix[region_map$Subregion == lab]
    iso_codes <- adm0_sub$GID_0[adm0_sub$Subregion == lab]
    name0     <- paste0(prefix, "_adm0_", suf)
    name1     <- paste0(prefix, "_adm1_", suf)
    name2     <- paste0(prefix, "_adm2_", suf)
    
    # adm0
    sub0 <- adm0_sub %>% filter(Subregion == lab) %>%
      mutate(ID = seq_len(n()), mofuss_reg = name0)
    st_write(sub0, sprintf("regions_adm0/%s.gpkg", name0), delete_layer = TRUE)
    sub0 %>% st_transform(paste0(proj_auth, ":", pcs)) %>%
      st_write(sprintf("regions_adm0_p/%s_p.gpkg", name0), delete_layer = TRUE)
    
    # adm1
    sub1 <- adm1_sf %>% filter(GID_0 %in% iso_codes) %>%
      mutate(ID = seq_len(n()))
    st_write(sub1, sprintf("regions_adm1/%s.gpkg", name1), delete_layer = TRUE)
    sub1 %>% st_transform(paste0(proj_auth, ":", pcs)) %>%
      st_write(sprintf("regions_adm1_p/%s_p.gpkg", name1), delete_layer = TRUE)
    
    # adm2
    sub2 <- adm2_sf %>% filter(GID_0 %in% iso_codes) %>%
      mutate(ID = seq_len(n()))
    st_write(sub2, sprintf("regions_adm2/%s.gpkg", name2), delete_layer = TRUE)
    sub2 %>% st_transform(paste0(proj_auth, ":", pcs)) %>%
      st_write(sprintf("regions_adm2_p/%s_p.gpkg", name2), delete_layer = TRUE)
    
    out[[suf]] <- sub0
  }
  invisible(out)
}

# ============================================================================
# Build all five major regions
# ============================================================================

# --- SSA (data-driven across 4 scenarios) ---
subregionsSSA <- readr::read_csv(subregionsSSA_v, show_col_types = FALSE)
message("Unique SSA subregions in '", subregionsSSA_v, "':")
print(sort(unique(subregionsSSA$Subregion)))
ssa_built <- build_and_write_regions(
  prefix = "SSA", subregions_df = subregionsSSA, adm0_sf = SSA_adm0,
  adm1_sf = gadm_adm1_sel, adm2_sf = gadm_adm2_sel,
  region_map = ssa_region_map, pcs = epsg_pcs, proj_auth = proj_authority
)

# --- LATAM ---
subregionsLATAM <- readr::read_csv(subregionsLATAM_v, show_col_types = FALSE)
latam_built <- build_and_write_regions(
  prefix = "LATAM", subregions_df = subregionsLATAM, adm0_sf = LATAM_adm0,
  adm1_sf = gadm_adm1_sel, adm2_sf = gadm_adm2_sel,
  region_map = latam_region_map, pcs = epsg_pcs, proj_auth = proj_authority,
  drop_labels = latam_drop_labels
)

# --- ASIA ---
subregionsASIA <- readr::read_csv(subregionsASIA_v, show_col_types = FALSE)
asia_built <- build_and_write_regions(
  prefix = "ASIA", subregions_df = subregionsASIA, adm0_sf = ASIA_adm0,
  adm1_sf = gadm_adm1_sel, adm2_sf = gadm_adm2_sel,
  region_map = asia_region_map, pcs = epsg_pcs, proj_auth = proj_authority
)

# --- OCEANIA (v7 builds only PNG; we drop the other 9 explicitly) ---
subregionsOCEANIA <- readr::read_csv(subregionsOCEANIA_v, show_col_types = FALSE)
oceania_built <- build_and_write_regions(
  prefix = "OCEANIA", subregions_df = subregionsOCEANIA, adm0_sf = OCEANIA_adm0,
  adm1_sf = gadm_adm1_sel, adm2_sf = gadm_adm2_sel,
  region_map = oceania_region_map, pcs = epsg_pcs, proj_auth = proj_authority,
  drop_labels = oceania_drop_labels
)

# --- NorAfri ---
subregionsNorAfr <- readr::read_csv(subregionsNorAfri_v, show_col_types = FALSE)
norafr_built <- build_and_write_regions(
  prefix = "NorAfr", subregions_df = subregionsNorAfr, adm0_sf = NorAfr_adm0,
  adm1_sf = gadm_adm1_sel, adm2_sf = gadm_adm2_sel,
  region_map = norafr_region_map, pcs = epsg_pcs, proj_auth = proj_authority
)

# ============================================================================
# Combine all per-region files into mofuss_regions{0,1,2}.gpkg + .shp
# (UNCHANGED from v7)
# ============================================================================
regions.list0 <- list.files(path = "regions_adm0", pattern = "*.gpkg", full.names = TRUE)
mofuss_regions0 <- do.call("rbind", lapply(regions.list0, st_read))
st_write(mofuss_regions0, "regions_adm0/mofuss_regions0.gpkg", delete_layer = TRUE)
st_write(mofuss_regions0, "regions_adm0/mofuss_regions0.shp",  delete_layer = TRUE)
unique(mofuss_regions0$GID_0)

regions.list1 <- list.files(path = "regions_adm1", pattern = "*.gpkg", full.names = TRUE)
mofuss_regions1 <- do.call("rbind", lapply(regions.list1, st_read))
st_write(mofuss_regions1, "regions_adm1/mofuss_regions1.gpkg", delete_layer = TRUE)
st_write(mofuss_regions1, "regions_adm1/mofuss_regions1.shp",  delete_layer = TRUE)

regions.list2 <- list.files(path = "regions_adm2", pattern = "*.gpkg", full.names = TRUE)
mofuss_regions2 <- do.call("rbind", lapply(regions.list2, st_read))
st_write(mofuss_regions2, "regions_adm2/mofuss_regions2.gpkg", delete_layer = TRUE)
st_write(mofuss_regions2, "regions_adm2/mofuss_regions2.shp",  delete_layer = TRUE)

# ============================================================================
# Simplified polygons for the web server (UNCHANGED)
# ============================================================================
if (run_ms == "Yes") {
  library(rmapshaper)
  check_sys_mapshaper()
  system("mapshaper --version")
  
  adm0_regtest <- st_read("regions_adm0/mofuss_regions0.gpkg")
  adm1_regtest <- st_read("regions_adm1/mofuss_regions1.gpkg")
  adm2_regtest <- st_read("regions_adm2/mofuss_regions2.gpkg")
  
  mofuss_regions0_simp <- adm0_regtest %>%
    st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
    ms_simplify(sys = TRUE) %>%
    st_transform(epsg_gcs)
  st_write(mofuss_regions0_simp, "regions_adm0/mofuss_regions0_simp.shp", delete_layer = TRUE)
  
  mofuss_regions1_simp <- adm1_regtest %>%
    st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
    ms_simplify(sys = TRUE, sys_mem = 16) %>%
    st_transform(epsg_gcs)
  st_write(mofuss_regions1_simp, "regions_adm1/mofuss_regions1_simp.shp", delete_layer = TRUE)
  
  mofuss_regions2_simp <- adm2_regtest %>%
    st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
    st_simplify(preserveTopology = TRUE, dTolerance = 50) %>%
    ms_simplify(keep = 0.6, keep_shapes = FALSE, sys = TRUE, sys_mem = 24) %>%
    st_transform(epsg_gcs)
  st_write(mofuss_regions2_simp, "regions_adm2/mofuss_regions2_simp.shp", delete_layer = TRUE)
}

# Project mofuss layers
st_read("regions_adm0/mofuss_regions0.gpkg") %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/mofuss_regions0_p.gpkg", delete_layer = TRUE)
st_read("regions_adm1/mofuss_regions1.gpkg") %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/mofuss_regions1_p.gpkg", delete_layer = TRUE)
st_read("regions_adm2/mofuss_regions2.gpkg") %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/mofuss_regions2_p.gpkg", delete_layer = TRUE)

# Update demand_in folder with latest mofuss_regions{0,1,2}.gpkg
for (lvl in 0:2) {
  src <- sprintf("regions_adm%d/mofuss_regions%d.gpkg", lvl, lvl)
  dst_dir <- paste0(demanddir, "/demand_in")
  dst <- file.path(dst_dir, sprintf("mofuss_regions%d.gpkg", lvl))
  
  # Remove destination first if it exists
  if (file.exists(dst)) {
    removed <- file.remove(dst)
    if (!removed) {
      stop("Could not delete ", dst,
           " — is it open in QGIS or another program?")
    }
  }
  
  # Now copy (no overwrite needed, destination is clean)
  ok <- file.copy(from = src, to = dst)
  if (!ok) {
    stop("Failed to copy ", src, " to ", dst)
  }
  message(sprintf("Copied %s (%.0f MB) -> %s",
                  basename(src),
                  file.info(src)$size / 1024^2,
                  dst))
}

# ============================================================================
# Ecoregions 2017 (UNCHANGED)
# ============================================================================
mofuss_regions04crop <- st_read("regions_adm0/mofuss_regions0.gpkg")

ecoregions_raw <- st_read("ecoregions2017.gpkg") %>%
  dplyr::select(-OBJECTID, -BIOME_NUM, -BIOME_NAME, -REALM, -ECO_BIOME_, -NNH,
                -SHAPE_LENG, -SHAPE_AREA, -COLOR, -COLOR_BIO, -COLOR_NNH,
                -LICENSE)

ecoregions_fixed <- st_make_valid(ecoregions_raw)
stopifnot(all(st_is_valid(ecoregions_fixed)))

ecoregions_intersected <- ecoregions_fixed %>%
  st_intersection(mofuss_regions04crop) %>%
  st_zm(drop = TRUE, what = "ZM")
st_write(ecoregions_intersected, "ecoregions/ecoregions2017.gpkg",
         layer = "ecoregions_mofuss", delete_layer = TRUE)

ecoregions_intersected %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("ecoregions_p/ecoregions2017_p.gpkg",
           layer = "ecoregions_mofuss", delete_layer = TRUE)

# End of script ----