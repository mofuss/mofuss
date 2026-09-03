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
# Version 9
# Date: Aug 2026
#
# This script loads GADM admin 0/1/2 levels and selects Global South countries
# with complete WHO demand data and positive coverage in the active WorldPop
# 1-km raster. Countries without native ADM1/ADM2 use the fallback geometries
# created below.
#
# Version 9 uses M67_GME_V2 as the sole Global South regionalization: 67
# evidence-led regions across 111 demand-ready countries. Its five major-region
# adapters are loaded together so one run builds the complete global regional
# vector. The first downstream model test focuses on the nine-country Great
# Lakes-East Africa region (RunCode GLEA).
#
# Future re-regionalization should start from regionalization_M67_GME_V2.csv,
# produce a new complete set of five adapter CSVs, and update the versioned
# filenames and candidate constants below. Each adapter must supply a unique,
# filesystem-safe RunCode for every region.
# 
# ============================================================================
# Internal parameters
# ============================================================================
run_ms <- "Yes"  # Run ms_simplify?
regionalization_candidate <- "M67_GME_V2"
expected_regionalization_countries <- 111L
expected_regionalization_regions <- 67L

regionalization_inputs <- c(
  SSA     = "subregionsSSA_v6_M67_GME_V2.csv",
  LATAM   = "subregionsLATAM_v4_M67_GME_V2.csv",
  ASIA    = "subregionsASIA_v6_M67_GME_V2.csv",
  OCEANIA = "subregionsOCEANIA_v2_M67_GME_V2.csv",
  NorAfr  = "subregionsNorAfri_v4_M67_GME_V2.csv"
)
subregionsSSA_v     <- regionalization_inputs[["SSA"]]
subregionsLATAM_v   <- regionalization_inputs[["LATAM"]]
subregionsASIA_v    <- regionalization_inputs[["ASIA"]]
subregionsOCEANIA_v <- regionalization_inputs[["OCEANIA"]]
subregionsNorAfri_v <- regionalization_inputs[["NorAfr"]]

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

library(jsonlite)
library(lwgeom)
library(magrittr)
library(sf)
library(tibble)
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
            "regions_adm0_p","regions_adm1_p","regions_adm2_p"
            ,"ecoregions","ecoregions_p")) {
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

# Audited against wp_global1000m_gcs.tif on 2026-08-29. These countries occur
# in the WHO workbook and GADM but have no positive cells in that raster.
worldpop_no_coverage_iso <- c(
  "COK", "FJI", "FSM", "KIR", "MHL", "NIU",
  "SLB", "TON", "TUV", "VUT", "WSM"
)
non_global_south_region <- "Northern America (M49) and Europe (M49)"
demand_ready_iso <- whodb_sel_u %>%
  dplyr::filter(
    region != non_global_south_region,
    !iso3 %in% worldpop_no_coverage_iso
  ) %>%
  dplyr::pull(iso3) %>%
  unique()

# Select WHO macro-regions by label, not by their order in the workbook.
region1        <- whodb_sel_u %>% dplyr::filter(startsWith(region, "Central Asia"))
region2        <- whodb_sel_u %>% dplyr::filter(startsWith(region, "Northern America"))
regionNorAfr   <- whodb_sel_u %>% dplyr::filter(startsWith(region, "Western Asia")) %>%
  filter(!iso3 %in% c("AZE","GEO","IRQ","JOR","SYR","TUR","YEM","ARM","SDN"))
regionWestAsia <- whodb_sel_u %>% dplyr::filter(startsWith(region, "Western Asia")) %>%
  filter(!iso3 %in% c("EGY","DZA","MAR","TUN","SDN"))
regionSSA      <- whodb_sel_u %>% dplyr::filter(startsWith(region, "Sub-Saharan Africa"))
regionLATAM    <- whodb_sel_u %>% dplyr::filter(startsWith(region, "Latin America"))
region6        <- whodb_sel_u %>% dplyr::filter(startsWith(region, "Eastern Asia"))
region7        <- whodb_sel_u %>% dplyr::filter(startsWith(region, "Oceania"))

R1_adm0          <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% region1$iso3)
R2_adm0          <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% region2$iso3)
NorAfr_adm0      <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% regionNorAfr$iso3)
WestAsia_adm0     <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% regionWestAsia$iso3)
SSA_adm0         <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% regionSSA$iso3)
LATAM_adm0       <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% regionLATAM$iso3)
R6_adm0          <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% region6$iso3)
OCEANIA_adm0     <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% region7$iso3)

ASIA_adm0 <- rbind(R1_adm0, R6_adm0, WestAsia_adm0) %>%
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
#
# Returns: invisibly a named list of the per-region adm0 sf objects.
build_and_write_regions <- function(prefix, subregions_df, adm0_sf,
                                    adm1_sf, adm2_sf,
                                    region_map, pcs, proj_auth) {
  
  required_cols <- c("Subregion", "GID_0", "NAME_0")
  missing_cols <- setdiff(required_cols, names(subregions_df))
  if (length(missing_cols) > 0) {
    stop(sprintf("[%s] Missing CSV columns: %s",
                 prefix, paste(missing_cols, collapse = ", ")))
  }

  if (nrow(subregions_df) == 0) {
    stop(sprintf("[%s] The selected regionalization contains no countries.", prefix))
  }
  if (anyNA(subregions_df[required_cols]) ||
      any(trimws(as.matrix(subregions_df[required_cols])) == "")) {
    stop(sprintf("[%s] Subregion, GID_0 and NAME_0 must not contain missing or blank values.",
                 prefix))
  }
  duplicate_iso <- unique(subregions_df$GID_0[duplicated(subregions_df$GID_0)])
  if (length(duplicate_iso) > 0) {
    stop(sprintf("[%s] Countries assigned more than once: %s",
                 prefix, paste(duplicate_iso, collapse = ", ")))
  }
  if (anyDuplicated(region_map$Subregion) || anyDuplicated(region_map$suffix)) {
    stop(sprintf("[%s] region_map requires unique Subregion labels and suffixes.", prefix))
  }
  
  # Sanity: every remaining label MUST be in region_map
  csv_labels <- unique(subregions_df$Subregion)
  unknown    <- setdiff(csv_labels, region_map$Subregion)
  if (length(unknown) > 0) {
    stop(sprintf("[%s] Subregion labels missing from region_map: %s",
                 prefix, paste(unknown, collapse = ", ")))
  }

  missing_geometry <- setdiff(subregions_df$GID_0, adm0_sf$GID_0)
  if (length(missing_geometry) > 0) {
    stop(sprintf("[%s] CSV countries missing from the selected GADM/WHO region: %s",
                 prefix, paste(missing_geometry, collapse = ", ")))
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

read_subregions <- function(path, major_region) {
  if (!file.exists(path)) {
    stop(sprintf("[%s] Regionalization CSV not found: %s", major_region, path))
  }
  readr::read_csv(
    path,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8")
  )
}

# M67 adapters carry their stable output suffix in RunCode; no separate map is
# maintained in code.
region_map_from_run_codes <- function(subregions_df, major_region) {
  configured_map <- subregions_df %>%
    dplyr::transmute(
      Subregion = trimws(as.character(Subregion)),
      suffix = trimws(as.character(RunCode))
    ) %>%
    dplyr::distinct()

  if (anyNA(configured_map) ||
      any(configured_map$Subregion == "") ||
      any(configured_map$suffix == "")) {
    stop(sprintf("[%s] Subregion and RunCode must not be missing or blank.",
                 major_region))
  }
  if (any(!grepl("^[A-Za-z0-9_]+$", configured_map$suffix))) {
    stop(sprintf(
      "[%s] RunCode values may contain only letters, numbers and underscores.",
      major_region
    ))
  }

  labels_with_multiple_codes <- configured_map %>%
    dplyr::count(Subregion) %>%
    dplyr::filter(n != 1L) %>%
    dplyr::pull(Subregion)
  codes_with_multiple_labels <- configured_map %>%
    dplyr::count(suffix) %>%
    dplyr::filter(n != 1L) %>%
    dplyr::pull(suffix)
  if (length(labels_with_multiple_codes) > 0 ||
      length(codes_with_multiple_labels) > 0) {
    stop(sprintf(
      paste0(
        "[%s] RunCode must map one-to-one with Subregion. ",
        "Repeated labels: %s; repeated codes: %s"
      ),
      major_region,
      paste(labels_with_multiple_codes, collapse = ", "),
      paste(codes_with_multiple_labels, collapse = ", ")
    ))
  }

  message(sprintf(
    "[%s] Using %d CSV-supplied RunCode values.",
    major_region, nrow(configured_map)
  ))
  configured_map
}

validate_m67_adapter <- function(subregions_df, major_region) {
  required_cols <- c(
    "Subregion", "GID_0", "NAME_0", "RunCode", "CandidateID",
    "CandidateRegionID", "ImporterV", "EvidenceConfidence", "Status"
  )
  missing_cols <- setdiff(required_cols, names(subregions_df))
  if (length(missing_cols) > 0) {
    stop(sprintf("[%s] Missing M67 adapter columns: %s",
                 major_region, paste(missing_cols, collapse = ", ")))
  }

  for (field in setdiff(required_cols, "ImporterV")) {
    values <- trimws(as.character(subregions_df[[field]]))
    if (anyNA(values) || any(values == "")) {
      stop(sprintf("[%s] %s must not be missing or blank.",
                   major_region, field))
    }
  }

  importer_values <- trimws(as.character(subregions_df$ImporterV))
  if (anyNA(importer_values) || any(!importer_values %in% c("0", "1"))) {
    stop(sprintf("[%s] ImporterV must contain only 0 or 1.", major_region))
  }
  if (!identical(unique(trimws(as.character(subregions_df$CandidateID))),
                 regionalization_candidate)) {
    stop(sprintf("[%s] CandidateID must be %s.",
                 major_region, regionalization_candidate))
  }
  invisible(subregions_df)
}

subregionsSSA <- read_subregions(subregionsSSA_v, "SSA")
subregionsLATAM <- read_subregions(subregionsLATAM_v, "LATAM")
subregionsASIA <- read_subregions(subregionsASIA_v, "ASIA")
subregionsOCEANIA <- read_subregions(subregionsOCEANIA_v, "OCEANIA")
subregionsNorAfr <- read_subregions(subregionsNorAfri_v, "NorAfr")

validate_m67_adapter(subregionsSSA, "SSA")
validate_m67_adapter(subregionsLATAM, "LATAM")
validate_m67_adapter(subregionsASIA, "ASIA")
validate_m67_adapter(subregionsOCEANIA, "OCEANIA")
validate_m67_adapter(subregionsNorAfr, "NorAfr")

region_maps <- list(
  SSA = region_map_from_run_codes(subregionsSSA, "SSA"),
  LATAM = region_map_from_run_codes(subregionsLATAM, "LATAM"),
  ASIA = region_map_from_run_codes(subregionsASIA, "ASIA"),
  OCEANIA = region_map_from_run_codes(subregionsOCEANIA, "OCEANIA"),
  NorAfr = region_map_from_run_codes(subregionsNorAfr, "NorAfr")
)

# Validate the complete M67 country and region extent before writing files.
selected_extent <- dplyr::bind_rows(
  subregionsSSA %>% dplyr::transmute(
    major_region = "SSA", GID_0, CandidateRegionID, RunCode, Subregion
  ),
  subregionsLATAM %>% dplyr::transmute(
    major_region = "LATAM", GID_0, CandidateRegionID, RunCode, Subregion
  ),
  subregionsASIA %>% dplyr::transmute(
    major_region = "ASIA", GID_0, CandidateRegionID, RunCode, Subregion
  ),
  subregionsOCEANIA %>% dplyr::transmute(
    major_region = "OCEANIA", GID_0, CandidateRegionID, RunCode, Subregion
  ),
  subregionsNorAfr %>% dplyr::transmute(
    major_region = "NorAfr", GID_0, CandidateRegionID, RunCode, Subregion
  )
)

duplicate_extent_iso <- selected_extent %>%
  dplyr::count(GID_0) %>%
  dplyr::filter(n != 1L) %>%
  dplyr::pull(GID_0)
missing_extent_iso <- setdiff(demand_ready_iso, selected_extent$GID_0)
unexpected_extent_iso <- setdiff(selected_extent$GID_0, demand_ready_iso)
region_definitions <- selected_extent %>%
  dplyr::distinct(major_region, CandidateRegionID, RunCode, Subregion)
if (length(demand_ready_iso) != expected_regionalization_countries ||
    nrow(selected_extent) != expected_regionalization_countries ||
    nrow(region_definitions) != expected_regionalization_regions ||
    dplyr::n_distinct(region_definitions$CandidateRegionID) !=
      expected_regionalization_regions ||
    dplyr::n_distinct(region_definitions$RunCode) !=
      expected_regionalization_regions ||
    length(duplicate_extent_iso) > 0 ||
    length(missing_extent_iso) > 0 ||
    length(unexpected_extent_iso) > 0) {
  stop(sprintf(
    paste0(
      "%s must contain %d countries and %d one-to-one region definitions ",
      "matching the demand-ready extent. Duplicated: %s; missing: %s; ",
      "unexpected: %s"
    ),
    regionalization_candidate,
    expected_regionalization_countries,
    expected_regionalization_regions,
    paste(duplicate_extent_iso, collapse = ", "),
    paste(missing_extent_iso, collapse = ", "),
    paste(unexpected_extent_iso, collapse = ", ")
  ))
}
message(sprintf(
  "%s validated: %d countries and %d regions across 5 major regions.",
  regionalization_candidate, nrow(selected_extent), nrow(region_definitions)
))

# --- SSA ---
message("Unique SSA subregions in '", subregionsSSA_v, "':")
print(sort(unique(subregionsSSA$Subregion)))
ssa_built <- build_and_write_regions(
  prefix = "SSA", subregions_df = subregionsSSA, adm0_sf = SSA_adm0,
  adm1_sf = gadm_adm1_sel, adm2_sf = gadm_adm2_sel,
  region_map = region_maps$SSA, pcs = epsg_pcs, proj_auth = proj_authority
)

# --- LATAM ---
latam_built <- build_and_write_regions(
  prefix = "LATAM", subregions_df = subregionsLATAM, adm0_sf = LATAM_adm0,
  adm1_sf = gadm_adm1_sel, adm2_sf = gadm_adm2_sel,
  region_map = region_maps$LATAM, pcs = epsg_pcs, proj_auth = proj_authority
)

# --- ASIA ---
asia_built <- build_and_write_regions(
  prefix = "ASIA", subregions_df = subregionsASIA, adm0_sf = ASIA_adm0,
  adm1_sf = gadm_adm1_sel, adm2_sf = gadm_adm2_sel,
  region_map = region_maps$ASIA, pcs = epsg_pcs, proj_auth = proj_authority
)

# --- OCEANIA ---
oceania_built <- build_and_write_regions(
  prefix = "OCEANIA", subregions_df = subregionsOCEANIA, adm0_sf = OCEANIA_adm0,
  adm1_sf = gadm_adm1_sel, adm2_sf = gadm_adm2_sel,
  region_map = region_maps$OCEANIA, pcs = epsg_pcs, proj_auth = proj_authority
)

# --- NorAfri ---
norafr_built <- build_and_write_regions(
  prefix = "NorAfr", subregions_df = subregionsNorAfr, adm0_sf = NorAfr_adm0,
  adm1_sf = gadm_adm1_sel, adm2_sf = gadm_adm2_sel,
  region_map = region_maps$NorAfr, pcs = epsg_pcs, proj_auth = proj_authority
)

# ============================================================================
# Combine all per-region files into mofuss_regions{0,1,2}.gpkg + .shp
# (UNCHANGED from v7)
# ============================================================================
regions.list0 <- list.files(path = "regions_adm0", pattern = "*.gpkg", full.names = TRUE)
mofuss_regions0 <- do.call("rbind", lapply(regions.list0, st_read))
st_write(mofuss_regions0, "regions_adm0/mofuss_regions0.gpkg", delete_layer = TRUE)

mofuss_regions0 %>%
  sf::st_drop_geometry() %>%
  dplyr::transmute(
    Value  = GID_0,
    Option = NAME_0
  ) %>%
  dplyr::arrange(Option) %>%
  jsonlite::write_json(
    "mofuss_countries0.json",
    pretty = TRUE,
    na = "null"
  )

st_write(mofuss_regions0, "regions_adm0/mofuss_regions0.shp",  delete_layer = TRUE)
unique(mofuss_regions0$GID_0)

files <- list.files("regions_adm0", full.names = FALSE)
file_options <- files %>%
  # Exclude subdirectories and files beginning with "mofuss"
  .[!dir.exists(file.path("regions_adm0", .))] %>%
  .[!startsWith(., "mofuss")] %>%
  # Remove the .gpkg extension
  sub("\\.gpkg$", "", ., ignore.case = TRUE) %>%
  tibble(
    Value  = .,
    Option = .
  ) %>%
  arrange(Option)
write_json(
  file_options,
  file.path("mofuss_regions0.json"),
  pretty = TRUE,
  na = "null"
)

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
