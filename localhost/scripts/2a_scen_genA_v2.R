# MoFuSS
# Version 3
# Date: Mar 2024

# 2dolist ----

# Internal parameters ----
years_target <- 1990:2050

# Load libraries ----
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)
library(openxlsx)  # or writexl if you prefer

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
  dplyr::filter(Var == "region2BprocessedCtry_iso") %>%
  pull(ParCHR) -> region2BprocessedCtry_iso 

# Set directory to demand_in
setwd(paste0(demanddir,"/demand_in"))

#---------------------------------------------------------------------#
# 1. Prepare rural/urban data and baseline shares ----
#---------------------------------------------------------------------#

# TIDY the code

outfile <- "A_LMIC_Estimates_2050_popmedian_original.xlsx"

if (!file.exists(outfile)) {
  pop_file <- "A_LMIC_Estimates_2050_popmedian.xlsx"
  pop_rawplus <- read_excel(pop_file)
  openxlsx::write.xlsx(
    pop_rawplus,
    file = outfile,
    overwrite = FALSE
  )
} else {
  message("File already exists — not overwriting: ", outfile," 
          Reading from original")
  pop_file <- "A_LMIC_Estimates_2050_popmedian_original.xlsx"
  pop_rawplus <- read_excel(pop_file)
}

pop_raw <- pop_rawplus %>%
  # 1. Remove anything starting with "total" (any capitalization)
  filter(!str_detect(fuel, regex("^total", ignore_case = TRUE))) %>%

  # 2. Remove any existing Overall rows (any capitalization)
  filter(!str_detect(area, regex("^overall$", ignore_case = TRUE))) %>%

  # 3. Clean fuel labels
  mutate(
    fuel = tolower(trimws(fuel)),
    fuel = if_else(fuel == "biomass",  "fuelwood",    fuel),
    fuel = if_else(fuel == "electric", "electricity", fuel),
    fuel = str_to_title(fuel)
  )

# # 4. Create new Overall rows = Rural + Urban per year & fuel
# {
#   base <- .
# 
#   overall <- base %>%
#     filter(area %in% c("Rural", "Urban")) %>%
#     group_by(iso3, year, fuel) %>%          # ← per iso3 & year & fuel
#     summarise(
#       area = "Overall",
#       pop  = sum(pop, na.rm = TRUE),
#       .groups = "drop"
#     )
# 
#   dplyr::bind_rows(base, overall)
# } %>%
  
  # 5. Keep iso3 fuels + rural/urban
  pop_slv_ru <- pop_raw %>%
  filter(
    iso3 == region2BprocessedCtry_iso
  )

  # # 5. Nice ordering
  # arrange(iso3, year, area, fuel)

unique(pop_raw$area)
unique(pop_raw$fuel)


# # We work only with 6 fuels; drop Total Clean / Total Polluting etc.
# fuels_keep <- c("Biomass", "Charcoal", "Coal", "Electricity", "Gas", "Kerosene")
# 
# # Keep SLV + 6 fuels + rural/urban
# pop_slv_ru <- pop_rawplus %>%
#   filter(
#     iso3 == region2BprocessedCtry_iso,
#     fuel %in% fuels_keep,
#     area %in% c("Rural", "Urban")
#   )


unique(pop_slv_ru$area)
unique(pop_slv_ru$fuel)







# Write fixed full table
openxlsx::write.xlsx(
  pop_fixed,
  file = "A_LMIC_Estimates_2050_popmedian_temp.xlsx",
  overwrite = TRUE
)



#############


# Total population per area & year & fuel
totals_area_year <- pop_slv_ru %>%
  group_by(iso3, country, region, area, year) %>%
  summarise(
    total_pop_area = sum(pop),  # thousands of people
    .groups = "drop"
  )
unique(totals_area_year$area)
# unique(totals_area_year$fuel)

# Baseline share per fuel (rural/urban separately)
pop_slv_ru_shares <- pop_slv_ru %>%
  left_join(totals_area_year,
            by = c("iso3", "country", "region", "area", "year")) %>%
  mutate(
    share_old = if_else(total_pop_area > 0, pop / total_pop_area, 0)  # fraction [0–1]
  )

#---------------------------------------------------------------------#
# 2. Define anchor shares for Fuelwood & Charcoal (2009, 2016) ----
#---------------------------------------------------------------------#
base_path <- paste0(countrydir, "/LULCC/DownloadedDatasets")

# Find the folder that starts with "SourceData"
country_subfolder <- list.dirs(base_path, full.names = FALSE, recursive = FALSE) %>%
  grep("^SourceData", ., value = TRUE)

full_path <- file.path(base_path, country_subfolder)

full_path

file <- paste0(full_path,"/demand_parameters.xlsx")

anchor_shares_input <- read_excel(
  file,
  sheet = "share",
  skip = 0
) |> 
  mutate(
    area = as.character(area),
    fuel = as.character(fuel),
    year = as.integer(year),
    share_anchor = as.numeric(share_anchor)
  ) %>%
  dplyr::filter(!is.na(share_anchor))

# Stop is share is > 0.99 for any given year and area
# Check sums by year and area
check_sums <- anchor_shares_input %>%
  group_by(year, area) %>%
  summarise(total_share = sum(share_anchor, na.rm = TRUE), .groups = "drop")

# Identify any problematic rows
bad <- check_sums %>% 
  filter(total_share > 0.99)

# Stop if ANY violation is found
if (nrow(bad) > 0) {
  stop(
    paste0(
      "ERROR: share_anchor sums exceed 0.99 for:\n",
      paste(bad$year, bad$area, " (sum = ", round(bad$total_share, 3), ")", collapse = "\n")
    )
  )
}

#---------------------------------------------------------------------#
# 3. Interpolate / extrapolate target shares for 2010–2050 ----
#---------------------------------------------------------------------#
target_shares <- anchor_shares_input %>%
  group_by(area, fuel) %>%
  group_modify(~ {
    df <- .x
    approx(
      x    = df$year,
      y    = df$share_anchor,
      xout = years_target,
      rule = 2        # linear extrapolation beyond 2009–2016
    ) %>%
      as_tibble() %>%
      rename(year = x, share_target = y)
  }) %>%
  ungroup()

#---------------------------------------------------------------------#
# 4. Apply target shares & adjust other fuels so sum = 1 ----
#---------------------------------------------------------------------#
# Core logic:
#   
#   For each (iso3, area, year):
#   
#   Take baseline shares (share_old) for all fuels.
# 
# Replace Fuelwood & Charcoal with interpolated share_target.
# 
# If Fuelwood+Charcoal total > 1 → scale them down so they sum to 1, others → 0.
# 
# Else:
#   
#   “Other fuels” keep relative proportions, but are scaled so that
# sum(other_new_shares) = 1 – (Fuelwood+Charcoal).

pop_slv_ru_adjusted <- pop_slv_ru_shares %>%
  # Attach target shares only for B & C, and only in target years
  left_join(target_shares,
            by = c("area", "fuel", "year")) %>%
  group_by(iso3, country, region, area, year) %>%
  mutate(
    is_BC = fuel %in% c("Fuelwood", "Charcoal"),
    
    # Target share for B/C, fallback to old share if no target for that year
    share_target_BC = if_else(is_BC, share_target, NA_real_),
    share_target_BC = if_else(is.na(share_target_BC), share_old, share_target_BC),
    
    # Sum of target B + C shares in this area-year
    sum_BC_target = sum(if_else(is_BC, share_target_BC, 0), na.rm = TRUE),
    
    # Scale B/C down if they exceed 1
    scale_BC = if_else(sum_BC_target > 1, 1 / sum_BC_target, 1),
    share_BC_final = if_else(is_BC, share_target_BC * scale_BC, NA_real_),
    
    # Final B + C sum after scaling
    sum_BC_final = sum(if_else(is_BC, share_BC_final, 0), na.rm = TRUE),
    
    # Baseline sum of "other" shares (non B/C)
    sum_other_old = sum(if_else(!is_BC, share_old, 0), na.rm = TRUE),
    
    # Remaining share available for other fuels
    remaining_share = pmax(0, 1 - sum_BC_final),
    
    # Scale factor for other fuels so they fill the remaining share
    scale_others = if_else(sum_other_old > 0, remaining_share / sum_other_old, 0),
    
    # New share per fuel
    share_new = case_when(
      is_BC              ~ share_BC_final,
      !is_BC & sum_other_old > 0 ~ share_old * scale_others,
      TRUE                ~ 0
    ),
    
    # New population per fuel (thousands)
    pop_new = share_new * total_pop_area
  ) %>%
  ungroup()

# This keeps:
#   
#   pop (original population)
# 
# share_old (original fuel share per area/year)
# 
# share_new (adjusted fuel share per area/year)
# 
# pop_new (adjusted population, in thousands)

# Inspection
pop_slv_ru_adjusted %>%
  filter(year %in% c(1990, 2009, 2016, 2050),
         area == "Urban") %>%
  select(year, fuel, pop, pop_new, share_old, share_new) %>%
  arrange(year, fuel)

pop_slv_ru_adjusted %>%
  filter(area == "Urban", year %in% c(2009, 2010, 2016, 2030)) %>%
  group_by(area, year) %>%
  summarise(sum_new = sum(share_new), .groups = "drop")

pop_slv_ru_adjusted %>%
  filter(area == "Urban", fuel == "Fuelwood", year >= 2009) %>%
  select(year, total_pop_area, share_new, pop_new) %>%
  arrange(year)

#---------------------------------------------------------------------#
# 5. Rebuild “overall” as rural + urban (optional but probably useful) ----
#---------------------------------------------------------------------#  
pop_slv_overall_adjusted <- pop_slv_ru_adjusted %>%
  group_by(iso3, country, region, year, fuel) %>%
  summarise(
    area    = "Overall",
    pop     = sum(pop),      # original total (for comparison)
    pop_new = sum(pop_new),  # adjusted total
    .groups = "drop"
  ) %>%
  group_by(iso3, country, region, year, area) %>%
  mutate(
    total_pop_area     = sum(pop),
    total_pop_area_new = sum(pop_new),
    share_old_overall  = if_else(total_pop_area > 0, pop / total_pop_area, 0),
    share_new_overall  = if_else(total_pop_area_new > 0, pop_new / total_pop_area_new, 0)
  ) %>%
  ungroup()

#---------------------------------------------------------------------#
# 6. Final combined table (rural + urban + overall) ----
#---------------------------------------------------------------------#  
pop_slv_adjusted_final <- bind_rows(
  pop_slv_ru_adjusted,       # rural & urban
  pop_slv_overall_adjusted   # overall
) %>%
  arrange(area, year, fuel)

# Write to Excel
openxlsx::write.xlsx(
  pop_slv_adjusted_final,
  file = paste0("pop_",region2BprocessedCtry_iso,"_adjusted_final.xlsx"),
  overwrite = TRUE
)

# SLV replacement rows: iso3, country, region, area, fuel, year, pop_new
slv_replacements <- bind_rows(
  pop_slv_ru_adjusted %>% select(iso3, country, region, area, fuel, year, pop_new),
  pop_slv_overall_adjusted %>% select(iso3, country, region, area, fuel, year, pop_new)
)
unique(slv_replacements$area)
# pop_raw is your original full table from read_excel("A_LMIC_Estimates_2050_popmedian.xlsx")
pop_fixed <- pop_raw %>%
  # join SLV replacement rows
  left_join(
    slv_replacements %>% rename(pop_new_slv = pop_new),
    by = c("iso3", "country", "region", "area", "fuel", "year")
  ) %>%
  # replace pop only where we have a new value (i.e., SLV + selected fuels/areas)
  mutate(
    pop = if_else(!is.na(pop_new_slv), pop_new_slv, pop)
  ) %>%
  select(-pop_new_slv)  # ditch helper column; structure back to original

unique(pop_raw$area)
unique(slv_replacements$area)

# Write fixed full table
openxlsx::write.xlsx(
  pop_fixed,
  file = "A_LMIC_Estimates_2050_popmedian.xlsx",
  overwrite = TRUE
)

