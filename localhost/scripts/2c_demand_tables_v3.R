# MoFuSS
# Version 3
# Date: Mar 2024

# 2dolist ----

# Internal parameters ----
year_min_whodb  <- 1990
year_min_wfdb   <- 2010
year_max        <- 2050

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

country_parameters %>%
  dplyr::filter(Var == "subcountry") %>%
  pull(ParCHR) -> subcountry

country_parameters %>%
  dplyr::filter(Var == "scenario_ver") %>%
  pull(ParCHR) -> scenario_ver

country_parameters %>%
  dplyr::filter(Var == "demand_col") %>%
  pull(ParCHR) -> demand_col

# Set directory to demand
setwd(demanddir)

# Reads WHO dataset

# Reads WHO dataset
if (subcountry != 1) {
  whodb <- read_excel("demand_in/A_LMIC_Estimates_2050_popmedian.xlsx")
  # undb <- read_excel("admin_regions/UN_Rural-Urban_Pop_projections_formatted.xlsx") # https://population.un.org/wpp/Download/Standard/Population/
  whodb <- whodb %>%
    # Remove anything starting with "total" (any capitalization)
    filter(!str_detect(fuel, regex("^total", ignore_case = TRUE))) %>%
    
    # Remove rows where area == "Overall" (any capitalization)
    filter(!str_detect(area, regex("^overall$", ignore_case = TRUE))) %>%
    
    mutate(
      # 1. trim spaces and lowercase everything first
      fuel = tolower(trimws(fuel)),
      
      # 2. replace "biomass" → "fuelwood"
      fuel = if_else(fuel == "biomass", "fuelwood", fuel),
      
      # 3. replace "electric" → "electricity"
      fuel = if_else(fuel == "electric", "electricity", fuel),
      
      # 4. Capitalize first letter after all replacements
      fuel = str_to_title(fuel)
    )
  terra::unique(whodb$fuel)
  terra::unique(whodb$area)
}
getwd()

# Define scenarios ----
if (scenario_ver == "BaU") {
  wfdb <- read_csv("demand_in/cons_fuels_years.csv")
} else if (scenario_ver == "ICS") {
  wfdb <- read_csv("demand_in/cons_fuels_years_proj.csv")
} else if (scenario_ver == "BaU_vehicle_only") {
  wfdb <- read_csv("demand_in/cons_fuels_years_charc_and_urb_fw_only.csv")
} else if (scenario_ver == "BaU_walking_only") {
  wfdb <- read_csv("demand_in/cons_fuels_years_rural_fw_only.csv")
} else if (scenario_ver == "BaU_lusaka_notlusaka") {
  wfdb <- read_csv("demand_in/cons_fuels_years_BAU_Lusaka-NotLusaka.csv")
} else if (scenario_ver == "ICS1_lusaka_notlusaka") {
  wfdb <- read_csv("demand_in/cons_fuels_years_Proj1_Lusaka-NotLusaka.csv")
} else if (scenario_ver == "ICS2_lusaka_notlusaka") {
  wfdb <- read_csv("demand_in/cons_fuels_years_Proj2_Lusaka-NotLusaka.csv")
} else if (scenario_ver == "ICS3_lusaka_notlusaka") {
  wfdb <- read_csv("demand_in/cons_fuels_years_Proj3_Lusaka-NotLusaka.csv")
}  else if (scenario_ver == "MWI_BAU_fuel_cons") {
  wfdb <- read_csv("demand_in/MWI_BAU_fuel_cons.csv")
}
unique(wfdb$fuel)

wfdb <- wfdb %>%
  mutate(
    # 1. trim spaces and lowercase everything first
    fuel = tolower(trimws(fuel)),
    
    # 2. replace "biomass" → "fuelwood"
    fuel = if_else(fuel == "biomass", "fuelwood", fuel),
    
    # 3. replace "electric" → "electricity"
    fuel = if_else(fuel == "electric", "electricity", fuel),
    
    # 4. capitalize first letter only ONCE, after all replacements
    fuel = str_to_title(fuel)
  )

unique(wfdb$fuel)

outdir <- "demand_atlas"
full_path <- file.path(countrydir, outdir)

if (!dir.exists(full_path)) {
  dir.create(full_path, recursive = TRUE)
}

# Optional: verify
stopifnot(dir.exists(full_path))

setwd(countrydir)

# efchratio
base_path <- paste0(countrydir, "/LULCC/DownloadedDatasets")

# Find the folder that starts with "SourceData"
country_subfolder <- list.dirs(base_path, full.names = FALSE, recursive = FALSE) %>%
  grep("^SourceData", ., value = TRUE)

full_path <- file.path(base_path, country_subfolder)

full_path

file <- paste0(full_path,"/demand_parameters.xlsx")

efchratio_tb <- read_excel(
  file,
  sheet = "efchratio",
  skip = 0
) |> 
  mutate(
    efchratio = as.numeric(efchratio)
  )
efchratio <- efchratio_tb %>% dplyr::pull(efchratio)
efchratio

# ── LIBS ─────────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2)
  library(rlang); library(readr); library(scales); library(forcats)
})

# helper: consistent area ordering if present
order_area <- function(x) {
  lv <- c("Rural","Urban","Overall")
  x <- factor(x, levels = lv[lv %in% unique(x)])
  if (any(is.na(x))) x <- factor(as.character(x)) # fallback if different labels exist
  x
}

# ─────────────────────────────────────────────────────────────────────────────
# 1) WHODB (keep area; people = pop*1000; drop total categories)
# ─────────────────────────────────────────────────────────────────────────────
whodb_clean <- whodb %>%
  filter(
    iso3 == region2BprocessedCtry_iso,
    year >= year_min_whodb,
    year <= year_max
  ) %>%
  mutate(pop = pop * 1000) %>%
  
  # create Overall rows
  { 
    base <- .
    overall <- base %>%
      filter(area %in% c("Rural", "Urban")) %>%
      group_by(iso3, year, fuel) %>%
      summarise(
        area = "Overall",
        pop  = sum(pop, na.rm = TRUE),
        .groups = "drop"
      )
    
    bind_rows(base, overall)
  } %>%
  
  mutate(area = order_area(area)) %>%
  arrange(year, area, fuel)


# Long table: year, area, fuel, pop
write_csv(
  whodb_clean %>% select(year, area, fuel, pop),
  file.path(outdir, sprintf("whodb_pop_long_%s_%s_%s.csv", region2BprocessedCtry_iso, year_min_whodb, year_max))
)

# Wide table: one row per (year, area), columns = fuels [people]
whodb_wide <- whodb_clean %>%
  mutate(colname = paste0(fuel, " [people]")) %>%
  select(year, area, colname, pop) %>%
  pivot_wider(names_from = colname, values_from = pop) %>%
  arrange(year, area)

write_csv(
  whodb_wide,
  file.path(outdir, sprintf("whodb_pop_wide_%s_%s_%s.csv", region2BprocessedCtry_iso, year_min_whodb, year_max))
)

# 1) Max stacked height in Overall (sum across fuels per year, then take the max)
ymax_overall <- whodb_clean %>%
  dplyr::filter(area == "Overall") %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(total = sum(pop, na.rm = TRUE), .groups = "drop") %>%
  dplyr::summarise(ymax = max(total, na.rm = TRUE)) %>%
  dplyr::pull(ymax)


p_whodb <- ggplot(whodb_clean, aes(x = year, y = pop, fill = fuel)) +
  geom_area(alpha = 0.95, color = "grey30", linewidth = 0.2) +
  labs(
    title = sprintf("Population using each fuel in %s", region2BprocessedCtry_iso),
    subtitle = sprintf("%d–%d • Faceted by area (Y from max stacked in Overall)", year_min_whodb, year_max),
    x = NULL, y = "People", fill = "Fuel"
  ) +
  scale_x_continuous(breaks = seq(year_min_whodb, year_max, by = 5)) +
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_si("")),
    expand = expansion(mult = c(0, .05))
  ) +
  coord_cartesian(ylim = c(0, ymax_overall)) +   # common Y range
  facet_wrap(~ area, ncol = 3, scales = "fixed") +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        legend.position = "bottom")


ggsave(
  file.path(outdir, sprintf("whodb_pop_stack_faceted_%s_%s_%s.png", region2BprocessedCtry_iso, year_min_whodb, year_max)),
  p_whodb, width = 14, height = 7, dpi = 300, bg = "white"
)

# ─────────────────────────────────────────────────────────────────────────────
# 2) WFDB (keep area; Fuelwood + Charcoal only; Charcoal ÷ efchratio)
# ─────────────────────────────────────────────────────────────────────────────
col_sym <- rlang::sym(demand_col)

wfdb_twofuels <- wfdb %>%
  filter(iso3 == region2BprocessedCtry_iso,
         year >= year_min_wfdb, year <= year_max,
         fuel %in% c("Fuelwood", "Charcoal")) %>%
  mutate(
    area = order_area(area)
  ) %>%
  # NO extra summing across areas; just ensure stable ordering
  group_by(year, area, fuel) %>%
  summarise(value_woodeq_t = sum(!!col_sym, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    value_t = if_else(fuel == "Charcoal", value_woodeq_t / efchratio, value_woodeq_t),
    units   = "tonnes"
  ) %>%
  arrange(year, area, fuel)

# Long table: year, area, fuel, tonnes
write_csv(
  wfdb_twofuels %>% select(year, area, fuel, value_t, units),
  file.path(outdir, sprintf("wfdb_fw_char_long_%s_%s_%s_%s_byarea.csv", region2BprocessedCtry_iso, demand_col, year_min_wfdb, year_max))
)

# Wide table: one row per (year, area), columns = Fuelwood [t], Charcoal [t]
wfdb_twofuels_wide <- wfdb_twofuels %>%
  mutate(colname = paste0(fuel, " [t]")) %>%
  select(year, area, colname, value_t) %>%
  pivot_wider(names_from = colname, values_from = value_t) %>%
  arrange(year, area)

write_csv(
  wfdb_twofuels_wide,
  file.path(outdir, sprintf("wfdb_fw_char_wide_%s_%s_%s_%s_byarea.csv", region2BprocessedCtry_iso, demand_col, year_min_wfdb, year_max))
)

# Compute common Y max from Overall (max stacked across years)
ymax_overall_wfdb <- wfdb_twofuels %>%
  dplyr::filter(area == "Overall") %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(total = sum(value_t, na.rm = TRUE), .groups = "drop") %>%
  dplyr::summarise(ymax = max(total, na.rm = TRUE)) %>%
  dplyr::pull(ymax)

# Plot: 3-panel (facet by area), common Y from Overall
p_wfdb <- ggplot(wfdb_twofuels, aes(x = year, y = value_t, fill = fuel)) +
  geom_area(alpha = 0.95, color = "grey30", linewidth = 0.2) +
  labs(
    title = sprintf("Fuelwood & Charcoal demand in %s (tonnes, charcoal ÷ %s)", region2BprocessedCtry_iso, efchratio),
    subtitle = sprintf("%d–%d • Faceted by area (Y from max stacked in Overall) • source col: %s",
                       year_min_wfdb, year_max, demand_col),
    x = NULL, y = "Tonnes", fill = "Fuel"
  ) +
  scale_x_continuous(breaks = seq(year_min_wfdb, year_max, by = 5)) +
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_si("")),
    expand = expansion(mult = c(0, .05))
  ) +
  coord_cartesian(ylim = c(0, ymax_overall_wfdb)) +   # common Y range
  facet_wrap(~ area, ncol = 3, scales = "fixed") +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        legend.position = "bottom")


ggsave(
  file.path(outdir, sprintf("wfdb_fw_char_stack_faceted_%s_%s_%s_%s.png", region2BprocessedCtry_iso, demand_col, year_min_wfdb, year_max)),
  p_wfdb, width = 14, height = 7, dpi = 300, bg = "white"
)
