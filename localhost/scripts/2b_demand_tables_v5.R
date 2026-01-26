# MoFuSS
# Version 3
# Date: Mar 2024

# 2dolist ----

# Internal parameters ----
year_min_whodb  <- 1990
year_min_wfdb   <- 2010
year_max        <- 2050

# Load libraries ----
library(conflicted)

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2)
  library(rlang); library(readr); library(scales); library(forcats)
})

# Print parameters table ----
country_parameters <- read_delim(parameters_file_path, delim = delimiter)
print(tibble::as_tibble(country_parameters), n=100)

country_parameters %>%
  dplyr::filter(Var == "demand_tuning") %>%
  pull(ParCHR) %>%
  as.integer(.) -> demand_tuning

country_parameters %>%
  dplyr::filter(Var == "efchratio") %>%
  pull(ParCHR) %>%
  as.integer(.) -> efchratio

setwd(demanddir)

# Reads WHO dataset
if (subcountry != 1) {
  whodb <- read_excel("demand_in/A_LMIC_Estimates_2050_popmedian.xlsx")
  # undb <- read_excel("admin_regions/UN_Rural-Urban_Pop_projections_formatted.xlsx") # https://population.un.org/wpp/Download/Standard/Population/
  terra::unique(whodb$fuel)
  # terra::unique(whodb$year)
  # terra::unique(whodb$iso3)
}
getwd()

# Define scenarios ----
detect_delimiter <- function(file) {
  line1 <- readLines(file, n = 1)
  if (stringr::str_detect(line1, ";")) return(";")
  return(",")
}

read_wfdb <- function(file) {
  delim <- detect_delimiter(file)
  readr::read_delim(file, delim = delim, show_col_types = FALSE)
}

# Define scenarios ----
if (scenario_ver == "BaU") {
  wfdb <- read_wfdb("demand_in/cons_fuels_years.csv")
  
} else if (scenario_ver == "ICS") {
  wfdb <- read_wfdb("demand_in/cons_fuels_years_proj.csv")
  
} else if (scenario_ver == "BaU_vehicle_only") {
  wfdb <- read_wfdb("demand_in/cons_fuels_years_charc_and_urb_fw_only.csv")
  
} else if (scenario_ver == "BaU_walking_only") {
  wfdb <- read_wfdb("demand_in/cons_fuels_years_rural_fw_only.csv")
  
} else if (scenario_ver == "BaU_lusaka_notlusaka") {
  wfdb <- read_wfdb("demand_in/cons_fuels_years_BAU_Lusaka-NotLusaka.csv")
  
} else if (scenario_ver == "ICS1_lusaka_notlusaka") {
  wfdb <- read_wfdb("demand_in/cons_fuels_years_Proj1_Lusaka-NotLusaka.csv")
  
} else if (scenario_ver == "ICS2_lusaka_notlusaka") {
  wfdb <- read_wfdb("demand_in/cons_fuels_years_Proj2_Lusaka-NotLusaka.csv")
  
} else if (scenario_ver == "ICS3_lusaka_notlusaka") {
  wfdb <- read_wfdb("demand_in/cons_fuels_years_Proj3_Lusaka-NotLusaka.csv")
  
} else if (scenario_ver == "MWI_BAU_fuel_cons") {
  wfdb <- read_wfdb("demand_in/MWI_BAU_fuel_cons.csv")
  
} else if (scenario_ver == "MWI_BAU_chyield") {
  wfdb <- read_wfdb("demand_in/MWI_BAU_fuel_cons_chyields4unfccc.csv")
  
} else if (scenario_ver == "KHM_BAU_fuel_cons") {
  wfdb <- read_wfdb("demand_in/KHM_BAU_fuel_cons.csv")
}

unique(wfdb$fuel)

wfdb <- wfdb %>%
  dplyr::mutate(
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
head(wfdb)
print(scenario_ver) # save as text to recover later down the river

outdir <- "demand_atlas"
full_path <- file.path(countrydir, outdir)

if (!dir.exists(full_path)) {
  dir.create(full_path, recursive = TRUE)
}

# Optional: verify
stopifnot(dir.exists(full_path))

setwd(countrydir)

# helper: consistent area ordering if present
order_area <- function(x) {
  lv <- c("Rural","Urban","Overall")
  x <- factor(x, levels = lv[lv %in% unique(x)])
  if (any(is.na(x))) x <- factor(as.character(x)) # fallback if different labels exist
  x
}

# ─────────────────────────────────────────────────────────────────────────────
# 1) POPULATION BY FUEL (WHODB when subcountry != 1; WFDB when subcountry == 1)
#    - If subcountry == 1, create split = country (e.g., Lusaka / NotLusaka)
#    - Otherwise, no split
# ─────────────────────────────────────────────────────────────────────────────

# --- helper: safe ymax from a chosen area level ---
.ymax_from_area <- function(df, area_level = "Overall", value_col = pop) {
  value_col <- rlang::enquo(value_col)
  df %>%
    dplyr::filter(area == area_level) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(total = sum(!!value_col, na.rm = TRUE), .groups = "drop") %>%
    dplyr::summarise(ymax = max(total, na.rm = TRUE), .groups = "drop") %>%
    dplyr::pull(ymax)
}

# --- helper: fuel colors (your existing palette) ---
fuel_palette <- c(
  # Impact / implied fuels → lighter tints
  "Imp_fuelwood" = "#C9A27D",  # light brown (tint of Fuelwood)
  "Imp_charcoal" = "#7A7A7A",  # mid grey (tint of Charcoal)
  
  # Real fuels → strong anchors
  "Fuelwood"     = "#8B4513",  # brown → wood
  "Charcoal"     = "#2B2B2B",  # near-black → carbon
  
  "Coal"         = "#4B4B4B",
  "Kerosene"     = "#E69F00",
  "Gas"          = "#56B4E9",
  "Electricity"  = "#F0E442",
  "Biogas"       = "#A65628",
  "Pellets"      = "#999999",
  "Ethanol"      = "#CC79A7"
)

# --- 1A) Build ONE clean population table, with optional split column ---
if (subcountry != 1) {
  
  popdb_clean <- whodb %>%
    dplyr::filter(
      iso3 == region2BprocessedCtry_iso,
      year >= year_min_whodb, year <= year_max,
      !fuel %in% c("Total Polluting", "Total Clean")
    ) %>%
    dplyr::mutate(
      pop  = pop * 1000,
      area = order_area(area),
      split = NA_character_   # keep column for consistent downstream code
    ) %>%
    dplyr::select(iso3, region, split, area, fuel, year, pop) %>%
    dplyr::arrange(year, area, fuel)
  
  pop_prefix <- "whodb"
  year_min_pop <- year_min_whodb
  
} else {
  
  # IMPORTANT: in your subcountry files, 'country' is the split label (Lusaka/NotLusaka)
  popdb_clean <- wfdb %>%
    dplyr::filter(
      iso3 == region2BprocessedCtry_iso,
      year >= year_min_wfdb, year <= year_max
    ) %>%
    dplyr::mutate(
      pop   = people * 1000,
      area  = order_area(area),
      split = as.character(country)  # Lusaka / NotLusaka
    ) %>%
    dplyr::select(iso3, region, split, area, fuel, year, pop) %>%
    dplyr::arrange(split, year, area, fuel)
  
  pop_prefix <- "robdb"
  year_min_pop <- year_min_wfdb
}

popdb_clean %>% count(split, area, fuel, year) %>% dplyr::filter(n > 1)

# --- 1B) Write LONG table (always) ---
write_csv(
  popdb_clean %>% dplyr::select(year, area, fuel, pop, split),
  file.path(outdir, sprintf("%s_pop_long_%s_%s_%s.csv",
                            pop_prefix, region2BprocessedCtry_iso, year_min_pop, year_max))
)

# --- 1C) Write WIDE table (key depends on split existence) ---
# If split is all NA => widen by (year, area)
# Else => widen by (split, year, area)
wide_keys <- if (all(is.na(popdb_clean$split))) c("year","area") else c("split","year","area")

popdb_wide <- popdb_clean %>%
  dplyr::mutate(colname = paste0(fuel, " [people]")) %>%
  dplyr::group_by(dplyr::across(dplyr::all_of(c(wide_keys, "colname")))) %>%
  dplyr::summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop") %>%  # <- major fix: no list-cols
  tidyr::pivot_wider(names_from = colname, values_from = pop) %>%
  dplyr::arrange(dplyr::across(dplyr::all_of(wide_keys)))

write_csv(
  popdb_wide,
  file.path(outdir, sprintf("%s_pop_wide_%s_%s_%s.csv",
                            pop_prefix, region2BprocessedCtry_iso, year_min_pop, year_max))
)

# --- 1D) Plotting function (reusable for each split) ---
plot_pop_stack <- function(df, title_suffix = NULL, out_png) {
  
  # use Overall if present; otherwise fallback to max over all areas
  has_overall <- any(df$area == "Overall")
  if (has_overall) {
    ymax <- .ymax_from_area(df, "Overall", pop)
  } else {
    ymax <- df %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(total = sum(pop, na.rm = TRUE), .groups = "drop") %>%
      dplyr::summarise(ymax = max(total, na.rm = TRUE), .groups = "drop") %>%
      dplyr::pull(ymax)
  }
  
  p <- ggplot(df, aes(x = year, y = pop, fill = fuel)) +
    geom_area(alpha = 0.95, color = "grey30", linewidth = 0.2) +
    labs(
      title = paste0("Population using each fuel in ", region2BprocessedCtry_iso,
                     if (!is.null(title_suffix)) paste0(" — ", title_suffix) else ""),
      subtitle = sprintf("%d–%d • Faceted by area (common Y from %s)",
                         year_min_pop, year_max, if (has_overall) "Overall" else "max across areas"),
      x = NULL, y = "People", fill = "Fuel"
    ) +
    scale_x_continuous(breaks = seq(year_min_pop, year_max, by = 5)) +
    scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_si("")),
      expand = expansion(mult = c(0, .05))
    ) +
    coord_cartesian(ylim = c(0, ymax)) +
    facet_wrap(~ area, ncol = 3, scales = "fixed") +
    scale_fill_manual(values = fuel_palette, na.value = "grey70") +
    theme_bw(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          plot.title.position = "plot",
          legend.position = "bottom")
  
  ggsave(out_png, p, width = 14, height = 7, dpi = 300, bg = "white")
  invisible(p)
}

# --- 1E) Make plots ---
if (all(is.na(popdb_clean$split))) {
  
  # Single plot (no split)
  plot_pop_stack(
    df = popdb_clean,
    title_suffix = NULL,
    out_png = file.path(outdir, sprintf("%s_pop_stack_faceted_%s_%s_%s.png",
                                        pop_prefix, region2BprocessedCtry_iso, year_min_pop, year_max))
  )
  
} else {
  
  # One plot per split (e.g., Lusaka / NotLusaka)
  splits <- sort(unique(popdb_clean$split))
  
  for (sp in splits) {
    df_sp <- popdb_clean %>% dplyr::filter(split == sp)
    
    plot_pop_stack(
      df = df_sp,
      title_suffix = sp,
      out_png = file.path(outdir, sprintf("%s_pop_stack_faceted_%s_%s_%s_%s.png",
                                          pop_prefix, region2BprocessedCtry_iso, sp, year_min_pop, year_max))
    )
  }
  
  # Optional: one combined plot with split as rows and area as columns
  # (comment out if you don’t want it)
  # Compute Y max as the maximum of Overall totals
  # across Lusaka / NotLusaka (but NOT larger combinations)
  ymax_all <- popdb_clean %>%
    dplyr::filter(area == "Overall") %>%          # key line
    dplyr::group_by(split, year) %>%
    dplyr::summarise(total = sum(pop, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(split) %>%
    dplyr::summarise(ymax_split = max(total, na.rm = TRUE), .groups = "drop") %>%
    dplyr::summarise(ymax = max(ymax_split, na.rm = TRUE)) %>%
    dplyr::pull(ymax)
  
  p_combined <- ggplot(popdb_clean, aes(x = year, y = pop, fill = fuel)) +
    geom_area(alpha = 0.95, color = "grey30", linewidth = 0.2) +
    labs(
      title = sprintf("Population using each fuel in %s — by subcountry split", region2BprocessedCtry_iso),
      subtitle = sprintf("%d–%d • Rows = split • Columns = area (common Y across all splits)",
                         year_min_pop, year_max),
      x = NULL, y = "People", fill = "Fuel"
    ) +
    scale_x_continuous(breaks = seq(year_min_pop, year_max, by = 5)) +
    scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_si("")),
      expand = expansion(mult = c(0, .05))
    ) +
    coord_cartesian(ylim = c(0, ymax_all)) +
    facet_grid(split ~ area, scales = "fixed") +
    scale_fill_manual(values = fuel_palette, na.value = "grey70") +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          plot.title.position = "plot",
          legend.position = "bottom")
  
  ggsave(
    file.path(outdir, sprintf("%s_pop_stack_splitgrid_%s_%s_%s.png",
                              pop_prefix, region2BprocessedCtry_iso, year_min_pop, year_max)),
    p_combined, width = 14, height = 9, dpi = 300, bg = "white"
  )
}


# ─────────────────────────────────────────────────────────────────────────────
# 2) WFDB (keep area; Fuelwood + Charcoal only; Charcoal ÷ efchratio)
#    - If subcountry == 1, treat `country` as split (Lusaka/NotLusaka)
# ─────────────────────────────────────────────────────────────────────────────

col_sym <- rlang::sym(demand_col)

wfdb_base <- wfdb %>%
  dplyr::filter(
    iso3 == region2BprocessedCtry_iso,
    year >= year_min_wfdb, year <= year_max,
    fuel %in% c("Fuelwood", "Charcoal")
  ) %>%
  dplyr::mutate(
    area = order_area(area),
    split = if (subcountry == 1) as.character(country) else NA_character_
  )


# Summarise with or without split
wfdb_twofuels <- if (subcountry == 1) {
  
  wfdb_base %>%
    dplyr::group_by(split, year, area, fuel) %>%
    dplyr::summarise(value_woodeq_t = sum(!!col_sym, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
      value_t = dplyr::if_else(fuel == "Charcoal", value_woodeq_t / efchratio, value_woodeq_t),
      units   = "tonnes"
    ) %>%
    dplyr::arrange(split, year, area, fuel)
  
} else {
  
  wfdb_base %>%
    dplyr::group_by(year, area, fuel) %>%
    dplyr::summarise(value_woodeq_t = sum(!!col_sym, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
      value_t = dplyr::if_else(fuel == "Charcoal", value_woodeq_t / efchratio, value_woodeq_t),
      units   = "tonnes"
    ) %>%
    dplyr::arrange(year, area, fuel)
}

# Long table
write_csv(
  wfdb_twofuels %>%
    dplyr::select(dplyr::any_of("split"), year, area, fuel, value_t, units),
  file.path(outdir, sprintf("wfdb_fw_char_long_%s_%s_%s_%s_byarea%s.csv",
                            region2BprocessedCtry_iso, demand_col, year_min_wfdb, year_max,
                            ifelse(subcountry == 1, "_bysplit", "")))
)

# Wide table
wfdb_twofuels_wide <- wfdb_twofuels %>%
  dplyr::mutate(colname = paste0(fuel, " [t]")) %>%
  {
    if (subcountry == 1) {
      dplyr::select(., split, year, area, colname, value_t) %>%
        tidyr::pivot_wider(names_from = colname, values_from = value_t) %>%
        dplyr::arrange(split, year, area)
    } else {
      dplyr::select(., year, area, colname, value_t) %>%
        tidyr::pivot_wider(names_from = colname, values_from = value_t) %>%
        dplyr::arrange(year, area)
    }
  }

write_csv(
  wfdb_twofuels_wide,
  file.path(outdir, sprintf("wfdb_fw_char_wide_%s_%s_%s_%s_byarea%s.csv",
                            region2BprocessedCtry_iso, demand_col, year_min_wfdb, year_max,
                            ifelse(subcountry == 1, "_bysplit", "")))
)

# ── Y limits: SAME logic you liked ──
# If split exists: common Y = max of split-wise Overall maxima (tight, not inflated)
# Else: common Y = Overall max (as before)

if (subcountry == 1) {
  
  ymax_all_wfdb <- wfdb_twofuels %>%
    dplyr::filter(area == "Overall") %>%
    dplyr::group_by(split, year) %>%
    dplyr::summarise(total = sum(value_t, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(split) %>%
    dplyr::summarise(ymax_split = max(total, na.rm = TRUE), .groups = "drop") %>%
    dplyr::summarise(ymax = max(ymax_split, na.rm = TRUE), .groups = "drop") %>%
    dplyr::pull(ymax)
  
  # One plot per split (3-panel by area)
  # One plot per split (3-panel by area) — each uses its OWN ymax from Overall
  for (sp in sort(unique(wfdb_twofuels$split))) {
    
    df_sp <- wfdb_twofuels %>% dplyr::filter(split == sp)
    
    ymax_sp <- df_sp %>%
      dplyr::filter(area == "Overall") %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(total = sum(value_t, na.rm = TRUE), .groups = "drop") %>%
      dplyr::summarise(ymax = max(total, na.rm = TRUE), .groups = "drop") %>%
      dplyr::pull(ymax)
    
    p_sp <- ggplot(df_sp, aes(x = year, y = value_t, fill = fuel)) +
      geom_area(alpha = 0.95, color = "grey30", linewidth = 0.2) +
      labs(
        title = sprintf("Fuelwood & Charcoal demand in %s — %s (tonnes, charcoal ÷ %s)",
                        region2BprocessedCtry_iso, sp, efchratio),
        subtitle = sprintf("%d–%d • Faceted by area (Y from max stacked in Overall) • source col: %s",
                           year_min_wfdb, year_max, demand_col),
        x = NULL, y = "Tonnes", fill = "Fuel"
      ) +
      scale_x_continuous(breaks = seq(year_min_wfdb, year_max, by = 5)) +
      scale_y_continuous(
        labels = scales::label_number(scale_cut = scales::cut_si("")),
        expand = expansion(mult = c(0, .05))
      ) +
      coord_cartesian(ylim = c(0, ymax_sp)) +  # <- KEY CHANGE (per-split ymax)
      facet_wrap(~ area, ncol = 3, scales = "fixed") +
      scale_fill_manual(values = c("Fuelwood" = "#8B4513", "Charcoal" = "#2B2B2B")) +
      theme_bw(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            plot.title.position = "plot",
            legend.position = "bottom")
    
    ggsave(
      file.path(outdir, sprintf("wfdb_fw_char_stack_faceted_%s_%s_%s_%s_%s.png",
                                region2BprocessedCtry_iso, sp, demand_col, year_min_wfdb, year_max)),
      p_sp, width = 14, height = 7, dpi = 300, bg = "white"
    )
  }
  
  
  # Optional: combined split grid (rows = split, cols = area), same Y
  p_grid <- ggplot(wfdb_twofuels, aes(x = year, y = value_t, fill = fuel)) +
    geom_area(alpha = 0.95, color = "grey30", linewidth = 0.2) +
    labs(
      title = sprintf("Fuelwood & Charcoal demand in %s — by subcountry split (tonnes, charcoal ÷ %s)",
                      region2BprocessedCtry_iso, efchratio),
      subtitle = sprintf("%d–%d • Rows = split • Columns = area (common Y based on max(Overall) across splits) • source col: %s",
                         year_min_wfdb, year_max, demand_col),
      x = NULL, y = "Tonnes", fill = "Fuel"
    ) +
    scale_x_continuous(breaks = seq(year_min_wfdb, year_max, by = 5)) +
    scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_si("")),
      expand = expansion(mult = c(0, .05))
    ) +
    coord_cartesian(ylim = c(0, ymax_all_wfdb)) +
    facet_grid(split ~ area, scales = "fixed") +
    scale_fill_manual(values = c("Fuelwood" = "#8B4513", "Charcoal" = "#2B2B2B")) +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          plot.title.position = "plot",
          legend.position = "bottom")
  
  ggsave(
    file.path(outdir, sprintf("wfdb_fw_char_stack_splitgrid_%s_%s_%s_%s.png",
                              region2BprocessedCtry_iso, demand_col, year_min_wfdb, year_max)),
    p_grid, width = 14, height = 9, dpi = 300, bg = "white"
  )
  
} else {
  
  # Original behavior (no split)
  ymax_overall_wfdb <- wfdb_twofuels %>%
    dplyr::filter(area == "Overall") %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(total = sum(value_t, na.rm = TRUE), .groups = "drop") %>%
    dplyr::summarise(ymax = max(total, na.rm = TRUE), .groups = "drop") %>%
    dplyr::pull(ymax)
  
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
    coord_cartesian(ylim = c(0, ymax_overall_wfdb)) +
    facet_wrap(~ area, ncol = 3, scales = "fixed") +
    scale_fill_manual(values = c("Fuelwood" = "#8B4513", "Charcoal" = "#2B2B2B")) +
    theme_bw(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          plot.title.position = "plot",
          legend.position = "bottom")
  
  ggsave(
    file.path(outdir, sprintf("wfdb_fw_char_stack_faceted_%s_%s_%s_%s.png",
                              region2BprocessedCtry_iso, demand_col, year_min_wfdb, year_max)),
    p_wfdb, width = 14, height = 7, dpi = 300, bg = "white"
  )
}
