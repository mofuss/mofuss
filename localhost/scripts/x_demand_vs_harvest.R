# Load necessary libraries
library(dplyr)
library(readr)
library(purrr)
library(openxlsx)

# PARAMETERS
fulldemand <- 0   # 1 = insert all 4 demand columns, 0 = insert only Demand_2020_2030


# Step 1: Read the tables
summary_adm0 <- read_csv("https://zenodo.org/records/15054385/files/summary_adm0.csv?download=1")
cons_fuels_years <- read_csv("D:/demand/demand_in/cons_fuels_years.csv")

# Step 2: Filter cons_fuels_years for Biomass or Charcoal and area Overall
filtered_demand <- cons_fuels_years %>%
  filter(fuel %in% c("Biomass", "Charcoal"), area == "Overall")

# Step 3: Summarize fuel_tons3 by iso3 and year bins
demand_summary <- filtered_demand %>%
  group_by(iso3) %>%
  summarize(
    Demand_2020_2050 = sum(fuel_tons3[year >= 2021 & year <= 2050], na.rm = TRUE),
    Demand_2020_2030 = sum(fuel_tons3[year >= 2021 & year <= 2030], na.rm = TRUE),
    Demand_2030_2040 = sum(fuel_tons3[year >= 2031 & year <= 2040], na.rm = TRUE),
    Demand_2040_2050 = sum(fuel_tons3[year >= 2041 & year <= 2050], na.rm = TRUE)
  )

# Step 4: Join summary_adm0 with demand_summary
final_table <- summary_adm0 %>%
  left_join(demand_summary, by = c("GID_0" = "iso3"))

# Step 4.5: Drop extra demand columns if fulldemand == 0
if (fulldemand == 0) {
  final_table <- final_table %>%
    dplyr::select(-c(Demand_2020_2050, Demand_2030_2040, Demand_2040_2050))
}

# Step 5: Correctly reorder columns to insert demand columns

cols <- names(final_table)
pos1 <- which(cols == "Harv_2020_2050_se")
pos2 <- which(cols == "Harv_2020_2030_se")
pos3 <- which(cols == "Harv_2030_2040_se")
pos4 <- which(cols == "Harv_2040_2050_se")

if (fulldemand == 1) {
  new_cols <- c(
    cols[1:pos1],
    "Demand_2020_2050",
    cols[(pos1+1):pos2],
    "Demand_2020_2030",
    cols[(pos2+1):pos3],
    "Demand_2030_2040",
    cols[(pos3+1):pos4],
    "Demand_2040_2050",
    cols[(pos4+1):length(cols)]
  )
} else {
  # Only insert Demand_2020_2030 after Harv_2020_2030_se
  new_cols <- c(
    cols[1:pos2],
    "Demand_2020_2030",
    cols[(pos2+1):length(cols)]
  )
}

final_table <- final_table %>%
  select(all_of(new_cols))


# Step 6: Sort alphabetically by mofuss_reg
final_table <- final_table %>%
  arrange(mofuss_reg)

# Step 7: Divide by 1000 from NRB_2020_2050_mean up to just before the fNRB columns
start_col <- which(names(final_table) == "NRB_2020_2050_mean")
stop_col <- min(grep("^fNRB", names(final_table)))

cols_to_divide <- names(final_table)[start_col:(stop_col - 1)]

final_table <- final_table %>%
  mutate(across(all_of(cols_to_divide), ~ round(.x / 1000)))

# Step 8: Insert Unmet Demand (kton) and Unmet Demand (%) after Demand_2020_2030
final_table <- final_table %>%
  mutate(
    delta_2020_2030_kton = Demand_2020_2030 - Harv_2020_2030_mean,
    delta_2020_2030_perc = ifelse(Demand_2020_2030 > 0, 
                                  round((Demand_2020_2030 - Harv_2020_2030_mean) / Demand_2020_2030 * 100, 1),
                                  NA)
  )

# Reorder columns to place the two new columns immediately after Demand_2020_2030
cols <- names(final_table)
idx <- which(cols == "Demand_2020_2030")

final_table <- final_table %>%
  select(
    all_of(cols[1:idx]),
    delta_2020_2030_kton,
    delta_2020_2030_perc,
    all_of(cols[(idx+1):length(cols)])
  )

# Step 8.5: Manual tweaks to Demand_2020_2030
# (before calculating subtotals)

# Define a lookup table of adjustments
adjustments <- tibble::tibble(
  GID_0 = c("BGD", "IND", "IDN", "NPL", "PAK", 
            "COL", "DOM", "HTI", "JAM", "PNG", 
            "BDI", "COD", "RWA", "BRA", "MEX",
            "AGO", "BEN", "KEN", "MDG", "MLI", 
            "BWA", "NAM", "SWZ", "ZAF", "CRI",
            "GTM", "HND", "NIC", "PAN", "GUY",
            "PHL", "LKA", "ECU", "PER", "MYS",
            "BFA", "CIV", "TCD", "DJI", "ERI",
            "ETH", "SOM", "GHA", "MOZ", "MWI",
            "MRT", "NER", "SDN", "SSD", "GMB",
            "SEN", "TGO", "UGA", "CAF", "CMR", 
            "NGA", "GIN", "GNB", "LBR", "SLE",
            "COG", "GAB", "GNQ", "ZMB", "MMR"
            
  ),
  factor = c(0.48, 1.11, 1.11, 1.04, 1.02, 
             1.13, 1.95, 1.95, 0.77, 0.85, 
             0.89, 0.89, 0.89, 0.93, 0.90, 
             0.90, 0.90, 1.04, 1.08, 0.90, 
             1.13, 1.13, 1.13, 1.13, 0.92, 
             0.92, 0.92, 0.92, 0.92, 1.05,
             0.93, 0.93, 0.94, 0.94, 0.94,
             0.91, 0.93, 0.95, 0.91, 0.91,
             0.91, 0.91, 0.93, 0.95, 0.95,
             0.95, 0.95, 0.95, 0.95, 0.90,
             0.90, 0.94, 0.90, 0.90, 0.90,
             0.90, 0.90, 0.90, 0.90, 0.90,
             0.94, 0.94, 0.94, 0.95, 0.95
             ) # reductions <1, rises >1
)

# Apply the adjustments
final_table <- final_table %>%
  mutate(Demand_2020_2030 = ifelse(GID_0 %in% adjustments$GID_0,
                                   round(Demand_2020_2030 * adjustments$factor[match(GID_0, adjustments$GID_0)]),
                                   Demand_2020_2030))

# Recalculate delta values after adjustment
final_table <- final_table %>%
  mutate(
    delta_2020_2030_kton = Demand_2020_2030 - Harv_2020_2030_mean,
    delta_2020_2030_perc = ifelse(Demand_2020_2030 > 0,
                                  round((Demand_2020_2030 - Harv_2020_2030_mean) / Demand_2020_2030 * 100, 1),
                                  NA)
  )




# Step 9: Create subtotal rows for each mofuss_reg

# 9.1 Calculate subtotals of only the needed columns
subtotals <- final_table %>%
  group_by(mofuss_reg) %>%
  summarize(
    Harv_2020_2030_mean = sum(Harv_2020_2030_mean, na.rm = TRUE),
    Demand_2020_2030 = sum(Demand_2020_2030, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    delta_2020_2030_kton = Demand_2020_2030 - Harv_2020_2030_mean,
    delta_2020_2030_perc = ifelse(Demand_2020_2030 > 0,
                                  round((Demand_2020_2030 - Harv_2020_2030_mean) / Demand_2020_2030 * 100, 1),
                                  NA)
  )


# 9.2 Create full empty skeleton
skeleton <- final_table[0, ] # 0-row data frame, just structure

# 9.3 For each subtotal, create a skeleton row and fill only the required fields
subtotals_full <- subtotals %>%
  rowwise() %>%
  mutate(row = list({
    new_row <- skeleton[1, ]
    new_row$mofuss_reg <- mofuss_reg
    new_row$NAME_0 <- "Subtotal"
    new_row$Harv_2020_2030_mean <- Harv_2020_2030_mean
    new_row$Demand_2020_2030 <- Demand_2020_2030
    new_row$delta_2020_2030_kton <- delta_2020_2030_kton
    new_row$delta_2020_2030_perc <- delta_2020_2030_perc
    new_row
  })) %>%
  pull(row) %>%
  bind_rows()

# 9.4 Insert subtotal rows manually after each group
# Group the main table
grouped_tables <- final_table %>%
  group_by(mofuss_reg) %>%
  group_split()

# Prepare a list of correct subtotals
subtotals_by_group <- subtotals_full %>%
  split(.$mofuss_reg)

# Bind each group followed by its subtotal
table_with_subtotals <- purrr::map_dfr(grouped_tables, function(group) {
  mofuss_value <- unique(group$mofuss_reg)
  if (mofuss_value %in% names(subtotals_by_group)) {
    bind_rows(group, subtotals_by_group[[mofuss_value]])
  } else {
    group  # if no subtotal available (shouldn't happen, but safe)
  }
})

# 9.5 Overwrite final_table
final_table <- table_with_subtotals


# Step 10: Save final table with bold, gray subtotal rows, autofit columns, and freeze header
# Create workbook
wb <- createWorkbook()

# Add worksheet
addWorksheet(wb, "Sheet1")

# Write data
writeData(wb, "Sheet1", final_table)

# Create bold + gray style for subtotals
bold_gray_style <- createStyle(textDecoration = "bold", fgFill = "#D9D9D9")

# Find subtotal rows
subtotal_rows <- which(final_table$NAME_0 == "Subtotal") + 1  # +1 because header counts as row 1

# Apply style to subtotal rows
for (r in subtotal_rows) {
  addStyle(wb, "Sheet1", style = bold_gray_style, rows = r, cols = 1:ncol(final_table), gridExpand = TRUE)
}

# Auto-fit column widths
setColWidths(wb, sheet = "Sheet1", cols = 1:ncol(final_table), widths = "auto")

# To freeze cell G2, you simply use
freezePane(wb, sheet = "Sheet1", firstActiveRow = 2, firstActiveCol = 7)

# Save workbook
saveWorkbook(wb, "D:/demand/demand_in/summary_adm0_with_demand.xlsx", overwrite = TRUE)





