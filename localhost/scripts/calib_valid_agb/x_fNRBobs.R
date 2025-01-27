# MoFuSS
# Version 1
# Date: Jan 2025

# 2dolist ----

# Internal parameters ----
agbdir <- "E:/agb3rdparties/"
demanddir <- "D:/demand/demand_in/"
polygon_file <- "D:/admin_regions/regions_adm0/mofuss_regions0.gpkg"
selected_country_code <- "GHA"
carbon2biomass <- 0.47
# Ctrees years
ctress <- 1
ctrees_years <- c(2010, 2017, 2020, 2022)

# Load libraries ----
library(sf)
library(terra)
library(tidyverse)
library(purrr)
library(dplyr)
library(ggplot2)

setwd(agbdir)

# Detect OS
os <- Sys.info()["sysname"]

# Load the polygon file
world_polygons <- st_read(polygon_file)

# Filter the selected polygon based on the GID_0 code
selected_polygon <- world_polygons %>% dplyr::filter(GID_0 == selected_country_code)

# Convert the selected polygon to terra's SpatVector
selected_polygon_vect <- vect(selected_polygon)

# Load the raster files ----
## CTrees - 1km ----
agb_rasters <- map(
  ctrees_years,
  function(year) {
    raster <- rast(paste0("Pantropical_AGC/ctrees_global_", year, "_AGC_pantropic_1km_MgC02_ha.tif")) %>%
      crop(selected_polygon_vect) %>%
      mask(selected_polygon_vect) %>%
      (function(x) x * (12 / 44)) %>%  # Convert MgCO2/ha to MgC/ha
      (function(x) x / 0.47)           # Convert MgC/ha to AGB (Mg/ha)
    
    # Get resolution (in degrees) and calculate approximate cell area in hectares
    res_deg <- res(raster) # Resolution in degrees
    cell_area_ha <- abs(res_deg[1] * res_deg[2]) * 111.32^2 * 100 # Convert to hectares
    
    # Multiply raster values by cell area (if needed for sum computation)
    raster * cell_area_ha
  }
)

# Name the list by years for clarity
names(agb_rasters) <- as.character(ctrees_years)

# Compute losses and store results
losses <- map_dbl(ctrees_years[-1], function(year) {
  # Compute difference (AGB 2010 - AGB year)
  diff_raster <- agb_rasters[["2010"]] - agb_rasters[[as.character(year)]]
  
  # Mask out positive values (gains)
  loss_raster <- diff_raster
  loss_raster[loss_raster < 0] <- NA
  
  # Sum all loss values
  global_loss <- global(loss_raster, "sum", na.rm = TRUE)
  
  # Extract the numeric value (first column of the data.frame)
  as.numeric(global_loss[1, 1])
})

# Create a summary table
loss_table <- data.frame(
  Period = paste0("2010-", ctrees_years[-1]),
  AGB_Loss = round(losses, 0)
)

# Rename the AGB_Loss column to include the country code
colnames(loss_table)[2] <- paste0("AGB_Loss") #_", selected_country_code)

# Print the table
print(loss_table)

# Read woodfuel demand tables
cons_fuels_years <- read_csv(paste0(demanddir, "cons_fuels_years.csv"))

# Filter data for the selected country and conditions
country_data <- cons_fuels_years %>%
  filter(
    iso3 %in% selected_country_code,
    fuel %in% c("Biomass", "Charcoal"),
    area %in% c("Rural", "Urban")
  )

# Summarize fuel data for each target year
fuel_sums <- purrr::map_dfr(
  ctrees_years[-1], # Exclude the first year (2010)
  function(end_year) {
    country_data %>%
      filter(year >= 2010 & year <= end_year) %>% # Filter for years in the range
      summarise(
        Period = paste0("2010-", end_year),
        Sum_fuel_tons1 = sum(fuel_tons1, na.rm = TRUE),
        Sum_fuel_tons2 = sum(fuel_tons2, na.rm = TRUE),
        Sum_fuel_tons3 = sum(fuel_tons3, na.rm = TRUE)
      )
  }
)

# Combine the results with the loss_table
fNRBobs_table <- fuel_sums %>%
  left_join(loss_table, by = "Period") %>% # Join on Period
  mutate(
    fNRBobs1 = round(AGB_Loss / Sum_fuel_tons1 * 100,0),
    fNRBobs2 = round(AGB_Loss / Sum_fuel_tons2 * 100,0),
    fNRBobs3 = round(AGB_Loss / Sum_fuel_tons3 * 100,0)
  )

# Print the updated loss_table
print(fNRBobs_table)
write.csv(fNRBobs_table, file = paste0(selected_country_code,"_1km_ctrees.csv"), row.names = FALSE)

