# parameters
file_path <- "D:/demand/demand_in/cons_fuels_years.csv"
# Specify the GID_0 code for the country you want to crop by
selected_country_code <- "BRA"  # Example: Brazil (GID_0 for Brazil is "BRA")
startyr <- 2010
endyr <- 2017

# Load packages ----
library(sf)
library(terra)
library("rnaturalearth")
library("rnaturalearthdata")
library(readr)
library(dplyr)

# Filter the selected polygon based on the GID_0 code
world <- ne_countries(scale = "medium", returnclass = "sf")
selected_polygon <- world %>% dplyr::filter(iso_a3 == selected_country_code)
selected_polygon_vect <- vect(selected_polygon)
sort(unique(world$iso_a3))

crop_by_country2 <- function(raster, selected_polygon) {
  cropped_raster <- terra::crop(raster, selected_polygon)
  masked_raster <- terra::mask(cropped_raster, selected_polygon)
  return(masked_raster)
}

# Load the raster files
agb2010CO2 <- rast("E:/agb3rdparties/Pantropical_AGC/ctrees_global_2010_AGC_pantropic_1km_MgC02_ha.tif")
agb20XXCO2 <- rast(paste0("E:/agb3rdparties/Pantropical_AGC/ctrees_global_",endyr,"_AGC_pantropic_1km_MgC02_ha.tif"))
agb2010 <- agb2010CO2 * 12/44 / 0.47
agb20XX <- agb20XXCO2 * 12/44 / 0.47

agb2010_cropped <- terra::crop(agb2010, selected_polygon_vect)
agb20XX_cropped <- terra::crop(agb20XX, selected_polygon_vect)

agb2010_masked <- terra::mask(agb2010_cropped, selected_polygon_vect)
agb20XX_masked <- terra::mask(agb20XX_cropped, selected_polygon_vect)

# Calculate the area of each pixel in square meters (terra works in meters for Geographic Coordinate Systems)
agb2010_masked_pixel_area_m2 <- cellSize(agb2010_masked, unit = "m")
agb20XX_masked_pixel_area_m2 <- cellSize(agb20XX_masked, unit = "m")

# Convert the area to hectares (1 hectare = 10,000 square meters)
agb2010_masked_pixel_area_ha <- agb2010_masked_pixel_area_m2 / 10000
agb20XX_masked_pixel_area_ha <- agb20XX_masked_pixel_area_m2 / 10000

# Multiply the AGB values (Mg/ha) by the area of each pixel to get the total AGB per pixel (in Mg)
agb2010_masked2 <- agb2010_masked * agb2010_masked_pixel_area_ha
agb20XX_masked2 <- agb20XX_masked * agb20XX_masked_pixel_area_ha

agblosses10_XX <- agb2010_masked2 - agb20XX_masked2
agblosses10_XX[agblosses10_XX <= 0] <- NA

# Sum all positive pixel values and extract the numeric value from the data frame
total_agblosses10_XX_df <- global(agblosses10_XX, "sum", na.rm = TRUE)
total_agblosses10_XX <- round(total_agblosses10_XX_df[1, 1],0)  # Extract the first value
total_agblosses10_XX


data_wf <- read_csv(file_path)
demand_sum <- data_wf %>%
  dplyr::filter(iso3 %in% selected_country_code,
         year >= startyr, year <= endyr,
         fuel == "Biomass" & area %in% c("Rural", "Urban") |
           fuel == "Charcoal" & area %in% c("Rural", "Urban")) %>%
  summarise(total_value = sum(fuel_tons3, na.rm = TRUE)) %>%
  pull(total_value) %>%
  round(.,0)
demand_sum

fNRB_obs <- round(total_agblosses10_XX/demand_sum*100,0)
fNRB_obs

total_agblosses10_XX
