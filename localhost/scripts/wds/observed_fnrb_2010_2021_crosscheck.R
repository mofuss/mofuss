# Run the shiny app in a background R process
setwd("C:/Users/aghil/Documents/wf_demand_shinny")

library(sf)
library(terra)

# Load the polygon file
polygon_file <- "C:/Users/aghil/Documents/admin_regions/regions_adm0/mofuss_regions0.gpkg"
world_polygons <- st_read(polygon_file)

# Specify the GID_0 code for the country you want to crop by
selected_country_code <- "HND"  # Example: Brazil (GID_0 for Brazil is "BRA")

# Filter the selected polygon based on the GID_0 code
selected_polygon <- world_polygons %>% dplyr::filter(GID_0 == selected_country_code)

# Convert the selected polygon to terra's SpatVector
selected_polygon_vect <- vect(selected_polygon)

# Load the raster files

agb2010 <- rast("esacciagb_2010_2020_pcs_1km/out_gcs_2010/world_agb_2010_c.tif")
agb2021 <- rast("esacciagb_2010_2020_pcs_1km/out_gcs_2021/world_agb_2021_c.tif")

agb2010 <- rast("esacci_agb_2010_hnd/out_gcs/CA_agb_2010_c.tif")
agb2021 <- rast("esacci_agb_2021_hnd/out_gcs/CA_agb_2021_c.tif")

# Crop and mask the rasters using the selected polygon
agb2010_cropped <- terra::crop(agb2010, selected_polygon_vect)
agb2021_cropped <- terra::crop(agb2021, selected_polygon_vect)

agb2010_masked <- terra::mask(agb2010_cropped, selected_polygon_vect)
agb2021_masked <- terra::mask(agb2021_cropped, selected_polygon_vect)

# # Save the cropped rasters
# writeRaster(agb2010_masked, "Cropped_agb2010.tif", overwrite = TRUE)
# writeRaster(agb2021_masked, "Cropped_agb2021.tif", overwrite = TRUE)

# Optional: Display a message to confirm the rasters were cropped
print("Rasters cropped and saved successfully.")

# Compute agblosses10_21 for the current country
plot(agb2010_masked)
plot(agb2021_masked)
agb2010_masked2 <- agb2010_masked * 100
agb2021_masked2 <- agb2021_masked * 100
agblosses10_21 <- agb2010_masked2 - agb2021_masked2
agblosses10_21[agblosses10_21 < 0] <- NA
plot(agblosses10_21)
# Sum all positive pixel values and extract the numeric value from the data frame
total_agblosses10_21_df <- global(agblosses10_21, "sum", na.rm = TRUE)
total_agblosses10_21 <- round(total_agblosses10_21_df[1, 1],0)  # Extract the first value
total_agblosses10_21



