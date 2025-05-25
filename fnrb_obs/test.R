library(terra)
library(sf)

# Load AGB raster (not projected, in degrees)
agb_esa_2010 <- rast("C:/Users/aghil/Downloads/agbd_2010_1000m.tif")
agb_esa_2022 <- rast("C:/Users/aghil/Downloads/agbd_2022_1000m.tif")

agbx_2010 <- rast("C:/Users/aghil/Downloads/ctrees_global_2010_AGC_pantropic_1km_MgC02_ha.tif")
agb_ctrees_2010 <- agbx_2010 * (12/44) * (1/0.47)

agbx_2022 <- rast("C:/Users/aghil/Downloads/ctrees_global_2022_AGC_pantropic_1km_MgC02_ha.tif")
agb_ctrees_2022 <- agbx_2022 * (12/44) * (1/0.47)


# Load admin boundaries and filter for Kenya (GID_0 = "KEN")
regions <- st_read("D:/admin_regions/regions_adm0/mofuss_regions0.gpkg")
kenya <- regions[regions$GID_0 == "KEN", ]

# Project Kenya polygon to match raster CRS (if needed)
kenya_proj <- st_transform(kenya, crs(agb))

# Crop and mask raster
agb_kenya <- crop(agb, vect(kenya_proj))
agb_kenya <- mask(agb_kenya, vect(kenya_proj))

# Compute pixel area (in hectares) since the raster is not projected
# Default method for lat/lon CRS assumes WGS84 ellipsoid
pixel_area_ha <- cellSize(agb_kenya, unit = "ha")

# Multiply biomass by pixel area to get Mg per pixel
agb_total_kenya <- agb_kenya * pixel_area_ha

# Save the cropped biomass raster
writeRaster(agb_total_kenya, 
            "C:/Users/aghil/Downloads/agbd_2010_kenya_Mg_per_pixel.tif",
            overwrite = TRUE)

# Sum total biomass in Mg (i.e. tonnes)
total_Mg <- global(agb_total_kenya, fun = "sum", na.rm = TRUE)[1,1]

# Convert to Mt (millions of tonnes)
total_Mt <- total_Mg / 1e6

# Print result
cat("Total aboveground biomass in Kenya (Mt):", round(total_Mt, 2), "\n")

# agb <- rast("C:/Users/aghil/Downloads/




