# Download the shapefile from here: https://ecoregions.appspot.com/ and save in path (admin_regions in NRBv1...)

library(sf)

# Read the shapefile
ecoregions <- st_read("path/Ecoregions2017.shp")

# Save it as a GeoPackage
st_write(ecoregions, "G:/Mi unidad/webpages/2024_MoFuSSGlobal_Datasets/admin_regions/ecoregions2017.gpkg", layer = "ecoregions2017", delete_layer = TRUE)

file.exists("G:/Mi unidad/webpages/2024_MoFuSSGlobal_Datasets/admin_regions/ecoregions2017.gpkg")
st_layers("G:/Mi unidad/webpages/2024_MoFuSSGlobal_Datasets/admin_regions/ecoregions2017.gpkg")
ecoregions_check <- st_read("G:/Mi unidad/webpages/2024_MoFuSSGlobal_Datasets/admin_regions/ecoregions2017.gpkg", layer = "ecoregions2017")
# Check geometry validity
all(st_is_valid(ecoregions_check))

# Check number of rows and columns
dim(ecoregions_check)

# Check column names and types
str(ecoregions_check)

# Find the invalid geometries
which(!st_is_valid(ecoregions_check))
# Try fixing them
ecoregions_fixed <- st_make_valid(ecoregions_check)
all(st_is_valid(ecoregions_fixed))  # Should return TRUE now

st_write(ecoregions_fixed, "G:/Mi unidad/webpages/2024_MoFuSSGlobal_Datasets/admin_regions/ecoregions2017.gpkg",
         layer = "ecoregions2017", delete_layer = TRUE)

ecoregions_verified <- st_read("G:/Mi unidad/webpages/2024_MoFuSSGlobal_Datasets/admin_regions/ecoregions2017.gpkg")
all(st_is_valid(ecoregions_verified))  # Should be TRUE


