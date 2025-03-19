# TNC before and after

library(terra)
AE20102035_oldraster <- rast("G:/Mi unidad/webpages/2024_MoFuSSGlobal_Datasets/mofussDS_emissions4tnc/old_datasets/EmissionsTNC/2035/AE2035_gcs_tpp.tif")
# Mask negative values and sum only positive ones
sumold_positive_values <- global(AE20102035_oldraster * (AE20102035_oldraster > 0), "sum", na.rm = TRUE)
sumold <- round(sumold_positive_values/1000000/26,0)
sumold

AE20202035_newraster <- rast("G:/Mi unidad/webpages/2024_MoFuSSGlobal_Datasets/2035SSA_Con/AE2035_gcs_tpp_mean.tif")
# Mask negative values and sum only positive ones
sumnew_positive_values <- global(AE20202035_newraster * (AE20202035_newraster > 0), "sum", na.rm = TRUE)
sumnew <- round(sumnew_positive_values/1000000/16,0)
sumnew


# When using per hectare?
AE20202035_newraster_ha <- rast("G:/Mi unidad/webpages/2024_MoFuSSGlobal_Datasets/2035SSA_Con/AE2035_gcs_thayr_mean.tif")

library(terra)

# Load the raster
AE20202035_newraster_ha <- rast("G:/Mi unidad/webpages/2024_MoFuSSGlobal_Datasets/2035SSA_Con/AE2035_gcs_thayr_mean.tif")

# Create a cell area raster in hectares (ensures SpatRaster output)
cell_areas <- cellSize(AE20202035_newraster_ha, unit = "ha")

# Mask negative values
positive_values <- AE20202035_newraster_ha * (AE20202035_newraster_ha > 0)

# Convert from t/ha to total tons per cell
total_tons_per_cell <- positive_values * cell_areas

# Sum all positive contributions
sum_positive_total_tons <- global(total_tons_per_cell, "sum", na.rm = TRUE)
sumnew_ha <- round(sum_positive_total_tons / 1000000,0)
sumnew_ha

